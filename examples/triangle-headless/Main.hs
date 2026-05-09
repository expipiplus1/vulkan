{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main
  ( main
  )
where

import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JP
import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Bits
import qualified Data.ByteString.Lazy as BSL
import Data.Functor.Identity (Identity (..))
import qualified Data.Vector as V
import Data.Word
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peek, sizeOf)
import Say (sayErr)
import qualified Vma
import Vulkan.CStruct.Extends (SomeStruct (..))
import qualified Vulkan.Core10 as CommandBufferBeginInfo (CommandBufferBeginInfo (..))
import qualified Vulkan.Core10 as CommandPoolCreateInfo (CommandPoolCreateInfo (..))
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Core10.DeviceInitialization as DI
import qualified Vulkan.Core10.Image as Image
import Vulkan.Extensions.VK_EXT_debug_utils
import Vulkan.Requirement (InstanceRequirement (..))
import Vulkan.Utils.Debug (debugCallbackPtr, nameObject)
import qualified Vulkan.Utils.Init.Headless as Init
import Vulkan.Utils.Initialization (createDeviceFromRequirements, physicalDeviceName, pickPhysicalDevice)
import Vulkan.Utils.QueueAssignment (QueueFamilyIndex (..), QueueSpec (..), assignQueues, isGraphicsQueueFamily)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (frag, vert)
import Vulkan.Zero (zero)
import qualified VulkanMemoryAllocator as VMA

main :: IO ()
main = runResourceT $ do
  inst <- Main.createInstance
  (phys, graphicsQueueFamilyIndex, dev) <- Main.createDevice inst
  allocator <- Vma.createVMA zero myApiVersion inst phys dev
  image <- render allocator dev graphicsQueueFamilyIndex
  Vk.deviceWaitIdle dev
  let filename = "triangle.png"
  sayErr $ "Writing " <> filename
  liftIO $ BSL.writeFile filename (JP.encodePng image)

{- | This function renders a triangle and reads the image on the CPU

It:
- Initializes two images
  - A GPU image which is used as the framebuffer
  - A CPU image which is copied to and read on the CPU
- Creates a RenderPass with a single subpass
- Creates a graphics pipeline
- Creates command pool and allocated a single command buffer
- Uses the command buffer to
  - Render into the GPU image
  - Issue a barrier to make it safe to transfer from the GPU image
  - Issue a barrier to make it safe to write to the CPU image
  - Perform an image copy
  - Issue a barrier to make it safe to read the CPU image on the host
- Submits and waits for the command buffer to finish executing
- Invalidates the CPU image allocation (if it isn't HOST_COHERENT)
- Copies the data from the CPU image and returns it
-}
render
  :: VMA.Allocator
  -> Vk.Device
  -> Word32
  -> ResourceT IO (JP.Image JP.PixelRGBA8)
render allocator dev graphicsQueueFamilyIndex = do
  let
    imageFormat = Vk.FORMAT_R8G8B8A8_UNORM
    width = 256
    height = 256

  -- Create an image to be our render target
  let
    imageCreateInfo =
      zero
        { Vk.imageType = Vk.IMAGE_TYPE_2D
        , Vk.format = imageFormat
        , Vk.extent = Vk.Extent3D width height 1
        , Vk.mipLevels = 1
        , Vk.arrayLayers = 1
        , Vk.samples = Vk.SAMPLE_COUNT_1_BIT
        , Vk.tiling = Vk.IMAGE_TILING_OPTIMAL
        , Vk.usage =
            Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT
              .|. Vk.IMAGE_USAGE_TRANSFER_SRC_BIT
        , Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED
        }
    allocationCreateInfo =
      zero
        { VMA.flags = VMA.ALLOCATION_CREATE_MAPPED_BIT
        , VMA.usage = VMA.MEMORY_USAGE_GPU_ONLY
        }
  (_, (image, _, _)) <-
    VMA.withImage
      allocator
      imageCreateInfo
      allocationCreateInfo
      allocate
  nameObject dev image "GPU side image"

  -- Create an image to read on the CPU
  let
    cpuImageCreateInfo =
      zero
        { Vk.imageType = Vk.IMAGE_TYPE_2D
        , Vk.format = imageFormat
        , Vk.extent = Vk.Extent3D width height 1
        , Vk.mipLevels = 1
        , Vk.arrayLayers = 1
        , Vk.samples = Vk.SAMPLE_COUNT_1_BIT
        , Vk.tiling = Vk.IMAGE_TILING_LINEAR
        , Vk.usage = Vk.IMAGE_USAGE_TRANSFER_DST_BIT
        , Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED
        }
    cpuAllocationCreateInfo =
      zero
        { VMA.flags = VMA.ALLOCATION_CREATE_MAPPED_BIT
        , VMA.usage = VMA.MEMORY_USAGE_GPU_TO_CPU
        }
  (_, (cpuImage, cpuImageAllocation, cpuImageAllocationInfo)) <-
    VMA.withImage
      allocator
      cpuImageCreateInfo
      cpuAllocationCreateInfo
      allocate
  nameObject dev cpuImage "CPU side image"

  -- Create an image view
  let
    imageSubresourceRange =
      Vk.ImageSubresourceRange
        { Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT
        , Vk.baseMipLevel = 0
        , Vk.levelCount = 1
        , Vk.baseArrayLayer = 0
        , Vk.layerCount = 1
        }
    imageViewCreateInfo =
      zero
        { Vk.image = image
        , Vk.viewType = Vk.IMAGE_VIEW_TYPE_2D
        , Vk.format = imageFormat
        , Vk.components =
            Vk.ComponentMapping
              Vk.COMPONENT_SWIZZLE_IDENTITY
              Vk.COMPONENT_SWIZZLE_IDENTITY
              Vk.COMPONENT_SWIZZLE_IDENTITY
              Vk.COMPONENT_SWIZZLE_IDENTITY
        , Vk.subresourceRange = imageSubresourceRange
        }
  (_, imageView) <- Vk.withImageView dev imageViewCreateInfo Nothing allocate

  -- Create a renderpass with a single subpass
  let
    attachmentDescription :: Vk.AttachmentDescription
    attachmentDescription =
      zero
        { Vk.format = imageFormat
        , Vk.samples = Vk.SAMPLE_COUNT_1_BIT
        , Vk.loadOp = Vk.ATTACHMENT_LOAD_OP_CLEAR
        , Vk.storeOp = Vk.ATTACHMENT_STORE_OP_STORE
        , Vk.stencilLoadOp = Vk.ATTACHMENT_LOAD_OP_DONT_CARE
        , Vk.stencilStoreOp = Vk.ATTACHMENT_STORE_OP_DONT_CARE
        , Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED
        , Vk.finalLayout = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        }
    subpass :: Vk.SubpassDescription
    subpass =
      zero
        { Vk.pipelineBindPoint = Vk.PIPELINE_BIND_POINT_GRAPHICS
        , Vk.colorAttachments =
            [ zero
                { Vk.attachment = 0
                , Vk.layout = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                }
            ]
        }
    subpassDependency :: Vk.SubpassDependency
    subpassDependency =
      zero
        { Vk.srcSubpass = Vk.SUBPASS_EXTERNAL
        , Vk.dstSubpass = 0
        , Vk.srcStageMask = Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        , Vk.srcAccessMask = zero
        , Vk.dstStageMask = Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        , Vk.dstAccessMask =
            Vk.ACCESS_COLOR_ATTACHMENT_READ_BIT
              .|. Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT
        }
  (_, renderPass) <-
    Vk.withRenderPass
      dev
      zero
        { Vk.attachments = [attachmentDescription]
        , Vk.subpasses = [subpass]
        , Vk.dependencies = [subpassDependency]
        }
      Nothing
      allocate

  -- Create a framebuffer
  let
    framebufferCreateInfo :: Vk.FramebufferCreateInfo '[]
    framebufferCreateInfo =
      zero
        { Vk.renderPass = renderPass
        , Vk.attachments = [imageView]
        , Vk.width = width
        , Vk.height = height
        , Vk.layers = 1
        }
  (_, framebuffer) <- Vk.withFramebuffer dev framebufferCreateInfo Nothing allocate

  -- Create the most vanilla rendering pipeline
  shaderStages <- createShaders dev
  (_, pipelineLayout) <- Vk.withPipelineLayout dev zero Nothing allocate
  let
    pipelineCreateInfo :: Vk.GraphicsPipelineCreateInfo '[]
    pipelineCreateInfo =
      zero
        { Vk.stages = shaderStages
        , Vk.vertexInputState = Just zero
        , Vk.inputAssemblyState =
            Just
              zero
                { Vk.topology = Vk.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
                , Vk.primitiveRestartEnable = False
                }
        , Vk.viewportState =
            Just $
              SomeStruct
                zero
                  { Vk.viewports =
                      [ Vk.Viewport
                          { Vk.x = 0
                          , Vk.y = 0
                          , Vk.width = realToFrac (width :: Word32)
                          , Vk.height = realToFrac (height :: Word32)
                          , Vk.minDepth = 0
                          , Vk.maxDepth = 1
                          }
                      ]
                  , Vk.scissors =
                      [ Vk.Rect2D
                          { Vk.offset = Vk.Offset2D 0 0
                          , Vk.extent = Vk.Extent2D width height
                          }
                      ]
                  }
        , Vk.rasterizationState =
            Just $
              SomeStruct
                zero
                  { Vk.depthClampEnable = False
                  , Vk.rasterizerDiscardEnable = False
                  , Vk.lineWidth = 1
                  , Vk.polygonMode = Vk.POLYGON_MODE_FILL
                  , Vk.cullMode = Vk.CULL_MODE_NONE
                  , Vk.frontFace = Vk.FRONT_FACE_CLOCKWISE
                  , Vk.depthBiasEnable = False
                  }
        , Vk.multisampleState =
            Just $
              SomeStruct
                zero
                  { Vk.sampleShadingEnable = False
                  , Vk.rasterizationSamples = Vk.SAMPLE_COUNT_1_BIT
                  , Vk.minSampleShading = 1
                  , Vk.sampleMask = [maxBound]
                  }
        , Vk.depthStencilState = Nothing
        , Vk.colorBlendState =
            Just $
              SomeStruct
                zero
                  { Vk.logicOpEnable = False
                  , Vk.attachments =
                      [ zero
                          { Vk.colorWriteMask =
                              Vk.COLOR_COMPONENT_R_BIT
                                .|. Vk.COLOR_COMPONENT_G_BIT
                                .|. Vk.COLOR_COMPONENT_B_BIT
                                .|. Vk.COLOR_COMPONENT_A_BIT
                          , Vk.blendEnable = False
                          }
                      ]
                  }
        , Vk.dynamicState = Nothing
        , Vk.layout = pipelineLayout
        , Vk.renderPass = renderPass
        , Vk.subpass = 0
        , Vk.basePipelineHandle = zero
        }
  (_, (_, [graphicsPipeline])) <- Vk.withGraphicsPipelines dev zero [SomeStruct pipelineCreateInfo] Nothing allocate

  -- Create a command buffer
  let commandPoolCreateInfo =
        zero
          { CommandPoolCreateInfo.queueFamilyIndex = graphicsQueueFamilyIndex
          }
  (_, commandPool) <- Vk.withCommandPool dev commandPoolCreateInfo Nothing allocate
  let commandBufferAllocateInfo =
        zero
          { Vk.commandPool = commandPool
          , Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY
          , Vk.commandBufferCount = 1
          }
  (_, [commandBuffer]) <- Vk.withCommandBuffers dev commandBufferAllocateInfo allocate

  -- Fill command buffer
  --
  -- - Execute the renderpass
  -- - Transition the images to be able to perform the copy
  -- - Copy the image to CPU mapped memory
  Vk.useCommandBuffer commandBuffer zero{CommandBufferBeginInfo.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT} do
    let renderPassBeginInfo =
          zero
            { Vk.renderPass = renderPass
            , Vk.framebuffer = framebuffer
            , Vk.renderArea = Vk.Rect2D zero (Vk.Extent2D width height)
            , Vk.clearValues = [Vk.Color (Vk.Float32 0.1 0.1 0.1 1)]
            }
    Vk.cmdUseRenderPass commandBuffer renderPassBeginInfo Vk.SUBPASS_CONTENTS_INLINE do
      Vk.cmdBindPipeline
        commandBuffer
        Vk.PIPELINE_BIND_POINT_GRAPHICS
        graphicsPipeline
      Vk.cmdDraw commandBuffer 3 1 0 0

    -- Transition render target to transfer source
    Vk.cmdPipelineBarrier
      commandBuffer
      Vk.PIPELINE_STAGE_ALL_GRAPHICS_BIT
      Vk.PIPELINE_STAGE_TRANSFER_BIT
      zero
      []
      []
      [ SomeStruct
          zero
            { Vk.srcAccessMask = Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT
            , Vk.dstAccessMask = Vk.ACCESS_TRANSFER_READ_BIT
            , Vk.oldLayout = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
            , Vk.newLayout = Vk.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
            , Vk.image = image
            , Vk.subresourceRange = imageSubresourceRange
            }
      ]

    -- Transition cpu image to transfer dest
    Vk.cmdPipelineBarrier
      commandBuffer
      Vk.PIPELINE_STAGE_TOP_OF_PIPE_BIT
      Vk.PIPELINE_STAGE_TRANSFER_BIT
      zero
      []
      []
      [ SomeStruct
          zero
            { Vk.srcAccessMask = zero
            , Vk.dstAccessMask = Vk.ACCESS_TRANSFER_WRITE_BIT
            , Vk.oldLayout = Vk.IMAGE_LAYOUT_UNDEFINED
            , Vk.newLayout = Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
            , Vk.image = cpuImage
            , Vk.subresourceRange = imageSubresourceRange
            }
      ]

    -- Copy the image
    Vk.cmdCopyImage
      commandBuffer
      image
      Vk.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
      cpuImage
      Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
      [ Vk.ImageCopy
          { srcSubresource =
              Vk.ImageSubresourceLayers
                { Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT
                , Vk.mipLevel = 0
                , Vk.baseArrayLayer = 0
                , Vk.layerCount = 1
                }
          , srcOffset = Vk.Offset3D 0 0 0
          , dstSubresource =
              Vk.ImageSubresourceLayers
                { Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT
                , Vk.mipLevel = 0
                , Vk.baseArrayLayer = 0
                , Vk.layerCount = 1
                }
          , dstOffset = Vk.Offset3D 0 0 0
          , extent = Vk.Extent3D width height 1
          }
      ]

    -- Transition cpu image to LAYOUT_GENERAL for reading
    Vk.cmdPipelineBarrier
      commandBuffer
      Vk.PIPELINE_STAGE_TRANSFER_BIT
      Vk.PIPELINE_STAGE_HOST_BIT
      zero
      []
      []
      [ SomeStruct
          zero
            { Vk.srcAccessMask = Vk.ACCESS_TRANSFER_WRITE_BIT
            , Vk.dstAccessMask = Vk.ACCESS_HOST_READ_BIT
            , Vk.oldLayout = Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
            , Vk.newLayout = Vk.IMAGE_LAYOUT_GENERAL
            , Vk.image = cpuImage
            , Vk.subresourceRange = imageSubresourceRange
            }
      ]

  -- Create a fence so we can know when render is finished
  (_, fence) <- Vk.withFence dev zero Nothing allocate

  -- Submit the command buffer and wait for it to execute
  let submitInfo =
        zero
          { Vk.waitSemaphores = []
          , Vk.waitDstStageMask = []
          , Vk.commandBuffers = [Vk.commandBufferHandle commandBuffer]
          , Vk.signalSemaphores = []
          }
  graphicsQueue <- Vk.getDeviceQueue dev graphicsQueueFamilyIndex 0
  Vk.queueSubmit graphicsQueue [SomeStruct submitInfo] fence
  let fenceTimeout = 1e9 -- 1 second
  Vk.waitForFences dev [fence] True fenceTimeout >>= \case
    Vk.TIMEOUT -> throwString "Timed out waiting for image render and copy"
    _ -> pure ()

  -- If the cpu image allocation is not HOST_COHERENT this will ensure the
  -- changes are present on the CPU.
  VMA.invalidateAllocation allocator cpuImageAllocation 0 Vk.WHOLE_SIZE

  -- Find the image layout and read it into a JuicyPixels Image
  cpuImageLayout <-
    Image.getImageSubresourceLayout
      dev
      cpuImage
      Vk.ImageSubresource
        { Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT
        , Vk.mipLevel = 0
        , Vk.arrayLayer = 0
        }

  let
    pixelAddr :: Int -> Int -> Ptr Word32
    pixelAddr x y =
      plusPtr
        (VMA.mappedData cpuImageAllocationInfo)
        ( fromIntegral (Image.offset cpuImageLayout)
            + (y * fromIntegral (Image.rowPitch cpuImageLayout))
            + (x * sizeOf (0 :: Word32))
        )
  liftIO $
    JP.withImage
      width
      height
      (\x y -> JP.unpackPixel @JP.PixelRGBA8 <$> peek (pixelAddr x y))

-- | Create a vertex and fragment shader which render a colored triangle
createShaders
  :: Vk.Device
  -> ResourceT IO (V.Vector (SomeStruct Vk.PipelineShaderStageCreateInfo))
createShaders dev = do
  let
    fragCode =
      [frag|
        #version 450
        #extension GL_ARB_separate_shader_objects : enable

        layout(location = 0) in vec3 fragColor;

        layout(location = 0) out vec4 outColor;

        void main() {
            outColor = vec4(fragColor, 1.0);
        }
      |]
    vertCode =
      [vert|
        #version 450
        #extension GL_ARB_separate_shader_objects : enable

        layout(location = 0) out vec3 fragColor;

        vec2 positions[3] = vec2[](
          vec2(0.0, -0.5),
          vec2(0.5, 0.5),
          vec2(-0.5, 0.5)
        );
        vec3 colors[3] = vec3[](
          vec3(1.0, 1.0, 0.0),
          vec3(0.0, 1.0, 1.0),
          vec3(1.0, 0.0, 1.0)
        );

        void main() {
          gl_Position = vec4(positions[gl_VertexIndex], 0.0, 1.0);
          fragColor = colors[gl_VertexIndex];
        }
      |]
  (_, fragModule) <- Vk.withShaderModule dev zero{Vk.code = fragCode} Nothing allocate
  (_, vertModule) <- Vk.withShaderModule dev zero{Vk.code = vertCode} Nothing allocate
  let
    vertShaderStageCreateInfo =
      zero
        { Vk.stage = Vk.SHADER_STAGE_VERTEX_BIT
        , Vk.module' = vertModule
        , Vk.name = "main"
        }
    fragShaderStageCreateInfo =
      zero
        { Vk.stage = Vk.SHADER_STAGE_FRAGMENT_BIT
        , Vk.module' = fragModule
        , Vk.name = "main"
        }
  pure
    [ SomeStruct vertShaderStageCreateInfo
    , SomeStruct fragShaderStageCreateInfo
    ]

----------------------------------------------------------------
-- Initialization
----------------------------------------------------------------

myApiVersion :: Word32
myApiVersion = Vk.API_VERSION_1_0

-- | Create an instance with a debug messenger and validation layer.
createInstance :: (MonadResource m) => m Vk.Instance
createInstance = do
  inst <-
    Init.withInstance
      (Just zero{Vk.applicationName = Nothing, Vk.apiVersion = myApiVersion})
      [ RequireInstanceExtension
          { instanceExtensionLayerName = Nothing
          , instanceExtensionName = EXT_DEBUG_UTILS_EXTENSION_NAME
          , instanceExtensionMinVersion = minBound
          }
      ]
      []
  let debugMessengerCreateInfo =
        zero
          { messageSeverity =
              DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
                .|. DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
          , messageType =
              DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
                .|. DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
                .|. DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
          , pfnUserCallback = debugCallbackPtr
          }
  _ <- withDebugUtilsMessengerEXT inst debugMessengerCreateInfo Nothing allocate
  pure inst

createDevice
  :: (MonadResource m, MonadThrow m)
  => Vk.Instance
  -> m (Vk.PhysicalDevice, Word32, Vk.Device)
createDevice inst = do
  mPd <- pickPhysicalDevice inst hasGraphicsQueue id
  (_, phys) <- maybe (throwString "Unable to find appropriate PhysicalDevice") pure mPd
  sayErr . ("Using device: " <>) =<< physicalDeviceName phys

  mAssign <- assignQueues phys (Identity (QueueSpec 1 (\_ q -> pure (isGraphicsQueueFamily q))))
  (qInfos, getQs) <-
    maybe
      (throwString "Unable to assign graphics queue")
      pure
      mAssign
  dev <-
    createDeviceFromRequirements
      []
      []
      phys
      zero{Vk.queueCreateInfos = SomeStruct <$> qInfos}
  Identity (QueueFamilyIndex graphicsFamilyIdx, _q) <- liftIO (getQs dev)
  pure (phys, graphicsFamilyIdx, dev)
  where
    hasGraphicsQueue :: (MonadIO m) => Vk.PhysicalDevice -> m (Maybe Word64)
    hasGraphicsQueue phys = do
      qProps <- Vk.getPhysicalDeviceQueueFamilyProperties phys
      if V.any isGraphicsQueueFamily qProps
        then do
          heaps <- Vk.memoryHeaps <$> Vk.getPhysicalDeviceMemoryProperties phys
          pure (Just (sum (DI.size <$> heaps)))
        else pure Nothing
