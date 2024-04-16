{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main
  ( main
  )
where

import           AutoApply
import qualified Codec.Picture                 as JP
import qualified Codec.Picture.Types           as JP
import           Control.Exception.Safe
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe      ( MaybeT(..) )
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import           Data.Bits
import qualified Data.ByteString.Lazy          as BSL
import           Data.Foldable
import           Data.List                      ( partition )
import           Data.Maybe                     ( catMaybes )
import           Data.Ord                       ( comparing )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( decodeUtf8 )
import qualified Data.Vector                   as V
import           Data.Word
import           Foreign.Ptr
import           Foreign.Storable               ( peek
                                                , sizeOf
                                                )
import           Say

#if defined(RENDERDOC)
import           Control.Monad                  ( when )
import qualified Data.Map.Strict               as Map
import qualified Language.C.Inline             as C
import qualified Language.C.Inline.Context     as C
import qualified Language.C.Types              as C
#endif

import           Vulkan.CStruct.Extends
import           Vulkan.Core10                 as Vk
                                         hiding ( withImage )
import qualified Vulkan.Core10.DeviceInitialization as DI
import qualified Vulkan.Core10.Image           as SL
import           Vulkan.Dynamic                 ( DeviceCmds
                                                  ( DeviceCmds
                                                  , pVkGetDeviceProcAddr
                                                  )
                                                , InstanceCmds
                                                  ( InstanceCmds
                                                  , pVkGetInstanceProcAddr
                                                  )
                                                )
import           Vulkan.Extensions.VK_EXT_debug_utils
import           Vulkan.Extensions.VK_EXT_validation_features
import           Vulkan.Utils.Debug
import           Vulkan.Utils.ShaderQQ.GLSL.Glslang
import           Vulkan.Zero
import           VulkanMemoryAllocator         as VMA
                                         hiding ( getPhysicalDeviceProperties )

#if defined(RENDERDOC)
data RENDERDOC_API_1_1_2
C.context
  (C.baseCtx <> mempty
    { C.ctxTypesTable = Map.fromList
      [(C.TypeName "RENDERDOC_API_1_1_2", [t|RENDERDOC_API_1_1_2|])]
    }
  )

C.include "<renderdoc_app.h>"
C.include "<dlfcn.h>"
C.include "<assert.h>"
C.include "<stddef.h>"
#endif

----------------------------------------------------------------
-- Define the monad in which most of the program will run
----------------------------------------------------------------

-- | @V@ keeps track of a bunch of "global" handles and performs resource
-- management.
newtype V a = V { unV :: ReaderT GlobalHandles (ResourceT IO) a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadFail
                   , MonadThrow
                   , MonadCatch
                   , MonadMask
                   , MonadIO
                   , MonadResource
                   )

runV
  :: Instance
  -> PhysicalDevice
  -> Word32
  -> Device
  -> Allocator
  -> V a
  -> ResourceT IO a
runV ghInstance ghPhysicalDevice ghGraphicsQueueFamilyIndex ghDevice ghAllocator
  = flip runReaderT GlobalHandles { .. } . unV

data GlobalHandles = GlobalHandles
  { ghInstance                 :: Instance
  , ghPhysicalDevice           :: PhysicalDevice
  , ghDevice                   :: Device
  , ghAllocator                :: Allocator
  , ghGraphicsQueueFamilyIndex :: Word32
  }

-- Getters for global handles

getInstance :: V Instance
getInstance = V (asks ghInstance)

getGraphicsQueueFamilyIndex :: V Word32
getGraphicsQueueFamilyIndex = V (asks ghGraphicsQueueFamilyIndex)

getPhysicalDevice :: V PhysicalDevice
getPhysicalDevice = V (asks ghPhysicalDevice)

getDevice :: V Device
getDevice = V (asks ghDevice)

getAllocator :: V Allocator
getAllocator = V (asks ghAllocator)

noAllocationCallbacks :: Maybe AllocationCallbacks
noAllocationCallbacks = Nothing

--
-- Wrap a bunch of Vulkan commands so that they automatically pull global
-- handles from 'V'
--
-- Wrapped functions are suffixed with "'"
--
autoapplyDecs
  (<> "'")
  [ 'getDevice
  , 'getPhysicalDevice
  , 'getInstance
  , 'getAllocator
  , 'noAllocationCallbacks
  ]
  ['allocate]
  [ 'invalidateAllocation
  , 'withImage
  , 'deviceWaitIdle
  , 'getDeviceQueue
  , 'getImageSubresourceLayout
  , 'waitForFences
  , 'withCommandBuffers
  , 'withCommandPool
  , 'withFence
  , 'withFramebuffer
  , 'withGraphicsPipelines
  , 'withImageView
  , 'withInstance
  , 'withPipelineLayout
  , 'withRenderPass
  , 'withShaderModule
  , 'nameObject
  ]

----------------------------------------------------------------
-- The program
----------------------------------------------------------------

main :: IO ()
main = runResourceT $ do
  -- Create Instance, PhysicalDevice, Device and Allocator
  inst             <- Main.createInstance
  (phys, pdi, dev) <- Main.createDevice inst
  (_, allocator)   <- withAllocator
    zero
      { flags            = zero
      , physicalDevice   = physicalDeviceHandle phys
      , device           = deviceHandle dev
      , instance'        = instanceHandle inst
      , vulkanApiVersion = myApiVersion
      , vulkanFunctions  = Just $ case inst of
        Instance _ InstanceCmds {..} -> case dev of
          Device _ DeviceCmds {..} -> zero
            { vkGetInstanceProcAddr = castFunPtr pVkGetInstanceProcAddr
            , vkGetDeviceProcAddr   = castFunPtr pVkGetDeviceProcAddr
            }
      }
    allocate

#if defined(RENDERDOC)
  -- We need to mark the beginning and end of the capture explicitly as this
  -- application doesn't present frames with a swapchain which is the trigger
  -- RenderDoc usually uses.
  rdoc_api <- liftIO [C.block| RENDERDOC_API_1_1_2* {
    RENDERDOC_API_1_1_2* rdoc_api = NULL;
    void* mod = dlopen("librenderdoc.so", RTLD_NOW | RTLD_NOLOAD);

    if (mod) {
      pRENDERDOC_GetAPI RENDERDOC_GetAPI =
          (pRENDERDOC_GetAPI)dlsym(mod, "RENDERDOC_GetAPI");
      int ret =
          RENDERDOC_GetAPI(eRENDERDOC_API_Version_1_1_2, (void **)&rdoc_api);
      assert(ret == 1);
    };
    return rdoc_api;
  }|]
  when (rdoc_api /= nullPtr) $
    sayErr "Running under RenderDoc"

  let rdBegin = liftIO [C.exp| void { if($(RENDERDOC_API_1_1_2* rdoc_api)) $(RENDERDOC_API_1_1_2* rdoc_api)->StartFrameCapture(NULL, NULL); } |]
      rdEnd = liftIO [C.exp| void { if($(RENDERDOC_API_1_1_2* rdoc_api)) $(RENDERDOC_API_1_1_2* rdoc_api)->EndFrameCapture(NULL, NULL); } |]
  _ <- allocate rdBegin (const rdEnd)
#endif

  -- Run our application
  runV inst phys (pdiGraphicsQueueFamilyIndex pdi) dev allocator
    . (`finally` deviceWaitIdle')
    $ do
        image <- render
        let filename = "triangle.png"
        sayErr $ "Writing " <> filename
        liftIO $ BSL.writeFile filename (JP.encodePng image)

-- | This function renders a triangle and reads the image on the CPU
--
-- It:
-- - Initializes two images
--   - A GPU image which is used as the framebuffer
--   - A CPU image which is copied to and read on the CPU
-- - Creates a RenderPass with a single subpass
-- - Creates a graphics pipeline
-- - Creates command pool and allocated a single command buffer
-- - Uses the command buffer to
--   - Render into the GPU image
--   - Issue a barrier to make it safe to transfer from the GPU image
--   - Issue a barrier to make it safe to write to the CPU image
--   - Perform an image copy
--   - Issue a barrier to make it safe to read the CPU image on the host
-- - Submits and waits for the command buffer to finish executing
-- - Invalidates the CPU image allocation (if it isn't HOST_COHERENT)
-- - Copies the data from the CPU image and returns it
render :: V (JP.Image JP.PixelRGBA8)
render = do
  -- Some things to reuse
  let imageFormat = FORMAT_R8G8B8A8_UNORM
      width       = 256
      height      = 256

  -- Create an image to be our render target
  let
    imageCreateInfo = zero
      { imageType     = IMAGE_TYPE_2D
      , format        = imageFormat
      , extent        = Extent3D width height 1
      , mipLevels     = 1
      , arrayLayers   = 1
      , samples       = SAMPLE_COUNT_1_BIT
      , tiling        = IMAGE_TILING_OPTIMAL
      , usage         = IMAGE_USAGE_COLOR_ATTACHMENT_BIT
                          .|. IMAGE_USAGE_TRANSFER_SRC_BIT
      , initialLayout = IMAGE_LAYOUT_UNDEFINED
      }
    allocationCreateInfo :: AllocationCreateInfo
    allocationCreateInfo = zero { flags = ALLOCATION_CREATE_MAPPED_BIT
                                , usage = MEMORY_USAGE_GPU_ONLY
                                }
  -- Allocate the image with VMA
  (_, (image, _, _)) <- withImage' imageCreateInfo allocationCreateInfo
  nameObject' image "GPU side image"

  -- Create an image to read on the CPU
  let cpuImageCreateInfo = zero { imageType     = IMAGE_TYPE_2D
                                , format        = imageFormat
                                , extent        = Extent3D width height 1
                                , mipLevels     = 1
                                , arrayLayers   = 1
                                , samples       = SAMPLE_COUNT_1_BIT
                                , tiling        = IMAGE_TILING_LINEAR
                                , usage         = IMAGE_USAGE_TRANSFER_DST_BIT
                                , initialLayout = IMAGE_LAYOUT_UNDEFINED
                                }
      cpuAllocationCreateInfo :: AllocationCreateInfo
      cpuAllocationCreateInfo = zero { flags = ALLOCATION_CREATE_MAPPED_BIT
                                     , usage = MEMORY_USAGE_GPU_TO_CPU
                                     }
  (_, (cpuImage, cpuImageAllocation, cpuImageAllocationInfo)) <- withImage'
    cpuImageCreateInfo
    cpuAllocationCreateInfo
  nameObject' cpuImage "CPU side image"

  -- Create an image view
  let imageSubresourceRange = ImageSubresourceRange
        { aspectMask     = IMAGE_ASPECT_COLOR_BIT
        , baseMipLevel   = 0
        , levelCount     = 1
        , baseArrayLayer = 0
        , layerCount     = 1
        }
      imageViewCreateInfo = zero
        { image            = image
        , viewType         = IMAGE_VIEW_TYPE_2D
        , format           = imageFormat
        , components       = ComponentMapping COMPONENT_SWIZZLE_IDENTITY
                                              COMPONENT_SWIZZLE_IDENTITY
                                              COMPONENT_SWIZZLE_IDENTITY
                                              COMPONENT_SWIZZLE_IDENTITY
        , subresourceRange = imageSubresourceRange
        }
  (_, imageView) <- withImageView' imageViewCreateInfo

  -- Create a renderpass with a single subpass
  let
    attachmentDescription :: AttachmentDescription
    attachmentDescription = zero
      { format         = imageFormat
      , samples        = SAMPLE_COUNT_1_BIT
      , loadOp         = ATTACHMENT_LOAD_OP_CLEAR
      , storeOp        = ATTACHMENT_STORE_OP_STORE
      , stencilLoadOp  = ATTACHMENT_LOAD_OP_DONT_CARE
      , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
      , initialLayout  = IMAGE_LAYOUT_UNDEFINED
      , finalLayout    = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
      }
    subpass :: SubpassDescription
    subpass = zero
      { pipelineBindPoint = PIPELINE_BIND_POINT_GRAPHICS
      , colorAttachments  =
        [ zero { attachment = 0
               , layout     = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
               }
        ]
      }
    subpassDependency :: SubpassDependency
    subpassDependency = zero
      { srcSubpass    = SUBPASS_EXTERNAL
      , dstSubpass    = 0
      , srcStageMask  = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
      , srcAccessMask = zero
      , dstStageMask  = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
      , dstAccessMask = ACCESS_COLOR_ATTACHMENT_READ_BIT
                          .|. ACCESS_COLOR_ATTACHMENT_WRITE_BIT
      }
  (_, renderPass) <- withRenderPass' zero
    { attachments  = [attachmentDescription]
    , subpasses    = [subpass]
    , dependencies = [subpassDependency]
    }

  -- Create a framebuffer
  let framebufferCreateInfo :: FramebufferCreateInfo '[]
      framebufferCreateInfo = zero { renderPass  = renderPass
                                   , attachments = [imageView]
                                   , width       = width
                                   , height      = height
                                   , layers      = 1
                                   }
  (_, framebuffer)    <- withFramebuffer' framebufferCreateInfo

  -- Create the most vanilla rendering pipeline
  shaderStages        <- createShaders
  (_, pipelineLayout) <- withPipelineLayout' zero
  let
    pipelineCreateInfo :: GraphicsPipelineCreateInfo '[]
    pipelineCreateInfo = zero
      { stages             = shaderStages
      , vertexInputState   = Just zero
      , inputAssemblyState = Just zero
                               { topology = PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
                               , primitiveRestartEnable = False
                               }
      , viewportState      = Just . SomeStruct $ zero
                               { viewports =
                                 [ Viewport { x = 0
                                            , y = 0
                                            , width = realToFrac (width :: Word32)
                                            , height = realToFrac (height :: Word32)
                                            , minDepth = 0
                                            , maxDepth = 1
                                            }
                                 ]
                               , scissors = [ Rect2D { offset = Offset2D 0 0
                                                     , extent = Extent2D width height
                                                     }
                                            ]
                               }
      , rasterizationState = Just . SomeStruct $ zero
                               { depthClampEnable        = False
                               , rasterizerDiscardEnable = False
                               , lineWidth               = 1
                               , polygonMode             = POLYGON_MODE_FILL
                               , cullMode                = CULL_MODE_NONE
                               , frontFace               = FRONT_FACE_CLOCKWISE
                               , depthBiasEnable         = False
                               }
      , multisampleState   = Just . SomeStruct $ zero
                               { sampleShadingEnable  = False
                               , rasterizationSamples = SAMPLE_COUNT_1_BIT
                               , minSampleShading     = 1
                               , sampleMask           = [maxBound]
                               }
      , depthStencilState  = Nothing
      , colorBlendState    = Just . SomeStruct $ zero
                               { logicOpEnable = False
                               , attachments   = [ zero
                                                     { colorWriteMask =
                                                       COLOR_COMPONENT_R_BIT
                                                       .|. COLOR_COMPONENT_G_BIT
                                                       .|. COLOR_COMPONENT_B_BIT
                                                       .|. COLOR_COMPONENT_A_BIT
                                                     , blendEnable    = False
                                                     }
                                                 ]
                               }
      , dynamicState       = Nothing
      , layout             = pipelineLayout
      , renderPass         = renderPass
      , subpass            = 0
      , basePipelineHandle = zero
      }
  (_, (_, [graphicsPipeline])) <- withGraphicsPipelines'
    zero
    [SomeStruct pipelineCreateInfo]

  -- Create a command buffer
  graphicsQueueFamilyIndex <- getGraphicsQueueFamilyIndex
  let commandPoolCreateInfo :: CommandPoolCreateInfo
      commandPoolCreateInfo =
        zero { queueFamilyIndex = graphicsQueueFamilyIndex }
  (_, commandPool) <- withCommandPool' commandPoolCreateInfo
  let commandBufferAllocateInfo = zero { commandPool = commandPool
                                       , level = COMMAND_BUFFER_LEVEL_PRIMARY
                                       , commandBufferCount = 1
                                       }
  (_, [commandBuffer]) <- withCommandBuffers' commandBufferAllocateInfo

  -- Fill command buffer
  --
  -- - Execute the renderpass
  -- - Transition the images to be able to perform the copy
  -- - Copy the image to CPU mapped memory
  useCommandBuffer commandBuffer
                   zero { flags = COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT }
    $ do
        let renderPassBeginInfo = zero
              { renderPass  = renderPass
              , framebuffer = framebuffer
              , renderArea  = Rect2D zero (Extent2D width height)
              , clearValues = [Color (Float32 0.1 0.1 0.1 1)]
              }
        cmdUseRenderPass commandBuffer
                         renderPassBeginInfo
                         SUBPASS_CONTENTS_INLINE
          $ do
              cmdBindPipeline commandBuffer
                              PIPELINE_BIND_POINT_GRAPHICS
                              graphicsPipeline
              cmdDraw commandBuffer 3 1 0 0

        -- Transition render target to transfer source
        cmdPipelineBarrier
          commandBuffer
          PIPELINE_STAGE_ALL_GRAPHICS_BIT
          PIPELINE_STAGE_TRANSFER_BIT
          zero
          []
          []
          [ SomeStruct zero { srcAccessMask = ACCESS_COLOR_ATTACHMENT_WRITE_BIT
                            , dstAccessMask = ACCESS_TRANSFER_READ_BIT
                            , oldLayout = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                            , newLayout = IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
                            , image = image
                            , subresourceRange = imageSubresourceRange
                            }
          ]

        -- Transition cpu image to transfer dest
        cmdPipelineBarrier
          commandBuffer
          PIPELINE_STAGE_TOP_OF_PIPE_BIT
          PIPELINE_STAGE_TRANSFER_BIT
          zero
          []
          []
          [ SomeStruct zero { srcAccessMask    = zero
                            , dstAccessMask    = ACCESS_TRANSFER_WRITE_BIT
                            , oldLayout        = IMAGE_LAYOUT_UNDEFINED
                            , newLayout = IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
                            , image            = cpuImage
                            , subresourceRange = imageSubresourceRange
                            }
          ]

        -- Copy the image
        cmdCopyImage
          commandBuffer
          image
          IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
          cpuImage
          IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
          [ ImageCopy
              { srcSubresource = ImageSubresourceLayers
                                   { aspectMask     = IMAGE_ASPECT_COLOR_BIT
                                   , mipLevel       = 0
                                   , baseArrayLayer = 0
                                   , layerCount     = 1
                                   }
              , srcOffset      = Offset3D 0 0 0
              , dstSubresource = ImageSubresourceLayers
                                   { aspectMask     = IMAGE_ASPECT_COLOR_BIT
                                   , mipLevel       = 0
                                   , baseArrayLayer = 0
                                   , layerCount     = 1
                                   }
              , dstOffset      = Offset3D 0 0 0
              , extent         = Extent3D width height 1
              }
          ]

        -- Transition cpu image to LAYOUT_GENERAL for reading
        cmdPipelineBarrier
          commandBuffer
          PIPELINE_STAGE_TRANSFER_BIT
          PIPELINE_STAGE_HOST_BIT
          zero
          []
          []
          [ SomeStruct zero { srcAccessMask    = ACCESS_TRANSFER_WRITE_BIT
                            , dstAccessMask    = ACCESS_HOST_READ_BIT
                            , oldLayout = IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
                            , newLayout        = IMAGE_LAYOUT_GENERAL
                            , image            = cpuImage
                            , subresourceRange = imageSubresourceRange
                            }
          ]

  -- Create a fence so we can know when render is finished
  (_, fence) <- withFence' zero

  -- Submit the command buffer and wait for it to execute
  let submitInfo = zero { waitSemaphores   = []
                        , waitDstStageMask = []
                        , commandBuffers   = [commandBufferHandle commandBuffer]
                        , signalSemaphores = []
                        }
  graphicsQueue <- getDeviceQueue' graphicsQueueFamilyIndex 0
  queueSubmit graphicsQueue [SomeStruct submitInfo] fence
  let fenceTimeout = 1e9 -- 1 second
  waitForFences' [fence] True fenceTimeout >>= \case
    TIMEOUT -> throwString "Timed out waiting for image render and copy"
    _       -> pure ()

  -- If the cpu image allocation is not HOST_COHERENT this will ensure the
  -- changes are present on the CPU.
  invalidateAllocation' cpuImageAllocation 0 WHOLE_SIZE

  -- Find the image layout and read it into a JuicyPixels Image
  cpuImageLayout <- getImageSubresourceLayout'
    cpuImage
    ImageSubresource { aspectMask = IMAGE_ASPECT_COLOR_BIT
                     , mipLevel   = 0
                     , arrayLayer = 0
                     }

  let pixelAddr :: Int -> Int -> Ptr Word32
      pixelAddr x y = plusPtr
        (mappedData cpuImageAllocationInfo)
        ( fromIntegral (SL.offset cpuImageLayout)
        + (y * fromIntegral (rowPitch cpuImageLayout))
        + (x * sizeOf (0 :: Word32))
        )
  liftIO $ JP.withImage
    width
    height
    (\x y -> JP.unpackPixel @JP.PixelRGBA8 <$> peek (pixelAddr x y))

-- | Create a vertex and fragment shader which render a colored triangle
createShaders :: V (V.Vector (SomeStruct PipelineShaderStageCreateInfo))
createShaders = do
  let fragCode = [frag|
        #version 450
        #extension GL_ARB_separate_shader_objects : enable

        layout(location = 0) in vec3 fragColor;

        layout(location = 0) out vec4 outColor;

        void main() {
            outColor = vec4(fragColor, 1.0);
        }
      |]
      vertCode = [vert|
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
  (_, fragModule) <- withShaderModule' zero { code = fragCode }
  (_, vertModule) <- withShaderModule' zero { code = vertCode }
  let vertShaderStageCreateInfo = zero { stage   = SHADER_STAGE_VERTEX_BIT
                                       , module' = vertModule
                                       , name    = "main"
                                       }
      fragShaderStageCreateInfo = zero { stage   = SHADER_STAGE_FRAGMENT_BIT
                                       , module' = fragModule
                                       , name    = "main"
                                       }
  pure
    [SomeStruct vertShaderStageCreateInfo, SomeStruct fragShaderStageCreateInfo]


----------------------------------------------------------------
-- Initialization
----------------------------------------------------------------

myApiVersion :: Word32
myApiVersion = API_VERSION_1_0

-- | Create an instance with a debug messenger
createInstance :: MonadResource m => m Instance
createInstance = do
  availableExtensionNames <-
    toList
    .   fmap extensionName
    .   snd
    <$> enumerateInstanceExtensionProperties Nothing
  availableLayerNames <-
    toList . fmap layerName . snd <$> enumerateInstanceLayerProperties

  let requiredLayers     = []
      optionalLayers     = ["VK_LAYER_KHRONOS_validation"]
      requiredExtensions = [EXT_DEBUG_UTILS_EXTENSION_NAME]
      optionalExtensions = [EXT_VALIDATION_FEATURES_EXTENSION_NAME]

  extensions <- partitionOptReq "extension"
                                availableExtensionNames
                                optionalExtensions
                                requiredExtensions
  layers <- partitionOptReq "layer"
                            availableLayerNames
                            optionalLayers
                            requiredLayers

  let debugMessengerCreateInfo = zero
        { messageSeverity = DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
                              .|. DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
        , messageType     = DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
                            .|. DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
                            .|. DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
        , pfnUserCallback = debugCallbackPtr
        }
      instanceCreateInfo =
        zero
            { applicationInfo       = Just zero { applicationName = Nothing
                                                , apiVersion      = myApiVersion
                                                }
            , enabledLayerNames     = V.fromList layers
            , enabledExtensionNames = V.fromList extensions
            }
          ::& debugMessengerCreateInfo
          :&  ValidationFeaturesEXT
                [VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT]
                []
          :&  ()
  (_, inst) <- withInstance' instanceCreateInfo
  _ <- withDebugUtilsMessengerEXT inst debugMessengerCreateInfo Nothing allocate
  pure inst

createDevice
  :: (MonadResource m, MonadThrow m)
  => Instance
  -> m (PhysicalDevice, PhysicalDeviceInfo, Device)
createDevice inst = do
  (pdi, phys) <- pickPhysicalDevice inst physicalDeviceInfo
  sayErr . ("Using device: " <>) =<< physicalDeviceName phys

  let deviceCreateInfo = zero
        { queueCreateInfos =
          [ SomeStruct zero { queueFamilyIndex = pdiGraphicsQueueFamilyIndex pdi
                            , queuePriorities  = [1]
                            }
          ]
        }

  (_, dev) <- withDevice phys deviceCreateInfo Nothing allocate
  pure (phys, pdi, dev)

----------------------------------------------------------------
-- Physical device tools
----------------------------------------------------------------

-- | Get a single PhysicalDevice deciding with a scoring function
pickPhysicalDevice
  :: (MonadIO m, MonadThrow m, Ord a)
  => Instance
  -> (PhysicalDevice -> m (Maybe a))
  -- ^ Some "score" for a PhysicalDevice, Nothing if it is not to be chosen.
  -> m (a, PhysicalDevice)
pickPhysicalDevice inst devScore = do
  (_, devs) <- enumeratePhysicalDevices inst
  scores    <- catMaybes
    <$> sequence [ fmap (, d) <$> devScore d | d <- toList devs ]
  case scores of
    [] -> throwString "Unable to find appropriate PhysicalDevice"
    _  -> pure (maximumBy (comparing fst) scores)

-- | The Ord instance prioritises devices with more memory
data PhysicalDeviceInfo = PhysicalDeviceInfo
  { pdiTotalMemory              :: Word64
  , pdiGraphicsQueueFamilyIndex :: Word32
  }
  deriving (Eq, Ord)

physicalDeviceInfo
  :: MonadIO m => PhysicalDevice -> m (Maybe PhysicalDeviceInfo)
physicalDeviceInfo phys = runMaybeT $ do
  pdiTotalMemory <- do
    heaps <- memoryHeaps <$> getPhysicalDeviceMemoryProperties phys
    pure $ sum (DI.size <$> heaps)
  pdiGraphicsQueueFamilyIndex <- do
    queueFamilyProperties <- getPhysicalDeviceQueueFamilyProperties phys
    let isGraphicsQueue q =
          (QUEUE_GRAPHICS_BIT .&&. queueFlags q) && (queueCount q > 0)
        graphicsQueueIndices = fromIntegral . fst <$> V.filter
          (isGraphicsQueue . snd)
          (V.indexed queueFamilyProperties)
    MaybeT (pure $ graphicsQueueIndices V.!? 0)
  pure PhysicalDeviceInfo { .. }

physicalDeviceName :: MonadIO m => PhysicalDevice -> m Text
physicalDeviceName phys = do
  props <- getPhysicalDeviceProperties phys
  pure $ decodeUtf8 (deviceName props)

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

partitionOptReq
  :: (Show a, Eq a, MonadIO m) => Text -> [a] -> [a] -> [a] -> m [a]
partitionOptReq type' available optional required = do
  let (optHave, optMissing) = partition (`elem` available) optional
      (reqHave, reqMissing) = partition (`elem` available) required
      tShow                 = T.pack . show
  for_ optMissing
    $ \n -> sayErr $ "Missing optional " <> type' <> ": " <> tShow n
  case reqMissing of
    []  -> pure ()
    [x] -> sayErr $ "Missing required " <> type' <> ": " <> tShow x
    xs  -> sayErr $ "Missing required " <> type' <> "s: " <> tShow xs
  pure (reqHave <> optHave)

----------------------------------------------------------------
-- Bit utils
----------------------------------------------------------------

(.&&.) :: Bits a => a -> a -> Bool
x .&&. y = (/= zeroBits) (x .&. y)
