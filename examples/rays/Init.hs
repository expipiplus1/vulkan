{-# LANGUAGE QuasiQuotes #-}

module Init
  ( Init.createInstance
  , Init.createDevice
  , PhysicalDeviceInfo(..)
  , RTInfo(..)
  , createVMA
  ) where

import           Control.Applicative            ( empty )
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe      ( MaybeT(..) )
import           Control.Monad.Trans.Resource
import           Data.Foldable                  ( traverse_ )
import           Data.Vector                    ( Vector )
import           Data.Word
import qualified SDL.Video                     as SDL
import           Say
import           Utils                          ( noSuchThing
                                                )
import           VkResources                    ( Queues(..) )
import qualified Vma
import           Vulkan.CStruct.Extends
import           Vulkan.Core10                 as Vk
                                         hiding ( withBuffer
                                                , withImage
                                                )
import qualified Vulkan.Core10                 as MemoryHeap (MemoryHeap(..))
import           Vulkan.Core11                  ( pattern API_VERSION_1_1 )
import           Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address
import           Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore
                                                ( PhysicalDeviceTimelineSemaphoreFeatures(..)
                                                )
import           Vulkan.Extensions.VK_EXT_debug_utils
                                                ( pattern EXT_DEBUG_UTILS_EXTENSION_NAME )
import           Vulkan.Extensions.VK_KHR_acceleration_structure
import           Vulkan.Extensions.VK_KHR_get_physical_device_properties2
import           Vulkan.Extensions.VK_KHR_ray_tracing_pipeline
import           Vulkan.Extensions.VK_KHR_surface
import           Vulkan.Requirement
import qualified Vulkan.Utils.Init.SDL2        as VkInit
import           Vulkan.Utils.Initialization
import           Vulkan.Utils.QueueAssignment
import           Vulkan.Utils.Requirements
import           Vulkan.Utils.Requirements.TH   ( reqs )
import           Vulkan.Zero
import           VulkanMemoryAllocator          ( Allocator
                                                , AllocatorCreateFlagBits(..)
                                                )
import           Window.SDL2

myApiVersion :: Word32
myApiVersion = API_VERSION_1_1

----------------------------------------------------------------
-- Instance Creation
----------------------------------------------------------------

createInstance :: MonadResource m => SDL.Window -> m Instance
createInstance win = VkInit.withInstance
  win
  (Just zero { applicationName = Nothing, apiVersion = myApiVersion })
  [ RequireInstanceExtension
      Nothing
      KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
      minBound
  -- Required so the @nameObject@ calls scattered through the example can load
  -- their function pointer; we don't enable the messenger though.
  , RequireInstanceExtension Nothing EXT_DEBUG_UTILS_EXTENSION_NAME minBound
  ]
  []

----------------------------------------------------------------
-- Device creation
----------------------------------------------------------------

-- | Information for ray tracing (queried from device properties).
data RTInfo = RTInfo
  { rtiShaderGroupHandleSize    :: Word32
  , rtiShaderGroupBaseAlignment :: Word32
  }

createDevice
  :: forall m
   . (MonadResource m)
  => Instance
  -> SDL.Window
  -> m
       ( PhysicalDevice
       , PhysicalDeviceInfo
       , Device
       , Queues (QueueFamilyIndex, Queue)
       , SurfaceKHR
       )
createDevice inst win = do
  (_                    , surf) <- createSurface inst win

  ((pdi, SomeStruct dci), phys) <-
    maybe (noSuchThing "Unable to find appropriate PhysicalDevice") pure
      =<< pickPhysicalDevice inst (physicalDeviceInfo surf) (pdiScore . fst)
  sayErr . ("Using device: " <>) =<< physicalDeviceName phys

  (_, dev) <- withDevice phys dci Nothing allocate

  queues <- liftIO $ pdiGetQueues pdi dev

  pure (phys, pdi, dev, queues, surf)

deviceRequirements :: [DeviceRequirement]
deviceRequirements = [reqs|
    VK_KHR_swapchain

    VK_KHR_timeline_semaphore
    PhysicalDeviceTimelineSemaphoreFeatures.timelineSemaphore

    -- Ray tracing
    1.2.162
    PhysicalDeviceRayTracingPipelineFeaturesKHR.rayTracingPipeline
    PhysicalDeviceAccelerationStructureFeaturesKHR.accelerationStructure
    PhysicalDeviceBufferDeviceAddressFeatures.bufferDeviceAddress
    VK_KHR_ray_tracing_pipeline
    VK_KHR_acceleration_structure
    VK_EXT_descriptor_indexing
    VK_KHR_buffer_device_address
    VK_KHR_deferred_host_operations
    VK_KHR_get_memory_requirements2
    VK_KHR_maintenance3
    VK_KHR_pipeline_library
|]

----------------------------------------------------------------
-- Physical device tools
----------------------------------------------------------------

data PhysicalDeviceInfo = PhysicalDeviceInfo
  { pdiTotalMemory      :: Word64
  , pdiRTInfo           :: RTInfo
  , pdiQueueCreateInfos :: Vector (DeviceQueueCreateInfo '[])
  , pdiGetQueues        :: Device -> IO (Queues (QueueFamilyIndex, Queue))
  }

pdiScore :: PhysicalDeviceInfo -> Word64
pdiScore = pdiTotalMemory

physicalDeviceInfo
  :: MonadIO m
  => SurfaceKHR
  -> PhysicalDevice
  -> m (Maybe (PhysicalDeviceInfo, SomeStruct DeviceCreateInfo))
physicalDeviceInfo surf phys = runMaybeT $ do
  (mbDCI, rs, os)                     <- checkDeviceRequirements deviceRequirements [] phys zero
  traverse_ sayErrString (requirementReport rs os)
  SomeStruct dciNoQueues              <- maybe empty pure mbDCI

  (pdiQueueCreateInfos, pdiGetQueues) <- MaybeT
    $ assignQueues phys (queueRequirements phys surf)
  let dci =
        dciNoQueues { queueCreateInfos = SomeStruct <$> pdiQueueCreateInfos }

  pdiRTInfo      <- getDeviceRTProps phys

  pdiTotalMemory <- do
    heaps <- memoryHeaps <$> getPhysicalDeviceMemoryProperties phys
    pure $ sum (MemoryHeap.size <$> heaps)

  pure (PhysicalDeviceInfo { .. }, SomeStruct dci)

queueRequirements
  :: MonadIO m => PhysicalDevice -> SurfaceKHR -> Queues (QueueSpec m)
queueRequirements phys surf = Queues (QueueSpec 1 isGraphicsPresentQueue)
 where
  isGraphicsPresentQueue queueFamilyIndex queueFamilyProperties =
    (&& isGraphicsQueueFamily queueFamilyProperties)
      <$> isPresentQueueFamily phys surf queueFamilyIndex

getDeviceRTProps :: MonadIO m => PhysicalDevice -> m RTInfo
getDeviceRTProps phys = do
  props <- getPhysicalDeviceProperties2KHR phys
  let _ ::& PhysicalDeviceRayTracingPipelinePropertiesKHR {..} :& () = props
  pure RTInfo { rtiShaderGroupHandleSize    = shaderGroupHandleSize
              , rtiShaderGroupBaseAlignment = shaderGroupBaseAlignment
              }

----------------------------------------------------------------
-- VulkanMemoryAllocator
----------------------------------------------------------------

createVMA
  :: MonadResource m => Instance -> PhysicalDevice -> Device -> m Allocator
createVMA = Vma.createVMA ALLOCATOR_CREATE_BUFFER_DEVICE_ADDRESS_BIT myApiVersion
