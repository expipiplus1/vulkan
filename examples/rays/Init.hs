{-# LANGUAGE QuasiQuotes #-}

module Init
  ( Init.createInstance
  , Init.createDevice
  , PhysicalDeviceInfo(..)
  , createVMA
  , createCommandPools
  ) where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe      ( MaybeT(..) )
import           Control.Monad.Trans.Resource
import qualified Data.ByteString               as BS
import           Data.Foldable                  ( for_
                                                , traverse_
                                                )
import qualified Data.Vector                   as V
import           Data.Vector                    ( Vector )
import           Data.Word
import           GHC.IO.Exception               ( IOErrorType(NoSuchThing)
                                                , IOException(IOError)
                                                )
import           HasVulkan
import           MonadVulkan                    ( Queues(..)
                                                , RTInfo(..)
                                                , checkCommands
                                                )
import qualified SDL.Video                     as SDL
import qualified SDL.Video.Vulkan              as SDL
import           Say
import           UnliftIO.Exception
import           Vulkan.CStruct.Extends
import           Vulkan.Core10                 as Vk
                                         hiding ( withBuffer
                                                , withImage
                                                )
import           Vulkan.Core11                  ( pattern API_VERSION_1_1 )
import           Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address
import           Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore
                                                ( PhysicalDeviceTimelineSemaphoreFeatures(..)
                                                )
import           Vulkan.Extensions.VK_KHR_acceleration_structure
import           Vulkan.Extensions.VK_KHR_get_physical_device_properties2
import           Vulkan.Extensions.VK_KHR_ray_tracing_pipeline
import           Vulkan.Extensions.VK_KHR_surface
import           Vulkan.Requirement
import           Vulkan.Utils.Initialization
import           Vulkan.Utils.QueueAssignment
import           Vulkan.Utils.Requirements
import           Vulkan.Utils.Requirements.TH   ( reqs )
import           Vulkan.Zero
import           VulkanMemoryAllocator          ( Allocator
                                                , AllocatorCreateFlagBits(..)
                                                , AllocatorCreateInfo(..)
                                                , withAllocator
                                                )
import           Window

myApiVersion :: Word32
myApiVersion = API_VERSION_1_1

----------------------------------------------------------------
-- Instance Creation
----------------------------------------------------------------

-- | Create an instance with a debug messenger
createInstance :: MonadResource m => SDL.Window -> m Instance
createInstance win = do
  windowExtensions <-
    liftIO $ traverse BS.packCString =<< SDL.vkGetInstanceExtensions win
  let createInfo = zero
        { applicationInfo = Just zero { applicationName = Nothing
                                      , apiVersion      = myApiVersion
                                      }
        }
      requirements =
        (\n -> RequireInstanceExtension Nothing n minBound)
          <$> ( KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
              : windowExtensions
              )
  createDebugInstanceFromRequirements requirements [] createInfo

----------------------------------------------------------------
-- Device creation
----------------------------------------------------------------

-- TODO: check VkPhysicalDeviceBufferDeviceAddressFeatures::bufferDeviceAddress.
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

  requireCommands inst dev

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
    -- ^ The relevant information from PhysicalDeviceProperties2KHR
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
  --
  -- Check device requirements
  --
  (mbDCI, rs, os) <- checkDeviceRequirements deviceRequirements [] phys zero
  -- Report any missing features
  traverse_ sayErrString (requirementReport rs os)
  -- Fail if we didn't meet requirements
  SomeStruct dciNoQueues              <- maybe empty pure mbDCI

  --
  -- Assign queues
  --
  (pdiQueueCreateInfos, pdiGetQueues) <- MaybeT
    $ assignQueues phys (queueRequirements phys surf)
  let dci =
        dciNoQueues { queueCreateInfos = SomeStruct <$> pdiQueueCreateInfos }

  --
  -- Query properties
  --
  pdiRTInfo      <- getDeviceRTProps phys

  --
  -- We'll use the amount of memory to pick the "best" device
  --
  pdiTotalMemory <- do
    heaps <- memoryHeaps <$> getPhysicalDeviceMemoryProperties phys
    pure $ sum ((size :: MemoryHeap -> DeviceSize) <$> heaps)

  pure (PhysicalDeviceInfo { .. }, SomeStruct dci)

-- | Requirements for a 'Queue' which has graphics suppor and can present to
-- the specified surface.
queueRequirements
  :: MonadIO m => PhysicalDevice -> SurfaceKHR -> Queues (QueueSpec m)
queueRequirements phys surf = Queues (QueueSpec 1 isGraphicsPresentQueue)
 where
  isGraphicsPresentQueue queueFamilyIndex queueFamilyProperties =
    pure (isGraphicsQueueFamily queueFamilyProperties)
      <&&> isPresentQueueFamily phys surf queueFamilyIndex

----------------------------------------------------------------
-- Physical device tools
----------------------------------------------------------------

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
createVMA inst phys dev =
  snd
    <$> withAllocator
          zero { flags            = ALLOCATOR_CREATE_BUFFER_DEVICE_ADDRESS_BIT
               , physicalDevice   = physicalDeviceHandle phys
               , device           = deviceHandle dev
               , instance'        = instanceHandle inst
               , vulkanApiVersion = myApiVersion
               }
          allocate

----------------------------------------------------------------
-- Command pools
----------------------------------------------------------------

-- | Create several command pools for a queue family
createCommandPools
  :: MonadResource m
  => Device
  -> Int
  -- ^ Number of pools to create
  -> QueueFamilyIndex
  -- ^ Queue family for the pools
  -> m (Vector CommandPool)
createCommandPools dev n (QueueFamilyIndex queueFamilyIndex) = do
  let commandPoolCreateInfo :: CommandPoolCreateInfo
      commandPoolCreateInfo = zero { queueFamilyIndex = queueFamilyIndex }
  V.replicateM
    n
    (   snd
    <$> withCommandPool dev
                        commandPoolCreateInfo
                        noAllocationCallbacks
                        allocate
    )

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

requireCommands :: MonadIO f => Instance -> Device -> f ()
requireCommands inst dev = case checkCommands inst dev of
  [] -> pure ()
  xs -> do
    for_ xs $ \n -> sayErr ("Failed to load function pointer for: " <> n)
    noSuchThing "Missing commands"

noSuchThing :: MonadIO m => String -> m a
noSuchThing message =
  liftIO . throwIO $ IOError Nothing NoSuchThing "" message Nothing Nothing

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)
