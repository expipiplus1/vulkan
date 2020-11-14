{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main where


import           AutoApply
import           Control.Exception              ( throwIO )
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Resource
import qualified Data.Vector                   as V
import           Data.Word
import           Say
import           Vulkan.CStruct.Extends
import           Vulkan.Core10
import           Vulkan.Core12
import           Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore
                                               as Timeline
import           Vulkan.Exception
import           Vulkan.Utils.Initialization
import           Vulkan.Utils.Misc
import           Vulkan.Zero

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
  [ 'noAllocationCallbacks
  ]
  -- Allocate doesn't subsume the continuation type on the "with" commands, so
  -- put it in the unifying group.
  [ 'allocate ]
  [ 'deviceWaitIdle
  , 'withFence
  , 'withInstance
  , 'withSemaphore
  , 'Timeline.waitSemaphores
  ]

main :: IO ()
main = runResourceT $ do
  inst             <- Main.createInstance
  (_phys, pdi, dev) <- Main.createDevice inst
  timelineTest dev pdi

timelineTest :: (MonadResource m) => Device -> PhysicalDeviceInfo -> m ()
timelineTest dev pdi = do
  -- first, test timeline semaphores
  (_, sem) <- withSemaphore'
    dev
    (zero ::& SemaphoreTypeCreateInfo SEMAPHORE_TYPE_TIMELINE 1 :& ())

  -- Create some GPU work which waits for the semaphore to be '2' and then
  -- bumps it to '3'
  queue <- getDeviceQueue dev (pdiComputeQueueFamilyIndex pdi) 0
  queueSubmit
    queue
    [ SomeStruct
        (   zero { Vulkan.Core10.waitSemaphores = [sem]
                 , signalSemaphores             = [sem]
                 , commandBuffers               = []
                 , waitDstStageMask = [PIPELINE_STAGE_TOP_OF_PIPE_BIT]
                 }
        ::& zero { waitSemaphoreValues = [2], signalSemaphoreValues = [3] }
        :&  ()
        )
    ]
    zero

  -- Go
  signalSemaphore dev zero { semaphore = sem, value = 2 }

  waitSemaphores' dev zero { semaphores = [sem], values = [3] } 1e9 >>= \case
    TIMEOUT -> sayErr "Timed out waiting for semaphore"
    SUCCESS -> sayErr "Waited for semaphore"
    e       -> do
      sayErrShow e
      liftIO $ throwIO (VulkanException e)

  deviceWaitIdle' dev

----------------------------------------------------------------
-- Vulkan utils
----------------------------------------------------------------

-- | Create an instance with a debug messenger
createInstance :: MonadResource m => m Instance
createInstance =
  let createInfo = zero
        { applicationInfo = Just zero { applicationName = Nothing
                                      , apiVersion      = API_VERSION_1_2
                                      }
        }
  in  createDebugInstanceWithExtensions [] [] [] [] createInfo

createDevice
  :: (MonadResource m)
  => Instance
  -> m (PhysicalDevice, PhysicalDeviceInfo, Device)
createDevice inst = do
  (pdi, phys) <- pickPhysicalDevice inst physicalDeviceInfo
  sayErr . ("Using device: " <>) =<< physicalDeviceName phys
  let deviceCreateInfo =
        zero
            { queueCreateInfos      =
              [ SomeStruct zero
                  { queueFamilyIndex = pdiComputeQueueFamilyIndex pdi
                  , queuePriorities  = [1]
                  }
              ]
            }
          ::& PhysicalDeviceTimelineSemaphoreFeatures True
          :&  ()
  dev <- createDeviceWithExtensions phys [] [] deviceCreateInfo
  pure (phys, pdi, dev)

----------------------------------------------------------------
-- Physical device tools
----------------------------------------------------------------

-- | The Ord instance prioritises devices with more memory
data PhysicalDeviceInfo = PhysicalDeviceInfo
  { pdiTotalMemory             :: Word64
  , pdiComputeQueueFamilyIndex :: Word32
    -- ^ The queue family index of the first compute queue
  }
  deriving (Eq, Ord)

physicalDeviceInfo
  :: MonadIO m => PhysicalDevice -> m (Maybe PhysicalDeviceInfo)
physicalDeviceInfo phys = runMaybeT $ do
  pdiTotalMemory <- do
    heaps <- memoryHeaps <$> getPhysicalDeviceMemoryProperties phys
    pure $ sum ((size :: MemoryHeap -> DeviceSize) <$> heaps)
  pdiComputeQueueFamilyIndex <- do
    queueFamilyProperties <- getPhysicalDeviceQueueFamilyProperties phys
    let isComputeQueue q =
          (QUEUE_COMPUTE_BIT .&&. queueFlags q) && (queueCount q > 0)
        computeQueueIndices = fromIntegral . fst <$> V.filter
          (isComputeQueue . snd)
          (V.indexed queueFamilyProperties)
    MaybeT (pure $ computeQueueIndices V.!? 0)
  pure PhysicalDeviceInfo { .. }
