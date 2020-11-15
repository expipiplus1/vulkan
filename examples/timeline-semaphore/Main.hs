{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main
  ( main
  ) where


import           AutoApply
import           Control.Exception              ( throwIO )
import           Control.Monad                  ( guard )
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Resource
import           Data.Coerce                    ( coerce )
import           Data.Vector                    ( Vector )
import           Data.Word
import           GHC.Exception                  ( SomeException )
import           Say
import           UnliftIO                       ( Exception(displayException)
                                                , catch
                                                )
import           Vulkan.CStruct.Extends
import           Vulkan.Core10
import           Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2
                                                ( PhysicalDeviceFeatures2 )
import           Vulkan.Core12
import           Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore
                                               as Timeline
import           Vulkan.Dynamic
import           Vulkan.Exception
import           Vulkan.Extensions.VK_KHR_get_physical_device_properties2
import           Vulkan.Extensions.VK_KHR_timeline_semaphore
import           Vulkan.Utils.Initialization
import           Vulkan.Utils.QueueAssignment
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
  , 'withSemaphore
  , 'Timeline.waitSemaphores
  ]

main :: IO ()
main = runResourceT . traceException $ do
  inst <- Main.createInstance
  (_phys, dev, MyQueues computeQueue) <- Main.createDevice inst
  timelineTest dev computeQueue

traceException :: MonadUnliftIO m => m a -> m a
traceException m =
  m
    `catch` (\(e :: SomeException) ->
              sayErrString (displayException e) >> liftIO (throwIO e)
            )

timelineTest :: (MonadResource m) => Device -> Queue -> m ()
timelineTest dev computeQueue = do
  (_, sem) <- withSemaphore'
    dev
    (zero ::& SemaphoreTypeCreateInfo SEMAPHORE_TYPE_TIMELINE 1 :& ())

  -- Create some GPU work which waits for the semaphore to be '2' and then
  -- bumps it to '3'
  queueSubmit
    computeQueue
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

  -- Bump the semaphore to '2' to start the GPU work
  signalSemaphore dev zero { semaphore = sem, value = 2 }

  -- Wait for the GPU to set it to '3'
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
                                      , apiVersion      = API_VERSION_1_0
                                      }
        }
  in  createDebugInstanceWithExtensions
        []
        []
        [KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME]
        []
        createInfo

createDevice
  :: forall m
   . (MonadResource m)
  => Instance
  -> m (PhysicalDevice, Device, MyQueues Queue)
createDevice inst = do
  (pdi, phys) <- pickPhysicalDevice inst physicalDeviceInfo pdiScore
  sayErr . ("Using device: " <>) =<< physicalDeviceName phys
  let deviceCreateInfo =
        zero { queueCreateInfos = SomeStruct <$> pdiQueueCreateInfos pdi }
          ::& PhysicalDeviceTimelineSemaphoreFeatures True
          :&  ()
  dev' <- createDeviceWithExtensions phys
                                     []
                                     [KHR_TIMELINE_SEMAPHORE_EXTENSION_NAME]
                                     deviceCreateInfo
  wait <- getDeviceProcAddr dev' "vkWaitSemaphoresKHR"
  sig  <- getDeviceProcAddr dev' "vkSignalSemaphoreKHR"
  let dev :: Device
      dev = dev'
        { deviceCmds = (deviceCmds (dev' :: Device))
                         { pVkWaitSemaphores  = coerce wait
                         , pVkSignalSemaphore = coerce sig
                         }
        }
  queues <- liftIO $ pdiGetQueues pdi dev
  pure (phys, dev, queues)


----------------------------------------------------------------
-- Physical device tools
----------------------------------------------------------------

-- | The Ord instance prioritises devices with more memory
data PhysicalDeviceInfo = PhysicalDeviceInfo
  { pdiTotalMemory      :: Word64
  , pdiQueueCreateInfos :: Vector (DeviceQueueCreateInfo '[])
  , pdiGetQueues        :: Device -> IO (MyQueues Queue)
  }

pdiScore :: PhysicalDeviceInfo -> Word64
pdiScore = pdiTotalMemory

newtype MyQueues a = MyQueues { _myComputeQueue :: a }
  deriving (Functor, Foldable, Traversable)

physicalDeviceInfo
  :: MonadIO m => PhysicalDevice -> m (Maybe PhysicalDeviceInfo)
physicalDeviceInfo phys = runMaybeT $ do
  feats <- getPhysicalDeviceFeatures2KHR phys
  let
    _ ::& (PhysicalDeviceTimelineSemaphoreFeatures hasTimelineSemaphores :& ())
      = feats
  guard hasTimelineSemaphores
  pdiTotalMemory <- do
    heaps <- memoryHeaps <$> getPhysicalDeviceMemoryProperties phys
    pure $ sum ((size :: MemoryHeap -> DeviceSize) <$> heaps)
  (pdiQueueCreateInfos, pdiGetQueues) <- MaybeT $ assignQueues
    phys
    (MyQueues (QueueSpec 1 (const (pure . isComputeQueueFamily))))
  pure PhysicalDeviceInfo { .. }
