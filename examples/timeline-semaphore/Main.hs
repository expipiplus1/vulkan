{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main where


import           AutoApply
import           Control.Exception.Safe         ( throwString )
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Resource
import           Data.Bits
import           Data.Foldable
import           Data.List                      ( partition )
import           Data.Maybe
import           Data.Ord
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( decodeUtf8 )
import           Data.Traversable
import qualified Data.Vector                   as V
import           Data.Word
import           Say
import           System.Exit                    ( exitFailure )
import           Vulkan.CStruct.Extends
import           Vulkan.Core10
import           Vulkan.Core11
import           Vulkan.Core12
import           Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore
                                               as Timeline
import           Vulkan.Exception
import           Vulkan.Extensions.VK_EXT_debug_utils
import           Vulkan.Extensions.VK_EXT_validation_features
import           Vulkan.Extensions.VK_KHR_external_fence_fd
import           Vulkan.Extensions.VK_KHR_external_semaphore
import           Vulkan.Extensions.VK_KHR_external_semaphore_fd
import           Vulkan.Utils.Debug
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
  _                <- register (deviceWaitIdle' dev)

  -- After initializaion, perform our tests
  timelineTest dev pdi
  fenceFdTest dev
  semaphoreFdTest dev

timelineTest :: (MonadResource m, MonadThrow m) => Device -> PhysicalDeviceInfo -> m ()
timelineTest dev pdi = do
  -- first, test timeline semaphores
  (_, sem)         <- withSemaphore'
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
    e       -> throwM (VulkanException e)

fenceFdTest :: MonadResource m => Device -> m ()
fenceFdTest dev = do
  (_, fenceExport) <- withFence'
    dev
    (zero ::& ExportFenceCreateInfo EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT :& ()
    )

  fenceFd <- getFenceFdKHR
    dev
    zero { fence      = fenceExport
         , handleType = EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT
         }
  sayErrString $ "fence fd: " <> show fenceFd

semaphoreFdTest :: MonadResource m => Device -> m ()
semaphoreFdTest dev = do
  (_, semExport) <- withSemaphore'
    dev
    (   zero
    ::& ExportSemaphoreCreateInfo EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT
    :&  ()
    )

  fd <- getSemaphoreFdKHR
    dev
    zero { semaphore  = semExport
         , handleType = EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT
         }
  sayErrString $ "fd: " <> show fd


----------------------------------------------------------------
-- Vulkan utils
----------------------------------------------------------------

myApiVersion :: Word32
myApiVersion = API_VERSION_1_2

-- | Create an instance with a debug messenger
createInstance :: (MonadResource m, MonadThrow m) => m Instance
createInstance = do
  availableLayerNames <-
    toList . fmap layerName . snd <$> enumerateInstanceLayerProperties
  availableInstanceExtensionNames <-
    toList
    .   fmap extensionName
    .   snd
    <$> enumerateInstanceExtensionProperties Nothing
  availableLayerExtensionNames <- for availableLayerNames $ \l ->
    toList . fmap extensionName . snd <$> enumerateInstanceExtensionProperties
      (Just l)
  let availableExtensionNames =
        concat (availableInstanceExtensionNames : availableLayerExtensionNames)

  let requiredLayers     = []
      optionalLayers     = ["VK_LAYER_KHRONOS_validation"]
      requiredExtensions = [EXT_DEBUG_UTILS_EXTENSION_NAME]
      optionalExtensions = [EXT_VALIDATION_FEATURES_EXTENSION_NAME]

  extensions <-
    maybe (throwString "missing required extensions") pure
      =<< partitionOptReq "extension"
                          availableExtensionNames
                          optionalExtensions
                          requiredExtensions
  layers <-
    maybe (throwString "missing required layers") pure
      =<< partitionOptReq
            "layer"
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

  let deviceCreateInfo =
        zero
            { queueCreateInfos      =
              [ SomeStruct zero
                  { queueFamilyIndex = pdiComputeQueueFamilyIndex pdi
                  , queuePriorities  = [1]
                  }
              ]
            , enabledExtensionNames = [ KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME
                                      , KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME
                                      ]
            }
          ::& PhysicalDeviceTimelineSemaphoreFeatures True
          :&  ()

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

physicalDeviceName :: MonadIO m => PhysicalDevice -> m Text
physicalDeviceName phys = do
  props <- getPhysicalDeviceProperties phys
  pure $ decodeUtf8 (deviceName props)

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

partitionOptReq
  :: (Show a, Eq a, MonadIO m) => Text -> [a] -> [a] -> [a] -> m (Maybe [a])
partitionOptReq type' available optional required = do
  let (optHave, optMissing) = partition (`elem` available) optional
      (reqHave, reqMissing) = partition (`elem` available) required
      tShow                 = T.pack . show
  for_ optMissing
    $ \n -> sayErr $ "Missing optional " <> type' <> ": " <> tShow n
  case reqMissing of
    []  -> pure $ Just (reqHave <> optHave)
    [x] -> Nothing <$ sayErr ("Missing required " <> type' <> ": " <> tShow x)
    xs  -> Nothing <$ sayErr ("Missing required " <> type' <> "s: " <> tShow xs)

----------------------------------------------------------------
-- Bit utils
----------------------------------------------------------------

(.&&.) :: Bits a => a -> a -> Bool
x .&&. y = (/= zeroBits) (x .&. y)
