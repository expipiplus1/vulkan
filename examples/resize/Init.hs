{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Init
  ( Init.createInstance
  , Init.createDevice
  , DeviceParams(..)
  , createVMA
  ) where

import           Control.Monad                  ( guard )
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe      ( MaybeT(..) )
import           Control.Monad.Trans.Resource
import           Data.Bits
import           Data.ByteString                ( ByteString )
import           Data.Foldable
import           Data.List                      ( partition )
import           Data.Maybe                     ( catMaybes )
import           Data.Monoid                    ( Endo(..)
                                                , appEndo
                                                )
import           Data.Ord                       ( comparing )
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( decodeUtf8 )
import qualified Data.Vector                   as V
import           Data.Word
import           Say
import           UnliftIO.Exception

import           Vulkan.CStruct.Extends
import           Vulkan.Core10                 as Vk
                                         hiding ( withBuffer
                                                , withImage
                                                )
import           Vulkan.Extensions.VK_EXT_debug_utils
import           Vulkan.Extensions.VK_EXT_validation_features
import           Vulkan.Extensions.VK_KHR_surface
import           Vulkan.Extensions.VK_KHR_swapchain
import           Vulkan.Utils.Debug
import           Vulkan.Zero
import           VulkanMemoryAllocator          ( Allocator
                                                , AllocatorCreateInfo(..)
                                                , withAllocator
                                                )

import           MonadVulkan

myApiVersion :: Word32
myApiVersion = API_VERSION_1_0

----------------------------------------------------------------
-- Instance Creation
----------------------------------------------------------------

-- | Create an instance with a debug messenger and validation
createInstance :: forall m . MonadResource m => [ByteString] -> m Instance
createInstance extraExtensions = do
  let partitionOptReq :: (Show a, Eq a) => Text -> [a] -> [a] -> [a] -> m [a]
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

  availableExtensions <-
    toList
    .   fmap extensionName
    .   snd
    <$> enumerateInstanceExtensionProperties Nothing
  availableLayers <-
    toList . fmap layerName . snd <$> enumerateInstanceLayerProperties

  extensions <- partitionOptReq
    "extension"
    availableExtensions
    [EXT_VALIDATION_FEATURES_EXTENSION_NAME]
    (EXT_DEBUG_UTILS_EXTENSION_NAME : extraExtensions)
  layers <- partitionOptReq "layer"
                            availableLayers
                            ["VK_LAYER_KHRONOS_validation"]
                            []

  let instanceCreateInfo =
        let extend =
              [ extendSomeStruct debugMessengerCreateInfo
                | EXT_DEBUG_UTILS_EXTENSION_NAME `elem` extensions
                ]
                <> [ extendSomeStruct validationFeatures
                   | EXT_VALIDATION_FEATURES_EXTENSION_NAME `elem` extensions
                   ]
            base = zero
              { applicationInfo       = Just zero { applicationName = Nothing
                                                  , apiVersion = myApiVersion
                                                  }
              , enabledLayerNames     = V.fromList layers
              , enabledExtensionNames = V.fromList extensions
              }
        in  appEndo (foldMap @[] Endo extend) (SomeStruct base)
  inst <- withSomeStruct instanceCreateInfo (fmap snd . withInstance')
  _ <- withDebugUtilsMessengerEXT inst debugMessengerCreateInfo Nothing allocate
  pure inst

validationFeatures :: ValidationFeaturesEXT
validationFeatures =
  ValidationFeaturesEXT [VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT] []

debugMessengerCreateInfo :: DebugUtilsMessengerCreateInfoEXT
debugMessengerCreateInfo = zero
  { messageSeverity = DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
                        .|. DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
  , messageType     = DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
                      .|. DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
                      .|. DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
  , pfnUserCallback = debugCallbackPtr
  }

----------------------------------------------------------------
-- Device Creation
----------------------------------------------------------------

data DeviceParams = DeviceParams
  { dpDeviceName               :: Text
  , dpPhysicalDevice           :: PhysicalDevice
  , dpDevice                   :: Device
  , dpGraphicsQueue            :: Queue
    -- ^ Also the present queue
  , dpGraphicsQueueFamilyIndex :: Word32
  }
  deriving Show

-- | Creates a device with swapchain support
createDevice
  :: (MonadResource m, MonadThrow m) => Instance -> SurfaceKHR -> m DeviceParams
createDevice inst surf = do

  --
  -- Get a physical device
  --
  (pdi, phys) <- pickPhysicalDevice inst (physicalDeviceInfo surf) >>= \case
    Nothing -> throwString "Unable to find suitable physical device"
    Just x  -> pure x
  devName <- physicalDeviceName phys

  --
  -- Get a logical device
  --
  let graphicsQueueFamilyIndex = pdiGraphicsQueueFamilyIndex pdi
      deviceCreateInfo         = zero
        { queueCreateInfos      = [ SomeStruct zero
                                      { queueFamilyIndex = graphicsQueueFamilyIndex
                                      , queuePriorities = [1]
                                      }
                                  ]
        , enabledExtensionNames = [KHR_SWAPCHAIN_EXTENSION_NAME]
        }
  (_, dev)      <- withDevice phys deviceCreateInfo Nothing allocate
  graphicsQueue <- getDeviceQueue dev graphicsQueueFamilyIndex 0

  pure $ DeviceParams devName phys dev graphicsQueue graphicsQueueFamilyIndex

----------------------------------------------------------------
-- Physical device tools
----------------------------------------------------------------

-- | Get a single PhysicalDevice deciding with a scoring function
pickPhysicalDevice
  :: (MonadIO m, MonadThrow m, Ord a)
  => Instance
  -> (PhysicalDevice -> m (Maybe a))
  -- ^ Some "score" for a PhysicalDevice, Nothing if it is not to be chosen.
  -> m (Maybe (a, PhysicalDevice))
  -- ^ Throws if no device could be found
pickPhysicalDevice inst devScore = do
  (_, devs) <- enumeratePhysicalDevices inst
  scores    <- catMaybes
    <$> sequence [ fmap (, d) <$> devScore d | d <- toList devs ]
  pure
    $ if null scores then Nothing else Just $ maximumBy (comparing fst) scores

-- | The Ord instance prioritises devices with more memory
data PhysicalDeviceInfo = PhysicalDeviceInfo
  { pdiTotalMemory              :: Word64
  , pdiGraphicsQueueFamilyIndex :: Word32
  }
  deriving (Eq, Ord)

-- | Requires the device to have a graphics queue
--
-- The graphics queue index will be able to present to the specified surface
physicalDeviceInfo
  :: MonadIO m => SurfaceKHR -> PhysicalDevice -> m (Maybe PhysicalDeviceInfo)
physicalDeviceInfo surf phys = runMaybeT $ do
  -- We must be able to use the swapchain extension
  guard =<< deviceHasSwapChain phys

  -- It must have a graphics and present queue
  pdiGraphicsQueueFamilyIndex <- do
    queueFamilyProperties <- getPhysicalDeviceQueueFamilyProperties phys
    let isGraphicsQueue q =
          (QUEUE_GRAPHICS_BIT .&&. queueFlags q) && (queueCount q > 0)
        graphicsQueueIndices = fromIntegral . fst <$> V.filter
          (isGraphicsQueue . snd)
          (V.indexed queueFamilyProperties)
    let isPresentQueue i = getPhysicalDeviceSurfaceSupportKHR phys i surf
    presentQueueIndices <- V.filterM isPresentQueue graphicsQueueIndices
    MaybeT (pure $ presentQueueIndices V.!? 0)

  -- Score based on the total memory
  pdiTotalMemory <- do
    heaps <- memoryHeaps <$> getPhysicalDeviceMemoryProperties phys
    pure $ sum ((size :: MemoryHeap -> DeviceSize) <$> heaps)

  pure PhysicalDeviceInfo { .. }

deviceHasSwapChain :: MonadIO m => PhysicalDevice -> m Bool
deviceHasSwapChain dev = do
  (_, extensions) <- enumerateDeviceExtensionProperties dev Nothing
  pure $ V.any ((KHR_SWAPCHAIN_EXTENSION_NAME ==) . extensionName) extensions

physicalDeviceName :: MonadIO m => PhysicalDevice -> m Text
physicalDeviceName phys = do
  props <- getPhysicalDeviceProperties phys
  pure $ decodeUtf8 (deviceName props)


----------------------------------------------------------------
-- VulkanMemoryAllocator
----------------------------------------------------------------

createVMA
  :: MonadResource m => Instance -> PhysicalDevice -> Device -> m Allocator
createVMA inst phys dev =
  snd
    <$> withAllocator
          zero { flags            = zero
               , physicalDevice   = physicalDeviceHandle phys
               , device           = deviceHandle dev
               , instance'        = instanceHandle inst
               , vulkanApiVersion = myApiVersion
               }
          allocate

----------------------------------------------------------------
-- Bit utils
----------------------------------------------------------------

infixl 4 .&&.
(.&&.) :: Bits a => a -> a -> Bool
x .&&. y = (/= zeroBits) (x .&. y)
