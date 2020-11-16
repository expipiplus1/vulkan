{-# LANGUAGE OverloadedLists #-}

module Vulkan.Utils.Initialization
  ( createDebugInstanceWithExtensions
  , createInstanceWithExtensions
  , pickPhysicalDevice
  , physicalDeviceName
  , createDeviceWithExtensions
  ) where

import           Control.Exception              ( throwIO )
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Bits
import           Data.ByteString                ( ByteString )
import           Data.Foldable
import           Data.Maybe
import           Data.Ord
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( decodeUtf8 )
import qualified Data.Vector                   as V
import           GHC.IO.Exception               ( IOErrorType(NoSuchThing)
                                                , IOException(..)
                                                )
import           Vulkan.CStruct.Extends
import           Vulkan.Core10
import           Vulkan.Extensions.VK_EXT_debug_utils
import           Vulkan.Extensions.VK_EXT_validation_features
import           Vulkan.Utils.Debug
import           Vulkan.Utils.Misc
import           Vulkan.Zero

----------------------------------------------------------------
-- * Instance Creation
----------------------------------------------------------------

-- | Like 'createInstanceWithExtensions' except it will create a debug utils
-- messenger (from the @VK_EXT_debug_utils@ extension).
--
-- If the @VK_EXT_validation_features@ extension (from the
-- @VK_LAYER_KHRONOS_validation@ layer) is available is it will be enabled and
-- best practices messages enabled.
createDebugInstanceWithExtensions
  :: forall es m
   . (Extendss InstanceCreateInfo es, PokeChain es, MonadResource m)
  => [ByteString]
  -- ^ Required layers
  -> [ByteString]
  -- ^ Optional layers
  -> [ByteString]
  -- ^ Required extensions
  -> [ByteString]
  -- ^ Optional extensions
  -> InstanceCreateInfo es
  -> m Instance
createDebugInstanceWithExtensions requiredLayers optionalLayers requiredExtensions optionalExtensions instanceCreateInfo
  = do
    let debugMessengerCreateInfo = zero
          { messageSeverity = DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
                                .|. DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
          , messageType     = DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
                              .|. DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
                              .|. DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
          , pfnUserCallback = debugCallbackPtr
          }
        validationFeatures = ValidationFeaturesEXT
          [VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT]
          []
        instanceCreateInfo'
          :: InstanceCreateInfo
               (DebugUtilsMessengerCreateInfoEXT : ValidationFeaturesEXT : es)
        instanceCreateInfo' = instanceCreateInfo
          { next = debugMessengerCreateInfo :& validationFeatures :& next
                     (instanceCreateInfo :: InstanceCreateInfo es)
          }
    inst <- createInstanceWithExtensions
      requiredLayers
      ("VK_LAYER_KHRONOS_validation" : optionalLayers)
      (EXT_DEBUG_UTILS_EXTENSION_NAME : requiredExtensions)
      (EXT_VALIDATION_FEATURES_EXTENSION_NAME : optionalExtensions)
      instanceCreateInfo'
    _ <- withDebugUtilsMessengerEXT inst
                                    debugMessengerCreateInfo
                                    Nothing
                                    allocate
    pure inst

-- | Create an 'Instance' with some layers and extensions, the layers and
-- extensions will be added to the provided 'InstanceCreateInfo'.
--
-- Will throw an 'IOError in the case of missing layers or extensions. Details
-- on missing layers and extensions will be reported in stderr.
createInstanceWithExtensions
  :: forall es m
   . (Extendss InstanceCreateInfo es, PokeChain es, MonadResource m)
  => [ByteString]
  -- ^ Required layers
  -> [ByteString]
  -- ^ Optional layers
  -> [ByteString]
  -- ^ Required extensions
  -> [ByteString]
  -- ^ Optional extensions
  -> InstanceCreateInfo es
  -> m Instance
createInstanceWithExtensions requiredLayers optionalLayers requiredExtensions optionalExtensions instanceCreateInfo
  = do
    --
    -- First get the layers, they're needed to get the list of supported
    -- extensions, as some of them may only be present in layers.
    --
    availableLayerNames <-
      toList . fmap layerName . snd <$> enumerateInstanceLayerProperties
    layers <- partitionOptReqIO "layer"
                                availableLayerNames
                                optionalLayers
                                requiredLayers

    -- Run 'enumerateInstanceExtensionProperties' once for the instance itself,
    -- and once for each layer and collect the results.
    availableExtensionNames <- concat <$> traverse
      ( fmap (toList . fmap extensionName . snd)
      . enumerateInstanceExtensionProperties
      )
      (Nothing : (Just <$> layers))
    extensions <- partitionOptReqIO "instance extension"
                                    availableExtensionNames
                                    optionalExtensions
                                    requiredExtensions

    let
      instanceCreateInfo' :: InstanceCreateInfo es
      instanceCreateInfo' = instanceCreateInfo
        { enabledLayerNames     =
          enabledLayerNames (instanceCreateInfo :: InstanceCreateInfo es)
            <> V.fromList layers
        , enabledExtensionNames =
          enabledExtensionNames (instanceCreateInfo :: InstanceCreateInfo es)
            <> V.fromList extensions
        }
    (_, inst) <- withInstance instanceCreateInfo' Nothing allocate
    pure inst

----------------------------------------------------------------
-- * Physical device selection
----------------------------------------------------------------

-- | Get a single 'PhysicalDevice' deciding with a scoring function
--
-- Pass a function which will extract any required values from a device in the
-- spirit of parse-don't validate. Also provide a function to compare these
-- results for sorting multiple devices.
--
-- For example the result function could return a tuple of device memory and
-- the compute queue family index, and the scoring function could be 'fst' to
-- select devices based on their memory capacity.
--
-- If no devices are deemed suitable then an 'IOError' is thrown.
pickPhysicalDevice
  :: (MonadIO m, Ord b)
  => Instance
  -> (PhysicalDevice -> m (Maybe a))
  -- ^ Some result for a PhysicalDevice, Nothing if it is not to be chosen.
  -> (a -> b)
  -- ^ Scoring function to rate this result
  -> m (a, PhysicalDevice)
  -- ^ The score and the device
pickPhysicalDevice inst devInfo score = do
  (_, devs) <- enumeratePhysicalDevices inst
  infos    <- catMaybes
    <$> sequence [ fmap (, d) <$> devInfo d | d <- toList devs ]
  case maximumByMay (comparing (score.fst)) infos of
    Nothing -> liftIO $ noSuchThing "Unable to find appropriate PhysicalDevice"
    Just d  -> pure d

-- | Extract the name of a 'PhysicalDevice' with 'getPhysicalDeviceProperties'
physicalDeviceName :: MonadIO m => PhysicalDevice -> m Text
physicalDeviceName =
  fmap (decodeUtf8 . deviceName) . getPhysicalDeviceProperties

----------------------------------------------------------------
-- * Device initialization
----------------------------------------------------------------

-- | Create a 'Device' with some extensions, the extensions will be added to
-- the provided 'DeviceCreateInfo'.
--
-- Will throw an 'IOError in the case of missing extensions. Missing extensions
-- will be listed on stderr.
createDeviceWithExtensions
  :: forall es m
   . (Extendss DeviceCreateInfo es, PokeChain es, MonadResource m)
  => PhysicalDevice
  -> [ByteString]
  -- ^ Required extensions
  -> [ByteString]
  -- ^ Optional extensions
  -> DeviceCreateInfo es
  -> m Device
createDeviceWithExtensions phys requiredExtensions optionalExtensions deviceCreateInfo
  = do
    availableExtensionNames <-
      fmap extensionName
      .   snd
      <$> enumerateDeviceExtensionProperties phys Nothing
    extensions <- partitionOptReqIO "device extension"
                                    (toList availableExtensionNames)
                                    requiredExtensions
                                    optionalExtensions

    let
      deviceCreateInfo' :: DeviceCreateInfo es
      deviceCreateInfo' = deviceCreateInfo
        { enabledExtensionNames =
          enabledExtensionNames (deviceCreateInfo :: DeviceCreateInfo es)
            <> V.fromList extensions
        }

    (_, dev) <- withDevice phys deviceCreateInfo' Nothing allocate
    pure dev

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

noSuchThing :: String -> IO a
noSuchThing message =
  throwIO $ IOError Nothing NoSuchThing "" message Nothing Nothing

maximumByMay :: Foldable t => (a -> a -> Ordering) -> t a -> Maybe a
maximumByMay f xs = if null xs then Nothing else Just (maximumBy f xs)
