{-# LANGUAGE OverloadedLists #-}

module Vulkan.Utils.Initialization
  ( -- * Instance creation
    createInstanceFromRequirements
  , createDebugInstanceFromRequirements
    -- * Device creation
  , createDeviceFromRequirements
  , -- * Physical device selection
    pickPhysicalDevice
  , physicalDeviceName
  ) where

import           Control.Monad.IO.Class
import           Data.Foldable
import           Data.Maybe
import           Data.Ord
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( decodeUtf8 )
import           Vulkan.Core10
import           Vulkan.Extensions.VK_EXT_debug_utils
import           Vulkan.Extensions.VK_EXT_validation_features
import           Data.Bits
import           Vulkan.Zero
import           Vulkan.Utils.Debug
import           Control.Monad.Trans.Resource
import           Vulkan.Requirement
import           Vulkan.CStruct.Extends
import           Vulkan.Utils.Internal
import           Vulkan.Utils.Requirements

----------------------------------------------------------------
-- Instance
----------------------------------------------------------------

-- | Like 'createInstanceFromRequirements' except it will create a debug utils
-- messenger (from the @VK_EXT_debug_utils@ extension).
--
-- If the @VK_EXT_validation_features@ extension (from the
-- @VK_LAYER_KHRONOS_validation@ layer) is available is it will be enabled and
-- best practices messages enabled.
createDebugInstanceFromRequirements
  :: forall m es r o
   . ( MonadResource m
     , Extendss InstanceCreateInfo es
     , PokeChain es
     , Foldable r
     , Foldable o
     )
  => r InstanceRequirement
  -> o InstanceRequirement
  -> InstanceCreateInfo es
  -> m Instance
createDebugInstanceFromRequirements required optional baseCreateInfo = do
  let debugMessengerCreateInfo = zero
        { messageSeverity = DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
                              .|. DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
        , messageType     = DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
                            .|. DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
                            .|. DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
        , pfnUserCallback = debugCallbackPtr
        }
      validationFeatures =
        ValidationFeaturesEXT [VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT] []
      instanceCreateInfo
        :: InstanceCreateInfo
             (DebugUtilsMessengerCreateInfoEXT : ValidationFeaturesEXT : es)
      instanceCreateInfo = baseCreateInfo
        { next = debugMessengerCreateInfo :& validationFeatures :& next
                   (baseCreateInfo :: InstanceCreateInfo es)
        }
      additionalRequirements =
        [ RequireInstanceExtension
            { instanceExtensionLayerName  = Nothing
            , instanceExtensionName       = EXT_DEBUG_UTILS_EXTENSION_NAME
            , instanceExtensionMinVersion = minBound
            }
        ]
      additionalOptionalRequirements =
        [ RequireInstanceLayer
          { instanceLayerName       = "VK_LAYER_KHRONOS_validation"
          , instanceLayerMinVersion = minBound
          }
        , RequireInstanceExtension
          { instanceExtensionLayerName  = Just "VK_LAYER_KHRONOS_validation"
          , instanceExtensionName       = EXT_VALIDATION_FEATURES_EXTENSION_NAME
          , instanceExtensionMinVersion = minBound
          }
        ]
  inst <- createInstanceFromRequirements
    (additionalRequirements <> toList required)
    (additionalOptionalRequirements <> toList optional)
    instanceCreateInfo
  _ <- withDebugUtilsMessengerEXT inst debugMessengerCreateInfo Nothing allocate
  pure inst

-- | Create an 'Instance from some requirements.
--
-- Will throw an 'IOError in the case of unsatisfied non-optional requirements.
-- Unsatisfied requirements will be listed on stderr.
createInstanceFromRequirements
  :: ( MonadResource m
     , Extendss InstanceCreateInfo es
     , PokeChain es
     , Traversable r
     , Traversable o
     )
  => r InstanceRequirement
  -> o InstanceRequirement
  -> InstanceCreateInfo es
  -> m Instance
createInstanceFromRequirements required optional baseCreateInfo = do
  (mbICI, rrs, ors) <- checkInstanceRequirements required
                                                 optional
                                                 baseCreateInfo
  traverse_ sayErr (requirementReport rrs ors)
  case mbICI of
    Nothing  -> liftIO $ unsatisfiedConstraints "Failed to create instance"
    Just ici -> snd <$> withInstance ici Nothing allocate

----------------------------------------------------------------
-- * Device creation
----------------------------------------------------------------

-- | Create a 'Device' from some requirements.
--
-- Will throw an 'IOError in the case of unsatisfied non-optional requirements.
-- Unsatisfied requirements will be listed on stderr.
createDeviceFromRequirements
  :: forall m
   . MonadResource m
  => PhysicalDevice
  -> [DeviceRequirement]
  -- ^ Required
  -> [DeviceRequirement]
  -- ^ Optional
  -> DeviceCreateInfo '[]
  -> m Device
createDeviceFromRequirements phys required optional baseCreateInfo = do
  (mbDCI, rrs, ors) <- checkDeviceRequirements required
                                               optional
                                               phys
                                               baseCreateInfo
  traverse_ sayErr (requirementReport rrs ors)
  case mbDCI of
    Nothing -> liftIO $ unsatisfiedConstraints "Failed to create instance"
    Just (SomeStruct dci) -> snd <$> withDevice phys dci Nothing allocate

----------------------------------------------------------------
-- * Physical device selection
----------------------------------------------------------------

-- | Get a single 'PhysicalDevice' deciding with a scoring function
--
-- Pass a function which will extract any required values from a device in the
-- spirit of parse-don't-validate. Also provide a function to compare these
-- results for sorting multiple suitable devices.
--
-- As an example, the suitability function could return a tuple of device
-- memory and the compute queue family index, and the scoring function could be
-- 'fst' to select devices based on their memory capacity. Consider using
-- 'Vulkan.Utils.QueueAssignment.assignQueues' to find your desired queues in
-- the suitability function.
--
-- Pehaps also use the functionality in 'Vulkan.Utils.Requirements' and return
-- the 'DeviceCreateInfo' too.
--
-- If no devices are deemed suitable then a 'NoSuchThing' 'IOError' is thrown.
pickPhysicalDevice
  :: (MonadIO m, Ord b)
  => Instance
  -> (PhysicalDevice -> m (Maybe a))
  -- ^ A suitability funcion for a 'PhysicalDevice', 'Nothing' if it is not to
  -- be chosen.
  -> (a -> b)
  -- ^ Scoring function to rate this result
  -> m (Maybe (a, PhysicalDevice))
  -- ^ The score and the device
pickPhysicalDevice inst devInfo score = do
  (_, devs) <- enumeratePhysicalDevices inst
  infos     <- catMaybes
    <$> sequence [ fmap (, d) <$> devInfo d | d <- toList devs ]
  pure $ maximumByMay (comparing (score . fst)) infos

-- | Extract the name of a 'PhysicalDevice' with 'getPhysicalDeviceProperties'
physicalDeviceName :: MonadIO m => PhysicalDevice -> m Text
physicalDeviceName =
  fmap (decodeUtf8 . deviceName) . getPhysicalDeviceProperties


----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

maximumByMay :: Foldable t => (a -> a -> Ordering) -> t a -> Maybe a
maximumByMay f xs = if null xs then Nothing else Just (maximumBy f xs)

