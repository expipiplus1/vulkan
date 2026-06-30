{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}

module Vulkan.Utils.Initialization
  ( -- * Instance creation
    allocateInstanceFromRequirements
  , allocateDebugInstanceFromRequirements
  , allocateVulkanInstance

    -- * macOS portability
  , portabilityRequirements
  , portabilityFlags
  , devicePortabilityRequirements

    -- * Device creation
  , allocateDeviceFromRequirements

    -- * Physical device selection
  , pickPhysicalDevice
  , physicalDeviceName

    -- * Deprecated aliases
  , createInstanceFromRequirements
  , createDebugInstanceFromRequirements
  , createDeviceFromRequirements
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Bits
import Data.ByteString (ByteString)
import Data.Foldable
import Data.Maybe
import Data.Ord
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Vector (Vector)
import Vulkan.CStruct.Extends
import Vulkan.Core10
import qualified Vulkan.Core10 as Instance (InstanceCreateInfo (..))
import Vulkan.Extensions.VK_EXT_debug_utils
import Vulkan.Extensions.VK_EXT_validation_features
import Vulkan.Requirement
import Vulkan.Utils.Debug
import Vulkan.Utils.Internal
import Vulkan.Utils.Requirements
import Vulkan.Zero

#if defined(darwin_HOST_OS)
import           Vulkan.Core10.Enums.InstanceCreateFlagBits
                                                ( pattern INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR )
import           Vulkan.Extensions.VK_KHR_portability_enumeration
                                                ( pattern KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME )
import           Vulkan.Extensions.VK_KHR_portability_subset
                                                ( pattern KHR_PORTABILITY_SUBSET_EXTENSION_NAME )
#endif

----------------------------------------------------------------
-- Instance
----------------------------------------------------------------

{- | Like 'allocateInstanceFromRequirements' except it will create a debug utils
messenger (from the @VK_EXT_debug_utils@ extension).

If the @VK_EXT_validation_features@ extension (from the
@VK_LAYER_KHRONOS_validation@ layer) is available is it will be enabled and
best practices messages enabled.
-}
allocateDebugInstanceFromRequirements
  :: forall m es
   . (MonadResource m, Extendss InstanceCreateInfo es, PokeChain es)
  => [InstanceRequirement]
  -- ^ Required
  -> [InstanceRequirement]
  -- ^ Optional
  -> InstanceCreateInfo es
  -> m Instance
allocateDebugInstanceFromRequirements required optional baseCreateInfo = do
  let
    debugMessengerCreateInfo =
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
    validationFeatures =
      ValidationFeaturesEXT [VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT] []
    instanceCreateInfo
      :: InstanceCreateInfo
           (DebugUtilsMessengerCreateInfoEXT : ValidationFeaturesEXT : es)
    instanceCreateInfo =
      baseCreateInfo
        { Instance.next =
            debugMessengerCreateInfo
              :& validationFeatures
              :& Instance.next baseCreateInfo
        }
    additionalRequirements =
      [ RequireInstanceExtension
          { instanceExtensionLayerName = Nothing
          , instanceExtensionName = EXT_DEBUG_UTILS_EXTENSION_NAME
          , instanceExtensionMinVersion = minBound
          }
      ]
    additionalOptionalRequirements =
      [ RequireInstanceLayer
          { instanceLayerName = "VK_LAYER_KHRONOS_validation"
          , instanceLayerMinVersion = minBound
          }
      , RequireInstanceExtension
          { instanceExtensionLayerName = Just "VK_LAYER_KHRONOS_validation"
          , instanceExtensionName = EXT_VALIDATION_FEATURES_EXTENSION_NAME
          , instanceExtensionMinVersion = minBound
          }
      ]
  inst <-
    allocateInstanceFromRequirements
      (additionalRequirements <> toList required)
      (additionalOptionalRequirements <> toList optional)
      instanceCreateInfo
  _ <- withDebugUtilsMessengerEXT inst debugMessengerCreateInfo Nothing allocate
  pure inst

{- | Create an 'Instance from some requirements.

Will throw an 'IOError in the case of unsatisfied non-optional requirements.
Unsatisfied requirements will be listed on stderr.
-}
allocateInstanceFromRequirements
  :: (MonadResource m, Extendss InstanceCreateInfo es, PokeChain es)
  => [InstanceRequirement]
  -- ^ Required
  -> [InstanceRequirement]
  -- ^ Optional
  -> InstanceCreateInfo es
  -> m Instance
allocateInstanceFromRequirements required optional baseCreateInfo = do
  (mbICI, rrs, ors) <-
    checkInstanceRequirements
      required
      optional
      baseCreateInfo
  traverse_ sayErr (requirementReport rrs ors)
  case mbICI of
    Nothing -> liftIO $ unsatisfiedConstraints "Failed to create instance"
    Just ici -> snd <$> withInstance ici Nothing allocate

----------------------------------------------------------------
-- macOS portability + windowing-friendly instance creation
----------------------------------------------------------------

portabilityRequirements :: [InstanceRequirement]

{- | Instance create flag bits that pair with 'portabilityRequirements'.
'zero' on every non-macOS platform.
-}
portabilityFlags :: InstanceCreateFlags

{- | Device requirements needed on macOS: the Vulkan spec mandates that
@VK_KHR_portability_subset@ be enabled whenever a physical device advertises
it (as MoltenVK does). Empty on every other platform.
-}
devicePortabilityRequirements :: [DeviceRequirement]

#if defined(darwin_HOST_OS)
portabilityRequirements =
  [ RequireInstanceExtension
      { instanceExtensionLayerName  = Nothing
      , instanceExtensionName       = KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME
      , instanceExtensionMinVersion = minBound
      }
  ]
portabilityFlags = INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR
devicePortabilityRequirements =
  [ RequireDeviceExtension
      { deviceExtensionLayerName  = Nothing
      , deviceExtensionName       = KHR_PORTABILITY_SUBSET_EXTENSION_NAME
      , deviceExtensionMinVersion = minBound
      }
  ]
#else
portabilityRequirements = []
portabilityFlags        = zero
devicePortabilityRequirements = []
#endif

{- | Build a Vulkan 'Instance' from a backend-supplied extension list plus
caller-supplied requirements. Automatically merges 'portabilityRequirements'
into the required list and 'portabilityFlags' into the create flags so
macOS apps work without per-call plumbing.

Pass 'mempty' for the extension list when running headless; or call
'Vulkan.Utils.Init.Headless.allocateInstance' which does so.
-}
allocateVulkanInstance
  :: (MonadResource m)
  => Vector ByteString
  {- ^ Backend-required instance extensions (e.g. from
  @Vulkan.Utils.Init.SDL2.getRequiredInstanceExtensions@). 'mempty' for
  headless.
  -}
  -> Maybe ApplicationInfo
  -> [InstanceRequirement]
  -- ^ Caller's required requirements
  -> [InstanceRequirement]
  -- ^ Caller's optional requirements
  -> m Instance
allocateVulkanInstance exts appInfo reqs optReqs =
  allocateInstanceFromRequirements
    (portabilityRequirements <> reqs)
    optReqs
    zero
      { applicationInfo = appInfo
      , enabledExtensionNames = exts
      , flags = portabilityFlags
      }

----------------------------------------------------------------

-- * Device creation

----------------------------------------------------------------

{- | Create a 'Device' from some requirements.

Will throw an 'IOError in the case of unsatisfied non-optional requirements.
Unsatisfied requirements will be listed on stderr.
-}
allocateDeviceFromRequirements
  :: forall m
   . (MonadResource m)
  => [DeviceRequirement]
  -- ^ Required
  -> [DeviceRequirement]
  -- ^ Optional
  -> PhysicalDevice
  -> DeviceCreateInfo '[]
  -> m Device
allocateDeviceFromRequirements required optional phys baseCreateInfo = do
  (mbDCI, rrs, ors) <-
    checkDeviceRequirements
      (devicePortabilityRequirements <> required)
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

{- | Get a single 'PhysicalDevice' deciding with a scoring function

Pass a function which will extract any required values from a device in the
spirit of parse-don't-validate. Also provide a function to compare these
results for sorting multiple suitable devices.

As an example, the suitability function could return a tuple of device
memory and the compute queue family index, and the scoring function could be
'fst' to select devices based on their memory capacity. Consider using
'Vulkan.Utils.QueueAssignment.assignQueues' to find your desired queues in
the suitability function.

Pehaps also use the functionality in 'Vulkan.Utils.Requirements' and return
the 'DeviceCreateInfo' too.

If no devices are deemed suitable then a 'NoSuchThing' 'IOError' is thrown.
-}
pickPhysicalDevice
  :: (MonadIO m, Ord b)
  => Instance
  -> (PhysicalDevice -> m (Maybe a))
  {- ^ A suitability funcion for a 'PhysicalDevice', 'Nothing' if it is not to
  be chosen.
  -}
  -> (a -> b)
  -- ^ Scoring function to rate this result
  -> m (Maybe (a, PhysicalDevice))
  -- ^ The score and the device
pickPhysicalDevice inst devInfo score = do
  (_, devs) <- enumeratePhysicalDevices inst
  infos <-
    catMaybes
      <$> sequence
        [ do
            isCPU <-
              (PHYSICAL_DEVICE_TYPE_CPU ==) . deviceType
                <$> getPhysicalDeviceProperties d
            if isCPU then pure Nothing else fmap (,d) <$> devInfo d
        | d <- toList devs
        ]
  pure $ maximumBy_ (comparing (score . fst)) infos

-- | Extract the name of a 'PhysicalDevice' with 'getPhysicalDeviceProperties'
physicalDeviceName :: (MonadIO m) => PhysicalDevice -> m Text
physicalDeviceName =
  fmap (decodeUtf8 . deviceName) . getPhysicalDeviceProperties

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

maximumBy_ :: (Foldable t) => (a -> a -> Ordering) -> t a -> Maybe a
maximumBy_ f xs = if null xs then Nothing else Just (maximumBy f xs)

----------------------------------------------------------------
-- Deprecated aliases
----------------------------------------------------------------

{-# DEPRECATED createInstanceFromRequirements "Renamed to allocateInstanceFromRequirements" #-}

-- | Deprecated alias for 'allocateInstanceFromRequirements'.
createInstanceFromRequirements
  :: (MonadResource m, Extendss InstanceCreateInfo es, PokeChain es)
  => [InstanceRequirement]
  -> [InstanceRequirement]
  -> InstanceCreateInfo es
  -> m Instance
createInstanceFromRequirements = allocateInstanceFromRequirements

{-# DEPRECATED createDebugInstanceFromRequirements "Renamed to allocateDebugInstanceFromRequirements" #-}

-- | Deprecated alias for 'allocateDebugInstanceFromRequirements'.
createDebugInstanceFromRequirements
  :: (MonadResource m, Extendss InstanceCreateInfo es, PokeChain es)
  => [InstanceRequirement]
  -> [InstanceRequirement]
  -> InstanceCreateInfo es
  -> m Instance
createDebugInstanceFromRequirements = allocateDebugInstanceFromRequirements

{-# DEPRECATED createDeviceFromRequirements "Renamed to allocateDeviceFromRequirements" #-}

-- | Deprecated alias for 'allocateDeviceFromRequirements'.
createDeviceFromRequirements
  :: (MonadResource m)
  => [DeviceRequirement]
  -> [DeviceRequirement]
  -> PhysicalDevice
  -> DeviceCreateInfo '[]
  -> m Device
createDeviceFromRequirements = allocateDeviceFromRequirements
