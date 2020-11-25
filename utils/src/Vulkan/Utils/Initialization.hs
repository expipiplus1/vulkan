{-# LANGUAGE OverloadedLists, NamedFieldPuns #-}

module Vulkan.Utils.Initialization
  ( createInstanceFromRequirements
  , addDebugRequirements
  , pickPhysicalDevice
  , physicalDeviceName
  , createDeviceFromRequirements
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Bits
import           Data.ByteString                ( ByteString )
import           Data.Foldable
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HashMap
import           Data.HashSet                   ( HashSet )
import qualified Data.HashSet                  as HashSet
import           Data.Maybe
import           Data.Ord
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Text.Encoding             ( decodeUtf8 )
import qualified Data.Vector                   as V
import           Data.Word
import           Vulkan.CStruct.Extends
import           Vulkan.Core10
import           Vulkan.Extensions.VK_EXT_debug_utils
import           Vulkan.Extensions.VK_EXT_validation_features
import           Vulkan.Requirements
import           Vulkan.Utils.Debug
import           Vulkan.Utils.Internal
import           Vulkan.Utils.Misc
import           Vulkan.Version
import           Vulkan.Zero

----------------------------------------------------------------
-- * Instance Creation
----------------------------------------------------------------

-- | Create an 'Instance' with some layers and extensions, the layers and
-- extensions will be added to the provided 'InstanceCreateInfo'.
--
-- Returns all the names of instance layers which were enabled.
--
-- Will throw an 'IOError in the case of missing layers or extensions. Details
-- on missing layers and extensions will be reported in stderr.
createInstanceFromRequests
  :: forall m
   . MonadResource m
  => Requests Instance
  -> InstanceCreateInfo '[]
  -> m (Instance, [ByteString])
createInstanceFromRequests
  ( InstanceRequests
    { version                 = ReqAndOpt reqVersion optVersion
    , instanceLayers          = ReqAndOpt requiredLayers optionalLayers
    , instanceExtensions      = reqAndOptExtensions
    , enableSettingsOtherThan = enableSettingsOtherThan :: HashSet ByteString -> Chain settings
    }
  )
  instanceCreateInfo
  = do
    --
    -- We start by checking that the Vulkan API version
    -- that the user asked for is compatible with the requirements.
    --
    let
      mbGivenVersion :: Maybe Word32
      mbGivenVersion = ( apiVersion :: ApplicationInfo -> Word32 ) <$> applicationInfo ( instanceCreateInfo :: InstanceCreateInfo '[] )
    for_ mbGivenVersion $ \ givenVersion -> do
      unless (givenVersion >= reqVersion)
        ( liftIO $ noSuchThing
          ( "Requirements imply a minimum Vulkan API version " <> showVersion reqVersion <> ",\n\
            \but the version specified by the application is " <> showVersion givenVersion <> "."
          )
        )
      unless (givenVersion >= optVersion)
        ( sayErr
          ( "Optional preference: minimum Vulkan API version " <> showVersion optVersion <> ",\n\
            \but the version specified by the application is " <> showVersion givenVersion <> "."
          )
        )
    --
    -- Next get the layers, they're needed to get the list of supported
    -- extensions, as some of them may only be present in layers.
    --
    availableLayerNames <-
      toList . fmap layerName . snd <$> enumerateInstanceLayerProperties
    (okLayers, missingOptLayers)
      <- partitionOptReqIO "layer"
            availableLayerNames
            (HashMap.keys $ optionalLayers)
            (HashMap.keys $ requiredLayers)
    -- Run 'enumerateInstanceExtensionProperties' once for the instance itself,
    -- and once for each layer and collect the results.
    availableExtensionNames <- concat <$> traverse
      ( fmap (toList . fmap extensionName . snd)
      . enumerateInstanceExtensionProperties
      )
      (Nothing : (Just <$> okLayers))
    let
      optionalExtensions, requiredExtensions :: [ByteString]
      ReqAndOpt requiredExtensions optionalExtensions
        = ( ( \ ( req, opt ) -> ReqAndOpt ( HashMap.keys =<< req ) ( HashMap.keys =<< opt ) ) )
        . unzip
        . map ( \ ( ReqAndOpt req opt ) -> (req,opt) )
        $ HashMap.elems reqAndOptExtensions
    (okExts, missingOptExts)
      <- partitionOptReqIO "instance extension"
            availableExtensionNames
            optionalExtensions
            requiredExtensions
    let
      enabledLayers, enabledExtensions :: V.Vector ByteString
      enabledLayers
        = enabledLayerNames (instanceCreateInfo :: InstanceCreateInfo '[])
        <> V.fromList okLayers
      enabledExtensions
        = enabledExtensionNames (instanceCreateInfo :: InstanceCreateInfo '[])
        <> V.fromList okExts
      instanceCreateInfo' :: InstanceCreateInfo settings
      instanceCreateInfo' = instanceCreateInfo
        { next                  = enableSettingsOtherThan HashSet.empty -- not using the 'OtherThan' functionality here
        , enabledLayerNames     = enabledLayers
        , enabledExtensionNames = enabledExtensions
        }
    (_, inst) <- withInstance instanceCreateInfo' Nothing allocate
    pure (inst, okLayers)

-- | Like 'createInstanceFromExtensions' except it will create a debug utils
-- messenger (from the @VK_EXT_debug_utils@ extension).
--
-- If the @VK_EXT_validation_features@ extension (from the
-- @VK_LAYER_KHRONOS_validation@ layer) is available is it will be enabled and
-- best practices messages enabled.
createDebugInstanceFromRequests
  :: forall m
   . MonadResource m
  => Requests Instance
  -> InstanceCreateInfo '[]
  -> m (Instance, [ByteString])
createDebugInstanceFromRequests reqs createInfo = do
  res@(inst,_) <- createInstanceFromRequests debugReqs createInfo
  _ <- withDebugUtilsMessengerEXT inst
                                  debugMessengerCreateInfo
                                  Nothing
                                  allocate
  pure res

  where
    debugReqs :: Requests Instance
    debugReqs = addRequests reqs
      [ Option  $ RequireInstanceLayer "VK_LAYER_KHRONOS_validation" 0
      , Require $ RequireInstanceExtension Nothing EXT_DEBUG_UTILS_EXTENSION_NAME 0
      , Option  $ RequireInstanceExtension Nothing EXT_VALIDATION_FEATURES_EXTENSION_NAME 0
      , Require $ RequireInstanceSetting validationFeatures
      , Require $ RequireInstanceSetting debugMessengerInfo
      ]
    validationFeatures :: ValidationFeaturesEXT
    validationFeatures = ValidationFeaturesEXT
      [VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT]
      []
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
-- * Physical device selection
----------------------------------------------------------------

-- | Check whether a 'PhysicalDevice' meets the provided 'Requests'.
-- 
-- If the 'PhysicalDevice' meets the 'Requests':
--   - returns 'Just' the optional properties, features and extensions (in this order) that did not meet the 'Requests'.
-- If the 'PhysicalDevice' did not fit the 'Requests':
--   - returns 'Nothing'.
physicalDeviceFitsRequests
  :: MonadIO m
  => Requests PhysicalDevice
  -> PhysicalDevice
  -> m (Maybe (HashSet ByteString, HashSet ByteString, HashSet ByteString))
physicalDeviceFitsRequests
  ( PhysicalDeviceRequests
    { missingProperties = missingProperties :: PhysicalDeviceProperties2 props -> ReqAndOpt (HashSet ByteString)
    , missingFeatures   = missingFeatures   :: PhysicalDeviceFeatures2   feats -> ReqAndOpt (HashSet ByteString)
    , deviceExtensions  = reqAndOptExtensions
    }
  )
  physicalDevice = do
    devName <- physicalDeviceName physicalDevice
    physicalDeviceProperties <- getPhysicalDeviceProperties physicalDevice
    let
      vulkan1p1 :: Word32
      vulkan1p1 = MAKE_VERSION 1 1 0
      deviceVersion :: Word32
      deviceVersion = (apiVersion :: VkPhysicalDeviceProperties -> Word32) physicalDeviceProperties
    ( ReqAndOpt missingReqProps missingOptProps :: ReqAndOpt ( HashSet ByteString ) ) <-
      case sLength @props of
        SNil    -> do
          pure $ missingProperties ( zero { properties = physicalDeviceProperties } )
        SCons _ -> do
          when (deviceVersion < vulkan1p1)
            ( liftIO $ noSuchThing
              ( "Device properties need Vulkan API version " <> showVersion vulkan1p1 <> ",\n\
                \but the current device Vulkan version is " <> showVersion deviceVersion <> "."
              )
            )
          physicalDeviceProperties2 <- getPhysicalDeviceProperties2 @props physicalDevice
          pure $ missingProperties physicalDeviceProperties2
    ( ReqAndOpt missingReqFeats missingOptFeats :: ReqAndOpt ( HashSet ByteString ) ) <-
      case sLength @feats of
        SNil    -> do
          physicalDeviceFeatures <- getPhysicalDeviceFeatures physicalDevice
          pure $ missingFeatures ( zero { features = physicalDeviceFeatures } )
        SCons _ -> do
          when (deviceVersion < vulkan1p1)
            ( liftIO $ noSuchThing
              ( "Device features need Vulkan API version " <> showVersion vulkan1p1 <> ",\n\
                \but the current device Vulkan version is " <> showVersion deviceVersion <> "."
              )
            )
          physicalDeviceFeatures2 <- getPhysicalDeviceFeatures2 @feats physicalDevice
          pure $ missingFeatures physicalDeviceFeatures2
    if HashSet.null missingReqProps && HashSet.null missingReqFeats
    then do
      let
        optionalExtensions, requiredExtensions :: [ByteString]
        ReqAndOpt requiredExtensions optionalExtensions
          = ( ( \ ( req, opt ) -> ReqAndOpt ( HashMap.keys =<< req ) ( HashMap.keys =<< opt ) ) )
          . unzip
          . map ( \ ( ReqAndOpt req opt ) -> (req,opt) )
          $ HashMap.elems reqAndOptExtensions
      availableExtensionNames <-
        fmap extensionName . snd <$> enumerateDeviceExtensionProperties phys Nothing
      ( missingOptExts, eitherMissingReqExtsOrEnabledExts) <-
        partitionOptReq
          availableExtensionNames
          optionalExtensions
          requiredExtensions
      case eitherMissingReqExtsOrEnabledExts of
        Left missingExts -> do
          devName <- physicalDeviceName physicalDevice
          sayErr
            (  "Physical device " <> Text.unpack devName
            <> " is missing the following required extensions:\n"
            <> show missingExts
            )
          pure Nothing
        Right _ ->
          pure ( Just ( missingOptProps, missingOptFeats, missingOptExts ) )
    else do
      sayErr
        ( "Physical device " <> Text.unpack devName <> " does not meet requirements.\n\
          \Missing required properties: " <> show missingReqProps <> "\n\
          \Missing required features  : " <> show missingReqFeats
        )
      pure Nothing
      
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
-- * Device initialization
----------------------------------------------------------------

-- | Create a 'Device' with some extensions, the extensions will be added to
-- the provided 'DeviceCreateInfo'.
--
-- Assumes the provided 'PhysicalDevice' satisfies the 'Requirements',
-- and that the provided layers have indeed been enabled in the current Vulkan instance.
--
-- Will throw an 'IOError in the case of missing extensions. Missing extensions
-- will be listed on stderr.
createDeviceFromRequests
  :: MonadResource m
  => PhysicalDevice
  -> Requests PhysicalDevice
  -> HashSet ByteString
  -- ^ Optional features that the device does not support,
  -- as returned in the second component by 'physicalDeviceFitsRequirements'.
  -> [ByteString]
  -- ^ Enabled instance layers,
  -- as returned by 'createInstanceFromRequirements'.
  -> V.Vector (SomeStruct DeviceQueueCreateInfo)
  -> m Device
createDeviceFromRequests
  phys
  ( PhysicalDeviceRequests
    { enableFeaturesOtherThan = enableFeaturesOtherThan :: HashSet ByteString -> PhysicalDeviceFeatures2 feats
    }
  )
  don'tEnableThese
  enabledLayers
  queueCreateInfos
  = do
    case sLength @feats of
      SNil    -> do
        let
          deviceCreateInfo :: DeviceCreateInfo '[]
          deviceCreateInfo = deviceCreateInfo
              { next                  = ()
              , flags                 = zero
              , queueCreateInfos
              , enabledLayerNames     = V.fromList enabledLayers
              , enabledExtensionNames = V.fromList okExtensions
              , enabledFeatures       = Just $ features ( enableFeaturesOtherThan don'tEnableThese )
              }
        snd <$> withDevice phys deviceCreateInfo Nothing allocate
      SCons _ -> do
        let
          deviceCreateInfo :: DeviceCreateInfo ( PhysicalDeviceFeatures2 '[] ': feats )
          deviceCreateInfo
            | PhysicalDeviceFeatures2 {next, features} <- enableFeaturesOtherThan don'tEnableThese
            = DeviceCreateInfo
              { next                  = (zero {features}, next)
              , flags                 = zero
              , queueCreateInfos
              , enabledLayerNames     = V.fromList enabledLayers
              , enabledExtensionNames = V.fromList okExtensions
              , enabledFeatures       = Nothing
              }
        snd <$> withDevice phys deviceCreateInfo Nothing allocate

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

maximumByMay :: Foldable t => (a -> a -> Ordering) -> t a -> Maybe a
maximumByMay f xs = if null xs then Nothing else Just (maximumBy f xs)

showVersion :: Word32 -> String
showVersion ver = show maj <> " " <> show min <> " " <> show patch
  where
    maj, min, patch :: Word32
    maj   = _VERSION_MAJOR ver
    min   = _VERSION_MINOR ver
    patch = _VERSION_PATCH ver
