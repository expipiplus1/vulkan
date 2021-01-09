{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Vulkan.Utils.Requirements
  ( -- * Instance requirements
    checkInstanceRequirements
  , -- * Device requirements
    checkDeviceRequirements
  , -- * Results
    RequirementResult(..)
  , Unsatisfied(..)
  , requirementReport
  , prettyRequirementResult
  ) where

import           Control.Arrow                  ( Arrow((***)) )
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.Bifunctor
import           Data.ByteString                ( ByteString )
import qualified Data.Dependent.Map            as DMap
import           Data.Dependent.Map             ( DMap )
import           Data.Dependent.Sum             ( DSum((:=>)) )
import           Data.Foldable
import           Data.Functor.Product           ( Product(..) )
import qualified Data.HashMap.Strict           as Map
import           Data.Kind                      ( Type )
import           Data.List                      ( intercalate )
import           Data.List.Extra                ( nubOrd )
import           Data.Proxy
import           Data.Semigroup                 ( Endo(..) )
import           Data.Traversable
import           Data.Typeable                  ( eqT )
import qualified Data.Vector                   as V
import           Data.Vector                    ( Vector )
import           Data.Word
import           Foreign.Ptr                    ( FunPtr
                                                , Ptr
                                                , nullFunPtr
                                                )
import           GHC.Base                       ( Proxy# )
import           GHC.Exts                       ( proxy# )
import           Type.Reflection
import           Vulkan.CStruct                 ( FromCStruct
                                                , ToCStruct
                                                )
import           Vulkan.CStruct.Extends
import           Vulkan.Core10
import           Vulkan.Core11.DeviceInitialization
import           Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2
import           Vulkan.Dynamic                 ( InstanceCmds
                                                  ( pVkGetPhysicalDeviceFeatures2
                                                  , pVkGetPhysicalDeviceProperties2
                                                  )
                                                )
import           Vulkan.NamedType
import           Vulkan.Requirement
import           Vulkan.Version
import           Vulkan.Zero                    ( Zero(..) )

----------------------------------------------------------------
-- * Instance Creation
----------------------------------------------------------------

checkInstanceRequirements
  :: forall m o r es
   . (MonadIO m, Traversable r, Traversable o)
  => r InstanceRequirement
  -- ^ Required requests
  -> o InstanceRequirement
  -- ^ Optional requests
  -> InstanceCreateInfo es
  -- ^ An 'InstanceCreateInfo', this will be returned appropriately modified by
  -- the requirements
  -> m
       ( Maybe (InstanceCreateInfo es)
       , r RequirementResult
       , o RequirementResult
       )
checkInstanceRequirements required optional baseCreateInfo = do
  let requiredList = toList required
      allAsList    = requiredList <> toList optional
  foundVersion    <- enumerateInstanceVersion
  (_, layerProps) <- enumerateInstanceLayerProperties
  lookupExtension <- getLookupExtension
    Nothing
    [ instanceExtensionLayerName
    | RequireInstanceExtension { instanceExtensionLayerName } <- allAsList
    ]

  (r, continue) <- flip runStateT True $ for required $ \r ->
    case checkInstanceRequest foundVersion layerProps lookupExtension r of
      res -> do
        unless (res == Satisfied) (put False)
        pure res

  (o, goodOptions) <- flip runStateT mempty $ for optional $ \o ->
    case checkInstanceRequest foundVersion layerProps lookupExtension o of
      res -> do
        when (res == Satisfied) $ modify (o :)
        pure res

  let ici = do
        guard continue
        pure $ makeInstanceCreateInfo (requiredList <> goodOptions)
                                      baseCreateInfo
  pure (ici, r, o)

-- | Insert the settings of the requirements in to the provided instance create
-- info
makeInstanceCreateInfo
  :: forall es
   . [InstanceRequirement]
  -> InstanceCreateInfo es
  -> InstanceCreateInfo es
makeInstanceCreateInfo reqs baseCreateInfo =
  let
    layers = [ instanceLayerName | RequireInstanceLayer {..} <- reqs ]
    extensions =
      [ instanceExtensionName | RequireInstanceExtension {..} <- reqs ]
  in
    baseCreateInfo
      { enabledLayerNames     =
        enabledLayerNames (baseCreateInfo :: InstanceCreateInfo es)
          <> V.fromList layers
      , enabledExtensionNames =
        enabledExtensionNames (baseCreateInfo :: InstanceCreateInfo es)
          <> V.fromList extensions
      }

checkInstanceRequest
  :: ("apiVersion" ::: Word32)
  -> ("properties" ::: Vector LayerProperties)
  -> (  ("layerName" ::: Maybe ByteString)
     -> ByteString
     -> Maybe ExtensionProperties
     )
  -> InstanceRequirement
  -> RequirementResult
checkInstanceRequest foundVersion layerProps lookupExtension = \case
  RequireInstanceVersion minVersion -> if foundVersion >= minVersion
    then Satisfied
    else UnsatisfiedInstanceVersion (Unsatisfied minVersion foundVersion)

  RequireInstanceLayer { instanceLayerName, instanceLayerMinVersion }
    | Just props <- find ((== instanceLayerName) . layerName) layerProps
    , foundLayerVersion <- implementationVersion props
    -> if foundVersion >= instanceLayerMinVersion
      then Satisfied
      else UnsatisfiedLayerVersion
        instanceLayerName
        (Unsatisfied instanceLayerMinVersion foundLayerVersion)
    | otherwise
    -> MissingLayer instanceLayerName

  RequireInstanceExtension { instanceExtensionLayerName, instanceExtensionName, instanceExtensionMinVersion }
    | Just eProps <- lookupExtension instanceExtensionLayerName
                                     instanceExtensionName
    -> let foundInstanceExtensionVersion =
             specVersion (eProps :: ExtensionProperties)
       in  if foundInstanceExtensionVersion >= instanceExtensionMinVersion
             then Satisfied
             else UnsatisfiedInstanceExtensionVersion
               instanceExtensionName
               (Unsatisfied instanceExtensionMinVersion
                            foundInstanceExtensionVersion
               )
    | otherwise
    -> UnsatisfiedInstanceExtension instanceExtensionName

----------------------------------------------------------------
-- Device
----------------------------------------------------------------

checkDeviceRequirements
  :: forall m o r
   . (MonadIO m, Traversable r, Traversable o)
  => r DeviceRequirement
  -- ^ Required requests
  -> o DeviceRequirement
  -- ^ Optional requests
  -> PhysicalDevice
  -> DeviceCreateInfo '[]
  -- ^ A deviceCreateInfo with no extensions. If you need elements in the
  -- struct chain you can add them later with
  -- 'Vulkan.CStruct.Extends.extendSomeStruct'
  -> m
       ( Maybe (SomeStruct DeviceCreateInfo)
       , r RequirementResult
       , o RequirementResult
       )
checkDeviceRequirements required optional phys baseCreateInfo = do
  let requiredList = toList required
      allAsList    = requiredList <> toList optional

  --
  -- First collect the types and properties that we'll need to query using
  -- getPhysicalDeviceProperties2 and getPhysicalDeviceFeatures2
  --
  withDeviceFeatureStructs allAsList $ \(_ :: Proxy fs) ->
    withDevicePropertyStructs allAsList $ \(_ :: Proxy ps) -> do
      --
      -- Fetch everything
      --
      feats           <- getPhysicalDeviceFeaturesMaybe @fs phys
      props           <- getPhysicalDevicePropertiesMaybe @ps phys
      lookupExtension <- getLookupExtension
        (Just phys)
        [ deviceExtensionLayerName
        | RequireDeviceExtension { deviceExtensionLayerName } <- allAsList
        ]

      (r, continue) <- flip runStateT True $ for required $ \r ->
        case checkDeviceRequest feats props lookupExtension r of
          res -> do
            unless (res == Satisfied) (put False)
            pure res

      (o, goodOptions) <- flip runStateT mempty $ for optional $ \o ->
        case checkDeviceRequest feats props lookupExtension o of
          res -> do
            when (res == Satisfied) $ modify (o :)
            pure res

      --
      -- Now create the types for just the available features
      --
      let dci = do
            guard continue
            pure $ makeDeviceCreateInfo (requiredList <> goodOptions)
                                        baseCreateInfo
      pure (dci, r, o)

{-# ANN makeDeviceCreateInfo ("HLint: ignore Move guards forward" :: String) #-}
-- | Generate 'DeviceCreateInfo' from some requirements.
--
-- The returned struct chain will enable all required features and extensions.
makeDeviceCreateInfo
  :: [DeviceRequirement] -> DeviceCreateInfo '[] -> SomeStruct DeviceCreateInfo
makeDeviceCreateInfo allReqs baseCreateInfo =
  let
    featureSetters :: DMap TypeRep (Product (Has KnownFeatureStruct) Endo)
    featureSetters = DMap.fromListWithKey
      (\_ l r -> catProducts l r)
      [ typeRep :=> Pair Has (Endo enableFeature)
      | RequireDeviceFeature { enableFeature } <- allReqs
      ]

    makeZeroFeatureExts :: [Endo (SomeStruct DeviceCreateInfo)]
    makeZeroFeatureExts =
      [ Endo (extendSomeStruct s)
      | _ :=> Pair Has (f :: Endo s) <- DMap.toList featureSetters
      , ExtendedFeatureStruct        <- pure $ sFeatureStruct @s
      , let s = appEndo f zero
      ]

    addBasicFeatures :: Endo (SomeStruct DeviceCreateInfo)
    addBasicFeatures =
      case DMap.lookup (typeRep @PhysicalDeviceFeatures) featureSetters of
        Nothing         -> mempty
        Just (Pair _ s) -> Endo
          (extendSomeStruct
            ((zero :: PhysicalDeviceFeatures2 '[]) { features = appEndo s zero }
            )
          )

    extensionNames :: [ByteString]
    extensionNames =
      [ deviceExtensionName
      | RequireDeviceExtension { deviceExtensionName } <- allReqs
      ]

    newFeatures :: SomeStruct DeviceCreateInfo
    newFeatures = appEndo
      (fold (addBasicFeatures : makeZeroFeatureExts))
      (SomeStruct (baseCreateInfo :: DeviceCreateInfo '[])
        { enabledExtensionNames = V.fromList extensionNames
        }
      )
  in
    newFeatures

checkDeviceRequest
  :: forall fs ps
   . (KnownChain fs, KnownChain ps)
  => Maybe (PhysicalDeviceFeatures2 fs)
  -> Maybe (PhysicalDeviceProperties2 ps)
  -> (  ("layerName" ::: Maybe ByteString)
     -> ("extensionName" ::: ByteString)
     -> Maybe ExtensionProperties
     )
  -- ^ Lookup an extension
  -> DeviceRequirement
  -- ^ The requirement to test
  -> RequirementResult
  -- ^ The result
checkDeviceRequest mbFeats mbProps lookupExtension = \case
  RequireDeviceVersion minVersion
    | Just props <- mbProps
    , foundVersion <- apiVersion
      (properties (props :: PhysicalDeviceProperties2 ps) :: PhysicalDeviceProperties
      )
    -> if foundVersion >= minVersion
      then Satisfied
      else UnsatisfiedDeviceVersion (Unsatisfied minVersion foundVersion)
    | otherwise
    -> UnattemptedProperties "apiVersion"

  RequireDeviceFeature { featureName, checkFeature }
    | Just feats <- mbFeats -> case getFeatureStruct feats of
      Nothing ->
        error "Impossible: didn't find requested feature in struct chain"
      Just s ->
        if checkFeature s then Satisfied else UnsatisfiedFeature featureName
    | otherwise -> UnattemptedFeatures featureName

  RequireDeviceProperty { propertyName, checkProperty }
    | Just props <- mbProps -> case getPropertyStruct props of
      Nothing ->
        error "Impossible: didn't find requested property in struct chain"
      Just s ->
        if checkProperty s then Satisfied else UnsatisfiedProperty propertyName
    | otherwise -> UnattemptedProperties propertyName

  RequireDeviceExtension { deviceExtensionLayerName, deviceExtensionName, deviceExtensionMinVersion }
    | Just eProps <- lookupExtension deviceExtensionLayerName
                                     deviceExtensionName
    -> let foundVersion = specVersion (eProps :: ExtensionProperties)
       in  if foundVersion >= deviceExtensionMinVersion
             then Satisfied
             else UnsatisfiedDeviceExtensionVersion
               deviceExtensionName
               (Unsatisfied deviceExtensionMinVersion foundVersion)
    | otherwise
    -> UnsatisfiedDeviceExtension deviceExtensionName


----------------------------------------------------------------
-- Results
----------------------------------------------------------------

-- TODO, better version reporting for extensions
-- TODO, better reporting for properties
data RequirementResult
  = Satisfied
    -- ^ All the requirements were met
  | UnattemptedProperties ByteString
    -- ^ Didn't attempt this check because it required
    -- getPhysicalDeviceProperties2 which wasn't loaded
  | UnattemptedFeatures ByteString
    -- ^ Didn't attempt this check because it required
    -- getPhysicalDeviceFeatures2 which wasn't loaded
  | MissingLayer ByteString
    -- ^ A Layer was not found
  | UnsatisfiedDeviceVersion (Unsatisfied Word32)
    -- ^ A device version didn't meet the minimum requested
  | UnsatisfiedInstanceVersion (Unsatisfied Word32)
    -- ^ The instance version didn't meet the minimum requested
  | UnsatisfiedLayerVersion ByteString (Unsatisfied Word32)
    -- ^ A layer version didn't meet the minimum requested
  | UnsatisfiedFeature ByteString
    -- ^ A feature was missing
  | UnsatisfiedProperty ByteString
    -- ^ A propery was not an appropriate value
  | UnsatisfiedDeviceExtension ByteString
    -- ^ A device extension was missing
  | UnsatisfiedDeviceExtensionVersion ByteString (Unsatisfied Word32)
    -- ^ A device extension was found but the version didn't meet requirements
  | UnsatisfiedInstanceExtension ByteString
    -- ^ An instance extension was missing
  | UnsatisfiedInstanceExtensionVersion ByteString (Unsatisfied Word32)
    -- ^ An instance extension was found but the version didn't meet requirements
  deriving (Eq, Ord)

data Unsatisfied a = Unsatisfied
  { unsatisfiedMinimum :: a
    -- ^ The minimum value to be accepted
  , unsatisfiedActual  :: a
    -- ^ The value we got, less than 'unsatisfiedMinumum'
  }
  deriving (Eq, Ord)

-- | Generate a string describing which requirements were not met, if
-- everything was satisfied return 'Nothing'.
requirementReport
  :: (Foldable r, Foldable o)
  => r RequirementResult
  -> o RequirementResult
  -> Maybe String
requirementReport required optional =
  let pList xs =
        nubOrd [ prettyRequirementResult r | r <- toList xs, r /= Satisfied ]
      reqStrings = pList required
      optStrings = pList optional
      withHeader s = \case
        [] -> []
        xs -> (s <> " requirements not met:") : (("  " <>) <$> xs)
      reportLines =
        withHeader "Required" reqStrings <> withHeader "Optional" optStrings
  in  if null reportLines then Nothing else Just $ unlines reportLines

prettyRequirementResult :: RequirementResult -> String
prettyRequirementResult = \case
  Satisfied -> "Satisfied"
  UnattemptedProperties n ->
    "Did not attempt to check "
      <> show n
      <> " because the 'getPhysicalDeviceProperties' function was not loaded"
  UnattemptedFeatures n ->
    "Did not attempt to check "
      <> show n
      <> " because the 'getPhysicalDeviceFeatures' function was not loaded"
  MissingLayer               n -> "Couldn't find layer: " <> show n
  UnsatisfiedInstanceVersion u -> "Unsatisfied Instance version: " <> p u
  UnsatisfiedDeviceVersion   u -> "Unsatisfied Device version: " <> p u
  UnsatisfiedLayerVersion n u ->
    "Unsatisfied layer version for " <> show n <> ": " <> p u
  UnsatisfiedFeature  n -> "Missing feature: " <> show n
  UnsatisfiedProperty n -> "Unsatisfied property: " <> show n
  UnsatisfiedInstanceExtension n ->
    "Couldn't find instance extension: " <> show n
  UnsatisfiedInstanceExtensionVersion n u ->
    "Unsatisfied Instance extension version " <> show n <> " " <> p u
  UnsatisfiedDeviceExtension n -> "Couldn't find device extension: " <> show n
  UnsatisfiedDeviceExtensionVersion n u ->
    "Unsatisfied Device extension version " <> show n <> " " <> p u
  where p = prettyUnsatisfied showVersion

-- How I'm feeling after writing all this type level nonsense
prettyUnsatisfied :: (t -> String) -> Unsatisfied t -> String
prettyUnsatisfied s Unsatisfied {..} =
  "Wanted minimum of "
    <> s unsatisfiedMinimum
    <> ", got: "
    <> s unsatisfiedActual

----------------------------------------------------------------
-- Chain lenses
----------------------------------------------------------------

-- | Enough information to focus on any structure within a Vulkan structure chain.
class (PeekChain xs, PokeChain xs) => KnownChain (xs :: [Type]) where
  -- | If the given structure can be found within a chain, return a lens to it.
  -- Otherwise, return 'Nothing'.
  has :: forall a. Typeable a => Proxy# a -> Maybe (Chain xs -> a, (a -> a) -> (Chain xs -> Chain xs))
  -- | Is this chain empty?
  knownChainNull :: Maybe (xs :~: '[])

instance KnownChain '[] where
  has _ = Nothing
  knownChainNull = Just Refl

instance (Typeable x, ToCStruct x, FromCStruct x, KnownChain xs) => KnownChain (x ': xs) where
  has (px :: Proxy# a) | Just Refl <- eqT @a @x = Just (fst, first)
                       | otherwise = ((. snd) *** (second .)) <$> has px
  knownChainNull = Nothing

getPropertyStruct
  :: forall s es
   . (Typeable s, KnownChain es)
  => PhysicalDeviceProperties2 es
  -> Maybe s
getPropertyStruct c = case eqT @PhysicalDeviceProperties @s of
  Just Refl -> Just (properties (c :: PhysicalDeviceProperties2 es))
  Nothing   -> getStruct c

getFeatureStruct
  :: forall s es
   . (Typeable s, KnownChain es)
  => PhysicalDeviceFeatures2 es
  -> Maybe s
getFeatureStruct c = case eqT @PhysicalDeviceFeatures @s of
  Just Refl -> Just (features (c :: PhysicalDeviceFeatures2 es))
  Nothing   -> getStruct c

getStruct
  :: forall s h es
   . (Typeable h, Typeable s, KnownChain es, Extensible h)
  => h es
  -> Maybe s
getStruct c = ($ getNext c) . fst <$> has (proxy# :: Proxy# s)

----------------------------------------------------------------
-- Helpers for 'Device' and 'Instance' extensions
----------------------------------------------------------------

-- | Make a lookup function for extensions in layers
getLookupExtension
  :: MonadIO m
  => Maybe PhysicalDevice
  -- ^ Pass 'Nothing' for 'Instance' extensions, pass a PhysicalDevice for
  -- device extensions.
  -> ["layerName" ::: Maybe ByteString]
  -> m
       (  ("layerName" ::: Maybe ByteString)
       -> ByteString
       -> Maybe ExtensionProperties
       )
getLookupExtension mbPhys extensionLayers = do
  let enumerate = maybe enumerateInstanceExtensionProperties
                        enumerateDeviceExtensionProperties
                        mbPhys
  extensions <- for (nubOrd extensionLayers) $ \layer -> do
    (_, props) <- enumerate layer
    pure (layer, props)
  let extensionMap = Map.fromListWith (<>) extensions
  pure $ \layer name ->
    let es = Map.lookupDefault mempty layer extensionMap
    in  find ((== name) . extensionName) es

----------------------------------------------------------------
-- Helpers for extracting the type of chain used by a set of requirements
----------------------------------------------------------------

withDevicePropertyStructs
  :: forall a . [DeviceRequirement] -> ChainCont DevicePropertyChain a -> a
withDevicePropertyStructs = go @'[] []
 where
  go
    :: forall (fs :: [Type])
     . DevicePropertyChain fs
    => [SomeTypeRep]
    -> [DeviceRequirement]
    -> ChainCont DevicePropertyChain a
    -> a
  go seen reqs f = case reqs of
    -- We've been through all the reqs, call the continuation with the types
    [] -> f (Proxy @fs)
    -- This is a device property, add it to the list if we've not seen it before
    (RequireDeviceProperty _ (_ :: s -> Bool)) : rs
      | ExtendedPropertyStruct <- sPropertyStruct @s
      , sRep <- SomeTypeRep (typeRep @s)
      , sRep `notElem` seen
      -> go @(s:fs) (sRep : seen) rs f
    -- Otherwise skip
    _ : rs -> go @fs seen rs f

withDeviceFeatureStructs
  :: forall a . [DeviceRequirement] -> ChainCont DeviceFeatureChain a -> a
withDeviceFeatureStructs = go @'[] []
 where
  go
    :: forall (fs :: [Type])
     . DeviceFeatureChain fs
    => [SomeTypeRep]
    -> [DeviceRequirement]
    -> ChainCont DeviceFeatureChain a
    -> a
  go seen reqs f = case reqs of
    -- We've been through all the reqs, call the continuation with the types
    [] -> f (Proxy @fs)
    -- This is a device feature, add it to the list if we've not seen it before
    (RequireDeviceFeature _ _ (_ :: s -> s)) : rs
      | ExtendedFeatureStruct <- sFeatureStruct @s
      , sRep <- SomeTypeRep (typeRep @s)
      , sRep `notElem` seen
      -> go @(s:fs) (sRep : seen) rs f
    -- Otherwise skip
    _ : rs -> go @fs seen rs f

class (KnownChain es, Extendss PhysicalDeviceFeatures2 es, Show (Chain es)) => DeviceFeatureChain es where
instance (KnownChain es, Extendss PhysicalDeviceFeatures2 es, Show (Chain es)) => DeviceFeatureChain es where

class (KnownChain es, Extendss PhysicalDeviceProperties2 es) => DevicePropertyChain es where
instance (KnownChain es, Extendss PhysicalDeviceProperties2 es) => DevicePropertyChain es where

type ChainCont c a = forall (es :: [Type]) . (c es) => Proxy es -> a

----------------------------------------------------------------
-- Helpers for getting features and properties without using the extended
-- versions of the functions if possible.
----------------------------------------------------------------

getPhysicalDeviceFeaturesMaybe
  :: forall fs m
   . (MonadIO m, KnownChain fs, Extendss PhysicalDeviceFeatures2 fs)
  => PhysicalDevice
  -> m (Maybe (PhysicalDeviceFeatures2 fs))
getPhysicalDeviceFeaturesMaybe = getMaybe pVkGetPhysicalDeviceFeatures2
                                          (PhysicalDeviceFeatures2 ())
                                          getPhysicalDeviceFeatures
                                          getPhysicalDeviceFeatures2

getPhysicalDevicePropertiesMaybe
  :: forall fs m
   . (MonadIO m, KnownChain fs, Extendss PhysicalDeviceProperties2 fs)
  => PhysicalDevice
  -> m (Maybe (PhysicalDeviceProperties2 fs))
getPhysicalDevicePropertiesMaybe = getMaybe pVkGetPhysicalDeviceProperties2
                                            (PhysicalDeviceProperties2 ())
                                            getPhysicalDeviceProperties
                                            getPhysicalDeviceProperties2

getMaybe
  :: forall fs s1 s2 m
   . (MonadIO m, KnownChain fs, Extendss s2 fs)
  => (  InstanceCmds
     -> FunPtr (Ptr PhysicalDevice_T -> Ptr (SomeStruct s2) -> IO ())
     )
  -> (s1 -> s2 '[])
  -> (PhysicalDevice -> m s1)
  -> (PhysicalDevice -> m (s2 fs))
  -> PhysicalDevice
  -> m (Maybe (s2 fs))
getMaybe funPtr wrapper2 get1 get2 phys =
  let hasFunPtr = funPtr (instanceCmds (phys :: PhysicalDevice)) /= nullFunPtr
  in  case knownChainNull @fs of
        Just Refl -> Just . wrapper2 <$> get1 phys
        Nothing   -> if hasFunPtr then Just <$> get2 phys else pure Nothing

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

showVersion :: Word32 -> String
showVersion ver = intercalate "." [show ma, show mi, show pa]
  where MAKE_VERSION ma mi pa = ver

data Has c a where
  Has ::c a => Has c a

instance Semigroup (Has c a) where
  Has <> _ = Has

-- | There is no Semigroup instance for 'Product' in base
catProducts
  :: (Semigroup (f a), Semigroup (g a))
  => Product f g a
  -> Product f g a
  -> Product f g a
catProducts (Pair a1 b1) (Pair a2 b2) = Pair (a1 <> a2) (b1 <> b2)
