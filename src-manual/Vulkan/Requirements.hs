{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DuplicateRecordFields   #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE MagicHash               #-}
{-# LANGUAGE NamedFieldPuns          #-}
{-# LANGUAGE PatternSynonyms         #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE RecordWildCards         #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE Strict                  #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Vulkan.Requirements where

-- base
import Control.Arrow
  ( Arrow(first,second,(***)) )
import Data.Foldable
  ( foldr' )
import Data.Kind
  ( Type )
import Data.Type.Equality
  ( (:~:)(Refl) )
import Data.Typeable
  ( Typeable, eqT )
import Data.Word
  ( Word32 )
import GHC.Exts
  ( Proxy#, proxy# )

-- bytestring
import Data.ByteString
  ( ByteString )

-- unordered-containers
import Data.HashMap.Strict
  ( HashMap )
import qualified Data.HashMap.Strict as HashMap
  ( empty, insertWith, singleton, unionWith )
import Data.HashSet
  ( HashSet )
import qualified Data.HashSet as HashSet
  ( empty, insert )

-- vulkan
import Vulkan.Core10.DeviceInitialization
  ( InstanceCreateInfo, PhysicalDeviceFeatures, PhysicalDeviceProperties )
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2
  ( PhysicalDeviceFeatures2(..) )
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2
  ( PhysicalDeviceProperties2(..) )
import Vulkan.CStruct
  ( FromCStruct, ToCStruct )
import Vulkan.CStruct.Extends
  ( Chain, PeekChain, PokeChain, Extends, Extendss )
import Vulkan.Version
  ( pattern MAKE_VERSION )
import Vulkan.Zero
  ( Zero(zero) )

----------------------------------------------------------------------------

-- | A Vulkan requirement.
data Requirement where
  -- | Require a minimum Vulkan API version.
  RequireVersion
    :: { version :: Word32 }
    -> Requirement
  -- | Require a Vulkan layer.
  RequireInstanceLayer
    :: { instanceLayerName       :: ByteString
       , instanceLayerMinVersion :: Word32
       }
    -> Requirement
  -- | Require a Vulkan instance extension.
  RequireInstanceExtension
    :: { instanceExtensionLayerName  :: Maybe ByteString
       , instanceExtensionName       :: ByteString
       , instanceExtensionMinVersion :: Word32
       }
    -> Requirement
  -- | Require a Vulkan device extension.
  RequireDeviceExtension
    :: { deviceExtensionLayerName  :: Maybe ByteString
       , deviceExtensionName       :: ByteString
       , deviceExtensionMinVersion :: Word32
       }
    -> Requirement
  -- | Require a Vulkan instance setting (e.g. enable or disable a validation feature).
  RequireInstanceSetting
    :: forall struct
    .  KnownInstanceSettingStruct struct
    => { setInstanceSetting :: struct -> struct }
    -> Requirement
  -- | Require a Vulkan device feature.
  RequireFeature
    :: forall struct
    .  KnownFeatureStruct struct
    => { featureName   :: ByteString
       , checkFeature  :: struct -> Bool
       , enableFeature :: struct -> struct
       }
    -> Requirement
  -- | Require a Vulkan device property.
  RequireProperty
    :: forall struct
    .  KnownPropertyStruct struct
    => { propertyName  :: ByteString
       , checkProperty :: struct -> Bool
       }
    -> Requirement

-- | A Vulkan structure that can appear in 'InstanceCreateInfo'.
class    ( Typeable sett, Extends InstanceCreateInfo sett, Zero sett, FromCStruct sett, ToCStruct sett )
    => KnownInstanceSettingStruct sett where
instance ( Typeable sett, Extends InstanceCreateInfo sett, Zero sett, FromCStruct sett, ToCStruct sett )
    => KnownInstanceSettingStruct sett where

-- | Singleton for a Vulkan structure that can appear in 'PhysicalDeviceFeatures2'.
--
-- It is either 'PhysicalDeviceFeatures', or it 'Extends' 'PhysicalDeviceFeatures2'.
data SFeatureStruct feat where
  BasicFeatureStruct
    :: SFeatureStruct PhysicalDeviceFeatures
  ExtendedFeatureStruct
    :: ( Typeable feat, Extends PhysicalDeviceFeatures2 feat, Zero feat, FromCStruct feat, ToCStruct feat )
    => SFeatureStruct feat
-- | A Vulkan structure that can appear in 'PhysicalDeviceFeatures2'.
class KnownFeatureStruct feat where
  sFeatureStruct :: SFeatureStruct feat
instance KnownFeatureStruct PhysicalDeviceFeatures where
  sFeatureStruct = BasicFeatureStruct
instance {-# OVERLAPPABLE #-}
         ( Typeable feat, Extends PhysicalDeviceFeatures2 feat, Zero feat, FromCStruct feat, ToCStruct feat )
      => KnownFeatureStruct feat where
  sFeatureStruct = ExtendedFeatureStruct

-- | Singleton for a Vulkan structure that can appear in 'PhysicalDeviceProperties2'.
--
-- It is either 'PhysicalDeviceProperties', or it 'Extends' 'PhysicalDeviceProperties2'.
data SPropertyStruct prop where
  BasicPropertyStruct
    :: SPropertyStruct PhysicalDeviceProperties
  ExtendedPropertyStruct
    :: ( Typeable prop, Extends PhysicalDeviceProperties2 prop, FromCStruct prop, ToCStruct prop )
    => SPropertyStruct prop
-- | A Vulkan structure that can appear in 'PhysicalDeviceProperties2'.
class KnownPropertyStruct prop where
  sPropertyStruct :: SPropertyStruct prop
instance KnownPropertyStruct PhysicalDeviceProperties where
  sPropertyStruct = BasicPropertyStruct
instance {-# OVERLAPPABLE #-}
         ( Typeable prop, Extends PhysicalDeviceProperties2 prop, FromCStruct prop, ToCStruct prop )
       => KnownPropertyStruct prop where
  sPropertyStruct = ExtendedPropertyStruct


-- | Enough information to focus on any structure within a Vulkan structure chain.
class (PeekChain xs, PokeChain xs) => KnownChain (xs :: [Type]) where
  -- | If the given structure can be found within a chain, return a lens to it.
  -- Otherwise, return 'Nothing'.
  has :: forall a. Typeable a => Proxy# a -> Maybe (Chain xs -> a, (a -> a) -> (Chain xs -> Chain xs))
instance KnownChain '[] where
  has _ = Nothing
instance (Typeable x, ToCStruct x, FromCStruct x, KnownChain xs) => KnownChain (x ': xs) where 
  has (px :: Proxy# a)
    | Just Refl <- eqT @a @x
    = Just (fst,first)
    | otherwise
    = ((. snd) *** (second .)) <$> has px

data LayerName
  = BaseLayer
  | LayerName ByteString

-- | A collection of Vulkan requirements.
--
-- Parameterised over the structure chains used by 'PhysicalDeviceFeatures2' and 'PhysicalDeviceProperties2'.
data Requirements where
  Requirements
    :: forall setts feats props
    .  ( KnownChain setts, Extendss InstanceCreateInfo        setts
       , KnownChain feats, Extendss PhysicalDeviceFeatures2   feats
       , KnownChain props, Extendss PhysicalDeviceProperties2 props
       )
    => {
       -- | Minimum Vulkan API version required.
         version            :: Word32 
       -- | Required Vulkan layer names and their minimum versions.
       , instanceLayers     :: HashMap ByteString Word32
       -- | Required Vulkan instance extension names and their minimum versions,
       -- collected by the Vulkan layer name they come from.
       , instanceExtensions :: HashMap (Maybe ByteString) (HashMap ByteString Word32)
       -- | Required Vulkan device extension names and their minimum versions,
       -- collected by the Vulkan layer name they come from.
       , deviceExtensions   :: HashMap (Maybe ByteString) (HashMap ByteString Word32)
       -- | Required Vulkan instance settings ('InstanceCreateInfo' structure extension chain).
       , neededSettings     :: Chain setts
       -- | Returns all the required features that were not enabled in 'PhysicalDeviceFeatures2'.
       , missingFeatures    :: PhysicalDeviceFeatures2   feats -> HashSet ByteString
       -- | All required features, to be used in 'DeviceCreateInfo'.
       , neededFeatures     :: PhysicalDeviceFeatures2   feats
       -- | Returns all the required properties that were not satisfied in 'PhysicalDeviceProperties2'
       , missingProperties  :: PhysicalDeviceProperties2 props -> HashSet ByteString
       }
    -> Requirements

-- | Collect up requirements.
requirements :: Foldable f => f Requirement -> Requirements
requirements = foldr' addRequirement noRequire
  where
    noRequire :: Requirements
    noRequire =
      Requirements @'[] @'[] @'[]
        (MAKE_VERSION 1 0 0)
        HashMap.empty HashMap.empty HashMap.empty
        ()
        (const HashSet.empty) zero (const HashSet.empty)
    addRequirement :: Requirement -> Requirements -> Requirements
    addRequirement (RequireVersion {..}) =
      requireVersion version
    addRequirement (RequireInstanceLayer {..}) =
      requireInstanceLayer instanceLayerName instanceLayerMinVersion
    addRequirement (RequireInstanceExtension {..}) =
      requireInstanceExtension instanceExtensionLayerName instanceExtensionName instanceExtensionMinVersion
    addRequirement (RequireDeviceExtension {..}) =
      requireDeviceExtension deviceExtensionLayerName deviceExtensionName deviceExtensionMinVersion
    addRequirement (RequireInstanceSetting {..}) =
      requireInstanceSetting setInstanceSetting
    addRequirement (RequireFeature {..}) =
      requireFeature featureName checkFeature enableFeature
    addRequirement (RequireProperty {..}) =
      requireProperty propertyName checkProperty


requireVersion :: Word32 -> Requirements -> Requirements
requireVersion ver1 req@(Requirements{version = ver2}) =
  req {version = max ver1 ver2}

requireInstanceLayer :: ByteString -> Word32-> Requirements -> Requirements
requireInstanceLayer layName layVer req@(Requirements{instanceLayers = lays}) =
  req {instanceLayers = HashMap.insertWith max layName layVer lays}

requireInstanceExtension :: Maybe ByteString -> ByteString -> Word32 -> Requirements -> Requirements
requireInstanceExtension mbLayName extName extVer req@(Requirements{instanceExtensions = exts}) =
  (case mbLayName of { Just layName -> requireInstanceLayer layName 0; Nothing -> id }) $
    req {instanceExtensions = HashMap.insertWith (HashMap.unionWith max) mbLayName (HashMap.singleton extName extVer) exts}

requireDeviceExtension :: Maybe ByteString -> ByteString -> Word32 -> Requirements -> Requirements
requireDeviceExtension mbLayName extName extVer req@(Requirements{deviceExtensions = exts}) =
  (case mbLayName of { Just layName -> requireInstanceLayer layName 0; Nothing -> id }) $
    req {deviceExtensions = HashMap.insertWith (HashMap.unionWith max) mbLayName (HashMap.singleton extName extVer) exts}

requireInstanceSetting :: forall struct
                       .  ( KnownInstanceSettingStruct struct )
                       => (struct -> struct) -> Requirements -> Requirements
requireInstanceSetting setSetting
  ( Requirements ver lays instExts devExts
      (setts :: Chain setts)
      missingFeats enabledFeats missingProps
  )
  = case has @setts @struct proxy# of
      Just (_, modifySettingStruct)
        | let
            setts' :: Chain setts
            setts' = modifySettingStruct setSetting setts
        -> Requirements ver lays instExts devExts setts' missingFeats enabledFeats missingProps
      Nothing 
        | let
            setts' :: Chain (struct ': setts)
            setts' = (setSetting zero, setts)
        -> Requirements ver lays instExts devExts setts' missingFeats enabledFeats missingProps

requireFeature :: forall struct
               .  ( KnownFeatureStruct struct )
               => ByteString -> (struct -> Bool) -> (struct -> struct) -> Requirements -> Requirements
requireFeature featName checkFeat enableFeat
  ( Requirements ver lays instExts devExts setts
      (missingFeats :: PhysicalDeviceFeatures2 feats -> HashSet ByteString)
      enabledFeats@(PhysicalDeviceFeatures2{next=nextEnabledFeats,features=prevEnabledFeats})
      missingProps
  )
  = case sFeatureStruct @struct of
      BasicFeatureStruct ->
        let
          missingFeats' :: PhysicalDeviceFeatures2 feats -> HashSet ByteString
          missingFeats' feats2@(PhysicalDeviceFeatures2{features}) =
            (if checkFeat features then id else HashSet.insert featName)
              (missingFeats feats2)
          enabledFeats' :: PhysicalDeviceFeatures2 feats
          enabledFeats' = enabledFeats { features =  enableFeat prevEnabledFeats }
        in Requirements ver lays instExts devExts setts missingFeats' enabledFeats' missingProps
      ExtendedFeatureStruct
        | Just (getFeatureStruct, modifyFeatureStruct) <- has @feats @struct proxy#
        , let
            missingFeats' :: PhysicalDeviceFeatures2 feats -> HashSet ByteString
            missingFeats' feats2@(PhysicalDeviceFeatures2{next}) =
              (if checkFeat (getFeatureStruct next) then id else HashSet.insert featName)
                (missingFeats feats2)
            enabledFeats' :: PhysicalDeviceFeatures2 feats
            enabledFeats' = enabledFeats { next = modifyFeatureStruct enableFeat nextEnabledFeats }
        -> Requirements ver lays instExts devExts setts missingFeats' enabledFeats' missingProps
        | let
            missingFeats' :: PhysicalDeviceFeatures2 (struct ': feats) -> HashSet ByteString
            missingFeats' feats2@(PhysicalDeviceFeatures2{next=(struct,feats)}) =
              (if checkFeat struct then id else HashSet.insert featName)
                (missingFeats (feats2{next=feats}))
            enabledFeats' :: PhysicalDeviceFeatures2 (struct ': feats)
            enabledFeats' = enabledFeats { next = (enableFeat zero, nextEnabledFeats) }
        -> Requirements ver lays instExts devExts setts missingFeats' enabledFeats' missingProps

requireProperty :: forall struct
                .  ( KnownPropertyStruct struct )
                => ByteString -> (struct -> Bool) -> Requirements -> Requirements
requireProperty propName checkProp
  ( Requirements ver lays instExts devExts setts missingFeats enabledFeats
      (missingProps :: PhysicalDeviceProperties2 props -> HashSet ByteString)
  )
  = case sPropertyStruct @struct of
      BasicPropertyStruct ->
        let
          missingProps' :: PhysicalDeviceProperties2 props -> HashSet ByteString
          missingProps' props2@(PhysicalDeviceProperties2{properties}) =
            (if checkProp properties then id else HashSet.insert propName)
              (missingProps props2)
        in Requirements ver lays instExts devExts setts missingFeats enabledFeats missingProps'

      ExtendedPropertyStruct
        | Just (getPropertyStruct,_) <- has @props @struct proxy#
        , let
            missingProps' :: PhysicalDeviceProperties2 props -> HashSet ByteString
            missingProps' props2@(PhysicalDeviceProperties2{next}) =
              (if checkProp (getPropertyStruct next) then id else HashSet.insert propName)
                (missingProps props2)
        -> Requirements ver lays instExts devExts setts missingFeats enabledFeats missingProps'
        | let
            missingProps' :: PhysicalDeviceProperties2 (struct ': props) -> HashSet ByteString
            missingProps' props2@(PhysicalDeviceProperties2{next=(struct,props)}) =
              (if checkProp struct then id else HashSet.insert propName)
                (missingProps (props2{next=props}))
        -> Requirements ver lays instExts devExts setts missingFeats enabledFeats missingProps'
