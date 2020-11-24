{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DerivingStrategies      #-}
{-# LANGUAGE DeriveTraversable       #-}
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
import Control.Applicative
  ( liftA2 )
import Control.Arrow
  ( Arrow(first,second,(***)) )
import Control.Monad
  ( unless )
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

-- transformers
import Control.Monad.IO.Class
  ( MonadIO )

-- unordered-containers
import Data.HashMap.Strict
  ( HashMap )
import qualified Data.HashMap.Strict as HashMap
  ( elems, empty, insertWith, keys, singleton, unionWith )
import Data.HashSet
  ( HashSet )
import qualified Data.HashSet as HashSet
  ( empty, insert, member, null )

-- vector
import qualified Data.Vector as Boxed
  ( Vector )
import qualified Data.Vector as Boxed.Vector
  ( fromList )

-- vulkan
import Vulkan.Core10.Device
  ( Device, DeviceCreateInfo(..), DeviceQueueCreateInfo
  , createDevice, destroyDevice
  )
import Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks, ApplicationInfo(..), Instance, InstanceCreateInfo(..)
  , PhysicalDevice, PhysicalDeviceFeatures, PhysicalDeviceProperties
  , createInstance, destroyInstance
  , getPhysicalDeviceFeatures, getPhysicalDeviceProperties
  )
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2
  ( PhysicalDeviceFeatures2(..), getPhysicalDeviceFeatures2 )
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2
  ( PhysicalDeviceProperties2(..), getPhysicalDeviceProperties2 )
import Vulkan.CStruct
  ( FromCStruct, ToCStruct )
import Vulkan.CStruct.Extends
  ( Chain, Extends, Extendss, PeekChain, PokeChain, SomeStruct(..) )
import Vulkan.Version
  ( pattern MAKE_VERSION )
import Vulkan.Zero
  ( Zero(zero) )

----------------------------------------------------------------------------

-- | A Vulkan requirement, possibly optional.
type OptionalRequirement = ( Requirement, Bool )

{-# COMPLETE Optionally, Necessarily #-}
pattern Optionally, Necessarily :: Requirement -> ( Requirement, Bool )
pattern Optionally  r = ( r, True  )
pattern Necessarily r = ( r, False )

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
    => { settingName        :: ByteString
       , setInstanceSetting :: struct -> struct
       }
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
    :: ( Typeable feat
       , Extends PhysicalDeviceFeatures2 feat, Extends DeviceCreateInfo feat
       , Zero feat, FromCStruct feat, ToCStruct feat
       )
    => SFeatureStruct feat
-- | A Vulkan structure that can appear in 'PhysicalDeviceFeatures2'.
class KnownFeatureStruct feat where
  sFeatureStruct :: SFeatureStruct feat
instance KnownFeatureStruct PhysicalDeviceFeatures where
  sFeatureStruct = BasicFeatureStruct
instance {-# OVERLAPPABLE #-}
         ( Typeable feat
         , Extends PhysicalDeviceFeatures2 feat, Extends DeviceCreateInfo feat
         , Zero feat, FromCStruct feat, ToCStruct feat
         )
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

-- | Singleton allowing inspection of the length of a list.
data SLength (xs :: [Type]) where
  SNil  :: SLength '[]
  SCons :: SLength xs -> SLength (x ': xs)

-- | Enough information to focus on any structure within a Vulkan structure chain.
class (PeekChain xs, PokeChain xs) => KnownChain (xs :: [Type]) where
  sLength :: SLength xs
  -- | If the given structure can be found within a chain, return a lens to it.
  -- Otherwise, return 'Nothing'.
  has :: forall a. Typeable a => Proxy# a -> Maybe (Chain xs -> a, (a -> a) -> (Chain xs -> Chain xs))
instance KnownChain '[] where
  sLength = SNil
  has _ = Nothing
instance (Typeable x, ToCStruct x, FromCStruct x, KnownChain xs) => KnownChain (x ': xs) where 
  sLength = SCons (sLength @xs)
  has (px :: Proxy# a)
    | Just Refl <- eqT @a @x
    = Just (fst,first)
    | otherwise
    = ((. snd) *** (second .)) <$> has px

data ReqAndOpt a =
  ReqAndOpt
    { req :: a
    , opt :: a
    }
  deriving stock ( Show, Functor, Foldable, Traversable )
instance Applicative ReqAndOpt where
  pure a = ReqAndOpt a a
  ReqAndOpt f g <*> ReqAndOpt a b = ReqAndOpt (f a) (g b)

-- | A collection of Vulkan requirements.
--
-- Parameterised over the structure chains used by 'PhysicalDeviceFeatures2' and 'PhysicalDeviceProperties2'.
data Requirements where
  Requirements
    :: forall setts feats props
    .  ( KnownChain setts, Extendss InstanceCreateInfo        setts
       , KnownChain feats, Extendss PhysicalDeviceFeatures2   feats, Extendss DeviceCreateInfo feats
       , KnownChain props, Extendss PhysicalDeviceProperties2 props
       )
    => {
       -- | Required and optional minimum Vulkan API version.
         version                 :: ReqAndOpt Word32
       -- | Required and optional Vulkan layer names and their minimum versions.
       , instanceLayers          :: ReqAndOpt (HashMap ByteString Word32)
       -- | Required and optional Vulkan instance extension names and their minimum versions,
       -- collected by the Vulkan layer name they come from.
       , instanceExtensions      :: HashMap (Maybe ByteString) (ReqAndOpt (HashMap ByteString Word32))
       -- | Required and optional Vulkan device extension names and their minimum versions,
       -- collected by the Vulkan layer name they come from.
       , deviceExtensions        :: HashMap (Maybe ByteString) (ReqAndOpt (HashMap ByteString Word32))
       -- | Required Vulkan instance settings ('InstanceCreateInfo' structure extension chain).
       --
       -- Argument: optional settings that the function will __not__ attempt to enable.
       , enableSettingsOtherThan :: HashSet ByteString -> Chain setts
       -- | Returns all the required and optional features that were not enabled in 'PhysicalDeviceFeatures2'.
       , missingFeatures         :: PhysicalDeviceFeatures2   feats -> ReqAndOpt (HashSet ByteString)
       -- | All required features, to be used in 'DeviceCreateInfo'.
       --
       -- Argument: optional features that should __not__ be enabled.
       -- These must be the missing optional features returned by 'missingFeatures'.
       , enableFeaturesOtherThan :: HashSet ByteString -> PhysicalDeviceFeatures2 feats
       -- | Returns all the required and optional properties that were not satisfied in 'PhysicalDeviceProperties2'
       , missingProperties       :: PhysicalDeviceProperties2 props -> ReqAndOpt (HashSet ByteString)
       }
    -> Requirements

-- | Collect up requirements.
requirements :: Foldable f => f OptionalRequirement -> Requirements
requirements = foldr' addRequirement noRequire
  where
    noRequire :: Requirements
    noRequire =
      Requirements @'[] @'[] @'[]
        (ReqAndOpt (MAKE_VERSION 1 0 0) (MAKE_VERSION 1 0 0))
        (ReqAndOpt HashMap.empty HashMap.empty)
        HashMap.empty
        HashMap.empty
        (const ())
        (const $ ReqAndOpt HashSet.empty HashSet.empty)
        (const zero)
        (const $ ReqAndOpt HashSet.empty HashSet.empty)

-- | Add a single requirement to a collection of requirements.
addRequirement :: OptionalRequirement -> Requirements -> Requirements
addRequirement (RequireVersion {..}, isOptional) =
  requireVersion isOptional version
addRequirement (RequireInstanceLayer {..}, isOptional) =
  requireInstanceLayer isOptional instanceLayerName instanceLayerMinVersion
addRequirement (RequireInstanceExtension {..}, isOptional) =
  requireInstanceExtension isOptional instanceExtensionLayerName instanceExtensionName instanceExtensionMinVersion
addRequirement (RequireDeviceExtension {..}, isOptional) =
  requireDeviceExtension isOptional deviceExtensionLayerName deviceExtensionName deviceExtensionMinVersion
addRequirement (RequireInstanceSetting {..}, isOptional) =
  requireInstanceSetting isOptional settingName setInstanceSetting
addRequirement (RequireFeature {..}, isOptional) =
  requireFeature isOptional featureName checkFeature enableFeature
addRequirement (RequireProperty {..}, isOptional) =
  requireProperty isOptional propertyName checkProperty


requireVersion :: Bool -> Word32 -> Requirements -> Requirements
requireVersion isOptional ver req@(Requirements{version = ReqAndOpt reqVer optVer})
  | isOptional
  = req {version = ReqAndOpt reqVer (max ver optVer)}
  | otherwise
  = req {version = ReqAndOpt (max ver reqVer) optVer}

requireInstanceLayer :: Bool -> ByteString -> Word32-> Requirements -> Requirements
requireInstanceLayer isOptional layName layVer req@(Requirements{instanceLayers = ReqAndOpt reqLays optLays})
  | isOptional
  = req {instanceLayers = ReqAndOpt reqLays (HashMap.insertWith max layName layVer optLays)}
  | otherwise
  = req {instanceLayers = ReqAndOpt (HashMap.insertWith max layName layVer reqLays) optLays}

requireInstanceExtension :: Bool -> Maybe ByteString -> ByteString -> Word32 -> Requirements -> Requirements
requireInstanceExtension isOptional mbLayName extName extVer req@(Requirements{instanceExtensions = exts})
  | isOptional
  = (case mbLayName of { Just layName -> requireInstanceLayer isOptional layName 0; Nothing -> id }) $
      req
        { instanceExtensions =
          HashMap.insertWith (liftA2 $ HashMap.unionWith max)
            mbLayName
            (ReqAndOpt (HashMap.singleton extName extVer) HashMap.empty)
            exts
        }
  | otherwise
  = (case mbLayName of { Just layName -> requireInstanceLayer isOptional layName 0; Nothing -> id }) $
      req
        { instanceExtensions =
          HashMap.insertWith (liftA2 $ HashMap.unionWith max)
            mbLayName
            (ReqAndOpt HashMap.empty (HashMap.singleton extName extVer))
            exts
        }

requireDeviceExtension :: Bool -> Maybe ByteString -> ByteString -> Word32 -> Requirements -> Requirements
requireDeviceExtension isOptional mbLayName extName extVer req@(Requirements{deviceExtensions = exts})
  | isOptional
  = (case mbLayName of { Just layName -> requireInstanceLayer isOptional layName 0; Nothing -> id }) $
      req
        { deviceExtensions =
          HashMap.insertWith (liftA2 $ HashMap.unionWith max)
            mbLayName
            (ReqAndOpt (HashMap.singleton extName extVer) HashMap.empty)
            exts
        }
  | otherwise
  = (case mbLayName of { Just layName -> requireInstanceLayer isOptional layName 0; Nothing -> id }) $
      req
        { deviceExtensions =
          HashMap.insertWith (liftA2 $ HashMap.unionWith max)
            mbLayName
            (ReqAndOpt HashMap.empty (HashMap.singleton extName extVer))
            exts
        }

requireInstanceSetting
  :: forall struct
  .  ( KnownInstanceSettingStruct struct )
  => Bool -> ByteString -> (struct -> struct) -> Requirements -> Requirements
requireInstanceSetting isOptional settingName setSetting
  ( Requirements ver lays instExts devExts
      (setts :: HashSet ByteString -> Chain setts)
      missingFeats enableFeats missingProps
  )
  = case has @setts @struct proxy# of
      Just (_, modifySettingStruct)
        | let
            setts' :: HashSet ByteString -> Chain setts
            setts' don'tEnable
              | isOptional && settingName `HashSet.member` don'tEnable
              = setts don'tEnable
              | otherwise
              = modifySettingStruct setSetting (setts don'tEnable)
        -> Requirements ver lays instExts devExts setts' missingFeats enableFeats missingProps
      Nothing
        | let
            setts' :: HashSet ByteString -> Chain (struct ': setts)
            setts' don'tEnable
              | isOptional && settingName `HashSet.member` don'tEnable
              = (zero, setts don'tEnable)
              | otherwise
              = (setSetting zero, setts don'tEnable)
        -> Requirements ver lays instExts devExts setts' missingFeats enableFeats missingProps

requireFeature
  :: forall struct
  .  ( KnownFeatureStruct struct )
  => Bool -> ByteString -> (struct -> Bool) -> (struct -> struct) -> Requirements -> Requirements
requireFeature isOptional featName checkFeat enableFeat
  ( Requirements ver lays instExts devExts setts
      (missingFeats :: PhysicalDeviceFeatures2 feats -> ReqAndOpt (HashSet ByteString))
      enableFeats
      missingProps
  )
  = case sFeatureStruct @struct of
      BasicFeatureStruct ->
        let
          missingFeats' :: PhysicalDeviceFeatures2 feats -> ReqAndOpt (HashSet ByteString)
          missingFeats' feats2@(PhysicalDeviceFeatures2{features}) =
            insertReqOptUnless (checkFeat features) featName isOptional
              (missingFeats feats2)
          enabledFeats' :: HashSet ByteString -> PhysicalDeviceFeatures2 feats
          enabledFeats' don'tEnable
            | isOptional && featName `HashSet.member` don'tEnable
            = enableFeats don'tEnable
            | enabled@(PhysicalDeviceFeatures2{features=prevEnabledFeats})
                <- enableFeats don'tEnable
            = enabled { features = enableFeat prevEnabledFeats }
        in Requirements ver lays instExts devExts setts missingFeats' enabledFeats' missingProps
      ExtendedFeatureStruct
        | Just (getFeatureStruct, modifyFeatureStruct) <- has @feats @struct proxy#
        , let
            missingFeats' :: PhysicalDeviceFeatures2 feats -> ReqAndOpt (HashSet ByteString)
            missingFeats' feats2@(PhysicalDeviceFeatures2{next}) =
              insertReqOptUnless (checkFeat (getFeatureStruct next)) featName isOptional
                (missingFeats feats2)
            enabledFeats' :: HashSet ByteString -> PhysicalDeviceFeatures2 feats
            enabledFeats' don'tEnable
              | isOptional && featName `HashSet.member` don'tEnable
              = enableFeats don'tEnable
              | enabled@(PhysicalDeviceFeatures2{next=nextEnabledFeats})
                <- enableFeats don'tEnable
              = enabled { next = modifyFeatureStruct enableFeat nextEnabledFeats }
        -> Requirements ver lays instExts devExts setts missingFeats' enabledFeats' missingProps
        | let
            missingFeats' :: PhysicalDeviceFeatures2 (struct ': feats) -> ReqAndOpt (HashSet ByteString)
            missingFeats' feats2@(PhysicalDeviceFeatures2{next=(struct,feats)}) =
              insertReqOptUnless (checkFeat struct) featName isOptional
                (missingFeats (feats2{next=feats}))
            enabledFeats' :: HashSet ByteString -> PhysicalDeviceFeatures2 (struct ': feats)
            enabledFeats' don'tEnable
              | let
                  struct :: struct
                  struct
                    | isOptional && featName `HashSet.member` don'tEnable
                    = zero
                    | otherwise
                    = enableFeat zero
              , enabled@(PhysicalDeviceFeatures2{next=nextEnabledFeats})
                <- enableFeats don'tEnable
              = enabled { next = (struct, nextEnabledFeats) }
        -> Requirements ver lays instExts devExts setts missingFeats' enabledFeats' missingProps

requireProperty
  :: forall struct
  .  ( KnownPropertyStruct struct )
  => Bool -> ByteString -> (struct -> Bool) -> Requirements -> Requirements
requireProperty isOptional propName checkProp
  ( Requirements ver lays instExts devExts setts missingFeats enabledFeats
      (missingProps :: PhysicalDeviceProperties2 props -> ReqAndOpt (HashSet ByteString))
  )
  = case sPropertyStruct @struct of
      BasicPropertyStruct ->
        let
          missingProps' :: PhysicalDeviceProperties2 props -> ReqAndOpt (HashSet ByteString)
          missingProps' props2@(PhysicalDeviceProperties2{properties}) =
            insertReqOptUnless (checkProp properties) propName isOptional
              (missingProps props2)
        in Requirements ver lays instExts devExts setts missingFeats enabledFeats missingProps'
      ExtendedPropertyStruct
        | Just (getPropertyStruct,_) <- has @props @struct proxy#
        , let
            missingProps' :: PhysicalDeviceProperties2 props -> ReqAndOpt (HashSet ByteString)
            missingProps' props2@(PhysicalDeviceProperties2{next}) =
              insertReqOptUnless (checkProp (getPropertyStruct next)) propName isOptional
                (missingProps props2)
        -> Requirements ver lays instExts devExts setts missingFeats enabledFeats missingProps'
        | let
            missingProps' :: PhysicalDeviceProperties2 (struct ': props) -> ReqAndOpt (HashSet ByteString)
            missingProps' props2@(PhysicalDeviceProperties2{next=(struct,props)}) =
              insertReqOptUnless (checkProp struct) propName isOptional
                (missingProps (props2{next=props}))
        -> Requirements ver lays instExts devExts setts missingFeats enabledFeats missingProps'

insertReqOptUnless :: Bool -> ByteString -> Bool -> ReqAndOpt (HashSet ByteString) -> ReqAndOpt (HashSet ByteString)
insertReqOptUnless True  _    _     = id
insertReqOptUnless False name True  = \ (ReqAndOpt reqs opts) -> ReqAndOpt reqs (HashSet.insert name opts)
insertReqOptUnless False name False = \ (ReqAndOpt reqs opts) -> ReqAndOpt (HashSet.insert name reqs) opts
