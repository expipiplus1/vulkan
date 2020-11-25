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
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Vulkan.Requirements
  ( -- * Vulkan requirements
    Requirement(..)

    -- * Requesting certain requirements
  , Request(Require,Option,..)
  , Requests(..), ReqAndOpt(..), KnownRequestTarget(..)
  , addRequests, mkRequests

  -- * Utility functionality for handling structure chains
  , SLength(..), KnownChain(..) -- TODO: move to Vulkan.CStruct.Extends
  )
 where

-- base
import Control.Applicative
  ( liftA2 )
import Control.Arrow
  ( Arrow(first,second,(***)) )
import Control.Monad
  ( unless )
import Data.Foldable
  ( foldl' )
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
  )
import Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks, ApplicationInfo(..), Instance, InstanceCreateInfo(..)
  , PhysicalDevice, PhysicalDeviceFeatures, PhysicalDeviceProperties
  )
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2
  ( PhysicalDeviceFeatures2(..) )
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2
  ( PhysicalDeviceProperties2(..) )
import Vulkan.CStruct
  ( FromCStruct, ToCStruct )
import Vulkan.CStruct.Extends
  ( Chain, Extends, Extendss, PeekChain, PokeChain, SomeStruct(..) )
import Vulkan.Version
  ( pattern MAKE_VERSION )
import Vulkan.Zero
  ( Zero(zero) )

----------------------------------------------------------------------------
-- Vulkan requirements.

-- | A Vulkan requirement, such as an 'Instance' layer or a 'PhysicalDevice' feature.
data family Requirement (t :: Type)
-- | A requirement on a Vulkan 'Instance'.
data instance Requirement Instance where
  -- | Require a minimum Vulkan instance version.
  RequireInstanceVersion
    :: { version :: Word32 }
    -> Requirement Instance
  -- | Require a Vulkan layer.
  RequireInstanceLayer
    :: { instanceLayerName       :: ByteString
       , instanceLayerMinVersion :: Word32
       -- | Specifying order in which to enable a layer.
       -- Lower order: enabled first.
       , instanceLayerOrder      :: Int
       }
    -> Requirement Instance
  -- | Require a Vulkan instance setting (e.g. a validation feature).
  RequireInstanceSetting
    :: forall struct
    .  KnownInstanceSettingStruct struct
    => { -- | Specify a Vulkan structure that extends 'InstanceCreateInfo'.
         instanceSetting :: struct
       }
    -> Requirement Instance
  -- | Require a Vulkan instance extension.
  RequireInstanceExtension
    :: { instanceExtensionLayerName  :: Maybe ByteString
       , instanceExtensionName       :: ByteString
       , instanceExtensionMinVersion :: Word32
       }
    -> Requirement Instance
-- A requirement on a Vulkan 'PhysicalDevice'.
data instance Requirement PhysicalDevice where
  -- | Require a minimum device version.
  RequireDeviceVersion
    :: { version :: Word32 }
    -> Requirement PhysicalDevice
  -- | Require a Vulkan physical device feature.
  RequireFeature
    :: forall struct
    .  KnownFeatureStruct struct
    => { featureName   :: ByteString
       , checkFeature  :: struct -> Bool
       , enableFeature :: struct -> struct
       }
    -> Requirement PhysicalDevice
  -- | Require a Vulkan physical device property.
  RequireProperty
    :: forall struct
    .  KnownPropertyStruct struct
    => { propertyName  :: ByteString
       , checkProperty :: struct -> Bool
       }
    -> Requirement PhysicalDevice
  -- | Require a Vulkan device extension.
  RequireDeviceExtension
    :: { deviceExtensionLayerName  :: Maybe ByteString
       , deviceExtensionName       :: ByteString
       , deviceExtensionMinVersion :: Word32
       }
    -> Requirement PhysicalDevice

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

----------------------------------------------------------------------------
-- Requesting certain requirements.

-- | Request a Vulkan feature/extension/layer to be enabled (possibly optionally).
data Request t = Request { request :: Requirement t, requestIsOptional :: Bool }

{-# COMPLETE Require, Option #-}
pattern Require, Option :: Requirement t -> Request t
pattern Require en = Request en False
pattern Option  en = Request en True

-- | Keep track of required and optional terms of a given type.
data ReqAndOpt a =
  ReqAndOpt
    { req :: a
    , opt :: a
    }
  deriving stock ( Show, Functor, Foldable, Traversable )
instance Applicative ReqAndOpt where
  pure a = ReqAndOpt a a
  ReqAndOpt f g <*> ReqAndOpt a b = ReqAndOpt (f a) (g b)

-- | A collection of requests, which pertain either to an 'Instance' or to a 'PhysicalDevice'.
data family Requests (t :: Type)
-- | A collection of requests on a Vulkan 'Instance'.
--
-- Parameterised over the structure chain used in 'InstanceCreateInfo'.
data instance Requests Instance where
  InstanceRequests
    :: forall setts
    .  ( KnownChain setts, Extendss InstanceCreateInfo setts )
    => {
       -- | Required and optional minimum Vulkan instance version.
         version                 :: ReqAndOpt Word32
       -- | Required and optional Vulkan layer names and their minimum versions.
       -- Each instance layer has an ordering value which defines the order in which layers should be enabled.
       -- Lower value: enabled first.
       , instanceLayers          :: ReqAndOpt (HashMap ByteString (Int,Word32))
       -- | Required and optional Vulkan instance extension names and their minimum versions,
       -- collected by the Vulkan layer name they come from.
       , instanceExtensions      :: HashMap (Maybe ByteString) (ReqAndOpt (HashMap ByteString Word32))
       -- | Required Vulkan instance settings ('InstanceCreateInfo' structure extension chain).
       , enableSettings          :: Chain setts
       }
    -> Requests Instance
-- | A collection of requests on a Vulkan 'PhysicalDevice'
--
-- Parameterised over the structure chains used by 'PhysicalDeviceFeatures2'/'DeviceCreateInfo' and 'PhysicalDeviceProperties2'.
data instance Requests PhysicalDevice where
  PhysicalDeviceRequests
    :: forall feats props
    .  ( KnownChain feats, Extendss PhysicalDeviceFeatures2   feats, Extendss DeviceCreateInfo feats
       , KnownChain props, Extendss PhysicalDeviceProperties2 props
       )
    => {
       -- | Required and optional minimum device version.
         version                 :: ReqAndOpt Word32
       -- | Required and optional Vulkan device extension names and their minimum versions,
       -- collected by the Vulkan layer name they come from.
       , deviceExtensions        :: HashMap (Maybe ByteString) (ReqAndOpt (HashMap ByteString Word32))
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
    -> Requests PhysicalDevice

class    KnownRequestTarget (t :: Type) where
  -- | Make no requests.
  noRequests :: Requests t
  -- | Specify a single request.
  addRequest :: Request t -> Requests t -> Requests t
instance KnownRequestTarget Instance where
  noRequests =
    InstanceRequests @'[]
      (ReqAndOpt (MAKE_VERSION 1 0 0) (MAKE_VERSION 1 0 0))
      (ReqAndOpt HashMap.empty HashMap.empty)
      HashMap.empty
      ()
  addRequest (Request req isOptional) = case req of
    RequireInstanceVersion {..}
      -> requestInstanceVersion isOptional version
    RequireInstanceLayer {..}
      -> requestInstanceLayer isOptional instanceLayerName instanceLayerOrder instanceLayerMinVersion
    RequireInstanceSetting {..}
      -> requestInstanceSetting isOptional instanceSetting
    RequireInstanceExtension {..}
      -> requestInstanceExtension isOptional instanceExtensionLayerName instanceExtensionName instanceExtensionMinVersion

instance KnownRequestTarget PhysicalDevice where
  noRequests =
    PhysicalDeviceRequests @'[] @'[]
        (ReqAndOpt (MAKE_VERSION 1 0 0) (MAKE_VERSION 1 0 0))
        HashMap.empty
        (const $ ReqAndOpt HashSet.empty HashSet.empty)
        (const zero)
        (const $ ReqAndOpt HashSet.empty HashSet.empty)
  addRequest (Request req isOptional) = case req of
    RequireDeviceVersion {..}
      -> requestDeviceVersion isOptional version
    RequireDeviceExtension {..}
      -> requestDeviceExtension isOptional deviceExtensionLayerName deviceExtensionName deviceExtensionMinVersion
    RequireFeature {..}
      -> requestFeature isOptional featureName checkFeature enableFeature
    RequireProperty {..}
      -> requestProperty isOptional propertyName checkProperty

-- | Add a single request to a collection of requests.
addRequests
  :: forall f t
   . (Foldable f, KnownRequestTarget t)
  => Requests t -> f (Request t) -> Requests t
addRequests = foldl' (flip $ addRequest @t)

-- | Collect up requests.
mkRequests
  :: forall f t
   . (Foldable f, KnownRequestTarget t)
  => f (Request t) -> Requests t
mkRequests = addRequests @f @t (noRequests @t)

requestInstanceVersion :: Bool -> Word32 -> Requests Instance -> Requests Instance
requestInstanceVersion isOptional ver req@(InstanceRequests{version = ReqAndOpt reqVer optVer})
  | isOptional
  = req {version = ReqAndOpt reqVer (max ver optVer)}
  | otherwise
  = req {version = ReqAndOpt (max ver reqVer) optVer}

requestDeviceVersion :: Bool -> Word32 -> Requests PhysicalDevice -> Requests PhysicalDevice
requestDeviceVersion isOptional ver req@(PhysicalDeviceRequests{version = ReqAndOpt reqVer optVer})
  | isOptional
  = req {version = ReqAndOpt reqVer (max ver optVer)}
  | otherwise
  = req {version = ReqAndOpt (max ver reqVer) optVer}

requestInstanceLayer :: Bool -> ByteString -> Int -> Word32 -> Requests Instance -> Requests Instance
requestInstanceLayer isOptional layName layOrder layVer req@(InstanceRequests{instanceLayers = ReqAndOpt reqLays optLays})
  | isOptional
  = req {instanceLayers = ReqAndOpt reqLays (HashMap.insertWith minMax layName (layOrder,layVer) optLays)}
  | otherwise
  = req {instanceLayers = ReqAndOpt (HashMap.insertWith minMax layName (layOrder,layVer) reqLays) optLays}
  where
    minMax :: (Int, Word32) -> (Int, Word32) -> (Int, Word32)
    minMax (o1,v1) (o2,v2) = (min o1 o2, max v1 v2)

requestInstanceExtension :: Bool -> Maybe ByteString -> ByteString -> Word32 -> Requests Instance -> Requests Instance
requestInstanceExtension isOptional mbLayName extName extVer req@(InstanceRequests{instanceExtensions = exts})
  | isOptional
  = req
      { instanceExtensions =
        HashMap.insertWith (liftA2 $ HashMap.unionWith max)
          mbLayName
          (ReqAndOpt (HashMap.singleton extName extVer) HashMap.empty)
          exts
      }
  | otherwise
  = req
      { instanceExtensions =
        HashMap.insertWith (liftA2 $ HashMap.unionWith max)
          mbLayName
          (ReqAndOpt HashMap.empty (HashMap.singleton extName extVer))
          exts
      }

requestDeviceExtension :: Bool -> Maybe ByteString -> ByteString -> Word32 -> Requests PhysicalDevice -> Requests PhysicalDevice
requestDeviceExtension isOptional mbLayName extName extVer req@(PhysicalDeviceRequests{deviceExtensions = exts})
  | isOptional
  = req
      { deviceExtensions =
        HashMap.insertWith (liftA2 $ HashMap.unionWith max)
          mbLayName
          (ReqAndOpt (HashMap.singleton extName extVer) HashMap.empty)
          exts
      }
  | otherwise
  = req
      { deviceExtensions =
        HashMap.insertWith (liftA2 $ HashMap.unionWith max)
          mbLayName
          (ReqAndOpt HashMap.empty (HashMap.singleton extName extVer))
          exts
      }

requestInstanceSetting
  :: forall struct
  .  ( KnownInstanceSettingStruct struct )
  => Bool -> struct -> Requests Instance -> Requests Instance
requestInstanceSetting _isOptional setting
  ( InstanceRequests ver lays instExts
      (setts :: Chain setts)
  )
  = case has @setts @struct proxy# of
      -- TODO: throw a warning when a setting is set twice.
      Just (_, modifySettingStruct)
        | let
            setts' :: Chain setts
            setts' = modifySettingStruct (const setting) setts
        -> InstanceRequests ver lays instExts setts'
      Nothing
        | let
            setts' :: Chain (struct ': setts)
            setts' = (setting, setts)
        -> InstanceRequests ver lays instExts setts'

requestFeature
  :: forall struct
  .  ( KnownFeatureStruct struct )
  => Bool -> ByteString -> (struct -> Bool) -> (struct -> struct) -> Requests PhysicalDevice -> Requests PhysicalDevice
requestFeature isOptional featName checkFeat enableFeat
  ( PhysicalDeviceRequests ver devExts
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
        in PhysicalDeviceRequests ver devExts missingFeats' enabledFeats' missingProps
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
        -> PhysicalDeviceRequests ver devExts missingFeats' enabledFeats' missingProps
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
        -> PhysicalDeviceRequests ver devExts missingFeats' enabledFeats' missingProps

requestProperty
  :: forall struct
  .  ( KnownPropertyStruct struct )
  => Bool -> ByteString -> (struct -> Bool) -> Requests PhysicalDevice -> Requests PhysicalDevice
requestProperty isOptional propName checkProp
  ( PhysicalDeviceRequests ver devExts missingFeats enabledFeats
      (missingProps :: PhysicalDeviceProperties2 props -> ReqAndOpt (HashSet ByteString))
  )
  = case sPropertyStruct @struct of
      BasicPropertyStruct ->
        let
          missingProps' :: PhysicalDeviceProperties2 props -> ReqAndOpt (HashSet ByteString)
          missingProps' props2@(PhysicalDeviceProperties2{properties}) =
            insertReqOptUnless (checkProp properties) propName isOptional
              (missingProps props2)
        in PhysicalDeviceRequests ver devExts missingFeats enabledFeats missingProps'
      ExtendedPropertyStruct
        | Just (getPropertyStruct,_) <- has @props @struct proxy#
        , let
            missingProps' :: PhysicalDeviceProperties2 props -> ReqAndOpt (HashSet ByteString)
            missingProps' props2@(PhysicalDeviceProperties2{next}) =
              insertReqOptUnless (checkProp (getPropertyStruct next)) propName isOptional
                (missingProps props2)
        -> PhysicalDeviceRequests ver devExts missingFeats enabledFeats missingProps'
        | let
            missingProps' :: PhysicalDeviceProperties2 (struct ': props) -> ReqAndOpt (HashSet ByteString)
            missingProps' props2@(PhysicalDeviceProperties2{next=(struct,props)}) =
              insertReqOptUnless (checkProp struct) propName isOptional
                (missingProps (props2{next=props}))
        -> PhysicalDeviceRequests ver devExts missingFeats enabledFeats missingProps'

insertReqOptUnless :: Bool -> ByteString -> Bool -> ReqAndOpt (HashSet ByteString) -> ReqAndOpt (HashSet ByteString)
insertReqOptUnless True  _    _     = id
insertReqOptUnless False name True  = \ (ReqAndOpt reqs opts) -> ReqAndOpt reqs (HashSet.insert name opts)
insertReqOptUnless False name False = \ (ReqAndOpt reqs opts) -> ReqAndOpt (HashSet.insert name reqs) opts
