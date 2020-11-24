{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Vulkan.Requirements where

-- base
import Control.Arrow
  ( Arrow(first,second,(***)) )
import Data.Foldable
  ( for_, foldr' )
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

-- vulkan
import Vulkan.Core10.DeviceInitialization
  ( PhysicalDeviceFeatures, PhysicalDeviceProperties )
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
  -- | Require a Vulkan extension.
  RequireExtension
    :: { extension :: ByteString }
    -> Requirement
  -- | Require a Vulkan device feature.
  RequireFeature
    :: forall struct
    .  KnownFeature struct
    => { featureName       :: ByteString
       , checkFeature      :: struct -> Bool
       , enableFeature     :: struct -> struct
       , requireMinVersion :: Maybe Word32
       , requireExtensions :: [ByteString]
       }
    -> Requirement
  -- | Require a Vulkan device property.
  RequireProperty
    :: forall struct
    .  KnownProperty struct
    => { propertyName      :: ByteString
       , checkProperty     :: struct -> Bool
       , requireMinVersion :: Maybe Word32
       , requireExtensions :: [ByteString]
       }
    -> Requirement

data SFeature feat where
  BasicFeature
    :: SFeature PhysicalDeviceFeatures
  ExtendedFeature
    :: ( Typeable feat, Extends PhysicalDeviceFeatures2 feat, Zero feat, FromCStruct feat, ToCStruct feat )
    => SFeature feat
class KnownFeature feat where
  sFeature :: SFeature feat
instance KnownFeature PhysicalDeviceFeatures where
  sFeature = BasicFeature
instance {-# OVERLAPPABLE #-}
         ( Typeable feat, Extends PhysicalDeviceFeatures2 feat, Zero feat, FromCStruct feat, ToCStruct feat )
      => KnownFeature feat where
  sFeature = ExtendedFeature

data SProperty prop where
  BasicProperty
    :: SProperty PhysicalDeviceProperties
  ExtendedProperty
    :: ( Typeable prop, Extends PhysicalDeviceProperties2 prop, FromCStruct prop, ToCStruct prop )
    => SProperty prop
class KnownProperty prop where
  sProperty :: SProperty prop
instance KnownProperty PhysicalDeviceProperties where
  sProperty = BasicProperty
instance {-# OVERLAPPABLE #-}
         ( Typeable prop, Extends PhysicalDeviceProperties2 prop, FromCStruct prop, ToCStruct prop )
       => KnownProperty prop where
  sProperty = ExtendedProperty


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


-- | A collection of Vulkan requirements.
--
-- Parameterised over the structure chains used by 'PhysicalDeviceFeatures2' and 'PhysicalDeviceProperties2'.
data Requirements where
  Requirements
    :: forall feats props
    .  ( KnownChain feats, Extendss PhysicalDeviceFeatures2   feats
       , KnownChain props, Extendss PhysicalDeviceProperties2 props
       )
    => { version           :: Word32 -- ^ Minimum Vulkan API version required.
       , extensions        :: [ ByteString ] -- ^ Required Vulkan device extension names.
       , missingFeatures   :: PhysicalDeviceFeatures2   feats -> [ByteString] -- ^ Returns all the required features that were not enabled in 'PhysicalDeviceFeatures2'.
       , neededFeatures    :: PhysicalDeviceFeatures2   feats -- ^ All required features, to be used in 'DeviceCreateInfo'.
       , missingProperties :: PhysicalDeviceProperties2 props -> [ByteString] -- ^ Returns all the required properties that were not satisfied in 'PhysicalDeviceProperties2'
       }
    -> Requirements

-- | Collect up requirements.
requirements :: Foldable f => f Requirement -> Requirements
requirements = foldr' addRequirement noRequire
  where
    noRequire :: Requirements
    noRequire = Requirements @'[] @'[] (MAKE_VERSION 1 0 0) [] (const []) zero (const [])
    addRequirement :: Requirement -> Requirements -> Requirements
    addRequirement (RequireVersion {version}) = requireVersion version
    addRequirement (RequireExtension {extension}) = requireExtension extension
    addRequirement (RequireFeature {..}) = do
      for_ requireMinVersion requireVersion
      for_ requireExtensions requireExtension
      requireFeature featureName checkFeature enableFeature
    addRequirement (RequireProperty {..}) = do
      for_ requireMinVersion requireVersion
      for_ requireExtensions requireExtension
      requireProperty propertyName checkProperty


requireVersion :: Word32 -> Requirements -> Requirements
requireVersion ver1 req@(Requirements{version = ver2}) = req {version = max ver1 ver2}

requireExtension :: ByteString -> Requirements -> Requirements
requireExtension ext req@(Requirements{extensions = exts}) = req {extensions = ext:exts}

requireFeature :: forall struct
               .  ( KnownFeature struct )
               => ByteString -> (struct -> Bool) -> (struct -> struct) -> Requirements -> Requirements
requireFeature featName checkFeat enableFeat
  ( Requirements ver exts
      (missingFeats :: PhysicalDeviceFeatures2 feats -> [ByteString])
      enabledFeats@(PhysicalDeviceFeatures2{next=nextEnabledFeats,features=prevEnabledFeats})
      missingProps
  )
  = case sFeature @struct of
      BasicFeature ->
        let
          missingFeats' :: PhysicalDeviceFeatures2 feats -> [ByteString]
          missingFeats' feats2@(PhysicalDeviceFeatures2{features}) =
            (if checkFeat features then id else (featName:))
              (missingFeats feats2)
          enabledFeats' :: PhysicalDeviceFeatures2 feats
          enabledFeats' = enabledFeats { features =  enableFeat prevEnabledFeats }
        in Requirements ver exts missingFeats' enabledFeats' missingProps
      ExtendedFeature
        | Just (getFeatureStruct, modifyFeatureStruct) <- has @feats @struct proxy#
        , let
            missingFeats' :: PhysicalDeviceFeatures2 feats -> [ByteString]
            missingFeats' feats2@(PhysicalDeviceFeatures2{next}) =
              (if checkFeat (getFeatureStruct next) then id else (featName:))
                (missingFeats feats2)
            enabledFeats' :: PhysicalDeviceFeatures2 feats
            enabledFeats' = enabledFeats { next = modifyFeatureStruct enableFeat nextEnabledFeats }
        -> Requirements ver exts missingFeats' enabledFeats' missingProps
        | let
            missingFeats' :: PhysicalDeviceFeatures2 (struct ': feats) -> [ByteString]
            missingFeats' feats2@(PhysicalDeviceFeatures2{next=(struct,feats)}) =
              (if checkFeat struct then id else (featName:))
                (missingFeats (feats2{next=feats}))
            enabledFeats' :: PhysicalDeviceFeatures2 (struct ': feats)
            enabledFeats' = enabledFeats { next = (enableFeat zero, nextEnabledFeats) }
        -> Requirements ver exts missingFeats' enabledFeats' missingProps

requireProperty :: forall struct
                .  ( KnownProperty struct )
                => ByteString -> (struct -> Bool) -> Requirements -> Requirements
requireProperty propName checkProp
  ( Requirements ver exts missingFeats enabledFeats
      (missingProps :: PhysicalDeviceProperties2 props -> [ByteString])
  )
  = case sProperty @struct of
      BasicProperty ->
        let
          missingProps' :: PhysicalDeviceProperties2 props -> [ByteString]
          missingProps' props2@(PhysicalDeviceProperties2{properties}) =
            (if checkProp properties then id else (propName:))
              (missingProps props2)
        in Requirements ver exts missingFeats enabledFeats missingProps'

      ExtendedProperty
        | Just (getPropertyStruct,_) <- has @props @struct proxy#
        , let
            missingProps' :: PhysicalDeviceProperties2 props -> [ByteString]
            missingProps' props2@(PhysicalDeviceProperties2{next}) =
              (if checkProp (getPropertyStruct next) then id else (propName:))
                (missingProps props2)
        -> Requirements ver exts missingFeats enabledFeats missingProps'
        | let
            missingProps' :: PhysicalDeviceProperties2 (struct ': props) -> [ByteString]
            missingProps' props2@(PhysicalDeviceProperties2{next=(struct,props)}) =
              (if checkProp struct then id else (propName:))
                (missingProps (props2{next=props}))
        -> Requirements ver exts missingFeats enabledFeats missingProps'
