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

module Vulkan.Requirement
  ( -- * Vulkan requirements
    InstanceRequirement(..)
  , DeviceRequirement(..)
  , -- * Utility functionality for handling structure chains
    KnownFeatureStruct(..)
  , KnownPropertyStruct(..)
  , SFeatureStruct(..)
  , SPropertyStruct(..)
  ) where

-- base
import Data.Typeable
  ( Typeable )
import Data.Word
  ( Word32 )

-- bytestring
import Data.ByteString
  ( ByteString )

-- vulkan
import Vulkan.Core10.Device
  ( DeviceCreateInfo(..)
  )
import Vulkan.Core10.DeviceInitialization
  ( PhysicalDeviceFeatures, PhysicalDeviceProperties
  )
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2
    ( PhysicalDeviceFeatures2(..), PhysicalDeviceProperties2(..) )
import Vulkan.CStruct
  ( FromCStruct, ToCStruct )
import Vulkan.CStruct.Extends
  ( Extends )
import Vulkan.Zero

----------------------------------------------------------------
-- Instance Requirements
----------------------------------------------------------------

-- | A requirement on a Vulkan 'Instance'.
data InstanceRequirement where
  -- | Require a minimum Vulkan instance version.
  RequireInstanceVersion
    :: { version :: Word32 }
    -> InstanceRequirement
  -- | Require a Vulkan layer.
  RequireInstanceLayer
    :: { instanceLayerName       :: ByteString
       , instanceLayerMinVersion :: Word32
         -- ^ The 'implementationVersion' of the layer must meet or exceed this
         -- version
       }
    -> InstanceRequirement
  -- | Require a Vulkan instance extension.
  RequireInstanceExtension
    :: { instanceExtensionLayerName  :: Maybe ByteString
       , instanceExtensionName       :: ByteString
       , instanceExtensionMinVersion :: Word32
       }
    -> InstanceRequirement

----------------------------------------------------------------
-- Device Requirements
----------------------------------------------------------------

-- A requirement on a Vulkan 'PhysicalDevice'.
data DeviceRequirement where
  -- | Require a minimum device version.
  RequireDeviceVersion
    :: { version :: Word32 }
    -> DeviceRequirement
  -- | Require a Vulkan physical device feature.
  RequireDeviceFeature
    :: forall struct
    .  KnownFeatureStruct struct
    => { featureName   :: ByteString
       , checkFeature  :: struct -> Bool
       , enableFeature :: struct -> struct
       }
    -> DeviceRequirement
  -- | Require a Vulkan physical device property.
  RequireDeviceProperty
    :: forall struct
    .  KnownPropertyStruct struct
    => { propertyName  :: ByteString
       , checkProperty :: struct -> Bool
       }
    -> DeviceRequirement
  -- | Require a Vulkan device extension.
  RequireDeviceExtension
    :: { deviceExtensionLayerName  :: Maybe ByteString
       , deviceExtensionName       :: ByteString
       , deviceExtensionMinVersion :: Word32
       }
    -> DeviceRequirement

-- | Singleton for a Vulkan structure that can appear in 'PhysicalDeviceFeatures2'.
--
-- It is either 'PhysicalDeviceFeatures', or it 'Extends' 'PhysicalDeviceFeatures2'.
data SFeatureStruct feat where
  BasicFeatureStruct
    :: SFeatureStruct PhysicalDeviceFeatures
  ExtendedFeatureStruct
    :: ( Show feat
       , Extends PhysicalDeviceFeatures2 feat, Extends DeviceCreateInfo feat
       , Zero feat, FromCStruct feat, ToCStruct feat
       )
    => SFeatureStruct feat

-- | A Vulkan structure that can appear in 'PhysicalDeviceFeatures2'.
class Typeable feat => KnownFeatureStruct feat where
  sFeatureStruct :: SFeatureStruct feat

instance KnownFeatureStruct PhysicalDeviceFeatures where
  sFeatureStruct = BasicFeatureStruct

instance {-# OVERLAPPABLE #-}
         ( Typeable feat, Show feat
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
class Typeable prop => KnownPropertyStruct prop where
  sPropertyStruct :: SPropertyStruct prop
instance KnownPropertyStruct PhysicalDeviceProperties where
  sPropertyStruct = BasicPropertyStruct
instance {-# OVERLAPPABLE #-}
         ( Typeable prop, Extends PhysicalDeviceProperties2 prop, FromCStruct prop, ToCStruct prop )
       => KnownPropertyStruct prop where
  sPropertyStruct = ExtendedPropertyStruct
