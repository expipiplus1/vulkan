{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_fragment_shader_interlock  ( PhysicalDeviceFragmentShaderInterlockFeaturesEXT(..)
                                                           , EXT_FRAGMENT_SHADER_INTERLOCK_SPEC_VERSION
                                                           , pattern EXT_FRAGMENT_SHADER_INTERLOCK_SPEC_VERSION
                                                           , EXT_FRAGMENT_SHADER_INTERLOCK_EXTENSION_NAME
                                                           , pattern EXT_FRAGMENT_SHADER_INTERLOCK_EXTENSION_NAME
                                                           ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Vulkan.Core10.BaseType (bool32ToBool)
import Vulkan.Core10.BaseType (boolToBool32)
import Vulkan.Core10.BaseType (Bool32)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_INTERLOCK_FEATURES_EXT))
-- | VkPhysicalDeviceFragmentShaderInterlockFeaturesEXT - Structure
-- describing fragment shader interlock features that can be supported by
-- an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceFragmentShaderInterlockFeaturesEXT'
-- structure describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceFragmentShaderInterlockFeaturesEXT' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether the feature is supported.
-- 'PhysicalDeviceFragmentShaderInterlockFeaturesEXT' /can/ also be
-- included in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo'
-- to enable features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.BaseType.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceFragmentShaderInterlockFeaturesEXT = PhysicalDeviceFragmentShaderInterlockFeaturesEXT
  { -- | @fragmentShaderSampleInterlock@ indicates that the implementation
    -- supports the @FragmentShaderSampleInterlockEXT@ SPIR-V capability.
    fragmentShaderSampleInterlock :: Bool
  , -- | @fragmentShaderPixelInterlock@ indicates that the implementation
    -- supports the @FragmentShaderPixelInterlockEXT@ SPIR-V capability.
    fragmentShaderPixelInterlock :: Bool
  , -- | @fragmentShaderShadingRateInterlock@ indicates that the implementation
    -- supports the @FragmentShaderShadingRateInterlockEXT@ SPIR-V capability.
    fragmentShaderShadingRateInterlock :: Bool
  }
  deriving (Typeable, Eq)
deriving instance Show PhysicalDeviceFragmentShaderInterlockFeaturesEXT

instance ToCStruct PhysicalDeviceFragmentShaderInterlockFeaturesEXT where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceFragmentShaderInterlockFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_INTERLOCK_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (fragmentShaderSampleInterlock))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (fragmentShaderPixelInterlock))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (fragmentShaderShadingRateInterlock))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_INTERLOCK_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceFragmentShaderInterlockFeaturesEXT where
  peekCStruct p = do
    fragmentShaderSampleInterlock <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    fragmentShaderPixelInterlock <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    fragmentShaderShadingRateInterlock <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    pure $ PhysicalDeviceFragmentShaderInterlockFeaturesEXT
             (bool32ToBool fragmentShaderSampleInterlock) (bool32ToBool fragmentShaderPixelInterlock) (bool32ToBool fragmentShaderShadingRateInterlock)

instance Storable PhysicalDeviceFragmentShaderInterlockFeaturesEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceFragmentShaderInterlockFeaturesEXT where
  zero = PhysicalDeviceFragmentShaderInterlockFeaturesEXT
           zero
           zero
           zero


type EXT_FRAGMENT_SHADER_INTERLOCK_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_FRAGMENT_SHADER_INTERLOCK_SPEC_VERSION"
pattern EXT_FRAGMENT_SHADER_INTERLOCK_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_FRAGMENT_SHADER_INTERLOCK_SPEC_VERSION = 1


type EXT_FRAGMENT_SHADER_INTERLOCK_EXTENSION_NAME = "VK_EXT_fragment_shader_interlock"

-- No documentation found for TopLevel "VK_EXT_FRAGMENT_SHADER_INTERLOCK_EXTENSION_NAME"
pattern EXT_FRAGMENT_SHADER_INTERLOCK_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_FRAGMENT_SHADER_INTERLOCK_EXTENSION_NAME = "VK_EXT_fragment_shader_interlock"

