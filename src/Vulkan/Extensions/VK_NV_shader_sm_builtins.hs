{-# language CPP #-}
module Vulkan.Extensions.VK_NV_shader_sm_builtins  ( PhysicalDeviceShaderSMBuiltinsPropertiesNV(..)
                                                   , PhysicalDeviceShaderSMBuiltinsFeaturesNV(..)
                                                   , NV_SHADER_SM_BUILTINS_SPEC_VERSION
                                                   , pattern NV_SHADER_SM_BUILTINS_SPEC_VERSION
                                                   , NV_SHADER_SM_BUILTINS_EXTENSION_NAME
                                                   , pattern NV_SHADER_SM_BUILTINS_EXTENSION_NAME
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
import Data.Word (Word32)
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SM_BUILTINS_FEATURES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SM_BUILTINS_PROPERTIES_NV))
-- | VkPhysicalDeviceShaderSMBuiltinsPropertiesNV - Structure describing
-- shader SM Builtins properties supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceShaderSMBuiltinsPropertiesNV'
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- If the 'PhysicalDeviceShaderSMBuiltinsPropertiesNV' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderSMBuiltinsPropertiesNV = PhysicalDeviceShaderSMBuiltinsPropertiesNV
  { -- | @shaderSMCount@ is the number of SMs on the device.
    shaderSMCount :: Word32
  , -- | @shaderWarpsPerSM@ is the maximum number of simultaneously executing
    -- warps on an SM.
    shaderWarpsPerSM :: Word32
  }
  deriving (Typeable)
deriving instance Show PhysicalDeviceShaderSMBuiltinsPropertiesNV

instance ToCStruct PhysicalDeviceShaderSMBuiltinsPropertiesNV where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderSMBuiltinsPropertiesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SM_BUILTINS_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (shaderSMCount)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (shaderWarpsPerSM)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SM_BUILTINS_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceShaderSMBuiltinsPropertiesNV where
  peekCStruct p = do
    shaderSMCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    shaderWarpsPerSM <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pure $ PhysicalDeviceShaderSMBuiltinsPropertiesNV
             shaderSMCount shaderWarpsPerSM

instance Storable PhysicalDeviceShaderSMBuiltinsPropertiesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderSMBuiltinsPropertiesNV where
  zero = PhysicalDeviceShaderSMBuiltinsPropertiesNV
           zero
           zero


-- | VkPhysicalDeviceShaderSMBuiltinsFeaturesNV - Structure describing the
-- shader SM Builtins features that can be supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceShaderSMBuiltinsFeaturesNV' structure
-- describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceShaderSMBuiltinsFeaturesNV' structure is included
-- in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether the feature is supported.
-- 'PhysicalDeviceShaderSMBuiltinsFeaturesNV' /can/ also be included in the
-- @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to enable the
-- feature.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.BaseType.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderSMBuiltinsFeaturesNV = PhysicalDeviceShaderSMBuiltinsFeaturesNV
  { -- | @shaderSMBuiltins@ indicates whether the implementation supports the
    -- SPIR-V @ShaderSMBuiltinsNV@ capability.
    shaderSMBuiltins :: Bool }
  deriving (Typeable)
deriving instance Show PhysicalDeviceShaderSMBuiltinsFeaturesNV

instance ToCStruct PhysicalDeviceShaderSMBuiltinsFeaturesNV where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderSMBuiltinsFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SM_BUILTINS_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderSMBuiltins))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SM_BUILTINS_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderSMBuiltinsFeaturesNV where
  peekCStruct p = do
    shaderSMBuiltins <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderSMBuiltinsFeaturesNV
             (bool32ToBool shaderSMBuiltins)

instance Storable PhysicalDeviceShaderSMBuiltinsFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderSMBuiltinsFeaturesNV where
  zero = PhysicalDeviceShaderSMBuiltinsFeaturesNV
           zero


type NV_SHADER_SM_BUILTINS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_SHADER_SM_BUILTINS_SPEC_VERSION"
pattern NV_SHADER_SM_BUILTINS_SPEC_VERSION :: forall a . Integral a => a
pattern NV_SHADER_SM_BUILTINS_SPEC_VERSION = 1


type NV_SHADER_SM_BUILTINS_EXTENSION_NAME = "VK_NV_shader_sm_builtins"

-- No documentation found for TopLevel "VK_NV_SHADER_SM_BUILTINS_EXTENSION_NAME"
pattern NV_SHADER_SM_BUILTINS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_SHADER_SM_BUILTINS_EXTENSION_NAME = "VK_NV_shader_sm_builtins"

