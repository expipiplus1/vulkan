{-# language CPP #-}
-- No documentation found for Chapter "VK_NV_shader_sm_builtins"
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
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SM_BUILTINS_FEATURES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SM_BUILTINS_PROPERTIES_NV))

-- No documentation found for TopLevel "VkPhysicalDeviceShaderSMBuiltinsPropertiesNV"
data PhysicalDeviceShaderSMBuiltinsPropertiesNV = PhysicalDeviceShaderSMBuiltinsPropertiesNV
  { -- No documentation found for Nested "VkPhysicalDeviceShaderSMBuiltinsPropertiesNV" "shaderSMCount"
    shaderSMCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderSMBuiltinsPropertiesNV" "shaderWarpsPerSM"
    shaderWarpsPerSM :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderSMBuiltinsPropertiesNV)
#endif
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



-- No documentation found for TopLevel "VkPhysicalDeviceShaderSMBuiltinsFeaturesNV"
data PhysicalDeviceShaderSMBuiltinsFeaturesNV = PhysicalDeviceShaderSMBuiltinsFeaturesNV
  { -- No documentation found for Nested "VkPhysicalDeviceShaderSMBuiltinsFeaturesNV" "shaderSMBuiltins"
    shaderSMBuiltins :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderSMBuiltinsFeaturesNV)
#endif
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

