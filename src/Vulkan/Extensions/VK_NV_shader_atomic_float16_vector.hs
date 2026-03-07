{-# language CPP #-}
-- | = Name
--
-- VK_NV_shader_atomic_float16_vector - device extension
--
-- = VK_NV_shader_atomic_float16_vector
--
-- [__Name String__]
--     @VK_NV_shader_atomic_float16_vector@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     564
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_shader_atomic_fp16_vector.html SPV_NV_shader_atomic_fp16_vector>
--
-- [__Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_shader_atomic_float16_vector] @jeffbolznv%0A*Here describe the issue or question you have about the VK_NV_shader_atomic_float16_vector extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-02-03
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension provides API support for
--         <https://registry.khronos.org/OpenGL/extensions/NV/NV_shader_atomic_fp16_vector.txt GL_NV_shader_atomic_fp16_vector>
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension allows a shader to perform atomic add, min, max, and
-- exchange operations on 2- and 4-component vectors of float16. Buffer,
-- workgroup, and image storage classes are all supported.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderAtomicFloat16VectorFeaturesNV'
--
-- == New Enum Constants
--
-- -   'NV_SHADER_ATOMIC_FLOAT16_VECTOR_EXTENSION_NAME'
--
-- -   'NV_SHADER_ATOMIC_FLOAT16_VECTOR_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_FLOAT16_VECTOR_FEATURES_NV'
--
-- == Issues
--
-- None.
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-AtomicFloat16VectorNV AtomicFloat16VectorNV>
--
-- == Version History
--
-- -   Revision 1, 2024-02-03 (Jeff Bolz)
--
--     -   Internal revisions
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_NV_shader_atomic_float16_vector Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_shader_atomic_float16_vector  ( PhysicalDeviceShaderAtomicFloat16VectorFeaturesNV(..)
                                                             , NV_SHADER_ATOMIC_FLOAT16_VECTOR_SPEC_VERSION
                                                             , pattern NV_SHADER_ATOMIC_FLOAT16_VECTOR_SPEC_VERSION
                                                             , NV_SHADER_ATOMIC_FLOAT16_VECTOR_EXTENSION_NAME
                                                             , pattern NV_SHADER_ATOMIC_FLOAT16_VECTOR_EXTENSION_NAME
                                                             ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_FLOAT16_VECTOR_FEATURES_NV))
-- | VkPhysicalDeviceShaderAtomicFloat16VectorFeaturesNV - Structure
-- describing features supported by VK_NV_shader_atomic_float16_vector
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceShaderAtomicFloat16VectorFeaturesNV' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceShaderAtomicFloat16VectorFeaturesNV', it /must/ add an
-- instance of the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_shader_atomic_float16_vector VK_NV_shader_atomic_float16_vector>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderAtomicFloat16VectorFeaturesNV = PhysicalDeviceShaderAtomicFloat16VectorFeaturesNV
  { -- | #features-shaderFloat16VectorAtomics# @shaderFloat16VectorAtomics@
    -- indicates whether shaders /can/ perform 16-bit floating-point, 2- and
    -- 4-component vector atomic operations.
    shaderFloat16VectorAtomics :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderAtomicFloat16VectorFeaturesNV)
#endif
deriving instance Show PhysicalDeviceShaderAtomicFloat16VectorFeaturesNV

instance ToCStruct PhysicalDeviceShaderAtomicFloat16VectorFeaturesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderAtomicFloat16VectorFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_FLOAT16_VECTOR_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderFloat16VectorAtomics))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_FLOAT16_VECTOR_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderAtomicFloat16VectorFeaturesNV where
  peekCStruct p = do
    shaderFloat16VectorAtomics <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderAtomicFloat16VectorFeaturesNV
             (bool32ToBool shaderFloat16VectorAtomics)

instance Storable PhysicalDeviceShaderAtomicFloat16VectorFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderAtomicFloat16VectorFeaturesNV where
  zero = PhysicalDeviceShaderAtomicFloat16VectorFeaturesNV
           zero


type NV_SHADER_ATOMIC_FLOAT16_VECTOR_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_SHADER_ATOMIC_FLOAT16_VECTOR_SPEC_VERSION"
pattern NV_SHADER_ATOMIC_FLOAT16_VECTOR_SPEC_VERSION :: forall a . Integral a => a
pattern NV_SHADER_ATOMIC_FLOAT16_VECTOR_SPEC_VERSION = 1


type NV_SHADER_ATOMIC_FLOAT16_VECTOR_EXTENSION_NAME = "VK_NV_shader_atomic_float16_vector"

-- No documentation found for TopLevel "VK_NV_SHADER_ATOMIC_FLOAT16_VECTOR_EXTENSION_NAME"
pattern NV_SHADER_ATOMIC_FLOAT16_VECTOR_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_SHADER_ATOMIC_FLOAT16_VECTOR_EXTENSION_NAME = "VK_NV_shader_atomic_float16_vector"

