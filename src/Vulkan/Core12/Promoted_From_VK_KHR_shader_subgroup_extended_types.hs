{-# language CPP #-}
module Vulkan.Core12.Promoted_From_VK_KHR_shader_subgroup_extended_types  ( PhysicalDeviceShaderSubgroupExtendedTypesFeatures(..)
                                                                          , StructureType(..)
                                                                          ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_EXTENDED_TYPES_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
-- | VkPhysicalDeviceShaderSubgroupExtendedTypesFeatures - Structure
-- describing the extended types subgroups support feature for an
-- implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceShaderSubgroupExtendedTypesFeatures'
-- structure describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceShaderSubgroupExtendedTypesFeatures' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether each feature is supported.
-- 'PhysicalDeviceShaderSubgroupExtendedTypesFeatures' /can/ also be
-- included in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo'
-- to enable features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.BaseType.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderSubgroupExtendedTypesFeatures = PhysicalDeviceShaderSubgroupExtendedTypesFeatures
  { -- | @shaderSubgroupExtendedTypes@ is a boolean that specifies whether
    -- subgroup operations can use 8-bit integer, 16-bit integer, 64-bit
    -- integer, 16-bit floating-point, and vectors of these types in
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-group-operations group operations>
    -- with
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-scope-subgroup subgroup scope>if
    -- the implementation supports the types.
    shaderSubgroupExtendedTypes :: Bool }
  deriving (Typeable)
deriving instance Show PhysicalDeviceShaderSubgroupExtendedTypesFeatures

instance ToCStruct PhysicalDeviceShaderSubgroupExtendedTypesFeatures where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderSubgroupExtendedTypesFeatures{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_EXTENDED_TYPES_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderSubgroupExtendedTypes))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_EXTENDED_TYPES_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderSubgroupExtendedTypesFeatures where
  peekCStruct p = do
    shaderSubgroupExtendedTypes <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderSubgroupExtendedTypesFeatures
             (bool32ToBool shaderSubgroupExtendedTypes)

instance Storable PhysicalDeviceShaderSubgroupExtendedTypesFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderSubgroupExtendedTypesFeatures where
  zero = PhysicalDeviceShaderSubgroupExtendedTypesFeatures
           zero

