{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_shader_subgroup_rotateRoadmap"
module Vulkan.Core14.Promoted_From_VK_KHR_shader_subgroup_rotateRoadmap  ( PhysicalDeviceShaderSubgroupRotateFeatures(..)
                                                                         , StructureType(..)
                                                                         , SubgroupFeatureFlagBits(..)
                                                                         , SubgroupFeatureFlags
                                                                         ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_ROTATE_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
import Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlagBits(..))
import Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlags)
-- | VkPhysicalDeviceShaderSubgroupRotateFeatures - Structure describing
-- whether subgroup rotation is enabled
--
-- = Description
--
-- If the 'PhysicalDeviceShaderSubgroupRotateFeatures' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceShaderSubgroupRotateFeatures', it /must/ add an instance
-- of the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_subgroup_rotate VK_KHR_shader_subgroup_rotate>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderSubgroupRotateFeatures = PhysicalDeviceShaderSubgroupRotateFeatures
  { -- | #extension-features-shaderSubgroupRotate# @shaderSubgroupRotate@
    -- specifies whether shader modules /can/ declare the
    -- @GroupNonUniformRotateKHR@ capability.
    shaderSubgroupRotate :: Bool
  , -- | #extension-features-shaderSubgroupRotateClustered#
    -- @shaderSubgroupRotateClustered@ specifies whether shader modules /can/
    -- use the @ClusterSize@ operand to @OpGroupNonUniformRotateKHR@.
    shaderSubgroupRotateClustered :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderSubgroupRotateFeatures)
#endif
deriving instance Show PhysicalDeviceShaderSubgroupRotateFeatures

instance ToCStruct PhysicalDeviceShaderSubgroupRotateFeatures where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderSubgroupRotateFeatures{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_ROTATE_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderSubgroupRotate))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (shaderSubgroupRotateClustered))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_ROTATE_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderSubgroupRotateFeatures where
  peekCStruct p = do
    shaderSubgroupRotate <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    shaderSubgroupRotateClustered <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderSubgroupRotateFeatures
             (bool32ToBool shaderSubgroupRotate)
             (bool32ToBool shaderSubgroupRotateClustered)

instance Storable PhysicalDeviceShaderSubgroupRotateFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderSubgroupRotateFeatures where
  zero = PhysicalDeviceShaderSubgroupRotateFeatures
           zero
           zero

