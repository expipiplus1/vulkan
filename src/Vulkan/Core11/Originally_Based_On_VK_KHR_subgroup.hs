{-# language CPP #-}
module Vulkan.Core11.Originally_Based_On_VK_KHR_subgroup  ( PhysicalDeviceSubgroupProperties(..)
                                                          , StructureType(..)
                                                          , SubgroupFeatureFlagBits(..)
                                                          , SubgroupFeatureFlags
                                                          ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Vulkan.Core10.BaseType (bool32ToBool)
import Vulkan.Core10.BaseType (boolToBool32)
import Vulkan.Core10.BaseType (Bool32)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlags)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
import Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlagBits(..))
import Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlags)
-- | VkPhysicalDeviceSubgroupProperties - Structure describing subgroup
-- support for an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceSubgroupProperties' structure describe
-- the following implementation-dependent limits:
--
-- = Description
--
-- If the 'PhysicalDeviceSubgroupProperties' structure is included in the
-- @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- If @supportedOperations@ includes
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-subgroup-quad >,
-- @subgroupSize@ /must/ be greater than or equal to 4.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.BaseType.Bool32',
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Core11.Enums.SubgroupFeatureFlagBits.SubgroupFeatureFlags'
data PhysicalDeviceSubgroupProperties = PhysicalDeviceSubgroupProperties
  { -- | @subgroupSize@ is the default number of invocations in each subgroup.
    -- @subgroupSize@ is at least 1 if any of the physical device’s queues
    -- support 'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' or
    -- 'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT'. @subgroupSize@ is
    -- a power-of-two.
    subgroupSize :: Word32
  , -- | @supportedStages@ is a bitfield of
    -- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' describing
    -- the shader stages that
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-group-operations group operations>
    -- with
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-scope-subgroup subgroup scope>
    -- are supported in. @supportedStages@ will have the
    -- 'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_COMPUTE_BIT' bit
    -- set if any of the physical device’s queues support
    -- 'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT'.
    supportedStages :: ShaderStageFlags
  , -- | @supportedOperations@ is a bitmask of
    -- 'Vulkan.Core11.Enums.SubgroupFeatureFlagBits.SubgroupFeatureFlagBits'
    -- specifying the sets of
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-group-operations group operations>
    -- with
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-scope-subgroup subgroup scope>
    -- supported on this device. @supportedOperations@ will have the
    -- 'Vulkan.Core11.Enums.SubgroupFeatureFlagBits.SUBGROUP_FEATURE_BASIC_BIT'
    -- bit set if any of the physical device’s queues support
    -- 'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' or
    -- 'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT'.
    supportedOperations :: SubgroupFeatureFlags
  , -- | @quadOperationsInAllStages@ is a boolean specifying whether
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-quad-operations quad group operations>
    -- are available in all stages, or are restricted to fragment and compute
    -- stages.
    quadOperationsInAllStages :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceSubgroupProperties)
#endif
deriving instance Show PhysicalDeviceSubgroupProperties

instance ToCStruct PhysicalDeviceSubgroupProperties where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceSubgroupProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (subgroupSize)
    poke ((p `plusPtr` 20 :: Ptr ShaderStageFlags)) (supportedStages)
    poke ((p `plusPtr` 24 :: Ptr SubgroupFeatureFlags)) (supportedOperations)
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (quadOperationsInAllStages))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr ShaderStageFlags)) (zero)
    poke ((p `plusPtr` 24 :: Ptr SubgroupFeatureFlags)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceSubgroupProperties where
  peekCStruct p = do
    subgroupSize <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    supportedStages <- peek @ShaderStageFlags ((p `plusPtr` 20 :: Ptr ShaderStageFlags))
    supportedOperations <- peek @SubgroupFeatureFlags ((p `plusPtr` 24 :: Ptr SubgroupFeatureFlags))
    quadOperationsInAllStages <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    pure $ PhysicalDeviceSubgroupProperties
             subgroupSize supportedStages supportedOperations (bool32ToBool quadOperationsInAllStages)

instance Storable PhysicalDeviceSubgroupProperties where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceSubgroupProperties where
  zero = PhysicalDeviceSubgroupProperties
           zero
           zero
           zero
           zero

