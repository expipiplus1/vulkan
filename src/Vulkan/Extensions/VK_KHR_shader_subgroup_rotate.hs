{-# language CPP #-}
-- | = Name
--
-- VK_KHR_shader_subgroup_rotate - device extension
--
-- = VK_KHR_shader_subgroup_rotate
--
-- [__Name String__]
--     @VK_KHR_shader_subgroup_rotate@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     417
--
-- [__Revision__]
--     2
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     None
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_subgroup_rotate.html SPV_KHR_subgroup_rotate>
--
-- [__Contact__]
--
--     -   Kevin Petit
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_shader_subgroup_rotate] @kpet%0A*Here describe the issue or question you have about the VK_KHR_shader_subgroup_rotate extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_shader_subgroup_rotate.adoc VK_KHR_shader_subgroup_rotate>
--
-- [__Last Modified Date__]
--     2024-01-29
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Kévin Petit, Arm Ltd.
--
--     -   Tobias Hector, AMD
--
--     -   John Leech, Khronos
--
--     -   Matthew Netsch, Qualcomm
--
--     -   Jan-Harald Fredriksen, Arm Ltd.
--
--     -   Graeme Leese, Broadcom
--
--     -   Tom Olson, Arm Ltd.
--
--     -   Spencer Fricke, LunarG Inc.
--
-- This extension adds support for the subgroup rotate instruction defined
-- in SPV_KHR_subgroup_rotate.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderSubgroupRotateFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SHADER_SUBGROUP_ROTATE_EXTENSION_NAME'
--
-- -   'KHR_SHADER_SUBGROUP_ROTATE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_ROTATE_FEATURES_KHR'
--
-- -   Extending
--     'Vulkan.Core11.Enums.SubgroupFeatureFlagBits.SubgroupFeatureFlagBits':
--
--     -   'Vulkan.Core11.Enums.SubgroupFeatureFlagBits.SUBGROUP_FEATURE_ROTATE_BIT_KHR'
--
--     -   'Vulkan.Core11.Enums.SubgroupFeatureFlagBits.SUBGROUP_FEATURE_ROTATE_CLUSTERED_BIT_KHR'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-GroupNonUniformRotateKHR GroupNonUniformRotateKHR>
--
-- == Version History
--
-- -   Revision 2, 2024-01-29 (Kévin Petit)
--
--     -   Add
--         'Vulkan.Core11.Enums.SubgroupFeatureFlagBits.SUBGROUP_FEATURE_ROTATE_BIT_KHR'
--         and
--         'Vulkan.Core11.Enums.SubgroupFeatureFlagBits.SUBGROUP_FEATURE_ROTATE_CLUSTERED_BIT_KHR'
--
-- -   Revision 1, 2023-06-20 (Kévin Petit)
--
--     -   Initial revision
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_shader_subgroup_rotate Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_shader_subgroup_rotate  ( PhysicalDeviceShaderSubgroupRotateFeaturesKHR(..)
                                                        , KHR_SHADER_SUBGROUP_ROTATE_SPEC_VERSION
                                                        , pattern KHR_SHADER_SUBGROUP_ROTATE_SPEC_VERSION
                                                        , KHR_SHADER_SUBGROUP_ROTATE_EXTENSION_NAME
                                                        , pattern KHR_SHADER_SUBGROUP_ROTATE_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_ROTATE_FEATURES_KHR))
-- | VkPhysicalDeviceShaderSubgroupRotateFeaturesKHR - Structure describing
-- whether subgroup rotation is enabled
--
-- = Description
--
-- If the 'PhysicalDeviceShaderSubgroupRotateFeaturesKHR' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceShaderSubgroupRotateFeaturesKHR' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_subgroup_rotate VK_KHR_shader_subgroup_rotate>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderSubgroupRotateFeaturesKHR = PhysicalDeviceShaderSubgroupRotateFeaturesKHR
  { -- | #features-shaderSubgroupRotate# @shaderSubgroupRotate@ specifies whether
    -- shader modules /can/ declare the @GroupNonUniformRotateKHR@ capability.
    shaderSubgroupRotate :: Bool
  , -- | #features-shaderSubgroupRotateClustered# @shaderSubgroupRotateClustered@
    -- specifies whether shader modules /can/ use the @ClusterSize@ operand to
    -- @OpGroupNonUniformRotateKHR@.
    shaderSubgroupRotateClustered :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderSubgroupRotateFeaturesKHR)
#endif
deriving instance Show PhysicalDeviceShaderSubgroupRotateFeaturesKHR

instance ToCStruct PhysicalDeviceShaderSubgroupRotateFeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderSubgroupRotateFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_ROTATE_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderSubgroupRotate))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (shaderSubgroupRotateClustered))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_ROTATE_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderSubgroupRotateFeaturesKHR where
  peekCStruct p = do
    shaderSubgroupRotate <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    shaderSubgroupRotateClustered <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderSubgroupRotateFeaturesKHR
             (bool32ToBool shaderSubgroupRotate)
             (bool32ToBool shaderSubgroupRotateClustered)

instance Storable PhysicalDeviceShaderSubgroupRotateFeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderSubgroupRotateFeaturesKHR where
  zero = PhysicalDeviceShaderSubgroupRotateFeaturesKHR
           zero
           zero


type KHR_SHADER_SUBGROUP_ROTATE_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_KHR_SHADER_SUBGROUP_ROTATE_SPEC_VERSION"
pattern KHR_SHADER_SUBGROUP_ROTATE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SHADER_SUBGROUP_ROTATE_SPEC_VERSION = 2


type KHR_SHADER_SUBGROUP_ROTATE_EXTENSION_NAME = "VK_KHR_shader_subgroup_rotate"

-- No documentation found for TopLevel "VK_KHR_SHADER_SUBGROUP_ROTATE_EXTENSION_NAME"
pattern KHR_SHADER_SUBGROUP_ROTATE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SHADER_SUBGROUP_ROTATE_EXTENSION_NAME = "VK_KHR_shader_subgroup_rotate"

