{-# language CPP #-}
-- | = Name
--
-- VK_KHR_shader_subgroup_uniform_control_flow - device extension
--
-- == VK_KHR_shader_subgroup_uniform_control_flow
--
-- [__Name String__]
--     @VK_KHR_shader_subgroup_uniform_control_flow@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     324
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--
-- [__Contact__]
--
--     -   Alan Baker
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_shader_subgroup_uniform_control_flow] @alan-baker%0A*Here describe the issue or question you have about the VK_KHR_shader_subgroup_uniform_control_flow extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-08-27
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   Requires SPIR-V 1.3.
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_subgroup_uniform_control_flow.html SPV_KHR_subgroup_uniform_control_flow>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/ext/GL_EXT_subgroupuniform_qualifier.txt GL_EXT_subgroupuniform_qualifier>
--
-- [__Contributors__]
--
--     -   Alan Baker, Google
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension allows the use of the
-- @SPV_KHR_subgroup_uniform_control_flow@ SPIR-V extension in shader
-- modules. @SPV_KHR_subgroup_uniform_control_flow@ provides stronger
-- guarantees that diverged subgroups will reconverge.
--
-- Developers should utilize this extension if they use subgroup operations
-- to reduce the work performed by a uniform subgroup. This extension will
-- guarantee that uniform subgroup will reconverge in the same manner as
-- invocation groups (see “Uniform Control Flow” in the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirv-spec Khronos SPIR-V Specification>).
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderSubgroupUniformControlFlowFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SHADER_SUBGROUP_UNIFORM_CONTROL_FLOW_EXTENSION_NAME'
--
-- -   'KHR_SHADER_SUBGROUP_UNIFORM_CONTROL_FLOW_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_UNIFORM_CONTROL_FLOW_FEATURES_KHR'
--
-- == Version History
--
-- -   Revision 1, 2020-08-27 (Alan Baker)
--
--     -   Internal draft version
--
-- == See Also
--
-- 'PhysicalDeviceShaderSubgroupUniformControlFlowFeaturesKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_shader_subgroup_uniform_control_flow Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_shader_subgroup_uniform_control_flow  ( PhysicalDeviceShaderSubgroupUniformControlFlowFeaturesKHR(..)
                                                                      , KHR_SHADER_SUBGROUP_UNIFORM_CONTROL_FLOW_SPEC_VERSION
                                                                      , pattern KHR_SHADER_SUBGROUP_UNIFORM_CONTROL_FLOW_SPEC_VERSION
                                                                      , KHR_SHADER_SUBGROUP_UNIFORM_CONTROL_FLOW_EXTENSION_NAME
                                                                      , pattern KHR_SHADER_SUBGROUP_UNIFORM_CONTROL_FLOW_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_UNIFORM_CONTROL_FLOW_FEATURES_KHR))
-- | VkPhysicalDeviceShaderSubgroupUniformControlFlowFeaturesKHR - Structure
-- describing support for shader subgroup uniform control flow by an
-- implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceShaderSubgroupUniformControlFlowFeaturesKHR'
-- structure is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceShaderSubgroupUniformControlFlowFeaturesKHR'
-- /can/ also be used in the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively enable these
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_subgroup_uniform_control_flow VK_KHR_shader_subgroup_uniform_control_flow>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderSubgroupUniformControlFlowFeaturesKHR = PhysicalDeviceShaderSubgroupUniformControlFlowFeaturesKHR
  { -- | #features-shaderSubgroupUniformControlFlow#
    -- @shaderSubgroupUniformControlFlow@ specifies whether the implementation
    -- supports the shader execution mode @SubgroupUniformControlFlowKHR@
    shaderSubgroupUniformControlFlow :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderSubgroupUniformControlFlowFeaturesKHR)
#endif
deriving instance Show PhysicalDeviceShaderSubgroupUniformControlFlowFeaturesKHR

instance ToCStruct PhysicalDeviceShaderSubgroupUniformControlFlowFeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderSubgroupUniformControlFlowFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_UNIFORM_CONTROL_FLOW_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderSubgroupUniformControlFlow))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_UNIFORM_CONTROL_FLOW_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderSubgroupUniformControlFlowFeaturesKHR where
  peekCStruct p = do
    shaderSubgroupUniformControlFlow <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderSubgroupUniformControlFlowFeaturesKHR
             (bool32ToBool shaderSubgroupUniformControlFlow)

instance Storable PhysicalDeviceShaderSubgroupUniformControlFlowFeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderSubgroupUniformControlFlowFeaturesKHR where
  zero = PhysicalDeviceShaderSubgroupUniformControlFlowFeaturesKHR
           zero


type KHR_SHADER_SUBGROUP_UNIFORM_CONTROL_FLOW_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_SHADER_SUBGROUP_UNIFORM_CONTROL_FLOW_SPEC_VERSION"
pattern KHR_SHADER_SUBGROUP_UNIFORM_CONTROL_FLOW_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SHADER_SUBGROUP_UNIFORM_CONTROL_FLOW_SPEC_VERSION = 1


type KHR_SHADER_SUBGROUP_UNIFORM_CONTROL_FLOW_EXTENSION_NAME = "VK_KHR_shader_subgroup_uniform_control_flow"

-- No documentation found for TopLevel "VK_KHR_SHADER_SUBGROUP_UNIFORM_CONTROL_FLOW_EXTENSION_NAME"
pattern KHR_SHADER_SUBGROUP_UNIFORM_CONTROL_FLOW_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SHADER_SUBGROUP_UNIFORM_CONTROL_FLOW_EXTENSION_NAME = "VK_KHR_shader_subgroup_uniform_control_flow"

