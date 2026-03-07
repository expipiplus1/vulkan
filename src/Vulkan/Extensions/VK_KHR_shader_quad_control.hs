{-# language CPP #-}
-- | = Name
--
-- VK_KHR_shader_quad_control - device extension
--
-- = VK_KHR_shader_quad_control
--
-- [__Name String__]
--     @VK_KHR_shader_quad_control@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     236
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_vulkan_memory_model VK_KHR_vulkan_memory_model>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_maximal_reconvergence VK_KHR_shader_maximal_reconvergence>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_quad_control.html SPV_KHR_quad_control>
--
-- [__Contact__]
--
--     -   Tobias Hector
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_shader_quad_control] @tobski%0A*Here describe the issue or question you have about the VK_KHR_shader_quad_control extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_shader_quad_control.adoc VK_KHR_shader_quad_control>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-11-01
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Tobias Hector, AMD
--
--     -   Bill Licea-Kane, Qualcomm
--
--     -   Graeme Leese, Broadcom
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Nicolai Hähnle, AMD
--
--     -   Jeff Bolz, NVidia
--
--     -   Alan Baker, Google
--
--     -   Hans-Kristian Arntzen, Valve
--
-- == Description
--
-- This extension adds new quad any\/all operations, requires that
-- derivatives are well-defined in quad-uniform control flow, and adds the
-- ability to require helper invocations participate in group operations.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderQuadControlFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SHADER_QUAD_CONTROL_EXTENSION_NAME'
--
-- -   'KHR_SHADER_QUAD_CONTROL_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_QUAD_CONTROL_FEATURES_KHR'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-QuadControlKHR QuadControlKHR>
--
-- == Version History
--
-- -   Revision 1, 2023-11-01 (Tobias Hector)
--
--     -   Initial draft
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_shader_quad_control Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_shader_quad_control  ( PhysicalDeviceShaderQuadControlFeaturesKHR(..)
                                                     , KHR_SHADER_QUAD_CONTROL_SPEC_VERSION
                                                     , pattern KHR_SHADER_QUAD_CONTROL_SPEC_VERSION
                                                     , KHR_SHADER_QUAD_CONTROL_EXTENSION_NAME
                                                     , pattern KHR_SHADER_QUAD_CONTROL_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_QUAD_CONTROL_FEATURES_KHR))
-- | VkPhysicalDeviceShaderQuadControlFeaturesKHR - Structure describing
-- whether quad scopes are supported by the implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceShaderQuadControlFeaturesKHR' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceShaderQuadControlFeaturesKHR' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_quad_control VK_KHR_shader_quad_control>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderQuadControlFeaturesKHR = PhysicalDeviceShaderQuadControlFeaturesKHR
  { -- | #features-shaderQuadControl# @shaderQuadControl@ indicates whether the
    -- implementation supports shaders with the @QuadControlKHR@ capability.
    shaderQuadControl :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderQuadControlFeaturesKHR)
#endif
deriving instance Show PhysicalDeviceShaderQuadControlFeaturesKHR

instance ToCStruct PhysicalDeviceShaderQuadControlFeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderQuadControlFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_QUAD_CONTROL_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderQuadControl))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_QUAD_CONTROL_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderQuadControlFeaturesKHR where
  peekCStruct p = do
    shaderQuadControl <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderQuadControlFeaturesKHR
             (bool32ToBool shaderQuadControl)

instance Storable PhysicalDeviceShaderQuadControlFeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderQuadControlFeaturesKHR where
  zero = PhysicalDeviceShaderQuadControlFeaturesKHR
           zero


type KHR_SHADER_QUAD_CONTROL_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_SHADER_QUAD_CONTROL_SPEC_VERSION"
pattern KHR_SHADER_QUAD_CONTROL_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SHADER_QUAD_CONTROL_SPEC_VERSION = 1


type KHR_SHADER_QUAD_CONTROL_EXTENSION_NAME = "VK_KHR_shader_quad_control"

-- No documentation found for TopLevel "VK_KHR_SHADER_QUAD_CONTROL_EXTENSION_NAME"
pattern KHR_SHADER_QUAD_CONTROL_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SHADER_QUAD_CONTROL_EXTENSION_NAME = "VK_KHR_shader_quad_control"

