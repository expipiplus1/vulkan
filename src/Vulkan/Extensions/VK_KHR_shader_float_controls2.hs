{-# language CPP #-}
-- | = Name
--
-- VK_KHR_shader_float_controls2 - device extension
--
-- = VK_KHR_shader_float_controls2
--
-- [__Name String__]
--     @VK_KHR_shader_float_controls2@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     529
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
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_float_controls VK_KHR_shader_float_controls>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_float_controls2.html SPV_KHR_float_controls2>
--
-- [__Contact__]
--
--     -   Graeme Leese
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_shader_float_controls2] @gnl21%0A*Here describe the issue or question you have about the VK_KHR_shader_float_controls2 extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_shader_float_controls2.adoc VK_KHR_shader_float_controls2>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-05-16
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_float_controls2.html SPV_KHR_float_controls2>.
--
-- [__Contributors__]
--
--     -   Graeme Leese, Broadcom
--
-- == Description
--
-- This extension enables use of the more expressive fast floating-point
-- math flags in the SPV_KHR_float_controls2 extension. These flags give
-- finer- grained control over which optimizations compilers may apply,
-- potentially speeding up execution while retaining correct results.
--
-- The extension also adds control over the fast-math modes to the GLSL
-- extended instruction set, making these operations more consistent with
-- SPIR-V and allowing their use in situations where floating-point
-- conformance is important.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderFloatControls2FeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SHADER_FLOAT_CONTROLS_2_EXTENSION_NAME'
--
-- -   'KHR_SHADER_FLOAT_CONTROLS_2_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT_CONTROLS_2_FEATURES_KHR'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-FloatControls2 FloatControls2>
--
-- == Version History
--
-- -   Revision 1, 2023-05-16 (Graeme Leese)
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
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_shader_float_controls2 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_shader_float_controls2  ( PhysicalDeviceShaderFloatControls2FeaturesKHR(..)
                                                        , KHR_SHADER_FLOAT_CONTROLS_2_SPEC_VERSION
                                                        , pattern KHR_SHADER_FLOAT_CONTROLS_2_SPEC_VERSION
                                                        , KHR_SHADER_FLOAT_CONTROLS_2_EXTENSION_NAME
                                                        , pattern KHR_SHADER_FLOAT_CONTROLS_2_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT_CONTROLS_2_FEATURES_KHR))
-- | VkPhysicalDeviceShaderFloatControls2FeaturesKHR - Structure describing
-- shader float controls 2 features that can be supported by an
-- implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceShaderFloatControls2FeaturesKHR' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceShaderFloatControls2FeaturesKHR' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_float_controls2 VK_KHR_shader_float_controls2>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderFloatControls2FeaturesKHR = PhysicalDeviceShaderFloatControls2FeaturesKHR
  { -- | #features-shaderFloatControls2# @shaderFloatControls2@ specifies whether
    -- shader modules /can/ declare the @FloatControls2@ capability.
    shaderFloatControls2 :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderFloatControls2FeaturesKHR)
#endif
deriving instance Show PhysicalDeviceShaderFloatControls2FeaturesKHR

instance ToCStruct PhysicalDeviceShaderFloatControls2FeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderFloatControls2FeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT_CONTROLS_2_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderFloatControls2))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT_CONTROLS_2_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderFloatControls2FeaturesKHR where
  peekCStruct p = do
    shaderFloatControls2 <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderFloatControls2FeaturesKHR
             (bool32ToBool shaderFloatControls2)

instance Storable PhysicalDeviceShaderFloatControls2FeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderFloatControls2FeaturesKHR where
  zero = PhysicalDeviceShaderFloatControls2FeaturesKHR
           zero


type KHR_SHADER_FLOAT_CONTROLS_2_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_SHADER_FLOAT_CONTROLS_2_SPEC_VERSION"
pattern KHR_SHADER_FLOAT_CONTROLS_2_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SHADER_FLOAT_CONTROLS_2_SPEC_VERSION = 1


type KHR_SHADER_FLOAT_CONTROLS_2_EXTENSION_NAME = "VK_KHR_shader_float_controls2"

-- No documentation found for TopLevel "VK_KHR_SHADER_FLOAT_CONTROLS_2_EXTENSION_NAME"
pattern KHR_SHADER_FLOAT_CONTROLS_2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SHADER_FLOAT_CONTROLS_2_EXTENSION_NAME = "VK_KHR_shader_float_controls2"

