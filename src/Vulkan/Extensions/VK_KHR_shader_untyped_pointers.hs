{-# language CPP #-}
-- | = Name
--
-- VK_KHR_shader_untyped_pointers - device extension
--
-- = VK_KHR_shader_untyped_pointers
--
-- [__Name String__]
--     @VK_KHR_shader_untyped_pointers@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     388
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_untyped_pointers.html SPV_KHR_untyped_pointers>
--
-- [__Contact__]
--
--     -   Alan Baker
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_shader_untyped_pointers] @alan-baker%0A*Here describe the issue or question you have about the VK_KHR_shader_untyped_pointers extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_shader_untyped_pointers.adoc VK_KHR_shader_untyped_pointers>
--
-- [__Last Modified Date__]
--     2024-03-26
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   Requires the
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_untyped_pointers.html SPV_KHR_untyped_pointers>
--         SPIR-V extension.
--
-- [__Contributors__]
--
--     -   Alan Baker, Google
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Tom Olson, Arm
--
--     -   Spencer Fricke, LunarG
--
--     -   Shahbaz Youssefi, Google
--
--     -   Tobias Hector, AMD
--
-- This extension adds Vulkan support for the
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_untyped_pointers.html SPV_KHR_untyped_pointers>
-- SPIR-V extension. It provides an alternative to strongly-typed pointers.
-- Untyped pointers allow shader authors to reinterpret data accessed
-- through memory and atomic instructions versus the data type declared in
-- the variable without extra conversion instructions. Untyped pointers
-- also provide an efficient translation from templated load\/store
-- operations in high-level languages and simplify shaders that support
-- operations, but not storage, on smaller data types (e.g. 16-bit
-- floating-point types).
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderUntypedPointersFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SHADER_UNTYPED_POINTERS_EXTENSION_NAME'
--
-- -   'KHR_SHADER_UNTYPED_POINTERS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_UNTYPED_POINTERS_FEATURES_KHR'
--
-- == Version History
--
-- -   Revision 1, 2024-03-26 (Alan Baker)
--
--     -   Internal draft version
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_shader_untyped_pointers Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_shader_untyped_pointers  ( PhysicalDeviceShaderUntypedPointersFeaturesKHR(..)
                                                         , KHR_SHADER_UNTYPED_POINTERS_SPEC_VERSION
                                                         , pattern KHR_SHADER_UNTYPED_POINTERS_SPEC_VERSION
                                                         , KHR_SHADER_UNTYPED_POINTERS_EXTENSION_NAME
                                                         , pattern KHR_SHADER_UNTYPED_POINTERS_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_UNTYPED_POINTERS_FEATURES_KHR))
-- | VkPhysicalDeviceShaderUntypedPointersFeaturesKHR - Structure describing
-- support for untyped pointers in shader by an implementation
--
-- = Members
--
-- The members of 'PhysicalDeviceShaderUntypedPointersFeaturesKHR' describe
-- the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceShaderUntypedPointersFeaturesKHR' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether the features are supported.
-- 'PhysicalDeviceShaderUntypedPointersFeaturesKHR' /can/ also be included
-- in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- enable the features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_untyped_pointers VK_KHR_shader_untyped_pointers>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderUntypedPointersFeaturesKHR = PhysicalDeviceShaderUntypedPointersFeaturesKHR
  { -- | #features-shaderUntypedPointers# @shaderUntypedPointers@ specifies
    -- whether shader modules /can/ declare the @UntypedPointersKHR@ capability
    -- and untyped pointers in any
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#interfaces-resources-layout explicitly laid out storage class>.
    shaderUntypedPointers :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderUntypedPointersFeaturesKHR)
#endif
deriving instance Show PhysicalDeviceShaderUntypedPointersFeaturesKHR

instance ToCStruct PhysicalDeviceShaderUntypedPointersFeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderUntypedPointersFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_UNTYPED_POINTERS_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderUntypedPointers))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_UNTYPED_POINTERS_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderUntypedPointersFeaturesKHR where
  peekCStruct p = do
    shaderUntypedPointers <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderUntypedPointersFeaturesKHR
             (bool32ToBool shaderUntypedPointers)

instance Storable PhysicalDeviceShaderUntypedPointersFeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderUntypedPointersFeaturesKHR where
  zero = PhysicalDeviceShaderUntypedPointersFeaturesKHR
           zero


type KHR_SHADER_UNTYPED_POINTERS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_SHADER_UNTYPED_POINTERS_SPEC_VERSION"
pattern KHR_SHADER_UNTYPED_POINTERS_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SHADER_UNTYPED_POINTERS_SPEC_VERSION = 1


type KHR_SHADER_UNTYPED_POINTERS_EXTENSION_NAME = "VK_KHR_shader_untyped_pointers"

-- No documentation found for TopLevel "VK_KHR_SHADER_UNTYPED_POINTERS_EXTENSION_NAME"
pattern KHR_SHADER_UNTYPED_POINTERS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SHADER_UNTYPED_POINTERS_EXTENSION_NAME = "VK_KHR_shader_untyped_pointers"

