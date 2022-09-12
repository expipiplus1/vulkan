{-# language CPP #-}
-- | = Name
--
-- VK_AMD_shader_early_and_late_fragment_tests - device extension
--
-- == VK_AMD_shader_early_and_late_fragment_tests
--
-- [__Name String__]
--     @VK_AMD_shader_early_and_late_fragment_tests@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     322
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
-- [__Contact__]
--
--     -   Tobias Hector
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_AMD_shader_early_and_late_fragment_tests] @tobski%0A*Here describe the issue or question you have about the VK_AMD_shader_early_and_late_fragment_tests extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_AMD_shader_early_and_late_fragment_tests.adoc VK_AMD_shader_early_and_late_fragment_tests>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-09-14
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/AMD/SPV_AMD_shader_early_and_late_fragment_tests.html SPV_AMD_shader_early_and_late_fragment_tests>
--
--     -   This extension interacts with @VK_EXT_shader_stencil_export@
--
-- [__Contributors__]
--
--     -   Tobias Hector, AMD
--
-- == Description
--
-- This extension adds support for the
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/AMD/SPV_AMD_shader_early_and_late_fragment_tests.html SPV_AMD_shader_early_and_late_fragment_tests>
-- extension, allowing shaders to explicitly opt in to allowing both early
-- /and/ late fragment tests with the @EarlyAndLateFragmentTestsAMD@
-- execution mode.
--
-- If @VK_EXT_shader_stencil_export@ is supported, additional execution
-- modes allowing early depth tests similar to @DepthUnchanged@,
-- @DepthLess@, and @DepthGreater@ are provided.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderEarlyAndLateFragmentTestsFeaturesAMD'
--
-- == New Enum Constants
--
-- -   'AMD_SHADER_EARLY_AND_LATE_FRAGMENT_TESTS_EXTENSION_NAME'
--
-- -   'AMD_SHADER_EARLY_AND_LATE_FRAGMENT_TESTS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_EARLY_AND_LATE_FRAGMENT_TESTS_FEATURES_AMD'
--
-- == Version History
--
-- -   Revision 1, 2021-09-14 (Tobias Hector)
--
--     -   Initial draft
--
-- == See Also
--
-- 'PhysicalDeviceShaderEarlyAndLateFragmentTestsFeaturesAMD'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_AMD_shader_early_and_late_fragment_tests Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_AMD_shader_early_and_late_fragment_tests  ( PhysicalDeviceShaderEarlyAndLateFragmentTestsFeaturesAMD(..)
                                                                      , AMD_SHADER_EARLY_AND_LATE_FRAGMENT_TESTS_SPEC_VERSION
                                                                      , pattern AMD_SHADER_EARLY_AND_LATE_FRAGMENT_TESTS_SPEC_VERSION
                                                                      , AMD_SHADER_EARLY_AND_LATE_FRAGMENT_TESTS_EXTENSION_NAME
                                                                      , pattern AMD_SHADER_EARLY_AND_LATE_FRAGMENT_TESTS_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_EARLY_AND_LATE_FRAGMENT_TESTS_FEATURES_AMD))
-- | VkPhysicalDeviceShaderEarlyAndLateFragmentTestsFeaturesAMD - Structure
-- describing whether early and late fragment tests can be supported by an
-- implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceShaderEarlyAndLateFragmentTestsFeaturesAMD'
-- structure is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceShaderEarlyAndLateFragmentTestsFeaturesAMD'
-- /can/ also be used in the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively enable these
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_shader_early_and_late_fragment_tests VK_AMD_shader_early_and_late_fragment_tests>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderEarlyAndLateFragmentTestsFeaturesAMD = PhysicalDeviceShaderEarlyAndLateFragmentTestsFeaturesAMD
  { -- | #features-shaderEarlyAndLateFragmentTests#
    -- @shaderEarlyAndLateFragmentTests@ indicates whether the implementation
    -- supports the @EarlyAndLateFragmentTestsAMD@ @Execution@ @Mode@.
    shaderEarlyAndLateFragmentTests :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderEarlyAndLateFragmentTestsFeaturesAMD)
#endif
deriving instance Show PhysicalDeviceShaderEarlyAndLateFragmentTestsFeaturesAMD

instance ToCStruct PhysicalDeviceShaderEarlyAndLateFragmentTestsFeaturesAMD where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderEarlyAndLateFragmentTestsFeaturesAMD{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_EARLY_AND_LATE_FRAGMENT_TESTS_FEATURES_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderEarlyAndLateFragmentTests))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_EARLY_AND_LATE_FRAGMENT_TESTS_FEATURES_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderEarlyAndLateFragmentTestsFeaturesAMD where
  peekCStruct p = do
    shaderEarlyAndLateFragmentTests <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderEarlyAndLateFragmentTestsFeaturesAMD
             (bool32ToBool shaderEarlyAndLateFragmentTests)

instance Storable PhysicalDeviceShaderEarlyAndLateFragmentTestsFeaturesAMD where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderEarlyAndLateFragmentTestsFeaturesAMD where
  zero = PhysicalDeviceShaderEarlyAndLateFragmentTestsFeaturesAMD
           zero


type AMD_SHADER_EARLY_AND_LATE_FRAGMENT_TESTS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_AMD_SHADER_EARLY_AND_LATE_FRAGMENT_TESTS_SPEC_VERSION"
pattern AMD_SHADER_EARLY_AND_LATE_FRAGMENT_TESTS_SPEC_VERSION :: forall a . Integral a => a
pattern AMD_SHADER_EARLY_AND_LATE_FRAGMENT_TESTS_SPEC_VERSION = 1


type AMD_SHADER_EARLY_AND_LATE_FRAGMENT_TESTS_EXTENSION_NAME = "VK_AMD_shader_early_and_late_fragment_tests"

-- No documentation found for TopLevel "VK_AMD_SHADER_EARLY_AND_LATE_FRAGMENT_TESTS_EXTENSION_NAME"
pattern AMD_SHADER_EARLY_AND_LATE_FRAGMENT_TESTS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern AMD_SHADER_EARLY_AND_LATE_FRAGMENT_TESTS_EXTENSION_NAME = "VK_AMD_shader_early_and_late_fragment_tests"

