{-# language CPP #-}
-- | = Name
--
-- VK_KHR_shader_expect_assume - device extension
--
-- = VK_KHR_shader_expect_assume
--
-- [__Name String__]
--     @VK_KHR_shader_expect_assume@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     545
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
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_expect_assume.html SPV_KHR_expect_assume>
--
-- [__Contact__]
--
--     -   Kevin Petit
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_shader_expect_assume] @kpet%0A*Here describe the issue or question you have about the VK_KHR_shader_expect_assume extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_shader_expect_assume.adoc VK_KHR_shader_expect_assume>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-12-06
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Kevin Petit, Arm
--
--     -   Tobias Hector, AMD
--
--     -   James Fitzpatrick, Imagination Technologies
--
-- == Description
--
-- This extension allows the use of the @SPV_KHR_expect_assume@ extension
-- in SPIR-V shader modules which enables SPIR-V producers to provide
-- optimization hints to the Vulkan implementation.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderExpectAssumeFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SHADER_EXPECT_ASSUME_EXTENSION_NAME'
--
-- -   'KHR_SHADER_EXPECT_ASSUME_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_EXPECT_ASSUME_FEATURES_KHR'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-ExpectAssumeKHR ExpectAssumeKHR>
--
-- == Version History
--
-- -   Revision 1, 2023-12-06 (Kevin Petit)
--
--     -   Initial revision
--
-- == See Also
--
-- 'PhysicalDeviceShaderExpectAssumeFeaturesKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_shader_expect_assume Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_shader_expect_assume  ( PhysicalDeviceShaderExpectAssumeFeaturesKHR(..)
                                                      , KHR_SHADER_EXPECT_ASSUME_SPEC_VERSION
                                                      , pattern KHR_SHADER_EXPECT_ASSUME_SPEC_VERSION
                                                      , KHR_SHADER_EXPECT_ASSUME_EXTENSION_NAME
                                                      , pattern KHR_SHADER_EXPECT_ASSUME_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_EXPECT_ASSUME_FEATURES_KHR))
-- | VkPhysicalDeviceShaderExpectAssumeFeaturesKHR - Structure describing
-- shader expect assume features that can be supported by an implementation
--
-- = Description
--
-- If the 'PhysicalDeviceShaderExpectAssumeFeaturesKHR' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceShaderExpectAssumeFeaturesKHR' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_expect_assume VK_KHR_shader_expect_assume>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderExpectAssumeFeaturesKHR = PhysicalDeviceShaderExpectAssumeFeaturesKHR
  { -- | #features-shaderExpectAssume# @shaderExpectAssume@ specifies whether
    -- shader modules /can/ declare the @ExpectAssumeKHR@ capability.
    shaderExpectAssume :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderExpectAssumeFeaturesKHR)
#endif
deriving instance Show PhysicalDeviceShaderExpectAssumeFeaturesKHR

instance ToCStruct PhysicalDeviceShaderExpectAssumeFeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderExpectAssumeFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_EXPECT_ASSUME_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderExpectAssume))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_EXPECT_ASSUME_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderExpectAssumeFeaturesKHR where
  peekCStruct p = do
    shaderExpectAssume <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderExpectAssumeFeaturesKHR
             (bool32ToBool shaderExpectAssume)

instance Storable PhysicalDeviceShaderExpectAssumeFeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderExpectAssumeFeaturesKHR where
  zero = PhysicalDeviceShaderExpectAssumeFeaturesKHR
           zero


type KHR_SHADER_EXPECT_ASSUME_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_SHADER_EXPECT_ASSUME_SPEC_VERSION"
pattern KHR_SHADER_EXPECT_ASSUME_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SHADER_EXPECT_ASSUME_SPEC_VERSION = 1


type KHR_SHADER_EXPECT_ASSUME_EXTENSION_NAME = "VK_KHR_shader_expect_assume"

-- No documentation found for TopLevel "VK_KHR_SHADER_EXPECT_ASSUME_EXTENSION_NAME"
pattern KHR_SHADER_EXPECT_ASSUME_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SHADER_EXPECT_ASSUME_EXTENSION_NAME = "VK_KHR_shader_expect_assume"

