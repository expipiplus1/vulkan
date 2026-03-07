{-# language CPP #-}
-- | = Name
--
-- VK_KHR_shader_relaxed_extended_instruction - device extension
--
-- = VK_KHR_shader_relaxed_extended_instruction
--
-- [__Name String__]
--     @VK_KHR_shader_relaxed_extended_instruction@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     559
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     None
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_relaxed_extended_instruction.html SPV_KHR_relaxed_extended_instruction>
--
-- [__Contact__]
--
--     -   Nathan Gauër
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_shader_relaxed_extended_instruction] @Keenuts%0A*Here describe the issue or question you have about the VK_KHR_shader_relaxed_extended_instruction extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_shader_relaxed_extended_instruction.adoc VK_KHR_shader_relaxed_extended_instruction>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-01-24
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Alan Baker, Google LLC
--
--     -   Nathan Gauër, Google LLC
--
-- == Description
--
-- This extension allows the use of the
-- @SPV_KHR_relaxed_extended_instruction@ extension in SPIR-V shader
-- modules.
--
-- It adds a new SPIR-V instruction, which allows some usage of forward
-- references in non-semantic instruction sets. This extensions interacts
-- with the @SPV_KHR_non_semantic_info@ extension, hence with
-- @VK_KHR_shader_non_semantic_info@.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderRelaxedExtendedInstructionFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SHADER_RELAXED_EXTENDED_INSTRUCTION_EXTENSION_NAME'
--
-- -   'KHR_SHADER_RELAXED_EXTENDED_INSTRUCTION_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_RELAXED_EXTENDED_INSTRUCTION_FEATURES_KHR'
--
-- == Version History
--
-- -   Revision 1, 2024-01-24 (Nathan Gauër)
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
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_shader_relaxed_extended_instruction Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_shader_relaxed_extended_instruction  ( PhysicalDeviceShaderRelaxedExtendedInstructionFeaturesKHR(..)
                                                                     , KHR_SHADER_RELAXED_EXTENDED_INSTRUCTION_SPEC_VERSION
                                                                     , pattern KHR_SHADER_RELAXED_EXTENDED_INSTRUCTION_SPEC_VERSION
                                                                     , KHR_SHADER_RELAXED_EXTENDED_INSTRUCTION_EXTENSION_NAME
                                                                     , pattern KHR_SHADER_RELAXED_EXTENDED_INSTRUCTION_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_RELAXED_EXTENDED_INSTRUCTION_FEATURES_KHR))
-- | VkPhysicalDeviceShaderRelaxedExtendedInstructionFeaturesKHR - Structure
-- describing support for VK_KHR_shader_relaxed_extended_instruction an
-- implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceShaderRelaxedExtendedInstructionFeaturesKHR'
-- structure is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceShaderRelaxedExtendedInstructionFeaturesKHR'
-- /can/ also be used in the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively enable these
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_relaxed_extended_instruction VK_KHR_shader_relaxed_extended_instruction>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderRelaxedExtendedInstructionFeaturesKHR = PhysicalDeviceShaderRelaxedExtendedInstructionFeaturesKHR
  { -- | #features-shaderRelaxedExtendedInstruction#
    -- @shaderRelaxedExtendedInstruction@ specifies whether the implementation
    -- supports SPIR-V modules that use the
    -- @SPV_KHR_relaxed_extended_instruction@ extension.
    shaderRelaxedExtendedInstruction :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderRelaxedExtendedInstructionFeaturesKHR)
#endif
deriving instance Show PhysicalDeviceShaderRelaxedExtendedInstructionFeaturesKHR

instance ToCStruct PhysicalDeviceShaderRelaxedExtendedInstructionFeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderRelaxedExtendedInstructionFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_RELAXED_EXTENDED_INSTRUCTION_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderRelaxedExtendedInstruction))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_RELAXED_EXTENDED_INSTRUCTION_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderRelaxedExtendedInstructionFeaturesKHR where
  peekCStruct p = do
    shaderRelaxedExtendedInstruction <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderRelaxedExtendedInstructionFeaturesKHR
             (bool32ToBool shaderRelaxedExtendedInstruction)

instance Storable PhysicalDeviceShaderRelaxedExtendedInstructionFeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderRelaxedExtendedInstructionFeaturesKHR where
  zero = PhysicalDeviceShaderRelaxedExtendedInstructionFeaturesKHR
           zero


type KHR_SHADER_RELAXED_EXTENDED_INSTRUCTION_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_SHADER_RELAXED_EXTENDED_INSTRUCTION_SPEC_VERSION"
pattern KHR_SHADER_RELAXED_EXTENDED_INSTRUCTION_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SHADER_RELAXED_EXTENDED_INSTRUCTION_SPEC_VERSION = 1


type KHR_SHADER_RELAXED_EXTENDED_INSTRUCTION_EXTENSION_NAME = "VK_KHR_shader_relaxed_extended_instruction"

-- No documentation found for TopLevel "VK_KHR_SHADER_RELAXED_EXTENDED_INSTRUCTION_EXTENSION_NAME"
pattern KHR_SHADER_RELAXED_EXTENDED_INSTRUCTION_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SHADER_RELAXED_EXTENDED_INSTRUCTION_EXTENSION_NAME = "VK_KHR_shader_relaxed_extended_instruction"

