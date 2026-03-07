{-# language CPP #-}
-- | = Name
--
-- VK_NV_push_constant_bank - device extension
--
-- = VK_NV_push_constant_bank
--
-- [__Name String__]
--     @VK_NV_push_constant_bank@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     581
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     None
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_push_constant_bank.html SPV_NV_push_constant_bank>
--
-- [__Contact__]
--
--     -   Vassili Nikolaev
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_push_constant_bank] @vasnik1%0A*Here describe the issue or question you have about the VK_NV_push_constant_bank extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_NV_push_constant_bank.adoc VK_NV_push_constant_bank>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-09-15
--
-- [__Contributors__]
--
--     -   Pat Brown, NVIDIA
--
--     -   Piers Daniell, NVIDIA
--
--     -   Rodrigo Locatti, NVIDIA
--
--     -   Daniel Story, Nintendo
--
-- == Description
--
-- The
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_push_constant_bank VK_NV_push_constant_bank>
-- extension allows applications to specify a bank and offset for push
-- constants, enabling more flexible push constant management in descriptor
-- heap scenarios where shaders are able to access different root
-- descriptors.
--
-- Traditional push constants are placed in a default location, but this
-- extension allows applications to specify which hardware constant bank to
-- use and at what offset within that bank. This provides greater control
-- over memory layout and enables more efficient use of hardware resources
-- in advanced descriptor heap configurations.
--
-- The extension integrates with
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>
-- by allowing 'PushConstantBankInfoNV' structures to be chained to
-- 'Vulkan.Extensions.VK_EXT_descriptor_heap.DescriptorSetAndBindingMappingEXT',
-- 'Vulkan.Extensions.VK_EXT_descriptor_heap.PushDataInfoEXT',
-- 'Vulkan.Core14.Promoted_From_VK_KHR_maintenance6AdditionalFunctionality'.PushConstantsInfo',
-- or
-- 'Vulkan.Extensions.VK_EXT_device_generated_commands.IndirectCommandsLayoutTokenEXT'
-- structures, specifying the hardware bank where push constants should be
-- placed as part of the descriptor heap mapping configuration or push data
-- operations.
--
-- Key features include:
--
-- -   Bank and offset specification for push constant placement
--
-- -   Integration with descriptor heap mapping through structure chaining
--
-- -   Support for GLSL shader qualifiers for bank and offset specification
--     in SPIR-V
--
-- -   Validation of bank bounds and alignment requirements
--
-- -   Compatibility with existing push constant API
--
-- The number of available push constant banks is implementation-dependent
-- and can be queried through separate limits in
-- 'PhysicalDevicePushConstantBankPropertiesNV':
-- @maxGraphicsPushConstantBanks@ and @maxComputePushConstantBanks@ for
-- non-descriptor heap usage, and @maxGraphicsPushDataBanks@ and
-- @maxComputePushDataBanks@ for descriptor heap scenarios. Applications
-- must ensure bank indices remain within the appropriate
-- implementation-defined range based on the shader type and usage context.
--
-- Shader support for banks and member offsets are defined by the
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_push_constant_bank.html SPV_NV_push_constant_bank>
-- SPIR-V extension, which can be used with the
-- <https://github.com/KhronosGroup/GLSL/blob/main/extensions/nv/GLSL_NV_push_constant_bank.txt GLSL_NV_push_constant_bank>
-- GLSL extension.
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-PushConstantBanksNV PushConstantBanksNV>
--
-- == New SPIR-V Decorations
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#shaders-pushconstant-decorations-banknv BankNV>
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#shaders-pushconstant-decorations-memberoffsetnv MemberOffsetNV>
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DescriptorSetAndBindingMappingEXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.PushDataInfoEXT',
--     'Vulkan.Core14.Promoted_From_VK_KHR_maintenance6AdditionalFunctionality'.PushConstantsInfo',
--     'Vulkan.Extensions.VK_EXT_device_generated_commands.IndirectCommandsLayoutTokenEXT':
--
--     -   'PushConstantBankInfoNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePushConstantBankFeaturesNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDevicePushConstantBankPropertiesNV'
--
-- == New Enum Constants
--
-- -   'NV_PUSH_CONSTANT_BANK_EXTENSION_NAME'
--
-- -   'NV_PUSH_CONSTANT_BANK_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_CONSTANT_BANK_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_CONSTANT_BANK_PROPERTIES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PUSH_CONSTANT_BANK_INFO_NV'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2025-09-15 (NVIDIA Vassili Nikolaev)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_NV_push_constant_bank Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_push_constant_bank  ( PushConstantBankInfoNV(..)
                                                   , PhysicalDevicePushConstantBankFeaturesNV(..)
                                                   , PhysicalDevicePushConstantBankPropertiesNV(..)
                                                   , NV_PUSH_CONSTANT_BANK_SPEC_VERSION
                                                   , pattern NV_PUSH_CONSTANT_BANK_SPEC_VERSION
                                                   , NV_PUSH_CONSTANT_BANK_EXTENSION_NAME
                                                   , pattern NV_PUSH_CONSTANT_BANK_EXTENSION_NAME
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
import Data.Word (Word32)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_CONSTANT_BANK_FEATURES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_CONSTANT_BANK_PROPERTIES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PUSH_CONSTANT_BANK_INFO_NV))
-- | VkPushConstantBankInfoNV - Structure specifying push constant bank
-- information
--
-- = Description
--
-- This structure /can/ be chained to
-- 'Vulkan.Extensions.VK_EXT_descriptor_heap.PushDataInfoEXT',
-- 'Vulkan.Core14.Promoted_From_VK_KHR_maintenance6AdditionalFunctionality'.PushConstantsInfo',
-- 'Vulkan.Extensions.VK_EXT_descriptor_heap.DescriptorSetAndBindingMappingEXT',
-- and
-- 'Vulkan.Extensions.VK_EXT_device_generated_commands.IndirectCommandsLayoutTokenEXT'
-- via the @pNext@ chain to specify push constant bank placement:
--
-- -   When chained to
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.PushDataInfoEXT', it
--     specifies the hardware bank into which
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.cmdPushDataEXT' pushes the
--     data.
--
-- -   When chained to
--     'Vulkan.Core14.Promoted_From_VK_KHR_maintenance6AdditionalFunctionality'.PushConstantsInfo',
--     it specifies the hardware bank into which
--     'Vulkan.Core14.Promoted_From_VK_KHR_maintenance6AdditionalFunctionality'.cmdPushConstants2'
--     pushes the constants.
--
-- -   When chained to
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DescriptorSetAndBindingMappingEXT',
--     it specifies the hardware push data bank from which the push data is
--     read.
--
-- -   When chained to
--     'Vulkan.Extensions.VK_EXT_device_generated_commands.IndirectCommandsLayoutTokenEXT'
--     with
--     'Vulkan.Extensions.VK_EXT_device_generated_commands.INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_DATA_EXT',
--     it specifies the hardware bank into which indirect push data is
--     placed.
--
-- This allows for more flexible push constant management in descriptor
-- heap scenarios where shaders access different root descriptors with
-- specific bank requirements.
--
-- == Valid Usage
--
-- -   #VUID-VkPushConstantBankInfoNV-bank-12342# When chained to
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.PushDataInfoEXT', if the
--     command buffer is executing graphics operations, @bank@ /must/ be
--     less than
--     'PhysicalDevicePushConstantBankPropertiesNV'::@maxGraphicsPushDataBanks@
--
-- -   #VUID-VkPushConstantBankInfoNV-bank-12343# When chained to
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.PushDataInfoEXT', if the
--     command buffer is executing compute operations, @bank@ /must/ be
--     less than
--     'PhysicalDevicePushConstantBankPropertiesNV'::@maxComputePushDataBanks@
--
-- -   #VUID-VkPushConstantBankInfoNV-bank-12344# When chained to
--     'Vulkan.Core14.Promoted_From_VK_KHR_maintenance6AdditionalFunctionality'.PushConstantsInfo',
--     if VkPushConstantsInfo::stageFlags includes a graphics stage then
--     @bank@ /must/ be less than
--     'PhysicalDevicePushConstantBankPropertiesNV'::@maxGraphicsPushConstantBanks@
--
-- -   #VUID-VkPushConstantBankInfoNV-bank-12345# When chained to
--     'Vulkan.Core14.Promoted_From_VK_KHR_maintenance6AdditionalFunctionality'.PushConstantsInfo',
--     if VkPushConstantsInfo::stageFlags includes a compute stage then
--     @bank@ /must/ be less than
--     'PhysicalDevicePushConstantBankPropertiesNV'::@maxComputePushConstantBanks@
--
-- -   #VUID-VkPushConstantBankInfoNV-bank-12346# When chained to
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DescriptorSetAndBindingMappingEXT'
--     for a graphics shader stage, @bank@ /must/ be less than
--     'PhysicalDevicePushConstantBankPropertiesNV'::@maxGraphicsPushDataBanks@
--
-- -   #VUID-VkPushConstantBankInfoNV-bank-12347# When chained to
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DescriptorSetAndBindingMappingEXT'
--     for a compute shader stage, @bank@ /must/ be less than
--     'PhysicalDevicePushConstantBankPropertiesNV'::@maxComputePushDataBanks@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPushConstantBankInfoNV-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PUSH_CONSTANT_BANK_INFO_NV'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_push_constant_bank VK_NV_push_constant_bank>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PushConstantBankInfoNV = PushConstantBankInfoNV
  { -- | @bank@ is the index of the hardware bank into which the data is pushed.
    bank :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PushConstantBankInfoNV)
#endif
deriving instance Show PushConstantBankInfoNV

instance ToCStruct PushConstantBankInfoNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PushConstantBankInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PUSH_CONSTANT_BANK_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (bank)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PUSH_CONSTANT_BANK_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct PushConstantBankInfoNV where
  peekCStruct p = do
    bank <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ PushConstantBankInfoNV
             bank

instance Storable PushConstantBankInfoNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PushConstantBankInfoNV where
  zero = PushConstantBankInfoNV
           zero


-- | VkPhysicalDevicePushConstantBankFeaturesNV - Structure describing push
-- constant bank features
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDevicePushConstantBankFeaturesNV' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDevicePushConstantBankFeaturesNV', it /must/ add an instance of
-- the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_push_constant_bank VK_NV_push_constant_bank>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePushConstantBankFeaturesNV = PhysicalDevicePushConstantBankFeaturesNV
  { -- | #features-pushConstantBank# @pushConstantBank@ indicates whether push
    -- constant bank functionality is supported.
    pushConstantBank :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePushConstantBankFeaturesNV)
#endif
deriving instance Show PhysicalDevicePushConstantBankFeaturesNV

instance ToCStruct PhysicalDevicePushConstantBankFeaturesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePushConstantBankFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_CONSTANT_BANK_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (pushConstantBank))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_CONSTANT_BANK_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDevicePushConstantBankFeaturesNV where
  peekCStruct p = do
    pushConstantBank <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDevicePushConstantBankFeaturesNV
             (bool32ToBool pushConstantBank)

instance Storable PhysicalDevicePushConstantBankFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePushConstantBankFeaturesNV where
  zero = PhysicalDevicePushConstantBankFeaturesNV
           zero


-- | VkPhysicalDevicePushConstantBankPropertiesNV - Structure describing push
-- constant bank properties
--
-- = Members
--
-- This structure describes the following implementation-dependent limits:
--
-- = Description
--
-- If the 'PhysicalDevicePushConstantBankPropertiesNV' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- The number of banks available for descriptor heap usage
-- (@maxGraphicsPushDataBanks@ and @maxComputePushDataBanks@) is equal or
-- greater than the number of banks available for non-descriptor heap usage
-- (@maxGraphicsPushConstantBanks@ and @maxComputePushConstantBanks@).
--
-- For graphics shaders, both descriptor heap and non-descriptor heap
-- limits are greater than 1. For compute shaders, the number of banks is
-- equal to or greater than 1.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_push_constant_bank VK_NV_push_constant_bank>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePushConstantBankPropertiesNV = PhysicalDevicePushConstantBankPropertiesNV
  { -- | @maxGraphicsPushConstantBanks@ indicates the maximum number of push
    -- constant banks supported for graphics pipelines when used with
    -- non-descriptor heap scenarios.
    maxGraphicsPushConstantBanks :: Word32
  , -- | @maxComputePushConstantBanks@ indicates the maximum number of push
    -- constant banks supported for compute pipelines when used with
    -- non-descriptor heap scenarios.
    maxComputePushConstantBanks :: Word32
  , -- | @maxGraphicsPushDataBanks@ indicates the maximum number of push data
    -- banks supported for graphics pipelines when using descriptor heaps.
    maxGraphicsPushDataBanks :: Word32
  , -- | @maxComputePushDataBanks@ indicates the maximum number of push data
    -- banks supported for compute pipelines when using descriptor heaps.
    maxComputePushDataBanks :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePushConstantBankPropertiesNV)
#endif
deriving instance Show PhysicalDevicePushConstantBankPropertiesNV

instance ToCStruct PhysicalDevicePushConstantBankPropertiesNV where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePushConstantBankPropertiesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_CONSTANT_BANK_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxGraphicsPushConstantBanks)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxComputePushConstantBanks)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (maxGraphicsPushDataBanks)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (maxComputePushDataBanks)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_CONSTANT_BANK_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDevicePushConstantBankPropertiesNV where
  peekCStruct p = do
    maxGraphicsPushConstantBanks <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxComputePushConstantBanks <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    maxGraphicsPushDataBanks <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    maxComputePushDataBanks <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    pure $ PhysicalDevicePushConstantBankPropertiesNV
             maxGraphicsPushConstantBanks
             maxComputePushConstantBanks
             maxGraphicsPushDataBanks
             maxComputePushDataBanks

instance Storable PhysicalDevicePushConstantBankPropertiesNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePushConstantBankPropertiesNV where
  zero = PhysicalDevicePushConstantBankPropertiesNV
           zero
           zero
           zero
           zero


type NV_PUSH_CONSTANT_BANK_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_PUSH_CONSTANT_BANK_SPEC_VERSION"
pattern NV_PUSH_CONSTANT_BANK_SPEC_VERSION :: forall a . Integral a => a
pattern NV_PUSH_CONSTANT_BANK_SPEC_VERSION = 1


type NV_PUSH_CONSTANT_BANK_EXTENSION_NAME = "VK_NV_push_constant_bank"

-- No documentation found for TopLevel "VK_NV_PUSH_CONSTANT_BANK_EXTENSION_NAME"
pattern NV_PUSH_CONSTANT_BANK_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_PUSH_CONSTANT_BANK_EXTENSION_NAME = "VK_NV_push_constant_bank"

