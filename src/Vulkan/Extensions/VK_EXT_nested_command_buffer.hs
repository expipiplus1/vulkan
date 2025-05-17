{-# language CPP #-}
-- | = Name
--
-- VK_EXT_nested_command_buffer - device extension
--
-- == VK_EXT_nested_command_buffer
--
-- [__Name String__]
--     @VK_EXT_nested_command_buffer@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     452
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
-- [__Contact__]
--
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_nested_command_buffer] @pdaniell-nv%0A*Here describe the issue or question you have about the VK_EXT_nested_command_buffer extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-09-18
--
-- [__Contributors__]
--
--     -   Daniel Story, Nintendo
--
--     -   Peter Kohaut, NVIDIA
--
--     -   Shahbaz Youssefi, Google
--
--     -   Slawomir Grajewski, Intel
--
--     -   Stu Smith, AMD
--
-- == Description
--
-- With core Vulkan it is not legal to call
-- 'Vulkan.Core10.CommandBufferBuilding.cmdExecuteCommands' when recording
-- a secondary command buffer. This extension relaxes that restriction,
-- allowing secondary command buffers to execute other secondary command
-- buffers.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceNestedCommandBufferFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceNestedCommandBufferPropertiesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_NESTED_COMMAND_BUFFER_EXTENSION_NAME'
--
-- -   'EXT_NESTED_COMMAND_BUFFER_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core13.Enums.RenderingFlagBits.RenderingFlagBits':
--
--     -   'Vulkan.Core13.Enums.RenderingFlagBits.RENDERING_CONTENTS_INLINE_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_NESTED_COMMAND_BUFFER_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_NESTED_COMMAND_BUFFER_PROPERTIES_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.SubpassContents.SubpassContents':
--
--     -   'Vulkan.Core10.Enums.SubpassContents.SUBPASS_CONTENTS_INLINE_AND_SECONDARY_COMMAND_BUFFERS_EXT'
--
-- == Issues
--
-- 1) The Command Buffer Levels property for the Vulkan commands comes from
-- the @cmdbufferlevel@ attribute in @vk.xml@ for the command, and it is
-- currently not possible to modify this attribute based on whether an
-- extension is enabled. For this extension we want the @cmdbufferlevel@
-- attribute for vkCmdExecuteCommands to be @primary,secondary@ when this
-- extension is enabled and @primary@ otherwise.
--
-- __RESOLVED__: The @cmdbufferlevel@ attribute for
-- 'Vulkan.Core10.CommandBufferBuilding.cmdExecuteCommands' has been
-- changed to @primary,secondary@ and a new VUID added to prohibit
-- recording this command in a secondary command buffer unless this
-- extension is enabled.
--
-- == Version History
--
-- -   Revision 1, 2023-09-18 (Piers Daniell)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'PhysicalDeviceNestedCommandBufferFeaturesEXT',
-- 'PhysicalDeviceNestedCommandBufferPropertiesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_nested_command_buffer Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_nested_command_buffer  ( PhysicalDeviceNestedCommandBufferFeaturesEXT(..)
                                                       , PhysicalDeviceNestedCommandBufferPropertiesEXT(..)
                                                       , EXT_NESTED_COMMAND_BUFFER_SPEC_VERSION
                                                       , pattern EXT_NESTED_COMMAND_BUFFER_SPEC_VERSION
                                                       , EXT_NESTED_COMMAND_BUFFER_EXTENSION_NAME
                                                       , pattern EXT_NESTED_COMMAND_BUFFER_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_NESTED_COMMAND_BUFFER_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_NESTED_COMMAND_BUFFER_PROPERTIES_EXT))
-- | VkPhysicalDeviceNestedCommandBufferFeaturesEXT - Structure describing
-- whether nested command buffers are supported by the implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceNestedCommandBufferFeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceNestedCommandBufferFeaturesEXT' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_nested_command_buffer VK_EXT_nested_command_buffer>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceNestedCommandBufferFeaturesEXT = PhysicalDeviceNestedCommandBufferFeaturesEXT
  { -- | #features-nestedCommandBuffer# @nestedCommandBuffer@ indicates the
    -- implementation supports nested command buffers, which allows
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#glossary Secondary Command Buffers>
    -- to execute other
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#glossary Secondary Command Buffers>.
    nestedCommandBuffer :: Bool
  , -- | #features-nestedCommandBufferRendering# @nestedCommandBufferRendering@
    -- indicates that it is valid to call
    -- 'Vulkan.Core10.CommandBufferBuilding.cmdExecuteCommands' inside a
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#glossary Secondary Command Buffer>
    -- recorded with
    -- 'Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT'.
    nestedCommandBufferRendering :: Bool
  , -- | #features-nestedCommandBufferSimultaneousUse#
    -- @nestedCommandBufferSimultaneousUse@ indicates that the implementation
    -- supports nested command buffers with command buffers that are recorded
    -- with
    -- 'Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT'.
    nestedCommandBufferSimultaneousUse :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceNestedCommandBufferFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceNestedCommandBufferFeaturesEXT

instance ToCStruct PhysicalDeviceNestedCommandBufferFeaturesEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceNestedCommandBufferFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_NESTED_COMMAND_BUFFER_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (nestedCommandBuffer))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (nestedCommandBufferRendering))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (nestedCommandBufferSimultaneousUse))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_NESTED_COMMAND_BUFFER_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceNestedCommandBufferFeaturesEXT where
  peekCStruct p = do
    nestedCommandBuffer <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    nestedCommandBufferRendering <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    nestedCommandBufferSimultaneousUse <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    pure $ PhysicalDeviceNestedCommandBufferFeaturesEXT
             (bool32ToBool nestedCommandBuffer)
             (bool32ToBool nestedCommandBufferRendering)
             (bool32ToBool nestedCommandBufferSimultaneousUse)

instance Storable PhysicalDeviceNestedCommandBufferFeaturesEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceNestedCommandBufferFeaturesEXT where
  zero = PhysicalDeviceNestedCommandBufferFeaturesEXT
           zero
           zero
           zero


-- | VkPhysicalDeviceNestedCommandBufferPropertiesEXT - Structure describing
-- the nested command buffer limits of an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceNestedCommandBufferPropertiesEXT'
-- structure describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceNestedCommandBufferPropertiesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_nested_command_buffer VK_EXT_nested_command_buffer>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceNestedCommandBufferPropertiesEXT = PhysicalDeviceNestedCommandBufferPropertiesEXT
  { -- | #limits-maxCommandBufferNestingLevel# @maxCommandBufferNestingLevel@
    -- indicates the maximum nesting level of calls to
    -- 'Vulkan.Core10.CommandBufferBuilding.cmdExecuteCommands' from
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#glossary Secondary Command Buffers>.
    -- A @maxCommandBufferNestingLevel@ of @UINT32_MAX@ means there is no limit
    -- to the nesting level.
    maxCommandBufferNestingLevel :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceNestedCommandBufferPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceNestedCommandBufferPropertiesEXT

instance ToCStruct PhysicalDeviceNestedCommandBufferPropertiesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceNestedCommandBufferPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_NESTED_COMMAND_BUFFER_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxCommandBufferNestingLevel)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_NESTED_COMMAND_BUFFER_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceNestedCommandBufferPropertiesEXT where
  peekCStruct p = do
    maxCommandBufferNestingLevel <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ PhysicalDeviceNestedCommandBufferPropertiesEXT
             maxCommandBufferNestingLevel

instance Storable PhysicalDeviceNestedCommandBufferPropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceNestedCommandBufferPropertiesEXT where
  zero = PhysicalDeviceNestedCommandBufferPropertiesEXT
           zero


type EXT_NESTED_COMMAND_BUFFER_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_NESTED_COMMAND_BUFFER_SPEC_VERSION"
pattern EXT_NESTED_COMMAND_BUFFER_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_NESTED_COMMAND_BUFFER_SPEC_VERSION = 1


type EXT_NESTED_COMMAND_BUFFER_EXTENSION_NAME = "VK_EXT_nested_command_buffer"

-- No documentation found for TopLevel "VK_EXT_NESTED_COMMAND_BUFFER_EXTENSION_NAME"
pattern EXT_NESTED_COMMAND_BUFFER_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_NESTED_COMMAND_BUFFER_EXTENSION_NAME = "VK_EXT_nested_command_buffer"

