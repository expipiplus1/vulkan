{-# language CPP #-}
-- | = Name
--
-- VK_EXT_inline_uniform_block - device extension
--
-- == VK_EXT_inline_uniform_block
--
-- [__Name String__]
--     @VK_EXT_inline_uniform_block@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     139
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
--     -   Requires @VK_KHR_maintenance1@
--
-- [__Contact__]
--
--     -   Daniel Rakos
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_inline_uniform_block] @aqnuep%0A<<Here describe the issue or question you have about the VK_EXT_inline_uniform_block extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-08-01
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Daniel Rakos, AMD
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Slawomir Grajewski, Intel
--
--     -   Neil Henning, Codeplay
--
-- == Description
--
-- This extension introduces the ability to back uniform blocks directly
-- with descriptor sets by storing inline uniform data within descriptor
-- pool storage. Compared to push constants this new construct allows
-- uniform data to be reused across multiple disjoint sets of drawing or
-- dispatching commands and /may/ enable uniform data to be accessed with
-- fewer indirections compared to uniforms backed by buffer memory.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.DescriptorSet.DescriptorPoolCreateInfo':
--
--     -   'DescriptorPoolInlineUniformBlockCreateInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceInlineUniformBlockFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceInlineUniformBlockPropertiesEXT'
--
-- -   Extending 'Vulkan.Core10.DescriptorSet.WriteDescriptorSet':
--
--     -   'WriteDescriptorSetInlineUniformBlockEXT'
--
-- == New Enum Constants
--
-- -   'EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME'
--
-- -   'EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.DescriptorType.DescriptorType':
--
--     -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT'
--
-- == Issues
--
-- 1) Do we need a new storage class for inline uniform blocks vs uniform
-- blocks?
--
-- __RESOLVED__: No. The @Uniform@ storage class is used to allow the same
-- syntax used for both uniform buffers and inline uniform blocks.
--
-- 2) Is the descriptor array index and array size expressed in terms of
-- bytes or dwords for inline uniform block descriptors?
--
-- __RESOLVED__: In bytes, but both /must/ be a multiple of 4, similar to
-- how push constant ranges are specified. The @descriptorCount@ of
-- 'Vulkan.Core10.DescriptorSet.DescriptorSetLayoutBinding' thus provides
-- the total number of bytes a particular binding with an inline uniform
-- block descriptor type can hold, while the @srcArrayElement@,
-- @dstArrayElement@, and @descriptorCount@ members of
-- 'Vulkan.Core10.DescriptorSet.WriteDescriptorSet',
-- 'Vulkan.Core10.DescriptorSet.CopyDescriptorSet', and
-- 'Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.DescriptorUpdateTemplateEntry'
-- (where applicable) specify the byte offset and number of bytes to
-- write\/copy to the binding’s backing store. Additionally, the @stride@
-- member of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.DescriptorUpdateTemplateEntry'
-- is ignored for inline uniform blocks and a default value of one is used,
-- meaning that the data to update inline uniform block bindings with must
-- be contiguous in memory.
--
-- 3) What layout rules apply for uniform blocks corresponding to inline
-- constants?
--
-- __RESOLVED__: They use the same layout rules as uniform buffers.
--
-- 4) Do we need to add non-uniform indexing features\/properties as
-- introduced by @VK_EXT_descriptor_indexing@ for inline uniform blocks?
--
-- __RESOLVED__: No, because inline uniform blocks are not allowed to be
-- “arrayed”. A single binding with an inline uniform block descriptor type
-- corresponds to a single uniform block instance and the array indices
-- inside that binding refer to individual offsets within the uniform block
-- (see issue #2). However, this extension does introduce new
-- features\/properties about the level of support for update-after-bind
-- inline uniform blocks.
--
-- 5) Is the @descriptorBindingVariableDescriptorCount@ feature introduced
-- by @VK_EXT_descriptor_indexing@ supported for inline uniform blocks?
--
-- __RESOLVED__: Yes, as long as other inline uniform block specific limits
-- are respected.
--
-- 6) Do the robustness guarantees of @robustBufferAccess@ apply to inline
-- uniform block accesses?
--
-- __RESOLVED__: No, similarly to push constants, as they are not backed by
-- buffer memory like uniform buffers.
--
-- == Version History
--
-- -   Revision 1, 2018-08-01 (Daniel Rakos)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'DescriptorPoolInlineUniformBlockCreateInfoEXT',
-- 'PhysicalDeviceInlineUniformBlockFeaturesEXT',
-- 'PhysicalDeviceInlineUniformBlockPropertiesEXT',
-- 'WriteDescriptorSetInlineUniformBlockEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_inline_uniform_block Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_inline_uniform_block  ( PhysicalDeviceInlineUniformBlockFeaturesEXT(..)
                                                      , PhysicalDeviceInlineUniformBlockPropertiesEXT(..)
                                                      , WriteDescriptorSetInlineUniformBlockEXT(..)
                                                      , DescriptorPoolInlineUniformBlockCreateInfoEXT(..)
                                                      , EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION
                                                      , pattern EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION
                                                      , EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME
                                                      , pattern EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT))
-- | VkPhysicalDeviceInlineUniformBlockFeaturesEXT - Structure describing
-- inline uniform block features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceInlineUniformBlockFeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceInlineUniformBlockFeaturesEXT' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_inline_uniform_block VK_EXT_inline_uniform_block>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceInlineUniformBlockFeaturesEXT = PhysicalDeviceInlineUniformBlockFeaturesEXT
  { -- | #features-inlineUniformBlock# @inlineUniformBlock@ indicates whether the
    -- implementation supports inline uniform block descriptors. If this
    -- feature is not enabled,
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
    -- /must/ not be used.
    inlineUniformBlock :: Bool
  , -- | #features-descriptorBindingInlineUniformBlockUpdateAfterBind#
    -- @descriptorBindingInlineUniformBlockUpdateAfterBind@ indicates whether
    -- the implementation supports updating inline uniform block descriptors
    -- after a set is bound. If this feature is not enabled,
    -- 'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'
    -- /must/ not be used with
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'.
    descriptorBindingInlineUniformBlockUpdateAfterBind :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceInlineUniformBlockFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceInlineUniformBlockFeaturesEXT

instance ToCStruct PhysicalDeviceInlineUniformBlockFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceInlineUniformBlockFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (inlineUniformBlock))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (descriptorBindingInlineUniformBlockUpdateAfterBind))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceInlineUniformBlockFeaturesEXT where
  peekCStruct p = do
    inlineUniformBlock <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    descriptorBindingInlineUniformBlockUpdateAfterBind <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceInlineUniformBlockFeaturesEXT
             (bool32ToBool inlineUniformBlock) (bool32ToBool descriptorBindingInlineUniformBlockUpdateAfterBind)

instance Storable PhysicalDeviceInlineUniformBlockFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceInlineUniformBlockFeaturesEXT where
  zero = PhysicalDeviceInlineUniformBlockFeaturesEXT
           zero
           zero


-- | VkPhysicalDeviceInlineUniformBlockPropertiesEXT - Structure describing
-- inline uniform block properties that can be supported by an
-- implementation
--
-- = Description
--
-- If the 'PhysicalDeviceInlineUniformBlockPropertiesEXT' structure is
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_inline_uniform_block VK_EXT_inline_uniform_block>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceInlineUniformBlockPropertiesEXT = PhysicalDeviceInlineUniformBlockPropertiesEXT
  { -- | #limits-maxInlineUniformBlockSize# @maxInlineUniformBlockSize@ is the
    -- maximum size in bytes of an
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-inlineuniformblock inline uniform block>
    -- binding.
    maxInlineUniformBlockSize :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceInlineUniformBlockPropertiesEXT" "maxPerStageDescriptorInlineUniformBlocks"
    maxPerStageDescriptorInlineUniformBlocks :: Word32
  , -- | #limits-maxPerStageDescriptorUpdateAfterBindInlineUniformBlocks#
    -- @maxPerStageDescriptorUpdateAfterBindInlineUniformBlocks@ is similar to
    -- @maxPerStageDescriptorInlineUniformBlocks@ but counts descriptor
    -- bindings from descriptor sets created with or without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxPerStageDescriptorUpdateAfterBindInlineUniformBlocks :: Word32
  , -- | #limits-maxDescriptorSetInlineUniformBlocks#
    -- @maxDescriptorSetInlineUniformBlocks@ is the maximum number of inline
    -- uniform block bindings that /can/ be included in descriptor bindings in
    -- a pipeline layout across all pipeline shader stages and descriptor set
    -- numbers. Descriptor bindings with a descriptor type of
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
    -- count against this limit. Only descriptor bindings in descriptor set
    -- layouts created without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set count against this limit.
    maxDescriptorSetInlineUniformBlocks :: Word32
  , -- | #limits-maxDescriptorSetUpdateAfterBindInlineUniformBlocks#
    -- @maxDescriptorSetUpdateAfterBindInlineUniformBlocks@ is similar to
    -- @maxDescriptorSetInlineUniformBlocks@ but counts descriptor bindings
    -- from descriptor sets created with or without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxDescriptorSetUpdateAfterBindInlineUniformBlocks :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceInlineUniformBlockPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceInlineUniformBlockPropertiesEXT

instance ToCStruct PhysicalDeviceInlineUniformBlockPropertiesEXT where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceInlineUniformBlockPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxInlineUniformBlockSize)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxPerStageDescriptorInlineUniformBlocks)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (maxPerStageDescriptorUpdateAfterBindInlineUniformBlocks)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (maxDescriptorSetInlineUniformBlocks)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (maxDescriptorSetUpdateAfterBindInlineUniformBlocks)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceInlineUniformBlockPropertiesEXT where
  peekCStruct p = do
    maxInlineUniformBlockSize <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxPerStageDescriptorInlineUniformBlocks <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    maxPerStageDescriptorUpdateAfterBindInlineUniformBlocks <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    maxDescriptorSetInlineUniformBlocks <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    maxDescriptorSetUpdateAfterBindInlineUniformBlocks <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pure $ PhysicalDeviceInlineUniformBlockPropertiesEXT
             maxInlineUniformBlockSize maxPerStageDescriptorInlineUniformBlocks maxPerStageDescriptorUpdateAfterBindInlineUniformBlocks maxDescriptorSetInlineUniformBlocks maxDescriptorSetUpdateAfterBindInlineUniformBlocks

instance Storable PhysicalDeviceInlineUniformBlockPropertiesEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceInlineUniformBlockPropertiesEXT where
  zero = PhysicalDeviceInlineUniformBlockPropertiesEXT
           zero
           zero
           zero
           zero
           zero


-- | VkWriteDescriptorSetInlineUniformBlockEXT - Structure specifying inline
-- uniform block data
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_inline_uniform_block VK_EXT_inline_uniform_block>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data WriteDescriptorSetInlineUniformBlockEXT = WriteDescriptorSetInlineUniformBlockEXT
  { -- | @dataSize@ is the number of bytes of inline uniform block data pointed
    -- to by @pData@.
    --
    -- #VUID-VkWriteDescriptorSetInlineUniformBlockEXT-dataSize-02222#
    -- @dataSize@ /must/ be an integer multiple of @4@
    --
    -- #VUID-VkWriteDescriptorSetInlineUniformBlockEXT-dataSize-arraylength#
    -- @dataSize@ /must/ be greater than @0@
    dataSize :: Word32
  , -- | @pData@ is a pointer to @dataSize@ number of bytes of data to write to
    -- the inline uniform block.
    --
    -- #VUID-VkWriteDescriptorSetInlineUniformBlockEXT-pData-parameter# @pData@
    -- /must/ be a valid pointer to an array of @dataSize@ bytes
    data' :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (WriteDescriptorSetInlineUniformBlockEXT)
#endif
deriving instance Show WriteDescriptorSetInlineUniformBlockEXT

instance ToCStruct WriteDescriptorSetInlineUniformBlockEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p WriteDescriptorSetInlineUniformBlockEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (dataSize)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (data')
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (zero)
    f

instance FromCStruct WriteDescriptorSetInlineUniformBlockEXT where
  peekCStruct p = do
    dataSize <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pData <- peek @(Ptr ()) ((p `plusPtr` 24 :: Ptr (Ptr ())))
    pure $ WriteDescriptorSetInlineUniformBlockEXT
             dataSize pData

instance Storable WriteDescriptorSetInlineUniformBlockEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero WriteDescriptorSetInlineUniformBlockEXT where
  zero = WriteDescriptorSetInlineUniformBlockEXT
           zero
           zero


-- | VkDescriptorPoolInlineUniformBlockCreateInfoEXT - Structure specifying
-- the maximum number of inline uniform block bindings of a newly created
-- descriptor pool
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_inline_uniform_block VK_EXT_inline_uniform_block>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DescriptorPoolInlineUniformBlockCreateInfoEXT = DescriptorPoolInlineUniformBlockCreateInfoEXT
  { -- | @maxInlineUniformBlockBindings@ is the number of inline uniform block
    -- bindings to allocate.
    maxInlineUniformBlockBindings :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DescriptorPoolInlineUniformBlockCreateInfoEXT)
#endif
deriving instance Show DescriptorPoolInlineUniformBlockCreateInfoEXT

instance ToCStruct DescriptorPoolInlineUniformBlockCreateInfoEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorPoolInlineUniformBlockCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxInlineUniformBlockBindings)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct DescriptorPoolInlineUniformBlockCreateInfoEXT where
  peekCStruct p = do
    maxInlineUniformBlockBindings <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ DescriptorPoolInlineUniformBlockCreateInfoEXT
             maxInlineUniformBlockBindings

instance Storable DescriptorPoolInlineUniformBlockCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DescriptorPoolInlineUniformBlockCreateInfoEXT where
  zero = DescriptorPoolInlineUniformBlockCreateInfoEXT
           zero


type EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION"
pattern EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION = 1


type EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME = "VK_EXT_inline_uniform_block"

-- No documentation found for TopLevel "VK_EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME"
pattern EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME = "VK_EXT_inline_uniform_block"

