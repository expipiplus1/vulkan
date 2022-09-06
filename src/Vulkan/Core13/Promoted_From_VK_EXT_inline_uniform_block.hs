{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_EXT_inline_uniform_block"
module Vulkan.Core13.Promoted_From_VK_EXT_inline_uniform_block  ( PhysicalDeviceInlineUniformBlockFeatures(..)
                                                                , PhysicalDeviceInlineUniformBlockProperties(..)
                                                                , WriteDescriptorSetInlineUniformBlock(..)
                                                                , DescriptorPoolInlineUniformBlockCreateInfo(..)
                                                                , DescriptorType(..)
                                                                , StructureType(..)
                                                                ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK))
import Vulkan.Core10.Enums.DescriptorType (DescriptorType(..))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
-- | VkPhysicalDeviceInlineUniformBlockFeatures - Structure describing inline
-- uniform block features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceInlineUniformBlockFeatures' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceInlineUniformBlockFeatures' /can/ also be used
-- in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_inline_uniform_block VK_EXT_inline_uniform_block>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceInlineUniformBlockFeatures = PhysicalDeviceInlineUniformBlockFeatures
  { -- | #extension-features-inlineUniformBlock# @inlineUniformBlock@ indicates
    -- whether the implementation supports inline uniform block descriptors. If
    -- this feature is not enabled,
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK'
    -- /must/ not be used.
    inlineUniformBlock :: Bool
  , -- | #extension-features-descriptorBindingInlineUniformBlockUpdateAfterBind#
    -- @descriptorBindingInlineUniformBlockUpdateAfterBind@ indicates whether
    -- the implementation supports updating inline uniform block descriptors
    -- after a set is bound. If this feature is not enabled,
    -- 'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'
    -- /must/ not be used with
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK'.
    descriptorBindingInlineUniformBlockUpdateAfterBind :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceInlineUniformBlockFeatures)
#endif
deriving instance Show PhysicalDeviceInlineUniformBlockFeatures

instance ToCStruct PhysicalDeviceInlineUniformBlockFeatures where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceInlineUniformBlockFeatures{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (inlineUniformBlock))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (descriptorBindingInlineUniformBlockUpdateAfterBind))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceInlineUniformBlockFeatures where
  peekCStruct p = do
    inlineUniformBlock <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    descriptorBindingInlineUniformBlockUpdateAfterBind <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceInlineUniformBlockFeatures
             (bool32ToBool inlineUniformBlock) (bool32ToBool descriptorBindingInlineUniformBlockUpdateAfterBind)

instance Storable PhysicalDeviceInlineUniformBlockFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceInlineUniformBlockFeatures where
  zero = PhysicalDeviceInlineUniformBlockFeatures
           zero
           zero


-- | VkPhysicalDeviceInlineUniformBlockProperties - Structure describing
-- inline uniform block properties that can be supported by an
-- implementation
--
-- = Description
--
-- If the 'PhysicalDeviceInlineUniformBlockProperties' structure is
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceInlineUniformBlockProperties = PhysicalDeviceInlineUniformBlockProperties
  { -- | #extension-limits-maxInlineUniformBlockSize# @maxInlineUniformBlockSize@
    -- is the maximum size in bytes of an
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-inlineuniformblock inline uniform block>
    -- binding.
    maxInlineUniformBlockSize :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceInlineUniformBlockProperties" "maxPerStageDescriptorInlineUniformBlocks"
    maxPerStageDescriptorInlineUniformBlocks :: Word32
  , -- | #extension-limits-maxPerStageDescriptorUpdateAfterBindInlineUniformBlocks#
    -- @maxPerStageDescriptorUpdateAfterBindInlineUniformBlocks@ is similar to
    -- @maxPerStageDescriptorInlineUniformBlocks@ but counts descriptor
    -- bindings from descriptor sets created with or without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxPerStageDescriptorUpdateAfterBindInlineUniformBlocks :: Word32
  , -- | #extension-limits-maxDescriptorSetInlineUniformBlocks#
    -- @maxDescriptorSetInlineUniformBlocks@ is the maximum number of inline
    -- uniform block bindings that /can/ be included in descriptor bindings in
    -- a pipeline layout across all pipeline shader stages and descriptor set
    -- numbers. Descriptor bindings with a descriptor type of
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK'
    -- count against this limit. Only descriptor bindings in descriptor set
    -- layouts created without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set count against this limit.
    maxDescriptorSetInlineUniformBlocks :: Word32
  , -- | #extension-limits-maxDescriptorSetUpdateAfterBindInlineUniformBlocks#
    -- @maxDescriptorSetUpdateAfterBindInlineUniformBlocks@ is similar to
    -- @maxDescriptorSetInlineUniformBlocks@ but counts descriptor bindings
    -- from descriptor sets created with or without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxDescriptorSetUpdateAfterBindInlineUniformBlocks :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceInlineUniformBlockProperties)
#endif
deriving instance Show PhysicalDeviceInlineUniformBlockProperties

instance ToCStruct PhysicalDeviceInlineUniformBlockProperties where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceInlineUniformBlockProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES)
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
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceInlineUniformBlockProperties where
  peekCStruct p = do
    maxInlineUniformBlockSize <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxPerStageDescriptorInlineUniformBlocks <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    maxPerStageDescriptorUpdateAfterBindInlineUniformBlocks <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    maxDescriptorSetInlineUniformBlocks <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    maxDescriptorSetUpdateAfterBindInlineUniformBlocks <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pure $ PhysicalDeviceInlineUniformBlockProperties
             maxInlineUniformBlockSize maxPerStageDescriptorInlineUniformBlocks maxPerStageDescriptorUpdateAfterBindInlineUniformBlocks maxDescriptorSetInlineUniformBlocks maxDescriptorSetUpdateAfterBindInlineUniformBlocks

instance Storable PhysicalDeviceInlineUniformBlockProperties where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceInlineUniformBlockProperties where
  zero = PhysicalDeviceInlineUniformBlockProperties
           zero
           zero
           zero
           zero
           zero


-- | VkWriteDescriptorSetInlineUniformBlock - Structure specifying inline
-- uniform block data
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_inline_uniform_block VK_EXT_inline_uniform_block>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data WriteDescriptorSetInlineUniformBlock = WriteDescriptorSetInlineUniformBlock
  { -- | @dataSize@ is the number of bytes of inline uniform block data pointed
    -- to by @pData@.
    --
    -- #VUID-VkWriteDescriptorSetInlineUniformBlock-dataSize-02222# @dataSize@
    -- /must/ be an integer multiple of @4@
    --
    -- #VUID-VkWriteDescriptorSetInlineUniformBlock-dataSize-arraylength#
    -- @dataSize@ /must/ be greater than @0@
    dataSize :: Word32
  , -- | @pData@ is a pointer to @dataSize@ number of bytes of data to write to
    -- the inline uniform block.
    --
    -- #VUID-VkWriteDescriptorSetInlineUniformBlock-pData-parameter# @pData@
    -- /must/ be a valid pointer to an array of @dataSize@ bytes
    data' :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (WriteDescriptorSetInlineUniformBlock)
#endif
deriving instance Show WriteDescriptorSetInlineUniformBlock

instance ToCStruct WriteDescriptorSetInlineUniformBlock where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p WriteDescriptorSetInlineUniformBlock{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (dataSize)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (data')
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (zero)
    f

instance FromCStruct WriteDescriptorSetInlineUniformBlock where
  peekCStruct p = do
    dataSize <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pData <- peek @(Ptr ()) ((p `plusPtr` 24 :: Ptr (Ptr ())))
    pure $ WriteDescriptorSetInlineUniformBlock
             dataSize pData

instance Storable WriteDescriptorSetInlineUniformBlock where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero WriteDescriptorSetInlineUniformBlock where
  zero = WriteDescriptorSetInlineUniformBlock
           zero
           zero


-- | VkDescriptorPoolInlineUniformBlockCreateInfo - Structure specifying the
-- maximum number of inline uniform block bindings of a newly created
-- descriptor pool
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_inline_uniform_block VK_EXT_inline_uniform_block>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DescriptorPoolInlineUniformBlockCreateInfo = DescriptorPoolInlineUniformBlockCreateInfo
  { -- | @maxInlineUniformBlockBindings@ is the number of inline uniform block
    -- bindings to allocate.
    maxInlineUniformBlockBindings :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DescriptorPoolInlineUniformBlockCreateInfo)
#endif
deriving instance Show DescriptorPoolInlineUniformBlockCreateInfo

instance ToCStruct DescriptorPoolInlineUniformBlockCreateInfo where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorPoolInlineUniformBlockCreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxInlineUniformBlockBindings)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct DescriptorPoolInlineUniformBlockCreateInfo where
  peekCStruct p = do
    maxInlineUniformBlockBindings <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ DescriptorPoolInlineUniformBlockCreateInfo
             maxInlineUniformBlockBindings

instance Storable DescriptorPoolInlineUniformBlockCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DescriptorPoolInlineUniformBlockCreateInfo where
  zero = DescriptorPoolInlineUniformBlockCreateInfo
           zero

