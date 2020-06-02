{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_inline_uniform_block  ( PhysicalDeviceInlineUniformBlockFeaturesEXT(..)
                                                      , PhysicalDeviceInlineUniformBlockPropertiesEXT(..)
                                                      , WriteDescriptorSetInlineUniformBlockEXT(..)
                                                      , DescriptorPoolInlineUniformBlockCreateInfoEXT(..)
                                                      , EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION
                                                      , pattern EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION
                                                      , EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME
                                                      , pattern EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME
                                                      ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
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
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT))
-- | VkPhysicalDeviceInlineUniformBlockFeaturesEXT - Structure describing
-- inline uniform block features that can be supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceInlineUniformBlockFeaturesEXT'
-- structure describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceInlineUniformBlockFeaturesEXT' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether each feature is supported.
-- 'PhysicalDeviceInlineUniformBlockFeaturesEXT' /can/ also be included in
-- the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to enable
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceInlineUniformBlockFeaturesEXT = PhysicalDeviceInlineUniformBlockFeaturesEXT
  { -- | @inlineUniformBlock@ indicates whether the implementation supports
    -- inline uniform block descriptors. If this feature is not enabled,
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
    -- /must/ not be used.
    inlineUniformBlock :: Bool
  , -- | @descriptorBindingInlineUniformBlockUpdateAfterBind@ indicates whether
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
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
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
-- = Members
--
-- The members of the 'PhysicalDeviceInlineUniformBlockPropertiesEXT'
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- If the 'PhysicalDeviceInlineUniformBlockPropertiesEXT' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceInlineUniformBlockPropertiesEXT = PhysicalDeviceInlineUniformBlockPropertiesEXT
  { -- | @maxInlineUniformBlockSize@ is the maximum size in bytes of an
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-inlineuniformblock inline uniform block>
    -- binding.
    maxInlineUniformBlockSize :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceInlineUniformBlockPropertiesEXT" "maxPerStageDescriptorInlineUniformBlocks"
    maxPerStageDescriptorInlineUniformBlocks :: Word32
  , -- | @maxPerStageDescriptorUpdateAfterBindInlineUniformBlocks@ is similar to
    -- @maxPerStageDescriptorInlineUniformBlocks@ but counts descriptor
    -- bindings from descriptor sets created with or without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxPerStageDescriptorUpdateAfterBindInlineUniformBlocks :: Word32
  , -- | @maxDescriptorSetInlineUniformBlocks@ is the maximum number of inline
    -- uniform block bindings that /can/ be included in descriptor bindings in
    -- a pipeline layout across all pipeline shader stages and descriptor set
    -- numbers. Descriptor bindings with a descriptor type of
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
    -- count against this limit. Only descriptor bindings in descriptor set
    -- layouts created without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set count against this limit.
    maxDescriptorSetInlineUniformBlocks :: Word32
  , -- | @maxDescriptorSetUpdateAfterBindInlineUniformBlocks@ is similar to
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
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
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
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data WriteDescriptorSetInlineUniformBlockEXT = WriteDescriptorSetInlineUniformBlockEXT
  { -- | @dataSize@ is the number of bytes of inline uniform block data pointed
    -- to by @pData@.
    --
    -- @dataSize@ /must/ be an integer multiple of @4@
    --
    -- @dataSize@ /must/ be greater than @0@
    dataSize :: Word32
  , -- | @pData@ is a pointer to @dataSize@ number of bytes of data to write to
    -- the inline uniform block.
    --
    -- @pData@ /must/ be a valid pointer to an array of @dataSize@ bytes
    data' :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (WriteDescriptorSetInlineUniformBlockEXT)
#endif
deriving instance Show WriteDescriptorSetInlineUniformBlockEXT

instance ToCStruct WriteDescriptorSetInlineUniformBlockEXT where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
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
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
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

