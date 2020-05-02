{-# language CPP #-}
module Graphics.Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing  ( PhysicalDeviceDescriptorIndexingFeatures(..)
                                                                        , PhysicalDeviceDescriptorIndexingProperties(..)
                                                                        , DescriptorSetLayoutBindingFlagsCreateInfo(..)
                                                                        , DescriptorSetVariableDescriptorCountAllocateInfo(..)
                                                                        , DescriptorSetVariableDescriptorCountLayoutSupport(..)
                                                                        , StructureType(..)
                                                                        , Result(..)
                                                                        , DescriptorPoolCreateFlagBits(..)
                                                                        , DescriptorPoolCreateFlags
                                                                        , DescriptorSetLayoutCreateFlagBits(..)
                                                                        , DescriptorSetLayoutCreateFlags
                                                                        , DescriptorBindingFlagBits(..)
                                                                        , DescriptorBindingFlags
                                                                        ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Utils (maybePeek)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Data.Either (Either)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Graphics.Vulkan.CStruct.Utils (advancePtrBytes)
import Graphics.Vulkan.Core10.BaseType (bool32ToBool)
import Graphics.Vulkan.Core10.BaseType (boolToBool32)
import Graphics.Vulkan.Core10.BaseType (Bool32)
import Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits (DescriptorBindingFlags)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES))
import Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits (DescriptorBindingFlagBits(..))
import Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits (DescriptorBindingFlags)
import Graphics.Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits (DescriptorPoolCreateFlagBits(..))
import Graphics.Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits (DescriptorPoolCreateFlags)
import Graphics.Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits (DescriptorSetLayoutCreateFlagBits(..))
import Graphics.Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits (DescriptorSetLayoutCreateFlags)
import Graphics.Vulkan.Core10.Enums.Result (Result(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(..))
-- | VkPhysicalDeviceDescriptorIndexingFeatures - Structure describing
-- descriptor indexing features that can be supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceDescriptorIndexingFeatures' structure
-- describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceDescriptorIndexingFeatures' structure is included
-- in the @pNext@ chain of
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether each feature is supported.
-- 'PhysicalDeviceDescriptorIndexingFeatures' /can/ also be included in the
-- @pNext@ chain of 'Graphics.Vulkan.Core10.Device.DeviceCreateInfo' to
-- enable features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDescriptorIndexingFeatures = PhysicalDeviceDescriptorIndexingFeatures
  { -- | @shaderInputAttachmentArrayDynamicIndexing@ indicates whether arrays of
    -- input attachments /can/ be indexed by dynamically uniform integer
    -- expressions in shader code. If this feature is not enabled, resources
    -- with a descriptor type of
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INPUT_ATTACHMENT'
    -- /must/ be indexed only by constant integral expressions when aggregated
    -- into arrays in shader code. This also indicates whether shader modules
    -- /can/ declare the @InputAttachmentArrayDynamicIndexing@ capability.
    shaderInputAttachmentArrayDynamicIndexing :: Bool
  , -- | @shaderUniformTexelBufferArrayDynamicIndexing@ indicates whether arrays
    -- of uniform texel buffers /can/ be indexed by dynamically uniform integer
    -- expressions in shader code. If this feature is not enabled, resources
    -- with a descriptor type of
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
    -- /must/ be indexed only by constant integral expressions when aggregated
    -- into arrays in shader code. This also indicates whether shader modules
    -- /can/ declare the @UniformTexelBufferArrayDynamicIndexing@ capability.
    shaderUniformTexelBufferArrayDynamicIndexing :: Bool
  , -- | @shaderStorageTexelBufferArrayDynamicIndexing@ indicates whether arrays
    -- of storage texel buffers /can/ be indexed by dynamically uniform integer
    -- expressions in shader code. If this feature is not enabled, resources
    -- with a descriptor type of
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
    -- /must/ be indexed only by constant integral expressions when aggregated
    -- into arrays in shader code. This also indicates whether shader modules
    -- /can/ declare the @StorageTexelBufferArrayDynamicIndexing@ capability.
    shaderStorageTexelBufferArrayDynamicIndexing :: Bool
  , -- | @shaderUniformBufferArrayNonUniformIndexing@ indicates whether arrays of
    -- uniform buffers /can/ be indexed by non-uniform integer expressions in
    -- shader code. If this feature is not enabled, resources with a descriptor
    -- type of
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER'
    -- or
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
    -- /must/ not be indexed by non-uniform integer expressions when aggregated
    -- into arrays in shader code. This also indicates whether shader modules
    -- /can/ declare the @UniformBufferArrayNonUniformIndexing@ capability.
    shaderUniformBufferArrayNonUniformIndexing :: Bool
  , -- | @shaderSampledImageArrayNonUniformIndexing@ indicates whether arrays of
    -- samplers or sampled images /can/ be indexed by non-uniform integer
    -- expressions in shader code. If this feature is not enabled, resources
    -- with a descriptor type of
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLER',
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
    -- or
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE'
    -- /must/ not be indexed by non-uniform integer expressions when aggregated
    -- into arrays in shader code. This also indicates whether shader modules
    -- /can/ declare the @SampledImageArrayNonUniformIndexing@ capability.
    shaderSampledImageArrayNonUniformIndexing :: Bool
  , -- | @shaderStorageBufferArrayNonUniformIndexing@ indicates whether arrays of
    -- storage buffers /can/ be indexed by non-uniform integer expressions in
    -- shader code. If this feature is not enabled, resources with a descriptor
    -- type of
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER'
    -- or
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
    -- /must/ not be indexed by non-uniform integer expressions when aggregated
    -- into arrays in shader code. This also indicates whether shader modules
    -- /can/ declare the @StorageBufferArrayNonUniformIndexing@ capability.
    shaderStorageBufferArrayNonUniformIndexing :: Bool
  , -- | @shaderStorageImageArrayNonUniformIndexing@ indicates whether arrays of
    -- storage images /can/ be indexed by non-uniform integer expressions in
    -- shader code. If this feature is not enabled, resources with a descriptor
    -- type of
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE'
    -- /must/ not be indexed by non-uniform integer expressions when aggregated
    -- into arrays in shader code. This also indicates whether shader modules
    -- /can/ declare the @StorageImageArrayNonUniformIndexing@ capability.
    shaderStorageImageArrayNonUniformIndexing :: Bool
  , -- | @shaderInputAttachmentArrayNonUniformIndexing@ indicates whether arrays
    -- of input attachments /can/ be indexed by non-uniform integer expressions
    -- in shader code. If this feature is not enabled, resources with a
    -- descriptor type of
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INPUT_ATTACHMENT'
    -- /must/ not be indexed by non-uniform integer expressions when aggregated
    -- into arrays in shader code. This also indicates whether shader modules
    -- /can/ declare the @InputAttachmentArrayNonUniformIndexing@ capability.
    shaderInputAttachmentArrayNonUniformIndexing :: Bool
  , -- | @shaderUniformTexelBufferArrayNonUniformIndexing@ indicates whether
    -- arrays of uniform texel buffers /can/ be indexed by non-uniform integer
    -- expressions in shader code. If this feature is not enabled, resources
    -- with a descriptor type of
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
    -- /must/ not be indexed by non-uniform integer expressions when aggregated
    -- into arrays in shader code. This also indicates whether shader modules
    -- /can/ declare the @UniformTexelBufferArrayNonUniformIndexing@
    -- capability.
    shaderUniformTexelBufferArrayNonUniformIndexing :: Bool
  , -- | @shaderStorageTexelBufferArrayNonUniformIndexing@ indicates whether
    -- arrays of storage texel buffers /can/ be indexed by non-uniform integer
    -- expressions in shader code. If this feature is not enabled, resources
    -- with a descriptor type of
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
    -- /must/ not be indexed by non-uniform integer expressions when aggregated
    -- into arrays in shader code. This also indicates whether shader modules
    -- /can/ declare the @StorageTexelBufferArrayNonUniformIndexing@
    -- capability.
    shaderStorageTexelBufferArrayNonUniformIndexing :: Bool
  , -- | @descriptorBindingUniformBufferUpdateAfterBind@ indicates whether the
    -- implementation supports updating uniform buffer descriptors after a set
    -- is bound. If this feature is not enabled,
    -- 'Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'
    -- /must/ not be used with
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER'.
    descriptorBindingUniformBufferUpdateAfterBind :: Bool
  , -- | @descriptorBindingSampledImageUpdateAfterBind@ indicates whether the
    -- implementation supports updating sampled image descriptors after a set
    -- is bound. If this feature is not enabled,
    -- 'Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'
    -- /must/ not be used with
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLER',
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
    -- or
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE'.
    descriptorBindingSampledImageUpdateAfterBind :: Bool
  , -- | @descriptorBindingStorageImageUpdateAfterBind@ indicates whether the
    -- implementation supports updating storage image descriptors after a set
    -- is bound. If this feature is not enabled,
    -- 'Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'
    -- /must/ not be used with
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE'.
    descriptorBindingStorageImageUpdateAfterBind :: Bool
  , -- | @descriptorBindingStorageBufferUpdateAfterBind@ indicates whether the
    -- implementation supports updating storage buffer descriptors after a set
    -- is bound. If this feature is not enabled,
    -- 'Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'
    -- /must/ not be used with
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER'.
    descriptorBindingStorageBufferUpdateAfterBind :: Bool
  , -- | @descriptorBindingUniformTexelBufferUpdateAfterBind@ indicates whether
    -- the implementation supports updating uniform texel buffer descriptors
    -- after a set is bound. If this feature is not enabled,
    -- 'Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'
    -- /must/ not be used with
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'.
    descriptorBindingUniformTexelBufferUpdateAfterBind :: Bool
  , -- | @descriptorBindingStorageTexelBufferUpdateAfterBind@ indicates whether
    -- the implementation supports updating storage texel buffer descriptors
    -- after a set is bound. If this feature is not enabled,
    -- 'Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'
    -- /must/ not be used with
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'.
    descriptorBindingStorageTexelBufferUpdateAfterBind :: Bool
  , -- | @descriptorBindingUpdateUnusedWhilePending@ indicates whether the
    -- implementation supports updating descriptors while the set is in use. If
    -- this feature is not enabled,
    -- 'Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT'
    -- /must/ not be used.
    descriptorBindingUpdateUnusedWhilePending :: Bool
  , -- | @descriptorBindingPartiallyBound@ indicates whether the implementation
    -- supports statically using a descriptor set binding in which some
    -- descriptors are not valid. If this feature is not enabled,
    -- 'Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT'
    -- /must/ not be used.
    descriptorBindingPartiallyBound :: Bool
  , -- | @descriptorBindingVariableDescriptorCount@ indicates whether the
    -- implementation supports descriptor sets with a variable-sized last
    -- binding. If this feature is not enabled,
    -- 'Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT'
    -- /must/ not be used.
    descriptorBindingVariableDescriptorCount :: Bool
  , -- | @runtimeDescriptorArray@ indicates whether the implementation supports
    -- the SPIR-V @RuntimeDescriptorArray@ capability. If this feature is not
    -- enabled, descriptors /must/ not be declared in runtime arrays.
    runtimeDescriptorArray :: Bool
  }
  deriving (Typeable)
deriving instance Show PhysicalDeviceDescriptorIndexingFeatures

instance ToCStruct PhysicalDeviceDescriptorIndexingFeatures where
  withCStruct x f = allocaBytesAligned 96 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDescriptorIndexingFeatures{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderInputAttachmentArrayDynamicIndexing))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (shaderUniformTexelBufferArrayDynamicIndexing))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (shaderStorageTexelBufferArrayDynamicIndexing))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (shaderUniformBufferArrayNonUniformIndexing))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (shaderSampledImageArrayNonUniformIndexing))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (shaderStorageBufferArrayNonUniformIndexing))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (shaderStorageImageArrayNonUniformIndexing))
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (shaderInputAttachmentArrayNonUniformIndexing))
    poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (shaderUniformTexelBufferArrayNonUniformIndexing))
    poke ((p `plusPtr` 52 :: Ptr Bool32)) (boolToBool32 (shaderStorageTexelBufferArrayNonUniformIndexing))
    poke ((p `plusPtr` 56 :: Ptr Bool32)) (boolToBool32 (descriptorBindingUniformBufferUpdateAfterBind))
    poke ((p `plusPtr` 60 :: Ptr Bool32)) (boolToBool32 (descriptorBindingSampledImageUpdateAfterBind))
    poke ((p `plusPtr` 64 :: Ptr Bool32)) (boolToBool32 (descriptorBindingStorageImageUpdateAfterBind))
    poke ((p `plusPtr` 68 :: Ptr Bool32)) (boolToBool32 (descriptorBindingStorageBufferUpdateAfterBind))
    poke ((p `plusPtr` 72 :: Ptr Bool32)) (boolToBool32 (descriptorBindingUniformTexelBufferUpdateAfterBind))
    poke ((p `plusPtr` 76 :: Ptr Bool32)) (boolToBool32 (descriptorBindingStorageTexelBufferUpdateAfterBind))
    poke ((p `plusPtr` 80 :: Ptr Bool32)) (boolToBool32 (descriptorBindingUpdateUnusedWhilePending))
    poke ((p `plusPtr` 84 :: Ptr Bool32)) (boolToBool32 (descriptorBindingPartiallyBound))
    poke ((p `plusPtr` 88 :: Ptr Bool32)) (boolToBool32 (descriptorBindingVariableDescriptorCount))
    poke ((p `plusPtr` 92 :: Ptr Bool32)) (boolToBool32 (runtimeDescriptorArray))
    f
  cStructSize = 96
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 52 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 56 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 60 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 64 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 68 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 72 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 76 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 80 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 84 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 88 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 92 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceDescriptorIndexingFeatures where
  peekCStruct p = do
    shaderInputAttachmentArrayDynamicIndexing <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    shaderUniformTexelBufferArrayDynamicIndexing <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    shaderStorageTexelBufferArrayDynamicIndexing <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    shaderUniformBufferArrayNonUniformIndexing <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    shaderSampledImageArrayNonUniformIndexing <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    shaderStorageBufferArrayNonUniformIndexing <- peek @Bool32 ((p `plusPtr` 36 :: Ptr Bool32))
    shaderStorageImageArrayNonUniformIndexing <- peek @Bool32 ((p `plusPtr` 40 :: Ptr Bool32))
    shaderInputAttachmentArrayNonUniformIndexing <- peek @Bool32 ((p `plusPtr` 44 :: Ptr Bool32))
    shaderUniformTexelBufferArrayNonUniformIndexing <- peek @Bool32 ((p `plusPtr` 48 :: Ptr Bool32))
    shaderStorageTexelBufferArrayNonUniformIndexing <- peek @Bool32 ((p `plusPtr` 52 :: Ptr Bool32))
    descriptorBindingUniformBufferUpdateAfterBind <- peek @Bool32 ((p `plusPtr` 56 :: Ptr Bool32))
    descriptorBindingSampledImageUpdateAfterBind <- peek @Bool32 ((p `plusPtr` 60 :: Ptr Bool32))
    descriptorBindingStorageImageUpdateAfterBind <- peek @Bool32 ((p `plusPtr` 64 :: Ptr Bool32))
    descriptorBindingStorageBufferUpdateAfterBind <- peek @Bool32 ((p `plusPtr` 68 :: Ptr Bool32))
    descriptorBindingUniformTexelBufferUpdateAfterBind <- peek @Bool32 ((p `plusPtr` 72 :: Ptr Bool32))
    descriptorBindingStorageTexelBufferUpdateAfterBind <- peek @Bool32 ((p `plusPtr` 76 :: Ptr Bool32))
    descriptorBindingUpdateUnusedWhilePending <- peek @Bool32 ((p `plusPtr` 80 :: Ptr Bool32))
    descriptorBindingPartiallyBound <- peek @Bool32 ((p `plusPtr` 84 :: Ptr Bool32))
    descriptorBindingVariableDescriptorCount <- peek @Bool32 ((p `plusPtr` 88 :: Ptr Bool32))
    runtimeDescriptorArray <- peek @Bool32 ((p `plusPtr` 92 :: Ptr Bool32))
    pure $ PhysicalDeviceDescriptorIndexingFeatures
             (bool32ToBool shaderInputAttachmentArrayDynamicIndexing) (bool32ToBool shaderUniformTexelBufferArrayDynamicIndexing) (bool32ToBool shaderStorageTexelBufferArrayDynamicIndexing) (bool32ToBool shaderUniformBufferArrayNonUniformIndexing) (bool32ToBool shaderSampledImageArrayNonUniformIndexing) (bool32ToBool shaderStorageBufferArrayNonUniformIndexing) (bool32ToBool shaderStorageImageArrayNonUniformIndexing) (bool32ToBool shaderInputAttachmentArrayNonUniformIndexing) (bool32ToBool shaderUniformTexelBufferArrayNonUniformIndexing) (bool32ToBool shaderStorageTexelBufferArrayNonUniformIndexing) (bool32ToBool descriptorBindingUniformBufferUpdateAfterBind) (bool32ToBool descriptorBindingSampledImageUpdateAfterBind) (bool32ToBool descriptorBindingStorageImageUpdateAfterBind) (bool32ToBool descriptorBindingStorageBufferUpdateAfterBind) (bool32ToBool descriptorBindingUniformTexelBufferUpdateAfterBind) (bool32ToBool descriptorBindingStorageTexelBufferUpdateAfterBind) (bool32ToBool descriptorBindingUpdateUnusedWhilePending) (bool32ToBool descriptorBindingPartiallyBound) (bool32ToBool descriptorBindingVariableDescriptorCount) (bool32ToBool runtimeDescriptorArray)

instance Storable PhysicalDeviceDescriptorIndexingFeatures where
  sizeOf ~_ = 96
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDescriptorIndexingFeatures where
  zero = PhysicalDeviceDescriptorIndexingFeatures
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkPhysicalDeviceDescriptorIndexingProperties - Structure describing
-- descriptor indexing properties that can be supported by an
-- implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceDescriptorIndexingProperties'
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- If the 'PhysicalDeviceDescriptorIndexingProperties' structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDescriptorIndexingProperties = PhysicalDeviceDescriptorIndexingProperties
  { -- | @maxUpdateAfterBindDescriptorsInAllPools@ is the maximum number of
    -- descriptors (summed over all descriptor types) that /can/ be created
    -- across all pools that are created with the
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT'
    -- bit set. Pool creation /may/ fail when this limit is exceeded, or when
    -- the space this limit represents is unable to satisfy a pool creation due
    -- to fragmentation.
    maxUpdateAfterBindDescriptorsInAllPools :: Word32
  , -- | @shaderUniformBufferArrayNonUniformIndexingNative@ is a boolean value
    -- indicating whether uniform buffer descriptors natively support
    -- nonuniform indexing. If this is 'Graphics.Vulkan.Core10.BaseType.FALSE',
    -- then a single dynamic instance of an instruction that nonuniformly
    -- indexes an array of uniform buffers /may/ execute multiple times in
    -- order to access all the descriptors.
    shaderUniformBufferArrayNonUniformIndexingNative :: Bool
  , -- | @shaderSampledImageArrayNonUniformIndexingNative@ is a boolean value
    -- indicating whether sampler and image descriptors natively support
    -- nonuniform indexing. If this is 'Graphics.Vulkan.Core10.BaseType.FALSE',
    -- then a single dynamic instance of an instruction that nonuniformly
    -- indexes an array of samplers or images /may/ execute multiple times in
    -- order to access all the descriptors.
    shaderSampledImageArrayNonUniformIndexingNative :: Bool
  , -- | @shaderStorageBufferArrayNonUniformIndexingNative@ is a boolean value
    -- indicating whether storage buffer descriptors natively support
    -- nonuniform indexing. If this is 'Graphics.Vulkan.Core10.BaseType.FALSE',
    -- then a single dynamic instance of an instruction that nonuniformly
    -- indexes an array of storage buffers /may/ execute multiple times in
    -- order to access all the descriptors.
    shaderStorageBufferArrayNonUniformIndexingNative :: Bool
  , -- | @shaderStorageImageArrayNonUniformIndexingNative@ is a boolean value
    -- indicating whether storage image descriptors natively support nonuniform
    -- indexing. If this is 'Graphics.Vulkan.Core10.BaseType.FALSE', then a
    -- single dynamic instance of an instruction that nonuniformly indexes an
    -- array of storage images /may/ execute multiple times in order to access
    -- all the descriptors.
    shaderStorageImageArrayNonUniformIndexingNative :: Bool
  , -- | @shaderInputAttachmentArrayNonUniformIndexingNative@ is a boolean value
    -- indicating whether input attachment descriptors natively support
    -- nonuniform indexing. If this is 'Graphics.Vulkan.Core10.BaseType.FALSE',
    -- then a single dynamic instance of an instruction that nonuniformly
    -- indexes an array of input attachments /may/ execute multiple times in
    -- order to access all the descriptors.
    shaderInputAttachmentArrayNonUniformIndexingNative :: Bool
  , -- | @robustBufferAccessUpdateAfterBind@ is a boolean value indicating
    -- whether
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
    -- /can/ be enabled in a device simultaneously with
    -- @descriptorBindingUniformBufferUpdateAfterBind@,
    -- @descriptorBindingStorageBufferUpdateAfterBind@,
    -- @descriptorBindingUniformTexelBufferUpdateAfterBind@, and\/or
    -- @descriptorBindingStorageTexelBufferUpdateAfterBind@. If this is
    -- 'Graphics.Vulkan.Core10.BaseType.FALSE', then either
    -- @robustBufferAccess@ /must/ be disabled or all of these
    -- update-after-bind features /must/ be disabled.
    robustBufferAccessUpdateAfterBind :: Bool
  , -- | @quadDivergentImplicitLod@ is a boolean value indicating whether
    -- implicit level of detail calculations for image operations have
    -- well-defined results when the image and\/or sampler objects used for the
    -- instruction are not uniform within a quad. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-derivative-image-operations Derivative Image Operations>.
    quadDivergentImplicitLod :: Bool
  , -- | @maxPerStageDescriptorUpdateAfterBindSamplers@ is similar to
    -- @maxPerStageDescriptorSamplers@ but counts descriptors from descriptor
    -- sets created with or without the
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxPerStageDescriptorUpdateAfterBindSamplers :: Word32
  , -- | @maxPerStageDescriptorUpdateAfterBindUniformBuffers@ is similar to
    -- @maxPerStageDescriptorUniformBuffers@ but counts descriptors from
    -- descriptor sets created with or without the
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxPerStageDescriptorUpdateAfterBindUniformBuffers :: Word32
  , -- | @maxPerStageDescriptorUpdateAfterBindStorageBuffers@ is similar to
    -- @maxPerStageDescriptorStorageBuffers@ but counts descriptors from
    -- descriptor sets created with or without the
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxPerStageDescriptorUpdateAfterBindStorageBuffers :: Word32
  , -- | @maxPerStageDescriptorUpdateAfterBindSampledImages@ is similar to
    -- @maxPerStageDescriptorSampledImages@ but counts descriptors from
    -- descriptor sets created with or without the
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxPerStageDescriptorUpdateAfterBindSampledImages :: Word32
  , -- | @maxPerStageDescriptorUpdateAfterBindStorageImages@ is similar to
    -- @maxPerStageDescriptorStorageImages@ but counts descriptors from
    -- descriptor sets created with or without the
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxPerStageDescriptorUpdateAfterBindStorageImages :: Word32
  , -- | @maxPerStageDescriptorUpdateAfterBindInputAttachments@ is similar to
    -- @maxPerStageDescriptorInputAttachments@ but counts descriptors from
    -- descriptor sets created with or without the
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxPerStageDescriptorUpdateAfterBindInputAttachments :: Word32
  , -- | @maxPerStageUpdateAfterBindResources@ is similar to
    -- @maxPerStageResources@ but counts descriptors from descriptor sets
    -- created with or without the
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxPerStageUpdateAfterBindResources :: Word32
  , -- | @maxDescriptorSetUpdateAfterBindSamplers@ is similar to
    -- @maxDescriptorSetSamplers@ but counts descriptors from descriptor sets
    -- created with or without the
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxDescriptorSetUpdateAfterBindSamplers :: Word32
  , -- | @maxDescriptorSetUpdateAfterBindUniformBuffers@ is similar to
    -- @maxDescriptorSetUniformBuffers@ but counts descriptors from descriptor
    -- sets created with or without the
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxDescriptorSetUpdateAfterBindUniformBuffers :: Word32
  , -- | @maxDescriptorSetUpdateAfterBindUniformBuffersDynamic@ is similar to
    -- @maxDescriptorSetUniformBuffersDynamic@ but counts descriptors from
    -- descriptor sets created with or without the
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxDescriptorSetUpdateAfterBindUniformBuffersDynamic :: Word32
  , -- | @maxDescriptorSetUpdateAfterBindStorageBuffers@ is similar to
    -- @maxDescriptorSetStorageBuffers@ but counts descriptors from descriptor
    -- sets created with or without the
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxDescriptorSetUpdateAfterBindStorageBuffers :: Word32
  , -- | @maxDescriptorSetUpdateAfterBindStorageBuffersDynamic@ is similar to
    -- @maxDescriptorSetStorageBuffersDynamic@ but counts descriptors from
    -- descriptor sets created with or without the
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxDescriptorSetUpdateAfterBindStorageBuffersDynamic :: Word32
  , -- | @maxDescriptorSetUpdateAfterBindSampledImages@ is similar to
    -- @maxDescriptorSetSampledImages@ but counts descriptors from descriptor
    -- sets created with or without the
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxDescriptorSetUpdateAfterBindSampledImages :: Word32
  , -- | @maxDescriptorSetUpdateAfterBindStorageImages@ is similar to
    -- @maxDescriptorSetStorageImages@ but counts descriptors from descriptor
    -- sets created with or without the
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxDescriptorSetUpdateAfterBindStorageImages :: Word32
  , -- | @maxDescriptorSetUpdateAfterBindInputAttachments@ is similar to
    -- @maxDescriptorSetInputAttachments@ but counts descriptors from
    -- descriptor sets created with or without the
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    maxDescriptorSetUpdateAfterBindInputAttachments :: Word32
  }
  deriving (Typeable)
deriving instance Show PhysicalDeviceDescriptorIndexingProperties

instance ToCStruct PhysicalDeviceDescriptorIndexingProperties where
  withCStruct x f = allocaBytesAligned 112 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDescriptorIndexingProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxUpdateAfterBindDescriptorsInAllPools)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (shaderUniformBufferArrayNonUniformIndexingNative))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (shaderSampledImageArrayNonUniformIndexingNative))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (shaderStorageBufferArrayNonUniformIndexingNative))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (shaderStorageImageArrayNonUniformIndexingNative))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (shaderInputAttachmentArrayNonUniformIndexingNative))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (robustBufferAccessUpdateAfterBind))
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (quadDivergentImplicitLod))
    poke ((p `plusPtr` 48 :: Ptr Word32)) (maxPerStageDescriptorUpdateAfterBindSamplers)
    poke ((p `plusPtr` 52 :: Ptr Word32)) (maxPerStageDescriptorUpdateAfterBindUniformBuffers)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (maxPerStageDescriptorUpdateAfterBindStorageBuffers)
    poke ((p `plusPtr` 60 :: Ptr Word32)) (maxPerStageDescriptorUpdateAfterBindSampledImages)
    poke ((p `plusPtr` 64 :: Ptr Word32)) (maxPerStageDescriptorUpdateAfterBindStorageImages)
    poke ((p `plusPtr` 68 :: Ptr Word32)) (maxPerStageDescriptorUpdateAfterBindInputAttachments)
    poke ((p `plusPtr` 72 :: Ptr Word32)) (maxPerStageUpdateAfterBindResources)
    poke ((p `plusPtr` 76 :: Ptr Word32)) (maxDescriptorSetUpdateAfterBindSamplers)
    poke ((p `plusPtr` 80 :: Ptr Word32)) (maxDescriptorSetUpdateAfterBindUniformBuffers)
    poke ((p `plusPtr` 84 :: Ptr Word32)) (maxDescriptorSetUpdateAfterBindUniformBuffersDynamic)
    poke ((p `plusPtr` 88 :: Ptr Word32)) (maxDescriptorSetUpdateAfterBindStorageBuffers)
    poke ((p `plusPtr` 92 :: Ptr Word32)) (maxDescriptorSetUpdateAfterBindStorageBuffersDynamic)
    poke ((p `plusPtr` 96 :: Ptr Word32)) (maxDescriptorSetUpdateAfterBindSampledImages)
    poke ((p `plusPtr` 100 :: Ptr Word32)) (maxDescriptorSetUpdateAfterBindStorageImages)
    poke ((p `plusPtr` 104 :: Ptr Word32)) (maxDescriptorSetUpdateAfterBindInputAttachments)
    f
  cStructSize = 112
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 52 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 60 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 64 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 68 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 72 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 76 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 80 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 84 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 88 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 92 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 96 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 100 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 104 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceDescriptorIndexingProperties where
  peekCStruct p = do
    maxUpdateAfterBindDescriptorsInAllPools <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    shaderUniformBufferArrayNonUniformIndexingNative <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    shaderSampledImageArrayNonUniformIndexingNative <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    shaderStorageBufferArrayNonUniformIndexingNative <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    shaderStorageImageArrayNonUniformIndexingNative <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    shaderInputAttachmentArrayNonUniformIndexingNative <- peek @Bool32 ((p `plusPtr` 36 :: Ptr Bool32))
    robustBufferAccessUpdateAfterBind <- peek @Bool32 ((p `plusPtr` 40 :: Ptr Bool32))
    quadDivergentImplicitLod <- peek @Bool32 ((p `plusPtr` 44 :: Ptr Bool32))
    maxPerStageDescriptorUpdateAfterBindSamplers <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    maxPerStageDescriptorUpdateAfterBindUniformBuffers <- peek @Word32 ((p `plusPtr` 52 :: Ptr Word32))
    maxPerStageDescriptorUpdateAfterBindStorageBuffers <- peek @Word32 ((p `plusPtr` 56 :: Ptr Word32))
    maxPerStageDescriptorUpdateAfterBindSampledImages <- peek @Word32 ((p `plusPtr` 60 :: Ptr Word32))
    maxPerStageDescriptorUpdateAfterBindStorageImages <- peek @Word32 ((p `plusPtr` 64 :: Ptr Word32))
    maxPerStageDescriptorUpdateAfterBindInputAttachments <- peek @Word32 ((p `plusPtr` 68 :: Ptr Word32))
    maxPerStageUpdateAfterBindResources <- peek @Word32 ((p `plusPtr` 72 :: Ptr Word32))
    maxDescriptorSetUpdateAfterBindSamplers <- peek @Word32 ((p `plusPtr` 76 :: Ptr Word32))
    maxDescriptorSetUpdateAfterBindUniformBuffers <- peek @Word32 ((p `plusPtr` 80 :: Ptr Word32))
    maxDescriptorSetUpdateAfterBindUniformBuffersDynamic <- peek @Word32 ((p `plusPtr` 84 :: Ptr Word32))
    maxDescriptorSetUpdateAfterBindStorageBuffers <- peek @Word32 ((p `plusPtr` 88 :: Ptr Word32))
    maxDescriptorSetUpdateAfterBindStorageBuffersDynamic <- peek @Word32 ((p `plusPtr` 92 :: Ptr Word32))
    maxDescriptorSetUpdateAfterBindSampledImages <- peek @Word32 ((p `plusPtr` 96 :: Ptr Word32))
    maxDescriptorSetUpdateAfterBindStorageImages <- peek @Word32 ((p `plusPtr` 100 :: Ptr Word32))
    maxDescriptorSetUpdateAfterBindInputAttachments <- peek @Word32 ((p `plusPtr` 104 :: Ptr Word32))
    pure $ PhysicalDeviceDescriptorIndexingProperties
             maxUpdateAfterBindDescriptorsInAllPools (bool32ToBool shaderUniformBufferArrayNonUniformIndexingNative) (bool32ToBool shaderSampledImageArrayNonUniformIndexingNative) (bool32ToBool shaderStorageBufferArrayNonUniformIndexingNative) (bool32ToBool shaderStorageImageArrayNonUniformIndexingNative) (bool32ToBool shaderInputAttachmentArrayNonUniformIndexingNative) (bool32ToBool robustBufferAccessUpdateAfterBind) (bool32ToBool quadDivergentImplicitLod) maxPerStageDescriptorUpdateAfterBindSamplers maxPerStageDescriptorUpdateAfterBindUniformBuffers maxPerStageDescriptorUpdateAfterBindStorageBuffers maxPerStageDescriptorUpdateAfterBindSampledImages maxPerStageDescriptorUpdateAfterBindStorageImages maxPerStageDescriptorUpdateAfterBindInputAttachments maxPerStageUpdateAfterBindResources maxDescriptorSetUpdateAfterBindSamplers maxDescriptorSetUpdateAfterBindUniformBuffers maxDescriptorSetUpdateAfterBindUniformBuffersDynamic maxDescriptorSetUpdateAfterBindStorageBuffers maxDescriptorSetUpdateAfterBindStorageBuffersDynamic maxDescriptorSetUpdateAfterBindSampledImages maxDescriptorSetUpdateAfterBindStorageImages maxDescriptorSetUpdateAfterBindInputAttachments

instance Storable PhysicalDeviceDescriptorIndexingProperties where
  sizeOf ~_ = 112
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDescriptorIndexingProperties where
  zero = PhysicalDeviceDescriptorIndexingProperties
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkDescriptorSetLayoutBindingFlagsCreateInfo - Structure specifying
-- creation flags for descriptor set layout bindings
--
-- = Description
--
-- If @bindingCount@ is zero or if this structure is not included in the
-- @pNext@ chain, the
-- 'Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits.DescriptorBindingFlags'
-- for each descriptor set layout binding is considered to be zero.
-- Otherwise, the descriptor set layout binding at
-- 'Graphics.Vulkan.Core10.DescriptorSet.DescriptorSetLayoutCreateInfo'::@pBindings@[i]
-- uses the flags in @pBindingFlags@[i].
--
-- == Valid Usage
--
-- -   If @bindingCount@ is not zero, @bindingCount@ /must/ equal
--     'Graphics.Vulkan.Core10.DescriptorSet.DescriptorSetLayoutCreateInfo'::@bindingCount@
--
-- -   If
--     'Graphics.Vulkan.Core10.DescriptorSet.DescriptorSetLayoutCreateInfo'::@flags@
--     includes
--     'Graphics.Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR',
--     then all elements of @pBindingFlags@ /must/ not include
--     'Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT',
--     'Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT',
--     or
--     'Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT'
--
-- -   If an element of @pBindingFlags@ includes
--     'Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT',
--     then all other elements of
--     'Graphics.Vulkan.Core10.DescriptorSet.DescriptorSetLayoutCreateInfo'::@pBindings@
--     /must/ have a smaller value of @binding@
--
-- -   If
--     'PhysicalDeviceDescriptorIndexingFeatures'::@descriptorBindingUniformBufferUpdateAfterBind@
--     is not enabled, all bindings with descriptor type
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER'
--     /must/ not use
--     'Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'
--
-- -   If
--     'PhysicalDeviceDescriptorIndexingFeatures'::@descriptorBindingSampledImageUpdateAfterBind@
--     is not enabled, all bindings with descriptor type
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLER',
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     or
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE'
--     /must/ not use
--     'Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'
--
-- -   If
--     'PhysicalDeviceDescriptorIndexingFeatures'::@descriptorBindingStorageImageUpdateAfterBind@
--     is not enabled, all bindings with descriptor type
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE'
--     /must/ not use
--     'Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'
--
-- -   If
--     'PhysicalDeviceDescriptorIndexingFeatures'::@descriptorBindingStorageBufferUpdateAfterBind@
--     is not enabled, all bindings with descriptor type
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER'
--     /must/ not use
--     'Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'
--
-- -   If
--     'PhysicalDeviceDescriptorIndexingFeatures'::@descriptorBindingUniformTexelBufferUpdateAfterBind@
--     is not enabled, all bindings with descriptor type
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
--     /must/ not use
--     'Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'
--
-- -   If
--     'PhysicalDeviceDescriptorIndexingFeatures'::@descriptorBindingStorageTexelBufferUpdateAfterBind@
--     is not enabled, all bindings with descriptor type
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
--     /must/ not use
--     'Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'
--
-- -   If
--     'Graphics.Vulkan.Extensions.VK_EXT_inline_uniform_block.PhysicalDeviceInlineUniformBlockFeaturesEXT'::@descriptorBindingInlineUniformBlockUpdateAfterBind@
--     is not enabled, all bindings with descriptor type
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
--     /must/ not use
--     'Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'
--
-- -   All bindings with descriptor type
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INPUT_ATTACHMENT',
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC',
--     or
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
--     /must/ not use
--     'Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'
--
-- -   If
--     'PhysicalDeviceDescriptorIndexingFeatures'::@descriptorBindingUpdateUnusedWhilePending@
--     is not enabled, all elements of @pBindingFlags@ /must/ not include
--     'Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT'
--
-- -   If
--     'PhysicalDeviceDescriptorIndexingFeatures'::@descriptorBindingPartiallyBound@
--     is not enabled, all elements of @pBindingFlags@ /must/ not include
--     'Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT'
--
-- -   If
--     'PhysicalDeviceDescriptorIndexingFeatures'::@descriptorBindingVariableDescriptorCount@
--     is not enabled, all elements of @pBindingFlags@ /must/ not include
--     'Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT'
--
-- -   If an element of @pBindingFlags@ includes
--     'Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT',
--     that element’s @descriptorType@ /must/ not be
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
--     or
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO'
--
-- -   If @bindingCount@ is not @0@, and @pBindingFlags@ is not @NULL@,
--     @pBindingFlags@ /must/ be a valid pointer to an array of
--     @bindingCount@ valid combinations of
--     'Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits.DescriptorBindingFlagBits'
--     values
--
-- = See Also
--
-- 'Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits.DescriptorBindingFlags',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data DescriptorSetLayoutBindingFlagsCreateInfo = DescriptorSetLayoutBindingFlagsCreateInfo
  { -- | @pBindingFlags@ is a pointer to an array of
    -- 'Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits.DescriptorBindingFlags'
    -- bitfields, one for each descriptor set layout binding.
    bindingFlags :: Either Word32 (Vector DescriptorBindingFlags) }
  deriving (Typeable)
deriving instance Show DescriptorSetLayoutBindingFlagsCreateInfo

instance ToCStruct DescriptorSetLayoutBindingFlagsCreateInfo where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorSetLayoutBindingFlagsCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (either id (fromIntegral . Data.Vector.length) (bindingFlags)) :: Word32))
    pBindingFlags'' <- case (bindingFlags) of
      Left _ -> pure nullPtr
      Right v -> do
        pPBindingFlags' <- ContT $ allocaBytesAligned @DescriptorBindingFlags ((Data.Vector.length (v)) * 4) 4
        lift $ Data.Vector.imapM_ (\i e -> poke (pPBindingFlags' `plusPtr` (4 * (i)) :: Ptr DescriptorBindingFlags) (e)) (v)
        pure $ pPBindingFlags'
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr DescriptorBindingFlags))) pBindingFlags''
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct DescriptorSetLayoutBindingFlagsCreateInfo where
  peekCStruct p = do
    bindingCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pBindingFlags <- peek @(Ptr DescriptorBindingFlags) ((p `plusPtr` 24 :: Ptr (Ptr DescriptorBindingFlags)))
    pBindingFlags' <- maybePeek (\j -> generateM (fromIntegral bindingCount) (\i -> peek @DescriptorBindingFlags (((j) `advancePtrBytes` (4 * (i)) :: Ptr DescriptorBindingFlags)))) pBindingFlags
    let pBindingFlags'' = maybe (Left bindingCount) Right pBindingFlags'
    pure $ DescriptorSetLayoutBindingFlagsCreateInfo
             pBindingFlags''

instance Zero DescriptorSetLayoutBindingFlagsCreateInfo where
  zero = DescriptorSetLayoutBindingFlagsCreateInfo
           (Left 0)


-- | VkDescriptorSetVariableDescriptorCountAllocateInfo - Structure
-- specifying additional allocation parameters for descriptor sets
--
-- = Description
--
-- If @descriptorSetCount@ is zero or this structure is not included in the
-- @pNext@ chain, then the variable lengths are considered to be zero.
-- Otherwise, @pDescriptorCounts@[i] is the number of descriptors in the
-- variable count descriptor binding in the corresponding descriptor set
-- layout. If the variable count descriptor binding in the corresponding
-- descriptor set layout has a descriptor type of
-- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
-- then @pDescriptorCounts@[i] specifies the binding’s capacity in bytes.
-- If
-- 'Graphics.Vulkan.Core10.DescriptorSet.DescriptorSetAllocateInfo'::@pSetLayouts@[i]
-- does not include a variable count descriptor binding, then
-- @pDescriptorCounts@[i] is ignored.
--
-- == Valid Usage
--
-- -   If @descriptorSetCount@ is not zero, @descriptorSetCount@ /must/
--     equal
--     'Graphics.Vulkan.Core10.DescriptorSet.DescriptorSetAllocateInfo'::@descriptorSetCount@
--
-- -   If
--     'Graphics.Vulkan.Core10.DescriptorSet.DescriptorSetAllocateInfo'::@pSetLayouts@[i]
--     has a variable descriptor count binding, then @pDescriptorCounts@[i]
--     /must/ be less than or equal to the descriptor count specified for
--     that binding when the descriptor set layout was created
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO'
--
-- -   If @descriptorSetCount@ is not @0@, @pDescriptorCounts@ /must/ be a
--     valid pointer to an array of @descriptorSetCount@ @uint32_t@ values
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data DescriptorSetVariableDescriptorCountAllocateInfo = DescriptorSetVariableDescriptorCountAllocateInfo
  { -- | @pDescriptorCounts@ is a pointer to an array of descriptor counts, with
    -- each member specifying the number of descriptors in a variable
    -- descriptor count binding in the corresponding descriptor set being
    -- allocated.
    descriptorCounts :: Vector Word32 }
  deriving (Typeable)
deriving instance Show DescriptorSetVariableDescriptorCountAllocateInfo

instance ToCStruct DescriptorSetVariableDescriptorCountAllocateInfo where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorSetVariableDescriptorCountAllocateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (descriptorCounts)) :: Word32))
    pPDescriptorCounts' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (descriptorCounts)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDescriptorCounts' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (descriptorCounts)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Word32))) (pPDescriptorCounts')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pPDescriptorCounts' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDescriptorCounts' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Word32))) (pPDescriptorCounts')
    lift $ f

instance FromCStruct DescriptorSetVariableDescriptorCountAllocateInfo where
  peekCStruct p = do
    descriptorSetCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pDescriptorCounts <- peek @(Ptr Word32) ((p `plusPtr` 24 :: Ptr (Ptr Word32)))
    pDescriptorCounts' <- generateM (fromIntegral descriptorSetCount) (\i -> peek @Word32 ((pDescriptorCounts `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pure $ DescriptorSetVariableDescriptorCountAllocateInfo
             pDescriptorCounts'

instance Zero DescriptorSetVariableDescriptorCountAllocateInfo where
  zero = DescriptorSetVariableDescriptorCountAllocateInfo
           mempty


-- | VkDescriptorSetVariableDescriptorCountLayoutSupport - Structure
-- returning information about whether a descriptor set layout can be
-- supported
--
-- = Description
--
-- If the create info includes a variable-sized descriptor, then
-- @supported@ is determined assuming the requested size of the
-- variable-sized descriptor, and @maxVariableDescriptorCount@ is set to
-- the maximum size of that descriptor that /can/ be successfully created
-- (which is greater than or equal to the requested size passed in). If the
-- create info does not include a variable-sized descriptor or if the
-- 'PhysicalDeviceDescriptorIndexingFeatures'::@descriptorBindingVariableDescriptorCount@
-- feature is not enabled, then @maxVariableDescriptorCount@ is set to
-- zero. For the purposes of this command, a variable-sized descriptor
-- binding with a @descriptorCount@ of zero is treated as if the
-- @descriptorCount@ is one, and thus the binding is not ignored and the
-- maximum descriptor count will be returned. If the layout is not
-- supported, then the value written to @maxVariableDescriptorCount@ is
-- undefined.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data DescriptorSetVariableDescriptorCountLayoutSupport = DescriptorSetVariableDescriptorCountLayoutSupport
  { -- | @maxVariableDescriptorCount@ indicates the maximum number of descriptors
    -- supported in the highest numbered binding of the layout, if that binding
    -- is variable-sized. If the highest numbered binding of the layout has a
    -- descriptor type of
    -- 'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
    -- then @maxVariableDescriptorCount@ indicates the maximum byte size
    -- supported for the binding, if that binding is variable-sized.
    maxVariableDescriptorCount :: Word32 }
  deriving (Typeable)
deriving instance Show DescriptorSetVariableDescriptorCountLayoutSupport

instance ToCStruct DescriptorSetVariableDescriptorCountLayoutSupport where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorSetVariableDescriptorCountLayoutSupport{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxVariableDescriptorCount)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct DescriptorSetVariableDescriptorCountLayoutSupport where
  peekCStruct p = do
    maxVariableDescriptorCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ DescriptorSetVariableDescriptorCountLayoutSupport
             maxVariableDescriptorCount

instance Storable DescriptorSetVariableDescriptorCountLayoutSupport where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DescriptorSetVariableDescriptorCountLayoutSupport where
  zero = DescriptorSetVariableDescriptorCountLayoutSupport
           zero

