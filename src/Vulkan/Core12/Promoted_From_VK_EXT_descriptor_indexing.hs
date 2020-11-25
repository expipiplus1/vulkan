{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_EXT_descriptor_indexing"
module Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing  ( PhysicalDeviceDescriptorIndexingFeatures(..)
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
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core12.Enums.DescriptorBindingFlagBits (DescriptorBindingFlags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES))
import Vulkan.Core12.Enums.DescriptorBindingFlagBits (DescriptorBindingFlagBits(..))
import Vulkan.Core12.Enums.DescriptorBindingFlagBits (DescriptorBindingFlags)
import Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits (DescriptorPoolCreateFlagBits(..))
import Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits (DescriptorPoolCreateFlags)
import Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits (DescriptorSetLayoutCreateFlagBits(..))
import Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits (DescriptorSetLayoutCreateFlags)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType(..))

-- No documentation found for TopLevel "VkPhysicalDeviceDescriptorIndexingFeatures"
data PhysicalDeviceDescriptorIndexingFeatures = PhysicalDeviceDescriptorIndexingFeatures
  { -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeatures" "shaderInputAttachmentArrayDynamicIndexing"
    shaderInputAttachmentArrayDynamicIndexing :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeatures" "shaderUniformTexelBufferArrayDynamicIndexing"
    shaderUniformTexelBufferArrayDynamicIndexing :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeatures" "shaderStorageTexelBufferArrayDynamicIndexing"
    shaderStorageTexelBufferArrayDynamicIndexing :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeatures" "shaderUniformBufferArrayNonUniformIndexing"
    shaderUniformBufferArrayNonUniformIndexing :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeatures" "shaderSampledImageArrayNonUniformIndexing"
    shaderSampledImageArrayNonUniformIndexing :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeatures" "shaderStorageBufferArrayNonUniformIndexing"
    shaderStorageBufferArrayNonUniformIndexing :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeatures" "shaderStorageImageArrayNonUniformIndexing"
    shaderStorageImageArrayNonUniformIndexing :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeatures" "shaderInputAttachmentArrayNonUniformIndexing"
    shaderInputAttachmentArrayNonUniformIndexing :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeatures" "shaderUniformTexelBufferArrayNonUniformIndexing"
    shaderUniformTexelBufferArrayNonUniformIndexing :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeatures" "shaderStorageTexelBufferArrayNonUniformIndexing"
    shaderStorageTexelBufferArrayNonUniformIndexing :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeatures" "descriptorBindingUniformBufferUpdateAfterBind"
    descriptorBindingUniformBufferUpdateAfterBind :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeatures" "descriptorBindingSampledImageUpdateAfterBind"
    descriptorBindingSampledImageUpdateAfterBind :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeatures" "descriptorBindingStorageImageUpdateAfterBind"
    descriptorBindingStorageImageUpdateAfterBind :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeatures" "descriptorBindingStorageBufferUpdateAfterBind"
    descriptorBindingStorageBufferUpdateAfterBind :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeatures" "descriptorBindingUniformTexelBufferUpdateAfterBind"
    descriptorBindingUniformTexelBufferUpdateAfterBind :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeatures" "descriptorBindingStorageTexelBufferUpdateAfterBind"
    descriptorBindingStorageTexelBufferUpdateAfterBind :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeatures" "descriptorBindingUpdateUnusedWhilePending"
    descriptorBindingUpdateUnusedWhilePending :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeatures" "descriptorBindingPartiallyBound"
    descriptorBindingPartiallyBound :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeatures" "descriptorBindingVariableDescriptorCount"
    descriptorBindingVariableDescriptorCount :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeatures" "runtimeDescriptorArray"
    runtimeDescriptorArray :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDescriptorIndexingFeatures)
#endif
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



-- No documentation found for TopLevel "VkPhysicalDeviceDescriptorIndexingProperties"
data PhysicalDeviceDescriptorIndexingProperties = PhysicalDeviceDescriptorIndexingProperties
  { -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingProperties" "maxUpdateAfterBindDescriptorsInAllPools"
    maxUpdateAfterBindDescriptorsInAllPools :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingProperties" "shaderUniformBufferArrayNonUniformIndexingNative"
    shaderUniformBufferArrayNonUniformIndexingNative :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingProperties" "shaderSampledImageArrayNonUniformIndexingNative"
    shaderSampledImageArrayNonUniformIndexingNative :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingProperties" "shaderStorageBufferArrayNonUniformIndexingNative"
    shaderStorageBufferArrayNonUniformIndexingNative :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingProperties" "shaderStorageImageArrayNonUniformIndexingNative"
    shaderStorageImageArrayNonUniformIndexingNative :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingProperties" "shaderInputAttachmentArrayNonUniformIndexingNative"
    shaderInputAttachmentArrayNonUniformIndexingNative :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingProperties" "robustBufferAccessUpdateAfterBind"
    robustBufferAccessUpdateAfterBind :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingProperties" "quadDivergentImplicitLod"
    quadDivergentImplicitLod :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingProperties" "maxPerStageDescriptorUpdateAfterBindSamplers"
    maxPerStageDescriptorUpdateAfterBindSamplers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingProperties" "maxPerStageDescriptorUpdateAfterBindUniformBuffers"
    maxPerStageDescriptorUpdateAfterBindUniformBuffers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingProperties" "maxPerStageDescriptorUpdateAfterBindStorageBuffers"
    maxPerStageDescriptorUpdateAfterBindStorageBuffers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingProperties" "maxPerStageDescriptorUpdateAfterBindSampledImages"
    maxPerStageDescriptorUpdateAfterBindSampledImages :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingProperties" "maxPerStageDescriptorUpdateAfterBindStorageImages"
    maxPerStageDescriptorUpdateAfterBindStorageImages :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingProperties" "maxPerStageDescriptorUpdateAfterBindInputAttachments"
    maxPerStageDescriptorUpdateAfterBindInputAttachments :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingProperties" "maxPerStageUpdateAfterBindResources"
    maxPerStageUpdateAfterBindResources :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingProperties" "maxDescriptorSetUpdateAfterBindSamplers"
    maxDescriptorSetUpdateAfterBindSamplers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingProperties" "maxDescriptorSetUpdateAfterBindUniformBuffers"
    maxDescriptorSetUpdateAfterBindUniformBuffers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingProperties" "maxDescriptorSetUpdateAfterBindUniformBuffersDynamic"
    maxDescriptorSetUpdateAfterBindUniformBuffersDynamic :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingProperties" "maxDescriptorSetUpdateAfterBindStorageBuffers"
    maxDescriptorSetUpdateAfterBindStorageBuffers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingProperties" "maxDescriptorSetUpdateAfterBindStorageBuffersDynamic"
    maxDescriptorSetUpdateAfterBindStorageBuffersDynamic :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingProperties" "maxDescriptorSetUpdateAfterBindSampledImages"
    maxDescriptorSetUpdateAfterBindSampledImages :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingProperties" "maxDescriptorSetUpdateAfterBindStorageImages"
    maxDescriptorSetUpdateAfterBindStorageImages :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingProperties" "maxDescriptorSetUpdateAfterBindInputAttachments"
    maxDescriptorSetUpdateAfterBindInputAttachments :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDescriptorIndexingProperties)
#endif
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



-- No documentation found for TopLevel "VkDescriptorSetLayoutBindingFlagsCreateInfo"
data DescriptorSetLayoutBindingFlagsCreateInfo = DescriptorSetLayoutBindingFlagsCreateInfo
  { -- No documentation found for Nested "VkDescriptorSetLayoutBindingFlagsCreateInfo" "pBindingFlags"
    bindingFlags :: Vector DescriptorBindingFlags }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DescriptorSetLayoutBindingFlagsCreateInfo)
#endif
deriving instance Show DescriptorSetLayoutBindingFlagsCreateInfo

instance ToCStruct DescriptorSetLayoutBindingFlagsCreateInfo where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorSetLayoutBindingFlagsCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (bindingFlags)) :: Word32))
    pPBindingFlags' <- ContT $ allocaBytesAligned @DescriptorBindingFlags ((Data.Vector.length (bindingFlags)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPBindingFlags' `plusPtr` (4 * (i)) :: Ptr DescriptorBindingFlags) (e)) (bindingFlags)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr DescriptorBindingFlags))) (pPBindingFlags')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pPBindingFlags' <- ContT $ allocaBytesAligned @DescriptorBindingFlags ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPBindingFlags' `plusPtr` (4 * (i)) :: Ptr DescriptorBindingFlags) (e)) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr DescriptorBindingFlags))) (pPBindingFlags')
    lift $ f

instance FromCStruct DescriptorSetLayoutBindingFlagsCreateInfo where
  peekCStruct p = do
    bindingCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pBindingFlags <- peek @(Ptr DescriptorBindingFlags) ((p `plusPtr` 24 :: Ptr (Ptr DescriptorBindingFlags)))
    pBindingFlags' <- generateM (fromIntegral bindingCount) (\i -> peek @DescriptorBindingFlags ((pBindingFlags `advancePtrBytes` (4 * (i)) :: Ptr DescriptorBindingFlags)))
    pure $ DescriptorSetLayoutBindingFlagsCreateInfo
             pBindingFlags'

instance Zero DescriptorSetLayoutBindingFlagsCreateInfo where
  zero = DescriptorSetLayoutBindingFlagsCreateInfo
           mempty



-- No documentation found for TopLevel "VkDescriptorSetVariableDescriptorCountAllocateInfo"
data DescriptorSetVariableDescriptorCountAllocateInfo = DescriptorSetVariableDescriptorCountAllocateInfo
  { -- No documentation found for Nested "VkDescriptorSetVariableDescriptorCountAllocateInfo" "pDescriptorCounts"
    descriptorCounts :: Vector Word32 }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DescriptorSetVariableDescriptorCountAllocateInfo)
#endif
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



-- No documentation found for TopLevel "VkDescriptorSetVariableDescriptorCountLayoutSupport"
data DescriptorSetVariableDescriptorCountLayoutSupport = DescriptorSetVariableDescriptorCountLayoutSupport
  { -- No documentation found for Nested "VkDescriptorSetVariableDescriptorCountLayoutSupport" "maxVariableDescriptorCount"
    maxVariableDescriptorCount :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DescriptorSetVariableDescriptorCountLayoutSupport)
#endif
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

