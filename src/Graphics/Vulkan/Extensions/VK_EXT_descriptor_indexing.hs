{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_descriptor_indexing
  ( DescriptorBindingFlagBitsEXT
  , pattern DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT
  , pattern DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT
  , pattern DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT
  , pattern DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT
  , DescriptorBindingFlagsEXT
#if defined(VK_USE_PLATFORM_GGP)
  , DescriptorSetLayoutBindingFlagsCreateInfoEXT(..)
  , DescriptorSetVariableDescriptorCountAllocateInfoEXT(..)
  , DescriptorSetVariableDescriptorCountLayoutSupportEXT(..)
  , PhysicalDeviceDescriptorIndexingFeaturesEXT(..)
  , PhysicalDeviceDescriptorIndexingPropertiesEXT(..)
#endif
  , pattern EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
  , pattern EXT_DESCRIPTOR_INDEXING_SPEC_VERSION
  , pattern STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT
  , pattern STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT
  , pattern DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT
  , pattern DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT
  , pattern ERROR_FRAGMENTATION_EXT
  ) where

import Data.String
  ( IsString
  )

#if defined(VK_USE_PLATFORM_GGP)
import Data.Vector
  ( Vector
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Data.Word
  ( Word32
  )
#endif



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing
  ( VkDescriptorBindingFlagBitsEXT(..)
  , pattern VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT
  , pattern VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT
  , pattern VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT
  , pattern VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT
  , pattern VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
  , pattern VK_EXT_DESCRIPTOR_INDEXING_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern ERROR_FRAGMENTATION_EXT
  , pattern STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT
  , pattern STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT
  )
import Graphics.Vulkan.Core10.DescriptorSet
  ( pattern DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT
  , pattern DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT
  )


-- No documentation found for TopLevel "DescriptorBindingFlagBitsEXT"
type DescriptorBindingFlagBitsEXT = VkDescriptorBindingFlagBitsEXT


{-# complete DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT, DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT, DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT, DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT :: DescriptorBindingFlagBitsEXT #-}


-- No documentation found for Nested "DescriptorBindingFlagBitsEXT" "DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT"
pattern DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT :: (a ~ DescriptorBindingFlagBitsEXT) => a
pattern DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT = VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT


-- No documentation found for Nested "DescriptorBindingFlagBitsEXT" "DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT"
pattern DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT :: (a ~ DescriptorBindingFlagBitsEXT) => a
pattern DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT = VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT


-- No documentation found for Nested "DescriptorBindingFlagBitsEXT" "DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT"
pattern DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT :: (a ~ DescriptorBindingFlagBitsEXT) => a
pattern DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT = VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT


-- No documentation found for Nested "DescriptorBindingFlagBitsEXT" "DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT"
pattern DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT :: (a ~ DescriptorBindingFlagBitsEXT) => a
pattern DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT = VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT

-- No documentation found for TopLevel "DescriptorBindingFlagsEXT"
type DescriptorBindingFlagsEXT = DescriptorBindingFlagBitsEXT


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDescriptorSetLayoutBindingFlagsCreateInfoEXT"
data DescriptorSetLayoutBindingFlagsCreateInfoEXT = DescriptorSetLayoutBindingFlagsCreateInfoEXT
  { -- No documentation found for Nested "DescriptorSetLayoutBindingFlagsCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DescriptorSetLayoutBindingFlagsCreateInfoEXT" "pBindingFlags"
  bindingFlags :: Either Word32 (Vector DescriptorBindingFlagsEXT)
  }
  deriving (Show, Eq)

instance Zero DescriptorSetLayoutBindingFlagsCreateInfoEXT where
  zero = DescriptorSetLayoutBindingFlagsCreateInfoEXT Nothing
                                                      (Left 0)

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDescriptorSetVariableDescriptorCountAllocateInfoEXT"
data DescriptorSetVariableDescriptorCountAllocateInfoEXT = DescriptorSetVariableDescriptorCountAllocateInfoEXT
  { -- No documentation found for Nested "DescriptorSetVariableDescriptorCountAllocateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DescriptorSetVariableDescriptorCountAllocateInfoEXT" "pDescriptorCounts"
  descriptorCounts :: Vector Word32
  }
  deriving (Show, Eq)

instance Zero DescriptorSetVariableDescriptorCountAllocateInfoEXT where
  zero = DescriptorSetVariableDescriptorCountAllocateInfoEXT Nothing
                                                             mempty

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDescriptorSetVariableDescriptorCountLayoutSupportEXT"
data DescriptorSetVariableDescriptorCountLayoutSupportEXT = DescriptorSetVariableDescriptorCountLayoutSupportEXT
  { -- No documentation found for Nested "DescriptorSetVariableDescriptorCountLayoutSupportEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DescriptorSetVariableDescriptorCountLayoutSupportEXT" "maxVariableDescriptorCount"
  maxVariableDescriptorCount :: Word32
  }
  deriving (Show, Eq)

instance Zero DescriptorSetVariableDescriptorCountLayoutSupportEXT where
  zero = DescriptorSetVariableDescriptorCountLayoutSupportEXT Nothing
                                                              zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceDescriptorIndexingFeaturesEXT"
data PhysicalDeviceDescriptorIndexingFeaturesEXT = PhysicalDeviceDescriptorIndexingFeaturesEXT
  { -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderInputAttachmentArrayDynamicIndexing"
  shaderInputAttachmentArrayDynamicIndexing :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderUniformTexelBufferArrayDynamicIndexing"
  shaderUniformTexelBufferArrayDynamicIndexing :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderStorageTexelBufferArrayDynamicIndexing"
  shaderStorageTexelBufferArrayDynamicIndexing :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderUniformBufferArrayNonUniformIndexing"
  shaderUniformBufferArrayNonUniformIndexing :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderSampledImageArrayNonUniformIndexing"
  shaderSampledImageArrayNonUniformIndexing :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderStorageBufferArrayNonUniformIndexing"
  shaderStorageBufferArrayNonUniformIndexing :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderStorageImageArrayNonUniformIndexing"
  shaderStorageImageArrayNonUniformIndexing :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderInputAttachmentArrayNonUniformIndexing"
  shaderInputAttachmentArrayNonUniformIndexing :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderUniformTexelBufferArrayNonUniformIndexing"
  shaderUniformTexelBufferArrayNonUniformIndexing :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderStorageTexelBufferArrayNonUniformIndexing"
  shaderStorageTexelBufferArrayNonUniformIndexing :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "descriptorBindingUniformBufferUpdateAfterBind"
  descriptorBindingUniformBufferUpdateAfterBind :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "descriptorBindingSampledImageUpdateAfterBind"
  descriptorBindingSampledImageUpdateAfterBind :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "descriptorBindingStorageImageUpdateAfterBind"
  descriptorBindingStorageImageUpdateAfterBind :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "descriptorBindingStorageBufferUpdateAfterBind"
  descriptorBindingStorageBufferUpdateAfterBind :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "descriptorBindingUniformTexelBufferUpdateAfterBind"
  descriptorBindingUniformTexelBufferUpdateAfterBind :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "descriptorBindingStorageTexelBufferUpdateAfterBind"
  descriptorBindingStorageTexelBufferUpdateAfterBind :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "descriptorBindingUpdateUnusedWhilePending"
  descriptorBindingUpdateUnusedWhilePending :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "descriptorBindingPartiallyBound"
  descriptorBindingPartiallyBound :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "descriptorBindingVariableDescriptorCount"
  descriptorBindingVariableDescriptorCount :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "runtimeDescriptorArray"
  runtimeDescriptorArray :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceDescriptorIndexingFeaturesEXT where
  zero = PhysicalDeviceDescriptorIndexingFeaturesEXT Nothing
                                                     False
                                                     False
                                                     False
                                                     False
                                                     False
                                                     False
                                                     False
                                                     False
                                                     False
                                                     False
                                                     False
                                                     False
                                                     False
                                                     False
                                                     False
                                                     False
                                                     False
                                                     False
                                                     False
                                                     False

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceDescriptorIndexingPropertiesEXT"
data PhysicalDeviceDescriptorIndexingPropertiesEXT = PhysicalDeviceDescriptorIndexingPropertiesEXT
  { -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "maxUpdateAfterBindDescriptorsInAllPools"
  maxUpdateAfterBindDescriptorsInAllPools :: Word32
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "shaderUniformBufferArrayNonUniformIndexingNative"
  shaderUniformBufferArrayNonUniformIndexingNative :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "shaderSampledImageArrayNonUniformIndexingNative"
  shaderSampledImageArrayNonUniformIndexingNative :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "shaderStorageBufferArrayNonUniformIndexingNative"
  shaderStorageBufferArrayNonUniformIndexingNative :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "shaderStorageImageArrayNonUniformIndexingNative"
  shaderStorageImageArrayNonUniformIndexingNative :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "shaderInputAttachmentArrayNonUniformIndexingNative"
  shaderInputAttachmentArrayNonUniformIndexingNative :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "robustBufferAccessUpdateAfterBind"
  robustBufferAccessUpdateAfterBind :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "quadDivergentImplicitLod"
  quadDivergentImplicitLod :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "maxPerStageDescriptorUpdateAfterBindSamplers"
  maxPerStageDescriptorUpdateAfterBindSamplers :: Word32
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "maxPerStageDescriptorUpdateAfterBindUniformBuffers"
  maxPerStageDescriptorUpdateAfterBindUniformBuffers :: Word32
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "maxPerStageDescriptorUpdateAfterBindStorageBuffers"
  maxPerStageDescriptorUpdateAfterBindStorageBuffers :: Word32
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "maxPerStageDescriptorUpdateAfterBindSampledImages"
  maxPerStageDescriptorUpdateAfterBindSampledImages :: Word32
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "maxPerStageDescriptorUpdateAfterBindStorageImages"
  maxPerStageDescriptorUpdateAfterBindStorageImages :: Word32
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "maxPerStageDescriptorUpdateAfterBindInputAttachments"
  maxPerStageDescriptorUpdateAfterBindInputAttachments :: Word32
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "maxPerStageUpdateAfterBindResources"
  maxPerStageUpdateAfterBindResources :: Word32
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "maxDescriptorSetUpdateAfterBindSamplers"
  maxDescriptorSetUpdateAfterBindSamplers :: Word32
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "maxDescriptorSetUpdateAfterBindUniformBuffers"
  maxDescriptorSetUpdateAfterBindUniformBuffers :: Word32
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "maxDescriptorSetUpdateAfterBindUniformBuffersDynamic"
  maxDescriptorSetUpdateAfterBindUniformBuffersDynamic :: Word32
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "maxDescriptorSetUpdateAfterBindStorageBuffers"
  maxDescriptorSetUpdateAfterBindStorageBuffers :: Word32
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "maxDescriptorSetUpdateAfterBindStorageBuffersDynamic"
  maxDescriptorSetUpdateAfterBindStorageBuffersDynamic :: Word32
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "maxDescriptorSetUpdateAfterBindSampledImages"
  maxDescriptorSetUpdateAfterBindSampledImages :: Word32
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "maxDescriptorSetUpdateAfterBindStorageImages"
  maxDescriptorSetUpdateAfterBindStorageImages :: Word32
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "maxDescriptorSetUpdateAfterBindInputAttachments"
  maxDescriptorSetUpdateAfterBindInputAttachments :: Word32
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceDescriptorIndexingPropertiesEXT where
  zero = PhysicalDeviceDescriptorIndexingPropertiesEXT Nothing
                                                       zero
                                                       False
                                                       False
                                                       False
                                                       False
                                                       False
                                                       False
                                                       False
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

#endif

-- No documentation found for TopLevel "VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME"
pattern EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME = VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_DESCRIPTOR_INDEXING_SPEC_VERSION"
pattern EXT_DESCRIPTOR_INDEXING_SPEC_VERSION :: Integral a => a
pattern EXT_DESCRIPTOR_INDEXING_SPEC_VERSION = VK_EXT_DESCRIPTOR_INDEXING_SPEC_VERSION
