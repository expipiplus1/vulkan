{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_descriptor_indexing
  ( DescriptorBindingFlagBitsEXT
  , DescriptorBindingFlagsEXT
  , withCStructDescriptorSetLayoutBindingFlagsCreateInfoEXT
  , fromCStructDescriptorSetLayoutBindingFlagsCreateInfoEXT
  , DescriptorSetLayoutBindingFlagsCreateInfoEXT(..)
  , withCStructDescriptorSetVariableDescriptorCountAllocateInfoEXT
  , fromCStructDescriptorSetVariableDescriptorCountAllocateInfoEXT
  , DescriptorSetVariableDescriptorCountAllocateInfoEXT(..)
  , withCStructDescriptorSetVariableDescriptorCountLayoutSupportEXT
  , fromCStructDescriptorSetVariableDescriptorCountLayoutSupportEXT
  , DescriptorSetVariableDescriptorCountLayoutSupportEXT(..)
  , withCStructPhysicalDeviceDescriptorIndexingFeaturesEXT
  , fromCStructPhysicalDeviceDescriptorIndexingFeaturesEXT
  , PhysicalDeviceDescriptorIndexingFeaturesEXT(..)
  , withCStructPhysicalDeviceDescriptorIndexingPropertiesEXT
  , fromCStructPhysicalDeviceDescriptorIndexingPropertiesEXT
  , PhysicalDeviceDescriptorIndexingPropertiesEXT(..)
  , pattern VK_EXT_DESCRIPTOR_INDEXING_SPEC_VERSION
  , pattern VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT
  , pattern VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT
  , pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT
  , pattern VK_ERROR_FRAGMENTATION_EXT
  ) where

import Data.Function
  ( (&)
  )
import Data.Maybe
  ( maybe
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( empty
  , generateM
  , length
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing
  ( VkDescriptorBindingFlagBitsEXT(..)
  , VkDescriptorSetLayoutBindingFlagsCreateInfoEXT(..)
  , VkDescriptorSetVariableDescriptorCountAllocateInfoEXT(..)
  , VkDescriptorSetVariableDescriptorCountLayoutSupportEXT(..)
  , VkPhysicalDeviceDescriptorIndexingFeaturesEXT(..)
  , VkPhysicalDeviceDescriptorIndexingPropertiesEXT(..)
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing
  ( pattern VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT
  , pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT
  , pattern VK_ERROR_FRAGMENTATION_EXT
  , pattern VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
  , pattern VK_EXT_DESCRIPTOR_INDEXING_SPEC_VERSION
  )


-- No documentation found for TopLevel "DescriptorBindingFlagBitsEXT"
type DescriptorBindingFlagBitsEXT = VkDescriptorBindingFlagBitsEXT
-- No documentation found for TopLevel "DescriptorBindingFlagsEXT"
type DescriptorBindingFlagsEXT = DescriptorBindingFlagBitsEXT
-- No documentation found for TopLevel "DescriptorSetLayoutBindingFlagsCreateInfoEXT"
data DescriptorSetLayoutBindingFlagsCreateInfoEXT = DescriptorSetLayoutBindingFlagsCreateInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "DescriptorSetLayoutBindingFlagsCreateInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  -- Optional length valued member elided
  , -- No documentation found for Nested "DescriptorSetLayoutBindingFlagsCreateInfoEXT" "pBindingFlags"
  vkPBindingFlags :: Maybe (Vector DescriptorBindingFlagsEXT)
  }
  deriving (Show, Eq)
withCStructDescriptorSetLayoutBindingFlagsCreateInfoEXT :: DescriptorSetLayoutBindingFlagsCreateInfoEXT -> (VkDescriptorSetLayoutBindingFlagsCreateInfoEXT -> IO a) -> IO a
withCStructDescriptorSetLayoutBindingFlagsCreateInfoEXT from cont = maybeWith (withVec (&)) (vkPBindingFlags (from :: DescriptorSetLayoutBindingFlagsCreateInfoEXT)) (\pBindingFlags -> maybeWith withSomeVkStruct (vkPNext (from :: DescriptorSetLayoutBindingFlagsCreateInfoEXT)) (\pPNext -> cont (VkDescriptorSetLayoutBindingFlagsCreateInfoEXT VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT pPNext (maybe 0 (fromIntegral . Data.Vector.length) (vkPBindingFlags (from :: DescriptorSetLayoutBindingFlagsCreateInfoEXT))) pBindingFlags)))
fromCStructDescriptorSetLayoutBindingFlagsCreateInfoEXT :: VkDescriptorSetLayoutBindingFlagsCreateInfoEXT -> IO DescriptorSetLayoutBindingFlagsCreateInfoEXT
fromCStructDescriptorSetLayoutBindingFlagsCreateInfoEXT c = DescriptorSetLayoutBindingFlagsCreateInfoEXT <$> -- Univalued Member elided
                                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDescriptorSetLayoutBindingFlagsCreateInfoEXT)))
                                                                                                         -- Optional length valued member elided
                                                                                                         <*> maybePeek (\p -> Data.Vector.generateM (fromIntegral (vkBindingCount (c :: VkDescriptorSetLayoutBindingFlagsCreateInfoEXT))) (peekElemOff p)) (vkPBindingFlags (c :: VkDescriptorSetLayoutBindingFlagsCreateInfoEXT))
instance Zero DescriptorSetLayoutBindingFlagsCreateInfoEXT where
  zero = DescriptorSetLayoutBindingFlagsCreateInfoEXT Nothing
                                                      Nothing
-- No documentation found for TopLevel "DescriptorSetVariableDescriptorCountAllocateInfoEXT"
data DescriptorSetVariableDescriptorCountAllocateInfoEXT = DescriptorSetVariableDescriptorCountAllocateInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "DescriptorSetVariableDescriptorCountAllocateInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "DescriptorSetVariableDescriptorCountAllocateInfoEXT" "pDescriptorCounts"
  vkPDescriptorCounts :: Vector Word32
  }
  deriving (Show, Eq)
withCStructDescriptorSetVariableDescriptorCountAllocateInfoEXT :: DescriptorSetVariableDescriptorCountAllocateInfoEXT -> (VkDescriptorSetVariableDescriptorCountAllocateInfoEXT -> IO a) -> IO a
withCStructDescriptorSetVariableDescriptorCountAllocateInfoEXT from cont = withVec (&) (vkPDescriptorCounts (from :: DescriptorSetVariableDescriptorCountAllocateInfoEXT)) (\pDescriptorCounts -> maybeWith withSomeVkStruct (vkPNext (from :: DescriptorSetVariableDescriptorCountAllocateInfoEXT)) (\pPNext -> cont (VkDescriptorSetVariableDescriptorCountAllocateInfoEXT VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT pPNext (fromIntegral (Data.Vector.length (vkPDescriptorCounts (from :: DescriptorSetVariableDescriptorCountAllocateInfoEXT)))) pDescriptorCounts)))
fromCStructDescriptorSetVariableDescriptorCountAllocateInfoEXT :: VkDescriptorSetVariableDescriptorCountAllocateInfoEXT -> IO DescriptorSetVariableDescriptorCountAllocateInfoEXT
fromCStructDescriptorSetVariableDescriptorCountAllocateInfoEXT c = DescriptorSetVariableDescriptorCountAllocateInfoEXT <$> -- Univalued Member elided
                                                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDescriptorSetVariableDescriptorCountAllocateInfoEXT)))
                                                                                                                       -- Length valued member elided
                                                                                                                       <*> (Data.Vector.generateM (fromIntegral (vkDescriptorSetCount (c :: VkDescriptorSetVariableDescriptorCountAllocateInfoEXT))) (peekElemOff (vkPDescriptorCounts (c :: VkDescriptorSetVariableDescriptorCountAllocateInfoEXT))))
instance Zero DescriptorSetVariableDescriptorCountAllocateInfoEXT where
  zero = DescriptorSetVariableDescriptorCountAllocateInfoEXT Nothing
                                                             Data.Vector.empty
-- No documentation found for TopLevel "DescriptorSetVariableDescriptorCountLayoutSupportEXT"
data DescriptorSetVariableDescriptorCountLayoutSupportEXT = DescriptorSetVariableDescriptorCountLayoutSupportEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "DescriptorSetVariableDescriptorCountLayoutSupportEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DescriptorSetVariableDescriptorCountLayoutSupportEXT" "maxVariableDescriptorCount"
  vkMaxVariableDescriptorCount :: Word32
  }
  deriving (Show, Eq)
withCStructDescriptorSetVariableDescriptorCountLayoutSupportEXT :: DescriptorSetVariableDescriptorCountLayoutSupportEXT -> (VkDescriptorSetVariableDescriptorCountLayoutSupportEXT -> IO a) -> IO a
withCStructDescriptorSetVariableDescriptorCountLayoutSupportEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: DescriptorSetVariableDescriptorCountLayoutSupportEXT)) (\pPNext -> cont (VkDescriptorSetVariableDescriptorCountLayoutSupportEXT VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT pPNext (vkMaxVariableDescriptorCount (from :: DescriptorSetVariableDescriptorCountLayoutSupportEXT))))
fromCStructDescriptorSetVariableDescriptorCountLayoutSupportEXT :: VkDescriptorSetVariableDescriptorCountLayoutSupportEXT -> IO DescriptorSetVariableDescriptorCountLayoutSupportEXT
fromCStructDescriptorSetVariableDescriptorCountLayoutSupportEXT c = DescriptorSetVariableDescriptorCountLayoutSupportEXT <$> -- Univalued Member elided
                                                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDescriptorSetVariableDescriptorCountLayoutSupportEXT)))
                                                                                                                         <*> pure (vkMaxVariableDescriptorCount (c :: VkDescriptorSetVariableDescriptorCountLayoutSupportEXT))
instance Zero DescriptorSetVariableDescriptorCountLayoutSupportEXT where
  zero = DescriptorSetVariableDescriptorCountLayoutSupportEXT Nothing
                                                              zero
-- No documentation found for TopLevel "PhysicalDeviceDescriptorIndexingFeaturesEXT"
data PhysicalDeviceDescriptorIndexingFeaturesEXT = PhysicalDeviceDescriptorIndexingFeaturesEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderInputAttachmentArrayDynamicIndexing"
  vkShaderInputAttachmentArrayDynamicIndexing :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderUniformTexelBufferArrayDynamicIndexing"
  vkShaderUniformTexelBufferArrayDynamicIndexing :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderStorageTexelBufferArrayDynamicIndexing"
  vkShaderStorageTexelBufferArrayDynamicIndexing :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderUniformBufferArrayNonUniformIndexing"
  vkShaderUniformBufferArrayNonUniformIndexing :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderSampledImageArrayNonUniformIndexing"
  vkShaderSampledImageArrayNonUniformIndexing :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderStorageBufferArrayNonUniformIndexing"
  vkShaderStorageBufferArrayNonUniformIndexing :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderStorageImageArrayNonUniformIndexing"
  vkShaderStorageImageArrayNonUniformIndexing :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderInputAttachmentArrayNonUniformIndexing"
  vkShaderInputAttachmentArrayNonUniformIndexing :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderUniformTexelBufferArrayNonUniformIndexing"
  vkShaderUniformTexelBufferArrayNonUniformIndexing :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderStorageTexelBufferArrayNonUniformIndexing"
  vkShaderStorageTexelBufferArrayNonUniformIndexing :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "descriptorBindingUniformBufferUpdateAfterBind"
  vkDescriptorBindingUniformBufferUpdateAfterBind :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "descriptorBindingSampledImageUpdateAfterBind"
  vkDescriptorBindingSampledImageUpdateAfterBind :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "descriptorBindingStorageImageUpdateAfterBind"
  vkDescriptorBindingStorageImageUpdateAfterBind :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "descriptorBindingStorageBufferUpdateAfterBind"
  vkDescriptorBindingStorageBufferUpdateAfterBind :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "descriptorBindingUniformTexelBufferUpdateAfterBind"
  vkDescriptorBindingUniformTexelBufferUpdateAfterBind :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "descriptorBindingStorageTexelBufferUpdateAfterBind"
  vkDescriptorBindingStorageTexelBufferUpdateAfterBind :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "descriptorBindingUpdateUnusedWhilePending"
  vkDescriptorBindingUpdateUnusedWhilePending :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "descriptorBindingPartiallyBound"
  vkDescriptorBindingPartiallyBound :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "descriptorBindingVariableDescriptorCount"
  vkDescriptorBindingVariableDescriptorCount :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "runtimeDescriptorArray"
  vkRuntimeDescriptorArray :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceDescriptorIndexingFeaturesEXT :: PhysicalDeviceDescriptorIndexingFeaturesEXT -> (VkPhysicalDeviceDescriptorIndexingFeaturesEXT -> IO a) -> IO a
withCStructPhysicalDeviceDescriptorIndexingFeaturesEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceDescriptorIndexingFeaturesEXT)) (\pPNext -> cont (VkPhysicalDeviceDescriptorIndexingFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT pPNext (boolToBool32 (vkShaderInputAttachmentArrayDynamicIndexing (from :: PhysicalDeviceDescriptorIndexingFeaturesEXT))) (boolToBool32 (vkShaderUniformTexelBufferArrayDynamicIndexing (from :: PhysicalDeviceDescriptorIndexingFeaturesEXT))) (boolToBool32 (vkShaderStorageTexelBufferArrayDynamicIndexing (from :: PhysicalDeviceDescriptorIndexingFeaturesEXT))) (boolToBool32 (vkShaderUniformBufferArrayNonUniformIndexing (from :: PhysicalDeviceDescriptorIndexingFeaturesEXT))) (boolToBool32 (vkShaderSampledImageArrayNonUniformIndexing (from :: PhysicalDeviceDescriptorIndexingFeaturesEXT))) (boolToBool32 (vkShaderStorageBufferArrayNonUniformIndexing (from :: PhysicalDeviceDescriptorIndexingFeaturesEXT))) (boolToBool32 (vkShaderStorageImageArrayNonUniformIndexing (from :: PhysicalDeviceDescriptorIndexingFeaturesEXT))) (boolToBool32 (vkShaderInputAttachmentArrayNonUniformIndexing (from :: PhysicalDeviceDescriptorIndexingFeaturesEXT))) (boolToBool32 (vkShaderUniformTexelBufferArrayNonUniformIndexing (from :: PhysicalDeviceDescriptorIndexingFeaturesEXT))) (boolToBool32 (vkShaderStorageTexelBufferArrayNonUniformIndexing (from :: PhysicalDeviceDescriptorIndexingFeaturesEXT))) (boolToBool32 (vkDescriptorBindingUniformBufferUpdateAfterBind (from :: PhysicalDeviceDescriptorIndexingFeaturesEXT))) (boolToBool32 (vkDescriptorBindingSampledImageUpdateAfterBind (from :: PhysicalDeviceDescriptorIndexingFeaturesEXT))) (boolToBool32 (vkDescriptorBindingStorageImageUpdateAfterBind (from :: PhysicalDeviceDescriptorIndexingFeaturesEXT))) (boolToBool32 (vkDescriptorBindingStorageBufferUpdateAfterBind (from :: PhysicalDeviceDescriptorIndexingFeaturesEXT))) (boolToBool32 (vkDescriptorBindingUniformTexelBufferUpdateAfterBind (from :: PhysicalDeviceDescriptorIndexingFeaturesEXT))) (boolToBool32 (vkDescriptorBindingStorageTexelBufferUpdateAfterBind (from :: PhysicalDeviceDescriptorIndexingFeaturesEXT))) (boolToBool32 (vkDescriptorBindingUpdateUnusedWhilePending (from :: PhysicalDeviceDescriptorIndexingFeaturesEXT))) (boolToBool32 (vkDescriptorBindingPartiallyBound (from :: PhysicalDeviceDescriptorIndexingFeaturesEXT))) (boolToBool32 (vkDescriptorBindingVariableDescriptorCount (from :: PhysicalDeviceDescriptorIndexingFeaturesEXT))) (boolToBool32 (vkRuntimeDescriptorArray (from :: PhysicalDeviceDescriptorIndexingFeaturesEXT)))))
fromCStructPhysicalDeviceDescriptorIndexingFeaturesEXT :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT -> IO PhysicalDeviceDescriptorIndexingFeaturesEXT
fromCStructPhysicalDeviceDescriptorIndexingFeaturesEXT c = PhysicalDeviceDescriptorIndexingFeaturesEXT <$> -- Univalued Member elided
                                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT)))
                                                                                                       <*> pure (bool32ToBool (vkShaderInputAttachmentArrayDynamicIndexing (c :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT)))
                                                                                                       <*> pure (bool32ToBool (vkShaderUniformTexelBufferArrayDynamicIndexing (c :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT)))
                                                                                                       <*> pure (bool32ToBool (vkShaderStorageTexelBufferArrayDynamicIndexing (c :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT)))
                                                                                                       <*> pure (bool32ToBool (vkShaderUniformBufferArrayNonUniformIndexing (c :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT)))
                                                                                                       <*> pure (bool32ToBool (vkShaderSampledImageArrayNonUniformIndexing (c :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT)))
                                                                                                       <*> pure (bool32ToBool (vkShaderStorageBufferArrayNonUniformIndexing (c :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT)))
                                                                                                       <*> pure (bool32ToBool (vkShaderStorageImageArrayNonUniformIndexing (c :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT)))
                                                                                                       <*> pure (bool32ToBool (vkShaderInputAttachmentArrayNonUniformIndexing (c :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT)))
                                                                                                       <*> pure (bool32ToBool (vkShaderUniformTexelBufferArrayNonUniformIndexing (c :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT)))
                                                                                                       <*> pure (bool32ToBool (vkShaderStorageTexelBufferArrayNonUniformIndexing (c :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT)))
                                                                                                       <*> pure (bool32ToBool (vkDescriptorBindingUniformBufferUpdateAfterBind (c :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT)))
                                                                                                       <*> pure (bool32ToBool (vkDescriptorBindingSampledImageUpdateAfterBind (c :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT)))
                                                                                                       <*> pure (bool32ToBool (vkDescriptorBindingStorageImageUpdateAfterBind (c :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT)))
                                                                                                       <*> pure (bool32ToBool (vkDescriptorBindingStorageBufferUpdateAfterBind (c :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT)))
                                                                                                       <*> pure (bool32ToBool (vkDescriptorBindingUniformTexelBufferUpdateAfterBind (c :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT)))
                                                                                                       <*> pure (bool32ToBool (vkDescriptorBindingStorageTexelBufferUpdateAfterBind (c :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT)))
                                                                                                       <*> pure (bool32ToBool (vkDescriptorBindingUpdateUnusedWhilePending (c :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT)))
                                                                                                       <*> pure (bool32ToBool (vkDescriptorBindingPartiallyBound (c :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT)))
                                                                                                       <*> pure (bool32ToBool (vkDescriptorBindingVariableDescriptorCount (c :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT)))
                                                                                                       <*> pure (bool32ToBool (vkRuntimeDescriptorArray (c :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT)))
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
-- No documentation found for TopLevel "PhysicalDeviceDescriptorIndexingPropertiesEXT"
data PhysicalDeviceDescriptorIndexingPropertiesEXT = PhysicalDeviceDescriptorIndexingPropertiesEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "maxUpdateAfterBindDescriptorsInAllPools"
  vkMaxUpdateAfterBindDescriptorsInAllPools :: Word32
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "shaderUniformBufferArrayNonUniformIndexingNative"
  vkShaderUniformBufferArrayNonUniformIndexingNative :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "shaderSampledImageArrayNonUniformIndexingNative"
  vkShaderSampledImageArrayNonUniformIndexingNative :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "shaderStorageBufferArrayNonUniformIndexingNative"
  vkShaderStorageBufferArrayNonUniformIndexingNative :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "shaderStorageImageArrayNonUniformIndexingNative"
  vkShaderStorageImageArrayNonUniformIndexingNative :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "shaderInputAttachmentArrayNonUniformIndexingNative"
  vkShaderInputAttachmentArrayNonUniformIndexingNative :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "robustBufferAccessUpdateAfterBind"
  vkRobustBufferAccessUpdateAfterBind :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "quadDivergentImplicitLod"
  vkQuadDivergentImplicitLod :: Bool
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "maxPerStageDescriptorUpdateAfterBindSamplers"
  vkMaxPerStageDescriptorUpdateAfterBindSamplers :: Word32
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "maxPerStageDescriptorUpdateAfterBindUniformBuffers"
  vkMaxPerStageDescriptorUpdateAfterBindUniformBuffers :: Word32
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "maxPerStageDescriptorUpdateAfterBindStorageBuffers"
  vkMaxPerStageDescriptorUpdateAfterBindStorageBuffers :: Word32
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "maxPerStageDescriptorUpdateAfterBindSampledImages"
  vkMaxPerStageDescriptorUpdateAfterBindSampledImages :: Word32
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "maxPerStageDescriptorUpdateAfterBindStorageImages"
  vkMaxPerStageDescriptorUpdateAfterBindStorageImages :: Word32
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "maxPerStageDescriptorUpdateAfterBindInputAttachments"
  vkMaxPerStageDescriptorUpdateAfterBindInputAttachments :: Word32
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "maxPerStageUpdateAfterBindResources"
  vkMaxPerStageUpdateAfterBindResources :: Word32
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "maxDescriptorSetUpdateAfterBindSamplers"
  vkMaxDescriptorSetUpdateAfterBindSamplers :: Word32
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "maxDescriptorSetUpdateAfterBindUniformBuffers"
  vkMaxDescriptorSetUpdateAfterBindUniformBuffers :: Word32
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "maxDescriptorSetUpdateAfterBindUniformBuffersDynamic"
  vkMaxDescriptorSetUpdateAfterBindUniformBuffersDynamic :: Word32
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "maxDescriptorSetUpdateAfterBindStorageBuffers"
  vkMaxDescriptorSetUpdateAfterBindStorageBuffers :: Word32
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "maxDescriptorSetUpdateAfterBindStorageBuffersDynamic"
  vkMaxDescriptorSetUpdateAfterBindStorageBuffersDynamic :: Word32
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "maxDescriptorSetUpdateAfterBindSampledImages"
  vkMaxDescriptorSetUpdateAfterBindSampledImages :: Word32
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "maxDescriptorSetUpdateAfterBindStorageImages"
  vkMaxDescriptorSetUpdateAfterBindStorageImages :: Word32
  , -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "maxDescriptorSetUpdateAfterBindInputAttachments"
  vkMaxDescriptorSetUpdateAfterBindInputAttachments :: Word32
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceDescriptorIndexingPropertiesEXT :: PhysicalDeviceDescriptorIndexingPropertiesEXT -> (VkPhysicalDeviceDescriptorIndexingPropertiesEXT -> IO a) -> IO a
withCStructPhysicalDeviceDescriptorIndexingPropertiesEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceDescriptorIndexingPropertiesEXT)) (\pPNext -> cont (VkPhysicalDeviceDescriptorIndexingPropertiesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT pPNext (vkMaxUpdateAfterBindDescriptorsInAllPools (from :: PhysicalDeviceDescriptorIndexingPropertiesEXT)) (boolToBool32 (vkShaderUniformBufferArrayNonUniformIndexingNative (from :: PhysicalDeviceDescriptorIndexingPropertiesEXT))) (boolToBool32 (vkShaderSampledImageArrayNonUniformIndexingNative (from :: PhysicalDeviceDescriptorIndexingPropertiesEXT))) (boolToBool32 (vkShaderStorageBufferArrayNonUniformIndexingNative (from :: PhysicalDeviceDescriptorIndexingPropertiesEXT))) (boolToBool32 (vkShaderStorageImageArrayNonUniformIndexingNative (from :: PhysicalDeviceDescriptorIndexingPropertiesEXT))) (boolToBool32 (vkShaderInputAttachmentArrayNonUniformIndexingNative (from :: PhysicalDeviceDescriptorIndexingPropertiesEXT))) (boolToBool32 (vkRobustBufferAccessUpdateAfterBind (from :: PhysicalDeviceDescriptorIndexingPropertiesEXT))) (boolToBool32 (vkQuadDivergentImplicitLod (from :: PhysicalDeviceDescriptorIndexingPropertiesEXT))) (vkMaxPerStageDescriptorUpdateAfterBindSamplers (from :: PhysicalDeviceDescriptorIndexingPropertiesEXT)) (vkMaxPerStageDescriptorUpdateAfterBindUniformBuffers (from :: PhysicalDeviceDescriptorIndexingPropertiesEXT)) (vkMaxPerStageDescriptorUpdateAfterBindStorageBuffers (from :: PhysicalDeviceDescriptorIndexingPropertiesEXT)) (vkMaxPerStageDescriptorUpdateAfterBindSampledImages (from :: PhysicalDeviceDescriptorIndexingPropertiesEXT)) (vkMaxPerStageDescriptorUpdateAfterBindStorageImages (from :: PhysicalDeviceDescriptorIndexingPropertiesEXT)) (vkMaxPerStageDescriptorUpdateAfterBindInputAttachments (from :: PhysicalDeviceDescriptorIndexingPropertiesEXT)) (vkMaxPerStageUpdateAfterBindResources (from :: PhysicalDeviceDescriptorIndexingPropertiesEXT)) (vkMaxDescriptorSetUpdateAfterBindSamplers (from :: PhysicalDeviceDescriptorIndexingPropertiesEXT)) (vkMaxDescriptorSetUpdateAfterBindUniformBuffers (from :: PhysicalDeviceDescriptorIndexingPropertiesEXT)) (vkMaxDescriptorSetUpdateAfterBindUniformBuffersDynamic (from :: PhysicalDeviceDescriptorIndexingPropertiesEXT)) (vkMaxDescriptorSetUpdateAfterBindStorageBuffers (from :: PhysicalDeviceDescriptorIndexingPropertiesEXT)) (vkMaxDescriptorSetUpdateAfterBindStorageBuffersDynamic (from :: PhysicalDeviceDescriptorIndexingPropertiesEXT)) (vkMaxDescriptorSetUpdateAfterBindSampledImages (from :: PhysicalDeviceDescriptorIndexingPropertiesEXT)) (vkMaxDescriptorSetUpdateAfterBindStorageImages (from :: PhysicalDeviceDescriptorIndexingPropertiesEXT)) (vkMaxDescriptorSetUpdateAfterBindInputAttachments (from :: PhysicalDeviceDescriptorIndexingPropertiesEXT))))
fromCStructPhysicalDeviceDescriptorIndexingPropertiesEXT :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT -> IO PhysicalDeviceDescriptorIndexingPropertiesEXT
fromCStructPhysicalDeviceDescriptorIndexingPropertiesEXT c = PhysicalDeviceDescriptorIndexingPropertiesEXT <$> -- Univalued Member elided
                                                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT)))
                                                                                                           <*> pure (vkMaxUpdateAfterBindDescriptorsInAllPools (c :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                                                                                                           <*> pure (bool32ToBool (vkShaderUniformBufferArrayNonUniformIndexingNative (c :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT)))
                                                                                                           <*> pure (bool32ToBool (vkShaderSampledImageArrayNonUniformIndexingNative (c :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT)))
                                                                                                           <*> pure (bool32ToBool (vkShaderStorageBufferArrayNonUniformIndexingNative (c :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT)))
                                                                                                           <*> pure (bool32ToBool (vkShaderStorageImageArrayNonUniformIndexingNative (c :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT)))
                                                                                                           <*> pure (bool32ToBool (vkShaderInputAttachmentArrayNonUniformIndexingNative (c :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT)))
                                                                                                           <*> pure (bool32ToBool (vkRobustBufferAccessUpdateAfterBind (c :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT)))
                                                                                                           <*> pure (bool32ToBool (vkQuadDivergentImplicitLod (c :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT)))
                                                                                                           <*> pure (vkMaxPerStageDescriptorUpdateAfterBindSamplers (c :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                                                                                                           <*> pure (vkMaxPerStageDescriptorUpdateAfterBindUniformBuffers (c :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                                                                                                           <*> pure (vkMaxPerStageDescriptorUpdateAfterBindStorageBuffers (c :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                                                                                                           <*> pure (vkMaxPerStageDescriptorUpdateAfterBindSampledImages (c :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                                                                                                           <*> pure (vkMaxPerStageDescriptorUpdateAfterBindStorageImages (c :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                                                                                                           <*> pure (vkMaxPerStageDescriptorUpdateAfterBindInputAttachments (c :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                                                                                                           <*> pure (vkMaxPerStageUpdateAfterBindResources (c :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                                                                                                           <*> pure (vkMaxDescriptorSetUpdateAfterBindSamplers (c :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                                                                                                           <*> pure (vkMaxDescriptorSetUpdateAfterBindUniformBuffers (c :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                                                                                                           <*> pure (vkMaxDescriptorSetUpdateAfterBindUniformBuffersDynamic (c :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                                                                                                           <*> pure (vkMaxDescriptorSetUpdateAfterBindStorageBuffers (c :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                                                                                                           <*> pure (vkMaxDescriptorSetUpdateAfterBindStorageBuffersDynamic (c :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                                                                                                           <*> pure (vkMaxDescriptorSetUpdateAfterBindSampledImages (c :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                                                                                                           <*> pure (vkMaxDescriptorSetUpdateAfterBindStorageImages (c :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                                                                                                           <*> pure (vkMaxDescriptorSetUpdateAfterBindInputAttachments (c :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
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
