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

import Data.Function
  ( (&)
  )
import Data.Maybe
  ( maybe
  )
import Data.String
  ( IsString
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
  , pattern VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT
  , pattern VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT
  , pattern VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT
  , pattern VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT
  , pattern VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
  , pattern VK_EXT_DESCRIPTOR_INDEXING_SPEC_VERSION
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


-- | VkDescriptorBindingFlagBitsEXT - Bitmask specifying descriptor set
-- layout binding properties
--
-- = Description
--
-- __Note__
--
-- Note that while
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT'
-- and
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT'
-- both involve updates to descriptor sets after they are bound,
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT'
-- is a weaker requirement since it is only about descriptors that are not
-- used, whereas
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT'
-- requires the implementation to observe updates to descriptors that are
-- used.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkDescriptorBindingFlagsEXT'
type DescriptorBindingFlagBitsEXT = VkDescriptorBindingFlagBitsEXT


{-# complete DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT, DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT, DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT, DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT :: DescriptorBindingFlagBitsEXT #-}


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT'
-- indicates that if descriptors in this binding are updated between when
-- the descriptor set is bound in a command buffer and when that command
-- buffer is submitted to a queue, then the submission will use the most
-- recently set descriptors for this binding and the updates do not
-- invalidate the command buffer. Descriptor bindings created with this
-- flag are also partially exempt from the external synchronization
-- requirement in
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkUpdateDescriptorSets'. They
-- /can/ be updated concurrently with the set being bound to a command
-- buffer in another thread, but not concurrently with the set being reset
-- or freed.
pattern DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT :: (a ~ DescriptorBindingFlagBitsEXT) => a
pattern DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT = VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT'
-- indicates that descriptors in this binding /can/ be updated after a
-- command buffer has bound this descriptor set, or while a command buffer
-- that uses this descriptor set is pending execution, as long as the
-- descriptors that are updated are not used by those command buffers. If
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT'
-- is also set, then descriptors /can/ be updated as long as they are not
-- dynamically used by any shader invocations. If
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT'
-- is not set, then descriptors /can/ be updated as long as they are not
-- statically used by any shader invocations.
pattern DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT :: (a ~ DescriptorBindingFlagBitsEXT) => a
pattern DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT = VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT'
-- indicates that descriptors in this binding that are not /dynamically
-- used/ need not contain valid descriptors at the time the descriptors are
-- consumed. A descriptor is dynamically used if any shader invocation
-- executes an instruction that performs any memory access using the
-- descriptor.
pattern DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT :: (a ~ DescriptorBindingFlagBitsEXT) => a
pattern DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT = VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT'
-- indicates that this descriptor binding has a variable size that will be
-- specified when a descriptor set is allocated using this layout. The
-- value of @descriptorCount@ is treated as an upper bound on the size of
-- the binding. This /must/ only be used for the last binding in the
-- descriptor set layout (i.e. the binding with the largest value of
-- @binding@). For the purposes of counting against limits such as
-- @maxDescriptorSet@* and @maxPerStageDescriptor@*, the full value of
-- @descriptorCount@ is counted .
pattern DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT :: (a ~ DescriptorBindingFlagBitsEXT) => a
pattern DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT = VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT

-- | VkDescriptorBindingFlagsEXT - Bitmask of VkDescriptorBindingFlagBitsEXT
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkDescriptorBindingFlagsEXT'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkDescriptorBindingFlagBitsEXT'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkDescriptorBindingFlagBitsEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkDescriptorSetLayoutBindingFlagsCreateInfoEXT'
type DescriptorBindingFlagsEXT = DescriptorBindingFlagBitsEXT


-- | VkDescriptorSetLayoutBindingFlagsCreateInfoEXT - Structure specifying
-- creation flags for descriptor set layout bindings
--
-- = Description
--
-- If @bindingCount@ is zero or if this structure is not in the @pNext@
-- chain, the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkDescriptorBindingFlagsEXT'
-- for each descriptor set layout binding is considered to be zero.
-- Otherwise, the descriptor set layout binding at
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutCreateInfo'::@pBindings@[i]
-- uses the flags in @pBindingFlags@[i].
--
-- == Valid Usage
--
-- -   If @bindingCount@ is not zero, @bindingCount@ /must/ equal
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutCreateInfo'::@bindingCount@
--
-- -   If an element of @pBindingFlags@ includes
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT',
--     then all other elements of
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutCreateInfo'::@pBindings@
--     /must/ have a smaller value of @binding@
--
-- -   If
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkPhysicalDeviceDescriptorIndexingFeaturesEXT'::@descriptorBindingUniformBufferUpdateAfterBind@
--     is not enabled, all bindings with descriptor type
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER'
--     /must/ not use
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT'
--
-- -   If
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkPhysicalDeviceDescriptorIndexingFeaturesEXT'::@descriptorBindingSampledImageUpdateAfterBind@
--     is not enabled, all bindings with descriptor type
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLER',
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     or
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE'
--     /must/ not use
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT'
--
-- -   If
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkPhysicalDeviceDescriptorIndexingFeaturesEXT'::@descriptorBindingStorageImageUpdateAfterBind@
--     is not enabled, all bindings with descriptor type
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_IMAGE'
--     /must/ not use
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT'
--
-- -   If
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkPhysicalDeviceDescriptorIndexingFeaturesEXT'::@descriptorBindingStorageBufferUpdateAfterBind@
--     is not enabled, all bindings with descriptor type
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER'
--     /must/ not use
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT'
--
-- -   If
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkPhysicalDeviceDescriptorIndexingFeaturesEXT'::@descriptorBindingUniformTexelBufferUpdateAfterBind@
--     is not enabled, all bindings with descriptor type
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
--     /must/ not use
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT'
--
-- -   If
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkPhysicalDeviceDescriptorIndexingFeaturesEXT'::@descriptorBindingStorageTexelBufferUpdateAfterBind@
--     is not enabled, all bindings with descriptor type
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
--     /must/ not use
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT'
--
-- -   All bindings with descriptor type
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT',
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC',
--     or
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
--     /must/ not use
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT'
--
-- -   If
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkPhysicalDeviceDescriptorIndexingFeaturesEXT'::@descriptorBindingUpdateUnusedWhilePending@
--     is not enabled, all elements of @pBindingFlags@ /must/ not include
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT'
--
-- -   If
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkPhysicalDeviceDescriptorIndexingFeaturesEXT'::@descriptorBindingPartiallyBound@
--     is not enabled, all elements of @pBindingFlags@ /must/ not include
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT'
--
-- -   If
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkPhysicalDeviceDescriptorIndexingFeaturesEXT'::@descriptorBindingVariableDescriptorCount@
--     is not enabled, all elements of @pBindingFlags@ /must/ not include
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT'
--
-- -   If an element of @pBindingFlags@ includes
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT',
--     that element’s @descriptorType@ /must/ not be
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
--     or
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT'
--
-- -   If @bindingCount@ is not @0@, and @pBindingFlags@ is not @NULL@,
--     @pBindingFlags@ /must/ be a valid pointer to an array of
--     @bindingCount@ valid combinations of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkDescriptorBindingFlagBitsEXT'
--     values
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkDescriptorBindingFlagsEXT',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data DescriptorSetLayoutBindingFlagsCreateInfoEXT = DescriptorSetLayoutBindingFlagsCreateInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "DescriptorSetLayoutBindingFlagsCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  -- Optional length valued member elided
  , -- No documentation found for Nested "DescriptorSetLayoutBindingFlagsCreateInfoEXT" "pBindingFlags"
  bindingFlags :: Maybe (Vector DescriptorBindingFlagsEXT)
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDescriptorSetLayoutBindingFlagsCreateInfoEXT' and
-- marshal a 'DescriptorSetLayoutBindingFlagsCreateInfoEXT' into it. The 'VkDescriptorSetLayoutBindingFlagsCreateInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDescriptorSetLayoutBindingFlagsCreateInfoEXT :: DescriptorSetLayoutBindingFlagsCreateInfoEXT -> (VkDescriptorSetLayoutBindingFlagsCreateInfoEXT -> IO a) -> IO a
withCStructDescriptorSetLayoutBindingFlagsCreateInfoEXT marshalled cont = maybeWith (withVec (&)) (bindingFlags (marshalled :: DescriptorSetLayoutBindingFlagsCreateInfoEXT)) (\pPBindingFlags -> maybeWith withSomeVkStruct (next (marshalled :: DescriptorSetLayoutBindingFlagsCreateInfoEXT)) (\pPNext -> cont (VkDescriptorSetLayoutBindingFlagsCreateInfoEXT VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT pPNext (maybe 0 (fromIntegral . Data.Vector.length) (bindingFlags (marshalled :: DescriptorSetLayoutBindingFlagsCreateInfoEXT))) pPBindingFlags)))

-- | A function to read a 'VkDescriptorSetLayoutBindingFlagsCreateInfoEXT' and all additional
-- structures in the pointer chain into a 'DescriptorSetLayoutBindingFlagsCreateInfoEXT'.
fromCStructDescriptorSetLayoutBindingFlagsCreateInfoEXT :: VkDescriptorSetLayoutBindingFlagsCreateInfoEXT -> IO DescriptorSetLayoutBindingFlagsCreateInfoEXT
fromCStructDescriptorSetLayoutBindingFlagsCreateInfoEXT c = DescriptorSetLayoutBindingFlagsCreateInfoEXT <$> -- Univalued Member elided
                                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDescriptorSetLayoutBindingFlagsCreateInfoEXT)))
                                                                                                         -- Optional length valued member elided
                                                                                                         <*> maybePeek (\p -> Data.Vector.generateM (fromIntegral (vkBindingCount (c :: VkDescriptorSetLayoutBindingFlagsCreateInfoEXT))) (peekElemOff p)) (vkPBindingFlags (c :: VkDescriptorSetLayoutBindingFlagsCreateInfoEXT))

instance Zero DescriptorSetLayoutBindingFlagsCreateInfoEXT where
  zero = DescriptorSetLayoutBindingFlagsCreateInfoEXT Nothing
                                                      Nothing



-- | VkDescriptorSetVariableDescriptorCountAllocateInfoEXT - Structure
-- specifying additional allocation parameters for descriptor sets
--
-- = Description
--
-- If @descriptorSetCount@ is zero or this structure is not included in the
-- @pNext@ chain, then the variable lengths are considered to be zero.
-- Otherwise, @pDescriptorCounts@[i] is the number of descriptors in the
-- variable count descriptor binding in the corresponding descriptor set
-- layout. If
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetAllocateInfo'::@pSetLayouts@[i]
-- does not include a variable count descriptor binding, then
-- @pDescriptorCounts@[i] is ignored.
--
-- == Valid Usage
--
-- -   If @descriptorSetCount@ is not zero, @descriptorSetCount@ /must/
--     equal
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetAllocateInfo'::@descriptorSetCount@
--
-- -   If
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetAllocateInfo'::@pSetLayouts@[i]
--     has a variable descriptor count binding, then @pDescriptorCounts@[i]
--     /must/ be less than or equal to the descriptor count specified for
--     that binding when the descriptor set layout was created.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT'
--
-- -   If @descriptorSetCount@ is not @0@, @pDescriptorCounts@ /must/ be a
--     valid pointer to an array of @descriptorSetCount@ @uint32_t@ values
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data DescriptorSetVariableDescriptorCountAllocateInfoEXT = DescriptorSetVariableDescriptorCountAllocateInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "DescriptorSetVariableDescriptorCountAllocateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "DescriptorSetVariableDescriptorCountAllocateInfoEXT" "pDescriptorCounts"
  descriptorCounts :: Vector Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDescriptorSetVariableDescriptorCountAllocateInfoEXT' and
-- marshal a 'DescriptorSetVariableDescriptorCountAllocateInfoEXT' into it. The 'VkDescriptorSetVariableDescriptorCountAllocateInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDescriptorSetVariableDescriptorCountAllocateInfoEXT :: DescriptorSetVariableDescriptorCountAllocateInfoEXT -> (VkDescriptorSetVariableDescriptorCountAllocateInfoEXT -> IO a) -> IO a
withCStructDescriptorSetVariableDescriptorCountAllocateInfoEXT marshalled cont = withVec (&) (descriptorCounts (marshalled :: DescriptorSetVariableDescriptorCountAllocateInfoEXT)) (\pPDescriptorCounts -> maybeWith withSomeVkStruct (next (marshalled :: DescriptorSetVariableDescriptorCountAllocateInfoEXT)) (\pPNext -> cont (VkDescriptorSetVariableDescriptorCountAllocateInfoEXT VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT pPNext (fromIntegral (Data.Vector.length (descriptorCounts (marshalled :: DescriptorSetVariableDescriptorCountAllocateInfoEXT)))) pPDescriptorCounts)))

-- | A function to read a 'VkDescriptorSetVariableDescriptorCountAllocateInfoEXT' and all additional
-- structures in the pointer chain into a 'DescriptorSetVariableDescriptorCountAllocateInfoEXT'.
fromCStructDescriptorSetVariableDescriptorCountAllocateInfoEXT :: VkDescriptorSetVariableDescriptorCountAllocateInfoEXT -> IO DescriptorSetVariableDescriptorCountAllocateInfoEXT
fromCStructDescriptorSetVariableDescriptorCountAllocateInfoEXT c = DescriptorSetVariableDescriptorCountAllocateInfoEXT <$> -- Univalued Member elided
                                                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDescriptorSetVariableDescriptorCountAllocateInfoEXT)))
                                                                                                                       -- Length valued member elided
                                                                                                                       <*> (Data.Vector.generateM (fromIntegral (vkDescriptorSetCount (c :: VkDescriptorSetVariableDescriptorCountAllocateInfoEXT))) (peekElemOff (vkPDescriptorCounts (c :: VkDescriptorSetVariableDescriptorCountAllocateInfoEXT))))

instance Zero DescriptorSetVariableDescriptorCountAllocateInfoEXT where
  zero = DescriptorSetVariableDescriptorCountAllocateInfoEXT Nothing
                                                             Data.Vector.empty



-- | VkDescriptorSetVariableDescriptorCountLayoutSupportEXT - Structure
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
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkPhysicalDeviceDescriptorIndexingFeaturesEXT'::@descriptorBindingVariableDescriptorCount@
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
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data DescriptorSetVariableDescriptorCountLayoutSupportEXT = DescriptorSetVariableDescriptorCountLayoutSupportEXT
  { -- Univalued member elided
  -- No documentation found for Nested "DescriptorSetVariableDescriptorCountLayoutSupportEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DescriptorSetVariableDescriptorCountLayoutSupportEXT" "maxVariableDescriptorCount"
  maxVariableDescriptorCount :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDescriptorSetVariableDescriptorCountLayoutSupportEXT' and
-- marshal a 'DescriptorSetVariableDescriptorCountLayoutSupportEXT' into it. The 'VkDescriptorSetVariableDescriptorCountLayoutSupportEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDescriptorSetVariableDescriptorCountLayoutSupportEXT :: DescriptorSetVariableDescriptorCountLayoutSupportEXT -> (VkDescriptorSetVariableDescriptorCountLayoutSupportEXT -> IO a) -> IO a
withCStructDescriptorSetVariableDescriptorCountLayoutSupportEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: DescriptorSetVariableDescriptorCountLayoutSupportEXT)) (\pPNext -> cont (VkDescriptorSetVariableDescriptorCountLayoutSupportEXT VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT pPNext (maxVariableDescriptorCount (marshalled :: DescriptorSetVariableDescriptorCountLayoutSupportEXT))))

-- | A function to read a 'VkDescriptorSetVariableDescriptorCountLayoutSupportEXT' and all additional
-- structures in the pointer chain into a 'DescriptorSetVariableDescriptorCountLayoutSupportEXT'.
fromCStructDescriptorSetVariableDescriptorCountLayoutSupportEXT :: VkDescriptorSetVariableDescriptorCountLayoutSupportEXT -> IO DescriptorSetVariableDescriptorCountLayoutSupportEXT
fromCStructDescriptorSetVariableDescriptorCountLayoutSupportEXT c = DescriptorSetVariableDescriptorCountLayoutSupportEXT <$> -- Univalued Member elided
                                                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDescriptorSetVariableDescriptorCountLayoutSupportEXT)))
                                                                                                                         <*> pure (vkMaxVariableDescriptorCount (c :: VkDescriptorSetVariableDescriptorCountLayoutSupportEXT))

instance Zero DescriptorSetVariableDescriptorCountLayoutSupportEXT where
  zero = DescriptorSetVariableDescriptorCountLayoutSupportEXT Nothing
                                                              zero



-- | VkPhysicalDeviceDescriptorIndexingFeaturesEXT - Structure describing
-- descriptor indexing features that can be supported by an implementation
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkPhysicalDeviceDescriptorIndexingFeaturesEXT'
-- structure describe the following features:
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkPhysicalDeviceDescriptorIndexingFeaturesEXT'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2KHR',
-- it is filled with values indicating whether each feature is supported.
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkPhysicalDeviceDescriptorIndexingFeaturesEXT'
-- /can/ also be used in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to enable features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PhysicalDeviceDescriptorIndexingFeaturesEXT = PhysicalDeviceDescriptorIndexingFeaturesEXT
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingFeaturesEXT" "pNext"
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

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceDescriptorIndexingFeaturesEXT' and
-- marshal a 'PhysicalDeviceDescriptorIndexingFeaturesEXT' into it. The 'VkPhysicalDeviceDescriptorIndexingFeaturesEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceDescriptorIndexingFeaturesEXT :: PhysicalDeviceDescriptorIndexingFeaturesEXT -> (VkPhysicalDeviceDescriptorIndexingFeaturesEXT -> IO a) -> IO a
withCStructPhysicalDeviceDescriptorIndexingFeaturesEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceDescriptorIndexingFeaturesEXT)) (\pPNext -> cont (VkPhysicalDeviceDescriptorIndexingFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT pPNext (boolToBool32 (shaderInputAttachmentArrayDynamicIndexing (marshalled :: PhysicalDeviceDescriptorIndexingFeaturesEXT))) (boolToBool32 (shaderUniformTexelBufferArrayDynamicIndexing (marshalled :: PhysicalDeviceDescriptorIndexingFeaturesEXT))) (boolToBool32 (shaderStorageTexelBufferArrayDynamicIndexing (marshalled :: PhysicalDeviceDescriptorIndexingFeaturesEXT))) (boolToBool32 (shaderUniformBufferArrayNonUniformIndexing (marshalled :: PhysicalDeviceDescriptorIndexingFeaturesEXT))) (boolToBool32 (shaderSampledImageArrayNonUniformIndexing (marshalled :: PhysicalDeviceDescriptorIndexingFeaturesEXT))) (boolToBool32 (shaderStorageBufferArrayNonUniformIndexing (marshalled :: PhysicalDeviceDescriptorIndexingFeaturesEXT))) (boolToBool32 (shaderStorageImageArrayNonUniformIndexing (marshalled :: PhysicalDeviceDescriptorIndexingFeaturesEXT))) (boolToBool32 (shaderInputAttachmentArrayNonUniformIndexing (marshalled :: PhysicalDeviceDescriptorIndexingFeaturesEXT))) (boolToBool32 (shaderUniformTexelBufferArrayNonUniformIndexing (marshalled :: PhysicalDeviceDescriptorIndexingFeaturesEXT))) (boolToBool32 (shaderStorageTexelBufferArrayNonUniformIndexing (marshalled :: PhysicalDeviceDescriptorIndexingFeaturesEXT))) (boolToBool32 (descriptorBindingUniformBufferUpdateAfterBind (marshalled :: PhysicalDeviceDescriptorIndexingFeaturesEXT))) (boolToBool32 (descriptorBindingSampledImageUpdateAfterBind (marshalled :: PhysicalDeviceDescriptorIndexingFeaturesEXT))) (boolToBool32 (descriptorBindingStorageImageUpdateAfterBind (marshalled :: PhysicalDeviceDescriptorIndexingFeaturesEXT))) (boolToBool32 (descriptorBindingStorageBufferUpdateAfterBind (marshalled :: PhysicalDeviceDescriptorIndexingFeaturesEXT))) (boolToBool32 (descriptorBindingUniformTexelBufferUpdateAfterBind (marshalled :: PhysicalDeviceDescriptorIndexingFeaturesEXT))) (boolToBool32 (descriptorBindingStorageTexelBufferUpdateAfterBind (marshalled :: PhysicalDeviceDescriptorIndexingFeaturesEXT))) (boolToBool32 (descriptorBindingUpdateUnusedWhilePending (marshalled :: PhysicalDeviceDescriptorIndexingFeaturesEXT))) (boolToBool32 (descriptorBindingPartiallyBound (marshalled :: PhysicalDeviceDescriptorIndexingFeaturesEXT))) (boolToBool32 (descriptorBindingVariableDescriptorCount (marshalled :: PhysicalDeviceDescriptorIndexingFeaturesEXT))) (boolToBool32 (runtimeDescriptorArray (marshalled :: PhysicalDeviceDescriptorIndexingFeaturesEXT)))))

-- | A function to read a 'VkPhysicalDeviceDescriptorIndexingFeaturesEXT' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceDescriptorIndexingFeaturesEXT'.
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



-- | VkPhysicalDeviceDescriptorIndexingPropertiesEXT - Structure describing
-- descriptor indexing properties that can be supported by an
-- implementation
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkPhysicalDeviceDescriptorIndexingPropertiesEXT'
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkPhysicalDeviceDescriptorIndexingPropertiesEXT'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PhysicalDeviceDescriptorIndexingPropertiesEXT = PhysicalDeviceDescriptorIndexingPropertiesEXT
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceDescriptorIndexingPropertiesEXT" "pNext"
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

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceDescriptorIndexingPropertiesEXT' and
-- marshal a 'PhysicalDeviceDescriptorIndexingPropertiesEXT' into it. The 'VkPhysicalDeviceDescriptorIndexingPropertiesEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceDescriptorIndexingPropertiesEXT :: PhysicalDeviceDescriptorIndexingPropertiesEXT -> (VkPhysicalDeviceDescriptorIndexingPropertiesEXT -> IO a) -> IO a
withCStructPhysicalDeviceDescriptorIndexingPropertiesEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceDescriptorIndexingPropertiesEXT)) (\pPNext -> cont (VkPhysicalDeviceDescriptorIndexingPropertiesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT pPNext (maxUpdateAfterBindDescriptorsInAllPools (marshalled :: PhysicalDeviceDescriptorIndexingPropertiesEXT)) (boolToBool32 (shaderUniformBufferArrayNonUniformIndexingNative (marshalled :: PhysicalDeviceDescriptorIndexingPropertiesEXT))) (boolToBool32 (shaderSampledImageArrayNonUniformIndexingNative (marshalled :: PhysicalDeviceDescriptorIndexingPropertiesEXT))) (boolToBool32 (shaderStorageBufferArrayNonUniformIndexingNative (marshalled :: PhysicalDeviceDescriptorIndexingPropertiesEXT))) (boolToBool32 (shaderStorageImageArrayNonUniformIndexingNative (marshalled :: PhysicalDeviceDescriptorIndexingPropertiesEXT))) (boolToBool32 (shaderInputAttachmentArrayNonUniformIndexingNative (marshalled :: PhysicalDeviceDescriptorIndexingPropertiesEXT))) (boolToBool32 (robustBufferAccessUpdateAfterBind (marshalled :: PhysicalDeviceDescriptorIndexingPropertiesEXT))) (boolToBool32 (quadDivergentImplicitLod (marshalled :: PhysicalDeviceDescriptorIndexingPropertiesEXT))) (maxPerStageDescriptorUpdateAfterBindSamplers (marshalled :: PhysicalDeviceDescriptorIndexingPropertiesEXT)) (maxPerStageDescriptorUpdateAfterBindUniformBuffers (marshalled :: PhysicalDeviceDescriptorIndexingPropertiesEXT)) (maxPerStageDescriptorUpdateAfterBindStorageBuffers (marshalled :: PhysicalDeviceDescriptorIndexingPropertiesEXT)) (maxPerStageDescriptorUpdateAfterBindSampledImages (marshalled :: PhysicalDeviceDescriptorIndexingPropertiesEXT)) (maxPerStageDescriptorUpdateAfterBindStorageImages (marshalled :: PhysicalDeviceDescriptorIndexingPropertiesEXT)) (maxPerStageDescriptorUpdateAfterBindInputAttachments (marshalled :: PhysicalDeviceDescriptorIndexingPropertiesEXT)) (maxPerStageUpdateAfterBindResources (marshalled :: PhysicalDeviceDescriptorIndexingPropertiesEXT)) (maxDescriptorSetUpdateAfterBindSamplers (marshalled :: PhysicalDeviceDescriptorIndexingPropertiesEXT)) (maxDescriptorSetUpdateAfterBindUniformBuffers (marshalled :: PhysicalDeviceDescriptorIndexingPropertiesEXT)) (maxDescriptorSetUpdateAfterBindUniformBuffersDynamic (marshalled :: PhysicalDeviceDescriptorIndexingPropertiesEXT)) (maxDescriptorSetUpdateAfterBindStorageBuffers (marshalled :: PhysicalDeviceDescriptorIndexingPropertiesEXT)) (maxDescriptorSetUpdateAfterBindStorageBuffersDynamic (marshalled :: PhysicalDeviceDescriptorIndexingPropertiesEXT)) (maxDescriptorSetUpdateAfterBindSampledImages (marshalled :: PhysicalDeviceDescriptorIndexingPropertiesEXT)) (maxDescriptorSetUpdateAfterBindStorageImages (marshalled :: PhysicalDeviceDescriptorIndexingPropertiesEXT)) (maxDescriptorSetUpdateAfterBindInputAttachments (marshalled :: PhysicalDeviceDescriptorIndexingPropertiesEXT))))

-- | A function to read a 'VkPhysicalDeviceDescriptorIndexingPropertiesEXT' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceDescriptorIndexingPropertiesEXT'.
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


-- No documentation found for TopLevel "VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME"
pattern EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME = VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_DESCRIPTOR_INDEXING_SPEC_VERSION"
pattern EXT_DESCRIPTOR_INDEXING_SPEC_VERSION :: Integral a => a
pattern EXT_DESCRIPTOR_INDEXING_SPEC_VERSION = VK_EXT_DESCRIPTOR_INDEXING_SPEC_VERSION
