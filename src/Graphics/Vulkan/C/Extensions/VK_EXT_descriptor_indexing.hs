{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing
  ( VkDescriptorBindingFlagBitsEXT(..)
  , pattern VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT
  , pattern VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT
  , pattern VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT
  , pattern VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT
  , VkDescriptorBindingFlagsEXT
  , VkDescriptorSetLayoutBindingFlagsCreateInfoEXT(..)
  , VkDescriptorSetVariableDescriptorCountAllocateInfoEXT(..)
  , VkDescriptorSetVariableDescriptorCountLayoutSupportEXT(..)
  , VkPhysicalDeviceDescriptorIndexingFeaturesEXT(..)
  , VkPhysicalDeviceDescriptorIndexingPropertiesEXT(..)
  , pattern VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT
  , pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT
  , pattern VK_ERROR_FRAGMENTATION_EXT
  , pattern VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
  , pattern VK_EXT_DESCRIPTOR_INDEXING_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import GHC.Read
  ( choose
  , expectP
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  , VkFlags
  )
import Graphics.Vulkan.C.Core10.DescriptorSet
  ( VkDescriptorPoolCreateFlagBits(..)
  , VkDescriptorSetLayoutCreateFlagBits(..)
  )


-- ** VkDescriptorBindingFlagBitsEXT

-- | VkDescriptorBindingFlagBitsEXT - Bitmask specifying descriptor set
-- layout binding properties
--
-- = Description
--
-- __Note__
--
-- Note that while 'VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT' and
-- 'VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT' both involve
-- updates to descriptor sets after they are bound,
-- 'VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT' is a weaker
-- requirement since it is only about descriptors that are not used,
-- whereas 'VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT' requires the
-- implementation to observe updates to descriptors that are used.
--
-- = See Also
--
-- No cross-references are available
newtype VkDescriptorBindingFlagBitsEXT = VkDescriptorBindingFlagBitsEXT VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkDescriptorBindingFlagBitsEXT where
  showsPrec _ VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT = showString "VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT"
  showsPrec _ VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT = showString "VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT"
  showsPrec _ VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT = showString "VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT"
  showsPrec _ VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT = showString "VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT"
  showsPrec p (VkDescriptorBindingFlagBitsEXT x) = showParen (p >= 11) (showString "VkDescriptorBindingFlagBitsEXT " . showsPrec 11 x)

instance Read VkDescriptorBindingFlagBitsEXT where
  readPrec = parens ( choose [ ("VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT",           pure VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT)
                             , ("VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT", pure VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT)
                             , ("VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT",             pure VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT)
                             , ("VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT",   pure VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDescriptorBindingFlagBitsEXT")
                        v <- step readPrec
                        pure (VkDescriptorBindingFlagBitsEXT v)
                        )
                    )

-- | 'VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT' indicates that if
-- descriptors in this binding are updated between when the descriptor set
-- is bound in a command buffer and when that command buffer is submitted
-- to a queue, then the submission will use the most recently set
-- descriptors for this binding and the updates do not invalidate the
-- command buffer. Descriptor bindings created with this flag are also
-- partially exempt from the external synchronization requirement in
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_descriptor_update_template.vkUpdateDescriptorSetWithTemplateKHR'
-- and 'Graphics.Vulkan.C.Core10.DescriptorSet.vkUpdateDescriptorSets'.
-- They /can/ be updated concurrently with the set being bound to a command
-- buffer in another thread, but not concurrently with the set being reset
-- or freed.
pattern VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT :: VkDescriptorBindingFlagBitsEXT
pattern VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT = VkDescriptorBindingFlagBitsEXT 0x00000001

-- | 'VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT' indicates
-- that descriptors in this binding /can/ be updated after a command buffer
-- has bound this descriptor set, or while a command buffer that uses this
-- descriptor set is pending execution, as long as the descriptors that are
-- updated are not used by those command buffers. If
-- 'VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT' is also set, then
-- descriptors /can/ be updated as long as they are not dynamically used by
-- any shader invocations. If
-- 'VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT' is not set, then
-- descriptors /can/ be updated as long as they are not statically used by
-- any shader invocations.
pattern VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT :: VkDescriptorBindingFlagBitsEXT
pattern VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT = VkDescriptorBindingFlagBitsEXT 0x00000002

-- | 'VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT' indicates that
-- descriptors in this binding that are not /dynamically used/ need not
-- contain valid descriptors at the time the descriptors are consumed. A
-- descriptor is dynamically used if any shader invocation executes an
-- instruction that performs any memory access using the descriptor.
pattern VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT :: VkDescriptorBindingFlagBitsEXT
pattern VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT = VkDescriptorBindingFlagBitsEXT 0x00000004

-- | 'VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT' indicates that
-- this descriptor binding has a variable size that will be specified when
-- a descriptor set is allocated using this layout. The value of
-- @descriptorCount@ is treated as an upper bound on the size of the
-- binding. This /must/ only be used for the last binding in the descriptor
-- set layout (i.e. the binding with the largest value of @binding@). For
-- the purposes of counting against limits such as @maxDescriptorSet@* and
-- @maxPerStageDescriptor@*, the full value of @descriptorCount@ is counted
-- , except for descriptor bindings with a descriptor type of
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
-- where @descriptorCount@ specifies the upper bound on the byte size of
-- the binding, thus it counts against the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#limits-maxInlineUniformBlockSize maxInlineUniformBlockSize>
-- limit instead. .
pattern VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT :: VkDescriptorBindingFlagBitsEXT
pattern VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT = VkDescriptorBindingFlagBitsEXT 0x00000008

-- | VkDescriptorBindingFlagsEXT - Bitmask of VkDescriptorBindingFlagBitsEXT
--
-- = Description
--
-- 'VkDescriptorBindingFlagsEXT' is a bitmask type for setting a mask of
-- zero or more 'VkDescriptorBindingFlagBitsEXT'.
--
-- = See Also
--
-- No cross-references are available
type VkDescriptorBindingFlagsEXT = VkDescriptorBindingFlagBitsEXT

-- | VkDescriptorSetLayoutBindingFlagsCreateInfoEXT - Structure specifying
-- creation flags for descriptor set layout bindings
--
-- = Description
--
-- If @bindingCount@ is zero or if this structure is not in the @pNext@
-- chain, the 'VkDescriptorBindingFlagsEXT' for each descriptor set layout
-- binding is considered to be zero. Otherwise, the descriptor set layout
-- binding at
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutCreateInfo'::@pBindings@[i]
-- uses the flags in @pBindingFlags@[i].
--
-- == Valid Usage
--
-- -   If @bindingCount@ is not zero, @bindingCount@ /must/ equal
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutCreateInfo'::@bindingCount@
--
-- -   If
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutCreateInfo'::@flags@
--     includes
--     'Graphics.Vulkan.C.Extensions.VK_KHR_push_descriptor.VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR',
--     then all elements of @pBindingFlags@ /must/ not include
--     'VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT',
--     'VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT', or
--     'VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT'
--
-- -   If an element of @pBindingFlags@ includes
--     'VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT', then all
--     other elements of
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutCreateInfo'::@pBindings@
--     /must/ have a smaller value of @binding@
--
-- -   If
--     'VkPhysicalDeviceDescriptorIndexingFeaturesEXT'::@descriptorBindingUniformBufferUpdateAfterBind@
--     is not enabled, all bindings with descriptor type
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER'
--     /must/ not use 'VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT'
--
-- -   If
--     'VkPhysicalDeviceDescriptorIndexingFeaturesEXT'::@descriptorBindingSampledImageUpdateAfterBind@
--     is not enabled, all bindings with descriptor type
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLER',
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     or
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE'
--     /must/ not use 'VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT'
--
-- -   If
--     'VkPhysicalDeviceDescriptorIndexingFeaturesEXT'::@descriptorBindingStorageImageUpdateAfterBind@
--     is not enabled, all bindings with descriptor type
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_IMAGE'
--     /must/ not use 'VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT'
--
-- -   If
--     'VkPhysicalDeviceDescriptorIndexingFeaturesEXT'::@descriptorBindingStorageBufferUpdateAfterBind@
--     is not enabled, all bindings with descriptor type
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER'
--     /must/ not use 'VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT'
--
-- -   If
--     'VkPhysicalDeviceDescriptorIndexingFeaturesEXT'::@descriptorBindingUniformTexelBufferUpdateAfterBind@
--     is not enabled, all bindings with descriptor type
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
--     /must/ not use 'VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT'
--
-- -   If
--     'VkPhysicalDeviceDescriptorIndexingFeaturesEXT'::@descriptorBindingStorageTexelBufferUpdateAfterBind@
--     is not enabled, all bindings with descriptor type
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
--     /must/ not use 'VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT'
--
-- -   If
--     'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VkPhysicalDeviceInlineUniformBlockFeaturesEXT'::@descriptorBindingInlineUniformBlockUpdateAfterBind@
--     is not enabled, all bindings with descriptor type
--     'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
--     /must/ not use 'VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT'
--
-- -   All bindings with descriptor type
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT',
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC',
--     or
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
--     /must/ not use 'VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT'
--
-- -   If
--     'VkPhysicalDeviceDescriptorIndexingFeaturesEXT'::@descriptorBindingUpdateUnusedWhilePending@
--     is not enabled, all elements of @pBindingFlags@ /must/ not include
--     'VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT'
--
-- -   If
--     'VkPhysicalDeviceDescriptorIndexingFeaturesEXT'::@descriptorBindingPartiallyBound@
--     is not enabled, all elements of @pBindingFlags@ /must/ not include
--     'VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT'
--
-- -   If
--     'VkPhysicalDeviceDescriptorIndexingFeaturesEXT'::@descriptorBindingVariableDescriptorCount@
--     is not enabled, all elements of @pBindingFlags@ /must/ not include
--     'VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT'
--
-- -   If an element of @pBindingFlags@ includes
--     'VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT', that
--     element’s @descriptorType@ /must/ not be
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
--     or
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
--
-- Unresolved directive in
-- VkDescriptorSetLayoutBindingFlagsCreateInfoEXT.txt -
-- include::{generated}\/validity\/structs\/VkDescriptorSetLayoutBindingFlagsCreateInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkDescriptorSetLayoutBindingFlagsCreateInfoEXT = VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @bindingCount@ is zero or the number of elements in @pBindingFlags@.
  vkBindingCount :: Word32
  , -- | @pBindingFlags@ is a pointer to an array of
  -- 'VkDescriptorBindingFlagsEXT' bitfields, one for each descriptor set
  -- layout binding.
  vkPBindingFlags :: Ptr VkDescriptorBindingFlagsEXT
  }
  deriving (Eq, Show)

instance Storable VkDescriptorSetLayoutBindingFlagsCreateInfoEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkDescriptorSetLayoutBindingFlagsCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                                            <*> peek (ptr `plusPtr` 8)
                                                            <*> peek (ptr `plusPtr` 16)
                                                            <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDescriptorSetLayoutBindingFlagsCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDescriptorSetLayoutBindingFlagsCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkBindingCount (poked :: VkDescriptorSetLayoutBindingFlagsCreateInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkPBindingFlags (poked :: VkDescriptorSetLayoutBindingFlagsCreateInfoEXT))

instance Zero VkDescriptorSetLayoutBindingFlagsCreateInfoEXT where
  zero = VkDescriptorSetLayoutBindingFlagsCreateInfoEXT VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT
                                                        zero
                                                        zero
                                                        zero

-- | VkDescriptorSetVariableDescriptorCountAllocateInfoEXT - Structure
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
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
-- then @pDescriptorCounts@[i] specifies the binding’s capacity in bytes.
-- If
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
-- Unresolved directive in
-- VkDescriptorSetVariableDescriptorCountAllocateInfoEXT.txt -
-- include::{generated}\/validity\/structs\/VkDescriptorSetVariableDescriptorCountAllocateInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkDescriptorSetVariableDescriptorCountAllocateInfoEXT = VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @descriptorSetCount@ is zero or the number of elements in
  -- @pDescriptorCounts@.
  vkDescriptorSetCount :: Word32
  , -- | @pDescriptorCounts@ is an array of descriptor counts, with each member
  -- specifying the number of descriptors in a variable descriptor count
  -- binding in the corresponding descriptor set being allocated.
  vkPDescriptorCounts :: Ptr Word32
  }
  deriving (Eq, Show)

instance Storable VkDescriptorSetVariableDescriptorCountAllocateInfoEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkDescriptorSetVariableDescriptorCountAllocateInfoEXT <$> peek (ptr `plusPtr` 0)
                                                                   <*> peek (ptr `plusPtr` 8)
                                                                   <*> peek (ptr `plusPtr` 16)
                                                                   <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDescriptorSetVariableDescriptorCountAllocateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDescriptorSetVariableDescriptorCountAllocateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkDescriptorSetCount (poked :: VkDescriptorSetVariableDescriptorCountAllocateInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkPDescriptorCounts (poked :: VkDescriptorSetVariableDescriptorCountAllocateInfoEXT))

instance Zero VkDescriptorSetVariableDescriptorCountAllocateInfoEXT where
  zero = VkDescriptorSetVariableDescriptorCountAllocateInfoEXT VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT
                                                               zero
                                                               zero
                                                               zero

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
-- 'VkPhysicalDeviceDescriptorIndexingFeaturesEXT'::@descriptorBindingVariableDescriptorCount@
-- feature is not enabled, then @maxVariableDescriptorCount@ is set to
-- zero. For the purposes of this command, a variable-sized descriptor
-- binding with a @descriptorCount@ of zero is treated as if the
-- @descriptorCount@ is one, and thus the binding is not ignored and the
-- maximum descriptor count will be returned. If the layout is not
-- supported, then the value written to @maxVariableDescriptorCount@ is
-- undefined.
--
-- Unresolved directive in
-- VkDescriptorSetVariableDescriptorCountLayoutSupportEXT.txt -
-- include::{generated}\/validity\/structs\/VkDescriptorSetVariableDescriptorCountLayoutSupportEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkDescriptorSetVariableDescriptorCountLayoutSupportEXT = VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @maxVariableDescriptorCount@ indicates the maximum number of descriptors
  -- supported in the highest numbered binding of the layout, if that binding
  -- is variable-sized. If the highest numbered binding of the layout has a
  -- descriptor type of
  -- 'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
  -- then @maxVariableDescriptorCount@ indicates the maximum byte size
  -- supported for the binding, if that binding is variable-sized.
  vkMaxVariableDescriptorCount :: Word32
  }
  deriving (Eq, Show)

instance Storable VkDescriptorSetVariableDescriptorCountLayoutSupportEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDescriptorSetVariableDescriptorCountLayoutSupportEXT <$> peek (ptr `plusPtr` 0)
                                                                    <*> peek (ptr `plusPtr` 8)
                                                                    <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDescriptorSetVariableDescriptorCountLayoutSupportEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDescriptorSetVariableDescriptorCountLayoutSupportEXT))
                *> poke (ptr `plusPtr` 16) (vkMaxVariableDescriptorCount (poked :: VkDescriptorSetVariableDescriptorCountLayoutSupportEXT))

instance Zero VkDescriptorSetVariableDescriptorCountLayoutSupportEXT where
  zero = VkDescriptorSetVariableDescriptorCountLayoutSupportEXT VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT
                                                                zero
                                                                zero

-- | VkPhysicalDeviceDescriptorIndexingFeaturesEXT - Structure describing
-- descriptor indexing features that can be supported by an implementation
--
-- = Members
--
-- The members of the 'VkPhysicalDeviceDescriptorIndexingFeaturesEXT'
-- structure describe the following features:
--
-- = Description
--
-- If the 'VkPhysicalDeviceDescriptorIndexingFeaturesEXT' structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2KHR',
-- it is filled with values indicating whether each feature is supported.
-- 'VkPhysicalDeviceDescriptorIndexingFeaturesEXT' /can/ also be used in
-- the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to enable features.
--
-- Unresolved directive in
-- VkPhysicalDeviceDescriptorIndexingFeaturesEXT.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceDescriptorIndexingFeaturesEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkPhysicalDeviceDescriptorIndexingFeaturesEXT = VkPhysicalDeviceDescriptorIndexingFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- | @shaderInputAttachmentArrayDynamicIndexing@ indicates whether arrays of
  -- input attachments /can/ be indexed by dynamically uniform integer
  -- expressions in shader code. If this feature is not enabled, resources
  -- with a descriptor type of
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT'
  -- /must/ be indexed only by constant integral expressions when aggregated
  -- into arrays in shader code. This also indicates whether shader modules
  -- /can/ declare the @InputAttachmentArrayDynamicIndexingEXT@ capability.
  vkShaderInputAttachmentArrayDynamicIndexing :: VkBool32
  , -- | @shaderUniformTexelBufferArrayDynamicIndexing@ indicates whether arrays
  -- of uniform texel buffers /can/ be indexed by dynamically uniform integer
  -- expressions in shader code. If this feature is not enabled, resources
  -- with a descriptor type of
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
  -- /must/ be indexed only by constant integral expressions when aggregated
  -- into arrays in shader code. This also indicates whether shader modules
  -- /can/ declare the @UniformTexelBufferArrayDynamicIndexingEXT@
  -- capability.
  vkShaderUniformTexelBufferArrayDynamicIndexing :: VkBool32
  , -- | @shaderStorageTexelBufferArrayDynamicIndexing@ indicates whether arrays
  -- of storage texel buffers /can/ be indexed by dynamically uniform integer
  -- expressions in shader code. If this feature is not enabled, resources
  -- with a descriptor type of
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
  -- /must/ be indexed only by constant integral expressions when aggregated
  -- into arrays in shader code. This also indicates whether shader modules
  -- /can/ declare the @StorageTexelBufferArrayDynamicIndexingEXT@
  -- capability.
  vkShaderStorageTexelBufferArrayDynamicIndexing :: VkBool32
  , -- | @shaderUniformBufferArrayNonUniformIndexing@ indicates whether arrays of
  -- uniform buffers /can/ be indexed by non-uniform integer expressions in
  -- shader code. If this feature is not enabled, resources with a descriptor
  -- type of
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER'
  -- or
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
  -- /must/ not be indexed by non-uniform integer expressions when aggregated
  -- into arrays in shader code. This also indicates whether shader modules
  -- /can/ declare the @UniformBufferArrayNonUniformIndexingEXT@ capability.
  vkShaderUniformBufferArrayNonUniformIndexing :: VkBool32
  , -- | @shaderSampledImageArrayNonUniformIndexing@ indicates whether arrays of
  -- samplers or sampled images /can/ be indexed by non-uniform integer
  -- expressions in shader code. If this feature is not enabled, resources
  -- with a descriptor type of
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLER',
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
  -- or
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE'
  -- /must/ not be indexed by non-uniform integer expressions when aggregated
  -- into arrays in shader code. This also indicates whether shader modules
  -- /can/ declare the @SampledImageArrayNonUniformIndexingEXT@ capability.
  vkShaderSampledImageArrayNonUniformIndexing :: VkBool32
  , -- | @shaderStorageBufferArrayNonUniformIndexing@ indicates whether arrays of
  -- storage buffers /can/ be indexed by non-uniform integer expressions in
  -- shader code. If this feature is not enabled, resources with a descriptor
  -- type of
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER'
  -- or
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
  -- /must/ not be indexed by non-uniform integer expressions when aggregated
  -- into arrays in shader code. This also indicates whether shader modules
  -- /can/ declare the @StorageBufferArrayNonUniformIndexingEXT@ capability.
  vkShaderStorageBufferArrayNonUniformIndexing :: VkBool32
  , -- | @shaderStorageImageArrayNonUniformIndexing@ indicates whether arrays of
  -- storage images /can/ be indexed by non-uniform integer expressions in
  -- shader code. If this feature is not enabled, resources with a descriptor
  -- type of
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_IMAGE'
  -- /must/ not be indexed by non-uniform integer expressions when aggregated
  -- into arrays in shader code. This also indicates whether shader modules
  -- /can/ declare the @StorageImageArrayNonUniformIndexingEXT@ capability.
  vkShaderStorageImageArrayNonUniformIndexing :: VkBool32
  , -- | @shaderInputAttachmentArrayNonUniformIndexing@ indicates whether arrays
  -- of input attachments /can/ be indexed by non-uniform integer expressions
  -- in shader code. If this feature is not enabled, resources with a
  -- descriptor type of
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT'
  -- /must/ not be indexed by non-uniform integer expressions when aggregated
  -- into arrays in shader code. This also indicates whether shader modules
  -- /can/ declare the @InputAttachmentArrayNonUniformIndexingEXT@
  -- capability.
  vkShaderInputAttachmentArrayNonUniformIndexing :: VkBool32
  , -- | @shaderUniformTexelBufferArrayNonUniformIndexing@ indicates whether
  -- arrays of uniform texel buffers /can/ be indexed by non-uniform integer
  -- expressions in shader code. If this feature is not enabled, resources
  -- with a descriptor type of
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
  -- /must/ not be indexed by non-uniform integer expressions when aggregated
  -- into arrays in shader code. This also indicates whether shader modules
  -- /can/ declare the @UniformTexelBufferArrayNonUniformIndexingEXT@
  -- capability.
  vkShaderUniformTexelBufferArrayNonUniformIndexing :: VkBool32
  , -- | @shaderStorageTexelBufferArrayNonUniformIndexing@ indicates whether
  -- arrays of storage texel buffers /can/ be indexed by non-uniform integer
  -- expressions in shader code. If this feature is not enabled, resources
  -- with a descriptor type of
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
  -- /must/ not be indexed by non-uniform integer expressions when aggregated
  -- into arrays in shader code. This also indicates whether shader modules
  -- /can/ declare the @StorageTexelBufferArrayNonUniformIndexingEXT@
  -- capability.
  vkShaderStorageTexelBufferArrayNonUniformIndexing :: VkBool32
  , -- | @descriptorBindingUniformBufferUpdateAfterBind@ indicates whether the
  -- implementation supports updating uniform buffer descriptors after a set
  -- is bound. If this feature is not enabled,
  -- 'VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT' /must/ not be used
  -- with
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER'.
  vkDescriptorBindingUniformBufferUpdateAfterBind :: VkBool32
  , -- | @descriptorBindingSampledImageUpdateAfterBind@ indicates whether the
  -- implementation supports updating sampled image descriptors after a set
  -- is bound. If this feature is not enabled,
  -- 'VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT' /must/ not be used
  -- with
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLER',
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
  -- or
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE'.
  vkDescriptorBindingSampledImageUpdateAfterBind :: VkBool32
  , -- | @descriptorBindingStorageImageUpdateAfterBind@ indicates whether the
  -- implementation supports updating storage image descriptors after a set
  -- is bound. If this feature is not enabled,
  -- 'VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT' /must/ not be used
  -- with
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_IMAGE'.
  vkDescriptorBindingStorageImageUpdateAfterBind :: VkBool32
  , -- | @descriptorBindingStorageBufferUpdateAfterBind@ indicates whether the
  -- implementation supports updating storage buffer descriptors after a set
  -- is bound. If this feature is not enabled,
  -- 'VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT' /must/ not be used
  -- with
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER'.
  vkDescriptorBindingStorageBufferUpdateAfterBind :: VkBool32
  , -- | @descriptorBindingUniformTexelBufferUpdateAfterBind@ indicates whether
  -- the implementation supports updating uniform texel buffer descriptors
  -- after a set is bound. If this feature is not enabled,
  -- 'VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT' /must/ not be used
  -- with
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'.
  vkDescriptorBindingUniformTexelBufferUpdateAfterBind :: VkBool32
  , -- | @descriptorBindingStorageTexelBufferUpdateAfterBind@ indicates whether
  -- the implementation supports updating storage texel buffer descriptors
  -- after a set is bound. If this feature is not enabled,
  -- 'VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT' /must/ not be used
  -- with
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'.
  vkDescriptorBindingStorageTexelBufferUpdateAfterBind :: VkBool32
  , -- | @descriptorBindingUpdateUnusedWhilePending@ indicates whether the
  -- implementation supports updating descriptors while the set is in use. If
  -- this feature is not enabled,
  -- 'VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT' /must/ not
  -- be used.
  vkDescriptorBindingUpdateUnusedWhilePending :: VkBool32
  , -- | @descriptorBindingPartiallyBound@ indicates whether the implementation
  -- supports statically using a descriptor set binding in which some
  -- descriptors are not valid. If this feature is not enabled,
  -- 'VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT' /must/ not be used.
  vkDescriptorBindingPartiallyBound :: VkBool32
  , -- | @descriptorBindingVariableDescriptorCount@ indicates whether the
  -- implementation supports descriptor sets with a variable-sized last
  -- binding. If this feature is not enabled,
  -- 'VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT' /must/ not be
  -- used.
  vkDescriptorBindingVariableDescriptorCount :: VkBool32
  , -- | @runtimeDescriptorArray@ indicates whether the implementation supports
  -- the SPIR-V @RuntimeDescriptorArrayEXT@ capability. If this feature is
  -- not enabled, descriptors /must/ not be declared in runtime arrays.
  vkRuntimeDescriptorArray :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceDescriptorIndexingFeaturesEXT where
  sizeOf ~_ = 96
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceDescriptorIndexingFeaturesEXT <$> peek (ptr `plusPtr` 0)
                                                           <*> peek (ptr `plusPtr` 8)
                                                           <*> peek (ptr `plusPtr` 16)
                                                           <*> peek (ptr `plusPtr` 20)
                                                           <*> peek (ptr `plusPtr` 24)
                                                           <*> peek (ptr `plusPtr` 28)
                                                           <*> peek (ptr `plusPtr` 32)
                                                           <*> peek (ptr `plusPtr` 36)
                                                           <*> peek (ptr `plusPtr` 40)
                                                           <*> peek (ptr `plusPtr` 44)
                                                           <*> peek (ptr `plusPtr` 48)
                                                           <*> peek (ptr `plusPtr` 52)
                                                           <*> peek (ptr `plusPtr` 56)
                                                           <*> peek (ptr `plusPtr` 60)
                                                           <*> peek (ptr `plusPtr` 64)
                                                           <*> peek (ptr `plusPtr` 68)
                                                           <*> peek (ptr `plusPtr` 72)
                                                           <*> peek (ptr `plusPtr` 76)
                                                           <*> peek (ptr `plusPtr` 80)
                                                           <*> peek (ptr `plusPtr` 84)
                                                           <*> peek (ptr `plusPtr` 88)
                                                           <*> peek (ptr `plusPtr` 92)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 16) (vkShaderInputAttachmentArrayDynamicIndexing (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 20) (vkShaderUniformTexelBufferArrayDynamicIndexing (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 24) (vkShaderStorageTexelBufferArrayDynamicIndexing (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 28) (vkShaderUniformBufferArrayNonUniformIndexing (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 32) (vkShaderSampledImageArrayNonUniformIndexing (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 36) (vkShaderStorageBufferArrayNonUniformIndexing (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 40) (vkShaderStorageImageArrayNonUniformIndexing (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 44) (vkShaderInputAttachmentArrayNonUniformIndexing (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 48) (vkShaderUniformTexelBufferArrayNonUniformIndexing (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 52) (vkShaderStorageTexelBufferArrayNonUniformIndexing (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 56) (vkDescriptorBindingUniformBufferUpdateAfterBind (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 60) (vkDescriptorBindingSampledImageUpdateAfterBind (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 64) (vkDescriptorBindingStorageImageUpdateAfterBind (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 68) (vkDescriptorBindingStorageBufferUpdateAfterBind (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 72) (vkDescriptorBindingUniformTexelBufferUpdateAfterBind (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 76) (vkDescriptorBindingStorageTexelBufferUpdateAfterBind (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 80) (vkDescriptorBindingUpdateUnusedWhilePending (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 84) (vkDescriptorBindingPartiallyBound (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 88) (vkDescriptorBindingVariableDescriptorCount (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 92) (vkRuntimeDescriptorArray (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))

instance Zero VkPhysicalDeviceDescriptorIndexingFeaturesEXT where
  zero = VkPhysicalDeviceDescriptorIndexingFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT
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

-- | VkPhysicalDeviceDescriptorIndexingPropertiesEXT - Structure describing
-- descriptor indexing properties that can be supported by an
-- implementation
--
-- = Members
--
-- The members of the 'VkPhysicalDeviceDescriptorIndexingPropertiesEXT'
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- If the 'VkPhysicalDeviceDescriptorIndexingPropertiesEXT' structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- Unresolved directive in
-- VkPhysicalDeviceDescriptorIndexingPropertiesEXT.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceDescriptorIndexingPropertiesEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkPhysicalDeviceDescriptorIndexingPropertiesEXT = VkPhysicalDeviceDescriptorIndexingPropertiesEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @maxUpdateAfterBindDescriptorsInAllPools@ is the maximum number of
  -- descriptors (summed over all descriptor types) that /can/ be created
  -- across all pools that are created with the
  -- 'VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT' bit set. Pool
  -- creation /may/ fail when this limit is exceeded, or when the space this
  -- limit represents is unable to satisfy a pool creation due to
  -- fragmentation.
  vkMaxUpdateAfterBindDescriptorsInAllPools :: Word32
  , -- | @shaderUniformBufferArrayNonUniformIndexingNative@ is a boolean value
  -- indicating whether uniform buffer descriptors natively support
  -- nonuniform indexing. If this is
  -- 'Graphics.Vulkan.C.Core10.Core.VK_FALSE', then a single dynamic instance
  -- of an instruction that nonuniformly indexes an array of uniform buffers
  -- /may/ execute multiple times in order to access all the descriptors.
  vkShaderUniformBufferArrayNonUniformIndexingNative :: VkBool32
  , -- | @shaderSampledImageArrayNonUniformIndexingNative@ is a boolean value
  -- indicating whether sampler and image descriptors natively support
  -- nonuniform indexing. If this is
  -- 'Graphics.Vulkan.C.Core10.Core.VK_FALSE', then a single dynamic instance
  -- of an instruction that nonuniformly indexes an array of samplers or
  -- images /may/ execute multiple times in order to access all the
  -- descriptors.
  vkShaderSampledImageArrayNonUniformIndexingNative :: VkBool32
  , -- | @shaderStorageBufferArrayNonUniformIndexingNative@ is a boolean value
  -- indicating whether storage buffer descriptors natively support
  -- nonuniform indexing. If this is
  -- 'Graphics.Vulkan.C.Core10.Core.VK_FALSE', then a single dynamic instance
  -- of an instruction that nonuniformly indexes an array of storage buffers
  -- /may/ execute multiple times in order to access all the descriptors.
  vkShaderStorageBufferArrayNonUniformIndexingNative :: VkBool32
  , -- | @shaderStorageImageArrayNonUniformIndexingNative@ is a boolean value
  -- indicating whether storage image descriptors natively support nonuniform
  -- indexing. If this is 'Graphics.Vulkan.C.Core10.Core.VK_FALSE', then a
  -- single dynamic instance of an instruction that nonuniformly indexes an
  -- array of storage images /may/ execute multiple times in order to access
  -- all the descriptors.
  vkShaderStorageImageArrayNonUniformIndexingNative :: VkBool32
  , -- | @shaderInputAttachmentArrayNonUniformIndexingNative@ is a boolean value
  -- indicating whether input attachment descriptors natively support
  -- nonuniform indexing. If this is
  -- 'Graphics.Vulkan.C.Core10.Core.VK_FALSE', then a single dynamic instance
  -- of an instruction that nonuniformly indexes an array of input
  -- attachments /may/ execute multiple times in order to access all the
  -- descriptors.
  vkShaderInputAttachmentArrayNonUniformIndexingNative :: VkBool32
  , -- | @robustBufferAccessUpdateAfterBind@ is a boolean value indicating
  -- whether
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
  -- /can/ be enabled in a device simultaneously with
  -- @descriptorBindingUniformBufferUpdateAfterBind@,
  -- @descriptorBindingStorageBufferUpdateAfterBind@,
  -- @descriptorBindingUniformTexelBufferUpdateAfterBind@, and\/or
  -- @descriptorBindingStorageTexelBufferUpdateAfterBind@. If this is
  -- 'Graphics.Vulkan.C.Core10.Core.VK_FALSE', then either
  -- @robustBufferAccess@ /must/ be disabled or all of these
  -- update-after-bind features /must/ be disabled.
  vkRobustBufferAccessUpdateAfterBind :: VkBool32
  , -- | @quadDivergentImplicitLod@ is a boolean value indicating whether
  -- implicit level of detail calculations for image operations have
  -- well-defined results when the image and\/or sampler objects used for the
  -- instruction are not uniform within a quad. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#textures-derivative-image-operations Derivative Image Operations>.
  vkQuadDivergentImplicitLod :: VkBool32
  , -- | @maxPerStageDescriptorUpdateAfterBindSamplers@ is similar to
  -- @maxPerStageDescriptorSamplers@ but counts descriptors from descriptor
  -- sets created with or without the
  -- 'VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT' bit
  -- set.
  vkMaxPerStageDescriptorUpdateAfterBindSamplers :: Word32
  , -- | @maxPerStageDescriptorUpdateAfterBindUniformBuffers@ is similar to
  -- @maxPerStageDescriptorUniformBuffers@ but counts descriptors from
  -- descriptor sets created with or without the
  -- 'VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT' bit
  -- set.
  vkMaxPerStageDescriptorUpdateAfterBindUniformBuffers :: Word32
  , -- | @maxPerStageDescriptorUpdateAfterBindStorageBuffers@ is similar to
  -- @maxPerStageDescriptorStorageBuffers@ but counts descriptors from
  -- descriptor sets created with or without the
  -- 'VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT' bit
  -- set.
  vkMaxPerStageDescriptorUpdateAfterBindStorageBuffers :: Word32
  , -- | @maxPerStageDescriptorUpdateAfterBindSampledImages@ is similar to
  -- @maxPerStageDescriptorSampledImages@ but counts descriptors from
  -- descriptor sets created with or without the
  -- 'VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT' bit
  -- set.
  vkMaxPerStageDescriptorUpdateAfterBindSampledImages :: Word32
  , -- | @maxPerStageDescriptorUpdateAfterBindStorageImages@ is similar to
  -- @maxPerStageDescriptorStorageImages@ but counts descriptors from
  -- descriptor sets created with or without the
  -- 'VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT' bit
  -- set.
  vkMaxPerStageDescriptorUpdateAfterBindStorageImages :: Word32
  , -- | @maxPerStageDescriptorUpdateAfterBindInputAttachments@ is similar to
  -- @maxPerStageDescriptorInputAttachments@ but counts descriptors from
  -- descriptor sets created with or without the
  -- 'VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT' bit
  -- set.
  vkMaxPerStageDescriptorUpdateAfterBindInputAttachments :: Word32
  , -- | @maxPerStageUpdateAfterBindResources@ is similar to
  -- @maxPerStageResources@ but counts descriptors from descriptor sets
  -- created with or without the
  -- 'VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT' bit
  -- set.
  vkMaxPerStageUpdateAfterBindResources :: Word32
  , -- | @maxDescriptorSetUpdateAfterBindSamplers@ is similar to
  -- @maxDescriptorSetSamplers@ but counts descriptors from descriptor sets
  -- created with or without the
  -- 'VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT' bit
  -- set.
  vkMaxDescriptorSetUpdateAfterBindSamplers :: Word32
  , -- | @maxDescriptorSetUpdateAfterBindUniformBuffers@ is similar to
  -- @maxDescriptorSetUniformBuffers@ but counts descriptors from descriptor
  -- sets created with or without the
  -- 'VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT' bit
  -- set.
  vkMaxDescriptorSetUpdateAfterBindUniformBuffers :: Word32
  , -- | @maxDescriptorSetUpdateAfterBindUniformBuffersDynamic@ is similar to
  -- @maxDescriptorSetUniformBuffersDynamic@ but counts descriptors from
  -- descriptor sets created with or without the
  -- 'VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT' bit
  -- set.
  vkMaxDescriptorSetUpdateAfterBindUniformBuffersDynamic :: Word32
  , -- | @maxDescriptorSetUpdateAfterBindStorageBuffers@ is similar to
  -- @maxDescriptorSetStorageBuffers@ but counts descriptors from descriptor
  -- sets created with or without the
  -- 'VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT' bit
  -- set.
  vkMaxDescriptorSetUpdateAfterBindStorageBuffers :: Word32
  , -- | @maxDescriptorSetUpdateAfterBindStorageBuffersDynamic@ is similar to
  -- @maxDescriptorSetStorageBuffersDynamic@ but counts descriptors from
  -- descriptor sets created with or without the
  -- 'VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT' bit
  -- set.
  vkMaxDescriptorSetUpdateAfterBindStorageBuffersDynamic :: Word32
  , -- | @maxDescriptorSetUpdateAfterBindSampledImages@ is similar to
  -- @maxDescriptorSetSampledImages@ but counts descriptors from descriptor
  -- sets created with or without the
  -- 'VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT' bit
  -- set.
  vkMaxDescriptorSetUpdateAfterBindSampledImages :: Word32
  , -- | @maxDescriptorSetUpdateAfterBindStorageImages@ is similar to
  -- @maxDescriptorSetStorageImages@ but counts descriptors from descriptor
  -- sets created with or without the
  -- 'VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT' bit
  -- set.
  vkMaxDescriptorSetUpdateAfterBindStorageImages :: Word32
  , -- | @maxDescriptorSetUpdateAfterBindInputAttachments@ is similar to
  -- @maxDescriptorSetInputAttachments@ but counts descriptors from
  -- descriptor sets created with or without the
  -- 'VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT' bit
  -- set.
  vkMaxDescriptorSetUpdateAfterBindInputAttachments :: Word32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceDescriptorIndexingPropertiesEXT where
  sizeOf ~_ = 112
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceDescriptorIndexingPropertiesEXT <$> peek (ptr `plusPtr` 0)
                                                             <*> peek (ptr `plusPtr` 8)
                                                             <*> peek (ptr `plusPtr` 16)
                                                             <*> peek (ptr `plusPtr` 20)
                                                             <*> peek (ptr `plusPtr` 24)
                                                             <*> peek (ptr `plusPtr` 28)
                                                             <*> peek (ptr `plusPtr` 32)
                                                             <*> peek (ptr `plusPtr` 36)
                                                             <*> peek (ptr `plusPtr` 40)
                                                             <*> peek (ptr `plusPtr` 44)
                                                             <*> peek (ptr `plusPtr` 48)
                                                             <*> peek (ptr `plusPtr` 52)
                                                             <*> peek (ptr `plusPtr` 56)
                                                             <*> peek (ptr `plusPtr` 60)
                                                             <*> peek (ptr `plusPtr` 64)
                                                             <*> peek (ptr `plusPtr` 68)
                                                             <*> peek (ptr `plusPtr` 72)
                                                             <*> peek (ptr `plusPtr` 76)
                                                             <*> peek (ptr `plusPtr` 80)
                                                             <*> peek (ptr `plusPtr` 84)
                                                             <*> peek (ptr `plusPtr` 88)
                                                             <*> peek (ptr `plusPtr` 92)
                                                             <*> peek (ptr `plusPtr` 96)
                                                             <*> peek (ptr `plusPtr` 100)
                                                             <*> peek (ptr `plusPtr` 104)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 16) (vkMaxUpdateAfterBindDescriptorsInAllPools (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 20) (vkShaderUniformBufferArrayNonUniformIndexingNative (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 24) (vkShaderSampledImageArrayNonUniformIndexingNative (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 28) (vkShaderStorageBufferArrayNonUniformIndexingNative (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 32) (vkShaderStorageImageArrayNonUniformIndexingNative (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 36) (vkShaderInputAttachmentArrayNonUniformIndexingNative (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 40) (vkRobustBufferAccessUpdateAfterBind (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 44) (vkQuadDivergentImplicitLod (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 48) (vkMaxPerStageDescriptorUpdateAfterBindSamplers (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 52) (vkMaxPerStageDescriptorUpdateAfterBindUniformBuffers (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 56) (vkMaxPerStageDescriptorUpdateAfterBindStorageBuffers (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 60) (vkMaxPerStageDescriptorUpdateAfterBindSampledImages (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 64) (vkMaxPerStageDescriptorUpdateAfterBindStorageImages (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 68) (vkMaxPerStageDescriptorUpdateAfterBindInputAttachments (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 72) (vkMaxPerStageUpdateAfterBindResources (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 76) (vkMaxDescriptorSetUpdateAfterBindSamplers (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 80) (vkMaxDescriptorSetUpdateAfterBindUniformBuffers (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 84) (vkMaxDescriptorSetUpdateAfterBindUniformBuffersDynamic (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 88) (vkMaxDescriptorSetUpdateAfterBindStorageBuffers (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 92) (vkMaxDescriptorSetUpdateAfterBindStorageBuffersDynamic (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 96) (vkMaxDescriptorSetUpdateAfterBindSampledImages (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 100) (vkMaxDescriptorSetUpdateAfterBindStorageImages (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 104) (vkMaxDescriptorSetUpdateAfterBindInputAttachments (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))

instance Zero VkPhysicalDeviceDescriptorIndexingPropertiesEXT where
  zero = VkPhysicalDeviceDescriptorIndexingPropertiesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT
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
                                                         zero

-- | 'VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT' specifies that
-- descriptor sets allocated from this pool /can/ include bindings with the
-- 'VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT' bit set. It is valid
-- to allocate descriptor sets that have bindings that do not set the
-- 'VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT' bit from a pool that
-- has 'VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT' set.
pattern VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT :: VkDescriptorPoolCreateFlagBits
pattern VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT = VkDescriptorPoolCreateFlagBits 0x00000002

-- | 'VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT'
-- specifies that descriptor sets using this layout /must/ be allocated
-- from a descriptor pool created with the
-- 'VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT' bit set.
-- Descriptor set layouts created with this bit set have alternate limits
-- for the maximum number of descriptors per-stage and per-pipeline layout.
-- The non-UpdateAfterBind limits only count descriptors in sets created
-- without this flag. The UpdateAfterBind limits count all descriptors, but
-- the limits /may/ be higher than the non-UpdateAfterBind limits.
pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT :: VkDescriptorSetLayoutCreateFlagBits
pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT = VkDescriptorSetLayoutCreateFlagBits 0x00000002

-- | 'VK_ERROR_FRAGMENTATION_EXT' A descriptor pool creation has failed due
-- to fragmentation.
pattern VK_ERROR_FRAGMENTATION_EXT :: VkResult
pattern VK_ERROR_FRAGMENTATION_EXT = VkResult (-1000161000)

-- No documentation found for TopLevel "VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME"
pattern VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME = "VK_EXT_descriptor_indexing"

-- No documentation found for TopLevel "VK_EXT_DESCRIPTOR_INDEXING_SPEC_VERSION"
pattern VK_EXT_DESCRIPTOR_INDEXING_SPEC_VERSION :: Integral a => a
pattern VK_EXT_DESCRIPTOR_INDEXING_SPEC_VERSION = 2

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT = VkStructureType 1000161000

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT = VkStructureType 1000161003

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT"
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT = VkStructureType 1000161004

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT = VkStructureType 1000161001

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT = VkStructureType 1000161002
