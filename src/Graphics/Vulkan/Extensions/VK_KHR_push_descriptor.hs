{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_push_descriptor
  ( pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR
  , pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR
  , pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR
  , pattern VK_KHR_PUSH_DESCRIPTOR_SPEC_VERSION
  , pattern VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME
  , vkCmdPushDescriptorSetKHR
  , vkCmdPushDescriptorSetWithTemplateKHR
  , VkPhysicalDevicePushDescriptorPropertiesKHR(..)
  ) where

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
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Core
  ( VkStructureType(..)
  )
import Graphics.Vulkan.Core10.DescriptorSet
  ( VkDescriptorSetLayoutCreateFlagBits(..)
  , VkWriteDescriptorSet(..)
  )
import Graphics.Vulkan.Core10.Pass
  ( VkPipelineBindPoint(..)
  )
import Graphics.Vulkan.Core10.Pipeline
  ( VkPipelineLayout
  )
import Graphics.Vulkan.Core10.Queue
  ( VkCommandBuffer
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_descriptor_update_template
  ( VkDescriptorUpdateTemplateType(..)
  , VkDescriptorUpdateTemplate
  )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR = VkStructureType 1000080000
-- | @VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR@ specifies that
-- the descriptor update template will be used for push descriptor updates
-- only.
pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR :: VkDescriptorUpdateTemplateType
pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR = VkDescriptorUpdateTemplateType 1
-- | @VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR@ specifies that
-- descriptor sets /must/ not be allocated using this layout, and
-- descriptors are instead pushed by 'vkCmdPushDescriptorSetKHR'.
pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR :: VkDescriptorSetLayoutCreateFlagBits
pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR = VkDescriptorSetLayoutCreateFlagBits 0x00000001
-- No documentation found for TopLevel "VK_KHR_PUSH_DESCRIPTOR_SPEC_VERSION"
pattern VK_KHR_PUSH_DESCRIPTOR_SPEC_VERSION :: Integral a => a
pattern VK_KHR_PUSH_DESCRIPTOR_SPEC_VERSION = 2
-- No documentation found for TopLevel "VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME"
pattern VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME = "VK_KHR_push_descriptor"
-- | vkCmdPushDescriptorSetKHR - Pushes descriptor updates into a command
-- buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer that the descriptors will be
--     recorded in.
--
-- -   @pipelineBindPoint@ is a
--     'Graphics.Vulkan.Core10.Pass.VkPipelineBindPoint' indicating whether
--     the descriptors will be used by graphics pipelines or compute
--     pipelines. There is a separate set of push descriptor bindings for
--     each of graphics and compute, so binding one does not disturb the
--     other.
--
-- -   @layout@ is a @VkPipelineLayout@ object used to program the
--     bindings.
--
-- -   @set@ is the set number of the descriptor set in the pipeline layout
--     that will be updated.
--
-- -   @descriptorWriteCount@ is the number of elements in the
--     @pDescriptorWrites@ array.
--
-- -   @pDescriptorWrites@ is a pointer to an array of
--     'Graphics.Vulkan.Core10.DescriptorSet.VkWriteDescriptorSet'
--     structures describing the descriptors to be updated.
--
-- = Description
--
-- /Push descriptors/ are a small bank of descriptors whose storage is
-- internally managed by the command buffer rather than being written into
-- a descriptor set and later bound to a command buffer. Push descriptors
-- allow for incremental updates of descriptors without managing the
-- lifetime of descriptor sets.
--
-- When a command buffer begins recording, all push descriptors have
-- undefined contents. Push descriptors /can/ be updated incrementally and
-- cause shaders to use the updated descriptors for subsequent rendering
-- commands (either compute or graphics, according to the
-- @pipelineBindPoint@) until the descriptor is overwritten, or else until
-- the set is disturbed as described in [Pipeline Layout
-- Compatibility](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility).
-- When the set is disturbed or push descriptors with a different
-- descriptor set layout are set, all push descriptors become invalid.
--
-- Valid descriptors /must/ be pushed for all bindings that any shaders in
-- a pipeline access, at the time that a draw or dispatch command is
-- recorded to execute using that pipeline. This includes immutable sampler
-- descriptors, which /must/ be pushed before they are accessed by a
-- pipeline. However, if none of the shaders in a pipeline statically use
-- certain bindings in the push descriptor set, then those descriptors need
-- not be valid.
--
-- Push descriptors do not use dynamic offsets. Instead, the corresponding
-- non-dynamic descriptor types /can/ be used and the @offset@ member of
-- 'Graphics.Vulkan.Core10.DescriptorSet.VkDescriptorBufferInfo' /can/ be
-- changed each time the descriptor is written.
--
-- Each element of @pDescriptorWrites@ is interpreted as in
-- 'Graphics.Vulkan.Core10.DescriptorSet.VkWriteDescriptorSet', except the
-- @dstSet@ member is ignored.
--
-- To push an immutable sampler, use a
-- 'Graphics.Vulkan.Core10.DescriptorSet.VkWriteDescriptorSet' with
-- @dstBinding@ and @dstArrayElement@ selecting the immutable sampler’s
-- binding. If the descriptor type is @VK_DESCRIPTOR_TYPE_SAMPLER@, the
-- @pImageInfo@ parameter is ignored and the immutable sampler is taken
-- from the push descriptor set layout in the pipeline layout. If the
-- descriptor type is @VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER@, the
-- @sampler@ member of the @pImageInfo@ parameter is ignored and the
-- immutable sampler is taken from the push descriptor set layout in the
-- pipeline layout.
--
-- == Valid Usage
--
-- -   @pipelineBindPoint@ /must/ be supported by the @commandBuffer@’s
--     parent @VkCommandPool@’s queue family
--
-- -   @set@ /must/ be less than
--     @VkPipelineLayoutCreateInfo@::@setLayoutCount@ provided when
--     @layout@ was created
--
-- -   @set@ /must/ be the unique set number in the pipeline layout that
--     uses a descriptor set layout that was created with
--     @VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @pipelineBindPoint@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Pass.VkPipelineBindPoint' value
--
-- -   @layout@ /must/ be a valid @VkPipelineLayout@ handle
--
-- -   @pDescriptorWrites@ /must/ be a valid pointer to an array of
--     @descriptorWriteCount@ valid @VkWriteDescriptorSet@ structures
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics, or compute operations
--
-- -   @descriptorWriteCount@ /must/ be greater than @0@
--
-- -   Both of @commandBuffer@, and @layout@ /must/ have been created,
--     allocated, or retrieved from the same @VkDevice@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Both                                                                                                       | Graphics                                                                                              |                                                                                                                            |
-- | Secondary                                                                                                   |                                                                                                            | Compute                                                                                               |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.Pass.VkPipelineBindPoint',
-- 'Graphics.Vulkan.Core10.Pipeline.VkPipelineLayout',
-- 'Graphics.Vulkan.Core10.DescriptorSet.VkWriteDescriptorSet'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdPushDescriptorSetKHR" vkCmdPushDescriptorSetKHR :: ("commandBuffer" ::: VkCommandBuffer) -> ("pipelineBindPoint" ::: VkPipelineBindPoint) -> ("layout" ::: VkPipelineLayout) -> ("set" ::: Word32) -> ("descriptorWriteCount" ::: Word32) -> ("pDescriptorWrites" ::: Ptr VkWriteDescriptorSet) -> IO ()
-- | vkCmdPushDescriptorSetWithTemplateKHR - Pushes descriptor updates into a
-- command buffer using a descriptor update template
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer that the descriptors will be
--     recorded in.
--
-- -   @descriptorUpdateTemplate@ A descriptor update template which
--     defines how to interpret the descriptor information in pData.
--
-- -   @layout@ is a @VkPipelineLayout@ object used to program the
--     bindings. It /must/ be compatible with the layout used to create the
--     @descriptorUpdateTemplate@ handle.
--
-- -   @set@ is the set number of the descriptor set in the pipeline layout
--     that will be updated. This /must/ be the same number used to create
--     the @descriptorUpdateTemplate@ handle.
--
-- -   @pData@ Points to memory which contains the descriptors for the
--     templated update.
--
-- == Valid Usage
--
-- -   The pipelineBindPoint specified during the creation of the
--     descriptor update template /must/ be supported by the
--     @commandBuffer@’s parent @VkCommandPool@’s queue family
--
-- -   @pData@ /must/ be a valid pointer to a memory that contains one or
--     more valid instances of
--     'Graphics.Vulkan.Core10.DescriptorSet.VkDescriptorImageInfo',
--     'Graphics.Vulkan.Core10.DescriptorSet.VkDescriptorBufferInfo', or
--     'Graphics.Vulkan.Core10.BufferView.VkBufferView' in a layout defined
--     by @descriptorUpdateTemplate@ when it was created with
--     'Graphics.Vulkan.Extensions.VK_KHR_descriptor_update_template.vkCreateDescriptorUpdateTemplateKHR'
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @descriptorUpdateTemplate@ /must/ be a valid
--     @VkDescriptorUpdateTemplate@ handle
--
-- -   @layout@ /must/ be a valid @VkPipelineLayout@ handle
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics, or compute operations
--
-- -   Each of @commandBuffer@, @descriptorUpdateTemplate@, and @layout@
--     /must/ have been created, allocated, or retrieved from the same
--     @VkDevice@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Both                                                                                                       | Graphics                                                                                              |                                                                                                                            |
-- | Secondary                                                                                                   |                                                                                                            | Compute                                                                                               |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- __API example.__
--
-- > struct AppBufferView {
-- >     VkBufferView bufferView;
-- >     uint32_t     applicationRelatedInformation;
-- > };
-- >
-- > struct AppDataStructure
-- > {
-- >     VkDescriptorImageInfo  imageInfo;          // a single image info
-- >     // ... some more application related data
-- > };
-- >
-- > const VkDescriptorUpdateTemplateEntry descriptorUpdateTemplateEntries[] =
-- > {
-- >     // binding to a single image descriptor
-- >     {
-- >         0,                                           // binding
-- >         0,                                           // dstArrayElement
-- >         1,                                           // descriptorCount
-- >         VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,   // descriptorType
-- >         offsetof(AppDataStructure, imageInfo),       // offset
-- >         0                                            // stride is not required if descriptorCount is 1.
-- >     }
-- >
-- > };
-- >
-- > // create an descriptor update template for descriptor set updates
-- > const VkDescriptorUpdateTemplateCreateInfo createInfo =
-- > {
-- >     VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO,  // sType
-- >     NULL,                                                          // pNext
-- >     0,                                                             // flags
-- >     1,                                                             // descriptorUpdateEntryCount
-- >     descriptorUpdateTemplateEntries,                               // pDescriptorUpdateEntries
-- >     VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR,       // templateType
-- >     0,                                                             // descriptorSetLayout, ignored by given templateType
-- >     VK_PIPELINE_BIND_POINT_GRAPHICS,                               // pipelineBindPoint
-- >     myPipelineLayout,                                              // pipelineLayout
-- >     0,                                                             // set
-- > };
-- >
-- > VkDescriptorUpdateTemplate myDescriptorUpdateTemplate;
-- > myResult = vkCreateDescriptorUpdateTemplate(
-- >     myDevice,
-- >     &createInfo,
-- >     NULL,
-- >     &myDescriptorUpdateTemplate);
-- > }
-- >
-- > AppDataStructure appData;
-- > // fill appData here or cache it in your engine
-- > vkCmdPushDescriptorSetWithTemplateKHR(myCmdBuffer, myDescriptorUpdateTemplate, myPipelineLayout, 0,&appData);
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_descriptor_update_template.VkDescriptorUpdateTemplate',
-- 'Graphics.Vulkan.Core10.Pipeline.VkPipelineLayout'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdPushDescriptorSetWithTemplateKHR" vkCmdPushDescriptorSetWithTemplateKHR :: ("commandBuffer" ::: VkCommandBuffer) -> ("descriptorUpdateTemplate" ::: VkDescriptorUpdateTemplate) -> ("layout" ::: VkPipelineLayout) -> ("set" ::: Word32) -> ("pData" ::: Ptr ()) -> IO ()
-- | VkPhysicalDevicePushDescriptorPropertiesKHR - Structure describing push
-- descriptor limits that can be supported by an implementation
--
-- = Members
--
-- The members of the @VkPhysicalDevicePushDescriptorPropertiesKHR@
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- -   @maxPushDescriptors@ is the maximum number of descriptors that /can/
--     be used in a descriptor set created with
--     @VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR@ set.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPhysicalDevicePushDescriptorPropertiesKHR = VkPhysicalDevicePushDescriptorPropertiesKHR
  { -- No documentation found for Nested "VkPhysicalDevicePushDescriptorPropertiesKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDevicePushDescriptorPropertiesKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDevicePushDescriptorPropertiesKHR" "maxPushDescriptors"
  vkMaxPushDescriptors :: Word32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDevicePushDescriptorPropertiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDevicePushDescriptorPropertiesKHR <$> peek (ptr `plusPtr` 0)
                                                         <*> peek (ptr `plusPtr` 8)
                                                         <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDevicePushDescriptorPropertiesKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDevicePushDescriptorPropertiesKHR))
                *> poke (ptr `plusPtr` 16) (vkMaxPushDescriptors (poked :: VkPhysicalDevicePushDescriptorPropertiesKHR))
