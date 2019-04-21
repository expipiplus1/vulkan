{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_push_descriptor
  ( withCStructPhysicalDevicePushDescriptorPropertiesKHR
  , fromCStructPhysicalDevicePushDescriptorPropertiesKHR
  , PhysicalDevicePushDescriptorPropertiesKHR(..)
  , cmdPushDescriptorSetKHR
  , cmdPushDescriptorSetWithTemplateKHR
  , pattern VK_KHR_PUSH_DESCRIPTOR_SPEC_VERSION
  , pattern VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR
  , pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR
  , pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR
  ) where

import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( length
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( Ptr
  , castPtr
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_push_descriptor
  ( VkPhysicalDevicePushDescriptorPropertiesKHR(..)
  , vkCmdPushDescriptorSetKHR
  , vkCmdPushDescriptorSetWithTemplateKHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR
  )
import Graphics.Vulkan.Core10.DescriptorSet
  ( WriteDescriptorSet(..)
  , withCStructWriteDescriptorSet
  )
import Graphics.Vulkan.Core10.Pass
  ( PipelineBindPoint
  )
import Graphics.Vulkan.Core10.Pipeline
  ( PipelineLayout
  )
import Graphics.Vulkan.Core10.Queue
  ( CommandBuffer(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_descriptor_update_template
  ( DescriptorUpdateTemplate
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_push_descriptor
  ( pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR
  , pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR
  , pattern VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME
  , pattern VK_KHR_PUSH_DESCRIPTOR_SPEC_VERSION
  )



-- | VkPhysicalDevicePushDescriptorPropertiesKHR - Structure describing push
-- descriptor limits that can be supported by an implementation
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_push_descriptor.VkPhysicalDevicePushDescriptorPropertiesKHR'
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_push_descriptor.VkPhysicalDevicePushDescriptorPropertiesKHR'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- Unresolved directive in VkPhysicalDevicePushDescriptorPropertiesKHR.txt
-- -
-- include::{generated}\/validity\/structs\/VkPhysicalDevicePushDescriptorPropertiesKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data PhysicalDevicePushDescriptorPropertiesKHR = PhysicalDevicePushDescriptorPropertiesKHR
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDevicePushDescriptorPropertiesKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDevicePushDescriptorPropertiesKHR" "maxPushDescriptors"
  maxPushDescriptors :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDevicePushDescriptorPropertiesKHR' and
-- marshal a 'PhysicalDevicePushDescriptorPropertiesKHR' into it. The 'VkPhysicalDevicePushDescriptorPropertiesKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDevicePushDescriptorPropertiesKHR :: PhysicalDevicePushDescriptorPropertiesKHR -> (VkPhysicalDevicePushDescriptorPropertiesKHR -> IO a) -> IO a
withCStructPhysicalDevicePushDescriptorPropertiesKHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDevicePushDescriptorPropertiesKHR)) (\pPNext -> cont (VkPhysicalDevicePushDescriptorPropertiesKHR VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR pPNext (maxPushDescriptors (marshalled :: PhysicalDevicePushDescriptorPropertiesKHR))))

-- | A function to read a 'VkPhysicalDevicePushDescriptorPropertiesKHR' and all additional
-- structures in the pointer chain into a 'PhysicalDevicePushDescriptorPropertiesKHR'.
fromCStructPhysicalDevicePushDescriptorPropertiesKHR :: VkPhysicalDevicePushDescriptorPropertiesKHR -> IO PhysicalDevicePushDescriptorPropertiesKHR
fromCStructPhysicalDevicePushDescriptorPropertiesKHR c = PhysicalDevicePushDescriptorPropertiesKHR <$> -- Univalued Member elided
                                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDevicePushDescriptorPropertiesKHR)))
                                                                                                   <*> pure (vkMaxPushDescriptors (c :: VkPhysicalDevicePushDescriptorPropertiesKHR))

instance Zero PhysicalDevicePushDescriptorPropertiesKHR where
  zero = PhysicalDevicePushDescriptorPropertiesKHR Nothing
                                                   zero



-- | vkCmdPushDescriptorSetKHR - Pushes descriptor updates into a command
-- buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer that the descriptors will be
--     recorded in.
--
-- -   @pipelineBindPoint@ is a
--     'Graphics.Vulkan.C.Core10.Pass.VkPipelineBindPoint' indicating
--     whether the descriptors will be used by graphics pipelines or
--     compute pipelines. There is a separate set of push descriptor
--     bindings for each of graphics and compute, so binding one does not
--     disturb the other.
--
-- -   @layout@ is a 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout'
--     object used to program the bindings.
--
-- -   @set@ is the set number of the descriptor set in the pipeline layout
--     that will be updated.
--
-- -   @descriptorWriteCount@ is the number of elements in the
--     @pDescriptorWrites@ array.
--
-- -   @pDescriptorWrites@ is a pointer to an array of
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkWriteDescriptorSet'
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
-- When a command buffer begins recording, all push descriptors are
-- undefined. Push descriptors /can/ be updated incrementally and cause
-- shaders to use the updated descriptors for subsequent rendering commands
-- (either compute or graphics, according to the @pipelineBindPoint@) until
-- the descriptor is overwritten, or else until the set is disturbed as
-- described in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-compatibility Pipeline Layout Compatibility>.
-- When the set is disturbed or push descriptors with a different
-- descriptor set layout are set, all push descriptors are undefined.
--
-- Push descriptors that are
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#shaders-staticuse statically used>
-- by a pipeline /must/ not be undefined at the time that a draw or
-- dispatch command is recorded to execute using that pipeline. This
-- includes immutable sampler descriptors, which /must/ be pushed before
-- they are accessed by a pipeline. Push descriptors that are not
-- statically used /can/ remain undefined.
--
-- Push descriptors do not use dynamic offsets. Instead, the corresponding
-- non-dynamic descriptor types /can/ be used and the @offset@ member of
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorBufferInfo' /can/ be
-- changed each time the descriptor is written.
--
-- Each element of @pDescriptorWrites@ is interpreted as in
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkWriteDescriptorSet', except
-- the @dstSet@ member is ignored.
--
-- To push an immutable sampler, use a
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkWriteDescriptorSet' with
-- @dstBinding@ and @dstArrayElement@ selecting the immutable sampler’s
-- binding. If the descriptor type is
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLER', the
-- @pImageInfo@ parameter is ignored and the immutable sampler is taken
-- from the push descriptor set layout in the pipeline layout. If the
-- descriptor type is
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
-- the @sampler@ member of the @pImageInfo@ parameter is ignored and the
-- immutable sampler is taken from the push descriptor set layout in the
-- pipeline layout.
--
-- == Valid Usage
--
-- Unresolved directive in vkCmdPushDescriptorSetKHR.txt -
-- include::{generated}\/validity\/protos\/vkCmdPushDescriptorSetKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
cmdPushDescriptorSetKHR :: CommandBuffer ->  PipelineBindPoint ->  PipelineLayout ->  Word32 ->  Vector WriteDescriptorSet ->  IO ()
cmdPushDescriptorSetKHR = \(CommandBuffer commandBuffer' commandTable) -> \pipelineBindPoint' -> \layout' -> \set' -> \descriptorWrites' -> withVec withCStructWriteDescriptorSet descriptorWrites' (\pDescriptorWrites' -> vkCmdPushDescriptorSetKHR commandTable commandBuffer' pipelineBindPoint' layout' set' (fromIntegral $ Data.Vector.length descriptorWrites') pDescriptorWrites' *> (pure ()))


-- | vkCmdPushDescriptorSetWithTemplateKHR - Pushes descriptor updates into a
-- command buffer using a descriptor update template
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer that the descriptors will be
--     recorded in.
--
-- -   @descriptorUpdateTemplate@ is a descriptor update template that
--     defines how to interpret the descriptor information in @pData@.
--
-- -   @layout@ is a 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout'
--     object used to program the bindings. It /must/ be compatible with
--     the layout used to create the @descriptorUpdateTemplate@ handle.
--
-- -   @set@ is the set number of the descriptor set in the pipeline layout
--     that will be updated. This /must/ be the same number used to create
--     the @descriptorUpdateTemplate@ handle.
--
-- -   @pData@ points to memory which contains the descriptors for the
--     templated update.
--
-- == Valid Usage
--
-- -   The @pipelineBindPoint@ specified during the creation of the
--     descriptor update template /must/ be supported by the
--     @commandBuffer@’s parent
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool'’s queue family
--
-- -   @pData@ /must/ be a valid pointer to a memory that contains one or
--     more valid instances of
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorImageInfo',
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorBufferInfo', or
--     'Graphics.Vulkan.C.Core10.BufferView.VkBufferView' in a layout
--     defined by @descriptorUpdateTemplate@ when it was created with
--     'Graphics.Vulkan.C.Extensions.VK_KHR_descriptor_update_template.vkCreateDescriptorUpdateTemplateKHR'
--
-- Unresolved directive in vkCmdPushDescriptorSetWithTemplateKHR.txt -
-- include::{generated}\/validity\/protos\/vkCmdPushDescriptorSetWithTemplateKHR.txt[]
--
-- __API example.__
--
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
-- >         0                                            // stride is not required if descriptorCount is 1
-- >     }
-- > };
-- >
-- > // create a descriptor update template for descriptor set updates
-- > const VkDescriptorUpdateTemplateCreateInfo createInfo =
-- > {
-- >     VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO,  // sType
-- >     NULL,                                                      // pNext
-- >     0,                                                         // flags
-- >     1,                                                         // descriptorUpdateEntryCount
-- >     descriptorUpdateTemplateEntries,                           // pDescriptorUpdateEntries
-- >     VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR,   // templateType
-- >     0,                                                         // descriptorSetLayout, ignored by given templateType
-- >     VK_PIPELINE_BIND_POINT_GRAPHICS,                           // pipelineBindPoint
-- >     myPipelineLayout,                                          // pipelineLayout
-- >     0,                                                         // set
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
-- No cross-references are available
cmdPushDescriptorSetWithTemplateKHR :: CommandBuffer ->  DescriptorUpdateTemplate ->  PipelineLayout ->  Word32 ->  Ptr () ->  IO ()
cmdPushDescriptorSetWithTemplateKHR = \(CommandBuffer commandBuffer' commandTable) -> \descriptorUpdateTemplate' -> \layout' -> \set' -> \pData' -> vkCmdPushDescriptorSetWithTemplateKHR commandTable commandBuffer' descriptorUpdateTemplate' layout' set' pData' *> (pure ())
