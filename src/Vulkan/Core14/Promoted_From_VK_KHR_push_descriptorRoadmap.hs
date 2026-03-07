{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_push_descriptorRoadmap"
module Vulkan.Core14.Promoted_From_VK_KHR_push_descriptorRoadmap  ( cmdPushDescriptorSet
                                                                  , cmdPushDescriptorSetWithTemplate
                                                                  , PhysicalDevicePushDescriptorProperties(..)
                                                                  , StructureType(..)
                                                                  , DescriptorUpdateTemplateType(..)
                                                                  , DescriptorSetLayoutCreateFlagBits(..)
                                                                  , DescriptorSetLayoutCreateFlags
                                                                  ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core11.Handles (DescriptorUpdateTemplate)
import Vulkan.Core11.Handles (DescriptorUpdateTemplate(..))
import Vulkan.Dynamic (DeviceCmds(pVkCmdPushDescriptorSet))
import Vulkan.Dynamic (DeviceCmds(pVkCmdPushDescriptorSetWithTemplate))
import Vulkan.Core10.Enums.PipelineBindPoint (PipelineBindPoint)
import Vulkan.Core10.Enums.PipelineBindPoint (PipelineBindPoint(..))
import Vulkan.Core10.Handles (PipelineLayout)
import Vulkan.Core10.Handles (PipelineLayout(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.DescriptorSet (WriteDescriptorSet)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES))
import Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits (DescriptorSetLayoutCreateFlagBits(..))
import Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits (DescriptorSetLayoutCreateFlags)
import Vulkan.Core11.Enums.DescriptorUpdateTemplateType (DescriptorUpdateTemplateType(..))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdPushDescriptorSet
  :: FunPtr (Ptr CommandBuffer_T -> PipelineBindPoint -> PipelineLayout -> Word32 -> Word32 -> Ptr (SomeStruct WriteDescriptorSet) -> IO ()) -> Ptr CommandBuffer_T -> PipelineBindPoint -> PipelineLayout -> Word32 -> Word32 -> Ptr (SomeStruct WriteDescriptorSet) -> IO ()

-- | vkCmdPushDescriptorSet - Pushes descriptor updates into a command buffer
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
-- shaders to use the updated descriptors for subsequent
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-bindpoint-commands bound pipeline commands>
-- with the pipeline type set by @pipelineBindPoint@ until the descriptor
-- is overwritten, or else until the set is disturbed as described in
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptorsets-compatibility Pipeline Layout Compatibility>.
-- When the set is disturbed or push descriptors with a different
-- descriptor set layout are set, all push descriptors are undefined.
--
-- Push descriptors that are
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#shaders-staticuse statically used>
-- by a pipeline /must/ not be undefined at the time that a drawing or
-- dispatching command is recorded to execute using that pipeline. This
-- includes immutable sampler descriptors, which /must/ be pushed before
-- they are accessed by a pipeline (the immutable samplers are pushed,
-- rather than the samplers in @pDescriptorWrites@). Push descriptors that
-- are not statically used /can/ remain undefined.
--
-- Push descriptors do not use dynamic offsets. Instead, the corresponding
-- non-dynamic descriptor types /can/ be used and the @offset@ member of
-- 'Vulkan.Core10.DescriptorSet.DescriptorBufferInfo' /can/ be changed each
-- time the descriptor is written.
--
-- Each element of @pDescriptorWrites@ is interpreted as in
-- 'Vulkan.Core10.DescriptorSet.WriteDescriptorSet', except the @dstSet@
-- member is ignored.
--
-- To push an immutable sampler, use a
-- 'Vulkan.Core10.DescriptorSet.WriteDescriptorSet' with @dstBinding@ and
-- @dstArrayElement@ selecting the immutable sampler’s binding. If the
-- descriptor type is
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLER', the
-- @pImageInfo@ parameter is ignored and the immutable sampler is taken
-- from the push descriptor set layout in the pipeline layout. If the
-- descriptor type is
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
-- the @sampler@ member of the @pImageInfo@ parameter is ignored and the
-- immutable sampler is taken from the push descriptor set layout in the
-- pipeline layout.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdPushDescriptorSet-commandBuffer-11295# If @commandBuffer@
--     is a secondary command buffer, it /must/ have begun with
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.CommandBufferInheritanceDescriptorHeapInfoEXT'::@pSamplerHeapBindInfo@
--     equal to @NULL@
--
-- -   #VUID-vkCmdPushDescriptorSet-commandBuffer-11296# If @commandBuffer@
--     is a secondary command buffer, it /must/ have begun with
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.CommandBufferInheritanceDescriptorHeapInfoEXT'::@pResourceHeapBindInfo@
--     equal to @NULL@
--
-- -   #VUID-vkCmdPushDescriptorSet-set-00364# @set@ /must/ be less than
--     'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo'::@setLayoutCount@
--     provided when @layout@ was created
--
-- -   #VUID-vkCmdPushDescriptorSet-set-00365# @set@ /must/ be the unique
--     set number in the pipeline layout that uses a descriptor set layout
--     that was created with
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT'
--
-- -   #VUID-vkCmdPushDescriptorSet-pDescriptorWrites-06494# For each
--     element i where @pDescriptorWrites@[i].@descriptorType@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLER',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE',
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INPUT_ATTACHMENT',
--     @pDescriptorWrites@[i].@pImageInfo@ /must/ be a valid pointer to an
--     array of @pDescriptorWrites@[i].@descriptorCount@ valid
--     'Vulkan.Core10.DescriptorSet.DescriptorImageInfo' structures
--
-- -   #VUID-vkCmdPushDescriptorSet-pipelineBindPoint-00363#
--     @pipelineBindPoint@ /must/ be supported by the @commandBuffer@’s
--     parent 'Vulkan.Core10.Handles.CommandPool'’s queue family
--
-- -   #VUID-vkCmdPushDescriptorSet-None-10356# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_push_descriptor VK_KHR_push_descriptor>
--     extension is not enabled,
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-pushDescriptor pushDescriptor>
--     /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdPushDescriptorSet-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdPushDescriptorSet-pipelineBindPoint-parameter#
--     @pipelineBindPoint@ /must/ be a valid
--     'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' value
--
-- -   #VUID-vkCmdPushDescriptorSet-layout-parameter# @layout@ /must/ be a
--     valid 'Vulkan.Core10.Handles.PipelineLayout' handle
--
-- -   #VUID-vkCmdPushDescriptorSet-pDescriptorWrites-parameter#
--     @pDescriptorWrites@ /must/ be a valid pointer to an array of
--     @descriptorWriteCount@ valid
--     'Vulkan.Core10.DescriptorSet.WriteDescriptorSet' structures
--
-- -   #VUID-vkCmdPushDescriptorSet-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdPushDescriptorSet-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT', or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' operations
--
-- -   #VUID-vkCmdPushDescriptorSet-videocoding# This command /must/ only
--     be called outside of a video coding scope
--
-- -   #VUID-vkCmdPushDescriptorSet-descriptorWriteCount-arraylength#
--     @descriptorWriteCount@ /must/ be greater than @0@
--
-- -   #VUID-vkCmdPushDescriptorSet-commonparent# Both of @commandBuffer@,
--     and @layout@ /must/ have been created, allocated, or retrieved from
--     the same 'Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | VK_QUEUE_COMPUTE_BIT                                                                                                  | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | VK_QUEUE_GRAPHICS_BIT                                                                                                 |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdPushDescriptorSet is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_push_descriptor VK_KHR_push_descriptor>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint',
-- 'Vulkan.Core10.Handles.PipelineLayout',
-- 'Vulkan.Core10.DescriptorSet.WriteDescriptorSet'
cmdPushDescriptorSet :: forall io
                      . (MonadIO io)
                     => -- | @commandBuffer@ is the command buffer that the descriptors will be
                        -- recorded in.
                        CommandBuffer
                     -> -- | @pipelineBindPoint@ is a
                        -- 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' indicating the
                        -- type of the pipeline that will use the descriptors. There is a separate
                        -- set of push descriptor bindings for each pipeline type, so binding one
                        -- does not disturb the others.
                        PipelineBindPoint
                     -> -- | @layout@ is a 'Vulkan.Core10.Handles.PipelineLayout' object used to
                        -- program the bindings.
                        PipelineLayout
                     -> -- | @set@ is the set number of the descriptor set in the pipeline layout
                        -- that will be updated.
                        ("set" ::: Word32)
                     -> -- | @pDescriptorWrites@ is a pointer to an array of
                        -- 'Vulkan.Core10.DescriptorSet.WriteDescriptorSet' structures describing
                        -- the descriptors to be updated.
                        ("descriptorWrites" ::: Vector (SomeStruct WriteDescriptorSet))
                     -> io ()
cmdPushDescriptorSet commandBuffer
                       pipelineBindPoint
                       layout
                       set
                       descriptorWrites = liftIO . evalContT $ do
  let vkCmdPushDescriptorSetPtr = pVkCmdPushDescriptorSet (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdPushDescriptorSetPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdPushDescriptorSet is null" Nothing Nothing
  let vkCmdPushDescriptorSet' = mkVkCmdPushDescriptorSet vkCmdPushDescriptorSetPtr
  pPDescriptorWrites <- ContT $ allocaBytes @(WriteDescriptorSet _) ((Data.Vector.length (descriptorWrites)) * 64)
  Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPDescriptorWrites `plusPtr` (64 * (i)) :: Ptr (WriteDescriptorSet _))) (e) . ($ ())) (descriptorWrites)
  lift $ traceAroundEvent "vkCmdPushDescriptorSet" (vkCmdPushDescriptorSet'
                                                      (commandBufferHandle (commandBuffer))
                                                      (pipelineBindPoint)
                                                      (layout)
                                                      (set)
                                                      ((fromIntegral (Data.Vector.length $ (descriptorWrites)) :: Word32))
                                                      (forgetExtensions (pPDescriptorWrites)))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdPushDescriptorSetWithTemplate
  :: FunPtr (Ptr CommandBuffer_T -> DescriptorUpdateTemplate -> PipelineLayout -> Word32 -> Ptr () -> IO ()) -> Ptr CommandBuffer_T -> DescriptorUpdateTemplate -> PipelineLayout -> Word32 -> Ptr () -> IO ()

-- | vkCmdPushDescriptorSetWithTemplate - Pushes descriptor updates into a
-- command buffer using a descriptor update template
--
-- == Valid Usage
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplate-commandBuffer-11295# If
--     @commandBuffer@ is a secondary command buffer, it /must/ have begun
--     with
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.CommandBufferInheritanceDescriptorHeapInfoEXT'::@pSamplerHeapBindInfo@
--     equal to @NULL@
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplate-commandBuffer-11296# If
--     @commandBuffer@ is a secondary command buffer, it /must/ have begun
--     with
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.CommandBufferInheritanceDescriptorHeapInfoEXT'::@pResourceHeapBindInfo@
--     equal to @NULL@
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplate-commandBuffer-00366# The
--     @pipelineBindPoint@ specified during the creation of the descriptor
--     update template /must/ be supported by the @commandBuffer@’s parent
--     'Vulkan.Core10.Handles.CommandPool'’s queue family
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplate-pData-01686# @pData@ /must/
--     be a valid pointer to a memory containing one or more valid
--     instances of 'Vulkan.Core10.DescriptorSet.DescriptorImageInfo',
--     'Vulkan.Core10.DescriptorSet.DescriptorBufferInfo', or
--     'Vulkan.Core10.Handles.BufferView' in a layout defined by
--     @descriptorUpdateTemplate@ when it was created with
--     'Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.createDescriptorUpdateTemplate'
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplate-layout-07993# @layout@
--     /must/ be compatible with the layout used to create
--     @descriptorUpdateTemplate@
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplate-descriptorUpdateTemplate-07994#
--     @descriptorUpdateTemplate@ /must/ have been created with a
--     @templateType@ of
--     'Vulkan.Core11.Enums.DescriptorUpdateTemplateType.DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS'
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplate-set-07995# @set@ /must/ be
--     the same value used to create @descriptorUpdateTemplate@
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplate-set-07304# @set@ /must/ be
--     less than
--     'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo'::@setLayoutCount@
--     provided when @layout@ was created
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplate-set-11854# @set@ /must/
--     reference a valid 'Vulkan.Core10.Handles.DescriptorSetLayout' handle
--     in @layout@
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplate-set-07305# @set@ /must/ be
--     the unique set number in the pipeline layout that uses a descriptor
--     set layout that was created with
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT'
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplate-None-10358# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_push_descriptor VK_KHR_push_descriptor>
--     extension is not enabled,
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-pushDescriptor pushDescriptor>
--     /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplate-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplate-descriptorUpdateTemplate-parameter#
--     @descriptorUpdateTemplate@ /must/ be a valid
--     'Vulkan.Core11.Handles.DescriptorUpdateTemplate' handle
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplate-layout-parameter# @layout@
--     /must/ be a valid 'Vulkan.Core10.Handles.PipelineLayout' handle
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplate-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplate-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT', or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' operations
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplate-videocoding# This command
--     /must/ only be called outside of a video coding scope
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplate-commonparent# Each of
--     @commandBuffer@, @descriptorUpdateTemplate@, and @layout@ /must/
--     have been created, allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | VK_QUEUE_COMPUTE_BIT                                                                                                  | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | VK_QUEUE_GRAPHICS_BIT                                                                                                 |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdPushDescriptorSetWithTemplate is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- __API Example__
--
-- > struct AppDataStructure
-- > {
-- >     VkDescriptorImageInfo  imageInfo;          // a single image info
-- >     // ... some more application-related data
-- > };
-- >
-- > const VkDescriptorUpdateTemplateEntry descriptorUpdateTemplateEntries[] =
-- > {
-- >     // binding to a single image descriptor
-- >     {
-- >         .binding = 0,
-- >         .dstArrayElement = 0,
-- >         .descriptorCount = 1,
-- >         .descriptorType = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
-- >         .offset = offsetof(AppDataStructure, imageInfo),
-- >         .stride = 0     // not required if descriptorCount is 1
-- >     }
-- > };
-- >
-- > // create a descriptor update template for push descriptor set updates
-- > const VkDescriptorUpdateTemplateCreateInfo createInfo =
-- > {
-- >     .sType = VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO,
-- >     .pNext = NULL,
-- >     .flags = 0,
-- >     .descriptorUpdateEntryCount = 1,
-- >     .pDescriptorUpdateEntries = descriptorUpdateTemplateEntries,
-- >     .templateType = VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS,
-- >     .descriptorSetLayout = 0,   // ignored by given templateType
-- >     .pipelineBindPoint = VK_PIPELINE_BIND_POINT_GRAPHICS,
-- >     .pipelineLayout = myPipelineLayout,
-- >     .set = 0,
-- > };
-- >
-- > VkDescriptorUpdateTemplate myDescriptorUpdateTemplate;
-- > myResult = vkCreateDescriptorUpdateTemplate(
-- >     myDevice,
-- >     &createInfo,
-- >     NULL,
-- >     &myDescriptorUpdateTemplate);
-- >
-- > AppDataStructure appData;
-- > // fill appData here or cache it in your engine
-- > vkCmdPushDescriptorSetWithTemplate(myCmdBuffer, myDescriptorUpdateTemplate, myPipelineLayout, 0,&appData);
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_descriptor_update_template VK_KHR_descriptor_update_template>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_push_descriptor VK_KHR_push_descriptor>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core11.Handles.DescriptorUpdateTemplate',
-- 'Vulkan.Core10.Handles.PipelineLayout'
cmdPushDescriptorSetWithTemplate :: forall io
                                  . (MonadIO io)
                                 => -- | @commandBuffer@ is the command buffer that the descriptors will be
                                    -- recorded in.
                                    CommandBuffer
                                 -> -- | @descriptorUpdateTemplate@ is a descriptor update template defining how
                                    -- to interpret the descriptor information in @pData@.
                                    DescriptorUpdateTemplate
                                 -> -- | @layout@ is a 'Vulkan.Core10.Handles.PipelineLayout' object used to
                                    -- program the bindings. It /must/ be compatible with the layout used to
                                    -- create the @descriptorUpdateTemplate@ handle.
                                    PipelineLayout
                                 -> -- | @set@ is the set number of the descriptor set in the pipeline layout
                                    -- that will be updated. This /must/ be the same number used to create the
                                    -- @descriptorUpdateTemplate@ handle.
                                    ("set" ::: Word32)
                                 -> -- | @pData@ is a pointer to memory containing descriptors for the templated
                                    -- update.
                                    ("data" ::: Ptr ())
                                 -> io ()
cmdPushDescriptorSetWithTemplate commandBuffer
                                   descriptorUpdateTemplate
                                   layout
                                   set
                                   data' = liftIO $ do
  let vkCmdPushDescriptorSetWithTemplatePtr = pVkCmdPushDescriptorSetWithTemplate (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdPushDescriptorSetWithTemplatePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdPushDescriptorSetWithTemplate is null" Nothing Nothing
  let vkCmdPushDescriptorSetWithTemplate' = mkVkCmdPushDescriptorSetWithTemplate vkCmdPushDescriptorSetWithTemplatePtr
  traceAroundEvent "vkCmdPushDescriptorSetWithTemplate" (vkCmdPushDescriptorSetWithTemplate'
                                                           (commandBufferHandle (commandBuffer))
                                                           (descriptorUpdateTemplate)
                                                           (layout)
                                                           (set)
                                                           (data'))
  pure $ ()


-- | VkPhysicalDevicePushDescriptorProperties - Structure describing push
-- descriptor limits that can be supported by an implementation
--
-- = Description
--
-- If the 'PhysicalDevicePushDescriptorProperties' structure is included in
-- the @pNext@ chain of the
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_push_descriptor VK_KHR_push_descriptor>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePushDescriptorProperties = PhysicalDevicePushDescriptorProperties
  { -- | #extension-limits-maxPushDescriptors# @maxPushDescriptors@ is the
    -- maximum number of descriptors that /can/ be used in a descriptor set
    -- layout created with
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT'
    -- set.
    maxPushDescriptors :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePushDescriptorProperties)
#endif
deriving instance Show PhysicalDevicePushDescriptorProperties

instance ToCStruct PhysicalDevicePushDescriptorProperties where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePushDescriptorProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxPushDescriptors)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDevicePushDescriptorProperties where
  peekCStruct p = do
    maxPushDescriptors <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ PhysicalDevicePushDescriptorProperties
             maxPushDescriptors

instance Storable PhysicalDevicePushDescriptorProperties where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePushDescriptorProperties where
  zero = PhysicalDevicePushDescriptorProperties
           zero

