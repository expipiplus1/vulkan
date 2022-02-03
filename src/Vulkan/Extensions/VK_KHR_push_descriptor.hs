{-# language CPP #-}
-- | = Name
--
-- VK_KHR_push_descriptor - device extension
--
-- == VK_KHR_push_descriptor
--
-- [__Name String__]
--     @VK_KHR_push_descriptor@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     81
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_push_descriptor] @jeffbolznv%0A<<Here describe the issue or question you have about the VK_KHR_push_descriptor extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-09-12
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Michael Worcester, Imagination Technologies
--
-- == Description
--
-- This extension allows descriptors to be written into the command buffer,
-- while the implementation is responsible for managing their memory. Push
-- descriptors may enable easier porting from older APIs and in some cases
-- can be more efficient than writing descriptors into descriptor sets.
--
-- == New Commands
--
-- -   'cmdPushDescriptorSetKHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_descriptor_update_template VK_KHR_descriptor_update_template>
-- is supported:
--
-- -   'cmdPushDescriptorSetWithTemplateKHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
-- is supported:
--
-- -   'cmdPushDescriptorSetWithTemplateKHR'
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDevicePushDescriptorPropertiesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_PUSH_DESCRIPTOR_EXTENSION_NAME'
--
-- -   'KHR_PUSH_DESCRIPTOR_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DescriptorSetLayoutCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_descriptor_update_template VK_KHR_descriptor_update_template>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core11.Enums.DescriptorUpdateTemplateType.DescriptorUpdateTemplateType':
--
--     -   'Vulkan.Core11.Enums.DescriptorUpdateTemplateType.DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core11.Enums.DescriptorUpdateTemplateType.DescriptorUpdateTemplateType':
--
--     -   'Vulkan.Core11.Enums.DescriptorUpdateTemplateType.DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR'
--
-- == Version History
--
-- -   Revision 1, 2016-10-15 (Jeff Bolz)
--
--     -   Internal revisions
--
-- -   Revision 2, 2017-09-12 (Tobias Hector)
--
--     -   Added interactions with Vulkan 1.1
--
-- == See Also
--
-- 'PhysicalDevicePushDescriptorPropertiesKHR', 'cmdPushDescriptorSetKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_push_descriptor Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_push_descriptor  ( cmdPushDescriptorSetKHR
                                                 , cmdPushDescriptorSetWithTemplateKHR
                                                 , PhysicalDevicePushDescriptorPropertiesKHR(..)
                                                 , KHR_PUSH_DESCRIPTOR_SPEC_VERSION
                                                 , pattern KHR_PUSH_DESCRIPTOR_SPEC_VERSION
                                                 , KHR_PUSH_DESCRIPTOR_EXTENSION_NAME
                                                 , pattern KHR_PUSH_DESCRIPTOR_EXTENSION_NAME
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
import Data.String (IsString)
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
import Vulkan.Dynamic (DeviceCmds(pVkCmdPushDescriptorSetKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdPushDescriptorSetWithTemplateKHR))
import Vulkan.Core10.Enums.PipelineBindPoint (PipelineBindPoint)
import Vulkan.Core10.Enums.PipelineBindPoint (PipelineBindPoint(..))
import Vulkan.Core10.Handles (PipelineLayout)
import Vulkan.Core10.Handles (PipelineLayout(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.DescriptorSet (WriteDescriptorSet)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdPushDescriptorSetKHR
  :: FunPtr (Ptr CommandBuffer_T -> PipelineBindPoint -> PipelineLayout -> Word32 -> Word32 -> Ptr (SomeStruct WriteDescriptorSet) -> IO ()) -> Ptr CommandBuffer_T -> PipelineBindPoint -> PipelineLayout -> Word32 -> Word32 -> Ptr (SomeStruct WriteDescriptorSet) -> IO ()

-- | vkCmdPushDescriptorSetKHR - Pushes descriptor updates into a command
-- buffer
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
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#pipeline-bindpoint-commands bound pipeline commands>
-- with the pipeline type set by @pipelineBindPoint@ until the descriptor
-- is overwritten, or else until the set is disturbed as described in
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-compatibility Pipeline Layout Compatibility>.
-- When the set is disturbed or push descriptors with a different
-- descriptor set layout are set, all push descriptors are undefined.
--
-- Push descriptors that are
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-staticuse statically used>
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
-- -   #VUID-vkCmdPushDescriptorSetKHR-pipelineBindPoint-00363#
--     @pipelineBindPoint@ /must/ be supported by the @commandBuffer@’s
--     parent 'Vulkan.Core10.Handles.CommandPool'’s queue family
--
-- -   #VUID-vkCmdPushDescriptorSetKHR-set-00364# @set@ /must/ be less than
--     'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo'::@setLayoutCount@
--     provided when @layout@ was created
--
-- -   #VUID-vkCmdPushDescriptorSetKHR-set-00365# @set@ /must/ be the
--     unique set number in the pipeline layout that uses a descriptor set
--     layout that was created with
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR'
--
-- -   #VUID-vkCmdPushDescriptorSetKHR-pDescriptorWrites-06494# For each
--     element i where @pDescriptorWrites@[i].@descriptorType@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE',
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INPUT_ATTACHMENT',
--     @pDescriptorWrites@[i].@pImageInfo@ /must/ be a valid pointer to an
--     array of @pDescriptorWrites@[i].@descriptorCount@ valid
--     'Vulkan.Core10.DescriptorSet.DescriptorImageInfo' structures
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdPushDescriptorSetKHR-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdPushDescriptorSetKHR-pipelineBindPoint-parameter#
--     @pipelineBindPoint@ /must/ be a valid
--     'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' value
--
-- -   #VUID-vkCmdPushDescriptorSetKHR-layout-parameter# @layout@ /must/ be
--     a valid 'Vulkan.Core10.Handles.PipelineLayout' handle
--
-- -   #VUID-vkCmdPushDescriptorSetKHR-pDescriptorWrites-parameter#
--     @pDescriptorWrites@ /must/ be a valid pointer to an array of
--     @descriptorWriteCount@ valid
--     'Vulkan.Core10.DescriptorSet.WriteDescriptorSet' structures
--
-- -   #VUID-vkCmdPushDescriptorSetKHR-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdPushDescriptorSetKHR-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   #VUID-vkCmdPushDescriptorSetKHR-descriptorWriteCount-arraylength#
--     @descriptorWriteCount@ /must/ be greater than @0@
--
-- -   #VUID-vkCmdPushDescriptorSetKHR-commonparent# Both of
--     @commandBuffer@, and @layout@ /must/ have been created, allocated,
--     or retrieved from the same 'Vulkan.Core10.Handles.Device'
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_push_descriptor VK_KHR_push_descriptor>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint',
-- 'Vulkan.Core10.Handles.PipelineLayout',
-- 'Vulkan.Core10.DescriptorSet.WriteDescriptorSet'
cmdPushDescriptorSetKHR :: forall io
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
cmdPushDescriptorSetKHR commandBuffer pipelineBindPoint layout set descriptorWrites = liftIO . evalContT $ do
  let vkCmdPushDescriptorSetKHRPtr = pVkCmdPushDescriptorSetKHR (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdPushDescriptorSetKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdPushDescriptorSetKHR is null" Nothing Nothing
  let vkCmdPushDescriptorSetKHR' = mkVkCmdPushDescriptorSetKHR vkCmdPushDescriptorSetKHRPtr
  pPDescriptorWrites <- ContT $ allocaBytes @(WriteDescriptorSet _) ((Data.Vector.length (descriptorWrites)) * 64)
  Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPDescriptorWrites `plusPtr` (64 * (i)) :: Ptr (WriteDescriptorSet _))) (e) . ($ ())) (descriptorWrites)
  lift $ traceAroundEvent "vkCmdPushDescriptorSetKHR" (vkCmdPushDescriptorSetKHR' (commandBufferHandle (commandBuffer)) (pipelineBindPoint) (layout) (set) ((fromIntegral (Data.Vector.length $ (descriptorWrites)) :: Word32)) (forgetExtensions (pPDescriptorWrites)))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdPushDescriptorSetWithTemplateKHR
  :: FunPtr (Ptr CommandBuffer_T -> DescriptorUpdateTemplate -> PipelineLayout -> Word32 -> Ptr () -> IO ()) -> Ptr CommandBuffer_T -> DescriptorUpdateTemplate -> PipelineLayout -> Word32 -> Ptr () -> IO ()

-- | vkCmdPushDescriptorSetWithTemplateKHR - Pushes descriptor updates into a
-- command buffer using a descriptor update template
--
-- == Valid Usage
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplateKHR-commandBuffer-00366# The
--     @pipelineBindPoint@ specified during the creation of the descriptor
--     update template /must/ be supported by the @commandBuffer@’s parent
--     'Vulkan.Core10.Handles.CommandPool'’s queue family
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplateKHR-pData-01686# @pData@
--     /must/ be a valid pointer to a memory containing one or more valid
--     instances of 'Vulkan.Core10.DescriptorSet.DescriptorImageInfo',
--     'Vulkan.Core10.DescriptorSet.DescriptorBufferInfo', or
--     'Vulkan.Core10.Handles.BufferView' in a layout defined by
--     @descriptorUpdateTemplate@ when it was created with
--     'Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.createDescriptorUpdateTemplate'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplateKHR-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplateKHR-descriptorUpdateTemplate-parameter#
--     @descriptorUpdateTemplate@ /must/ be a valid
--     'Vulkan.Core11.Handles.DescriptorUpdateTemplate' handle
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplateKHR-layout-parameter#
--     @layout@ /must/ be a valid 'Vulkan.Core10.Handles.PipelineLayout'
--     handle
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplateKHR-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplateKHR-commandBuffer-cmdpool#
--     The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplateKHR-commonparent# Each of
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
--
-- __API example__
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
-- > // create a descriptor update template for push descriptor set updates
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
-- >
-- > AppDataStructure appData;
-- > // fill appData here or cache it in your engine
-- > vkCmdPushDescriptorSetWithTemplateKHR(myCmdBuffer, myDescriptorUpdateTemplate, myPipelineLayout, 0,&appData);
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_descriptor_update_template VK_KHR_descriptor_update_template>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_push_descriptor VK_KHR_push_descriptor>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core11.Handles.DescriptorUpdateTemplate',
-- 'Vulkan.Core10.Handles.PipelineLayout'
cmdPushDescriptorSetWithTemplateKHR :: forall io
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
cmdPushDescriptorSetWithTemplateKHR commandBuffer descriptorUpdateTemplate layout set data' = liftIO $ do
  let vkCmdPushDescriptorSetWithTemplateKHRPtr = pVkCmdPushDescriptorSetWithTemplateKHR (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdPushDescriptorSetWithTemplateKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdPushDescriptorSetWithTemplateKHR is null" Nothing Nothing
  let vkCmdPushDescriptorSetWithTemplateKHR' = mkVkCmdPushDescriptorSetWithTemplateKHR vkCmdPushDescriptorSetWithTemplateKHRPtr
  traceAroundEvent "vkCmdPushDescriptorSetWithTemplateKHR" (vkCmdPushDescriptorSetWithTemplateKHR' (commandBufferHandle (commandBuffer)) (descriptorUpdateTemplate) (layout) (set) (data'))
  pure $ ()


-- | VkPhysicalDevicePushDescriptorPropertiesKHR - Structure describing push
-- descriptor limits that can be supported by an implementation
--
-- = Description
--
-- If the 'PhysicalDevicePushDescriptorPropertiesKHR' structure is included
-- in the @pNext@ chain of the
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
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePushDescriptorPropertiesKHR = PhysicalDevicePushDescriptorPropertiesKHR
  { -- | #limits-maxPushDescriptors# @maxPushDescriptors@ is the maximum number
    -- of descriptors that /can/ be used in a descriptor set created with
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR'
    -- set.
    maxPushDescriptors :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePushDescriptorPropertiesKHR)
#endif
deriving instance Show PhysicalDevicePushDescriptorPropertiesKHR

instance ToCStruct PhysicalDevicePushDescriptorPropertiesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePushDescriptorPropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxPushDescriptors)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDevicePushDescriptorPropertiesKHR where
  peekCStruct p = do
    maxPushDescriptors <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ PhysicalDevicePushDescriptorPropertiesKHR
             maxPushDescriptors

instance Storable PhysicalDevicePushDescriptorPropertiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePushDescriptorPropertiesKHR where
  zero = PhysicalDevicePushDescriptorPropertiesKHR
           zero


type KHR_PUSH_DESCRIPTOR_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_KHR_PUSH_DESCRIPTOR_SPEC_VERSION"
pattern KHR_PUSH_DESCRIPTOR_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_PUSH_DESCRIPTOR_SPEC_VERSION = 2


type KHR_PUSH_DESCRIPTOR_EXTENSION_NAME = "VK_KHR_push_descriptor"

-- No documentation found for TopLevel "VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME"
pattern KHR_PUSH_DESCRIPTOR_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_PUSH_DESCRIPTOR_EXTENSION_NAME = "VK_KHR_push_descriptor"

