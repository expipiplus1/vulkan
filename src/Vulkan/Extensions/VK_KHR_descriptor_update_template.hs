{-# language CPP #-}
-- | = Name
--
-- VK_KHR_descriptor_update_template - device extension
--
-- = VK_KHR_descriptor_update_template
--
-- [__Name String__]
--     @VK_KHR_descriptor_update_template@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     86
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     None
--
-- [__API Interactions__]
--
--     -   Interacts with VK_EXT_debug_report
--
--     -   Interacts with VK_KHR_push_descriptor
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1-promotions Vulkan 1.1>
--
-- [__Contact__]
--
--     -   Markus Tavenrath
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_descriptor_update_template] @mtavenrath%0A*Here describe the issue or question you have about the VK_KHR_descriptor_update_template extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-09-05
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   Interacts with @VK_KHR_push_descriptor@
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Michael Worcester, Imagination Technologies
--
-- == Description
--
-- Applications may wish to update a fixed set of descriptors in a large
-- number of descriptor sets very frequently, i.e. during initialization
-- phase or if it is required to rebuild descriptor sets for each frame.
-- For those cases it is also not unlikely that all information required to
-- update a single descriptor set is stored in a single struct. This
-- extension provides a way to update a fixed set of descriptors in a
-- single 'Vulkan.Core10.Handles.DescriptorSet' with a pointer to an
-- application-defined data structure describing the new descriptors.
--
-- == Promotion to Vulkan 1.1
--
-- 'cmdPushDescriptorSetWithTemplateKHR' is included as an interaction with
-- @VK_KHR_push_descriptor@. If Vulkan 1.1 and @VK_KHR_push_descriptor@ are
-- supported, this is included by @VK_KHR_push_descriptor@.
--
-- The base functionality in this extension is included in core Vulkan 1.1,
-- with the KHR suffix omitted. The original type, enum, and command names
-- are still available as aliases of the core functionality.
--
-- == New Object Types
--
-- -   'DescriptorUpdateTemplateKHR'
--
-- == New Commands
--
-- -   'createDescriptorUpdateTemplateKHR'
--
-- -   'destroyDescriptorUpdateTemplateKHR'
--
-- -   'updateDescriptorSetWithTemplateKHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_push_descriptor VK_KHR_push_descriptor>
-- is supported:
--
-- -   'cmdPushDescriptorSetWithTemplateKHR'
--
-- == New Structures
--
-- -   'DescriptorUpdateTemplateCreateInfoKHR'
--
-- -   'DescriptorUpdateTemplateEntryKHR'
--
-- == New Enums
--
-- -   'DescriptorUpdateTemplateTypeKHR'
--
-- == New Bitmasks
--
-- -   'DescriptorUpdateTemplateCreateFlagsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME'
--
-- -   'KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core11.Enums.DescriptorUpdateTemplateType.DescriptorUpdateTemplateType':
--
--     -   'DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.ObjectType.ObjectType':
--
--     -   'OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_debug_report VK_EXT_debug_report>
-- is supported:
--
-- -   Extending
--     'Vulkan.Extensions.VK_EXT_debug_report.DebugReportObjectTypeEXT':
--
--     -   'DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_push_descriptor VK_KHR_push_descriptor>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core11.Enums.DescriptorUpdateTemplateType.DescriptorUpdateTemplateType':
--
--     -   'Vulkan.Core11.Enums.DescriptorUpdateTemplateType.DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR'
--
-- == Version History
--
-- -   Revision 1, 2016-01-11 (Markus Tavenrath)
--
--     -   Initial draft
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_descriptor_update_template Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_descriptor_update_template  ( cmdPushDescriptorSetWithTemplateKHR
                                                            , pattern STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO_KHR
                                                            , pattern OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR
                                                            , pattern DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR
                                                            , pattern DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR_EXT
                                                            , createDescriptorUpdateTemplateKHR
                                                            , destroyDescriptorUpdateTemplateKHR
                                                            , updateDescriptorSetWithTemplateKHR
                                                            , DescriptorUpdateTemplateCreateFlagsKHR
                                                            , DescriptorUpdateTemplateKHR
                                                            , DescriptorUpdateTemplateTypeKHR
                                                            , DescriptorUpdateTemplateEntryKHR
                                                            , DescriptorUpdateTemplateCreateInfoKHR
                                                            , KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION
                                                            , pattern KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION
                                                            , KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME
                                                            , pattern KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME
                                                            , DebugReportObjectTypeEXT(..)
                                                            ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template (createDescriptorUpdateTemplate)
import Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template (destroyDescriptorUpdateTemplate)
import Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template (updateDescriptorSetWithTemplate)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core11.Handles (DescriptorUpdateTemplate)
import Vulkan.Core11.Handles (DescriptorUpdateTemplate(..))
import Vulkan.Core11.Enums.DescriptorUpdateTemplateCreateFlags (DescriptorUpdateTemplateCreateFlags)
import Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template (DescriptorUpdateTemplateCreateInfo)
import Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template (DescriptorUpdateTemplateEntry)
import Vulkan.Core11.Enums.DescriptorUpdateTemplateType (DescriptorUpdateTemplateType)
import Vulkan.Dynamic (DeviceCmds(pVkCmdPushDescriptorSetWithTemplateKHR))
import Vulkan.Core10.Handles (PipelineLayout)
import Vulkan.Core10.Handles (PipelineLayout(..))
import Vulkan.Extensions.VK_EXT_debug_report (DebugReportObjectTypeEXT(DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT))
import Vulkan.Core11.Enums.DescriptorUpdateTemplateType (DescriptorUpdateTemplateType(DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO))
import Vulkan.Extensions.VK_EXT_debug_report (DebugReportObjectTypeEXT(..))
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
-- -   #VUID-vkCmdPushDescriptorSetWithTemplateKHR-layout-07993# @layout@
--     /must/ be compatible with the layout used to create
--     @descriptorUpdateTemplate@
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplateKHR-descriptorUpdateTemplate-07994#
--     @descriptorUpdateTemplate@ /must/ have been created with a
--     @templateType@ of
--     'Vulkan.Core11.Enums.DescriptorUpdateTemplateType.DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR'
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplateKHR-set-07995# @set@ /must/
--     be the same value used to create @descriptorUpdateTemplate@
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplateKHR-set-07304# @set@ /must/
--     be less than
--     'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo'::@setLayoutCount@
--     provided when @layout@ was created
--
-- -   #VUID-vkCmdPushDescriptorSetWithTemplateKHR-set-07305# @set@ /must/
--     be the unique set number in the pipeline layout that uses a
--     descriptor set layout that was created with
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR'
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
-- -   #VUID-vkCmdPushDescriptorSetWithTemplateKHR-videocoding# This
--     command /must/ only be called outside of a video coding scope
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
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
-- >     .templateType = VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR,
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
cmdPushDescriptorSetWithTemplateKHR commandBuffer
                                      descriptorUpdateTemplate
                                      layout
                                      set
                                      data' = liftIO $ do
  let vkCmdPushDescriptorSetWithTemplateKHRPtr = pVkCmdPushDescriptorSetWithTemplateKHR (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdPushDescriptorSetWithTemplateKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdPushDescriptorSetWithTemplateKHR is null" Nothing Nothing
  let vkCmdPushDescriptorSetWithTemplateKHR' = mkVkCmdPushDescriptorSetWithTemplateKHR vkCmdPushDescriptorSetWithTemplateKHRPtr
  traceAroundEvent "vkCmdPushDescriptorSetWithTemplateKHR" (vkCmdPushDescriptorSetWithTemplateKHR'
                                                              (commandBufferHandle (commandBuffer))
                                                              (descriptorUpdateTemplate)
                                                              (layout)
                                                              (set)
                                                              (data'))
  pure $ ()


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO_KHR = STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO


-- No documentation found for TopLevel "VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR"
pattern OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR = OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE


-- No documentation found for TopLevel "VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR"
pattern DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR = DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET


-- No documentation found for TopLevel "VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR_EXT = DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT


-- No documentation found for TopLevel "vkCreateDescriptorUpdateTemplateKHR"
createDescriptorUpdateTemplateKHR = createDescriptorUpdateTemplate


-- No documentation found for TopLevel "vkDestroyDescriptorUpdateTemplateKHR"
destroyDescriptorUpdateTemplateKHR = destroyDescriptorUpdateTemplate


-- No documentation found for TopLevel "vkUpdateDescriptorSetWithTemplateKHR"
updateDescriptorSetWithTemplateKHR = updateDescriptorSetWithTemplate


-- No documentation found for TopLevel "VkDescriptorUpdateTemplateCreateFlagsKHR"
type DescriptorUpdateTemplateCreateFlagsKHR = DescriptorUpdateTemplateCreateFlags


-- No documentation found for TopLevel "VkDescriptorUpdateTemplateKHR"
type DescriptorUpdateTemplateKHR = DescriptorUpdateTemplate


-- No documentation found for TopLevel "VkDescriptorUpdateTemplateTypeKHR"
type DescriptorUpdateTemplateTypeKHR = DescriptorUpdateTemplateType


-- No documentation found for TopLevel "VkDescriptorUpdateTemplateEntryKHR"
type DescriptorUpdateTemplateEntryKHR = DescriptorUpdateTemplateEntry


-- No documentation found for TopLevel "VkDescriptorUpdateTemplateCreateInfoKHR"
type DescriptorUpdateTemplateCreateInfoKHR = DescriptorUpdateTemplateCreateInfo


type KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION"
pattern KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION = 1


type KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME = "VK_KHR_descriptor_update_template"

-- No documentation found for TopLevel "VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME"
pattern KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME = "VK_KHR_descriptor_update_template"

