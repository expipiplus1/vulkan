{-# language CPP #-}
-- | = Name
--
-- VK_KHR_create_renderpass2 - device extension
--
-- == VK_KHR_create_renderpass2
--
-- [__Name String__]
--     @VK_KHR_create_renderpass2@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     110
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_multiview VK_KHR_multiview>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance2 VK_KHR_maintenance2>
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2-promotions Vulkan 1.2>
--
-- [__Contact__]
--
--     -   Tobias Hector
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_create_renderpass2] @tobias%0A*Here describe the issue or question you have about the VK_KHR_create_renderpass2 extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-02-07
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.2 Core
--
-- [__Contributors__]
--
--     -   Tobias Hector
--
--     -   Jeff Bolz
--
-- == Description
--
-- This extension provides a new entry point to create render passes in a
-- way that can be easily extended by other extensions through the
-- substructures of render pass creation. The Vulkan 1.0 render pass
-- creation sub-structures do not include @sType@\/@pNext@ members.
-- Additionally, the render pass begin\/next\/end commands have been
-- augmented with new extensible structures for passing additional subpass
-- information.
--
-- The
-- 'Vulkan.Core11.Promoted_From_VK_KHR_multiview.RenderPassMultiviewCreateInfo'
-- and
-- 'Vulkan.Core11.Promoted_From_VK_KHR_maintenance2.InputAttachmentAspectReference'
-- structures that extended the original
-- 'Vulkan.Core10.Pass.RenderPassCreateInfo' are not accepted into the new
-- creation functions, and instead their parameters are folded into this
-- extension as follows:
--
-- -   Elements of
--     'Vulkan.Core11.Promoted_From_VK_KHR_multiview.RenderPassMultiviewCreateInfo'::@pViewMasks@
--     are now specified in 'SubpassDescription2KHR'::@viewMask@.
--
-- -   Elements of
--     'Vulkan.Core11.Promoted_From_VK_KHR_multiview.RenderPassMultiviewCreateInfo'::@pViewOffsets@
--     are now specified in 'SubpassDependency2KHR'::@viewOffset@.
--
-- -   'Vulkan.Core11.Promoted_From_VK_KHR_multiview.RenderPassMultiviewCreateInfo'::@correlationMaskCount@
--     and
--     'Vulkan.Core11.Promoted_From_VK_KHR_multiview.RenderPassMultiviewCreateInfo'::@pCorrelationMasks@
--     are directly specified in 'RenderPassCreateInfo2KHR'.
--
-- -   'Vulkan.Core11.Promoted_From_VK_KHR_maintenance2.InputAttachmentAspectReference'::@aspectMask@
--     is now specified in the relevant input attachment reference in
--     'AttachmentReference2KHR'::@aspectMask@
--
-- The details of these mappings are explained fully in the new structures.
--
-- == Promotion to Vulkan 1.2
--
-- All functionality in this extension is included in core Vulkan 1.2, with
-- the KHR suffix omitted. The original type, enum and command names are
-- still available as aliases of the core functionality.
--
-- == New Commands
--
-- -   'cmdBeginRenderPass2KHR'
--
-- -   'cmdEndRenderPass2KHR'
--
-- -   'cmdNextSubpass2KHR'
--
-- -   'createRenderPass2KHR'
--
-- == New Structures
--
-- -   'AttachmentDescription2KHR'
--
-- -   'AttachmentReference2KHR'
--
-- -   'RenderPassCreateInfo2KHR'
--
-- -   'SubpassBeginInfoKHR'
--
-- -   'SubpassDependency2KHR'
--
-- -   'SubpassDescription2KHR'
--
-- -   'SubpassEndInfoKHR'
--
-- == New Enum Constants
--
-- -   'KHR_CREATE_RENDERPASS_2_EXTENSION_NAME'
--
-- -   'KHR_CREATE_RENDERPASS_2_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2_KHR'
--
--     -   'STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2_KHR'
--
--     -   'STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2_KHR'
--
--     -   'STRUCTURE_TYPE_SUBPASS_BEGIN_INFO_KHR'
--
--     -   'STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2_KHR'
--
--     -   'STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2_KHR'
--
--     -   'STRUCTURE_TYPE_SUBPASS_END_INFO_KHR'
--
-- == Version History
--
-- -   Revision 1, 2018-02-07 (Tobias Hector)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'AttachmentDescription2KHR', 'AttachmentReference2KHR',
-- 'RenderPassCreateInfo2KHR', 'SubpassBeginInfoKHR',
-- 'SubpassDependency2KHR', 'SubpassDescription2KHR', 'SubpassEndInfoKHR',
-- 'cmdBeginRenderPass2KHR', 'cmdEndRenderPass2KHR', 'cmdNextSubpass2KHR',
-- 'createRenderPass2KHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_create_renderpass2 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_create_renderpass2  ( pattern STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2_KHR
                                                    , pattern STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2_KHR
                                                    , pattern STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2_KHR
                                                    , pattern STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2_KHR
                                                    , pattern STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2_KHR
                                                    , pattern STRUCTURE_TYPE_SUBPASS_BEGIN_INFO_KHR
                                                    , pattern STRUCTURE_TYPE_SUBPASS_END_INFO_KHR
                                                    , createRenderPass2KHR
                                                    , cmdBeginRenderPass2KHR
                                                    , cmdNextSubpass2KHR
                                                    , cmdEndRenderPass2KHR
                                                    , AttachmentDescription2KHR
                                                    , AttachmentReference2KHR
                                                    , SubpassDescription2KHR
                                                    , SubpassDependency2KHR
                                                    , RenderPassCreateInfo2KHR
                                                    , SubpassBeginInfoKHR
                                                    , SubpassEndInfoKHR
                                                    , KHR_CREATE_RENDERPASS_2_SPEC_VERSION
                                                    , pattern KHR_CREATE_RENDERPASS_2_SPEC_VERSION
                                                    , KHR_CREATE_RENDERPASS_2_EXTENSION_NAME
                                                    , pattern KHR_CREATE_RENDERPASS_2_EXTENSION_NAME
                                                    ) where

import Data.String (IsString)
import Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2 (cmdBeginRenderPass2)
import Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2 (cmdEndRenderPass2)
import Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2 (cmdNextSubpass2)
import Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2 (createRenderPass2)
import Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2 (AttachmentDescription2)
import Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2 (AttachmentReference2)
import Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2 (RenderPassCreateInfo2)
import Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2 (SubpassBeginInfo)
import Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2 (SubpassDependency2)
import Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2 (SubpassDescription2)
import Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2 (SubpassEndInfo)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SUBPASS_BEGIN_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SUBPASS_END_INFO))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2_KHR"
pattern STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2_KHR = STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2_KHR"
pattern STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2_KHR = STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2_KHR"
pattern STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2_KHR = STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2_KHR"
pattern STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2_KHR = STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2_KHR"
pattern STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2_KHR = STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_SUBPASS_BEGIN_INFO_KHR"
pattern STRUCTURE_TYPE_SUBPASS_BEGIN_INFO_KHR = STRUCTURE_TYPE_SUBPASS_BEGIN_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_SUBPASS_END_INFO_KHR"
pattern STRUCTURE_TYPE_SUBPASS_END_INFO_KHR = STRUCTURE_TYPE_SUBPASS_END_INFO


-- No documentation found for TopLevel "vkCreateRenderPass2KHR"
createRenderPass2KHR = createRenderPass2


-- No documentation found for TopLevel "vkCmdBeginRenderPass2KHR"
cmdBeginRenderPass2KHR = cmdBeginRenderPass2


-- No documentation found for TopLevel "vkCmdNextSubpass2KHR"
cmdNextSubpass2KHR = cmdNextSubpass2


-- No documentation found for TopLevel "vkCmdEndRenderPass2KHR"
cmdEndRenderPass2KHR = cmdEndRenderPass2


-- No documentation found for TopLevel "VkAttachmentDescription2KHR"
type AttachmentDescription2KHR = AttachmentDescription2


-- No documentation found for TopLevel "VkAttachmentReference2KHR"
type AttachmentReference2KHR = AttachmentReference2


-- No documentation found for TopLevel "VkSubpassDescription2KHR"
type SubpassDescription2KHR = SubpassDescription2


-- No documentation found for TopLevel "VkSubpassDependency2KHR"
type SubpassDependency2KHR = SubpassDependency2


-- No documentation found for TopLevel "VkRenderPassCreateInfo2KHR"
type RenderPassCreateInfo2KHR = RenderPassCreateInfo2


-- No documentation found for TopLevel "VkSubpassBeginInfoKHR"
type SubpassBeginInfoKHR = SubpassBeginInfo


-- No documentation found for TopLevel "VkSubpassEndInfoKHR"
type SubpassEndInfoKHR = SubpassEndInfo


type KHR_CREATE_RENDERPASS_2_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_CREATE_RENDERPASS_2_SPEC_VERSION"
pattern KHR_CREATE_RENDERPASS_2_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_CREATE_RENDERPASS_2_SPEC_VERSION = 1


type KHR_CREATE_RENDERPASS_2_EXTENSION_NAME = "VK_KHR_create_renderpass2"

-- No documentation found for TopLevel "VK_KHR_CREATE_RENDERPASS_2_EXTENSION_NAME"
pattern KHR_CREATE_RENDERPASS_2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_CREATE_RENDERPASS_2_EXTENSION_NAME = "VK_KHR_create_renderpass2"

