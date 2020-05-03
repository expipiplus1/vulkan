{-# language CPP #-}
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

