{-# language CPP #-}
-- | = Name
--
-- VK_EXT_subpass_merge_feedback - device extension
--
-- == VK_EXT_subpass_merge_feedback
--
-- [__Name String__]
--     @VK_EXT_subpass_merge_feedback@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     459
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Contact__]
--
--     -   Ting Wei
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_subpass_merge_feedback] @catweiting%0A<<Here describe the issue or question you have about the VK_EXT_subpass_merge_feedback extension>> >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_subpass_merge_feedback.asciidoc VK_EXT_subpass_merge_feedback>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-03-10
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Jorg Wagner, Arm
--
--     -   Ting Wei, Arm
--
-- == Description
--
-- This extension adds a mechanism to provide feedback to an application
-- about whether the subpasses specified on render pass creation are merged
-- by the implementation. Additionally, it provides a control to enable or
-- disable subpass merging in the render pass.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceSubpassMergeFeedbackFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.RenderPassCreateInfo2',
--     'RenderPassCreationControlEXT':
--
--     -   'RenderPassCreationFeedbackInfoEXT'
--
-- -   Extending
--     'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.RenderPassCreateInfo2',
--     'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.SubpassDescription2':
--
--     -   'RenderPassCreationControlEXT'
--
-- -   Extending
--     'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.SubpassDescription2',
--     'RenderPassCreationControlEXT':
--
--     -   'RenderPassSubpassFeedbackInfoEXT'
--
-- == New Enums
--
-- -   'SubpassMergeStatusEXT'
--
-- == New Enum Constants
--
-- -   'EXT_SUBPASS_MERGE_FEEDBACK_EXTENSION_NAME'
--
-- -   'EXT_SUBPASS_MERGE_FEEDBACK_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBPASS_MERGE_FEEDBACK_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDER_PASS_CREATION_CONTROL_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDER_PASS_CREATION_FEEDBACK_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDER_PASS_SUBPASS_FEEDBACK_INFO_EXT'
--
-- == Version History
--
-- -   Revision 1, 2022-03-10
--
--     -   Initial draft.
--
-- == See Also
--
-- 'PhysicalDeviceSubpassMergeFeedbackFeaturesEXT',
-- 'RenderPassCreationControlEXT', 'RenderPassCreationFeedbackInfoEXT',
-- 'RenderPassSubpassFeedbackInfoEXT', 'SubpassMergeStatusEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_subpass_merge_feedback Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_subpass_merge_feedback  ( PhysicalDeviceSubpassMergeFeedbackFeaturesEXT
                                                        , RenderPassCreationControlEXT
                                                        , RenderPassCreationFeedbackInfoEXT
                                                        , RenderPassSubpassFeedbackInfoEXT
                                                        ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
data PhysicalDeviceSubpassMergeFeedbackFeaturesEXT

instance ToCStruct PhysicalDeviceSubpassMergeFeedbackFeaturesEXT
instance Show PhysicalDeviceSubpassMergeFeedbackFeaturesEXT

instance FromCStruct PhysicalDeviceSubpassMergeFeedbackFeaturesEXT


type role RenderPassCreationControlEXT nominal
data RenderPassCreationControlEXT (es :: [Type])

instance (Extendss RenderPassCreationControlEXT es, PokeChain es) => ToCStruct (RenderPassCreationControlEXT es)
instance Show (Chain es) => Show (RenderPassCreationControlEXT es)

instance (Extendss RenderPassCreationControlEXT es, PeekChain es) => FromCStruct (RenderPassCreationControlEXT es)


data RenderPassCreationFeedbackInfoEXT

instance ToCStruct RenderPassCreationFeedbackInfoEXT
instance Show RenderPassCreationFeedbackInfoEXT

instance FromCStruct RenderPassCreationFeedbackInfoEXT


data RenderPassSubpassFeedbackInfoEXT

instance ToCStruct RenderPassSubpassFeedbackInfoEXT
instance Show RenderPassSubpassFeedbackInfoEXT

instance FromCStruct RenderPassSubpassFeedbackInfoEXT

