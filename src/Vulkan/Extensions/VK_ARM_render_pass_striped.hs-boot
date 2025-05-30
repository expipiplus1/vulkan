{-# language CPP #-}
-- | = Name
--
-- VK_ARM_render_pass_striped - device extension
--
-- == VK_ARM_render_pass_striped
--
-- [__Name String__]
--     @VK_ARM_render_pass_striped@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     425
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>
--
-- [__Contact__]
--
--     -   Jan-Harald Fredriksen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_ARM_render_pass_striped] @janharaldfredriksen-arm%0A*Here describe the issue or question you have about the VK_ARM_render_pass_striped extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_ARM_render_pass_striped.adoc VK_ARM_render_pass_striped>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-11-21
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Lisa Wu, Arm
--
--     -   Torbjorn Nilsson, Arm
--
--     -   Ying-Chieh Chen, Mediatek
--
--     -   Jim Chiu, Mediatek
--
-- == Description
--
-- This extension adds the ability to split a render pass instance into
-- stripes, and to get a notification when rendering has completed for each
-- stripe.
--
-- == New Structures
--
-- -   'RenderPassStripeInfoARM'
--
-- -   Extending
--     'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.CommandBufferSubmitInfo':
--
--     -   'RenderPassStripeSubmitInfoARM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceRenderPassStripedFeaturesARM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceRenderPassStripedPropertiesARM'
--
-- -   Extending
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo',
--     'Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo':
--
--     -   'RenderPassStripeBeginInfoARM'
--
-- == New Enum Constants
--
-- -   'ARM_RENDER_PASS_STRIPED_EXTENSION_NAME'
--
-- -   'ARM_RENDER_PASS_STRIPED_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_RENDER_PASS_STRIPED_FEATURES_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_RENDER_PASS_STRIPED_PROPERTIES_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDER_PASS_STRIPE_BEGIN_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDER_PASS_STRIPE_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDER_PASS_STRIPE_SUBMIT_INFO_ARM'
--
-- == Examples
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2023-11-21
--
--     -   Initial revision
--
-- == See Also
--
-- 'PhysicalDeviceRenderPassStripedFeaturesARM',
-- 'PhysicalDeviceRenderPassStripedPropertiesARM',
-- 'RenderPassStripeBeginInfoARM', 'RenderPassStripeInfoARM',
-- 'RenderPassStripeSubmitInfoARM'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_ARM_render_pass_striped Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_ARM_render_pass_striped  ( PhysicalDeviceRenderPassStripedFeaturesARM
                                                     , PhysicalDeviceRenderPassStripedPropertiesARM
                                                     , RenderPassStripeBeginInfoARM
                                                     , RenderPassStripeInfoARM
                                                     , RenderPassStripeSubmitInfoARM
                                                     ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceRenderPassStripedFeaturesARM

instance ToCStruct PhysicalDeviceRenderPassStripedFeaturesARM
instance Show PhysicalDeviceRenderPassStripedFeaturesARM

instance FromCStruct PhysicalDeviceRenderPassStripedFeaturesARM


data PhysicalDeviceRenderPassStripedPropertiesARM

instance ToCStruct PhysicalDeviceRenderPassStripedPropertiesARM
instance Show PhysicalDeviceRenderPassStripedPropertiesARM

instance FromCStruct PhysicalDeviceRenderPassStripedPropertiesARM


data RenderPassStripeBeginInfoARM

instance ToCStruct RenderPassStripeBeginInfoARM
instance Show RenderPassStripeBeginInfoARM

instance FromCStruct RenderPassStripeBeginInfoARM


data RenderPassStripeInfoARM

instance ToCStruct RenderPassStripeInfoARM
instance Show RenderPassStripeInfoARM

instance FromCStruct RenderPassStripeInfoARM


data RenderPassStripeSubmitInfoARM

instance ToCStruct RenderPassStripeSubmitInfoARM
instance Show RenderPassStripeSubmitInfoARM

instance FromCStruct RenderPassStripeSubmitInfoARM

