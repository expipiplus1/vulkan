{-# language CPP #-}
-- | = Name
--
-- VK_EXT_conditional_rendering - device extension
--
-- == VK_EXT_conditional_rendering
--
-- [__Name String__]
--     @VK_EXT_conditional_rendering@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     82
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Contact__]
--
--     -   Vikram Kushwaha
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_conditional_rendering:%20&body=@vkushwaha%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-05-21
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Vikram Kushwaha, NVIDIA
--
--     -   Daniel Rakos, AMD
--
--     -   Jesse Hall, Google
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Piers Daniell, NVIDIA
--
--     -   Stuart Smith, Imagination Technologies
--
-- == Description
--
-- This extension allows the execution of one or more rendering commands to
-- be conditional on a value in buffer memory. This may help an application
-- reduce the latency by conditionally discarding rendering commands
-- without application intervention. The conditional rendering commands are
-- limited to draws, compute dispatches and clearing attachments within a
-- conditional rendering block.
--
-- == New Commands
--
-- -   'cmdBeginConditionalRenderingEXT'
--
-- -   'cmdEndConditionalRenderingEXT'
--
-- == New Structures
--
-- -   'ConditionalRenderingBeginInfoEXT'
--
-- -   Extending
--     'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo':
--
--     -   'CommandBufferInheritanceConditionalRenderingInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceConditionalRenderingFeaturesEXT'
--
-- == New Enums
--
-- -   'ConditionalRenderingFlagBitsEXT'
--
-- == New Bitmasks
--
-- -   'ConditionalRenderingFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_CONDITIONAL_RENDERING_EXTENSION_NAME'
--
-- -   'EXT_CONDITIONAL_RENDERING_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits':
--
--     -   'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BufferUsageFlagBits':
--
--     -   'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT'
--
-- == Issues
--
-- 1) Should conditional rendering affect copy and blit commands?
--
-- RESOLVED: Conditional rendering should not affect copies and blits.
--
-- 2) Should secondary command buffers be allowed to execute while
-- conditional rendering is active in the primary command buffer?
--
-- RESOLVED: The rendering commands in secondary command buffer will be
-- affected by an active conditional rendering in primary command buffer if
-- the @conditionalRenderingEnable@ is set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE'. Conditional rendering /must/ not
-- be active in the primary command buffer if @conditionalRenderingEnable@
-- is 'Vulkan.Core10.FundamentalTypes.FALSE'.
--
-- == Examples
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2018-04-19 (Vikram Kushwaha)
--
--     -   First Version
--
-- -   Revision 2, 2018-05-21 (Vikram Kushwaha)
--
--     -   Add new pipeline stage, access flags and limit conditional
--         rendering to a subpass or entire renderpass.
--
-- = See Also
--
-- 'CommandBufferInheritanceConditionalRenderingInfoEXT',
-- 'ConditionalRenderingBeginInfoEXT', 'ConditionalRenderingFlagBitsEXT',
-- 'ConditionalRenderingFlagsEXT',
-- 'PhysicalDeviceConditionalRenderingFeaturesEXT',
-- 'cmdBeginConditionalRenderingEXT', 'cmdEndConditionalRenderingEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_conditional_rendering Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_conditional_rendering  ( CommandBufferInheritanceConditionalRenderingInfoEXT
                                                       , ConditionalRenderingBeginInfoEXT
                                                       , PhysicalDeviceConditionalRenderingFeaturesEXT
                                                       ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data CommandBufferInheritanceConditionalRenderingInfoEXT

instance ToCStruct CommandBufferInheritanceConditionalRenderingInfoEXT
instance Show CommandBufferInheritanceConditionalRenderingInfoEXT

instance FromCStruct CommandBufferInheritanceConditionalRenderingInfoEXT


data ConditionalRenderingBeginInfoEXT

instance ToCStruct ConditionalRenderingBeginInfoEXT
instance Show ConditionalRenderingBeginInfoEXT

instance FromCStruct ConditionalRenderingBeginInfoEXT


data PhysicalDeviceConditionalRenderingFeaturesEXT

instance ToCStruct PhysicalDeviceConditionalRenderingFeaturesEXT
instance Show PhysicalDeviceConditionalRenderingFeaturesEXT

instance FromCStruct PhysicalDeviceConditionalRenderingFeaturesEXT

