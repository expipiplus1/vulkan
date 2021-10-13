{-# language CPP #-}
-- | = Name
--
-- VK_NV_inherited_viewport_scissor - device extension
--
-- == VK_NV_inherited_viewport_scissor
--
-- [__Name String__]
--     @VK_NV_inherited_viewport_scissor@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     279
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
--     -   David Zhao Akeley
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_inherited_viewport_scissor] @akeley98%0A<<Here describe the issue or question you have about the VK_NV_inherited_viewport_scissor extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-02-04
--
-- [__Contributors__]
--
--     -   David Zhao Akeley, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Piers Daniell, NVIDIA
--
--     -   Christoph Kubisch, NVIDIA
--
-- == Description
--
-- This extension adds the ability for a secondary command buffer to
-- inherit the dynamic viewport and scissor state from a primary command
-- buffer, or a previous secondary command buffer executed within the same
-- 'Vulkan.Core10.CommandBufferBuilding.cmdExecuteCommands' call. It
-- addresses a frequent scenario in applications that deal with window
-- resizing and want to improve utilization of re-usable secondary command
-- buffers. The functionality is provided through
-- 'CommandBufferInheritanceViewportScissorInfoNV'. Viewport inheritance is
-- effectively limited to the 2D rectangle; secondary command buffers must
-- re-specify the inherited depth range values.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo':
--
--     -   'CommandBufferInheritanceViewportScissorInfoNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceInheritedViewportScissorFeaturesNV'
--
-- == New Enum Constants
--
-- -   'NV_INHERITED_VIEWPORT_SCISSOR_EXTENSION_NAME'
--
-- -   'NV_INHERITED_VIEWPORT_SCISSOR_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_VIEWPORT_SCISSOR_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_INHERITED_VIEWPORT_SCISSOR_FEATURES_NV'
--
-- == Issues
--
-- (1) Why are viewport depth values configured in the
-- 'CommandBufferInheritanceViewportScissorInfoNV' struct, rather than by a
-- @vkCmd…​@ function?
--
-- __DISCUSSION__:
--
-- We considered both adding a new @vkCmdSetViewportDepthNV@ function, and
-- modifying 'Vulkan.Core10.CommandBufferBuilding.cmdSetViewport' to ignore
-- the @x@, @y@, @width@, and @height@ values when called with a secondary
-- command buffer that activates this extension.
--
-- The primary design considerations for this extension are debuggability
-- and easy integration into existing applications. The main issue with
-- adding a new @vkCmdSetViewportDepthNV@ function is reducing
-- ease-of-integration. A new function pointer will have to be loaded, but
-- more importantly, a new function would require changes to be supported
-- in graphics debuggers; this would delay widespread adoption of the
-- extension.
--
-- The proposal to modify
-- 'Vulkan.Core10.CommandBufferBuilding.cmdSetViewport' would avoid these
-- issues. However, we expect that the intent of applications using this
-- extension is to have the viewport values used for drawing exactly match
-- the inherited values; thus, it would be better for debuggability if no
-- function for modifying the viewport depth alone is provided. By
-- specifying viewport depth values when starting secondary command buffer
-- recording, and requiring the specified depth values to match the
-- inherited depth values, we allow for validation layers that flag depth
-- changes as errors.
--
-- This design also better matches the hardware model. In fact, there is no
-- need to re-execute a depth-setting command. The graphics device retains
-- the viewport depth state; it is the CPU-side state of
-- 'Vulkan.Core10.Handles.CommandBuffer' that must be re-initialized.
--
-- (2) Why are viewport depth values specified as a partial
-- 'Vulkan.Core10.Pipeline.Viewport' struct, rather than a leaner
-- depth-only struct?
--
-- __DISCUSSION__:
--
-- We considered adding a new @VkViewportDepthNV@ struct containing only
-- @minDepth@ and @maxDepth@. However, as application developers would need
-- to maintain both a @VK_NV_inherited_viewport_scissor@ code path and a
-- fallback code path (at least in the short term), we ultimately chose to
-- continue using the existing 'Vulkan.Core10.Pipeline.Viewport' structure.
-- Doing so would allow application developers to reuse the same
-- 'Vulkan.Core10.Pipeline.Viewport' array for both code paths, rather than
-- constructing separate @VkViewportDepthNV@ and
-- 'Vulkan.Core10.Pipeline.Viewport' arrays for each code path.
--
-- == Version History
--
-- -   Revision 1, 2020-02-04 (David Zhao Akeley)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'CommandBufferInheritanceViewportScissorInfoNV',
-- 'PhysicalDeviceInheritedViewportScissorFeaturesNV'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_inherited_viewport_scissor Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_inherited_viewport_scissor  ( CommandBufferInheritanceViewportScissorInfoNV
                                                           , PhysicalDeviceInheritedViewportScissorFeaturesNV
                                                           ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data CommandBufferInheritanceViewportScissorInfoNV

instance ToCStruct CommandBufferInheritanceViewportScissorInfoNV
instance Show CommandBufferInheritanceViewportScissorInfoNV

instance FromCStruct CommandBufferInheritanceViewportScissorInfoNV


data PhysicalDeviceInheritedViewportScissorFeaturesNV

instance ToCStruct PhysicalDeviceInheritedViewportScissorFeaturesNV
instance Show PhysicalDeviceInheritedViewportScissorFeaturesNV

instance FromCStruct PhysicalDeviceInheritedViewportScissorFeaturesNV

