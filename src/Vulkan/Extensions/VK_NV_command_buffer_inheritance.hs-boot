{-# language CPP #-}
-- | = Name
--
-- VK_NV_command_buffer_inheritance - device extension
--
-- = VK_NV_command_buffer_inheritance
--
-- [__Name String__]
--     @VK_NV_command_buffer_inheritance@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     560
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
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Contact__]
--
--     -   Lujin Wang
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_command_buffer_inheritance] @lujinwangnv%0A*Here describe the issue or question you have about the VK_NV_command_buffer_inheritance extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-02-15
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Piers Daniell, NVIDIA
--
--     -   Daniel Story, Nintendo
--
-- == Description
--
-- This extension allows applications to take advantage of the graphics and
-- compute state that remains valid in the queue between executions of
-- submitted command buffers. This works across both primary and secondary
-- command buffers.
--
-- The state inherited includes the previously bound pipeline state,
-- previously bound shader objects, previously bound vertex and index
-- buffers, previously bound descriptor sets and push constants, and all
-- previously set dynamic state.
--
-- This extension relaxes the requirement that all that state needs to be
-- bound and set after begin command buffer and before the next draw or
-- dispatch.
--
-- By not having to set state that has been inherited applications can save
-- both CPU and GPU cycles by not having to set state redundantly, and also
-- have improved flexibility when reusing secondary command buffers.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceCommandBufferInheritanceFeaturesNV'
--
-- == New Enum Constants
--
-- -   'NV_COMMAND_BUFFER_INHERITANCE_EXTENSION_NAME'
--
-- -   'NV_COMMAND_BUFFER_INHERITANCE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COMMAND_BUFFER_INHERITANCE_FEATURES_NV'
--
-- == Issues
--
-- 1) How can the validation layer know when state is valid at draw or
-- dispatch time if it is inherited at execution time?
--
-- __RESOLVED__: Validation of invalid state at draw and dispatch time
-- cannot be done while recording those commands. Instead the validation
-- layer will need to keep track of any unset state when draw and dispatch
-- commands are recorded, but not report an error at that time. It should
-- also keep track of what state is valid at the end of each recorded
-- command buffer. When secondary command buffer execution is recorded the
-- validation layer can update its unset state tracking for that command
-- buffer, and also for draw and dispatch commands recorded after execution
-- of the secondary as they will inherit state from the executed secondary.
-- This can be done recursively so every recorded primary command buffer
-- has a final tally of any unset state used at draw and dispatch time.
-- Finally when the primary is submitted to the queue the validation layer
-- will know the previous primaries submitted to the queue and will know if
-- there is any unset state used and can report the error then.
--
-- == Version History
--
-- -   Revision 1, 2024-02-15 (Lujin Wang)
--
--     -   Internal revisions
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_NV_command_buffer_inheritance Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_command_buffer_inheritance  (PhysicalDeviceCommandBufferInheritanceFeaturesNV) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceCommandBufferInheritanceFeaturesNV

instance ToCStruct PhysicalDeviceCommandBufferInheritanceFeaturesNV
instance Show PhysicalDeviceCommandBufferInheritanceFeaturesNV

instance FromCStruct PhysicalDeviceCommandBufferInheritanceFeaturesNV

