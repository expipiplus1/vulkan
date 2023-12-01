{-# language CPP #-}
-- | = Name
--
-- VK_EXT_frame_boundary - device extension
--
-- == VK_EXT_frame_boundary
--
-- [__Name String__]
--     @VK_EXT_frame_boundary@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     376
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
-- [__Contact__]
--
--     -   James Fitzpatrick
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_frame_boundary] @jamesfitzpatrick%0A*Here describe the issue or question you have about the VK_EXT_frame_boundary extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_frame_boundary.adoc VK_EXT_frame_boundary>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-06-14
--
-- [__Contributors__]
--
--     -   James Fitzpatrick, Imagination Technologies
--
--     -   Hugues Evrard, Google
--
--     -   Melih Yasin Yalcin, Google
--
--     -   Andrew Garrard, Imagination Technologies
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Vassili Nikolaev, NVIDIA
--
--     -   Ting Wei, Huawei
--
-- == Description
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_frame_boundary VK_EXT_frame_boundary>
-- is a device extension that helps __tools__ (such as debuggers) to group
-- queue submissions per frames in non-trivial scenarios, typically when
-- 'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR' is not a relevant
-- frame boundary delimiter.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceFrameBoundaryFeaturesEXT'
--
-- -   Extending 'Vulkan.Core10.Queue.SubmitInfo',
--     'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.SubmitInfo2',
--     'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR',
--     'Vulkan.Core10.SparseResourceMemoryManagement.BindSparseInfo':
--
--     -   'FrameBoundaryEXT'
--
-- == New Enums
--
-- -   'FrameBoundaryFlagBitsEXT'
--
-- == New Bitmasks
--
-- -   'FrameBoundaryFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_FRAME_BOUNDARY_EXTENSION_NAME'
--
-- -   'EXT_FRAME_BOUNDARY_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_FRAME_BOUNDARY_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAME_BOUNDARY_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 0, 2022-01-14 (Hugues Evard)
--
--     -   Initial proposal
--
-- -   Revision 1, 2023-06-14 (James Fitzpatrick)
--
--     -   Initial draft
--
-- == See Also
--
-- 'FrameBoundaryEXT', 'FrameBoundaryFlagBitsEXT', 'FrameBoundaryFlagsEXT',
-- 'PhysicalDeviceFrameBoundaryFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_frame_boundary Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_frame_boundary  ( FrameBoundaryEXT
                                                , PhysicalDeviceFrameBoundaryFeaturesEXT
                                                ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data FrameBoundaryEXT

instance ToCStruct FrameBoundaryEXT
instance Show FrameBoundaryEXT

instance FromCStruct FrameBoundaryEXT


data PhysicalDeviceFrameBoundaryFeaturesEXT

instance ToCStruct PhysicalDeviceFrameBoundaryFeaturesEXT
instance Show PhysicalDeviceFrameBoundaryFeaturesEXT

instance FromCStruct PhysicalDeviceFrameBoundaryFeaturesEXT

