{-# language CPP #-}
-- | = Name
--
-- VK_EXT_present_mode_fifo_latest_ready - device extension
--
-- = VK_EXT_present_mode_fifo_latest_ready
--
-- [__Name String__]
--     @VK_EXT_present_mode_fifo_latest_ready@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     362
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_present_mode_fifo_latest_ready VK_KHR_present_mode_fifo_latest_ready>
--         extension
--
-- [__Contact__]
--
--     -   Lionel Duc
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_present_mode_fifo_latest_ready] @nvlduc%0A*Here describe the issue or question you have about the VK_EXT_present_mode_fifo_latest_ready extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_present_mode_fifo_latest_ready.adoc VK_EXT_present_mode_fifo_latest_ready>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-05-28
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   James Jones, NVIDIA
--
--     -   Lionel Duc, NVIDIA
--
-- == Description
--
-- This device extension adds a new present mode,
-- 'PRESENT_MODE_FIFO_LATEST_READY_EXT'.
--
-- This tear-free present mode behaves much like
-- 'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_FIFO_KHR', except that
-- each vertical blanking period dequeues consecutive present requests
-- until the latest ready is found to update the current image.
--
-- While this seems similar in concept to
-- 'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_MAILBOX_KHR', the
-- fundamental difference is that the processing of the present requests is
-- done during vblank. From the application perspective, this means for
-- example, that in a flip-based model, a single vblank /may/ cause
-- multiple swapchain images to be released at once, while
-- 'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_MAILBOX_KHR' /may/
-- continuously be releasing images as new requests become ready.
--
-- This additional present mode is useful when using a time-based present
-- API.
--
-- == Promotion to @VK_KHR_present_mode_fifo_latest_ready@
--
-- All functionality in this extension is included in
-- @VK_KHR_present_mode_fifo_latest_ready@, with the suffix changed to KHR.
-- The original type and enum names are still available as aliases of the
-- KHR names.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePresentModeFifoLatestReadyFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_PRESENT_MODE_FIFO_LATEST_READY_EXTENSION_NAME'
--
-- -   'EXT_PRESENT_MODE_FIFO_LATEST_READY_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Extensions.VK_KHR_surface.PresentModeKHR':
--
--     -   'PRESENT_MODE_FIFO_LATEST_READY_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_MODE_FIFO_LATEST_READY_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2024-05-28 (Lionel Duc)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_present_mode_fifo_latest_ready Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_present_mode_fifo_latest_ready  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_MODE_FIFO_LATEST_READY_FEATURES_EXT
                                                                , pattern PRESENT_MODE_FIFO_LATEST_READY_EXT
                                                                , PhysicalDevicePresentModeFifoLatestReadyFeaturesEXT
                                                                , EXT_PRESENT_MODE_FIFO_LATEST_READY_SPEC_VERSION
                                                                , pattern EXT_PRESENT_MODE_FIFO_LATEST_READY_SPEC_VERSION
                                                                , EXT_PRESENT_MODE_FIFO_LATEST_READY_EXTENSION_NAME
                                                                , pattern EXT_PRESENT_MODE_FIFO_LATEST_READY_EXTENSION_NAME
                                                                , PhysicalDevicePresentModeFifoLatestReadyFeaturesKHR(..)
                                                                , PresentModeKHR(..)
                                                                ) where

import Data.String (IsString)
import Vulkan.Extensions.VK_KHR_present_mode_fifo_latest_ready (PhysicalDevicePresentModeFifoLatestReadyFeaturesKHR)
import Vulkan.Extensions.VK_KHR_surface (PresentModeKHR(PRESENT_MODE_FIFO_LATEST_READY_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_MODE_FIFO_LATEST_READY_FEATURES_KHR))
import Vulkan.Extensions.VK_KHR_present_mode_fifo_latest_ready (PhysicalDevicePresentModeFifoLatestReadyFeaturesKHR(..))
import Vulkan.Extensions.VK_KHR_surface (PresentModeKHR(..))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_MODE_FIFO_LATEST_READY_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_MODE_FIFO_LATEST_READY_FEATURES_EXT = STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_MODE_FIFO_LATEST_READY_FEATURES_KHR


-- No documentation found for TopLevel "VK_PRESENT_MODE_FIFO_LATEST_READY_EXT"
pattern PRESENT_MODE_FIFO_LATEST_READY_EXT = PRESENT_MODE_FIFO_LATEST_READY_KHR


-- No documentation found for TopLevel "VkPhysicalDevicePresentModeFifoLatestReadyFeaturesEXT"
type PhysicalDevicePresentModeFifoLatestReadyFeaturesEXT = PhysicalDevicePresentModeFifoLatestReadyFeaturesKHR


type EXT_PRESENT_MODE_FIFO_LATEST_READY_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_PRESENT_MODE_FIFO_LATEST_READY_SPEC_VERSION"
pattern EXT_PRESENT_MODE_FIFO_LATEST_READY_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_PRESENT_MODE_FIFO_LATEST_READY_SPEC_VERSION = 1


type EXT_PRESENT_MODE_FIFO_LATEST_READY_EXTENSION_NAME = "VK_EXT_present_mode_fifo_latest_ready"

-- No documentation found for TopLevel "VK_EXT_PRESENT_MODE_FIFO_LATEST_READY_EXTENSION_NAME"
pattern EXT_PRESENT_MODE_FIFO_LATEST_READY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_PRESENT_MODE_FIFO_LATEST_READY_EXTENSION_NAME = "VK_EXT_present_mode_fifo_latest_ready"

