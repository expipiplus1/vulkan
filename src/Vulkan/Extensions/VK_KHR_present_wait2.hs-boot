{-# language CPP #-}
-- | = Name
--
-- VK_KHR_present_wait2 - device extension
--
-- = VK_KHR_present_wait2
--
-- [__Name String__]
--     @VK_KHR_present_wait2@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     481
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_surface_capabilities2 VK_KHR_get_surface_capabilities2>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface VK_KHR_surface>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_present_id2 VK_KHR_present_id2>
--
-- [__Contact__]
--
--     -   Daniel Stone
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_present_wait2.adoc VK_KHR_present_wait2>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-05-30
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   James Jones, NVIDIA
--
--     -   Daniel Stone, Collabora
--
--     -   Derek Foreman, Collabora
--
--     -   /contributors to
--         \`<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_present_wait VK_KHR_present_wait>\`/
--
-- == Description
--
-- This device extension allows an application that uses the
-- @VK_KHR_swapchain@ extension to wait for present operations to complete.
-- An application can use this to monitor and control the pacing of the
-- application by managing the number of outstanding images yet to be
-- presented.
--
-- == New Commands
--
-- -   'waitForPresent2KHR'
--
-- == New Structures
--
-- -   'PresentWait2InfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePresentWait2FeaturesKHR'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.SurfaceCapabilities2KHR':
--
--     -   'SurfaceCapabilitiesPresentWait2KHR'
--
-- == New Enum Constants
--
-- -   'KHR_PRESENT_WAIT_2_EXTENSION_NAME'
--
-- -   'KHR_PRESENT_WAIT_2_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_WAIT_2_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PRESENT_WAIT_2_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SURFACE_CAPABILITIES_PRESENT_WAIT_2_KHR'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateFlagBitsKHR':
--
--     -   'Vulkan.Extensions.VK_KHR_swapchain.SWAPCHAIN_CREATE_PRESENT_WAIT_2_BIT_KHR'
--
-- == Issues
--
-- 1) When does the wait finish?
--
-- __RESOLVED__. The wait request will complete when the timeout expires,
-- or after the corresponding presentation request has either taken effect
-- within the presentation engine or has been replaced without
-- presentation. Additionally, a wait may complete immediately if the
-- swapchain becomes out of date.
--
-- In circumstances outside the application’s control, this wait may be
-- particularly long. For example, a user session may have the display
-- locked and switched off for multiple days. During this time, the latest
-- image presented through the WSI will never be presented to the user
-- (because nothing is being presented), or replaced (because nothing newer
-- has been queued by the application). Each operating system may have a
-- separate mechanism to inform the application of states such as these,
-- however it is out of scope of the Vulkan WSI.
--
-- There is no requirement for any precise timing relationship between the
-- presentation of the image to the user and the end of the wait.
--
-- This extension is not intended for time-to-light estimation, which is
-- better solved by a separate extension dedicated to present-timing
-- feedback for audio\/visual\/input synchronization.
--
-- 2) Should this use fences or other existing synchronization mechanism?
--
-- __RESOLVED__. VkFence is a legacy primitive. Building a new API around a
-- legacy primitive is undesirable.
--
-- Other existing synchronization mechanisms may lack a platform-provided
-- framework for sharing synchronization objects between display and render
-- drivers.
--
-- For these reasons, this extension will provide a separate
-- synchronization API.
--
-- 3) Should this extension share present identification with other
-- extensions?
--
-- __RESOLVED__. Yes. A new extension, @VK_KHR_present_id2@, should be
-- created to provide a shared structure for presentation identifiers.
--
-- 4) What happens when presentations complete out of order with respect to
-- calls to vkQueuePresent? This could happen if the semaphores for the
-- presentations were ready out of order.
--
-- __OPTION A__: Require that when a PresentId is set that the driver
-- ensure that images are always presented in the order of calls to
-- vkQueuePresent.
--
-- __OPTION B__: Finish both waits when the earliest present completes.
-- This will complete the later present wait earlier than the actual
-- presentation. This should be the easiest to implement as the driver need
-- only track the largest present ID completed. This is also the
-- \'natural\' consequence of interpreting the existing vkWaitForPresentKHR
-- specification.
--
-- __OPTION C__: Finish both waits when both have completed. This will
-- complete the earlier presentation later than the actual presentation
-- time. This is allowed by the current specification as there is no
-- precise timing requirement for when the presentId value is updated. This
-- requires slightly more complexity in the driver as it will need to track
-- all outstanding presentId values.
--
-- __OPTION D__: The order of completion between outstanding
-- 'waitForPresent2KHR' calls is always undefined. However, a
-- 'Vulkan.Core10.Enums.Result.SUCCESS' return value in
-- 'PresentWait2InfoKHR'::@presentId@ implies that future calls to
-- 'waitForPresent2KHR' where 'PresentWait2InfoKHR'::@presentId@ is less
-- than or equal to N will complete immediately.
--
-- __RESOLVED__. __OPTION D__: This option ensures implementations do not
-- need to create complex internal queues to generate signals in the right
-- order.
--
-- 5) Should this extension deviate from @VK_KHR_present_wait@ and require
-- the presentation engine to provide the presentId values?
--
-- __RESOLVED__. No. This extension is intended to be a bugfix of
-- @VK_KHR_present_wait@, and existing low-latency apis require an
-- application provided id. At least on some platforms, a mapping mechanism
-- would be required to translate between presentation engine and
-- application ids. This exceeds the intended scope of this extension.
--
-- When needed in the future, we can introduce an independent presentation
-- engine driven id and a mechanism for mapping presentation engine ids to
-- application provided ids.
--
-- == Version History
--
-- -   Revision 1, 2022-10-05 (Daniel Stone)
--
--     -   Repurposed from VK_KHR_present_wait to be based on surface
--         capabilities
--
--     -   Reworded wait finish section to avoid time-to-light
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_present_wait2 Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_present_wait2  ( PhysicalDevicePresentWait2FeaturesKHR
                                               , PresentWait2InfoKHR
                                               , SurfaceCapabilitiesPresentWait2KHR
                                               ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDevicePresentWait2FeaturesKHR

instance ToCStruct PhysicalDevicePresentWait2FeaturesKHR
instance Show PhysicalDevicePresentWait2FeaturesKHR

instance FromCStruct PhysicalDevicePresentWait2FeaturesKHR


data PresentWait2InfoKHR

instance ToCStruct PresentWait2InfoKHR
instance Show PresentWait2InfoKHR

instance FromCStruct PresentWait2InfoKHR


data SurfaceCapabilitiesPresentWait2KHR

instance ToCStruct SurfaceCapabilitiesPresentWait2KHR
instance Show SurfaceCapabilitiesPresentWait2KHR

instance FromCStruct SurfaceCapabilitiesPresentWait2KHR

