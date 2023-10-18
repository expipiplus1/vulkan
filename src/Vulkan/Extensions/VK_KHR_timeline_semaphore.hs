{-# language CPP #-}
-- | = Name
--
-- VK_KHR_timeline_semaphore - device extension
--
-- == VK_KHR_timeline_semaphore
--
-- [__Name String__]
--     @VK_KHR_timeline_semaphore@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     208
--
-- [__Revision__]
--     2
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2-promotions Vulkan 1.2>
--
-- [__Contact__]
--
--     -   Faith Ekstrand
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_timeline_semaphore] @gfxstrand%0A*Here describe the issue or question you have about the VK_KHR_timeline_semaphore extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-06-12
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension interacts with
--         @VK_KHR_external_semaphore_capabilities@
--
--     -   This extension interacts with @VK_KHR_external_semaphore@
--
--     -   This extension interacts with @VK_KHR_external_semaphore_win32@
--
--     -   Promoted to Vulkan 1.2 Core
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Yuriy O’Donnell, Epic Games
--
--     -   Faith Ekstrand, Intel
--
--     -   Jesse Hall, Google
--
--     -   James Jones, NVIDIA
--
--     -   Jeff Juliano, NVIDIA
--
--     -   Daniel Rakos, AMD
--
--     -   Ray Smith, Arm
--
-- == Description
--
-- This extension introduces a new type of semaphore that has an integer
-- payload identifying a point in a timeline. Such timeline semaphores
-- support the following operations:
--
-- -   Host query - A host operation that allows querying the payload of
--     the timeline semaphore.
--
-- -   Host wait - A host operation that allows a blocking wait for a
--     timeline semaphore to reach a specified value.
--
-- -   Host signal - A host operation that allows advancing the timeline
--     semaphore to a specified value.
--
-- -   Device wait - A device operation that allows waiting for a timeline
--     semaphore to reach a specified value.
--
-- -   Device signal - A device operation that allows advancing the
--     timeline semaphore to a specified value.
--
-- == Promotion to Vulkan 1.2
--
-- All functionality in this extension is included in core Vulkan 1.2, with
-- the KHR suffix omitted. The original type, enum and command names are
-- still available as aliases of the core functionality.
--
-- == New Commands
--
-- -   'getSemaphoreCounterValueKHR'
--
-- -   'signalSemaphoreKHR'
--
-- -   'waitSemaphoresKHR'
--
-- == New Structures
--
-- -   'SemaphoreSignalInfoKHR'
--
-- -   'SemaphoreWaitInfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceTimelineSemaphoreFeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceTimelineSemaphorePropertiesKHR'
--
-- -   Extending 'Vulkan.Core10.QueueSemaphore.SemaphoreCreateInfo',
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore_capabilities.PhysicalDeviceExternalSemaphoreInfo':
--
--     -   'SemaphoreTypeCreateInfoKHR'
--
-- -   Extending 'Vulkan.Core10.Queue.SubmitInfo',
--     'Vulkan.Core10.SparseResourceMemoryManagement.BindSparseInfo':
--
--     -   'TimelineSemaphoreSubmitInfoKHR'
--
-- == New Enums
--
-- -   'SemaphoreTypeKHR'
--
-- -   'SemaphoreWaitFlagBitsKHR'
--
-- == New Bitmasks
--
-- -   'SemaphoreWaitFlagsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_TIMELINE_SEMAPHORE_EXTENSION_NAME'
--
-- -   'KHR_TIMELINE_SEMAPHORE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType':
--
--     -   'SEMAPHORE_TYPE_BINARY_KHR'
--
--     -   'SEMAPHORE_TYPE_TIMELINE_KHR'
--
-- -   Extending
--     'Vulkan.Core12.Enums.SemaphoreWaitFlagBits.SemaphoreWaitFlagBits':
--
--     -   'SEMAPHORE_WAIT_ANY_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_FEATURES_KHR'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_PROPERTIES_KHR'
--
--     -   'STRUCTURE_TYPE_SEMAPHORE_SIGNAL_INFO_KHR'
--
--     -   'STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO_KHR'
--
--     -   'STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO_KHR'
--
--     -   'STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO_KHR'
--
-- == Issues
--
-- 1) Do we need a new object type for this?
--
-- __RESOLVED__: No, we just introduce a new type of semaphore object, as
-- @VK_KHR_external_semaphore_win32@ already uses semaphores as the
-- destination for importing D3D12 fence objects, which are semantically
-- close\/identical to the proposed synchronization primitive.
--
-- 2) What type of payload the new synchronization primitive has?
--
-- __RESOLVED__: A 64-bit unsigned integer that can only be set to strictly
-- increasing values by signal operations and is not changed by wait
-- operations.
--
-- 3) Does the new synchronization primitive have the same
-- signal-before-wait requirement as the existing semaphores do?
--
-- __RESOLVED__: No. Timeline semaphores support signaling and waiting
-- entirely asynchronously. It is the responsibility of the client to avoid
-- deadlock.
--
-- 4) Does the new synchronization primitive allow resetting its payload?
--
-- __RESOLVED__: No, allowing the payload value to “go backwards” is
-- problematic. Applications looking for reset behavior should create a new
-- instance of the synchronization primitive instead.
--
-- 5) How do we enable host waits on the synchronization primitive?
--
-- __RESOLVED__: Both a non-blocking query of the current payload value of
-- the synchronization primitive, and a blocking wait operation are
-- provided.
--
-- 6) How do we enable device waits and signals on the synchronization
-- primitive?
--
-- __RESOLVED__: Similar to @VK_KHR_external_semaphore_win32@, this
-- extension introduces a new structure that can be chained to
-- 'Vulkan.Core10.Queue.SubmitInfo' to specify the values signaled
-- semaphores should be set to, and the values waited semaphores need to
-- reach.
--
-- 7) Can the new synchronization primitive be used to synchronize
-- presentation and swapchain image acquisition operations?
--
-- __RESOLVED__: Some implementations may have problems with supporting
-- that directly, thus it is not allowed in this extension.
--
-- 8) Do we want to support external sharing of the new synchronization
-- primitive type?
--
-- __RESOLVED__: Yes. Timeline semaphore specific external sharing
-- capabilities can be queried using
-- 'Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore_capabilities.getPhysicalDeviceExternalSemaphoreProperties'
-- by chaining the new 'SemaphoreTypeCreateInfoKHR' structure to its
-- @pExternalSemaphoreInfo@ structure. This allows having a different set
-- of external semaphore handle types supported for timeline semaphores vs.
-- binary semaphores.
--
-- 9) Do we need to add a host signal operation for the new synchronization
-- primitive type?
--
-- __RESOLVED__: Yes. This helps in situations where one host thread
-- submits a workload but another host thread has the information on when
-- the workload is ready to be executed.
--
-- 10) How should the new synchronization primitive interact with the
-- ordering requirements of the original 'Vulkan.Core10.Handles.Semaphore'?
--
-- __RESOLVED__: Prior to calling any command which /may/ cause a wait
-- operation on a binary semaphore, the client /must/ ensure that the
-- semaphore signal operation that has been submitted for execution and any
-- semaphore signal operations on which it depends (if any) /must/ have
-- also been submitted for execution.
--
-- 11) Should we have separate feature bits for different sub-features of
-- timeline semaphores?
--
-- __RESOLVED__: No. The only feature which cannot be supported universally
-- is timeline semaphore import\/export. For import\/export, the client is
-- already required to query available external handle types via
-- 'Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore_capabilities.getPhysicalDeviceExternalSemaphoreProperties'
-- and provide the semaphore type by adding a 'SemaphoreTypeCreateInfoKHR'
-- structure to the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore_capabilities.PhysicalDeviceExternalSemaphoreInfo'
-- so no new feature bit is required.
--
-- == Version History
--
-- -   Revision 1, 2018-05-10 (Faith Ekstrand)
--
--     -   Initial version
--
-- -   Revision 2, 2019-06-12 (Faith Ekstrand)
--
--     -   Added an initialValue parameter to timeline semaphore creation
--
-- == See Also
--
-- 'PhysicalDeviceTimelineSemaphoreFeaturesKHR',
-- 'PhysicalDeviceTimelineSemaphorePropertiesKHR',
-- 'SemaphoreSignalInfoKHR', 'SemaphoreTypeCreateInfoKHR',
-- 'SemaphoreTypeKHR', 'SemaphoreWaitFlagBitsKHR', 'SemaphoreWaitFlagsKHR',
-- 'SemaphoreWaitInfoKHR', 'TimelineSemaphoreSubmitInfoKHR',
-- 'getSemaphoreCounterValueKHR', 'signalSemaphoreKHR', 'waitSemaphoresKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_timeline_semaphore Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_timeline_semaphore  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_FEATURES_KHR
                                                    , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_PROPERTIES_KHR
                                                    , pattern STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO_KHR
                                                    , pattern STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO_KHR
                                                    , pattern STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO_KHR
                                                    , pattern STRUCTURE_TYPE_SEMAPHORE_SIGNAL_INFO_KHR
                                                    , pattern SEMAPHORE_TYPE_BINARY_KHR
                                                    , pattern SEMAPHORE_TYPE_TIMELINE_KHR
                                                    , pattern SEMAPHORE_WAIT_ANY_BIT_KHR
                                                    , getSemaphoreCounterValueKHR
                                                    , waitSemaphoresKHR
                                                    , signalSemaphoreKHR
                                                    , SemaphoreWaitFlagsKHR
                                                    , SemaphoreTypeKHR
                                                    , SemaphoreWaitFlagBitsKHR
                                                    , PhysicalDeviceTimelineSemaphoreFeaturesKHR
                                                    , PhysicalDeviceTimelineSemaphorePropertiesKHR
                                                    , SemaphoreTypeCreateInfoKHR
                                                    , TimelineSemaphoreSubmitInfoKHR
                                                    , SemaphoreWaitInfoKHR
                                                    , SemaphoreSignalInfoKHR
                                                    , KHR_TIMELINE_SEMAPHORE_SPEC_VERSION
                                                    , pattern KHR_TIMELINE_SEMAPHORE_SPEC_VERSION
                                                    , KHR_TIMELINE_SEMAPHORE_EXTENSION_NAME
                                                    , pattern KHR_TIMELINE_SEMAPHORE_EXTENSION_NAME
                                                    ) where

import Data.String (IsString)
import Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (getSemaphoreCounterValue)
import Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (signalSemaphore)
import Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (waitSemaphores)
import Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (PhysicalDeviceTimelineSemaphoreFeatures)
import Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (PhysicalDeviceTimelineSemaphoreProperties)
import Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (SemaphoreSignalInfo)
import Vulkan.Core12.Enums.SemaphoreType (SemaphoreType)
import Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (SemaphoreTypeCreateInfo)
import Vulkan.Core12.Enums.SemaphoreWaitFlagBits (SemaphoreWaitFlagBits)
import Vulkan.Core12.Enums.SemaphoreWaitFlagBits (SemaphoreWaitFlags)
import Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (SemaphoreWaitInfo)
import Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (TimelineSemaphoreSubmitInfo)
import Vulkan.Core12.Enums.SemaphoreType (SemaphoreType(SEMAPHORE_TYPE_BINARY))
import Vulkan.Core12.Enums.SemaphoreType (SemaphoreType(SEMAPHORE_TYPE_TIMELINE))
import Vulkan.Core12.Enums.SemaphoreWaitFlagBits (SemaphoreWaitFlags)
import Vulkan.Core12.Enums.SemaphoreWaitFlagBits (SemaphoreWaitFlagBits(SEMAPHORE_WAIT_ANY_BIT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SEMAPHORE_SIGNAL_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_FEATURES


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_PROPERTIES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_PROPERTIES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_PROPERTIES


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO_KHR = STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO_KHR"
pattern STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO_KHR = STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO_KHR"
pattern STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO_KHR = STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_SEMAPHORE_SIGNAL_INFO_KHR"
pattern STRUCTURE_TYPE_SEMAPHORE_SIGNAL_INFO_KHR = STRUCTURE_TYPE_SEMAPHORE_SIGNAL_INFO


-- No documentation found for TopLevel "VK_SEMAPHORE_TYPE_BINARY_KHR"
pattern SEMAPHORE_TYPE_BINARY_KHR = SEMAPHORE_TYPE_BINARY


-- No documentation found for TopLevel "VK_SEMAPHORE_TYPE_TIMELINE_KHR"
pattern SEMAPHORE_TYPE_TIMELINE_KHR = SEMAPHORE_TYPE_TIMELINE


-- No documentation found for TopLevel "VK_SEMAPHORE_WAIT_ANY_BIT_KHR"
pattern SEMAPHORE_WAIT_ANY_BIT_KHR = SEMAPHORE_WAIT_ANY_BIT


-- No documentation found for TopLevel "vkGetSemaphoreCounterValueKHR"
getSemaphoreCounterValueKHR = getSemaphoreCounterValue


-- No documentation found for TopLevel "vkWaitSemaphoresKHR"
waitSemaphoresKHR = waitSemaphores


-- No documentation found for TopLevel "vkSignalSemaphoreKHR"
signalSemaphoreKHR = signalSemaphore


-- No documentation found for TopLevel "VkSemaphoreWaitFlagsKHR"
type SemaphoreWaitFlagsKHR = SemaphoreWaitFlags


-- No documentation found for TopLevel "VkSemaphoreTypeKHR"
type SemaphoreTypeKHR = SemaphoreType


-- No documentation found for TopLevel "VkSemaphoreWaitFlagBitsKHR"
type SemaphoreWaitFlagBitsKHR = SemaphoreWaitFlagBits


-- No documentation found for TopLevel "VkPhysicalDeviceTimelineSemaphoreFeaturesKHR"
type PhysicalDeviceTimelineSemaphoreFeaturesKHR = PhysicalDeviceTimelineSemaphoreFeatures


-- No documentation found for TopLevel "VkPhysicalDeviceTimelineSemaphorePropertiesKHR"
type PhysicalDeviceTimelineSemaphorePropertiesKHR = PhysicalDeviceTimelineSemaphoreProperties


-- No documentation found for TopLevel "VkSemaphoreTypeCreateInfoKHR"
type SemaphoreTypeCreateInfoKHR = SemaphoreTypeCreateInfo


-- No documentation found for TopLevel "VkTimelineSemaphoreSubmitInfoKHR"
type TimelineSemaphoreSubmitInfoKHR = TimelineSemaphoreSubmitInfo


-- No documentation found for TopLevel "VkSemaphoreWaitInfoKHR"
type SemaphoreWaitInfoKHR = SemaphoreWaitInfo


-- No documentation found for TopLevel "VkSemaphoreSignalInfoKHR"
type SemaphoreSignalInfoKHR = SemaphoreSignalInfo


type KHR_TIMELINE_SEMAPHORE_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_KHR_TIMELINE_SEMAPHORE_SPEC_VERSION"
pattern KHR_TIMELINE_SEMAPHORE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_TIMELINE_SEMAPHORE_SPEC_VERSION = 2


type KHR_TIMELINE_SEMAPHORE_EXTENSION_NAME = "VK_KHR_timeline_semaphore"

-- No documentation found for TopLevel "VK_KHR_TIMELINE_SEMAPHORE_EXTENSION_NAME"
pattern KHR_TIMELINE_SEMAPHORE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_TIMELINE_SEMAPHORE_EXTENSION_NAME = "VK_KHR_timeline_semaphore"

