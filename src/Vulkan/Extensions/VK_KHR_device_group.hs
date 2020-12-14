{-# language CPP #-}
-- | = Name
--
-- VK_KHR_device_group - device extension
--
-- == VK_KHR_device_group
--
-- [__Name String__]
--     @VK_KHR_device_group@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     61
--
-- [__Revision__]
--     4
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_device_group_creation@
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1-promotions Vulkan 1.1>
--
-- [__Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_KHR_device_group:%20&body=@jeffbolznv%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-10-10
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.1 Core
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Tobias Hector, Imagination Technologies
--
-- == Description
--
-- This extension provides functionality to use a logical device that
-- consists of multiple physical devices, as created with the
-- @VK_KHR_device_group_creation@ extension. A device group can allocate
-- memory across the subdevices, bind memory from one subdevice to a
-- resource on another subdevice, record command buffers where some work
-- executes on an arbitrary subset of the subdevices, and potentially
-- present a swapchain image from one or more subdevices.
--
-- == Promotion to Vulkan 1.1
--
-- The following enums, types and commands are included as interactions
-- with @VK_KHR_swapchain@:
--
-- -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR'
--
-- -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR'
--
-- -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR'
--
-- -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR'
--
-- -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR'
--
-- -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR'
--
-- -   'Vulkan.Extensions.VK_KHR_swapchain.SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR'
--
-- -   'Vulkan.Extensions.VK_KHR_swapchain.DeviceGroupPresentModeFlagBitsKHR'
--
-- -   'Vulkan.Extensions.VK_KHR_swapchain.DeviceGroupPresentCapabilitiesKHR'
--
-- -   'Vulkan.Extensions.VK_KHR_swapchain.ImageSwapchainCreateInfoKHR'
--
-- -   'Vulkan.Extensions.VK_KHR_swapchain.BindImageMemorySwapchainInfoKHR'
--
-- -   'Vulkan.Extensions.VK_KHR_swapchain.AcquireNextImageInfoKHR'
--
-- -   'Vulkan.Extensions.VK_KHR_swapchain.DeviceGroupPresentInfoKHR'
--
-- -   'Vulkan.Extensions.VK_KHR_swapchain.DeviceGroupSwapchainCreateInfoKHR'
--
-- -   'Vulkan.Extensions.VK_KHR_swapchain.getDeviceGroupPresentCapabilitiesKHR'
--
-- -   'Vulkan.Extensions.VK_KHR_swapchain.getDeviceGroupSurfacePresentModesKHR'
--
-- -   'Vulkan.Extensions.VK_KHR_swapchain.getPhysicalDevicePresentRectanglesKHR'
--
-- -   'Vulkan.Extensions.VK_KHR_swapchain.acquireNextImage2KHR'
--
-- If Vulkan 1.1 and @VK_KHR_swapchain@ are supported, these are included
-- by @VK_KHR_swapchain@.
--
-- The base functionality in this extension is included in core Vulkan 1.1,
-- with the KHR suffix omitted. The original type, enum and command names
-- are still available as aliases of the core functionality.
--
-- == New Commands
--
-- -   'cmdDispatchBaseKHR'
--
-- -   'cmdSetDeviceMaskKHR'
--
-- -   'getDeviceGroupPeerMemoryFeaturesKHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface VK_KHR_surface>
-- is supported:
--
-- -   'Vulkan.Extensions.VK_KHR_swapchain.getDeviceGroupPresentCapabilitiesKHR'
--
-- -   'Vulkan.Extensions.VK_KHR_swapchain.getDeviceGroupSurfacePresentModesKHR'
--
-- -   'Vulkan.Extensions.VK_KHR_swapchain.getPhysicalDevicePresentRectanglesKHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>
-- is supported:
--
-- -   'Vulkan.Extensions.VK_KHR_swapchain.acquireNextImage2KHR'
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core10.SparseResourceMemoryManagement.BindSparseInfo':
--
--     -   'DeviceGroupBindSparseInfoKHR'
--
-- -   Extending 'Vulkan.Core10.CommandBuffer.CommandBufferBeginInfo':
--
--     -   'DeviceGroupCommandBufferBeginInfoKHR'
--
-- -   Extending 'Vulkan.Core10.Memory.MemoryAllocateInfo':
--
--     -   'MemoryAllocateFlagsInfoKHR'
--
-- -   Extending 'Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo':
--
--     -   'DeviceGroupRenderPassBeginInfoKHR'
--
-- -   Extending 'Vulkan.Core10.Queue.SubmitInfo':
--
--     -   'DeviceGroupSubmitInfoKHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_bind_memory2 VK_KHR_bind_memory2>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindBufferMemoryInfo':
--
--     -   'BindBufferMemoryDeviceGroupInfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindImageMemoryInfo':
--
--     -   'BindImageMemoryDeviceGroupInfoKHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface VK_KHR_surface>
-- is supported:
--
-- -   'Vulkan.Extensions.VK_KHR_swapchain.DeviceGroupPresentCapabilitiesKHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>
-- is supported:
--
-- -   'Vulkan.Extensions.VK_KHR_swapchain.AcquireNextImageInfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindImageMemoryInfo':
--
--     -   'Vulkan.Extensions.VK_KHR_swapchain.BindImageMemorySwapchainInfoKHR'
--
-- -   Extending 'Vulkan.Core10.Image.ImageCreateInfo':
--
--     -   'Vulkan.Extensions.VK_KHR_swapchain.ImageSwapchainCreateInfoKHR'
--
-- -   Extending 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR':
--
--     -   'Vulkan.Extensions.VK_KHR_swapchain.DeviceGroupPresentInfoKHR'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR':
--
--     -   'Vulkan.Extensions.VK_KHR_swapchain.DeviceGroupSwapchainCreateInfoKHR'
--
-- == New Enums
--
-- -   'MemoryAllocateFlagBitsKHR'
--
-- -   'PeerMemoryFeatureFlagBitsKHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface VK_KHR_surface>
-- is supported:
--
-- -   'Vulkan.Extensions.VK_KHR_swapchain.DeviceGroupPresentModeFlagBitsKHR'
--
-- == New Bitmasks
--
-- -   'MemoryAllocateFlagsKHR'
--
-- -   'PeerMemoryFeatureFlagsKHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface VK_KHR_surface>
-- is supported:
--
-- -   'Vulkan.Extensions.VK_KHR_swapchain.DeviceGroupPresentModeFlagsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_DEVICE_GROUP_EXTENSION_NAME'
--
-- -   'KHR_DEVICE_GROUP_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.DependencyFlagBits.DependencyFlagBits':
--
--     -   'DEPENDENCY_DEVICE_GROUP_BIT_KHR'
--
-- -   Extending
--     'Vulkan.Core11.Enums.MemoryAllocateFlagBits.MemoryAllocateFlagBits':
--
--     -   'MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHR'
--
-- -   Extending
--     'Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits.PeerMemoryFeatureFlagBits':
--
--     -   'PEER_MEMORY_FEATURE_COPY_DST_BIT_KHR'
--
--     -   'PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHR'
--
--     -   'PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHR'
--
--     -   'PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHR'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits':
--
--     -   'PIPELINE_CREATE_DISPATCH_BASE_KHR'
--
--     -   'PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO_KHR'
--
--     -   'STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO_KHR'
--
--     -   'STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO_KHR'
--
--     -   'STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO_KHR'
--
--     -   'STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_bind_memory2 VK_KHR_bind_memory2>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlagBits':
--
--     -   'IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO_KHR'
--
--     -   'STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface VK_KHR_surface>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateFlagBitsKHR':
--
--     -   'Vulkan.Extensions.VK_KHR_swapchain.SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR'
--
-- == New Built-in Variables
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-deviceindex DeviceIndex>
--
-- == New SPIR-V Capabilities
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-DeviceGroup DeviceGroup>
--
-- == Version History
--
-- -   Revision 1, 2016-10-19 (Jeff Bolz)
--
--     -   Internal revisions
--
-- -   Revision 2, 2017-05-19 (Tobias Hector)
--
--     -   Removed extended memory bind functions to VK_KHR_bind_memory2,
--         added dependency on that extension, and device-group-specific
--         structs for those functions.
--
-- -   Revision 3, 2017-10-06 (Ian Elliott)
--
--     -   Corrected Vulkan 1.1 interactions with the WSI extensions. All
--         Vulkan 1.1 WSI interactions are with the VK_KHR_swapchain
--         extension.
--
-- -   Revision 4, 2017-10-10 (Jeff Bolz)
--
--     -   Rename \"SFR\" bits and structure members to use the phrase
--         \"split instance bind regions\".
--
-- = See Also
--
-- 'DeviceGroupBindSparseInfoKHR', 'DeviceGroupCommandBufferBeginInfoKHR',
-- 'DeviceGroupRenderPassBeginInfoKHR', 'DeviceGroupSubmitInfoKHR',
-- 'MemoryAllocateFlagBitsKHR', 'MemoryAllocateFlagsInfoKHR',
-- 'MemoryAllocateFlagsKHR', 'PeerMemoryFeatureFlagBitsKHR',
-- 'PeerMemoryFeatureFlagsKHR', 'cmdDispatchBaseKHR',
-- 'cmdSetDeviceMaskKHR', 'getDeviceGroupPeerMemoryFeaturesKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_group Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_device_group  ( pattern STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO_KHR
                                              , pattern STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO_KHR
                                              , pattern STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO_KHR
                                              , pattern STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO_KHR
                                              , pattern STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO_KHR
                                              , pattern PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHR
                                              , pattern PEER_MEMORY_FEATURE_COPY_DST_BIT_KHR
                                              , pattern PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHR
                                              , pattern PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHR
                                              , pattern MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHR
                                              , pattern PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHR
                                              , pattern PIPELINE_CREATE_DISPATCH_BASE_KHR
                                              , pattern DEPENDENCY_DEVICE_GROUP_BIT_KHR
                                              , pattern STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO_KHR
                                              , pattern STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO_KHR
                                              , pattern IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR
                                              , getDeviceGroupPeerMemoryFeaturesKHR
                                              , cmdSetDeviceMaskKHR
                                              , cmdDispatchBaseKHR
                                              , PeerMemoryFeatureFlagsKHR
                                              , MemoryAllocateFlagsKHR
                                              , PeerMemoryFeatureFlagBitsKHR
                                              , MemoryAllocateFlagBitsKHR
                                              , MemoryAllocateFlagsInfoKHR
                                              , BindBufferMemoryDeviceGroupInfoKHR
                                              , BindImageMemoryDeviceGroupInfoKHR
                                              , DeviceGroupRenderPassBeginInfoKHR
                                              , DeviceGroupCommandBufferBeginInfoKHR
                                              , DeviceGroupSubmitInfoKHR
                                              , DeviceGroupBindSparseInfoKHR
                                              , KHR_DEVICE_GROUP_SPEC_VERSION
                                              , pattern KHR_DEVICE_GROUP_SPEC_VERSION
                                              , KHR_DEVICE_GROUP_EXTENSION_NAME
                                              , pattern KHR_DEVICE_GROUP_EXTENSION_NAME
                                              , SurfaceKHR(..)
                                              , SwapchainKHR(..)
                                              , DeviceGroupPresentCapabilitiesKHR(..)
                                              , ImageSwapchainCreateInfoKHR(..)
                                              , BindImageMemorySwapchainInfoKHR(..)
                                              , AcquireNextImageInfoKHR(..)
                                              , DeviceGroupPresentInfoKHR(..)
                                              , DeviceGroupSwapchainCreateInfoKHR(..)
                                              , getDeviceGroupPresentCapabilitiesKHR
                                              , getDeviceGroupSurfacePresentModesKHR
                                              , acquireNextImage2KHR
                                              , acquireNextImage2KHRSafe
                                              , getPhysicalDevicePresentRectanglesKHR
                                              , DeviceGroupPresentModeFlagBitsKHR(..)
                                              , DeviceGroupPresentModeFlagsKHR
                                              , SwapchainCreateFlagBitsKHR(..)
                                              , SwapchainCreateFlagsKHR
                                              ) where

import Data.String (IsString)
import Vulkan.Core11.Promoted_From_VK_KHR_device_group (cmdDispatchBase)
import Vulkan.Core11.Promoted_From_VK_KHR_device_group (cmdSetDeviceMask)
import Vulkan.Core11.Promoted_From_VK_KHR_device_group (getDeviceGroupPeerMemoryFeatures)
import Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2 (BindBufferMemoryDeviceGroupInfo)
import Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2 (BindImageMemoryDeviceGroupInfo)
import Vulkan.Core11.Promoted_From_VK_KHR_device_group (DeviceGroupBindSparseInfo)
import Vulkan.Core11.Promoted_From_VK_KHR_device_group (DeviceGroupCommandBufferBeginInfo)
import Vulkan.Core11.Promoted_From_VK_KHR_device_group (DeviceGroupRenderPassBeginInfo)
import Vulkan.Core11.Promoted_From_VK_KHR_device_group (DeviceGroupSubmitInfo)
import Vulkan.Core11.Enums.MemoryAllocateFlagBits (MemoryAllocateFlagBits)
import Vulkan.Core11.Enums.MemoryAllocateFlagBits (MemoryAllocateFlags)
import Vulkan.Core11.Promoted_From_VK_KHR_device_group (MemoryAllocateFlagsInfo)
import Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits (PeerMemoryFeatureFlagBits)
import Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits (PeerMemoryFeatureFlags)
import Vulkan.Core10.Enums.DependencyFlagBits (DependencyFlags)
import Vulkan.Core10.Enums.DependencyFlagBits (DependencyFlagBits(DEPENDENCY_DEVICE_GROUP_BIT))
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlags)
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlagBits(IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT))
import Vulkan.Core11.Enums.MemoryAllocateFlagBits (MemoryAllocateFlags)
import Vulkan.Core11.Enums.MemoryAllocateFlagBits (MemoryAllocateFlagBits(MEMORY_ALLOCATE_DEVICE_MASK_BIT))
import Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits (PeerMemoryFeatureFlags)
import Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits (PeerMemoryFeatureFlagBits(PEER_MEMORY_FEATURE_COPY_DST_BIT))
import Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits (PeerMemoryFeatureFlags)
import Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits (PeerMemoryFeatureFlagBits(PEER_MEMORY_FEATURE_COPY_SRC_BIT))
import Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits (PeerMemoryFeatureFlags)
import Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits (PeerMemoryFeatureFlagBits(PEER_MEMORY_FEATURE_GENERIC_DST_BIT))
import Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits (PeerMemoryFeatureFlags)
import Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits (PeerMemoryFeatureFlagBits(PEER_MEMORY_FEATURE_GENERIC_SRC_BIT))
import Vulkan.Core11.Promoted_From_VK_KHR_device_group (pattern PIPELINE_CREATE_DISPATCH_BASE)
import Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlags)
import Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlagBits(PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO))
import Vulkan.Extensions.VK_KHR_swapchain (acquireNextImage2KHR)
import Vulkan.Extensions.VK_KHR_swapchain (acquireNextImage2KHRSafe)
import Vulkan.Extensions.VK_KHR_swapchain (getDeviceGroupPresentCapabilitiesKHR)
import Vulkan.Extensions.VK_KHR_swapchain (getDeviceGroupSurfacePresentModesKHR)
import Vulkan.Extensions.VK_KHR_swapchain (getPhysicalDevicePresentRectanglesKHR)
import Vulkan.Extensions.VK_KHR_swapchain (AcquireNextImageInfoKHR(..))
import Vulkan.Extensions.VK_KHR_swapchain (BindImageMemorySwapchainInfoKHR(..))
import Vulkan.Extensions.VK_KHR_swapchain (DeviceGroupPresentCapabilitiesKHR(..))
import Vulkan.Extensions.VK_KHR_swapchain (DeviceGroupPresentInfoKHR(..))
import Vulkan.Extensions.VK_KHR_swapchain (DeviceGroupPresentModeFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_swapchain (DeviceGroupPresentModeFlagsKHR)
import Vulkan.Extensions.VK_KHR_swapchain (DeviceGroupSwapchainCreateInfoKHR(..))
import Vulkan.Extensions.VK_KHR_swapchain (ImageSwapchainCreateInfoKHR(..))
import Vulkan.Extensions.Handles (SurfaceKHR(..))
import Vulkan.Extensions.VK_KHR_swapchain (SwapchainCreateFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_swapchain (SwapchainCreateFlagsKHR)
import Vulkan.Extensions.Handles (SwapchainKHR(..))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO_KHR"
pattern STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO_KHR = STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO_KHR"
pattern STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO_KHR = STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO_KHR"
pattern STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO_KHR = STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO_KHR"
pattern STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO_KHR = STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO_KHR"
pattern STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO_KHR = STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO


-- No documentation found for TopLevel "VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHR"
pattern PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHR = PEER_MEMORY_FEATURE_COPY_SRC_BIT


-- No documentation found for TopLevel "VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHR"
pattern PEER_MEMORY_FEATURE_COPY_DST_BIT_KHR = PEER_MEMORY_FEATURE_COPY_DST_BIT


-- No documentation found for TopLevel "VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHR"
pattern PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHR = PEER_MEMORY_FEATURE_GENERIC_SRC_BIT


-- No documentation found for TopLevel "VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHR"
pattern PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHR = PEER_MEMORY_FEATURE_GENERIC_DST_BIT


-- No documentation found for TopLevel "VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHR"
pattern MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHR = MEMORY_ALLOCATE_DEVICE_MASK_BIT


-- No documentation found for TopLevel "VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHR"
pattern PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHR = PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT


-- No documentation found for TopLevel "VK_PIPELINE_CREATE_DISPATCH_BASE_KHR"
pattern PIPELINE_CREATE_DISPATCH_BASE_KHR = PIPELINE_CREATE_DISPATCH_BASE


-- No documentation found for TopLevel "VK_DEPENDENCY_DEVICE_GROUP_BIT_KHR"
pattern DEPENDENCY_DEVICE_GROUP_BIT_KHR = DEPENDENCY_DEVICE_GROUP_BIT


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO_KHR"
pattern STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO_KHR = STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO_KHR"
pattern STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO_KHR = STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO


-- No documentation found for TopLevel "VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR"
pattern IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR = IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT


-- No documentation found for TopLevel "vkGetDeviceGroupPeerMemoryFeaturesKHR"
getDeviceGroupPeerMemoryFeaturesKHR = getDeviceGroupPeerMemoryFeatures


-- No documentation found for TopLevel "vkCmdSetDeviceMaskKHR"
cmdSetDeviceMaskKHR = cmdSetDeviceMask


-- No documentation found for TopLevel "vkCmdDispatchBaseKHR"
cmdDispatchBaseKHR = cmdDispatchBase


-- No documentation found for TopLevel "VkPeerMemoryFeatureFlagsKHR"
type PeerMemoryFeatureFlagsKHR = PeerMemoryFeatureFlags


-- No documentation found for TopLevel "VkMemoryAllocateFlagsKHR"
type MemoryAllocateFlagsKHR = MemoryAllocateFlags


-- No documentation found for TopLevel "VkPeerMemoryFeatureFlagBitsKHR"
type PeerMemoryFeatureFlagBitsKHR = PeerMemoryFeatureFlagBits


-- No documentation found for TopLevel "VkMemoryAllocateFlagBitsKHR"
type MemoryAllocateFlagBitsKHR = MemoryAllocateFlagBits


-- No documentation found for TopLevel "VkMemoryAllocateFlagsInfoKHR"
type MemoryAllocateFlagsInfoKHR = MemoryAllocateFlagsInfo


-- No documentation found for TopLevel "VkBindBufferMemoryDeviceGroupInfoKHR"
type BindBufferMemoryDeviceGroupInfoKHR = BindBufferMemoryDeviceGroupInfo


-- No documentation found for TopLevel "VkBindImageMemoryDeviceGroupInfoKHR"
type BindImageMemoryDeviceGroupInfoKHR = BindImageMemoryDeviceGroupInfo


-- No documentation found for TopLevel "VkDeviceGroupRenderPassBeginInfoKHR"
type DeviceGroupRenderPassBeginInfoKHR = DeviceGroupRenderPassBeginInfo


-- No documentation found for TopLevel "VkDeviceGroupCommandBufferBeginInfoKHR"
type DeviceGroupCommandBufferBeginInfoKHR = DeviceGroupCommandBufferBeginInfo


-- No documentation found for TopLevel "VkDeviceGroupSubmitInfoKHR"
type DeviceGroupSubmitInfoKHR = DeviceGroupSubmitInfo


-- No documentation found for TopLevel "VkDeviceGroupBindSparseInfoKHR"
type DeviceGroupBindSparseInfoKHR = DeviceGroupBindSparseInfo


type KHR_DEVICE_GROUP_SPEC_VERSION = 4

-- No documentation found for TopLevel "VK_KHR_DEVICE_GROUP_SPEC_VERSION"
pattern KHR_DEVICE_GROUP_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_DEVICE_GROUP_SPEC_VERSION = 4


type KHR_DEVICE_GROUP_EXTENSION_NAME = "VK_KHR_device_group"

-- No documentation found for TopLevel "VK_KHR_DEVICE_GROUP_EXTENSION_NAME"
pattern KHR_DEVICE_GROUP_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_DEVICE_GROUP_EXTENSION_NAME = "VK_KHR_device_group"

