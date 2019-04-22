{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_device_group
  ( BindBufferMemoryDeviceGroupInfoKHR
  , BindImageMemoryDeviceGroupInfoKHR
  , DeviceGroupBindSparseInfoKHR
  , DeviceGroupCommandBufferBeginInfoKHR
  , DeviceGroupRenderPassBeginInfoKHR
  , DeviceGroupSubmitInfoKHR
  , MemoryAllocateFlagsInfoKHR
  , cmdDispatchBaseKHR
  , cmdSetDeviceMaskKHR
  , getDeviceGroupPeerMemoryFeaturesKHR
#if defined(VK_USE_PLATFORM_WIN32)
  , getDeviceGroupSurfacePresentModes2EXT
#endif
  , pattern DEPENDENCY_DEVICE_GROUP_BIT_KHR
  , pattern IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR
  , pattern KHR_DEVICE_GROUP_EXTENSION_NAME
  , pattern KHR_DEVICE_GROUP_SPEC_VERSION
  , pattern MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHR
  , pattern PEER_MEMORY_FEATURE_COPY_DST_BIT_KHR
  , pattern PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHR
  , pattern PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHR
  , pattern PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHR
  , pattern PIPELINE_CREATE_DISPATCH_BASE_KHR
  , pattern PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHR
  , pattern STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO_KHR
  , pattern STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO_KHR
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO_KHR
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO_KHR
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO_KHR
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO_KHR
  , pattern STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO_KHR
  , PeerMemoryFeatureFlagsKHR
  , PeerMemoryFeatureFlagBitsKHR
  , MemoryAllocateFlagsKHR
  , MemoryAllocateFlagBitsKHR
  , DeviceGroupPresentModeFlagBitsKHR
  , DeviceGroupPresentModeFlagsKHR
  , DeviceGroupPresentCapabilitiesKHR(..)
  , getDeviceGroupPresentCapabilitiesKHR
  , getDeviceGroupSurfacePresentModesKHR
  , getPhysicalDevicePresentRectanglesKHR
  , ImageSwapchainCreateInfoKHR(..)
  , BindImageMemorySwapchainInfoKHR(..)
  , AcquireNextImageInfoKHR(..)
  , DeviceGroupPresentInfoKHR(..)
  , DeviceGroupSwapchainCreateInfoKHR(..)
  , acquireNextImage2KHR
  , pattern STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO
  , pattern PEER_MEMORY_FEATURE_COPY_SRC_BIT
  , pattern PEER_MEMORY_FEATURE_COPY_DST_BIT
  , pattern PEER_MEMORY_FEATURE_GENERIC_SRC_BIT
  , pattern PEER_MEMORY_FEATURE_GENERIC_DST_BIT
  , pattern MEMORY_ALLOCATE_DEVICE_MASK_BIT
  , pattern PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT
  , pattern PIPELINE_CREATE_DISPATCH_BASE
  , pattern DEPENDENCY_DEVICE_GROUP_BIT
  , pattern STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO
  , pattern STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO
  , pattern IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR
  , pattern STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR
  , pattern STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR
  , pattern STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR
  , pattern SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR
  ) where


#if defined(VK_USE_PLATFORM_WIN32)
import Control.Exception
  ( throwIO
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32)
import Control.Monad
  ( when
  )
#endif
import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )

#if defined(VK_USE_PLATFORM_WIN32)
import Foreign.Marshal.Alloc
  ( alloca
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32)
import Foreign.Marshal.Utils
  ( with
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32)
import Foreign.Storable
  ( peek
  )
#endif


import Graphics.Vulkan.C.Core10.Core
  ( VkStructureType(..)
  )

#if defined(VK_USE_PLATFORM_WIN32)
import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_SUCCESS
  )
#endif
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkImageCreateFlagBits(..)
  )
import Graphics.Vulkan.C.Core10.Pass
  ( VkDependencyFlagBits(..)
  )
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkPipelineCreateFlagBits(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group
  ( VkMemoryAllocateFlagBits(..)
  , VkPeerMemoryFeatureFlagBits(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_device_group
  ( pattern VK_KHR_DEVICE_GROUP_EXTENSION_NAME
  , pattern VK_KHR_DEVICE_GROUP_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_WIN32)
import Graphics.Vulkan.C.Extensions.VK_KHR_device_group
  ( vkGetDeviceGroupSurfacePresentModes2EXT
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO
  , pattern STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO
  , pattern STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  , pattern IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT
  )
import Graphics.Vulkan.Core10.Pass
  ( pattern DEPENDENCY_DEVICE_GROUP_BIT
  )
import Graphics.Vulkan.Core10.Pipeline
  ( pattern PIPELINE_CREATE_DISPATCH_BASE
  , pattern PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT
  )
import Graphics.Vulkan.Core10.Queue
  ( CommandBuffer(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group
  ( DeviceGroupBindSparseInfo(..)
  , DeviceGroupCommandBufferBeginInfo(..)
  , DeviceGroupRenderPassBeginInfo(..)
  , DeviceGroupSubmitInfo(..)
  , MemoryAllocateFlagsInfo(..)
  , PeerMemoryFeatureFlags
  , cmdDispatchBase
  , cmdSetDeviceMask
  , getDeviceGroupPeerMemoryFeatures
  , pattern MEMORY_ALLOCATE_DEVICE_MASK_BIT
  , pattern PEER_MEMORY_FEATURE_COPY_DST_BIT
  , pattern PEER_MEMORY_FEATURE_COPY_SRC_BIT
  , pattern PEER_MEMORY_FEATURE_GENERIC_DST_BIT
  , pattern PEER_MEMORY_FEATURE_GENERIC_SRC_BIT
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2
  ( BindBufferMemoryDeviceGroupInfo(..)
  , BindImageMemoryDeviceGroupInfo(..)
  )

#if defined(VK_USE_PLATFORM_WIN32)
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32)
import Graphics.Vulkan.Extensions.VK_KHR_get_surface_capabilities2
  ( PhysicalDeviceSurfaceInfo2KHR(..)
  , withCStructPhysicalDeviceSurfaceInfo2KHR
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32)
import Graphics.Vulkan.Extensions.VK_KHR_swapchain
  ( DeviceGroupPresentModeFlagsKHR
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR
  , pattern STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR
  , pattern STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group
  ( MemoryAllocateFlagBitsKHR
  , MemoryAllocateFlagsKHR
  , PeerMemoryFeatureFlagBitsKHR
  , PeerMemoryFeatureFlagsKHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_swapchain
  ( AcquireNextImageInfoKHR(..)
  , BindImageMemorySwapchainInfoKHR(..)
  , DeviceGroupPresentCapabilitiesKHR(..)
  , DeviceGroupPresentInfoKHR(..)
  , DeviceGroupSwapchainCreateInfoKHR(..)
  , ImageSwapchainCreateInfoKHR(..)
  , DeviceGroupPresentModeFlagBitsKHR
  , DeviceGroupPresentModeFlagsKHR
  , acquireNextImage2KHR
  , getDeviceGroupPresentCapabilitiesKHR
  , getDeviceGroupSurfacePresentModesKHR
  , getPhysicalDevicePresentRectanglesKHR
  , pattern SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR
  )


type BindBufferMemoryDeviceGroupInfoKHR = BindBufferMemoryDeviceGroupInfo
-- TODO: Pattern constructor alias)

type BindImageMemoryDeviceGroupInfoKHR = BindImageMemoryDeviceGroupInfo
-- TODO: Pattern constructor alias)

type DeviceGroupBindSparseInfoKHR = DeviceGroupBindSparseInfo
-- TODO: Pattern constructor alias)

type DeviceGroupCommandBufferBeginInfoKHR = DeviceGroupCommandBufferBeginInfo
-- TODO: Pattern constructor alias)

type DeviceGroupRenderPassBeginInfoKHR = DeviceGroupRenderPassBeginInfo
-- TODO: Pattern constructor alias)

type DeviceGroupSubmitInfoKHR = DeviceGroupSubmitInfo
-- TODO: Pattern constructor alias)

type MemoryAllocateFlagsInfoKHR = MemoryAllocateFlagsInfo
-- TODO: Pattern constructor alias)

cmdDispatchBaseKHR :: CommandBuffer ->  Word32 ->  Word32 ->  Word32 ->  Word32 ->  Word32 ->  Word32 ->  IO ()
cmdDispatchBaseKHR = cmdDispatchBase

cmdSetDeviceMaskKHR :: CommandBuffer ->  Word32 ->  IO ()
cmdSetDeviceMaskKHR = cmdSetDeviceMask

getDeviceGroupPeerMemoryFeaturesKHR :: Device ->  Word32 ->  Word32 ->  Word32 ->  IO (PeerMemoryFeatureFlags)
getDeviceGroupPeerMemoryFeaturesKHR = getDeviceGroupPeerMemoryFeatures


#if defined(VK_USE_PLATFORM_WIN32)

-- | vkGetDeviceGroupSurfacePresentModes2EXT - Query device group present
-- capabilities for a surface
--
-- = Parameters
--
-- -   @device@ is the logical device.
--
-- -   @pSurfaceInfo@ points to an instance of the
--     VkPhysicalDeviceSurfaceInfo2KHR structure, describing the surface
--     and other fixed parameters that would be consumed by
--     vkCreateSwapchainKHR.
--
-- -   @pModes@ is a pointer to a value of type
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkDeviceGroupPresentModeFlagsKHR'
--     that is filled with the supported device group present modes for the
--     surface.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_device_group.vkGetDeviceGroupSurfacePresentModes2EXT'
-- behaves similarly to
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkGetDeviceGroupSurfacePresentModesKHR',
-- with the ability to specify extended inputs via chained input
-- structures.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_ERROR_SURFACE_LOST_KHR'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkDeviceGroupPresentModeFlagsKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_surface_capabilities2.VkPhysicalDeviceSurfaceInfo2KHR'
getDeviceGroupSurfacePresentModes2EXT :: Device ->  PhysicalDeviceSurfaceInfo2KHR ->  IO (DeviceGroupPresentModeFlagsKHR)
getDeviceGroupSurfacePresentModes2EXT = \(Device device' commandTable) -> \surfaceInfo' -> alloca (\pModes' -> (\marshalled -> withCStructPhysicalDeviceSurfaceInfo2KHR marshalled . flip with) surfaceInfo' (\pSurfaceInfo' -> vkGetDeviceGroupSurfacePresentModes2EXT commandTable device' pSurfaceInfo' pModes' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pModes'))))
#endif

-- No documentation found for TopLevel "DEPENDENCY_DEVICE_GROUP_BIT_KHR"
pattern DEPENDENCY_DEVICE_GROUP_BIT_KHR :: VkDependencyFlagBits
pattern DEPENDENCY_DEVICE_GROUP_BIT_KHR = DEPENDENCY_DEVICE_GROUP_BIT

-- No documentation found for TopLevel "IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR"
pattern IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR :: VkImageCreateFlagBits
pattern IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR = IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT

-- No documentation found for TopLevel "VK_KHR_DEVICE_GROUP_EXTENSION_NAME"
pattern KHR_DEVICE_GROUP_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_DEVICE_GROUP_EXTENSION_NAME = VK_KHR_DEVICE_GROUP_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_DEVICE_GROUP_SPEC_VERSION"
pattern KHR_DEVICE_GROUP_SPEC_VERSION :: Integral a => a
pattern KHR_DEVICE_GROUP_SPEC_VERSION = VK_KHR_DEVICE_GROUP_SPEC_VERSION

-- No documentation found for TopLevel "MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHR"
pattern MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHR :: VkMemoryAllocateFlagBits
pattern MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHR = MEMORY_ALLOCATE_DEVICE_MASK_BIT

-- No documentation found for TopLevel "PEER_MEMORY_FEATURE_COPY_DST_BIT_KHR"
pattern PEER_MEMORY_FEATURE_COPY_DST_BIT_KHR :: VkPeerMemoryFeatureFlagBits
pattern PEER_MEMORY_FEATURE_COPY_DST_BIT_KHR = PEER_MEMORY_FEATURE_COPY_DST_BIT

-- No documentation found for TopLevel "PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHR"
pattern PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHR :: VkPeerMemoryFeatureFlagBits
pattern PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHR = PEER_MEMORY_FEATURE_COPY_SRC_BIT

-- No documentation found for TopLevel "PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHR"
pattern PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHR :: VkPeerMemoryFeatureFlagBits
pattern PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHR = PEER_MEMORY_FEATURE_GENERIC_DST_BIT

-- No documentation found for TopLevel "PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHR"
pattern PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHR :: VkPeerMemoryFeatureFlagBits
pattern PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHR = PEER_MEMORY_FEATURE_GENERIC_SRC_BIT

-- No documentation found for TopLevel "PIPELINE_CREATE_DISPATCH_BASE_KHR"
pattern PIPELINE_CREATE_DISPATCH_BASE_KHR :: VkPipelineCreateFlagBits
pattern PIPELINE_CREATE_DISPATCH_BASE_KHR = PIPELINE_CREATE_DISPATCH_BASE

-- No documentation found for TopLevel "PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHR"
pattern PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHR :: VkPipelineCreateFlagBits
pattern PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHR = PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT

-- No documentation found for TopLevel "STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO_KHR"
pattern STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO_KHR :: VkStructureType
pattern STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO_KHR = STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO

-- No documentation found for TopLevel "STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO_KHR"
pattern STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO_KHR :: VkStructureType
pattern STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO_KHR = STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO

-- No documentation found for TopLevel "STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO_KHR"
pattern STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO_KHR :: VkStructureType
pattern STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO_KHR = STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO

-- No documentation found for TopLevel "STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO_KHR"
pattern STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO_KHR :: VkStructureType
pattern STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO_KHR = STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO

-- No documentation found for TopLevel "STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO_KHR"
pattern STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO_KHR :: VkStructureType
pattern STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO_KHR = STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO

-- No documentation found for TopLevel "STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO_KHR"
pattern STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO_KHR :: VkStructureType
pattern STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO_KHR = STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO

-- No documentation found for TopLevel "STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO_KHR"
pattern STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO_KHR :: VkStructureType
pattern STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO_KHR = STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO
