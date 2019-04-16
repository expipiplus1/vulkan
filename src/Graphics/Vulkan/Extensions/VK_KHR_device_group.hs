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
  , pattern VK_KHR_DEVICE_GROUP_SPEC_VERSION
  , pattern VK_KHR_DEVICE_GROUP_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO
  , PeerMemoryFeatureFlagsKHR
  , PeerMemoryFeatureFlagBitsKHR
  , pattern VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHR
  , pattern VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT
  , pattern VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHR
  , pattern VK_PEER_MEMORY_FEATURE_COPY_DST_BIT
  , pattern VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHR
  , pattern VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT
  , pattern VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHR
  , pattern VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT
  , MemoryAllocateFlagsKHR
  , MemoryAllocateFlagBitsKHR
  , pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHR
  , pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT
  , pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHR
  , pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT
  , pattern VK_PIPELINE_CREATE_DISPATCH_BASE_KHR
  , pattern VK_PIPELINE_CREATE_DISPATCH_BASE
  , pattern VK_DEPENDENCY_DEVICE_GROUP_BIT_KHR
  , pattern VK_DEPENDENCY_DEVICE_GROUP_BIT
  , pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO
  , pattern VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR
  , pattern VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR
  , DeviceGroupPresentModeFlagBitsKHR
  , DeviceGroupPresentModeFlagsKHR
  , DeviceGroupPresentCapabilitiesKHR(..)
  , getDeviceGroupPresentCapabilitiesKHR
  , getDeviceGroupSurfacePresentModesKHR
  , getPhysicalDevicePresentRectanglesKHR
  , pattern VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR
  , pattern VK_SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR
  , ImageSwapchainCreateInfoKHR(..)
  , BindImageMemorySwapchainInfoKHR(..)
  , AcquireNextImageInfoKHR(..)
  , DeviceGroupPresentInfoKHR(..)
  , DeviceGroupSwapchainCreateInfoKHR(..)
  , acquireNextImage2KHR
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

#if defined(VK_USE_PLATFORM_WIN32)
import qualified Graphics.Vulkan.C.Dynamic
  ( getDeviceGroupSurfacePresentModes2EXT
  )
#endif



#if defined(VK_USE_PLATFORM_WIN32)
import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_SUCCESS
  )
#endif
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
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
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group
  ( pattern VK_DEPENDENCY_DEVICE_GROUP_BIT
  , pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT
  , pattern VK_PEER_MEMORY_FEATURE_COPY_DST_BIT
  , pattern VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT
  , pattern VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT
  , pattern VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT
  , pattern VK_PIPELINE_CREATE_DISPATCH_BASE
  , pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO
  , pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2
  ( pattern VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT
  , pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_device_group
  ( pattern VK_DEPENDENCY_DEVICE_GROUP_BIT_KHR
  , pattern VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR
  , pattern VK_KHR_DEVICE_GROUP_EXTENSION_NAME
  , pattern VK_KHR_DEVICE_GROUP_SPEC_VERSION
  , pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHR
  , pattern VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHR
  , pattern VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHR
  , pattern VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHR
  , pattern VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHR
  , pattern VK_PIPELINE_CREATE_DISPATCH_BASE_KHR
  , pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHR
  , pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO_KHR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_swapchain
  ( pattern VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR
  , pattern VK_SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR
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
cmdDispatchBaseKHR :: CommandBuffer ->  Word32 ->  Word32 ->  Word32 ->  Word32 ->  Word32 ->  Word32 ->  IO (  )
cmdDispatchBaseKHR = cmdDispatchBase
cmdSetDeviceMaskKHR :: CommandBuffer ->  Word32 ->  IO ()
cmdSetDeviceMaskKHR = cmdSetDeviceMask
getDeviceGroupPeerMemoryFeaturesKHR :: Device ->  Word32 ->  Word32 ->  Word32 ->  IO (PeerMemoryFeatureFlags)
getDeviceGroupPeerMemoryFeaturesKHR = getDeviceGroupPeerMemoryFeatures

#if defined(VK_USE_PLATFORM_WIN32)

-- | Wrapper for 'vkGetDeviceGroupSurfacePresentModes2EXT'
getDeviceGroupSurfacePresentModes2EXT :: Device ->  PhysicalDeviceSurfaceInfo2KHR ->  IO (DeviceGroupPresentModeFlagsKHR)
getDeviceGroupSurfacePresentModes2EXT = \(Device device commandTable) -> \surfaceInfo -> alloca (\pModes -> (\a -> withCStructPhysicalDeviceSurfaceInfo2KHR a . flip with) surfaceInfo (\pSurfaceInfo -> Graphics.Vulkan.C.Dynamic.getDeviceGroupSurfacePresentModes2EXT commandTable device pSurfaceInfo pModes >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pModes))))
#endif
