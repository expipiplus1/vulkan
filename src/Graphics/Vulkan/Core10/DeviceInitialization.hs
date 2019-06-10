{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
#if defined(VK_USE_PLATFORM_GGP)
  , ApplicationInfo(..)
#endif
  , Device(..)
  , DeviceSize
  , Extent3D(..)
  , FormatFeatureFlagBits
  , pattern FORMAT_FEATURE_SAMPLED_IMAGE_BIT
  , pattern FORMAT_FEATURE_STORAGE_IMAGE_BIT
  , pattern FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT
  , pattern FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT
  , pattern FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT
  , pattern FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT
  , pattern FORMAT_FEATURE_VERTEX_BUFFER_BIT
  , pattern FORMAT_FEATURE_COLOR_ATTACHMENT_BIT
  , pattern FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT
  , pattern FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT
  , pattern FORMAT_FEATURE_BLIT_SRC_BIT
  , pattern FORMAT_FEATURE_BLIT_DST_BIT
  , pattern FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT
  , pattern FORMAT_FEATURE_TRANSFER_SRC_BIT
  , pattern FORMAT_FEATURE_TRANSFER_DST_BIT
  , pattern FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT
  , pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT
  , pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT
  , pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT
  , pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT
  , pattern FORMAT_FEATURE_DISJOINT_BIT
  , pattern FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT
  , pattern FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG
  , pattern FORMAT_FEATURE_RESERVED_27_BIT_KHR
  , pattern FORMAT_FEATURE_RESERVED_28_BIT_KHR
  , pattern FORMAT_FEATURE_RESERVED_25_BIT_KHR
  , pattern FORMAT_FEATURE_RESERVED_26_BIT_KHR
  , pattern FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT
  , pattern FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT
  , FormatFeatureFlags
  , FormatProperties(..)
  , ImageCreateFlagBits
  , pattern IMAGE_CREATE_SPARSE_BINDING_BIT
  , pattern IMAGE_CREATE_SPARSE_RESIDENCY_BIT
  , pattern IMAGE_CREATE_SPARSE_ALIASED_BIT
  , pattern IMAGE_CREATE_MUTABLE_FORMAT_BIT
  , pattern IMAGE_CREATE_CUBE_COMPATIBLE_BIT
  , pattern IMAGE_CREATE_ALIAS_BIT
  , pattern IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT
  , pattern IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT
  , pattern IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT
  , pattern IMAGE_CREATE_EXTENDED_USAGE_BIT
  , pattern IMAGE_CREATE_PROTECTED_BIT
  , pattern IMAGE_CREATE_DISJOINT_BIT
  , pattern IMAGE_CREATE_CORNER_SAMPLED_BIT_NV
  , pattern IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT
  , pattern IMAGE_CREATE_SUBSAMPLED_BIT_EXT
  , ImageCreateFlags
  , ImageFormatProperties(..)
  , ImageTiling
  , pattern IMAGE_TILING_OPTIMAL
  , pattern IMAGE_TILING_LINEAR
  , pattern IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT
  , ImageType
  , pattern IMAGE_TYPE_1D
  , pattern IMAGE_TYPE_2D
  , pattern IMAGE_TYPE_3D
  , ImageUsageFlagBits
  , pattern IMAGE_USAGE_TRANSFER_SRC_BIT
  , pattern IMAGE_USAGE_TRANSFER_DST_BIT
  , pattern IMAGE_USAGE_SAMPLED_BIT
  , pattern IMAGE_USAGE_STORAGE_BIT
  , pattern IMAGE_USAGE_COLOR_ATTACHMENT_BIT
  , pattern IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
  , pattern IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT
  , pattern IMAGE_USAGE_INPUT_ATTACHMENT_BIT
  , pattern IMAGE_USAGE_RESERVED_13_BIT_KHR
  , pattern IMAGE_USAGE_RESERVED_14_BIT_KHR
  , pattern IMAGE_USAGE_RESERVED_15_BIT_KHR
  , pattern IMAGE_USAGE_RESERVED_10_BIT_KHR
  , pattern IMAGE_USAGE_RESERVED_11_BIT_KHR
  , pattern IMAGE_USAGE_RESERVED_12_BIT_KHR
  , pattern IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV
  , pattern IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT
  , ImageUsageFlags
  , Instance(..)
  , InstanceCreateFlags
#if defined(VK_USE_PLATFORM_GGP)
  , InstanceCreateInfo(..)
#endif
  , MemoryHeap(..)
  , MemoryHeapFlagBits
  , pattern MEMORY_HEAP_DEVICE_LOCAL_BIT
  , pattern MEMORY_HEAP_MULTI_INSTANCE_BIT
  , MemoryHeapFlags
  , MemoryPropertyFlagBits
  , pattern MEMORY_PROPERTY_DEVICE_LOCAL_BIT
  , pattern MEMORY_PROPERTY_HOST_VISIBLE_BIT
  , pattern MEMORY_PROPERTY_HOST_COHERENT_BIT
  , pattern MEMORY_PROPERTY_HOST_CACHED_BIT
  , pattern MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT
  , pattern MEMORY_PROPERTY_PROTECTED_BIT
  , MemoryPropertyFlags
  , MemoryType(..)
  , PhysicalDevice(..)
  , PhysicalDeviceFeatures(..)
  , PhysicalDeviceLimits(..)
  , PhysicalDeviceMemoryProperties(..)
  , PhysicalDeviceProperties(..)
  , PhysicalDeviceSparseProperties(..)
  , PhysicalDeviceType
  , pattern PHYSICAL_DEVICE_TYPE_OTHER
  , pattern PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU
  , pattern PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
  , pattern PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU
  , pattern PHYSICAL_DEVICE_TYPE_CPU
  , QueueFamilyProperties(..)
  , QueueFlagBits
  , pattern QUEUE_GRAPHICS_BIT
  , pattern QUEUE_COMPUTE_BIT
  , pattern QUEUE_TRANSFER_BIT
  , pattern QUEUE_SPARSE_BINDING_BIT
  , pattern QUEUE_PROTECTED_BIT
  , pattern QUEUE_RESERVED_6_BIT_KHR
  , pattern QUEUE_RESERVED_5_BIT_KHR
  , QueueFlags
  , SampleCountFlagBits
  , pattern SAMPLE_COUNT_1_BIT
  , pattern SAMPLE_COUNT_2_BIT
  , pattern SAMPLE_COUNT_4_BIT
  , pattern SAMPLE_COUNT_8_BIT
  , pattern SAMPLE_COUNT_16_BIT
  , pattern SAMPLE_COUNT_32_BIT
  , pattern SAMPLE_COUNT_64_BIT
  , SampleCountFlags
#if defined(VK_USE_PLATFORM_ANDROID_KHR)
  , createInstance
#endif
  , destroyInstance
  , getNumPhysicalDevices
  , enumeratePhysicalDevices
  , enumerateAllPhysicalDevices
  , getDeviceProcAddr
  , getInstanceProcAddr
#if defined(VK_USE_PLATFORM_GGP)
  , getPhysicalDeviceFeatures
  , getPhysicalDeviceFormatProperties
  , getPhysicalDeviceImageFormatProperties
  , getPhysicalDeviceMemoryProperties
  , getPhysicalDeviceProperties
  , getNumPhysicalDeviceQueueFamilyProperties
  , getPhysicalDeviceQueueFamilyProperties
  , getAllPhysicalDeviceQueueFamilyProperties
#endif
  , withInstance
  , pattern VK_FORMAT_FEATURE_RESERVED_25_BIT_KHR
  , pattern VK_FORMAT_FEATURE_RESERVED_26_BIT_KHR
  , pattern VK_FORMAT_FEATURE_RESERVED_27_BIT_KHR
  , pattern VK_FORMAT_FEATURE_RESERVED_28_BIT_KHR
  , pattern VK_IMAGE_USAGE_RESERVED_10_BIT_KHR
  , pattern VK_IMAGE_USAGE_RESERVED_11_BIT_KHR
  , pattern VK_IMAGE_USAGE_RESERVED_12_BIT_KHR
  , pattern VK_IMAGE_USAGE_RESERVED_13_BIT_KHR
  , pattern VK_IMAGE_USAGE_RESERVED_14_BIT_KHR
  , pattern VK_IMAGE_USAGE_RESERVED_15_BIT_KHR
  , pattern VK_QUEUE_RESERVED_5_BIT_KHR
  , pattern VK_QUEUE_RESERVED_6_BIT_KHR
  ) where

import Control.Exception
  ( bracket
  )

#if defined(VK_USE_PLATFORM_GGP)
import Control.Monad
  ( (<=<)
  )
#endif
import Data.ByteString
  ( ByteString
  , useAsCString
  )
import Data.Function
  ( on
  )
import Data.Int
  ( Int32
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( generateM
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CSize(..)
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Array
  ( allocaArray
  )
import Foreign.Marshal.Utils
  ( maybeWith
  , with
  )
import Foreign.Ptr
  ( Ptr
  , nullPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkFormatFeatureFlagBits(..)
  , VkImageCreateFlagBits(..)
  , VkImageTiling(..)
  , VkImageType(..)
  , VkImageUsageFlagBits(..)
  , VkInstanceCreateFlags(..)
  , VkMemoryHeapFlagBits(..)
  , VkMemoryPropertyFlagBits(..)
  , VkPhysicalDeviceType(..)
  , VkQueueFlagBits(..)
  , VkSampleCountFlagBits(..)
  , PFN_vkAllocationFunction
  , PFN_vkFreeFunction
  , PFN_vkInternalAllocationNotification
  , PFN_vkInternalFreeNotification
  , PFN_vkReallocationFunction
  , PFN_vkVoidFunction
  , VkDevice
  , VkDeviceSize
  , VkInstance
  , VkPhysicalDevice
  , vkDestroyInstance
  , vkEnumeratePhysicalDevices
  , vkGetDeviceProcAddr
  , vkGetInstanceProcAddr
  , pattern VK_FORMAT_FEATURE_BLIT_DST_BIT
  , pattern VK_FORMAT_FEATURE_BLIT_SRC_BIT
  , pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT
  , pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT
  , pattern VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT
  , pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT
  , pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT
  , pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT
  , pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT
  , pattern VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT
  , pattern VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT
  , pattern VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT
  , pattern VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT
  , pattern VK_IMAGE_CREATE_SPARSE_ALIASED_BIT
  , pattern VK_IMAGE_CREATE_SPARSE_BINDING_BIT
  , pattern VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT
  , pattern VK_IMAGE_TILING_LINEAR
  , pattern VK_IMAGE_TILING_OPTIMAL
  , pattern VK_IMAGE_TYPE_1D
  , pattern VK_IMAGE_TYPE_2D
  , pattern VK_IMAGE_TYPE_3D
  , pattern VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
  , pattern VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
  , pattern VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT
  , pattern VK_IMAGE_USAGE_SAMPLED_BIT
  , pattern VK_IMAGE_USAGE_STORAGE_BIT
  , pattern VK_IMAGE_USAGE_TRANSFER_DST_BIT
  , pattern VK_IMAGE_USAGE_TRANSFER_SRC_BIT
  , pattern VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT
  , pattern VK_MEMORY_HEAP_DEVICE_LOCAL_BIT
  , pattern VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT
  , pattern VK_MEMORY_PROPERTY_HOST_CACHED_BIT
  , pattern VK_MEMORY_PROPERTY_HOST_COHERENT_BIT
  , pattern VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
  , pattern VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT
  , pattern VK_PHYSICAL_DEVICE_TYPE_CPU
  , pattern VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
  , pattern VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU
  , pattern VK_PHYSICAL_DEVICE_TYPE_OTHER
  , pattern VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU
  , pattern VK_QUEUE_COMPUTE_BIT
  , pattern VK_QUEUE_GRAPHICS_BIT
  , pattern VK_QUEUE_SPARSE_BINDING_BIT
  , pattern VK_QUEUE_TRANSFER_BIT
  , pattern VK_SAMPLE_COUNT_16_BIT
  , pattern VK_SAMPLE_COUNT_1_BIT
  , pattern VK_SAMPLE_COUNT_2_BIT
  , pattern VK_SAMPLE_COUNT_32_BIT
  , pattern VK_SAMPLE_COUNT_4_BIT
  , pattern VK_SAMPLE_COUNT_64_BIT
  , pattern VK_SAMPLE_COUNT_8_BIT
  )

#if defined(VK_USE_PLATFORM_ANDROID_KHR)
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( vkCreateInstance
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( vkGetPhysicalDeviceFeatures
  , vkGetPhysicalDeviceFormatProperties
  , vkGetPhysicalDeviceImageFormatProperties
  , vkGetPhysicalDeviceMemoryProperties
  , vkGetPhysicalDeviceProperties
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( vkGetPhysicalDeviceQueueFamilyProperties
  )
#endif
import Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory
  ( pattern VK_IMAGE_CREATE_PROTECTED_BIT
  , pattern VK_MEMORY_PROPERTY_PROTECTED_BIT
  , pattern VK_QUEUE_PROTECTED_BIT
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2
  ( pattern VK_IMAGE_CREATE_ALIAS_BIT
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2
  ( pattern VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation
  ( pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance1
  ( pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT
  , pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT
  , pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2
  ( pattern VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT
  , pattern VK_IMAGE_CREATE_EXTENDED_USAGE_BIT
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion
  ( pattern VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT
  , pattern VK_FORMAT_FEATURE_DISJOINT_BIT
  , pattern VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT
  , pattern VK_IMAGE_CREATE_DISJOINT_BIT
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  , InstanceCmds(..)
  )

#if defined(VK_USE_PLATFORM_ANDROID_KHR)
import Graphics.Vulkan.C.Dynamic
  ( initInstanceCmds
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map
  ( pattern VK_FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT
  , pattern VK_IMAGE_CREATE_SUBSAMPLED_BIT_EXT
  , pattern VK_IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier
  ( pattern VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations
  ( pattern VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_sampler_filter_minmax
  ( pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_IMG_filter_cubic
  ( pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG
  )
import Graphics.Vulkan.C.Extensions.VK_NV_corner_sampled_image
  ( pattern VK_IMAGE_CREATE_CORNER_SAMPLED_BIT_NV
  )
import Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image
  ( pattern VK_IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Core
  ( Format
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( FromCStruct(..)
  )
#endif



-- No documentation found for TopLevel "VkAllocationCallbacks"
data AllocationCallbacks = AllocationCallbacks
  { -- No documentation found for Nested "AllocationCallbacks" "pUserData"
  userData :: Ptr ()
  , -- No documentation found for Nested "AllocationCallbacks" "pfnAllocation"
  pfnAllocation :: PFN_vkAllocationFunction
  , -- No documentation found for Nested "AllocationCallbacks" "pfnReallocation"
  pfnReallocation :: PFN_vkReallocationFunction
  , -- No documentation found for Nested "AllocationCallbacks" "pfnFree"
  pfnFree :: PFN_vkFreeFunction
  , -- No documentation found for Nested "AllocationCallbacks" "pfnInternalAllocation"
  pfnInternalAllocation :: PFN_vkInternalAllocationNotification
  , -- No documentation found for Nested "AllocationCallbacks" "pfnInternalFree"
  pfnInternalFree :: PFN_vkInternalFreeNotification
  }
  deriving (Show, Eq)

instance Zero AllocationCallbacks where
  zero = AllocationCallbacks nullPtr
                             zero
                             zero
                             zero
                             zero
                             zero



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkApplicationInfo"
data ApplicationInfo = ApplicationInfo
  { -- No documentation found for Nested "ApplicationInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ApplicationInfo" "pApplicationName"
  applicationName :: Maybe ByteString
  , -- No documentation found for Nested "ApplicationInfo" "applicationVersion"
  applicationVersion :: Word32
  , -- No documentation found for Nested "ApplicationInfo" "pEngineName"
  engineName :: Maybe ByteString
  , -- No documentation found for Nested "ApplicationInfo" "engineVersion"
  engineVersion :: Word32
  , -- No documentation found for Nested "ApplicationInfo" "apiVersion"
  apiVersion :: Word32
  }
  deriving (Show, Eq)

instance Zero ApplicationInfo where
  zero = ApplicationInfo Nothing
                         mempty
                         zero
                         mempty
                         zero
                         zero

#endif

data Device = Device
  { deviceHandle :: VkDevice
  , deviceCmds    :: DeviceCmds
  }
  deriving Show

instance Eq Device where
  (==) = (==) `on` deviceHandle

instance Ord Device where
  compare = compare `on` deviceHandle


-- No documentation found for TopLevel "DeviceSize"
type DeviceSize = VkDeviceSize
  


-- No documentation found for TopLevel "VkExtent3D"
data Extent3D = Extent3D
  { -- No documentation found for Nested "Extent3D" "width"
  width :: Word32
  , -- No documentation found for Nested "Extent3D" "height"
  height :: Word32
  , -- No documentation found for Nested "Extent3D" "depth"
  depth :: Word32
  }
  deriving (Show, Eq)

instance Zero Extent3D where
  zero = Extent3D zero
                  zero
                  zero


-- No documentation found for TopLevel "FormatFeatureFlagBits"
type FormatFeatureFlagBits = VkFormatFeatureFlagBits


{-# complete FORMAT_FEATURE_SAMPLED_IMAGE_BIT, FORMAT_FEATURE_STORAGE_IMAGE_BIT, FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT, FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT, FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT, FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT, FORMAT_FEATURE_VERTEX_BUFFER_BIT, FORMAT_FEATURE_COLOR_ATTACHMENT_BIT, FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT, FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT, FORMAT_FEATURE_BLIT_SRC_BIT, FORMAT_FEATURE_BLIT_DST_BIT, FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT, FORMAT_FEATURE_TRANSFER_SRC_BIT, FORMAT_FEATURE_TRANSFER_DST_BIT, FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT, FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT, FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT, FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT, FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT, FORMAT_FEATURE_DISJOINT_BIT, FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT, FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG, FORMAT_FEATURE_RESERVED_27_BIT_KHR, FORMAT_FEATURE_RESERVED_28_BIT_KHR, FORMAT_FEATURE_RESERVED_25_BIT_KHR, FORMAT_FEATURE_RESERVED_26_BIT_KHR, FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT, FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT :: FormatFeatureFlagBits #-}


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_SAMPLED_IMAGE_BIT"
pattern FORMAT_FEATURE_SAMPLED_IMAGE_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_SAMPLED_IMAGE_BIT = VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_STORAGE_IMAGE_BIT"
pattern FORMAT_FEATURE_STORAGE_IMAGE_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_STORAGE_IMAGE_BIT = VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT"
pattern FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT = VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT"
pattern FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT = VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT"
pattern FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT = VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT"
pattern FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT = VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_VERTEX_BUFFER_BIT"
pattern FORMAT_FEATURE_VERTEX_BUFFER_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_VERTEX_BUFFER_BIT = VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_COLOR_ATTACHMENT_BIT"
pattern FORMAT_FEATURE_COLOR_ATTACHMENT_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_COLOR_ATTACHMENT_BIT = VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT"
pattern FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT = VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT"
pattern FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT = VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_BLIT_SRC_BIT"
pattern FORMAT_FEATURE_BLIT_SRC_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_BLIT_SRC_BIT = VK_FORMAT_FEATURE_BLIT_SRC_BIT


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_BLIT_DST_BIT"
pattern FORMAT_FEATURE_BLIT_DST_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_BLIT_DST_BIT = VK_FORMAT_FEATURE_BLIT_DST_BIT


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT"
pattern FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT = VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_TRANSFER_SRC_BIT"
pattern FORMAT_FEATURE_TRANSFER_SRC_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_TRANSFER_SRC_BIT = VK_FORMAT_FEATURE_TRANSFER_SRC_BIT


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_TRANSFER_DST_BIT"
pattern FORMAT_FEATURE_TRANSFER_DST_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_TRANSFER_DST_BIT = VK_FORMAT_FEATURE_TRANSFER_DST_BIT


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT"
pattern FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT = VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT"
pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT = VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT"
pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT = VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT"
pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT = VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT"
pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT = VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_DISJOINT_BIT"
pattern FORMAT_FEATURE_DISJOINT_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_DISJOINT_BIT = VK_FORMAT_FEATURE_DISJOINT_BIT


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT"
pattern FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT = VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG"
pattern FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG = VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_RESERVED_27_BIT_KHR"
pattern FORMAT_FEATURE_RESERVED_27_BIT_KHR :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_RESERVED_27_BIT_KHR = VK_FORMAT_FEATURE_RESERVED_27_BIT_KHR


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_RESERVED_28_BIT_KHR"
pattern FORMAT_FEATURE_RESERVED_28_BIT_KHR :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_RESERVED_28_BIT_KHR = VK_FORMAT_FEATURE_RESERVED_28_BIT_KHR


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_RESERVED_25_BIT_KHR"
pattern FORMAT_FEATURE_RESERVED_25_BIT_KHR :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_RESERVED_25_BIT_KHR = VK_FORMAT_FEATURE_RESERVED_25_BIT_KHR


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_RESERVED_26_BIT_KHR"
pattern FORMAT_FEATURE_RESERVED_26_BIT_KHR :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_RESERVED_26_BIT_KHR = VK_FORMAT_FEATURE_RESERVED_26_BIT_KHR


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT"
pattern FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT = VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT"
pattern FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT = VK_FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT

-- No documentation found for TopLevel "FormatFeatureFlags"
type FormatFeatureFlags = FormatFeatureFlagBits


-- No documentation found for TopLevel "VkFormatProperties"
data FormatProperties = FormatProperties
  { -- No documentation found for Nested "FormatProperties" "linearTilingFeatures"
  linearTilingFeatures :: FormatFeatureFlags
  , -- No documentation found for Nested "FormatProperties" "optimalTilingFeatures"
  optimalTilingFeatures :: FormatFeatureFlags
  , -- No documentation found for Nested "FormatProperties" "bufferFeatures"
  bufferFeatures :: FormatFeatureFlags
  }
  deriving (Show, Eq)

instance Zero FormatProperties where
  zero = FormatProperties zero
                          zero
                          zero


-- No documentation found for TopLevel "ImageCreateFlagBits"
type ImageCreateFlagBits = VkImageCreateFlagBits


{-# complete IMAGE_CREATE_SPARSE_BINDING_BIT, IMAGE_CREATE_SPARSE_RESIDENCY_BIT, IMAGE_CREATE_SPARSE_ALIASED_BIT, IMAGE_CREATE_MUTABLE_FORMAT_BIT, IMAGE_CREATE_CUBE_COMPATIBLE_BIT, IMAGE_CREATE_ALIAS_BIT, IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT, IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT, IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT, IMAGE_CREATE_EXTENDED_USAGE_BIT, IMAGE_CREATE_PROTECTED_BIT, IMAGE_CREATE_DISJOINT_BIT, IMAGE_CREATE_CORNER_SAMPLED_BIT_NV, IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT, IMAGE_CREATE_SUBSAMPLED_BIT_EXT :: ImageCreateFlagBits #-}


-- No documentation found for Nested "ImageCreateFlagBits" "IMAGE_CREATE_SPARSE_BINDING_BIT"
pattern IMAGE_CREATE_SPARSE_BINDING_BIT :: (a ~ ImageCreateFlagBits) => a
pattern IMAGE_CREATE_SPARSE_BINDING_BIT = VK_IMAGE_CREATE_SPARSE_BINDING_BIT


-- No documentation found for Nested "ImageCreateFlagBits" "IMAGE_CREATE_SPARSE_RESIDENCY_BIT"
pattern IMAGE_CREATE_SPARSE_RESIDENCY_BIT :: (a ~ ImageCreateFlagBits) => a
pattern IMAGE_CREATE_SPARSE_RESIDENCY_BIT = VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT


-- No documentation found for Nested "ImageCreateFlagBits" "IMAGE_CREATE_SPARSE_ALIASED_BIT"
pattern IMAGE_CREATE_SPARSE_ALIASED_BIT :: (a ~ ImageCreateFlagBits) => a
pattern IMAGE_CREATE_SPARSE_ALIASED_BIT = VK_IMAGE_CREATE_SPARSE_ALIASED_BIT


-- No documentation found for Nested "ImageCreateFlagBits" "IMAGE_CREATE_MUTABLE_FORMAT_BIT"
pattern IMAGE_CREATE_MUTABLE_FORMAT_BIT :: (a ~ ImageCreateFlagBits) => a
pattern IMAGE_CREATE_MUTABLE_FORMAT_BIT = VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT


-- No documentation found for Nested "ImageCreateFlagBits" "IMAGE_CREATE_CUBE_COMPATIBLE_BIT"
pattern IMAGE_CREATE_CUBE_COMPATIBLE_BIT :: (a ~ ImageCreateFlagBits) => a
pattern IMAGE_CREATE_CUBE_COMPATIBLE_BIT = VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT


-- No documentation found for Nested "ImageCreateFlagBits" "IMAGE_CREATE_ALIAS_BIT"
pattern IMAGE_CREATE_ALIAS_BIT :: (a ~ ImageCreateFlagBits) => a
pattern IMAGE_CREATE_ALIAS_BIT = VK_IMAGE_CREATE_ALIAS_BIT


-- No documentation found for Nested "ImageCreateFlagBits" "IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT"
pattern IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT :: (a ~ ImageCreateFlagBits) => a
pattern IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT = VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT


-- No documentation found for Nested "ImageCreateFlagBits" "IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT"
pattern IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT :: (a ~ ImageCreateFlagBits) => a
pattern IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT = VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT


-- No documentation found for Nested "ImageCreateFlagBits" "IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT"
pattern IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT :: (a ~ ImageCreateFlagBits) => a
pattern IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT = VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT


-- No documentation found for Nested "ImageCreateFlagBits" "IMAGE_CREATE_EXTENDED_USAGE_BIT"
pattern IMAGE_CREATE_EXTENDED_USAGE_BIT :: (a ~ ImageCreateFlagBits) => a
pattern IMAGE_CREATE_EXTENDED_USAGE_BIT = VK_IMAGE_CREATE_EXTENDED_USAGE_BIT


-- No documentation found for Nested "ImageCreateFlagBits" "IMAGE_CREATE_PROTECTED_BIT"
pattern IMAGE_CREATE_PROTECTED_BIT :: (a ~ ImageCreateFlagBits) => a
pattern IMAGE_CREATE_PROTECTED_BIT = VK_IMAGE_CREATE_PROTECTED_BIT


-- No documentation found for Nested "ImageCreateFlagBits" "IMAGE_CREATE_DISJOINT_BIT"
pattern IMAGE_CREATE_DISJOINT_BIT :: (a ~ ImageCreateFlagBits) => a
pattern IMAGE_CREATE_DISJOINT_BIT = VK_IMAGE_CREATE_DISJOINT_BIT


-- No documentation found for Nested "ImageCreateFlagBits" "IMAGE_CREATE_CORNER_SAMPLED_BIT_NV"
pattern IMAGE_CREATE_CORNER_SAMPLED_BIT_NV :: (a ~ ImageCreateFlagBits) => a
pattern IMAGE_CREATE_CORNER_SAMPLED_BIT_NV = VK_IMAGE_CREATE_CORNER_SAMPLED_BIT_NV


-- No documentation found for Nested "ImageCreateFlagBits" "IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT"
pattern IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT :: (a ~ ImageCreateFlagBits) => a
pattern IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT = VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT


-- No documentation found for Nested "ImageCreateFlagBits" "IMAGE_CREATE_SUBSAMPLED_BIT_EXT"
pattern IMAGE_CREATE_SUBSAMPLED_BIT_EXT :: (a ~ ImageCreateFlagBits) => a
pattern IMAGE_CREATE_SUBSAMPLED_BIT_EXT = VK_IMAGE_CREATE_SUBSAMPLED_BIT_EXT

-- No documentation found for TopLevel "ImageCreateFlags"
type ImageCreateFlags = ImageCreateFlagBits


-- No documentation found for TopLevel "VkImageFormatProperties"
data ImageFormatProperties = ImageFormatProperties
  { -- No documentation found for Nested "ImageFormatProperties" "maxExtent"
  maxExtent :: Extent3D
  , -- No documentation found for Nested "ImageFormatProperties" "maxMipLevels"
  maxMipLevels :: Word32
  , -- No documentation found for Nested "ImageFormatProperties" "maxArrayLayers"
  maxArrayLayers :: Word32
  , -- No documentation found for Nested "ImageFormatProperties" "sampleCounts"
  sampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "ImageFormatProperties" "maxResourceSize"
  maxResourceSize :: DeviceSize
  }
  deriving (Show, Eq)

instance Zero ImageFormatProperties where
  zero = ImageFormatProperties zero
                               zero
                               zero
                               zero
                               zero


-- No documentation found for TopLevel "ImageTiling"
type ImageTiling = VkImageTiling


{-# complete IMAGE_TILING_OPTIMAL, IMAGE_TILING_LINEAR, IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT :: ImageTiling #-}


-- No documentation found for Nested "ImageTiling" "IMAGE_TILING_OPTIMAL"
pattern IMAGE_TILING_OPTIMAL :: (a ~ ImageTiling) => a
pattern IMAGE_TILING_OPTIMAL = VK_IMAGE_TILING_OPTIMAL


-- No documentation found for Nested "ImageTiling" "IMAGE_TILING_LINEAR"
pattern IMAGE_TILING_LINEAR :: (a ~ ImageTiling) => a
pattern IMAGE_TILING_LINEAR = VK_IMAGE_TILING_LINEAR


-- No documentation found for Nested "ImageTiling" "IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT"
pattern IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT :: (a ~ ImageTiling) => a
pattern IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT = VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT

-- No documentation found for TopLevel "ImageType"
type ImageType = VkImageType


{-# complete IMAGE_TYPE_1D, IMAGE_TYPE_2D, IMAGE_TYPE_3D :: ImageType #-}


-- No documentation found for Nested "ImageType" "IMAGE_TYPE_1D"
pattern IMAGE_TYPE_1D :: (a ~ ImageType) => a
pattern IMAGE_TYPE_1D = VK_IMAGE_TYPE_1D


-- No documentation found for Nested "ImageType" "IMAGE_TYPE_2D"
pattern IMAGE_TYPE_2D :: (a ~ ImageType) => a
pattern IMAGE_TYPE_2D = VK_IMAGE_TYPE_2D


-- No documentation found for Nested "ImageType" "IMAGE_TYPE_3D"
pattern IMAGE_TYPE_3D :: (a ~ ImageType) => a
pattern IMAGE_TYPE_3D = VK_IMAGE_TYPE_3D

-- No documentation found for TopLevel "ImageUsageFlagBits"
type ImageUsageFlagBits = VkImageUsageFlagBits


{-# complete IMAGE_USAGE_TRANSFER_SRC_BIT, IMAGE_USAGE_TRANSFER_DST_BIT, IMAGE_USAGE_SAMPLED_BIT, IMAGE_USAGE_STORAGE_BIT, IMAGE_USAGE_COLOR_ATTACHMENT_BIT, IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT, IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT, IMAGE_USAGE_INPUT_ATTACHMENT_BIT, IMAGE_USAGE_RESERVED_13_BIT_KHR, IMAGE_USAGE_RESERVED_14_BIT_KHR, IMAGE_USAGE_RESERVED_15_BIT_KHR, IMAGE_USAGE_RESERVED_10_BIT_KHR, IMAGE_USAGE_RESERVED_11_BIT_KHR, IMAGE_USAGE_RESERVED_12_BIT_KHR, IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV, IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT :: ImageUsageFlagBits #-}


-- No documentation found for Nested "ImageUsageFlagBits" "IMAGE_USAGE_TRANSFER_SRC_BIT"
pattern IMAGE_USAGE_TRANSFER_SRC_BIT :: (a ~ ImageUsageFlagBits) => a
pattern IMAGE_USAGE_TRANSFER_SRC_BIT = VK_IMAGE_USAGE_TRANSFER_SRC_BIT


-- No documentation found for Nested "ImageUsageFlagBits" "IMAGE_USAGE_TRANSFER_DST_BIT"
pattern IMAGE_USAGE_TRANSFER_DST_BIT :: (a ~ ImageUsageFlagBits) => a
pattern IMAGE_USAGE_TRANSFER_DST_BIT = VK_IMAGE_USAGE_TRANSFER_DST_BIT


-- No documentation found for Nested "ImageUsageFlagBits" "IMAGE_USAGE_SAMPLED_BIT"
pattern IMAGE_USAGE_SAMPLED_BIT :: (a ~ ImageUsageFlagBits) => a
pattern IMAGE_USAGE_SAMPLED_BIT = VK_IMAGE_USAGE_SAMPLED_BIT


-- No documentation found for Nested "ImageUsageFlagBits" "IMAGE_USAGE_STORAGE_BIT"
pattern IMAGE_USAGE_STORAGE_BIT :: (a ~ ImageUsageFlagBits) => a
pattern IMAGE_USAGE_STORAGE_BIT = VK_IMAGE_USAGE_STORAGE_BIT


-- No documentation found for Nested "ImageUsageFlagBits" "IMAGE_USAGE_COLOR_ATTACHMENT_BIT"
pattern IMAGE_USAGE_COLOR_ATTACHMENT_BIT :: (a ~ ImageUsageFlagBits) => a
pattern IMAGE_USAGE_COLOR_ATTACHMENT_BIT = VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT


-- No documentation found for Nested "ImageUsageFlagBits" "IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT"
pattern IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT :: (a ~ ImageUsageFlagBits) => a
pattern IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT = VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT


-- No documentation found for Nested "ImageUsageFlagBits" "IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT"
pattern IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT :: (a ~ ImageUsageFlagBits) => a
pattern IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT = VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT


-- No documentation found for Nested "ImageUsageFlagBits" "IMAGE_USAGE_INPUT_ATTACHMENT_BIT"
pattern IMAGE_USAGE_INPUT_ATTACHMENT_BIT :: (a ~ ImageUsageFlagBits) => a
pattern IMAGE_USAGE_INPUT_ATTACHMENT_BIT = VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT


-- No documentation found for Nested "ImageUsageFlagBits" "IMAGE_USAGE_RESERVED_13_BIT_KHR"
pattern IMAGE_USAGE_RESERVED_13_BIT_KHR :: (a ~ ImageUsageFlagBits) => a
pattern IMAGE_USAGE_RESERVED_13_BIT_KHR = VK_IMAGE_USAGE_RESERVED_13_BIT_KHR


-- No documentation found for Nested "ImageUsageFlagBits" "IMAGE_USAGE_RESERVED_14_BIT_KHR"
pattern IMAGE_USAGE_RESERVED_14_BIT_KHR :: (a ~ ImageUsageFlagBits) => a
pattern IMAGE_USAGE_RESERVED_14_BIT_KHR = VK_IMAGE_USAGE_RESERVED_14_BIT_KHR


-- No documentation found for Nested "ImageUsageFlagBits" "IMAGE_USAGE_RESERVED_15_BIT_KHR"
pattern IMAGE_USAGE_RESERVED_15_BIT_KHR :: (a ~ ImageUsageFlagBits) => a
pattern IMAGE_USAGE_RESERVED_15_BIT_KHR = VK_IMAGE_USAGE_RESERVED_15_BIT_KHR


-- No documentation found for Nested "ImageUsageFlagBits" "IMAGE_USAGE_RESERVED_10_BIT_KHR"
pattern IMAGE_USAGE_RESERVED_10_BIT_KHR :: (a ~ ImageUsageFlagBits) => a
pattern IMAGE_USAGE_RESERVED_10_BIT_KHR = VK_IMAGE_USAGE_RESERVED_10_BIT_KHR


-- No documentation found for Nested "ImageUsageFlagBits" "IMAGE_USAGE_RESERVED_11_BIT_KHR"
pattern IMAGE_USAGE_RESERVED_11_BIT_KHR :: (a ~ ImageUsageFlagBits) => a
pattern IMAGE_USAGE_RESERVED_11_BIT_KHR = VK_IMAGE_USAGE_RESERVED_11_BIT_KHR


-- No documentation found for Nested "ImageUsageFlagBits" "IMAGE_USAGE_RESERVED_12_BIT_KHR"
pattern IMAGE_USAGE_RESERVED_12_BIT_KHR :: (a ~ ImageUsageFlagBits) => a
pattern IMAGE_USAGE_RESERVED_12_BIT_KHR = VK_IMAGE_USAGE_RESERVED_12_BIT_KHR


-- No documentation found for Nested "ImageUsageFlagBits" "IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV"
pattern IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV :: (a ~ ImageUsageFlagBits) => a
pattern IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV = VK_IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV


-- No documentation found for Nested "ImageUsageFlagBits" "IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT"
pattern IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT :: (a ~ ImageUsageFlagBits) => a
pattern IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT = VK_IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT

-- No documentation found for TopLevel "ImageUsageFlags"
type ImageUsageFlags = ImageUsageFlagBits

data Instance = Instance
  { instanceHandle :: VkInstance
  , instanceCmds    :: InstanceCmds
  }
  deriving Show

instance Eq Instance where
  (==) = (==) `on` instanceHandle

instance Ord Instance where
  compare = compare `on` instanceHandle


-- No documentation found for TopLevel "InstanceCreateFlags"
type InstanceCreateFlags = VkInstanceCreateFlags


-- No complete pragma for InstanceCreateFlags as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkInstanceCreateInfo"
data InstanceCreateInfo = InstanceCreateInfo
  { -- No documentation found for Nested "InstanceCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "InstanceCreateInfo" "flags"
  flags :: InstanceCreateFlags
  , -- No documentation found for Nested "InstanceCreateInfo" "pApplicationInfo"
  applicationInfo :: Maybe ApplicationInfo
  , -- No documentation found for Nested "InstanceCreateInfo" "ppEnabledLayerNames"
  enabledLayerNames :: Vector ByteString
  , -- No documentation found for Nested "InstanceCreateInfo" "ppEnabledExtensionNames"
  enabledExtensionNames :: Vector ByteString
  }
  deriving (Show, Eq)

instance Zero InstanceCreateInfo where
  zero = InstanceCreateInfo Nothing
                            zero
                            Nothing
                            mempty
                            mempty

#endif


-- No documentation found for TopLevel "VkMemoryHeap"
data MemoryHeap = MemoryHeap
  { -- No documentation found for Nested "MemoryHeap" "size"
  size :: DeviceSize
  , -- No documentation found for Nested "MemoryHeap" "flags"
  flags :: MemoryHeapFlags
  }
  deriving (Show, Eq)

instance Zero MemoryHeap where
  zero = MemoryHeap zero
                    zero


-- No documentation found for TopLevel "MemoryHeapFlagBits"
type MemoryHeapFlagBits = VkMemoryHeapFlagBits


{-# complete MEMORY_HEAP_DEVICE_LOCAL_BIT, MEMORY_HEAP_MULTI_INSTANCE_BIT :: MemoryHeapFlagBits #-}


-- No documentation found for Nested "MemoryHeapFlagBits" "MEMORY_HEAP_DEVICE_LOCAL_BIT"
pattern MEMORY_HEAP_DEVICE_LOCAL_BIT :: (a ~ MemoryHeapFlagBits) => a
pattern MEMORY_HEAP_DEVICE_LOCAL_BIT = VK_MEMORY_HEAP_DEVICE_LOCAL_BIT


-- No documentation found for Nested "MemoryHeapFlagBits" "MEMORY_HEAP_MULTI_INSTANCE_BIT"
pattern MEMORY_HEAP_MULTI_INSTANCE_BIT :: (a ~ MemoryHeapFlagBits) => a
pattern MEMORY_HEAP_MULTI_INSTANCE_BIT = VK_MEMORY_HEAP_MULTI_INSTANCE_BIT

-- No documentation found for TopLevel "MemoryHeapFlags"
type MemoryHeapFlags = MemoryHeapFlagBits

-- No documentation found for TopLevel "MemoryPropertyFlagBits"
type MemoryPropertyFlagBits = VkMemoryPropertyFlagBits


{-# complete MEMORY_PROPERTY_DEVICE_LOCAL_BIT, MEMORY_PROPERTY_HOST_VISIBLE_BIT, MEMORY_PROPERTY_HOST_COHERENT_BIT, MEMORY_PROPERTY_HOST_CACHED_BIT, MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT, MEMORY_PROPERTY_PROTECTED_BIT :: MemoryPropertyFlagBits #-}


-- No documentation found for Nested "MemoryPropertyFlagBits" "MEMORY_PROPERTY_DEVICE_LOCAL_BIT"
pattern MEMORY_PROPERTY_DEVICE_LOCAL_BIT :: (a ~ MemoryPropertyFlagBits) => a
pattern MEMORY_PROPERTY_DEVICE_LOCAL_BIT = VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT


-- No documentation found for Nested "MemoryPropertyFlagBits" "MEMORY_PROPERTY_HOST_VISIBLE_BIT"
pattern MEMORY_PROPERTY_HOST_VISIBLE_BIT :: (a ~ MemoryPropertyFlagBits) => a
pattern MEMORY_PROPERTY_HOST_VISIBLE_BIT = VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT


-- No documentation found for Nested "MemoryPropertyFlagBits" "MEMORY_PROPERTY_HOST_COHERENT_BIT"
pattern MEMORY_PROPERTY_HOST_COHERENT_BIT :: (a ~ MemoryPropertyFlagBits) => a
pattern MEMORY_PROPERTY_HOST_COHERENT_BIT = VK_MEMORY_PROPERTY_HOST_COHERENT_BIT


-- No documentation found for Nested "MemoryPropertyFlagBits" "MEMORY_PROPERTY_HOST_CACHED_BIT"
pattern MEMORY_PROPERTY_HOST_CACHED_BIT :: (a ~ MemoryPropertyFlagBits) => a
pattern MEMORY_PROPERTY_HOST_CACHED_BIT = VK_MEMORY_PROPERTY_HOST_CACHED_BIT


-- No documentation found for Nested "MemoryPropertyFlagBits" "MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT"
pattern MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT :: (a ~ MemoryPropertyFlagBits) => a
pattern MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT = VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT


-- No documentation found for Nested "MemoryPropertyFlagBits" "MEMORY_PROPERTY_PROTECTED_BIT"
pattern MEMORY_PROPERTY_PROTECTED_BIT :: (a ~ MemoryPropertyFlagBits) => a
pattern MEMORY_PROPERTY_PROTECTED_BIT = VK_MEMORY_PROPERTY_PROTECTED_BIT

-- No documentation found for TopLevel "MemoryPropertyFlags"
type MemoryPropertyFlags = MemoryPropertyFlagBits


-- No documentation found for TopLevel "VkMemoryType"
data MemoryType = MemoryType
  { -- No documentation found for Nested "MemoryType" "propertyFlags"
  propertyFlags :: MemoryPropertyFlags
  , -- No documentation found for Nested "MemoryType" "heapIndex"
  heapIndex :: Word32
  }
  deriving (Show, Eq)

instance Zero MemoryType where
  zero = MemoryType zero
                    zero


data PhysicalDevice = PhysicalDevice
  { physicalDeviceHandle :: VkPhysicalDevice
  , physicalDeviceCmds    :: InstanceCmds
  }
  deriving Show

instance Eq PhysicalDevice where
  (==) = (==) `on` physicalDeviceHandle

instance Ord PhysicalDevice where
  compare = compare `on` physicalDeviceHandle



-- No documentation found for TopLevel "VkPhysicalDeviceFeatures"
data PhysicalDeviceFeatures = PhysicalDeviceFeatures
  { -- No documentation found for Nested "PhysicalDeviceFeatures" "robustBufferAccess"
  robustBufferAccess :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "fullDrawIndexUint32"
  fullDrawIndexUint32 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "imageCubeArray"
  imageCubeArray :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "independentBlend"
  independentBlend :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "geometryShader"
  geometryShader :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "tessellationShader"
  tessellationShader :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "sampleRateShading"
  sampleRateShading :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "dualSrcBlend"
  dualSrcBlend :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "logicOp"
  logicOp :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "multiDrawIndirect"
  multiDrawIndirect :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "drawIndirectFirstInstance"
  drawIndirectFirstInstance :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "depthClamp"
  depthClamp :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "depthBiasClamp"
  depthBiasClamp :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "fillModeNonSolid"
  fillModeNonSolid :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "depthBounds"
  depthBounds :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "wideLines"
  wideLines :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "largePoints"
  largePoints :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "alphaToOne"
  alphaToOne :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "multiViewport"
  multiViewport :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "samplerAnisotropy"
  samplerAnisotropy :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "textureCompressionETC2"
  textureCompressionETC2 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "textureCompressionASTC_LDR"
  textureCompressionASTC_LDR :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "textureCompressionBC"
  textureCompressionBC :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "occlusionQueryPrecise"
  occlusionQueryPrecise :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "pipelineStatisticsQuery"
  pipelineStatisticsQuery :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "vertexPipelineStoresAndAtomics"
  vertexPipelineStoresAndAtomics :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "fragmentStoresAndAtomics"
  fragmentStoresAndAtomics :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderTessellationAndGeometryPointSize"
  shaderTessellationAndGeometryPointSize :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderImageGatherExtended"
  shaderImageGatherExtended :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderStorageImageExtendedFormats"
  shaderStorageImageExtendedFormats :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderStorageImageMultisample"
  shaderStorageImageMultisample :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderStorageImageReadWithoutFormat"
  shaderStorageImageReadWithoutFormat :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderStorageImageWriteWithoutFormat"
  shaderStorageImageWriteWithoutFormat :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderUniformBufferArrayDynamicIndexing"
  shaderUniformBufferArrayDynamicIndexing :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderSampledImageArrayDynamicIndexing"
  shaderSampledImageArrayDynamicIndexing :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderStorageBufferArrayDynamicIndexing"
  shaderStorageBufferArrayDynamicIndexing :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderStorageImageArrayDynamicIndexing"
  shaderStorageImageArrayDynamicIndexing :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderClipDistance"
  shaderClipDistance :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderCullDistance"
  shaderCullDistance :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderFloat64"
  shaderFloat64 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderInt64"
  shaderInt64 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderInt16"
  shaderInt16 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderResourceResidency"
  shaderResourceResidency :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderResourceMinLod"
  shaderResourceMinLod :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "sparseBinding"
  sparseBinding :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "sparseResidencyBuffer"
  sparseResidencyBuffer :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "sparseResidencyImage2D"
  sparseResidencyImage2D :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "sparseResidencyImage3D"
  sparseResidencyImage3D :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "sparseResidency2Samples"
  sparseResidency2Samples :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "sparseResidency4Samples"
  sparseResidency4Samples :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "sparseResidency8Samples"
  sparseResidency8Samples :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "sparseResidency16Samples"
  sparseResidency16Samples :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "sparseResidencyAliased"
  sparseResidencyAliased :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "variableMultisampleRate"
  variableMultisampleRate :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "inheritedQueries"
  inheritedQueries :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceFeatures where
  zero = PhysicalDeviceFeatures False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False



-- No documentation found for TopLevel "VkPhysicalDeviceLimits"
data PhysicalDeviceLimits = PhysicalDeviceLimits
  { -- No documentation found for Nested "PhysicalDeviceLimits" "maxImageDimension1D"
  maxImageDimension1D :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxImageDimension2D"
  maxImageDimension2D :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxImageDimension3D"
  maxImageDimension3D :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxImageDimensionCube"
  maxImageDimensionCube :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxImageArrayLayers"
  maxImageArrayLayers :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxTexelBufferElements"
  maxTexelBufferElements :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxUniformBufferRange"
  maxUniformBufferRange :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxStorageBufferRange"
  maxStorageBufferRange :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxPushConstantsSize"
  maxPushConstantsSize :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxMemoryAllocationCount"
  maxMemoryAllocationCount :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxSamplerAllocationCount"
  maxSamplerAllocationCount :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "bufferImageGranularity"
  bufferImageGranularity :: DeviceSize
  , -- No documentation found for Nested "PhysicalDeviceLimits" "sparseAddressSpaceSize"
  sparseAddressSpaceSize :: DeviceSize
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxBoundDescriptorSets"
  maxBoundDescriptorSets :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxPerStageDescriptorSamplers"
  maxPerStageDescriptorSamplers :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxPerStageDescriptorUniformBuffers"
  maxPerStageDescriptorUniformBuffers :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxPerStageDescriptorStorageBuffers"
  maxPerStageDescriptorStorageBuffers :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxPerStageDescriptorSampledImages"
  maxPerStageDescriptorSampledImages :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxPerStageDescriptorStorageImages"
  maxPerStageDescriptorStorageImages :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxPerStageDescriptorInputAttachments"
  maxPerStageDescriptorInputAttachments :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxPerStageResources"
  maxPerStageResources :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxDescriptorSetSamplers"
  maxDescriptorSetSamplers :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxDescriptorSetUniformBuffers"
  maxDescriptorSetUniformBuffers :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxDescriptorSetUniformBuffersDynamic"
  maxDescriptorSetUniformBuffersDynamic :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxDescriptorSetStorageBuffers"
  maxDescriptorSetStorageBuffers :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxDescriptorSetStorageBuffersDynamic"
  maxDescriptorSetStorageBuffersDynamic :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxDescriptorSetSampledImages"
  maxDescriptorSetSampledImages :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxDescriptorSetStorageImages"
  maxDescriptorSetStorageImages :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxDescriptorSetInputAttachments"
  maxDescriptorSetInputAttachments :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxVertexInputAttributes"
  maxVertexInputAttributes :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxVertexInputBindings"
  maxVertexInputBindings :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxVertexInputAttributeOffset"
  maxVertexInputAttributeOffset :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxVertexInputBindingStride"
  maxVertexInputBindingStride :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxVertexOutputComponents"
  maxVertexOutputComponents :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxTessellationGenerationLevel"
  maxTessellationGenerationLevel :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxTessellationPatchSize"
  maxTessellationPatchSize :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxTessellationControlPerVertexInputComponents"
  maxTessellationControlPerVertexInputComponents :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxTessellationControlPerVertexOutputComponents"
  maxTessellationControlPerVertexOutputComponents :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxTessellationControlPerPatchOutputComponents"
  maxTessellationControlPerPatchOutputComponents :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxTessellationControlTotalOutputComponents"
  maxTessellationControlTotalOutputComponents :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxTessellationEvaluationInputComponents"
  maxTessellationEvaluationInputComponents :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxTessellationEvaluationOutputComponents"
  maxTessellationEvaluationOutputComponents :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxGeometryShaderInvocations"
  maxGeometryShaderInvocations :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxGeometryInputComponents"
  maxGeometryInputComponents :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxGeometryOutputComponents"
  maxGeometryOutputComponents :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxGeometryOutputVertices"
  maxGeometryOutputVertices :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxGeometryTotalOutputComponents"
  maxGeometryTotalOutputComponents :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxFragmentInputComponents"
  maxFragmentInputComponents :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxFragmentOutputAttachments"
  maxFragmentOutputAttachments :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxFragmentDualSrcAttachments"
  maxFragmentDualSrcAttachments :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxFragmentCombinedOutputResources"
  maxFragmentCombinedOutputResources :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxComputeSharedMemorySize"
  maxComputeSharedMemorySize :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxComputeWorkGroupCount"
  maxComputeWorkGroupCount :: (Word32, Word32, Word32)
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxComputeWorkGroupInvocations"
  maxComputeWorkGroupInvocations :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxComputeWorkGroupSize"
  maxComputeWorkGroupSize :: (Word32, Word32, Word32)
  , -- No documentation found for Nested "PhysicalDeviceLimits" "subPixelPrecisionBits"
  subPixelPrecisionBits :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "subTexelPrecisionBits"
  subTexelPrecisionBits :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "mipmapPrecisionBits"
  mipmapPrecisionBits :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxDrawIndexedIndexValue"
  maxDrawIndexedIndexValue :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxDrawIndirectCount"
  maxDrawIndirectCount :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxSamplerLodBias"
  maxSamplerLodBias :: Float
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxSamplerAnisotropy"
  maxSamplerAnisotropy :: Float
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxViewports"
  maxViewports :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxViewportDimensions"
  maxViewportDimensions :: (Word32, Word32)
  , -- No documentation found for Nested "PhysicalDeviceLimits" "viewportBoundsRange"
  viewportBoundsRange :: (Float, Float)
  , -- No documentation found for Nested "PhysicalDeviceLimits" "viewportSubPixelBits"
  viewportSubPixelBits :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "minMemoryMapAlignment"
  minMemoryMapAlignment :: CSize
  , -- No documentation found for Nested "PhysicalDeviceLimits" "minTexelBufferOffsetAlignment"
  minTexelBufferOffsetAlignment :: DeviceSize
  , -- No documentation found for Nested "PhysicalDeviceLimits" "minUniformBufferOffsetAlignment"
  minUniformBufferOffsetAlignment :: DeviceSize
  , -- No documentation found for Nested "PhysicalDeviceLimits" "minStorageBufferOffsetAlignment"
  minStorageBufferOffsetAlignment :: DeviceSize
  , -- No documentation found for Nested "PhysicalDeviceLimits" "minTexelOffset"
  minTexelOffset :: Int32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxTexelOffset"
  maxTexelOffset :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "minTexelGatherOffset"
  minTexelGatherOffset :: Int32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxTexelGatherOffset"
  maxTexelGatherOffset :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "minInterpolationOffset"
  minInterpolationOffset :: Float
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxInterpolationOffset"
  maxInterpolationOffset :: Float
  , -- No documentation found for Nested "PhysicalDeviceLimits" "subPixelInterpolationOffsetBits"
  subPixelInterpolationOffsetBits :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxFramebufferWidth"
  maxFramebufferWidth :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxFramebufferHeight"
  maxFramebufferHeight :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxFramebufferLayers"
  maxFramebufferLayers :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "framebufferColorSampleCounts"
  framebufferColorSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "PhysicalDeviceLimits" "framebufferDepthSampleCounts"
  framebufferDepthSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "PhysicalDeviceLimits" "framebufferStencilSampleCounts"
  framebufferStencilSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "PhysicalDeviceLimits" "framebufferNoAttachmentsSampleCounts"
  framebufferNoAttachmentsSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxColorAttachments"
  maxColorAttachments :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "sampledImageColorSampleCounts"
  sampledImageColorSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "PhysicalDeviceLimits" "sampledImageIntegerSampleCounts"
  sampledImageIntegerSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "PhysicalDeviceLimits" "sampledImageDepthSampleCounts"
  sampledImageDepthSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "PhysicalDeviceLimits" "sampledImageStencilSampleCounts"
  sampledImageStencilSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "PhysicalDeviceLimits" "storageImageSampleCounts"
  storageImageSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxSampleMaskWords"
  maxSampleMaskWords :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "timestampComputeAndGraphics"
  timestampComputeAndGraphics :: Bool
  , -- No documentation found for Nested "PhysicalDeviceLimits" "timestampPeriod"
  timestampPeriod :: Float
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxClipDistances"
  maxClipDistances :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxCullDistances"
  maxCullDistances :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxCombinedClipAndCullDistances"
  maxCombinedClipAndCullDistances :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "discreteQueuePriorities"
  discreteQueuePriorities :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "pointSizeRange"
  pointSizeRange :: (Float, Float)
  , -- No documentation found for Nested "PhysicalDeviceLimits" "lineWidthRange"
  lineWidthRange :: (Float, Float)
  , -- No documentation found for Nested "PhysicalDeviceLimits" "pointSizeGranularity"
  pointSizeGranularity :: Float
  , -- No documentation found for Nested "PhysicalDeviceLimits" "lineWidthGranularity"
  lineWidthGranularity :: Float
  , -- No documentation found for Nested "PhysicalDeviceLimits" "strictLines"
  strictLines :: Bool
  , -- No documentation found for Nested "PhysicalDeviceLimits" "standardSampleLocations"
  standardSampleLocations :: Bool
  , -- No documentation found for Nested "PhysicalDeviceLimits" "optimalBufferCopyOffsetAlignment"
  optimalBufferCopyOffsetAlignment :: DeviceSize
  , -- No documentation found for Nested "PhysicalDeviceLimits" "optimalBufferCopyRowPitchAlignment"
  optimalBufferCopyRowPitchAlignment :: DeviceSize
  , -- No documentation found for Nested "PhysicalDeviceLimits" "nonCoherentAtomSize"
  nonCoherentAtomSize :: DeviceSize
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceLimits where
  zero = PhysicalDeviceLimits zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              (zero, zero, zero)
                              zero
                              (zero, zero, zero)
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              (zero, zero)
                              (zero, zero)
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              False
                              zero
                              zero
                              zero
                              zero
                              zero
                              (zero, zero)
                              (zero, zero)
                              zero
                              zero
                              False
                              False
                              zero
                              zero
                              zero



-- No documentation found for TopLevel "VkPhysicalDeviceMemoryProperties"
data PhysicalDeviceMemoryProperties = PhysicalDeviceMemoryProperties
  { -- No documentation found for Nested "PhysicalDeviceMemoryProperties" "memoryTypes"
  memoryTypes :: Vector MemoryType
  , -- No documentation found for Nested "PhysicalDeviceMemoryProperties" "memoryHeaps"
  memoryHeaps :: Vector MemoryHeap
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceMemoryProperties where
  zero = PhysicalDeviceMemoryProperties mempty
                                        mempty



-- No documentation found for TopLevel "VkPhysicalDeviceProperties"
data PhysicalDeviceProperties = PhysicalDeviceProperties
  { -- No documentation found for Nested "PhysicalDeviceProperties" "apiVersion"
  apiVersion :: Word32
  , -- No documentation found for Nested "PhysicalDeviceProperties" "driverVersion"
  driverVersion :: Word32
  , -- No documentation found for Nested "PhysicalDeviceProperties" "vendorID"
  vendorID :: Word32
  , -- No documentation found for Nested "PhysicalDeviceProperties" "deviceID"
  deviceID :: Word32
  , -- No documentation found for Nested "PhysicalDeviceProperties" "deviceType"
  deviceType :: PhysicalDeviceType
  , -- No documentation found for Nested "PhysicalDeviceProperties" "deviceName"
  deviceName :: ByteString
  , -- No documentation found for Nested "PhysicalDeviceProperties" "pipelineCacheUUID"
  pipelineCacheUUID :: ByteString
  , -- No documentation found for Nested "PhysicalDeviceProperties" "limits"
  limits :: PhysicalDeviceLimits
  , -- No documentation found for Nested "PhysicalDeviceProperties" "sparseProperties"
  sparseProperties :: PhysicalDeviceSparseProperties
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceProperties where
  zero = PhysicalDeviceProperties zero
                                  zero
                                  zero
                                  zero
                                  zero
                                  mempty
                                  mempty
                                  zero
                                  zero



-- No documentation found for TopLevel "VkPhysicalDeviceSparseProperties"
data PhysicalDeviceSparseProperties = PhysicalDeviceSparseProperties
  { -- No documentation found for Nested "PhysicalDeviceSparseProperties" "residencyStandard2DBlockShape"
  residencyStandard2DBlockShape :: Bool
  , -- No documentation found for Nested "PhysicalDeviceSparseProperties" "residencyStandard2DMultisampleBlockShape"
  residencyStandard2DMultisampleBlockShape :: Bool
  , -- No documentation found for Nested "PhysicalDeviceSparseProperties" "residencyStandard3DBlockShape"
  residencyStandard3DBlockShape :: Bool
  , -- No documentation found for Nested "PhysicalDeviceSparseProperties" "residencyAlignedMipSize"
  residencyAlignedMipSize :: Bool
  , -- No documentation found for Nested "PhysicalDeviceSparseProperties" "residencyNonResidentStrict"
  residencyNonResidentStrict :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceSparseProperties where
  zero = PhysicalDeviceSparseProperties False
                                        False
                                        False
                                        False
                                        False


-- No documentation found for TopLevel "PhysicalDeviceType"
type PhysicalDeviceType = VkPhysicalDeviceType


{-# complete PHYSICAL_DEVICE_TYPE_OTHER, PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU, PHYSICAL_DEVICE_TYPE_DISCRETE_GPU, PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU, PHYSICAL_DEVICE_TYPE_CPU :: PhysicalDeviceType #-}


-- No documentation found for Nested "PhysicalDeviceType" "PHYSICAL_DEVICE_TYPE_OTHER"
pattern PHYSICAL_DEVICE_TYPE_OTHER :: (a ~ PhysicalDeviceType) => a
pattern PHYSICAL_DEVICE_TYPE_OTHER = VK_PHYSICAL_DEVICE_TYPE_OTHER


-- No documentation found for Nested "PhysicalDeviceType" "PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU"
pattern PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU :: (a ~ PhysicalDeviceType) => a
pattern PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU = VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU


-- No documentation found for Nested "PhysicalDeviceType" "PHYSICAL_DEVICE_TYPE_DISCRETE_GPU"
pattern PHYSICAL_DEVICE_TYPE_DISCRETE_GPU :: (a ~ PhysicalDeviceType) => a
pattern PHYSICAL_DEVICE_TYPE_DISCRETE_GPU = VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU


-- No documentation found for Nested "PhysicalDeviceType" "PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU"
pattern PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU :: (a ~ PhysicalDeviceType) => a
pattern PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU = VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU


-- No documentation found for Nested "PhysicalDeviceType" "PHYSICAL_DEVICE_TYPE_CPU"
pattern PHYSICAL_DEVICE_TYPE_CPU :: (a ~ PhysicalDeviceType) => a
pattern PHYSICAL_DEVICE_TYPE_CPU = VK_PHYSICAL_DEVICE_TYPE_CPU


-- No documentation found for TopLevel "VkQueueFamilyProperties"
data QueueFamilyProperties = QueueFamilyProperties
  { -- No documentation found for Nested "QueueFamilyProperties" "queueFlags"
  queueFlags :: QueueFlags
  , -- No documentation found for Nested "QueueFamilyProperties" "queueCount"
  queueCount :: Word32
  , -- No documentation found for Nested "QueueFamilyProperties" "timestampValidBits"
  timestampValidBits :: Word32
  , -- No documentation found for Nested "QueueFamilyProperties" "minImageTransferGranularity"
  minImageTransferGranularity :: Extent3D
  }
  deriving (Show, Eq)

instance Zero QueueFamilyProperties where
  zero = QueueFamilyProperties zero
                               zero
                               zero
                               zero


-- No documentation found for TopLevel "QueueFlagBits"
type QueueFlagBits = VkQueueFlagBits


{-# complete QUEUE_GRAPHICS_BIT, QUEUE_COMPUTE_BIT, QUEUE_TRANSFER_BIT, QUEUE_SPARSE_BINDING_BIT, QUEUE_PROTECTED_BIT, QUEUE_RESERVED_6_BIT_KHR, QUEUE_RESERVED_5_BIT_KHR :: QueueFlagBits #-}


-- No documentation found for Nested "QueueFlagBits" "QUEUE_GRAPHICS_BIT"
pattern QUEUE_GRAPHICS_BIT :: (a ~ QueueFlagBits) => a
pattern QUEUE_GRAPHICS_BIT = VK_QUEUE_GRAPHICS_BIT


-- No documentation found for Nested "QueueFlagBits" "QUEUE_COMPUTE_BIT"
pattern QUEUE_COMPUTE_BIT :: (a ~ QueueFlagBits) => a
pattern QUEUE_COMPUTE_BIT = VK_QUEUE_COMPUTE_BIT


-- No documentation found for Nested "QueueFlagBits" "QUEUE_TRANSFER_BIT"
pattern QUEUE_TRANSFER_BIT :: (a ~ QueueFlagBits) => a
pattern QUEUE_TRANSFER_BIT = VK_QUEUE_TRANSFER_BIT


-- No documentation found for Nested "QueueFlagBits" "QUEUE_SPARSE_BINDING_BIT"
pattern QUEUE_SPARSE_BINDING_BIT :: (a ~ QueueFlagBits) => a
pattern QUEUE_SPARSE_BINDING_BIT = VK_QUEUE_SPARSE_BINDING_BIT


-- No documentation found for Nested "QueueFlagBits" "QUEUE_PROTECTED_BIT"
pattern QUEUE_PROTECTED_BIT :: (a ~ QueueFlagBits) => a
pattern QUEUE_PROTECTED_BIT = VK_QUEUE_PROTECTED_BIT


-- No documentation found for Nested "QueueFlagBits" "QUEUE_RESERVED_6_BIT_KHR"
pattern QUEUE_RESERVED_6_BIT_KHR :: (a ~ QueueFlagBits) => a
pattern QUEUE_RESERVED_6_BIT_KHR = VK_QUEUE_RESERVED_6_BIT_KHR


-- No documentation found for Nested "QueueFlagBits" "QUEUE_RESERVED_5_BIT_KHR"
pattern QUEUE_RESERVED_5_BIT_KHR :: (a ~ QueueFlagBits) => a
pattern QUEUE_RESERVED_5_BIT_KHR = VK_QUEUE_RESERVED_5_BIT_KHR

-- No documentation found for TopLevel "QueueFlags"
type QueueFlags = QueueFlagBits

-- No documentation found for TopLevel "SampleCountFlagBits"
type SampleCountFlagBits = VkSampleCountFlagBits


{-# complete SAMPLE_COUNT_1_BIT, SAMPLE_COUNT_2_BIT, SAMPLE_COUNT_4_BIT, SAMPLE_COUNT_8_BIT, SAMPLE_COUNT_16_BIT, SAMPLE_COUNT_32_BIT, SAMPLE_COUNT_64_BIT :: SampleCountFlagBits #-}


-- No documentation found for Nested "SampleCountFlagBits" "SAMPLE_COUNT_1_BIT"
pattern SAMPLE_COUNT_1_BIT :: (a ~ SampleCountFlagBits) => a
pattern SAMPLE_COUNT_1_BIT = VK_SAMPLE_COUNT_1_BIT


-- No documentation found for Nested "SampleCountFlagBits" "SAMPLE_COUNT_2_BIT"
pattern SAMPLE_COUNT_2_BIT :: (a ~ SampleCountFlagBits) => a
pattern SAMPLE_COUNT_2_BIT = VK_SAMPLE_COUNT_2_BIT


-- No documentation found for Nested "SampleCountFlagBits" "SAMPLE_COUNT_4_BIT"
pattern SAMPLE_COUNT_4_BIT :: (a ~ SampleCountFlagBits) => a
pattern SAMPLE_COUNT_4_BIT = VK_SAMPLE_COUNT_4_BIT


-- No documentation found for Nested "SampleCountFlagBits" "SAMPLE_COUNT_8_BIT"
pattern SAMPLE_COUNT_8_BIT :: (a ~ SampleCountFlagBits) => a
pattern SAMPLE_COUNT_8_BIT = VK_SAMPLE_COUNT_8_BIT


-- No documentation found for Nested "SampleCountFlagBits" "SAMPLE_COUNT_16_BIT"
pattern SAMPLE_COUNT_16_BIT :: (a ~ SampleCountFlagBits) => a
pattern SAMPLE_COUNT_16_BIT = VK_SAMPLE_COUNT_16_BIT


-- No documentation found for Nested "SampleCountFlagBits" "SAMPLE_COUNT_32_BIT"
pattern SAMPLE_COUNT_32_BIT :: (a ~ SampleCountFlagBits) => a
pattern SAMPLE_COUNT_32_BIT = VK_SAMPLE_COUNT_32_BIT


-- No documentation found for Nested "SampleCountFlagBits" "SAMPLE_COUNT_64_BIT"
pattern SAMPLE_COUNT_64_BIT :: (a ~ SampleCountFlagBits) => a
pattern SAMPLE_COUNT_64_BIT = VK_SAMPLE_COUNT_64_BIT

-- No documentation found for TopLevel "SampleCountFlags"
type SampleCountFlags = SampleCountFlagBits


#if defined(VK_USE_PLATFORM_ANDROID_KHR)

-- No documentation found for TopLevel "vkCreateInstance"
createInstance :: InstanceCreateInfo ->  Maybe AllocationCallbacks ->  IO (Instance)
createInstance = undefined {- {wrapped (pretty cName) :: Doc ()} -}
#endif


-- No documentation found for TopLevel "vkDestroyInstance"
destroyInstance :: Instance ->  Maybe AllocationCallbacks ->  IO ()
destroyInstance = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkEnumeratePhysicalDevices"
getNumPhysicalDevices :: Instance ->  IO (VkResult, Word32)
getNumPhysicalDevices = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "vkEnumeratePhysicalDevices"
enumeratePhysicalDevices :: Instance ->  Word32 ->  IO (VkResult, Vector PhysicalDevice)
enumeratePhysicalDevices = undefined {- {wrapped (pretty cName) :: Doc ()} -}
-- | Returns all the values available from 'enumeratePhysicalDevices'.
enumerateAllPhysicalDevices :: Instance ->  IO (Vector PhysicalDevice)
enumerateAllPhysicalDevices instance' =
  snd <$> getNumPhysicalDevices instance'
    >>= \num -> snd <$> enumeratePhysicalDevices instance' num



-- No documentation found for TopLevel "vkGetDeviceProcAddr"
getDeviceProcAddr :: Device ->  ByteString ->  IO (PFN_vkVoidFunction)
getDeviceProcAddr = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkGetInstanceProcAddr"
getInstanceProcAddr :: Instance ->  ByteString ->  IO (PFN_vkVoidFunction)
getInstanceProcAddr = undefined {- {wrapped (pretty cName) :: Doc ()} -}


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetPhysicalDeviceFeatures"
getPhysicalDeviceFeatures :: PhysicalDevice ->  IO (PhysicalDeviceFeatures)
getPhysicalDeviceFeatures = undefined {- {wrapped (pretty cName) :: Doc ()} -}
#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetPhysicalDeviceFormatProperties"
getPhysicalDeviceFormatProperties :: PhysicalDevice ->  Format ->  IO (FormatProperties)
getPhysicalDeviceFormatProperties = undefined {- {wrapped (pretty cName) :: Doc ()} -}
#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetPhysicalDeviceImageFormatProperties"
getPhysicalDeviceImageFormatProperties :: PhysicalDevice ->  Format ->  ImageType ->  ImageTiling ->  ImageUsageFlags ->  ImageCreateFlags ->  IO (ImageFormatProperties)
getPhysicalDeviceImageFormatProperties = undefined {- {wrapped (pretty cName) :: Doc ()} -}
#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetPhysicalDeviceMemoryProperties"
getPhysicalDeviceMemoryProperties :: PhysicalDevice ->  IO (PhysicalDeviceMemoryProperties)
getPhysicalDeviceMemoryProperties = undefined {- {wrapped (pretty cName) :: Doc ()} -}
#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetPhysicalDeviceProperties"
getPhysicalDeviceProperties :: PhysicalDevice ->  IO (PhysicalDeviceProperties)
getPhysicalDeviceProperties = undefined {- {wrapped (pretty cName) :: Doc ()} -}
#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetPhysicalDeviceQueueFamilyProperties"
getNumPhysicalDeviceQueueFamilyProperties :: PhysicalDevice ->  IO (Word32)
getNumPhysicalDeviceQueueFamilyProperties = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "vkGetPhysicalDeviceQueueFamilyProperties"
getPhysicalDeviceQueueFamilyProperties :: PhysicalDevice ->  Word32 ->  IO (Vector QueueFamilyProperties)
getPhysicalDeviceQueueFamilyProperties = undefined {- {wrapped (pretty cName) :: Doc ()} -}
-- | Returns all the values available from 'getPhysicalDeviceQueueFamilyProperties'.
getAllPhysicalDeviceQueueFamilyProperties :: PhysicalDevice ->  IO (Vector QueueFamilyProperties)
getAllPhysicalDeviceQueueFamilyProperties physicalDevice' =
  getNumPhysicalDeviceQueueFamilyProperties physicalDevice'
    >>= \num -> getPhysicalDeviceQueueFamilyProperties physicalDevice' num

#endif

-- | A safe wrapper for 'createInstance' and 'destroyInstance' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withInstance
  :: InstanceCreateInfo -> Maybe AllocationCallbacks -> (Instance -> IO a) -> IO a
withInstance instanceCreateInfo allocationCallbacks = bracket
  (createInstance instanceCreateInfo allocationCallbacks)
  (\o -> destroyInstance o allocationCallbacks)

-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_RESERVED_25_BIT_KHR"
pattern VK_FORMAT_FEATURE_RESERVED_25_BIT_KHR :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_RESERVED_25_BIT_KHR = VkFormatFeatureFlagBits 0x02000000

-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_RESERVED_26_BIT_KHR"
pattern VK_FORMAT_FEATURE_RESERVED_26_BIT_KHR :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_RESERVED_26_BIT_KHR = VkFormatFeatureFlagBits 0x04000000

-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_RESERVED_27_BIT_KHR"
pattern VK_FORMAT_FEATURE_RESERVED_27_BIT_KHR :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_RESERVED_27_BIT_KHR = VkFormatFeatureFlagBits 0x08000000

-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_RESERVED_28_BIT_KHR"
pattern VK_FORMAT_FEATURE_RESERVED_28_BIT_KHR :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_RESERVED_28_BIT_KHR = VkFormatFeatureFlagBits 0x10000000

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_RESERVED_10_BIT_KHR"
pattern VK_IMAGE_USAGE_RESERVED_10_BIT_KHR :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_RESERVED_10_BIT_KHR = VkImageUsageFlagBits 0x00000400

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_RESERVED_11_BIT_KHR"
pattern VK_IMAGE_USAGE_RESERVED_11_BIT_KHR :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_RESERVED_11_BIT_KHR = VkImageUsageFlagBits 0x00000800

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_RESERVED_12_BIT_KHR"
pattern VK_IMAGE_USAGE_RESERVED_12_BIT_KHR :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_RESERVED_12_BIT_KHR = VkImageUsageFlagBits 0x00001000

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_RESERVED_13_BIT_KHR"
pattern VK_IMAGE_USAGE_RESERVED_13_BIT_KHR :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_RESERVED_13_BIT_KHR = VkImageUsageFlagBits 0x00002000

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_RESERVED_14_BIT_KHR"
pattern VK_IMAGE_USAGE_RESERVED_14_BIT_KHR :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_RESERVED_14_BIT_KHR = VkImageUsageFlagBits 0x00004000

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_RESERVED_15_BIT_KHR"
pattern VK_IMAGE_USAGE_RESERVED_15_BIT_KHR :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_RESERVED_15_BIT_KHR = VkImageUsageFlagBits 0x00008000

-- No documentation found for Nested "VkQueueFlagBits" "VK_QUEUE_RESERVED_5_BIT_KHR"
pattern VK_QUEUE_RESERVED_5_BIT_KHR :: VkQueueFlagBits
pattern VK_QUEUE_RESERVED_5_BIT_KHR = VkQueueFlagBits 0x00000020

-- No documentation found for Nested "VkQueueFlagBits" "VK_QUEUE_RESERVED_6_BIT_KHR"
pattern VK_QUEUE_RESERVED_6_BIT_KHR :: VkQueueFlagBits
pattern VK_QUEUE_RESERVED_6_BIT_KHR = VkQueueFlagBits 0x00000040
