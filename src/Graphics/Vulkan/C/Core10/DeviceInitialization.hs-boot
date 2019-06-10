{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.DeviceInitialization
  ( PFN_vkAllocationFunction
  , PFN_vkFreeFunction
  , PFN_vkInternalAllocationNotification
  , PFN_vkInternalFreeNotification
  , PFN_vkReallocationFunction
  , PFN_vkVoidFunction
  , VkAllocationCallbacks
  , VkApplicationInfo
  , VkDevice
  , VkDeviceSize
  , VkExtent3D
  , VkFormatFeatureFlagBits
  , VkFormatFeatureFlags
  , VkFormatProperties
  , VkImageCreateFlagBits
  , VkImageCreateFlags
  , VkImageFormatProperties
  , VkImageTiling
  , VkImageType
  , VkImageUsageFlagBits
  , VkImageUsageFlags
  , VkInstance
  , VkInstanceCreateFlags
  , VkInstanceCreateInfo
  , VkInternalAllocationType
  , VkMemoryHeap
  , VkMemoryHeapFlagBits
  , VkMemoryHeapFlags
  , VkMemoryPropertyFlagBits
  , VkMemoryPropertyFlags
  , VkMemoryType
  , VkPhysicalDevice
  , VkPhysicalDeviceFeatures
  , VkPhysicalDeviceLimits
  , VkPhysicalDeviceMemoryProperties
  , VkPhysicalDeviceProperties
  , VkPhysicalDeviceSparseProperties
  , VkPhysicalDeviceType
  , VkQueueFamilyProperties
  , VkQueueFlagBits
  , VkQueueFlags
  , VkSampleCountFlagBits
  , VkSampleCountFlags
  , VkSystemAllocationScope
  , FN_vkCreateInstance
  , PFN_vkCreateInstance
  , FN_vkDestroyInstance
  , PFN_vkDestroyInstance
  , FN_vkEnumeratePhysicalDevices
  , PFN_vkEnumeratePhysicalDevices
  , FN_vkGetDeviceProcAddr
  , PFN_vkGetDeviceProcAddr
  , FN_vkGetInstanceProcAddr
  , PFN_vkGetInstanceProcAddr
  , vkGetInstanceProcAddr
  , FN_vkGetPhysicalDeviceFeatures
  , PFN_vkGetPhysicalDeviceFeatures
  , FN_vkGetPhysicalDeviceFormatProperties
  , PFN_vkGetPhysicalDeviceFormatProperties
  , FN_vkGetPhysicalDeviceImageFormatProperties
  , PFN_vkGetPhysicalDeviceImageFormatProperties
  , FN_vkGetPhysicalDeviceMemoryProperties
  , PFN_vkGetPhysicalDeviceMemoryProperties
  , FN_vkGetPhysicalDeviceProperties
  , PFN_vkGetPhysicalDeviceProperties
  , FN_vkGetPhysicalDeviceQueueFamilyProperties
  , PFN_vkGetPhysicalDeviceQueueFamilyProperties
  ) where

import Data.Word
  ( Word32
  , Word64
  )
import Foreign.C.Types
  ( CChar(..)
  , CSize(..)
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  )


import Graphics.Vulkan.NamedType
  ( (:::)
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Core
  ( VkFormat
  , VkResult
  )
import {-# source #-} Graphics.Vulkan.C.Dynamic
  ( InstanceCmds(..)
  )


-- No documentation found for TopLevel "PFN_vkAllocationFunction"
type PFN_vkAllocationFunction = Ptr (("pUserData" ::: Ptr ()) -> ("size" ::: CSize) -> ("alignment" ::: CSize) -> ("allocationScope" ::: VkSystemAllocationScope) -> IO (Ptr ()))

-- No documentation found for TopLevel "PFN_vkFreeFunction"
type PFN_vkFreeFunction = Ptr (("pUserData" ::: Ptr ()) -> ("pMemory" ::: Ptr ()) -> IO ())

-- No documentation found for TopLevel "PFN_vkInternalAllocationNotification"
type PFN_vkInternalAllocationNotification = Ptr (("pUserData" ::: Ptr ()) -> ("size" ::: CSize) -> ("allocationType" ::: VkInternalAllocationType) -> ("allocationScope" ::: VkSystemAllocationScope) -> IO ())

-- No documentation found for TopLevel "PFN_vkInternalFreeNotification"
type PFN_vkInternalFreeNotification = Ptr (("pUserData" ::: Ptr ()) -> ("size" ::: CSize) -> ("allocationType" ::: VkInternalAllocationType) -> ("allocationScope" ::: VkSystemAllocationScope) -> IO ())

-- No documentation found for TopLevel "PFN_vkReallocationFunction"
type PFN_vkReallocationFunction = Ptr (("pUserData" ::: Ptr ()) -> ("pOriginal" ::: Ptr ()) -> ("size" ::: CSize) -> ("alignment" ::: CSize) -> ("allocationScope" ::: VkSystemAllocationScope) -> IO (Ptr ()))

-- No documentation found for TopLevel "PFN_vkVoidFunction"
type PFN_vkVoidFunction = Ptr (() -> IO ())

data VkAllocationCallbacks

data VkApplicationInfo

-- | Dummy data to tag the 'Ptr' with
data VkDevice_T
-- No documentation found for TopLevel "VkDevice"
type VkDevice = Ptr VkDevice_T

-- No documentation found for TopLevel "VkDeviceSize"
type VkDeviceSize = Word64

data VkExtent3D

data VkFormatFeatureFlagBits

-- No documentation found for TopLevel "VkFormatFeatureFlags"
type VkFormatFeatureFlags = VkFormatFeatureFlagBits

data VkFormatProperties

data VkImageCreateFlagBits

-- No documentation found for TopLevel "VkImageCreateFlags"
type VkImageCreateFlags = VkImageCreateFlagBits

data VkImageFormatProperties

data VkImageTiling

data VkImageType

data VkImageUsageFlagBits

-- No documentation found for TopLevel "VkImageUsageFlags"
type VkImageUsageFlags = VkImageUsageFlagBits

-- | Dummy data to tag the 'Ptr' with
data VkInstance_T
-- No documentation found for TopLevel "VkInstance"
type VkInstance = Ptr VkInstance_T

data VkInstanceCreateFlags

data VkInstanceCreateInfo

data VkInternalAllocationType

data VkMemoryHeap

data VkMemoryHeapFlagBits

-- No documentation found for TopLevel "VkMemoryHeapFlags"
type VkMemoryHeapFlags = VkMemoryHeapFlagBits

data VkMemoryPropertyFlagBits

-- No documentation found for TopLevel "VkMemoryPropertyFlags"
type VkMemoryPropertyFlags = VkMemoryPropertyFlagBits

data VkMemoryType

-- | Dummy data to tag the 'Ptr' with
data VkPhysicalDevice_T
-- No documentation found for TopLevel "VkPhysicalDevice"
type VkPhysicalDevice = Ptr VkPhysicalDevice_T

data VkPhysicalDeviceFeatures

data VkPhysicalDeviceLimits

data VkPhysicalDeviceMemoryProperties

data VkPhysicalDeviceProperties

data VkPhysicalDeviceSparseProperties

data VkPhysicalDeviceType

data VkQueueFamilyProperties

data VkQueueFlagBits

-- No documentation found for TopLevel "VkQueueFlags"
type VkQueueFlags = VkQueueFlagBits

data VkSampleCountFlagBits

-- No documentation found for TopLevel "VkSampleCountFlags"
type VkSampleCountFlags = VkSampleCountFlagBits

data VkSystemAllocationScope

type FN_vkCreateInstance = ("pCreateInfo" ::: Ptr VkInstanceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pInstance" ::: Ptr VkInstance) -> IO VkResult
type PFN_vkCreateInstance = FunPtr FN_vkCreateInstance

type FN_vkDestroyInstance = ("instance" ::: VkInstance) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyInstance = FunPtr FN_vkDestroyInstance

type FN_vkEnumeratePhysicalDevices = ("instance" ::: VkInstance) -> ("pPhysicalDeviceCount" ::: Ptr Word32) -> ("pPhysicalDevices" ::: Ptr VkPhysicalDevice) -> IO VkResult
type PFN_vkEnumeratePhysicalDevices = FunPtr FN_vkEnumeratePhysicalDevices

type FN_vkGetDeviceProcAddr = ("device" ::: VkDevice) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction
type PFN_vkGetDeviceProcAddr = FunPtr FN_vkGetDeviceProcAddr

#if defined(EXPOSE_CORE10_COMMANDS)
vkGetInstanceProcAddr :: (("instance" ::: VkInstance) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction)
#else
vkGetInstanceProcAddr :: InstanceCmds -> (("instance" ::: VkInstance) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction)
#endif
type FN_vkGetInstanceProcAddr = ("instance" ::: VkInstance) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction
type PFN_vkGetInstanceProcAddr = FunPtr FN_vkGetInstanceProcAddr

type FN_vkGetPhysicalDeviceFeatures = ("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkPhysicalDeviceFeatures) -> IO ()
type PFN_vkGetPhysicalDeviceFeatures = FunPtr FN_vkGetPhysicalDeviceFeatures

type FN_vkGetPhysicalDeviceFormatProperties = ("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("pFormatProperties" ::: Ptr VkFormatProperties) -> IO ()
type PFN_vkGetPhysicalDeviceFormatProperties = FunPtr FN_vkGetPhysicalDeviceFormatProperties

type FN_vkGetPhysicalDeviceImageFormatProperties = ("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("tiling" ::: VkImageTiling) -> ("usage" ::: VkImageUsageFlags) -> ("flags" ::: VkImageCreateFlags) -> ("pImageFormatProperties" ::: Ptr VkImageFormatProperties) -> IO VkResult
type PFN_vkGetPhysicalDeviceImageFormatProperties = FunPtr FN_vkGetPhysicalDeviceImageFormatProperties

type FN_vkGetPhysicalDeviceMemoryProperties = ("physicalDevice" ::: VkPhysicalDevice) -> ("pMemoryProperties" ::: Ptr VkPhysicalDeviceMemoryProperties) -> IO ()
type PFN_vkGetPhysicalDeviceMemoryProperties = FunPtr FN_vkGetPhysicalDeviceMemoryProperties

type FN_vkGetPhysicalDeviceProperties = ("physicalDevice" ::: VkPhysicalDevice) -> ("pProperties" ::: Ptr VkPhysicalDeviceProperties) -> IO ()
type PFN_vkGetPhysicalDeviceProperties = FunPtr FN_vkGetPhysicalDeviceProperties

type FN_vkGetPhysicalDeviceQueueFamilyProperties = ("physicalDevice" ::: VkPhysicalDevice) -> ("pQueueFamilyPropertyCount" ::: Ptr Word32) -> ("pQueueFamilyProperties" ::: Ptr VkQueueFamilyProperties) -> IO ()
type PFN_vkGetPhysicalDeviceQueueFamilyProperties = FunPtr FN_vkGetPhysicalDeviceQueueFamilyProperties
