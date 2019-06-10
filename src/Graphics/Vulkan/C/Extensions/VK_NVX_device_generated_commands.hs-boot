{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands
  ( VkCmdProcessCommandsInfoNVX
  , VkCmdReserveSpaceForCommandsInfoNVX
  , VkDeviceGeneratedCommandsFeaturesNVX
  , VkDeviceGeneratedCommandsLimitsNVX
  , VkIndirectCommandsLayoutCreateInfoNVX
  , VkIndirectCommandsLayoutNVX
  , VkIndirectCommandsLayoutTokenNVX
  , VkIndirectCommandsLayoutUsageFlagBitsNVX
  , VkIndirectCommandsLayoutUsageFlagsNVX
  , VkIndirectCommandsTokenNVX
  , VkIndirectCommandsTokenTypeNVX
  , VkObjectEntryTypeNVX
  , VkObjectEntryUsageFlagBitsNVX
  , VkObjectEntryUsageFlagsNVX
  , VkObjectTableCreateInfoNVX
  , VkObjectTableDescriptorSetEntryNVX
  , VkObjectTableEntryNVX
  , VkObjectTableIndexBufferEntryNVX
  , VkObjectTableNVX
  , VkObjectTablePipelineEntryNVX
  , VkObjectTablePushConstantEntryNVX
  , VkObjectTableVertexBufferEntryNVX
  , FN_vkCmdProcessCommandsNVX
  , PFN_vkCmdProcessCommandsNVX
  , FN_vkCmdReserveSpaceForCommandsNVX
  , PFN_vkCmdReserveSpaceForCommandsNVX
  , FN_vkCreateIndirectCommandsLayoutNVX
  , PFN_vkCreateIndirectCommandsLayoutNVX
  , FN_vkCreateObjectTableNVX
  , PFN_vkCreateObjectTableNVX
  , FN_vkDestroyIndirectCommandsLayoutNVX
  , PFN_vkDestroyIndirectCommandsLayoutNVX
  , FN_vkDestroyObjectTableNVX
  , PFN_vkDestroyObjectTableNVX
  , FN_vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX
  , PFN_vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX
  , FN_vkRegisterObjectsNVX
  , PFN_vkRegisterObjectsNVX
  , FN_vkUnregisterObjectsNVX
  , PFN_vkUnregisterObjectsNVX
  ) where

import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  )


import Graphics.Vulkan.NamedType
  ( (:::)
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Core
  ( VkResult
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks
  , VkDevice
  , VkPhysicalDevice
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Queue
  ( VkCommandBuffer
  )


data VkCmdProcessCommandsInfoNVX

data VkCmdReserveSpaceForCommandsInfoNVX

data VkDeviceGeneratedCommandsFeaturesNVX

data VkDeviceGeneratedCommandsLimitsNVX

data VkIndirectCommandsLayoutCreateInfoNVX

-- | Dummy data to tag the 'Ptr' with
data VkIndirectCommandsLayoutNVX_T
-- No documentation found for TopLevel "VkIndirectCommandsLayoutNVX"
type VkIndirectCommandsLayoutNVX = Ptr VkIndirectCommandsLayoutNVX_T

data VkIndirectCommandsLayoutTokenNVX

data VkIndirectCommandsLayoutUsageFlagBitsNVX

-- No documentation found for TopLevel "VkIndirectCommandsLayoutUsageFlagsNVX"
type VkIndirectCommandsLayoutUsageFlagsNVX = VkIndirectCommandsLayoutUsageFlagBitsNVX

data VkIndirectCommandsTokenNVX

data VkIndirectCommandsTokenTypeNVX

data VkObjectEntryTypeNVX

data VkObjectEntryUsageFlagBitsNVX

-- No documentation found for TopLevel "VkObjectEntryUsageFlagsNVX"
type VkObjectEntryUsageFlagsNVX = VkObjectEntryUsageFlagBitsNVX

data VkObjectTableCreateInfoNVX

data VkObjectTableDescriptorSetEntryNVX

data VkObjectTableEntryNVX

data VkObjectTableIndexBufferEntryNVX

-- | Dummy data to tag the 'Ptr' with
data VkObjectTableNVX_T
-- No documentation found for TopLevel "VkObjectTableNVX"
type VkObjectTableNVX = Ptr VkObjectTableNVX_T

data VkObjectTablePipelineEntryNVX

data VkObjectTablePushConstantEntryNVX

data VkObjectTableVertexBufferEntryNVX

type FN_vkCmdProcessCommandsNVX = ("commandBuffer" ::: VkCommandBuffer) -> ("pProcessCommandsInfo" ::: Ptr VkCmdProcessCommandsInfoNVX) -> IO ()
type PFN_vkCmdProcessCommandsNVX = FunPtr FN_vkCmdProcessCommandsNVX

type FN_vkCmdReserveSpaceForCommandsNVX = ("commandBuffer" ::: VkCommandBuffer) -> ("pReserveSpaceInfo" ::: Ptr VkCmdReserveSpaceForCommandsInfoNVX) -> IO ()
type PFN_vkCmdReserveSpaceForCommandsNVX = FunPtr FN_vkCmdReserveSpaceForCommandsNVX

type FN_vkCreateIndirectCommandsLayoutNVX = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkIndirectCommandsLayoutCreateInfoNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pIndirectCommandsLayout" ::: Ptr VkIndirectCommandsLayoutNVX) -> IO VkResult
type PFN_vkCreateIndirectCommandsLayoutNVX = FunPtr FN_vkCreateIndirectCommandsLayoutNVX

type FN_vkCreateObjectTableNVX = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkObjectTableCreateInfoNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pObjectTable" ::: Ptr VkObjectTableNVX) -> IO VkResult
type PFN_vkCreateObjectTableNVX = FunPtr FN_vkCreateObjectTableNVX

type FN_vkDestroyIndirectCommandsLayoutNVX = ("device" ::: VkDevice) -> ("indirectCommandsLayout" ::: VkIndirectCommandsLayoutNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyIndirectCommandsLayoutNVX = FunPtr FN_vkDestroyIndirectCommandsLayoutNVX

type FN_vkDestroyObjectTableNVX = ("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyObjectTableNVX = FunPtr FN_vkDestroyObjectTableNVX

type FN_vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX = ("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkDeviceGeneratedCommandsFeaturesNVX) -> ("pLimits" ::: Ptr VkDeviceGeneratedCommandsLimitsNVX) -> IO ()
type PFN_vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX = FunPtr FN_vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX

type FN_vkRegisterObjectsNVX = ("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("objectCount" ::: Word32) -> ("ppObjectTableEntries" ::: Ptr (Ptr VkObjectTableEntryNVX)) -> ("pObjectIndices" ::: Ptr Word32) -> IO VkResult
type PFN_vkRegisterObjectsNVX = FunPtr FN_vkRegisterObjectsNVX

type FN_vkUnregisterObjectsNVX = ("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("objectCount" ::: Word32) -> ("pObjectEntryTypes" ::: Ptr VkObjectEntryTypeNVX) -> ("pObjectIndices" ::: Ptr Word32) -> IO VkResult
type PFN_vkUnregisterObjectsNVX = FunPtr FN_vkUnregisterObjectsNVX
