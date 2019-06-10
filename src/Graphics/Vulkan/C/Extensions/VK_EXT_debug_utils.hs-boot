{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils
  ( PFN_vkDebugUtilsMessengerCallbackEXT
  , VkDebugUtilsLabelEXT
  , VkDebugUtilsMessageSeverityFlagBitsEXT
  , VkDebugUtilsMessageSeverityFlagsEXT
  , VkDebugUtilsMessageTypeFlagBitsEXT
  , VkDebugUtilsMessageTypeFlagsEXT
  , VkDebugUtilsMessengerCallbackDataEXT
  , VkDebugUtilsMessengerCallbackDataFlagsEXT
  , VkDebugUtilsMessengerCreateFlagsEXT
  , VkDebugUtilsMessengerCreateInfoEXT
  , VkDebugUtilsMessengerEXT
  , VkDebugUtilsObjectNameInfoEXT
  , VkDebugUtilsObjectTagInfoEXT
  , FN_vkCmdBeginDebugUtilsLabelEXT
  , PFN_vkCmdBeginDebugUtilsLabelEXT
  , FN_vkCmdEndDebugUtilsLabelEXT
  , PFN_vkCmdEndDebugUtilsLabelEXT
  , FN_vkCmdInsertDebugUtilsLabelEXT
  , PFN_vkCmdInsertDebugUtilsLabelEXT
  , FN_vkCreateDebugUtilsMessengerEXT
  , PFN_vkCreateDebugUtilsMessengerEXT
  , FN_vkDestroyDebugUtilsMessengerEXT
  , PFN_vkDestroyDebugUtilsMessengerEXT
  , FN_vkQueueBeginDebugUtilsLabelEXT
  , PFN_vkQueueBeginDebugUtilsLabelEXT
  , FN_vkQueueEndDebugUtilsLabelEXT
  , PFN_vkQueueEndDebugUtilsLabelEXT
  , FN_vkQueueInsertDebugUtilsLabelEXT
  , PFN_vkQueueInsertDebugUtilsLabelEXT
  , FN_vkSetDebugUtilsObjectNameEXT
  , PFN_vkSetDebugUtilsObjectNameEXT
  , FN_vkSetDebugUtilsObjectTagEXT
  , PFN_vkSetDebugUtilsObjectTagEXT
  , FN_vkSubmitDebugUtilsMessageEXT
  , PFN_vkSubmitDebugUtilsMessageEXT
  ) where

import Foreign.Ptr
  ( FunPtr
  , Ptr
  )


import Graphics.Vulkan.NamedType
  ( (:::)
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Core
  ( VkBool32
  , VkResult
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks
  , VkDevice
  , VkInstance
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Queue
  ( VkCommandBuffer
  , VkQueue
  )


-- No documentation found for TopLevel "PFN_vkDebugUtilsMessengerCallbackEXT"
type PFN_vkDebugUtilsMessengerCallbackEXT = Ptr (("messageSeverity" ::: VkDebugUtilsMessageSeverityFlagBitsEXT) -> ("messageTypes" ::: VkDebugUtilsMessageTypeFlagsEXT) -> ("pCallbackData" ::: Ptr VkDebugUtilsMessengerCallbackDataEXT) -> ("pUserData" ::: Ptr ()) -> IO VkBool32)

data VkDebugUtilsLabelEXT

data VkDebugUtilsMessageSeverityFlagBitsEXT

-- No documentation found for TopLevel "VkDebugUtilsMessageSeverityFlagsEXT"
type VkDebugUtilsMessageSeverityFlagsEXT = VkDebugUtilsMessageSeverityFlagBitsEXT

data VkDebugUtilsMessageTypeFlagBitsEXT

-- No documentation found for TopLevel "VkDebugUtilsMessageTypeFlagsEXT"
type VkDebugUtilsMessageTypeFlagsEXT = VkDebugUtilsMessageTypeFlagBitsEXT

data VkDebugUtilsMessengerCallbackDataEXT

data VkDebugUtilsMessengerCallbackDataFlagsEXT

data VkDebugUtilsMessengerCreateFlagsEXT

data VkDebugUtilsMessengerCreateInfoEXT

-- | Dummy data to tag the 'Ptr' with
data VkDebugUtilsMessengerEXT_T
-- No documentation found for TopLevel "VkDebugUtilsMessengerEXT"
type VkDebugUtilsMessengerEXT = Ptr VkDebugUtilsMessengerEXT_T

data VkDebugUtilsObjectNameInfoEXT

data VkDebugUtilsObjectTagInfoEXT

type FN_vkCmdBeginDebugUtilsLabelEXT = ("commandBuffer" ::: VkCommandBuffer) -> ("pLabelInfo" ::: Ptr VkDebugUtilsLabelEXT) -> IO ()
type PFN_vkCmdBeginDebugUtilsLabelEXT = FunPtr FN_vkCmdBeginDebugUtilsLabelEXT

type FN_vkCmdEndDebugUtilsLabelEXT = ("commandBuffer" ::: VkCommandBuffer) -> IO ()
type PFN_vkCmdEndDebugUtilsLabelEXT = FunPtr FN_vkCmdEndDebugUtilsLabelEXT

type FN_vkCmdInsertDebugUtilsLabelEXT = ("commandBuffer" ::: VkCommandBuffer) -> ("pLabelInfo" ::: Ptr VkDebugUtilsLabelEXT) -> IO ()
type PFN_vkCmdInsertDebugUtilsLabelEXT = FunPtr FN_vkCmdInsertDebugUtilsLabelEXT

type FN_vkCreateDebugUtilsMessengerEXT = ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkDebugUtilsMessengerCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMessenger" ::: Ptr VkDebugUtilsMessengerEXT) -> IO VkResult
type PFN_vkCreateDebugUtilsMessengerEXT = FunPtr FN_vkCreateDebugUtilsMessengerEXT

type FN_vkDestroyDebugUtilsMessengerEXT = ("instance" ::: VkInstance) -> ("messenger" ::: VkDebugUtilsMessengerEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyDebugUtilsMessengerEXT = FunPtr FN_vkDestroyDebugUtilsMessengerEXT

type FN_vkQueueBeginDebugUtilsLabelEXT = ("queue" ::: VkQueue) -> ("pLabelInfo" ::: Ptr VkDebugUtilsLabelEXT) -> IO ()
type PFN_vkQueueBeginDebugUtilsLabelEXT = FunPtr FN_vkQueueBeginDebugUtilsLabelEXT

type FN_vkQueueEndDebugUtilsLabelEXT = ("queue" ::: VkQueue) -> IO ()
type PFN_vkQueueEndDebugUtilsLabelEXT = FunPtr FN_vkQueueEndDebugUtilsLabelEXT

type FN_vkQueueInsertDebugUtilsLabelEXT = ("queue" ::: VkQueue) -> ("pLabelInfo" ::: Ptr VkDebugUtilsLabelEXT) -> IO ()
type PFN_vkQueueInsertDebugUtilsLabelEXT = FunPtr FN_vkQueueInsertDebugUtilsLabelEXT

type FN_vkSetDebugUtilsObjectNameEXT = ("device" ::: VkDevice) -> ("pNameInfo" ::: Ptr VkDebugUtilsObjectNameInfoEXT) -> IO VkResult
type PFN_vkSetDebugUtilsObjectNameEXT = FunPtr FN_vkSetDebugUtilsObjectNameEXT

type FN_vkSetDebugUtilsObjectTagEXT = ("device" ::: VkDevice) -> ("pTagInfo" ::: Ptr VkDebugUtilsObjectTagInfoEXT) -> IO VkResult
type PFN_vkSetDebugUtilsObjectTagEXT = FunPtr FN_vkSetDebugUtilsObjectTagEXT

type FN_vkSubmitDebugUtilsMessageEXT = ("instance" ::: VkInstance) -> ("messageSeverity" ::: VkDebugUtilsMessageSeverityFlagBitsEXT) -> ("messageTypes" ::: VkDebugUtilsMessageTypeFlagsEXT) -> ("pCallbackData" ::: Ptr VkDebugUtilsMessengerCallbackDataEXT) -> IO ()
type PFN_vkSubmitDebugUtilsMessageEXT = FunPtr FN_vkSubmitDebugUtilsMessageEXT
