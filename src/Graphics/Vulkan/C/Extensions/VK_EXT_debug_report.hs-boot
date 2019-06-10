{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_debug_report
  ( PFN_vkDebugReportCallbackEXT
  , VkDebugReportCallbackCreateInfoEXT
  , VkDebugReportCallbackEXT
  , VkDebugReportFlagBitsEXT
  , VkDebugReportFlagsEXT
  , VkDebugReportObjectTypeEXT
  , FN_vkCreateDebugReportCallbackEXT
  , PFN_vkCreateDebugReportCallbackEXT
  , FN_vkDebugReportMessageEXT
  , PFN_vkDebugReportMessageEXT
  , FN_vkDestroyDebugReportCallbackEXT
  , PFN_vkDestroyDebugReportCallbackEXT
  ) where

import Data.Int
  ( Int32
  )
import Data.Word
  ( Word64
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
  ( VkBool32
  , VkResult
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks
  , VkInstance
  )


-- No documentation found for TopLevel "PFN_vkDebugReportCallbackEXT"
type PFN_vkDebugReportCallbackEXT = Ptr (("flags" ::: VkDebugReportFlagsEXT) -> ("objectType" ::: VkDebugReportObjectTypeEXT) -> ("object" ::: Word64) -> ("location" ::: CSize) -> ("messageCode" ::: Int32) -> ("pLayerPrefix" ::: Ptr CChar) -> ("pMessage" ::: Ptr CChar) -> ("pUserData" ::: Ptr ()) -> IO VkBool32)

data VkDebugReportCallbackCreateInfoEXT

-- | Dummy data to tag the 'Ptr' with
data VkDebugReportCallbackEXT_T
-- No documentation found for TopLevel "VkDebugReportCallbackEXT"
type VkDebugReportCallbackEXT = Ptr VkDebugReportCallbackEXT_T

data VkDebugReportFlagBitsEXT

-- No documentation found for TopLevel "VkDebugReportFlagsEXT"
type VkDebugReportFlagsEXT = VkDebugReportFlagBitsEXT

data VkDebugReportObjectTypeEXT

type FN_vkCreateDebugReportCallbackEXT = ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkDebugReportCallbackCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pCallback" ::: Ptr VkDebugReportCallbackEXT) -> IO VkResult
type PFN_vkCreateDebugReportCallbackEXT = FunPtr FN_vkCreateDebugReportCallbackEXT

type FN_vkDebugReportMessageEXT = ("instance" ::: VkInstance) -> ("flags" ::: VkDebugReportFlagsEXT) -> ("objectType" ::: VkDebugReportObjectTypeEXT) -> ("object" ::: Word64) -> ("location" ::: CSize) -> ("messageCode" ::: Int32) -> ("pLayerPrefix" ::: Ptr CChar) -> ("pMessage" ::: Ptr CChar) -> IO ()
type PFN_vkDebugReportMessageEXT = FunPtr FN_vkDebugReportMessageEXT

type FN_vkDestroyDebugReportCallbackEXT = ("instance" ::: VkInstance) -> ("callback" ::: VkDebugReportCallbackEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyDebugReportCallbackEXT = FunPtr FN_vkDestroyDebugReportCallbackEXT
