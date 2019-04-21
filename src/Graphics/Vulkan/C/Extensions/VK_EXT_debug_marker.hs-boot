{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_debug_marker
  ( VkDebugMarkerMarkerInfoEXT
  , VkDebugMarkerObjectNameInfoEXT
  , VkDebugMarkerObjectTagInfoEXT
  , FN_vkCmdDebugMarkerBeginEXT
  , PFN_vkCmdDebugMarkerBeginEXT
  , FN_vkCmdDebugMarkerEndEXT
  , PFN_vkCmdDebugMarkerEndEXT
  , FN_vkCmdDebugMarkerInsertEXT
  , PFN_vkCmdDebugMarkerInsertEXT
  , FN_vkDebugMarkerSetObjectNameEXT
  , PFN_vkDebugMarkerSetObjectNameEXT
  , FN_vkDebugMarkerSetObjectTagEXT
  , PFN_vkDebugMarkerSetObjectTagEXT
  ) where

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
  ( VkDevice
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Queue
  ( VkCommandBuffer
  )


data VkDebugMarkerMarkerInfoEXT

data VkDebugMarkerObjectNameInfoEXT

data VkDebugMarkerObjectTagInfoEXT

type FN_vkCmdDebugMarkerBeginEXT = ("commandBuffer" ::: VkCommandBuffer) -> ("pMarkerInfo" ::: Ptr VkDebugMarkerMarkerInfoEXT) -> IO ()
type PFN_vkCmdDebugMarkerBeginEXT = FunPtr FN_vkCmdDebugMarkerBeginEXT

type FN_vkCmdDebugMarkerEndEXT = ("commandBuffer" ::: VkCommandBuffer) -> IO ()
type PFN_vkCmdDebugMarkerEndEXT = FunPtr FN_vkCmdDebugMarkerEndEXT

type FN_vkCmdDebugMarkerInsertEXT = ("commandBuffer" ::: VkCommandBuffer) -> ("pMarkerInfo" ::: Ptr VkDebugMarkerMarkerInfoEXT) -> IO ()
type PFN_vkCmdDebugMarkerInsertEXT = FunPtr FN_vkCmdDebugMarkerInsertEXT

type FN_vkDebugMarkerSetObjectNameEXT = ("device" ::: VkDevice) -> ("pNameInfo" ::: Ptr VkDebugMarkerObjectNameInfoEXT) -> IO VkResult
type PFN_vkDebugMarkerSetObjectNameEXT = FunPtr FN_vkDebugMarkerSetObjectNameEXT

type FN_vkDebugMarkerSetObjectTagEXT = ("device" ::: VkDevice) -> ("pTagInfo" ::: Ptr VkDebugMarkerObjectTagInfoEXT) -> IO VkResult
type PFN_vkDebugMarkerSetObjectTagEXT = FunPtr FN_vkDebugMarkerSetObjectTagEXT
