{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_host_query_reset
  ( VkPhysicalDeviceHostQueryResetFeaturesEXT
  , FN_vkResetQueryPoolEXT
  , PFN_vkResetQueryPoolEXT
  ) where

import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  )


import Graphics.Vulkan.NamedType
  ( (:::)
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Query
  ( VkQueryPool
  )


data VkPhysicalDeviceHostQueryResetFeaturesEXT

type FN_vkResetQueryPoolEXT = ("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> IO ()
type PFN_vkResetQueryPoolEXT = FunPtr FN_vkResetQueryPoolEXT
