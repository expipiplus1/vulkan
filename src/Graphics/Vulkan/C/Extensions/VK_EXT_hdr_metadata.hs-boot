{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_hdr_metadata
  ( VkHdrMetadataEXT
  , VkXYColorEXT
  , FN_vkSetHdrMetadataEXT
  , PFN_vkSetHdrMetadataEXT
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
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_swapchain
  ( VkSwapchainKHR
  )


data VkHdrMetadataEXT

data VkXYColorEXT

type FN_vkSetHdrMetadataEXT = ("device" ::: VkDevice) -> ("swapchainCount" ::: Word32) -> ("pSwapchains" ::: Ptr VkSwapchainKHR) -> ("pMetadata" ::: Ptr VkHdrMetadataEXT) -> IO ()
type PFN_vkSetHdrMetadataEXT = FunPtr FN_vkSetHdrMetadataEXT
