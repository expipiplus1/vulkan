{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_NVX_image_view_handle
  ( VkImageViewHandleInfoNVX
  , FN_vkGetImageViewHandleNVX
  , PFN_vkGetImageViewHandleNVX
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


data VkImageViewHandleInfoNVX

type FN_vkGetImageViewHandleNVX = ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageViewHandleInfoNVX) -> IO Word32
type PFN_vkGetImageViewHandleNVX = FunPtr FN_vkGetImageViewHandleNVX
