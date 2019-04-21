{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer
  ( AHardwareBuffer
  , VkAndroidHardwareBufferFormatPropertiesANDROID
  , VkAndroidHardwareBufferPropertiesANDROID
  , VkAndroidHardwareBufferUsageANDROID
  , VkExternalFormatANDROID
  , VkImportAndroidHardwareBufferInfoANDROID
  , VkMemoryGetAndroidHardwareBufferInfoANDROID
  , FN_vkGetAndroidHardwareBufferPropertiesANDROID
  , PFN_vkGetAndroidHardwareBufferPropertiesANDROID
  , FN_vkGetMemoryAndroidHardwareBufferANDROID
  , PFN_vkGetMemoryAndroidHardwareBufferANDROID
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


-- | Opaque data
data AHardwareBuffer

data VkAndroidHardwareBufferFormatPropertiesANDROID

data VkAndroidHardwareBufferPropertiesANDROID

data VkAndroidHardwareBufferUsageANDROID

data VkExternalFormatANDROID

data VkImportAndroidHardwareBufferInfoANDROID

data VkMemoryGetAndroidHardwareBufferInfoANDROID

type FN_vkGetAndroidHardwareBufferPropertiesANDROID = ("device" ::: VkDevice) -> ("buffer" ::: Ptr AHardwareBuffer) -> ("pProperties" ::: Ptr VkAndroidHardwareBufferPropertiesANDROID) -> IO VkResult
type PFN_vkGetAndroidHardwareBufferPropertiesANDROID = FunPtr FN_vkGetAndroidHardwareBufferPropertiesANDROID

type FN_vkGetMemoryAndroidHardwareBufferANDROID = ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkMemoryGetAndroidHardwareBufferInfoANDROID) -> ("pBuffer" ::: Ptr (Ptr AHardwareBuffer)) -> IO VkResult
type PFN_vkGetMemoryAndroidHardwareBufferANDROID = FunPtr FN_vkGetMemoryAndroidHardwareBufferANDROID
