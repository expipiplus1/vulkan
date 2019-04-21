{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_host
  ( VkImportMemoryHostPointerInfoEXT
  , VkMemoryHostPointerPropertiesEXT
  , VkPhysicalDeviceExternalMemoryHostPropertiesEXT
  , FN_vkGetMemoryHostPointerPropertiesEXT
  , PFN_vkGetMemoryHostPointerPropertiesEXT
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
import {-# source #-} Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( VkExternalMemoryHandleTypeFlagBits
  )


data VkImportMemoryHostPointerInfoEXT

data VkMemoryHostPointerPropertiesEXT

data VkPhysicalDeviceExternalMemoryHostPropertiesEXT

type FN_vkGetMemoryHostPointerPropertiesEXT = ("device" ::: VkDevice) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagBits) -> ("pHostPointer" ::: Ptr ()) -> ("pMemoryHostPointerProperties" ::: Ptr VkMemoryHostPointerPropertiesEXT) -> IO VkResult
type PFN_vkGetMemoryHostPointerPropertiesEXT = FunPtr FN_vkGetMemoryHostPointerPropertiesEXT
