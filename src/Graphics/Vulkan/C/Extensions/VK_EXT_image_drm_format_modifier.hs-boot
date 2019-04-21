{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier
  ( VkDrmFormatModifierPropertiesEXT
  , VkDrmFormatModifierPropertiesListEXT
  , VkImageDrmFormatModifierExplicitCreateInfoEXT
  , VkImageDrmFormatModifierListCreateInfoEXT
  , VkImageDrmFormatModifierPropertiesEXT
  , VkPhysicalDeviceImageDrmFormatModifierInfoEXT
  , FN_vkGetImageDrmFormatModifierPropertiesEXT
  , PFN_vkGetImageDrmFormatModifierPropertiesEXT
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
import {-# source #-} Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkImage
  )


data VkDrmFormatModifierPropertiesEXT

data VkDrmFormatModifierPropertiesListEXT

data VkImageDrmFormatModifierExplicitCreateInfoEXT

data VkImageDrmFormatModifierListCreateInfoEXT

data VkImageDrmFormatModifierPropertiesEXT

data VkPhysicalDeviceImageDrmFormatModifierInfoEXT

type FN_vkGetImageDrmFormatModifierPropertiesEXT = ("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pProperties" ::: Ptr VkImageDrmFormatModifierPropertiesEXT) -> IO VkResult
type PFN_vkGetImageDrmFormatModifierPropertiesEXT = FunPtr FN_vkGetImageDrmFormatModifierPropertiesEXT
