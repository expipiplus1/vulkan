{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.ExtensionDiscovery
  ( VkExtensionProperties
  , FN_vkEnumerateDeviceExtensionProperties
  , PFN_vkEnumerateDeviceExtensionProperties
  , FN_vkEnumerateInstanceExtensionProperties
  , PFN_vkEnumerateInstanceExtensionProperties
  ) where

import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CChar(..)
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
  ( VkPhysicalDevice
  )


data VkExtensionProperties

type FN_vkEnumerateDeviceExtensionProperties = ("physicalDevice" ::: VkPhysicalDevice) -> ("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkExtensionProperties) -> IO VkResult
type PFN_vkEnumerateDeviceExtensionProperties = FunPtr FN_vkEnumerateDeviceExtensionProperties

type FN_vkEnumerateInstanceExtensionProperties = ("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkExtensionProperties) -> IO VkResult
type PFN_vkEnumerateInstanceExtensionProperties = FunPtr FN_vkEnumerateInstanceExtensionProperties
