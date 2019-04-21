{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.LayerDiscovery
  ( VkLayerProperties
  , FN_vkEnumerateDeviceLayerProperties
  , PFN_vkEnumerateDeviceLayerProperties
  , FN_vkEnumerateInstanceLayerProperties
  , PFN_vkEnumerateInstanceLayerProperties
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
import {-# source #-} Graphics.Vulkan.C.Core10.Core
  ( VkResult
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkPhysicalDevice
  )


data VkLayerProperties

type FN_vkEnumerateDeviceLayerProperties = ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkLayerProperties) -> IO VkResult
type PFN_vkEnumerateDeviceLayerProperties = FunPtr FN_vkEnumerateDeviceLayerProperties

type FN_vkEnumerateInstanceLayerProperties = ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkLayerProperties) -> IO VkResult
type PFN_vkEnumerateInstanceLayerProperties = FunPtr FN_vkEnumerateInstanceLayerProperties
