{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix
  ( VkComponentTypeNV
  , VkCooperativeMatrixPropertiesNV
  , VkPhysicalDeviceCooperativeMatrixFeaturesNV
  , VkPhysicalDeviceCooperativeMatrixPropertiesNV
  , VkScopeNV
  , FN_vkGetPhysicalDeviceCooperativeMatrixPropertiesNV
  , PFN_vkGetPhysicalDeviceCooperativeMatrixPropertiesNV
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


data VkComponentTypeNV

data VkCooperativeMatrixPropertiesNV

data VkPhysicalDeviceCooperativeMatrixFeaturesNV

data VkPhysicalDeviceCooperativeMatrixPropertiesNV

data VkScopeNV

type FN_vkGetPhysicalDeviceCooperativeMatrixPropertiesNV = ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkCooperativeMatrixPropertiesNV) -> IO VkResult
type PFN_vkGetPhysicalDeviceCooperativeMatrixPropertiesNV = FunPtr FN_vkGetPhysicalDeviceCooperativeMatrixPropertiesNV
