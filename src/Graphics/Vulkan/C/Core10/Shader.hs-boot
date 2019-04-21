{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.Shader
  ( VkShaderModule
  , VkShaderModuleCreateFlags
  , VkShaderModuleCreateInfo
  , FN_vkCreateShaderModule
  , PFN_vkCreateShaderModule
  , FN_vkDestroyShaderModule
  , PFN_vkDestroyShaderModule
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
  ( VkAllocationCallbacks
  , VkDevice
  )


-- | Dummy data to tag the 'Ptr' with
data VkShaderModule_T
-- | VkShaderModule - Opaque handle to a shader module object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineShaderStageCreateInfo',
-- 'vkCreateShaderModule', 'vkDestroyShaderModule'
type VkShaderModule = Ptr VkShaderModule_T

data VkShaderModuleCreateFlags

data VkShaderModuleCreateInfo

type FN_vkCreateShaderModule = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkShaderModuleCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pShaderModule" ::: Ptr VkShaderModule) -> IO VkResult
type PFN_vkCreateShaderModule = FunPtr FN_vkCreateShaderModule

type FN_vkDestroyShaderModule = ("device" ::: VkDevice) -> ("shaderModule" ::: VkShaderModule) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyShaderModule = FunPtr FN_vkDestroyShaderModule
