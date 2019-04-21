{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_AMD_shader_info
  ( VkShaderInfoTypeAMD
  , VkShaderResourceUsageAMD
  , VkShaderStatisticsInfoAMD
  , FN_vkGetShaderInfoAMD
  , PFN_vkGetShaderInfoAMD
  ) where

import Foreign.C.Types
  ( CSize(..)
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
  ( VkDevice
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Pipeline
  ( VkShaderStageFlagBits
  , VkPipeline
  )


data VkShaderInfoTypeAMD

data VkShaderResourceUsageAMD

data VkShaderStatisticsInfoAMD

type FN_vkGetShaderInfoAMD = ("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("shaderStage" ::: VkShaderStageFlagBits) -> ("infoType" ::: VkShaderInfoTypeAMD) -> ("pInfoSize" ::: Ptr CSize) -> ("pInfo" ::: Ptr ()) -> IO VkResult
type PFN_vkGetShaderInfoAMD = FunPtr FN_vkGetShaderInfoAMD
