{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_NV_device_diagnostic_checkpoints
  ( VkCheckpointDataNV
  , VkQueueFamilyCheckpointPropertiesNV
  , FN_vkCmdSetCheckpointNV
  , PFN_vkCmdSetCheckpointNV
  , FN_vkGetQueueCheckpointDataNV
  , PFN_vkGetQueueCheckpointDataNV
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
import {-# source #-} Graphics.Vulkan.C.Core10.Queue
  ( VkCommandBuffer
  , VkQueue
  )


data VkCheckpointDataNV

data VkQueueFamilyCheckpointPropertiesNV

type FN_vkCmdSetCheckpointNV = ("commandBuffer" ::: VkCommandBuffer) -> ("pCheckpointMarker" ::: Ptr ()) -> IO ()
type PFN_vkCmdSetCheckpointNV = FunPtr FN_vkCmdSetCheckpointNV

type FN_vkGetQueueCheckpointDataNV = ("queue" ::: VkQueue) -> ("pCheckpointDataCount" ::: Ptr Word32) -> ("pCheckpointData" ::: Ptr VkCheckpointDataNV) -> IO ()
type PFN_vkGetQueueCheckpointDataNV = FunPtr FN_vkGetQueueCheckpointDataNV
