{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader
  ( VkDrawMeshTasksIndirectCommandNV
  , VkPhysicalDeviceMeshShaderFeaturesNV
  , VkPhysicalDeviceMeshShaderPropertiesNV
  , FN_vkCmdDrawMeshTasksIndirectCountNV
  , PFN_vkCmdDrawMeshTasksIndirectCountNV
  , FN_vkCmdDrawMeshTasksIndirectNV
  , PFN_vkCmdDrawMeshTasksIndirectNV
  , FN_vkCmdDrawMeshTasksNV
  , PFN_vkCmdDrawMeshTasksNV
  ) where

import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  )


import Graphics.Vulkan.NamedType
  ( (:::)
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDeviceSize
  )
import {-# source #-} Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkBuffer
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Queue
  ( VkCommandBuffer
  )


data VkDrawMeshTasksIndirectCommandNV

data VkPhysicalDeviceMeshShaderFeaturesNV

data VkPhysicalDeviceMeshShaderPropertiesNV

type FN_vkCmdDrawMeshTasksIndirectCountNV = ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()
type PFN_vkCmdDrawMeshTasksIndirectCountNV = FunPtr FN_vkCmdDrawMeshTasksIndirectCountNV

type FN_vkCmdDrawMeshTasksIndirectNV = ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()
type PFN_vkCmdDrawMeshTasksIndirectNV = FunPtr FN_vkCmdDrawMeshTasksIndirectNV

type FN_vkCmdDrawMeshTasksNV = ("commandBuffer" ::: VkCommandBuffer) -> ("taskCount" ::: Word32) -> ("firstTask" ::: Word32) -> IO ()
type PFN_vkCmdDrawMeshTasksNV = FunPtr FN_vkCmdDrawMeshTasksNV
