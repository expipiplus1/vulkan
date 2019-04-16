{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader
  ( VkDrawMeshTasksIndirectCommandNV(..)
  , VkPhysicalDeviceMeshShaderFeaturesNV(..)
  , VkPhysicalDeviceMeshShaderPropertiesNV(..)
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkCmdDrawMeshTasksIndirectCountNV
#endif
  , FN_vkCmdDrawMeshTasksIndirectCountNV
  , PFN_vkCmdDrawMeshTasksIndirectCountNV
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkCmdDrawMeshTasksIndirectNV
#endif
  , FN_vkCmdDrawMeshTasksIndirectNV
  , PFN_vkCmdDrawMeshTasksIndirectNV
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkCmdDrawMeshTasksNV
#endif
  , FN_vkCmdDrawMeshTasksNV
  , PFN_vkCmdDrawMeshTasksNV
  , pattern VK_NV_MESH_SHADER_EXTENSION_NAME
  , pattern VK_NV_MESH_SHADER_SPEC_VERSION
  , pattern VK_PIPELINE_STAGE_MESH_SHADER_BIT_NV
  , pattern VK_PIPELINE_STAGE_TASK_SHADER_BIT_NV
  , pattern VK_SHADER_STAGE_MESH_BIT_NV
  , pattern VK_SHADER_STAGE_TASK_BIT_NV
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV
  ) where

import Data.String
  ( IsString
  )
import Data.Vector.Storable.Sized
  ( Vector
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDeviceSize
  )
import Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkBuffer
  )
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkShaderStageFlagBits(..)
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkPipelineStageFlagBits(..)
  , VkCommandBuffer
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkDrawMeshTasksIndirectCommandNV"
data VkDrawMeshTasksIndirectCommandNV = VkDrawMeshTasksIndirectCommandNV
  { -- No documentation found for Nested "VkDrawMeshTasksIndirectCommandNV" "taskCount"
  vkTaskCount :: Word32
  , -- No documentation found for Nested "VkDrawMeshTasksIndirectCommandNV" "firstTask"
  vkFirstTask :: Word32
  }
  deriving (Eq, Show)

instance Storable VkDrawMeshTasksIndirectCommandNV where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkDrawMeshTasksIndirectCommandNV <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkTaskCount (poked :: VkDrawMeshTasksIndirectCommandNV))
                *> poke (ptr `plusPtr` 4) (vkFirstTask (poked :: VkDrawMeshTasksIndirectCommandNV))
-- No documentation found for TopLevel "VkPhysicalDeviceMeshShaderFeaturesNV"
data VkPhysicalDeviceMeshShaderFeaturesNV = VkPhysicalDeviceMeshShaderFeaturesNV
  { -- No documentation found for Nested "VkPhysicalDeviceMeshShaderFeaturesNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceMeshShaderFeaturesNV" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceMeshShaderFeaturesNV" "taskShader"
  vkTaskShader :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceMeshShaderFeaturesNV" "meshShader"
  vkMeshShader :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceMeshShaderFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceMeshShaderFeaturesNV <$> peek (ptr `plusPtr` 0)
                                                  <*> peek (ptr `plusPtr` 8)
                                                  <*> peek (ptr `plusPtr` 16)
                                                  <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceMeshShaderFeaturesNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceMeshShaderFeaturesNV))
                *> poke (ptr `plusPtr` 16) (vkTaskShader (poked :: VkPhysicalDeviceMeshShaderFeaturesNV))
                *> poke (ptr `plusPtr` 20) (vkMeshShader (poked :: VkPhysicalDeviceMeshShaderFeaturesNV))
-- No documentation found for TopLevel "VkPhysicalDeviceMeshShaderPropertiesNV"
data VkPhysicalDeviceMeshShaderPropertiesNV = VkPhysicalDeviceMeshShaderPropertiesNV
  { -- No documentation found for Nested "VkPhysicalDeviceMeshShaderPropertiesNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceMeshShaderPropertiesNV" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceMeshShaderPropertiesNV" "maxDrawMeshTasksCount"
  vkMaxDrawMeshTasksCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceMeshShaderPropertiesNV" "maxTaskWorkGroupInvocations"
  vkMaxTaskWorkGroupInvocations :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceMeshShaderPropertiesNV" "maxTaskWorkGroupSize"
  vkMaxTaskWorkGroupSize :: Vector 3 Word32
  , -- No documentation found for Nested "VkPhysicalDeviceMeshShaderPropertiesNV" "maxTaskTotalMemorySize"
  vkMaxTaskTotalMemorySize :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceMeshShaderPropertiesNV" "maxTaskOutputCount"
  vkMaxTaskOutputCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceMeshShaderPropertiesNV" "maxMeshWorkGroupInvocations"
  vkMaxMeshWorkGroupInvocations :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceMeshShaderPropertiesNV" "maxMeshWorkGroupSize"
  vkMaxMeshWorkGroupSize :: Vector 3 Word32
  , -- No documentation found for Nested "VkPhysicalDeviceMeshShaderPropertiesNV" "maxMeshTotalMemorySize"
  vkMaxMeshTotalMemorySize :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceMeshShaderPropertiesNV" "maxMeshOutputVertices"
  vkMaxMeshOutputVertices :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceMeshShaderPropertiesNV" "maxMeshOutputPrimitives"
  vkMaxMeshOutputPrimitives :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceMeshShaderPropertiesNV" "maxMeshMultiviewViewCount"
  vkMaxMeshMultiviewViewCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceMeshShaderPropertiesNV" "meshOutputPerVertexGranularity"
  vkMeshOutputPerVertexGranularity :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceMeshShaderPropertiesNV" "meshOutputPerPrimitiveGranularity"
  vkMeshOutputPerPrimitiveGranularity :: Word32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceMeshShaderPropertiesNV where
  sizeOf ~_ = 88
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceMeshShaderPropertiesNV <$> peek (ptr `plusPtr` 0)
                                                    <*> peek (ptr `plusPtr` 8)
                                                    <*> peek (ptr `plusPtr` 16)
                                                    <*> peek (ptr `plusPtr` 20)
                                                    <*> peek (ptr `plusPtr` 24)
                                                    <*> peek (ptr `plusPtr` 36)
                                                    <*> peek (ptr `plusPtr` 40)
                                                    <*> peek (ptr `plusPtr` 44)
                                                    <*> peek (ptr `plusPtr` 48)
                                                    <*> peek (ptr `plusPtr` 60)
                                                    <*> peek (ptr `plusPtr` 64)
                                                    <*> peek (ptr `plusPtr` 68)
                                                    <*> peek (ptr `plusPtr` 72)
                                                    <*> peek (ptr `plusPtr` 76)
                                                    <*> peek (ptr `plusPtr` 80)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceMeshShaderPropertiesNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceMeshShaderPropertiesNV))
                *> poke (ptr `plusPtr` 16) (vkMaxDrawMeshTasksCount (poked :: VkPhysicalDeviceMeshShaderPropertiesNV))
                *> poke (ptr `plusPtr` 20) (vkMaxTaskWorkGroupInvocations (poked :: VkPhysicalDeviceMeshShaderPropertiesNV))
                *> poke (ptr `plusPtr` 24) (vkMaxTaskWorkGroupSize (poked :: VkPhysicalDeviceMeshShaderPropertiesNV))
                *> poke (ptr `plusPtr` 36) (vkMaxTaskTotalMemorySize (poked :: VkPhysicalDeviceMeshShaderPropertiesNV))
                *> poke (ptr `plusPtr` 40) (vkMaxTaskOutputCount (poked :: VkPhysicalDeviceMeshShaderPropertiesNV))
                *> poke (ptr `plusPtr` 44) (vkMaxMeshWorkGroupInvocations (poked :: VkPhysicalDeviceMeshShaderPropertiesNV))
                *> poke (ptr `plusPtr` 48) (vkMaxMeshWorkGroupSize (poked :: VkPhysicalDeviceMeshShaderPropertiesNV))
                *> poke (ptr `plusPtr` 60) (vkMaxMeshTotalMemorySize (poked :: VkPhysicalDeviceMeshShaderPropertiesNV))
                *> poke (ptr `plusPtr` 64) (vkMaxMeshOutputVertices (poked :: VkPhysicalDeviceMeshShaderPropertiesNV))
                *> poke (ptr `plusPtr` 68) (vkMaxMeshOutputPrimitives (poked :: VkPhysicalDeviceMeshShaderPropertiesNV))
                *> poke (ptr `plusPtr` 72) (vkMaxMeshMultiviewViewCount (poked :: VkPhysicalDeviceMeshShaderPropertiesNV))
                *> poke (ptr `plusPtr` 76) (vkMeshOutputPerVertexGranularity (poked :: VkPhysicalDeviceMeshShaderPropertiesNV))
                *> poke (ptr `plusPtr` 80) (vkMeshOutputPerPrimitiveGranularity (poked :: VkPhysicalDeviceMeshShaderPropertiesNV))
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkCmdDrawMeshTasksIndirectCountNV"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdDrawMeshTasksIndirectCountNV" vkCmdDrawMeshTasksIndirectCountNV :: ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()

#endif
type FN_vkCmdDrawMeshTasksIndirectCountNV = ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()
type PFN_vkCmdDrawMeshTasksIndirectCountNV = FunPtr FN_vkCmdDrawMeshTasksIndirectCountNV
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkCmdDrawMeshTasksIndirectNV"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdDrawMeshTasksIndirectNV" vkCmdDrawMeshTasksIndirectNV :: ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()

#endif
type FN_vkCmdDrawMeshTasksIndirectNV = ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()
type PFN_vkCmdDrawMeshTasksIndirectNV = FunPtr FN_vkCmdDrawMeshTasksIndirectNV
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkCmdDrawMeshTasksNV"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdDrawMeshTasksNV" vkCmdDrawMeshTasksNV :: ("commandBuffer" ::: VkCommandBuffer) -> ("taskCount" ::: Word32) -> ("firstTask" ::: Word32) -> IO ()

#endif
type FN_vkCmdDrawMeshTasksNV = ("commandBuffer" ::: VkCommandBuffer) -> ("taskCount" ::: Word32) -> ("firstTask" ::: Word32) -> IO ()
type PFN_vkCmdDrawMeshTasksNV = FunPtr FN_vkCmdDrawMeshTasksNV
-- No documentation found for TopLevel "VK_NV_MESH_SHADER_EXTENSION_NAME"
pattern VK_NV_MESH_SHADER_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NV_MESH_SHADER_EXTENSION_NAME = "VK_NV_mesh_shader"
-- No documentation found for TopLevel "VK_NV_MESH_SHADER_SPEC_VERSION"
pattern VK_NV_MESH_SHADER_SPEC_VERSION :: Integral a => a
pattern VK_NV_MESH_SHADER_SPEC_VERSION = 1
-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_MESH_SHADER_BIT_NV"
pattern VK_PIPELINE_STAGE_MESH_SHADER_BIT_NV :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_MESH_SHADER_BIT_NV = VkPipelineStageFlagBits 0x00100000
-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_TASK_SHADER_BIT_NV"
pattern VK_PIPELINE_STAGE_TASK_SHADER_BIT_NV :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_TASK_SHADER_BIT_NV = VkPipelineStageFlagBits 0x00080000
-- No documentation found for Nested "VkShaderStageFlagBits" "VK_SHADER_STAGE_MESH_BIT_NV"
pattern VK_SHADER_STAGE_MESH_BIT_NV :: VkShaderStageFlagBits
pattern VK_SHADER_STAGE_MESH_BIT_NV = VkShaderStageFlagBits 0x00000080
-- No documentation found for Nested "VkShaderStageFlagBits" "VK_SHADER_STAGE_TASK_BIT_NV"
pattern VK_SHADER_STAGE_TASK_BIT_NV :: VkShaderStageFlagBits
pattern VK_SHADER_STAGE_TASK_BIT_NV = VkShaderStageFlagBits 0x00000040
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV = VkStructureType 1000202000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV = VkStructureType 1000202001
