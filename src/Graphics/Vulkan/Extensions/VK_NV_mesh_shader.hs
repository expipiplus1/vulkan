{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_NV_mesh_shader
  ( withCStructDrawMeshTasksIndirectCommandNV
  , fromCStructDrawMeshTasksIndirectCommandNV
  , DrawMeshTasksIndirectCommandNV(..)
  , withCStructPhysicalDeviceMeshShaderFeaturesNV
  , fromCStructPhysicalDeviceMeshShaderFeaturesNV
  , PhysicalDeviceMeshShaderFeaturesNV(..)
  , withCStructPhysicalDeviceMeshShaderPropertiesNV
  , fromCStructPhysicalDeviceMeshShaderPropertiesNV
  , PhysicalDeviceMeshShaderPropertiesNV(..)
  , cmdDrawMeshTasksIndirectCountNV
  , cmdDrawMeshTasksIndirectNV
  , cmdDrawMeshTasksNV
  , pattern VK_NV_MESH_SHADER_SPEC_VERSION
  , pattern VK_NV_MESH_SHADER_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV
  , pattern VK_SHADER_STAGE_TASK_BIT_NV
  , pattern VK_SHADER_STAGE_MESH_BIT_NV
  , pattern VK_PIPELINE_STAGE_TASK_SHADER_BIT_NV
  , pattern VK_PIPELINE_STAGE_MESH_SHADER_BIT_NV
  ) where

import Data.Vector.Generic.Sized
  ( fromTuple
  )
import qualified Data.Vector.Storable.Sized
  ( unsafeIndex
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( cmdDrawMeshTasksIndirectCountNV
  , cmdDrawMeshTasksIndirectNV
  , cmdDrawMeshTasksNV
  )


import Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader
  ( VkDrawMeshTasksIndirectCommandNV(..)
  , VkPhysicalDeviceMeshShaderFeaturesNV(..)
  , VkPhysicalDeviceMeshShaderPropertiesNV(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( DeviceSize
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( Buffer
  )
import Graphics.Vulkan.Core10.Queue
  ( CommandBuffer(..)
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader
  ( pattern VK_NV_MESH_SHADER_EXTENSION_NAME
  , pattern VK_NV_MESH_SHADER_SPEC_VERSION
  , pattern VK_PIPELINE_STAGE_MESH_SHADER_BIT_NV
  , pattern VK_PIPELINE_STAGE_TASK_SHADER_BIT_NV
  , pattern VK_SHADER_STAGE_MESH_BIT_NV
  , pattern VK_SHADER_STAGE_TASK_BIT_NV
  )


-- No documentation found for TopLevel "DrawMeshTasksIndirectCommandNV"
data DrawMeshTasksIndirectCommandNV = DrawMeshTasksIndirectCommandNV
  { -- No documentation found for Nested "DrawMeshTasksIndirectCommandNV" "taskCount"
  vkTaskCount :: Word32
  , -- No documentation found for Nested "DrawMeshTasksIndirectCommandNV" "firstTask"
  vkFirstTask :: Word32
  }
  deriving (Show, Eq)
withCStructDrawMeshTasksIndirectCommandNV :: DrawMeshTasksIndirectCommandNV -> (VkDrawMeshTasksIndirectCommandNV -> IO a) -> IO a
withCStructDrawMeshTasksIndirectCommandNV from cont = cont (VkDrawMeshTasksIndirectCommandNV (vkTaskCount (from :: DrawMeshTasksIndirectCommandNV)) (vkFirstTask (from :: DrawMeshTasksIndirectCommandNV)))
fromCStructDrawMeshTasksIndirectCommandNV :: VkDrawMeshTasksIndirectCommandNV -> IO DrawMeshTasksIndirectCommandNV
fromCStructDrawMeshTasksIndirectCommandNV c = DrawMeshTasksIndirectCommandNV <$> pure (vkTaskCount (c :: VkDrawMeshTasksIndirectCommandNV))
                                                                             <*> pure (vkFirstTask (c :: VkDrawMeshTasksIndirectCommandNV))
-- No documentation found for TopLevel "PhysicalDeviceMeshShaderFeaturesNV"
data PhysicalDeviceMeshShaderFeaturesNV = PhysicalDeviceMeshShaderFeaturesNV
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceMeshShaderFeaturesNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceMeshShaderFeaturesNV" "taskShader"
  vkTaskShader :: Bool
  , -- No documentation found for Nested "PhysicalDeviceMeshShaderFeaturesNV" "meshShader"
  vkMeshShader :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceMeshShaderFeaturesNV :: PhysicalDeviceMeshShaderFeaturesNV -> (VkPhysicalDeviceMeshShaderFeaturesNV -> IO a) -> IO a
withCStructPhysicalDeviceMeshShaderFeaturesNV from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceMeshShaderFeaturesNV)) (\pPNext -> cont (VkPhysicalDeviceMeshShaderFeaturesNV VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV pPNext (boolToBool32 (vkTaskShader (from :: PhysicalDeviceMeshShaderFeaturesNV))) (boolToBool32 (vkMeshShader (from :: PhysicalDeviceMeshShaderFeaturesNV)))))
fromCStructPhysicalDeviceMeshShaderFeaturesNV :: VkPhysicalDeviceMeshShaderFeaturesNV -> IO PhysicalDeviceMeshShaderFeaturesNV
fromCStructPhysicalDeviceMeshShaderFeaturesNV c = PhysicalDeviceMeshShaderFeaturesNV <$> -- Univalued Member elided
                                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceMeshShaderFeaturesNV)))
                                                                                     <*> pure (bool32ToBool (vkTaskShader (c :: VkPhysicalDeviceMeshShaderFeaturesNV)))
                                                                                     <*> pure (bool32ToBool (vkMeshShader (c :: VkPhysicalDeviceMeshShaderFeaturesNV)))
-- No documentation found for TopLevel "PhysicalDeviceMeshShaderPropertiesNV"
data PhysicalDeviceMeshShaderPropertiesNV = PhysicalDeviceMeshShaderPropertiesNV
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceMeshShaderPropertiesNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceMeshShaderPropertiesNV" "maxDrawMeshTasksCount"
  vkMaxDrawMeshTasksCount :: Word32
  , -- No documentation found for Nested "PhysicalDeviceMeshShaderPropertiesNV" "maxTaskWorkGroupInvocations"
  vkMaxTaskWorkGroupInvocations :: Word32
  , -- No documentation found for Nested "PhysicalDeviceMeshShaderPropertiesNV" "maxTaskWorkGroupSize"
  vkMaxTaskWorkGroupSize :: (Word32, Word32, Word32)
  , -- No documentation found for Nested "PhysicalDeviceMeshShaderPropertiesNV" "maxTaskTotalMemorySize"
  vkMaxTaskTotalMemorySize :: Word32
  , -- No documentation found for Nested "PhysicalDeviceMeshShaderPropertiesNV" "maxTaskOutputCount"
  vkMaxTaskOutputCount :: Word32
  , -- No documentation found for Nested "PhysicalDeviceMeshShaderPropertiesNV" "maxMeshWorkGroupInvocations"
  vkMaxMeshWorkGroupInvocations :: Word32
  , -- No documentation found for Nested "PhysicalDeviceMeshShaderPropertiesNV" "maxMeshWorkGroupSize"
  vkMaxMeshWorkGroupSize :: (Word32, Word32, Word32)
  , -- No documentation found for Nested "PhysicalDeviceMeshShaderPropertiesNV" "maxMeshTotalMemorySize"
  vkMaxMeshTotalMemorySize :: Word32
  , -- No documentation found for Nested "PhysicalDeviceMeshShaderPropertiesNV" "maxMeshOutputVertices"
  vkMaxMeshOutputVertices :: Word32
  , -- No documentation found for Nested "PhysicalDeviceMeshShaderPropertiesNV" "maxMeshOutputPrimitives"
  vkMaxMeshOutputPrimitives :: Word32
  , -- No documentation found for Nested "PhysicalDeviceMeshShaderPropertiesNV" "maxMeshMultiviewViewCount"
  vkMaxMeshMultiviewViewCount :: Word32
  , -- No documentation found for Nested "PhysicalDeviceMeshShaderPropertiesNV" "meshOutputPerVertexGranularity"
  vkMeshOutputPerVertexGranularity :: Word32
  , -- No documentation found for Nested "PhysicalDeviceMeshShaderPropertiesNV" "meshOutputPerPrimitiveGranularity"
  vkMeshOutputPerPrimitiveGranularity :: Word32
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceMeshShaderPropertiesNV :: PhysicalDeviceMeshShaderPropertiesNV -> (VkPhysicalDeviceMeshShaderPropertiesNV -> IO a) -> IO a
withCStructPhysicalDeviceMeshShaderPropertiesNV from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceMeshShaderPropertiesNV)) (\pPNext -> cont (VkPhysicalDeviceMeshShaderPropertiesNV VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV pPNext (vkMaxDrawMeshTasksCount (from :: PhysicalDeviceMeshShaderPropertiesNV)) (vkMaxTaskWorkGroupInvocations (from :: PhysicalDeviceMeshShaderPropertiesNV)) (fromTuple (vkMaxTaskWorkGroupSize (from :: PhysicalDeviceMeshShaderPropertiesNV))) (vkMaxTaskTotalMemorySize (from :: PhysicalDeviceMeshShaderPropertiesNV)) (vkMaxTaskOutputCount (from :: PhysicalDeviceMeshShaderPropertiesNV)) (vkMaxMeshWorkGroupInvocations (from :: PhysicalDeviceMeshShaderPropertiesNV)) (fromTuple (vkMaxMeshWorkGroupSize (from :: PhysicalDeviceMeshShaderPropertiesNV))) (vkMaxMeshTotalMemorySize (from :: PhysicalDeviceMeshShaderPropertiesNV)) (vkMaxMeshOutputVertices (from :: PhysicalDeviceMeshShaderPropertiesNV)) (vkMaxMeshOutputPrimitives (from :: PhysicalDeviceMeshShaderPropertiesNV)) (vkMaxMeshMultiviewViewCount (from :: PhysicalDeviceMeshShaderPropertiesNV)) (vkMeshOutputPerVertexGranularity (from :: PhysicalDeviceMeshShaderPropertiesNV)) (vkMeshOutputPerPrimitiveGranularity (from :: PhysicalDeviceMeshShaderPropertiesNV))))
fromCStructPhysicalDeviceMeshShaderPropertiesNV :: VkPhysicalDeviceMeshShaderPropertiesNV -> IO PhysicalDeviceMeshShaderPropertiesNV
fromCStructPhysicalDeviceMeshShaderPropertiesNV c = PhysicalDeviceMeshShaderPropertiesNV <$> -- Univalued Member elided
                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceMeshShaderPropertiesNV)))
                                                                                         <*> pure (vkMaxDrawMeshTasksCount (c :: VkPhysicalDeviceMeshShaderPropertiesNV))
                                                                                         <*> pure (vkMaxTaskWorkGroupInvocations (c :: VkPhysicalDeviceMeshShaderPropertiesNV))
                                                                                         <*> pure (let x = (vkMaxTaskWorkGroupSize (c :: VkPhysicalDeviceMeshShaderPropertiesNV)) in ( Data.Vector.Storable.Sized.unsafeIndex x 0
                                                                                         , Data.Vector.Storable.Sized.unsafeIndex x 1
                                                                                         , Data.Vector.Storable.Sized.unsafeIndex x 2 ))
                                                                                         <*> pure (vkMaxTaskTotalMemorySize (c :: VkPhysicalDeviceMeshShaderPropertiesNV))
                                                                                         <*> pure (vkMaxTaskOutputCount (c :: VkPhysicalDeviceMeshShaderPropertiesNV))
                                                                                         <*> pure (vkMaxMeshWorkGroupInvocations (c :: VkPhysicalDeviceMeshShaderPropertiesNV))
                                                                                         <*> pure (let x = (vkMaxMeshWorkGroupSize (c :: VkPhysicalDeviceMeshShaderPropertiesNV)) in ( Data.Vector.Storable.Sized.unsafeIndex x 0
                                                                                         , Data.Vector.Storable.Sized.unsafeIndex x 1
                                                                                         , Data.Vector.Storable.Sized.unsafeIndex x 2 ))
                                                                                         <*> pure (vkMaxMeshTotalMemorySize (c :: VkPhysicalDeviceMeshShaderPropertiesNV))
                                                                                         <*> pure (vkMaxMeshOutputVertices (c :: VkPhysicalDeviceMeshShaderPropertiesNV))
                                                                                         <*> pure (vkMaxMeshOutputPrimitives (c :: VkPhysicalDeviceMeshShaderPropertiesNV))
                                                                                         <*> pure (vkMaxMeshMultiviewViewCount (c :: VkPhysicalDeviceMeshShaderPropertiesNV))
                                                                                         <*> pure (vkMeshOutputPerVertexGranularity (c :: VkPhysicalDeviceMeshShaderPropertiesNV))
                                                                                         <*> pure (vkMeshOutputPerPrimitiveGranularity (c :: VkPhysicalDeviceMeshShaderPropertiesNV))

-- | Wrapper for 'vkCmdDrawMeshTasksIndirectCountNV'
cmdDrawMeshTasksIndirectCountNV :: CommandBuffer ->  Buffer ->  DeviceSize ->  Buffer ->  DeviceSize ->  Word32 ->  Word32 ->  IO (  )
cmdDrawMeshTasksIndirectCountNV = \(CommandBuffer commandBuffer commandTable) -> \buffer -> \offset -> \countBuffer -> \countBufferOffset -> \maxDrawCount -> \stride -> Graphics.Vulkan.C.Dynamic.cmdDrawMeshTasksIndirectCountNV commandTable commandBuffer buffer offset countBuffer countBufferOffset maxDrawCount stride *> (pure ())

-- | Wrapper for 'vkCmdDrawMeshTasksIndirectNV'
cmdDrawMeshTasksIndirectNV :: CommandBuffer ->  Buffer ->  DeviceSize ->  Word32 ->  Word32 ->  IO ()
cmdDrawMeshTasksIndirectNV = \(CommandBuffer commandBuffer commandTable) -> \buffer -> \offset -> \drawCount -> \stride -> Graphics.Vulkan.C.Dynamic.cmdDrawMeshTasksIndirectNV commandTable commandBuffer buffer offset drawCount stride *> (pure ())

-- | Wrapper for 'vkCmdDrawMeshTasksNV'
cmdDrawMeshTasksNV :: CommandBuffer ->  Word32 ->  Word32 ->  IO ()
cmdDrawMeshTasksNV = \(CommandBuffer commandBuffer commandTable) -> \taskCount -> \firstTask -> Graphics.Vulkan.C.Dynamic.cmdDrawMeshTasksNV commandTable commandBuffer taskCount firstTask *> (pure ())
