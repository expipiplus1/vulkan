{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_NV_mesh_shader
  ( DrawMeshTasksIndirectCommandNV(..)
#if defined(VK_USE_PLATFORM_GGP)
  , PhysicalDeviceMeshShaderFeaturesNV(..)
  , PhysicalDeviceMeshShaderPropertiesNV(..)
#endif
  , cmdDrawMeshTasksIndirectCountNV
  , cmdDrawMeshTasksIndirectNV
  , cmdDrawMeshTasksNV
  , pattern NV_MESH_SHADER_EXTENSION_NAME
  , pattern NV_MESH_SHADER_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV
  , pattern SHADER_STAGE_TASK_BIT_NV
  , pattern SHADER_STAGE_MESH_BIT_NV
  , pattern PIPELINE_STAGE_TASK_SHADER_BIT_NV
  , pattern PIPELINE_STAGE_MESH_SHADER_BIT_NV
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader
  ( vkCmdDrawMeshTasksIndirectCountNV
  , vkCmdDrawMeshTasksIndirectNV
  , vkCmdDrawMeshTasksNV
  , pattern VK_NV_MESH_SHADER_EXTENSION_NAME
  , pattern VK_NV_MESH_SHADER_SPEC_VERSION
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

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV
  )
import Graphics.Vulkan.Core10.Pipeline
  ( pattern SHADER_STAGE_MESH_BIT_NV
  , pattern SHADER_STAGE_TASK_BIT_NV
  )
import Graphics.Vulkan.Core10.Queue
  ( pattern PIPELINE_STAGE_MESH_SHADER_BIT_NV
  , pattern PIPELINE_STAGE_TASK_SHADER_BIT_NV
  )



-- No documentation found for TopLevel "VkDrawMeshTasksIndirectCommandNV"
data DrawMeshTasksIndirectCommandNV = DrawMeshTasksIndirectCommandNV
  { -- No documentation found for Nested "DrawMeshTasksIndirectCommandNV" "taskCount"
  taskCount :: Word32
  , -- No documentation found for Nested "DrawMeshTasksIndirectCommandNV" "firstTask"
  firstTask :: Word32
  }
  deriving (Show, Eq)

instance Zero DrawMeshTasksIndirectCommandNV where
  zero = DrawMeshTasksIndirectCommandNV zero
                                        zero



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceMeshShaderFeaturesNV"
data PhysicalDeviceMeshShaderFeaturesNV = PhysicalDeviceMeshShaderFeaturesNV
  { -- No documentation found for Nested "PhysicalDeviceMeshShaderFeaturesNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceMeshShaderFeaturesNV" "taskShader"
  taskShader :: Bool
  , -- No documentation found for Nested "PhysicalDeviceMeshShaderFeaturesNV" "meshShader"
  meshShader :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceMeshShaderFeaturesNV where
  zero = PhysicalDeviceMeshShaderFeaturesNV Nothing
                                            False
                                            False

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceMeshShaderPropertiesNV"
data PhysicalDeviceMeshShaderPropertiesNV = PhysicalDeviceMeshShaderPropertiesNV
  { -- No documentation found for Nested "PhysicalDeviceMeshShaderPropertiesNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceMeshShaderPropertiesNV" "maxDrawMeshTasksCount"
  maxDrawMeshTasksCount :: Word32
  , -- No documentation found for Nested "PhysicalDeviceMeshShaderPropertiesNV" "maxTaskWorkGroupInvocations"
  maxTaskWorkGroupInvocations :: Word32
  , -- No documentation found for Nested "PhysicalDeviceMeshShaderPropertiesNV" "maxTaskWorkGroupSize"
  maxTaskWorkGroupSize :: (Word32, Word32, Word32)
  , -- No documentation found for Nested "PhysicalDeviceMeshShaderPropertiesNV" "maxTaskTotalMemorySize"
  maxTaskTotalMemorySize :: Word32
  , -- No documentation found for Nested "PhysicalDeviceMeshShaderPropertiesNV" "maxTaskOutputCount"
  maxTaskOutputCount :: Word32
  , -- No documentation found for Nested "PhysicalDeviceMeshShaderPropertiesNV" "maxMeshWorkGroupInvocations"
  maxMeshWorkGroupInvocations :: Word32
  , -- No documentation found for Nested "PhysicalDeviceMeshShaderPropertiesNV" "maxMeshWorkGroupSize"
  maxMeshWorkGroupSize :: (Word32, Word32, Word32)
  , -- No documentation found for Nested "PhysicalDeviceMeshShaderPropertiesNV" "maxMeshTotalMemorySize"
  maxMeshTotalMemorySize :: Word32
  , -- No documentation found for Nested "PhysicalDeviceMeshShaderPropertiesNV" "maxMeshOutputVertices"
  maxMeshOutputVertices :: Word32
  , -- No documentation found for Nested "PhysicalDeviceMeshShaderPropertiesNV" "maxMeshOutputPrimitives"
  maxMeshOutputPrimitives :: Word32
  , -- No documentation found for Nested "PhysicalDeviceMeshShaderPropertiesNV" "maxMeshMultiviewViewCount"
  maxMeshMultiviewViewCount :: Word32
  , -- No documentation found for Nested "PhysicalDeviceMeshShaderPropertiesNV" "meshOutputPerVertexGranularity"
  meshOutputPerVertexGranularity :: Word32
  , -- No documentation found for Nested "PhysicalDeviceMeshShaderPropertiesNV" "meshOutputPerPrimitiveGranularity"
  meshOutputPerPrimitiveGranularity :: Word32
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceMeshShaderPropertiesNV where
  zero = PhysicalDeviceMeshShaderPropertiesNV Nothing
                                              zero
                                              zero
                                              (zero, zero, zero)
                                              zero
                                              zero
                                              zero
                                              (zero, zero, zero)
                                              zero
                                              zero
                                              zero
                                              zero
                                              zero
                                              zero

#endif


-- No documentation found for TopLevel "vkCmdDrawMeshTasksIndirectCountNV"
cmdDrawMeshTasksIndirectCountNV :: CommandBuffer ->  Buffer ->  DeviceSize ->  Buffer ->  DeviceSize ->  Word32 ->  Word32 ->  IO ()
cmdDrawMeshTasksIndirectCountNV = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdDrawMeshTasksIndirectNV"
cmdDrawMeshTasksIndirectNV :: CommandBuffer ->  Buffer ->  DeviceSize ->  Word32 ->  Word32 ->  IO ()
cmdDrawMeshTasksIndirectNV = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdDrawMeshTasksNV"
cmdDrawMeshTasksNV :: CommandBuffer ->  Word32 ->  Word32 ->  IO ()
cmdDrawMeshTasksNV = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_NV_MESH_SHADER_EXTENSION_NAME"
pattern NV_MESH_SHADER_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NV_MESH_SHADER_EXTENSION_NAME = VK_NV_MESH_SHADER_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NV_MESH_SHADER_SPEC_VERSION"
pattern NV_MESH_SHADER_SPEC_VERSION :: Integral a => a
pattern NV_MESH_SHADER_SPEC_VERSION = VK_NV_MESH_SHADER_SPEC_VERSION
