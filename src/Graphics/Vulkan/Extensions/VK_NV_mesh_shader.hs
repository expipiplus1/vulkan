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


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader
  ( VkDrawMeshTasksIndirectCommandNV(..)
  , VkPhysicalDeviceMeshShaderFeaturesNV(..)
  , VkPhysicalDeviceMeshShaderPropertiesNV(..)
  , vkCmdDrawMeshTasksIndirectCountNV
  , vkCmdDrawMeshTasksIndirectNV
  , vkCmdDrawMeshTasksNV
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



-- | VkDrawMeshTasksIndirectCommandNV - Structure specifying a mesh tasks
-- draw indirect command
--
-- = Description
--
-- The members of
-- 'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VkDrawMeshTasksIndirectCommandNV'
-- have the same meaning as the similarly named parameters of
-- 'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.vkCmdDrawMeshTasksNV'.
--
-- == Valid Usage
--
-- Unresolved directive in VkDrawMeshTasksIndirectCommandNV.txt -
-- include::{generated}\/validity\/structs\/VkDrawMeshTasksIndirectCommandNV.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.vkCmdDrawMeshTasksIndirectNV'
data DrawMeshTasksIndirectCommandNV = DrawMeshTasksIndirectCommandNV
  { -- No documentation found for Nested "DrawMeshTasksIndirectCommandNV" "taskCount"
  taskCount :: Word32
  , -- No documentation found for Nested "DrawMeshTasksIndirectCommandNV" "firstTask"
  firstTask :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDrawMeshTasksIndirectCommandNV' and
-- marshal a 'DrawMeshTasksIndirectCommandNV' into it. The 'VkDrawMeshTasksIndirectCommandNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDrawMeshTasksIndirectCommandNV :: DrawMeshTasksIndirectCommandNV -> (VkDrawMeshTasksIndirectCommandNV -> IO a) -> IO a
withCStructDrawMeshTasksIndirectCommandNV marshalled cont = cont (VkDrawMeshTasksIndirectCommandNV (taskCount (marshalled :: DrawMeshTasksIndirectCommandNV)) (firstTask (marshalled :: DrawMeshTasksIndirectCommandNV)))

-- | A function to read a 'VkDrawMeshTasksIndirectCommandNV' and all additional
-- structures in the pointer chain into a 'DrawMeshTasksIndirectCommandNV'.
fromCStructDrawMeshTasksIndirectCommandNV :: VkDrawMeshTasksIndirectCommandNV -> IO DrawMeshTasksIndirectCommandNV
fromCStructDrawMeshTasksIndirectCommandNV c = DrawMeshTasksIndirectCommandNV <$> pure (vkTaskCount (c :: VkDrawMeshTasksIndirectCommandNV))
                                                                             <*> pure (vkFirstTask (c :: VkDrawMeshTasksIndirectCommandNV))

instance Zero DrawMeshTasksIndirectCommandNV where
  zero = DrawMeshTasksIndirectCommandNV zero
                                        zero



-- | VkPhysicalDeviceMeshShaderFeaturesNV - Structure describing mesh shading
-- features that can be supported by an implementation
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VkPhysicalDeviceMeshShaderFeaturesNV'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2',
-- it is filled with a value indicating whether the feature is supported.
-- 'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VkPhysicalDeviceMeshShaderFeaturesNV'
-- /can/ also be used in @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to enable the
-- features.
--
-- Unresolved directive in VkPhysicalDeviceMeshShaderFeaturesNV.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceMeshShaderFeaturesNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data PhysicalDeviceMeshShaderFeaturesNV = PhysicalDeviceMeshShaderFeaturesNV
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceMeshShaderFeaturesNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceMeshShaderFeaturesNV" "taskShader"
  taskShader :: Bool
  , -- No documentation found for Nested "PhysicalDeviceMeshShaderFeaturesNV" "meshShader"
  meshShader :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceMeshShaderFeaturesNV' and
-- marshal a 'PhysicalDeviceMeshShaderFeaturesNV' into it. The 'VkPhysicalDeviceMeshShaderFeaturesNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceMeshShaderFeaturesNV :: PhysicalDeviceMeshShaderFeaturesNV -> (VkPhysicalDeviceMeshShaderFeaturesNV -> IO a) -> IO a
withCStructPhysicalDeviceMeshShaderFeaturesNV marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceMeshShaderFeaturesNV)) (\pPNext -> cont (VkPhysicalDeviceMeshShaderFeaturesNV VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV pPNext (boolToBool32 (taskShader (marshalled :: PhysicalDeviceMeshShaderFeaturesNV))) (boolToBool32 (meshShader (marshalled :: PhysicalDeviceMeshShaderFeaturesNV)))))

-- | A function to read a 'VkPhysicalDeviceMeshShaderFeaturesNV' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceMeshShaderFeaturesNV'.
fromCStructPhysicalDeviceMeshShaderFeaturesNV :: VkPhysicalDeviceMeshShaderFeaturesNV -> IO PhysicalDeviceMeshShaderFeaturesNV
fromCStructPhysicalDeviceMeshShaderFeaturesNV c = PhysicalDeviceMeshShaderFeaturesNV <$> -- Univalued Member elided
                                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceMeshShaderFeaturesNV)))
                                                                                     <*> pure (bool32ToBool (vkTaskShader (c :: VkPhysicalDeviceMeshShaderFeaturesNV)))
                                                                                     <*> pure (bool32ToBool (vkMeshShader (c :: VkPhysicalDeviceMeshShaderFeaturesNV)))

instance Zero PhysicalDeviceMeshShaderFeaturesNV where
  zero = PhysicalDeviceMeshShaderFeaturesNV Nothing
                                            False
                                            False



-- | VkPhysicalDeviceMeshShaderPropertiesNV - Structure describing mesh
-- shading properties
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VkPhysicalDeviceMeshShaderPropertiesNV'
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VkPhysicalDeviceMeshShaderPropertiesNV'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- Unresolved directive in VkPhysicalDeviceMeshShaderPropertiesNV.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceMeshShaderPropertiesNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data PhysicalDeviceMeshShaderPropertiesNV = PhysicalDeviceMeshShaderPropertiesNV
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceMeshShaderPropertiesNV" "pNext"
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

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceMeshShaderPropertiesNV' and
-- marshal a 'PhysicalDeviceMeshShaderPropertiesNV' into it. The 'VkPhysicalDeviceMeshShaderPropertiesNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceMeshShaderPropertiesNV :: PhysicalDeviceMeshShaderPropertiesNV -> (VkPhysicalDeviceMeshShaderPropertiesNV -> IO a) -> IO a
withCStructPhysicalDeviceMeshShaderPropertiesNV marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceMeshShaderPropertiesNV)) (\pPNext -> cont (VkPhysicalDeviceMeshShaderPropertiesNV VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV pPNext (maxDrawMeshTasksCount (marshalled :: PhysicalDeviceMeshShaderPropertiesNV)) (maxTaskWorkGroupInvocations (marshalled :: PhysicalDeviceMeshShaderPropertiesNV)) (fromTuple (maxTaskWorkGroupSize (marshalled :: PhysicalDeviceMeshShaderPropertiesNV))) (maxTaskTotalMemorySize (marshalled :: PhysicalDeviceMeshShaderPropertiesNV)) (maxTaskOutputCount (marshalled :: PhysicalDeviceMeshShaderPropertiesNV)) (maxMeshWorkGroupInvocations (marshalled :: PhysicalDeviceMeshShaderPropertiesNV)) (fromTuple (maxMeshWorkGroupSize (marshalled :: PhysicalDeviceMeshShaderPropertiesNV))) (maxMeshTotalMemorySize (marshalled :: PhysicalDeviceMeshShaderPropertiesNV)) (maxMeshOutputVertices (marshalled :: PhysicalDeviceMeshShaderPropertiesNV)) (maxMeshOutputPrimitives (marshalled :: PhysicalDeviceMeshShaderPropertiesNV)) (maxMeshMultiviewViewCount (marshalled :: PhysicalDeviceMeshShaderPropertiesNV)) (meshOutputPerVertexGranularity (marshalled :: PhysicalDeviceMeshShaderPropertiesNV)) (meshOutputPerPrimitiveGranularity (marshalled :: PhysicalDeviceMeshShaderPropertiesNV))))

-- | A function to read a 'VkPhysicalDeviceMeshShaderPropertiesNV' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceMeshShaderPropertiesNV'.
fromCStructPhysicalDeviceMeshShaderPropertiesNV :: VkPhysicalDeviceMeshShaderPropertiesNV -> IO PhysicalDeviceMeshShaderPropertiesNV
fromCStructPhysicalDeviceMeshShaderPropertiesNV c = PhysicalDeviceMeshShaderPropertiesNV <$> -- Univalued Member elided
                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceMeshShaderPropertiesNV)))
                                                                                         <*> pure (vkMaxDrawMeshTasksCount (c :: VkPhysicalDeviceMeshShaderPropertiesNV))
                                                                                         <*> pure (vkMaxTaskWorkGroupInvocations (c :: VkPhysicalDeviceMeshShaderPropertiesNV))
                                                                                         <*> pure (let v = (vkMaxTaskWorkGroupSize (c :: VkPhysicalDeviceMeshShaderPropertiesNV)) in ( Data.Vector.Storable.Sized.unsafeIndex v 0
                                                                                         , Data.Vector.Storable.Sized.unsafeIndex v 1
                                                                                         , Data.Vector.Storable.Sized.unsafeIndex v 2 ))
                                                                                         <*> pure (vkMaxTaskTotalMemorySize (c :: VkPhysicalDeviceMeshShaderPropertiesNV))
                                                                                         <*> pure (vkMaxTaskOutputCount (c :: VkPhysicalDeviceMeshShaderPropertiesNV))
                                                                                         <*> pure (vkMaxMeshWorkGroupInvocations (c :: VkPhysicalDeviceMeshShaderPropertiesNV))
                                                                                         <*> pure (let v = (vkMaxMeshWorkGroupSize (c :: VkPhysicalDeviceMeshShaderPropertiesNV)) in ( Data.Vector.Storable.Sized.unsafeIndex v 0
                                                                                         , Data.Vector.Storable.Sized.unsafeIndex v 1
                                                                                         , Data.Vector.Storable.Sized.unsafeIndex v 2 ))
                                                                                         <*> pure (vkMaxMeshTotalMemorySize (c :: VkPhysicalDeviceMeshShaderPropertiesNV))
                                                                                         <*> pure (vkMaxMeshOutputVertices (c :: VkPhysicalDeviceMeshShaderPropertiesNV))
                                                                                         <*> pure (vkMaxMeshOutputPrimitives (c :: VkPhysicalDeviceMeshShaderPropertiesNV))
                                                                                         <*> pure (vkMaxMeshMultiviewViewCount (c :: VkPhysicalDeviceMeshShaderPropertiesNV))
                                                                                         <*> pure (vkMeshOutputPerVertexGranularity (c :: VkPhysicalDeviceMeshShaderPropertiesNV))
                                                                                         <*> pure (vkMeshOutputPerPrimitiveGranularity (c :: VkPhysicalDeviceMeshShaderPropertiesNV))

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



-- | vkCmdDrawMeshTasksIndirectCountNV - Perform an indirect mesh tasks draw
-- with the draw count sourced from a buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @buffer@ is the buffer containing draw parameters.
--
-- -   @offset@ is the byte offset into @buffer@ where parameters begin.
--
-- -   @countBuffer@ is the buffer containing the draw count.
--
-- -   @countBufferOffset@ is the byte offset into @countBuffer@ where the
--     draw count begins.
--
-- -   @maxDrawCount@ specifies the maximum number of draws that will be
--     executed. The actual number of executed draw calls is the minimum of
--     the count specified in @countBuffer@ and @maxDrawCount@.
--
-- -   @stride@ is the byte stride between successive sets of draw
--     parameters.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.vkCmdDrawMeshTasksIndirectCountNV'
-- behaves similarly to
-- 'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.vkCmdDrawMeshTasksIndirectNV'
-- except that the draw count is read by the device from a buffer during
-- execution. The command will read an unsigned 32-bit integer from
-- @countBuffer@ located at @countBufferOffset@ and use this as the draw
-- count.
--
-- == Valid Usage
--
-- Unresolved directive in vkCmdDrawMeshTasksIndirectCountNV.txt -
-- include::{chapters}\/commonvalidity\/draw_common.txt[] Unresolved
-- directive in vkCmdDrawMeshTasksIndirectCountNV.txt -
-- include::{chapters}\/commonvalidity\/draw_dispatch_indirect_common.txt[]
-- Unresolved directive in vkCmdDrawMeshTasksIndirectCountNV.txt -
-- include::{chapters}\/commonvalidity\/draw_indirect_count_common.txt[] *
-- @stride@ /must/ be a multiple of @4@ and /must/ be greater than or equal
-- to
-- @sizeof@('Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VkDrawMeshTasksIndirectCommandNV')
-- * If @maxDrawCount@ is greater than or equal to @1@, (@stride@ ×
-- (@maxDrawCount@ - 1) + @offset@ +
-- @sizeof@('Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VkDrawMeshTasksIndirectCommandNV'))
-- /must/ be less than or equal to the size of @buffer@ * If the count
-- stored in @countBuffer@ is equal to @1@, (@offset@ +
-- @sizeof@('Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VkDrawMeshTasksIndirectCommandNV'))
-- /must/ be less than or equal to the size of @buffer@ * If the count
-- stored in @countBuffer@ is greater than @1@, (@stride@ × (@drawCount@ -
-- 1) + @offset@ +
-- @sizeof@('Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VkDrawMeshTasksIndirectCommandNV'))
-- /must/ be less than or equal to the size of @buffer@
--
-- Unresolved directive in vkCmdDrawMeshTasksIndirectCountNV.txt -
-- include::{generated}\/validity\/protos\/vkCmdDrawMeshTasksIndirectCountNV.txt[]
--
-- = See Also
--
-- No cross-references are available
cmdDrawMeshTasksIndirectCountNV :: CommandBuffer ->  Buffer ->  DeviceSize ->  Buffer ->  DeviceSize ->  Word32 ->  Word32 ->  IO ()
cmdDrawMeshTasksIndirectCountNV = \(CommandBuffer commandBuffer' commandTable) -> \buffer' -> \offset' -> \countBuffer' -> \countBufferOffset' -> \maxDrawCount' -> \stride' -> vkCmdDrawMeshTasksIndirectCountNV commandTable commandBuffer' buffer' offset' countBuffer' countBufferOffset' maxDrawCount' stride' *> (pure ())


-- | vkCmdDrawMeshTasksIndirectNV - Issue an indirect mesh tasks draw into a
-- command buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @buffer@ is the buffer containing draw parameters.
--
-- -   @offset@ is the byte offset into @buffer@ where parameters begin.
--
-- -   @drawCount@ is the number of draws to execute, and /can/ be zero.
--
-- -   @stride@ is the byte stride between successive sets of draw
--     parameters.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.vkCmdDrawMeshTasksIndirectNV'
-- behaves similarly to
-- 'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.vkCmdDrawMeshTasksNV'
-- except that the parameters are read by the device from a buffer during
-- execution. @drawCount@ draws are executed by the command, with
-- parameters taken from @buffer@ starting at @offset@ and increasing by
-- @stride@ bytes for each successive draw. The parameters of each draw are
-- encoded in an array of
-- 'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VkDrawMeshTasksIndirectCommandNV'
-- structures. If @drawCount@ is less than or equal to one, @stride@ is
-- ignored.
--
-- == Valid Usage
--
-- Unresolved directive in vkCmdDrawMeshTasksIndirectNV.txt -
-- include::{chapters}\/commonvalidity\/draw_common.txt[] Unresolved
-- directive in vkCmdDrawMeshTasksIndirectNV.txt -
-- include::{chapters}\/commonvalidity\/draw_dispatch_indirect_common.txt[]
-- Unresolved directive in vkCmdDrawMeshTasksIndirectNV.txt -
-- include::{chapters}\/commonvalidity\/draw_indirect_drawcount.txt[] * If
-- @drawCount@ is greater than @1@, @stride@ /must/ be a multiple of @4@
-- and /must/ be greater than or equal to
-- @sizeof@('Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VkDrawMeshTasksIndirectCommandNV')
-- * If @drawCount@ is equal to @1@, (@offset@ +
-- @sizeof@('Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VkDrawMeshTasksIndirectCommandNV'))
-- /must/ be less than or equal to the size of @buffer@ * If @drawCount@ is
-- greater than @1@, (@stride@ × (@drawCount@ - 1) + @offset@ +
-- @sizeof@('Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VkDrawMeshTasksIndirectCommandNV'))
-- /must/ be less than or equal to the size of @buffer@
--
-- Unresolved directive in vkCmdDrawMeshTasksIndirectNV.txt -
-- include::{generated}\/validity\/protos\/vkCmdDrawMeshTasksIndirectNV.txt[]
--
-- = See Also
--
-- No cross-references are available
cmdDrawMeshTasksIndirectNV :: CommandBuffer ->  Buffer ->  DeviceSize ->  Word32 ->  Word32 ->  IO ()
cmdDrawMeshTasksIndirectNV = \(CommandBuffer commandBuffer' commandTable) -> \buffer' -> \offset' -> \drawCount' -> \stride' -> vkCmdDrawMeshTasksIndirectNV commandTable commandBuffer' buffer' offset' drawCount' stride' *> (pure ())


-- | vkCmdDrawMeshTasksNV - Draw mesh task work items
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @taskCount@ is the number of local workgroups to dispatch in the X
--     dimension. Y and Z dimension are implicitly set to one.
--
-- -   @firstTask@ is the X component of the first workgroup ID.
--
-- = Description
--
-- When the command is executed, a global workgroup consisting of
-- @taskCount@ local workgroups is assembled.
--
-- == Valid Usage
--
-- Unresolved directive in vkCmdDrawMeshTasksNV.txt -
-- include::{chapters}\/commonvalidity\/draw_common.txt[] * @taskCount@
-- /must/ be less than or equal to
-- 'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VkPhysicalDeviceMeshShaderPropertiesNV'::@maxDrawMeshTasksCount@
--
-- Unresolved directive in vkCmdDrawMeshTasksNV.txt -
-- include::{generated}\/validity\/protos\/vkCmdDrawMeshTasksNV.txt[]
--
-- = See Also
--
-- No cross-references are available
cmdDrawMeshTasksNV :: CommandBuffer ->  Word32 ->  Word32 ->  IO ()
cmdDrawMeshTasksNV = \(CommandBuffer commandBuffer' commandTable) -> \taskCount' -> \firstTask' -> vkCmdDrawMeshTasksNV commandTable commandBuffer' taskCount' firstTask' *> (pure ())
