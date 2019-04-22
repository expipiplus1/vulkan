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
  , pattern VK_NV_MESH_SHADER_EXTENSION_NAME
  , pattern VK_NV_MESH_SHADER_SPEC_VERSION
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
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
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
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
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
-- -   If a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' is sampled
--     with 'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_LINEAR' as a result
--     of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   If a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' is accessed
--     using atomic operations as a result of this command, then the image
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   For each set /n/ that is statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to the pipeline
--     bind point used by this command, a descriptor set /must/ have been
--     bound to /n/ at the same pipeline bind point, with a
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' that is
--     compatible for set /n/, with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' used to create
--     the current 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline', as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   For each push constant that is statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to the pipeline
--     bind point used by this command, a push constant value /must/ have
--     been set for the same pipeline bind point, with a
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' that is
--     compatible for push constants, with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' used to create
--     the current 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline', as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   Descriptors in each bound descriptor set, specified via
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindDescriptorSets',
--     /must/ be valid if they are statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to the pipeline
--     bind point used by this command
--
-- -   A valid pipeline /must/ be bound to the pipeline bind point used by
--     this command
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command requires any dynamic
--     state, that state /must/ have been set for @commandBuffer@
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used to sample
--     from any 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' with a
--     'Graphics.Vulkan.C.Core10.ImageView.VkImageView' of the type
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_3D',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_CUBE',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_1D_ARRAY',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_2D_ARRAY' or
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_CUBE_ARRAY',
--     in any shader stage
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions with @ImplicitLod@, @Dref@ or @Proj@ in their name, in
--     any shader stage
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions that includes a LOD bias or any offset values, in any
--     shader stage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound to the
--     pipeline bind point used by this command accesses a uniform buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound to the
--     pipeline bind point used by this command accesses a storage buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   The current render pass /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the @renderPass@ member of the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo'
--     structure specified when creating the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS'.
--
-- -   The subpass index of the current render pass /must/ be equal to the
--     @subpass@ member of the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo'
--     structure specified when creating the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS'.
--
-- -   Every input attachment used by the current subpass /must/ be bound
--     to the pipeline via a descriptor set
--
-- -   Image subresources used as attachments in the current render pass
--     /must/ not be accessed in any way other than as an attachment by
--     this command.
--
-- -   If @buffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- -   @buffer@ /must/ have been created with the
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   @offset@ /must/ be a multiple of @4@
--
-- -   If @countBuffer@ is non-sparse then it /must/ be bound completely
--     and contiguously to a single
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- -   @countBuffer@ /must/ have been created with the
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   @countBufferOffset@ /must/ be a multiple of @4@
--
-- -   The count stored in @countBuffer@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxDrawIndirectCount@
--
-- -   @stride@ /must/ be a multiple of @4@ and /must/ be greater than or
--     equal to
--     @sizeof@('Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VkDrawMeshTasksIndirectCommandNV')
--
-- -   If @maxDrawCount@ is greater than or equal to @1@, (@stride@ ×
--     (@maxDrawCount@ - 1) + @offset@ +
--     @sizeof@('Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VkDrawMeshTasksIndirectCommandNV'))
--     /must/ be less than or equal to the size of @buffer@
--
-- -   If the count stored in @countBuffer@ is equal to @1@, (@offset@ +
--     @sizeof@('Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VkDrawMeshTasksIndirectCommandNV'))
--     /must/ be less than or equal to the size of @buffer@
--
-- -   If the count stored in @countBuffer@ is greater than @1@, (@stride@
--     × (@drawCount@ - 1) + @offset@ +
--     @sizeof@('Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VkDrawMeshTasksIndirectCommandNV'))
--     /must/ be less than or equal to the size of @buffer@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @buffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- -   @countBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- -   This command /must/ only be called inside of a render pass instance
--
-- -   Each of @buffer@, @commandBuffer@, and @countBuffer@ /must/ have
--     been created, allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Inside          | Graphics        | Graphics        |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize'
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
-- -   If a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' is sampled
--     with 'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_LINEAR' as a result
--     of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   If a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' is accessed
--     using atomic operations as a result of this command, then the image
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   For each set /n/ that is statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to the pipeline
--     bind point used by this command, a descriptor set /must/ have been
--     bound to /n/ at the same pipeline bind point, with a
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' that is
--     compatible for set /n/, with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' used to create
--     the current 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline', as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   For each push constant that is statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to the pipeline
--     bind point used by this command, a push constant value /must/ have
--     been set for the same pipeline bind point, with a
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' that is
--     compatible for push constants, with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' used to create
--     the current 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline', as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   Descriptors in each bound descriptor set, specified via
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindDescriptorSets',
--     /must/ be valid if they are statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to the pipeline
--     bind point used by this command
--
-- -   A valid pipeline /must/ be bound to the pipeline bind point used by
--     this command
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command requires any dynamic
--     state, that state /must/ have been set for @commandBuffer@
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used to sample
--     from any 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' with a
--     'Graphics.Vulkan.C.Core10.ImageView.VkImageView' of the type
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_3D',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_CUBE',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_1D_ARRAY',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_2D_ARRAY' or
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_CUBE_ARRAY',
--     in any shader stage
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions with @ImplicitLod@, @Dref@ or @Proj@ in their name, in
--     any shader stage
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions that includes a LOD bias or any offset values, in any
--     shader stage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound to the
--     pipeline bind point used by this command accesses a uniform buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound to the
--     pipeline bind point used by this command accesses a storage buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   The current render pass /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the @renderPass@ member of the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo'
--     structure specified when creating the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS'.
--
-- -   The subpass index of the current render pass /must/ be equal to the
--     @subpass@ member of the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo'
--     structure specified when creating the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS'.
--
-- -   Every input attachment used by the current subpass /must/ be bound
--     to the pipeline via a descriptor set
--
-- -   Image subresources used as attachments in the current render pass
--     /must/ not be accessed in any way other than as an attachment by
--     this command.
--
-- -   If @buffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- -   @buffer@ /must/ have been created with the
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   @offset@ /must/ be a multiple of @4@
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-multiDrawIndirect multi-draw indirect>
--     feature is not enabled, @drawCount@ /must/ be @0@ or @1@
--
-- -   @drawCount@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxDrawIndirectCount@
--
-- -   If @drawCount@ is greater than @1@, @stride@ /must/ be a multiple of
--     @4@ and /must/ be greater than or equal to
--     @sizeof@('Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VkDrawMeshTasksIndirectCommandNV')
--
-- -   If @drawCount@ is equal to @1@, (@offset@ +
--     @sizeof@('Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VkDrawMeshTasksIndirectCommandNV'))
--     /must/ be less than or equal to the size of @buffer@
--
-- -   If @drawCount@ is greater than @1@, (@stride@ × (@drawCount@ - 1) +
--     @offset@ +
--     @sizeof@('Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VkDrawMeshTasksIndirectCommandNV'))
--     /must/ be less than or equal to the size of @buffer@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @buffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- -   This command /must/ only be called inside of a render pass instance
--
-- -   Both of @buffer@, and @commandBuffer@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Inside          | Graphics        | Graphics        |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize'
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
-- -   If a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' is sampled
--     with 'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_LINEAR' as a result
--     of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   If a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' is accessed
--     using atomic operations as a result of this command, then the image
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   For each set /n/ that is statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to the pipeline
--     bind point used by this command, a descriptor set /must/ have been
--     bound to /n/ at the same pipeline bind point, with a
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' that is
--     compatible for set /n/, with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' used to create
--     the current 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline', as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   For each push constant that is statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to the pipeline
--     bind point used by this command, a push constant value /must/ have
--     been set for the same pipeline bind point, with a
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' that is
--     compatible for push constants, with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' used to create
--     the current 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline', as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   Descriptors in each bound descriptor set, specified via
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindDescriptorSets',
--     /must/ be valid if they are statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to the pipeline
--     bind point used by this command
--
-- -   A valid pipeline /must/ be bound to the pipeline bind point used by
--     this command
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command requires any dynamic
--     state, that state /must/ have been set for @commandBuffer@
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used to sample
--     from any 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' with a
--     'Graphics.Vulkan.C.Core10.ImageView.VkImageView' of the type
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_3D',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_CUBE',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_1D_ARRAY',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_2D_ARRAY' or
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_CUBE_ARRAY',
--     in any shader stage
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions with @ImplicitLod@, @Dref@ or @Proj@ in their name, in
--     any shader stage
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions that includes a LOD bias or any offset values, in any
--     shader stage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound to the
--     pipeline bind point used by this command accesses a uniform buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound to the
--     pipeline bind point used by this command accesses a storage buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   The current render pass /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the @renderPass@ member of the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo'
--     structure specified when creating the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS'.
--
-- -   The subpass index of the current render pass /must/ be equal to the
--     @subpass@ member of the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo'
--     structure specified when creating the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS'.
--
-- -   Every input attachment used by the current subpass /must/ be bound
--     to the pipeline via a descriptor set
--
-- -   Image subresources used as attachments in the current render pass
--     /must/ not be accessed in any way other than as an attachment by
--     this command.
--
-- -   @taskCount@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VkPhysicalDeviceMeshShaderPropertiesNV'::@maxDrawMeshTasksCount@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- -   This command /must/ only be called inside of a render pass instance
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Inside          | Graphics        | Graphics        |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer'
cmdDrawMeshTasksNV :: CommandBuffer ->  Word32 ->  Word32 ->  IO ()
cmdDrawMeshTasksNV = \(CommandBuffer commandBuffer' commandTable) -> \taskCount' -> \firstTask' -> vkCmdDrawMeshTasksNV commandTable commandBuffer' taskCount' firstTask' *> (pure ())

-- No documentation found for TopLevel "VK_NV_MESH_SHADER_EXTENSION_NAME"
pattern NV_MESH_SHADER_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NV_MESH_SHADER_EXTENSION_NAME = VK_NV_MESH_SHADER_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NV_MESH_SHADER_SPEC_VERSION"
pattern NV_MESH_SHADER_SPEC_VERSION :: Integral a => a
pattern NV_MESH_SHADER_SPEC_VERSION = VK_NV_MESH_SHADER_SPEC_VERSION
