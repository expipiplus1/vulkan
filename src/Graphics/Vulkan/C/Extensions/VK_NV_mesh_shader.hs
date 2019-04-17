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
  , Zero(..)
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


-- | VkDrawMeshTasksIndirectCommandNV - Structure specifying a mesh tasks
-- draw indirect command
--
-- = Description
--
-- The members of @VkDrawMeshTasksIndirectCommandNV@ have the same meaning
-- as the similarly named parameters of 'vkCmdDrawMeshTasksNV'.
--
-- == Valid Usage
--
-- Unresolved directive in VkDrawMeshTasksIndirectCommandNV.txt -
-- include::..\/validity\/structs\/VkDrawMeshTasksIndirectCommandNV.txt[]
--
-- = See Also
--
-- UNKNOWN:vkCmdDrawMeshTasksIndirectNV
data VkDrawMeshTasksIndirectCommandNV = VkDrawMeshTasksIndirectCommandNV
  { -- | @taskCount@ /must/ be less than or equal to
  -- @VkPhysicalDeviceMeshShaderPropertiesNV@::@maxDrawMeshTasksCount@
  vkTaskCount :: Word32
  , -- | @firstTask@ is the X component of the first workgroup ID.
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

instance Zero VkDrawMeshTasksIndirectCommandNV where
  zero = VkDrawMeshTasksIndirectCommandNV zero
                                          zero
-- | VkPhysicalDeviceMeshShaderFeaturesNV - Structure describing mesh shading
-- features that can be supported by an implementation
--
-- = Description
--
-- If the @VkPhysicalDeviceMeshShaderFeaturesNV@ structure is included in
-- the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2',
-- it is filled with a value indicating whether the feature is supported.
-- @VkPhysicalDeviceMeshShaderFeaturesNV@ /can/ also be used in @pNext@
-- chain of 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to enable
-- the features.
--
-- Unresolved directive in VkPhysicalDeviceMeshShaderFeaturesNV.txt -
-- include::..\/validity\/structs\/VkPhysicalDeviceMeshShaderFeaturesNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkPhysicalDeviceMeshShaderFeaturesNV = VkPhysicalDeviceMeshShaderFeaturesNV
  { -- No documentation found for Nested "VkPhysicalDeviceMeshShaderFeaturesNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceMeshShaderFeaturesNV" "pNext"
  vkPNext :: Ptr ()
  , -- | @taskShader@ indicates whether the task shader stage is supported.
  vkTaskShader :: VkBool32
  , -- | @meshShader@ indicates whether the mesh shader stage is supported.
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

instance Zero VkPhysicalDeviceMeshShaderFeaturesNV where
  zero = VkPhysicalDeviceMeshShaderFeaturesNV zero
                                              zero
                                              zero
                                              zero
-- | VkPhysicalDeviceMeshShaderPropertiesNV - Structure describing mesh
-- shading properties
--
-- = Members
--
-- The members of the @VkPhysicalDeviceMeshShaderPropertiesNV@ structure
-- describe the following implementation-dependent limits:
--
-- = Description
--
-- If the @VkPhysicalDeviceMeshShaderPropertiesNV@ structure is included in
-- the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- Unresolved directive in VkPhysicalDeviceMeshShaderPropertiesNV.txt -
-- include::..\/validity\/structs\/VkPhysicalDeviceMeshShaderPropertiesNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkPhysicalDeviceMeshShaderPropertiesNV = VkPhysicalDeviceMeshShaderPropertiesNV
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @maxDrawMeshTasksCount@ is the maximum number of local workgroups that
  -- /can/ be launched by a single draw mesh tasks command. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#drawing-mesh-shading {html_spec_relative}#drawing-mesh-shading>.
  vkMaxDrawMeshTasksCount :: Word32
  , -- | @maxTaskWorkGroupInvocations@ is the maximum total number of task shader
  -- invocations in a single local workgroup. The product of the X, Y, and Z
  -- sizes as specified by the @LocalSize@ execution mode in shader modules
  -- and by the object decorated by the @WorkgroupSize@ decoration /must/ be
  -- less than or equal to this limit.
  vkMaxTaskWorkGroupInvocations :: Word32
  , -- | @maxTaskWorkGroupSize@[3] is the maximum size of a local task workgroup.
  -- These three values represent the maximum local workgroup size in the X,
  -- Y, and Z dimensions, respectively. The @x@, @y@, and @z@ sizes specified
  -- by the @LocalSize@ execution mode and by the object decorated by the
  -- @WorkgroupSize@ decoration in shader modules /must/ be less than or
  -- equal to the corresponding limit.
  vkMaxTaskWorkGroupSize :: Vector 3 Word32
  , -- | @maxTaskTotalMemorySize@ is the maximum number of bytes that the task
  -- shader can use in total for shared and output memory combined.
  vkMaxTaskTotalMemorySize :: Word32
  , -- | @maxTaskOutputCount@ is the maximum number of output tasks a single task
  -- shader workgroup can emit.
  vkMaxTaskOutputCount :: Word32
  , -- | @maxMeshWorkGroupInvocations@ is the maximum total number of mesh shader
  -- invocations in a single local workgroup. The product of the X, Y, and Z
  -- sizes as specified by the @LocalSize@ execution mode in shader modules
  -- and by the object decorated by the @WorkgroupSize@ decoration /must/ be
  -- less than or equal to this limit.
  vkMaxMeshWorkGroupInvocations :: Word32
  , -- | @maxMeshWorkGroupSize@[3] is the maximum size of a local mesh workgroup.
  -- These three values represent the maximum local workgroup size in the X,
  -- Y, and Z dimensions, respectively. The @x@, @y@, and @z@ sizes specified
  -- by the @LocalSize@ execution mode and by the object decorated by the
  -- @WorkgroupSize@ decoration in shader modules /must/ be less than or
  -- equal to the corresponding limit.
  vkMaxMeshWorkGroupSize :: Vector 3 Word32
  , -- | @maxMeshTotalMemorySize@ is the maximum number of bytes that the mesh
  -- shader can use in total for shared and output memory combined.
  vkMaxMeshTotalMemorySize :: Word32
  , -- | @maxMeshOutputVertices@ is the maximum number of vertices a mesh shader
  -- output can store.
  vkMaxMeshOutputVertices :: Word32
  , -- | @maxMeshOutputPrimitives@ is the maximum number of primitives a mesh
  -- shader output can store.
  vkMaxMeshOutputPrimitives :: Word32
  , -- | @maxMeshMultiviewViewCount@ is the maximum number of multi-view views a
  -- mesh shader can use.
  vkMaxMeshMultiviewViewCount :: Word32
  , -- | @meshOutputPerVertexGranularity@ is the granularity with which mesh
  -- vertex outputs are allocated. The value can be used to compute the
  -- memory size used by the mesh shader, which must be less than or equal to
  -- @maxMeshTotalMemorySize@.
  vkMeshOutputPerVertexGranularity :: Word32
  , -- | @meshOutputPerPrimitiveGranularity@ is the granularity with which mesh
  -- outputs qualified as per-primitive are allocated. The value can be used
  -- to compute the memory size used by the mesh shader, which must be less
  -- than or equal to @maxMeshTotalMemorySize@.
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

instance Zero VkPhysicalDeviceMeshShaderPropertiesNV where
  zero = VkPhysicalDeviceMeshShaderPropertiesNV zero
                                                zero
                                                zero
                                                zero
                                                zero
                                                zero
                                                zero
                                                zero
                                                zero
                                                zero
                                                zero
                                                zero
                                                zero
                                                zero
                                                zero
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
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
-- @vkCmdDrawMeshTasksIndirectCountNV@ behaves similarly to
-- 'vkCmdDrawMeshTasksIndirectNV' except that the draw count is read by the
-- device from a buffer during execution. The command will read an unsigned
-- 32-bit integer from @countBuffer@ located at @countBufferOffset@ and use
-- this as the draw count.
--
-- == Valid Usage
--
-- -   If @buffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single @VkDeviceMemory@ object
--
-- -   @buffer@ /must/ have been created with the
--     @VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT@ bit set
--
-- -   If @countBuffer@ is non-sparse then it /must/ be bound completely
--     and contiguously to a single @VkDeviceMemory@ object
--
-- -   @countBuffer@ /must/ have been created with the
--     @VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT@ bit set
--
-- -   @offset@ /must/ be a multiple of @4@
--
-- -   @countBufferOffset@ /must/ be a multiple of @4@
--
-- -   @stride@ /must/ be a multiple of @4@ and /must/ be greater than or
--     equal to @sizeof@(@VkDrawMeshTasksIndirectCommandNV@)
--
-- -   If @maxDrawCount@ is greater than or equal to @1@, (@stride@ ×
--     (@maxDrawCount@ - 1) + @offset@ +
--     @sizeof@(@VkDrawMeshTasksIndirectCommandNV@)) /must/ be less than or
--     equal to the size of @buffer@
--
-- -   The current render pass /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the @renderPass@ member of the @VkGraphicsPipelineCreateInfo@
--     structure specified when creating the @VkPipeline@ currently bound
--     to @VK_PIPELINE_BIND_POINT_GRAPHICS@.
--
-- -   The subpass index of the current render pass /must/ be equal to the
--     @subpass@ member of the @VkGraphicsPipelineCreateInfo@ structure
--     specified when creating the @VkPipeline@ currently bound to
--     @VK_PIPELINE_BIND_POINT_GRAPHICS@.
--
-- -   For each set /n/ that is statically used by the @VkPipeline@
--     currently bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@, a descriptor
--     set /must/ have been bound to /n/ at
--     @VK_PIPELINE_BIND_POINT_GRAPHICS@, with a @VkPipelineLayout@ that is
--     compatible for set /n/, with the @VkPipelineLayout@ used to create
--     the current @VkPipeline@, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility {html_spec_relative}#descriptorsets-compatibility>
--
-- -   For each push constant that is statically used by the @VkPipeline@
--     currently bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@, a push
--     constant value /must/ have been set for
--     @VK_PIPELINE_BIND_POINT_GRAPHICS@, with a @VkPipelineLayout@ that is
--     compatible for push constants, with the @VkPipelineLayout@ used to
--     create the current @VkPipeline@, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility {html_spec_relative}#descriptorsets-compatibility>
--
-- -   Descriptors in each bound descriptor set, specified via
--     @vkCmdBindDescriptorSets@, /must/ be valid if they are statically
--     used by the currently bound @VkPipeline@ object, specified via
--     @vkCmdBindPipeline@
--
-- -   A valid graphics pipeline /must/ be bound to the current command
--     buffer with @VK_PIPELINE_BIND_POINT_GRAPHICS@
--
-- -   If the @VkPipeline@ object currently bound to
--     @VK_PIPELINE_BIND_POINT_GRAPHICS@ requires any dynamic state, that
--     state /must/ have been set on the current command buffer
--
-- -   If the count stored in @countBuffer@ is equal to @1@, (@offset@ +
--     @sizeof@(@VkDrawMeshTasksIndirectCommandNV@)) /must/ be less than or
--     equal to the size of @buffer@
--
-- -   If the count stored in @countBuffer@ is greater than @1@, (@stride@
--     × (@drawCount@ - 1) + @offset@ +
--     @sizeof@(@VkDrawMeshTasksIndirectCommandNV@)) /must/ be less than or
--     equal to the size of @buffer@
--
-- -   The count stored in @countBuffer@ /must/ be less than or equal to
--     @VkPhysicalDeviceLimits@::@maxDrawIndirectCount@
--
-- -   Every input attachment used by the current subpass /must/ be bound
--     to the pipeline via a descriptor set
--
-- -   If any @VkSampler@ object that is accessed from a shader by the
--     @VkPipeline@ currently bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@
--     uses unnormalized coordinates, it /must/ not be used to sample from
--     any @VkImage@ with a @VkImageView@ of the type
--     @VK_IMAGE_VIEW_TYPE_3D@, @VK_IMAGE_VIEW_TYPE_CUBE@,
--     @VK_IMAGE_VIEW_TYPE_1D_ARRAY@, @VK_IMAGE_VIEW_TYPE_2D_ARRAY@ or
--     @VK_IMAGE_VIEW_TYPE_CUBE_ARRAY@, in any shader stage
--
-- -   If any @VkSampler@ object that is accessed from a shader by the
--     @VkPipeline@ currently bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@
--     uses unnormalized coordinates, it /must/ not be used with any of the
--     SPIR-V @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name, in any shader stage
--
-- -   If any @VkSampler@ object that is accessed from a shader by the
--     @VkPipeline@ currently bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@
--     uses unnormalized coordinates, it /must/ not be used with any of the
--     SPIR-V @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values, in any shader stage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and any shader stage in the @VkPipeline@
--     object currently bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ accesses
--     a uniform buffer, it /must/ not access values outside of the range
--     of that buffer specified in the currently bound descriptor set
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and any shader stage in the @VkPipeline@
--     object currently bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ accesses
--     a storage buffer, it /must/ not access values outside of the range
--     of that buffer specified in the currently bound descriptor set
--
-- -   Any @VkImageView@ being sampled with @VK_FILTER_LINEAR@ as a result
--     of this command /must/ be of a format which supports linear
--     filtering, as specified by the
--     @VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT@ flag in
--     @VkFormatProperties@::@linearTilingFeatures@ (for a linear image) or
--     @VkFormatProperties@::@optimalTilingFeatures@(for an optimally tiled
--     image) returned by @vkGetPhysicalDeviceFormatProperties@
--
-- -   Image subresources used as attachments in the current render pass
--     /must/ not be accessed in any way other than as an attachment by
--     this command.
--
-- -   If the draw is recorded in a render pass instance with multiview
--     enabled, the maximum instance index /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VkPhysicalDeviceMultiviewProperties'::@maxMultiviewInstanceIndex@.
--
-- -   If the currently bound graphics pipeline was created with
--     'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.VkPipelineSampleLocationsStateCreateInfoEXT'::@sampleLocationsEnable@
--     set to @VK_TRUE@ and the current subpass has a depth\/stencil
--     attachment, then that attachment /must/ have been created with the
--     @VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT@ bit set
--
-- Unresolved directive in vkCmdDrawMeshTasksIndirectCountNV.txt -
-- include::..\/validity\/protos\/vkCmdDrawMeshTasksIndirectCountNV.txt[]
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdDrawMeshTasksIndirectCountNV" vkCmdDrawMeshTasksIndirectCountNV :: ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()

#endif
type FN_vkCmdDrawMeshTasksIndirectCountNV = ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()
type PFN_vkCmdDrawMeshTasksIndirectCountNV = FunPtr FN_vkCmdDrawMeshTasksIndirectCountNV
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
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
-- @vkCmdDrawMeshTasksIndirectNV@ behaves similarly to
-- 'vkCmdDrawMeshTasksNV' except that the parameters are read by the device
-- from a buffer during execution. @drawCount@ draws are executed by the
-- command, with parameters taken from @buffer@ starting at @offset@ and
-- increasing by @stride@ bytes for each successive draw. The parameters of
-- each draw are encoded in an array of 'VkDrawMeshTasksIndirectCommandNV'
-- structures. If @drawCount@ is less than or equal to one, @stride@ is
-- ignored.
--
-- == Valid Usage
--
-- -   If @buffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single @VkDeviceMemory@ object
--
-- -   @buffer@ /must/ have been created with the
--     @VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT@ bit set
--
-- -   @offset@ /must/ be a multiple of @4@
--
-- -   If @drawCount@ is greater than @1@, @stride@ /must/ be a multiple of
--     @4@ and /must/ be greater than or equal to
--     @sizeof@(@VkDrawMeshTasksIndirectCommandNV@)
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-multiDrawIndirect multi-draw indirect>
--     feature is not enabled, @drawCount@ /must/ be @0@ or @1@
--
-- -   The current render pass /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the @renderPass@ member of the @VkGraphicsPipelineCreateInfo@
--     structure specified when creating the @VkPipeline@ currently bound
--     to @VK_PIPELINE_BIND_POINT_GRAPHICS@.
--
-- -   The subpass index of the current render pass /must/ be equal to the
--     @subpass@ member of the @VkGraphicsPipelineCreateInfo@ structure
--     specified when creating the @VkPipeline@ currently bound to
--     @VK_PIPELINE_BIND_POINT_GRAPHICS@.
--
-- -   For each set /n/ that is statically used by the @VkPipeline@
--     currently bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@, a descriptor
--     set /must/ have been bound to /n/ at
--     @VK_PIPELINE_BIND_POINT_GRAPHICS@, with a @VkPipelineLayout@ that is
--     compatible for set /n/, with the @VkPipelineLayout@ used to create
--     the current @VkPipeline@, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility {html_spec_relative}#descriptorsets-compatibility>
--
-- -   For each push constant that is statically used by the @VkPipeline@
--     currently bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@, a push
--     constant value /must/ have been set for
--     @VK_PIPELINE_BIND_POINT_GRAPHICS@, with a @VkPipelineLayout@ that is
--     compatible for push constants, with the @VkPipelineLayout@ used to
--     create the current @VkPipeline@, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility {html_spec_relative}#descriptorsets-compatibility>
--
-- -   Descriptors in each bound descriptor set, specified via
--     @vkCmdBindDescriptorSets@, /must/ be valid if they are statically
--     used by the currently bound @VkPipeline@ object, specified via
--     @vkCmdBindPipeline@
--
-- -   All vertex input bindings accessed via vertex input variables
--     declared in the vertex shader entry point’s interface /must/ have
--     valid buffers bound
--
-- -   A valid graphics pipeline /must/ be bound to the current command
--     buffer with @VK_PIPELINE_BIND_POINT_GRAPHICS@
--
-- -   If the @VkPipeline@ object currently bound to
--     @VK_PIPELINE_BIND_POINT_GRAPHICS@ requires any dynamic state, that
--     state /must/ have been set on the current command buffer
--
-- -   If @drawCount@ is equal to @1@, (@offset@ +
--     @sizeof@('VkDrawMeshTasksIndirectCommandNV')) /must/ be less than or
--     equal to the size of @buffer@
--
-- -   If @drawCount@ is greater than @1@, (@stride@ × (@drawCount@ - 1) +
--     @offset@ + @sizeof@('VkDrawMeshTasksIndirectCommandNV')) /must/ be
--     less than or equal to the size of @buffer@
--
-- -   @drawCount@ /must/ be less than or equal to
--     @VkPhysicalDeviceLimits@::@maxDrawIndirectCount@
--
-- -   Every input attachment used by the current subpass /must/ be bound
--     to the pipeline via a descriptor set
--
-- -   If any @VkSampler@ object that is accessed from a shader by the
--     @VkPipeline@ currently bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@
--     uses unnormalized coordinates, it /must/ not be used to sample from
--     any @VkImage@ with a @VkImageView@ of the type
--     @VK_IMAGE_VIEW_TYPE_3D@, @VK_IMAGE_VIEW_TYPE_CUBE@,
--     @VK_IMAGE_VIEW_TYPE_1D_ARRAY@, @VK_IMAGE_VIEW_TYPE_2D_ARRAY@ or
--     @VK_IMAGE_VIEW_TYPE_CUBE_ARRAY@, in any shader stage
--
-- -   If any @VkSampler@ object that is accessed from a shader by the
--     @VkPipeline@ currently bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@
--     uses unnormalized coordinates, it /must/ not be used with any of the
--     SPIR-V @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name, in any shader stage
--
-- -   If any @VkSampler@ object that is accessed from a shader by the
--     @VkPipeline@ currently bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@
--     uses unnormalized coordinates, it /must/ not be used with any of the
--     SPIR-V @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values, in any shader stage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and any shader stage in the @VkPipeline@
--     object currently bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ accesses
--     a uniform buffer, it /must/ not access values outside of the range
--     of that buffer specified in the currently bound descriptor set
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and any shader stage in the @VkPipeline@
--     object currently bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ accesses
--     a storage buffer, it /must/ not access values outside of the range
--     of that buffer specified in the currently bound descriptor set
--
-- -   Any @VkImageView@ being sampled with @VK_FILTER_LINEAR@ as a result
--     of this command /must/ be of a format which supports linear
--     filtering, as specified by the
--     @VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT@ flag in
--     @VkFormatProperties@::@linearTilingFeatures@ (for a linear image) or
--     @VkFormatProperties@::@optimalTilingFeatures@(for an optimally tiled
--     image) returned by @vkGetPhysicalDeviceFormatProperties@
--
-- -   Image subresources used as attachments in the current render pass
--     /must/ not be accessed in any way other than as an attachment by
--     this command.
--
-- -   Any 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' being sampled
--     with @VK_FILTER_CUBIC_IMG@ as a result of this command /must/ be of
--     a format which supports cubic filtering, as specified by the
--     @VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG@ flag in
--     @VkFormatProperties@::@linearTilingFeatures@ (for a linear image) or
--     @VkFormatProperties@::@optimalTilingFeatures@(for an optimally tiled
--     image) returned by @vkGetPhysicalDeviceFormatProperties@
--
-- -   Any 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' being sampled
--     with @VK_FILTER_CUBIC_IMG@ as a result of this command /must/ not
--     have a 'Graphics.Vulkan.C.Core10.ImageView.VkImageViewType' of
--     @VK_IMAGE_VIEW_TYPE_3D@, @VK_IMAGE_VIEW_TYPE_CUBE@, or
--     @VK_IMAGE_VIEW_TYPE_CUBE_ARRAY@
--
-- -   If the draw is recorded in a render pass instance with multiview
--     enabled, the maximum instance index /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VkPhysicalDeviceMultiviewProperties'::@maxMultiviewInstanceIndex@.
--
-- -   If the currently bound graphics pipeline was created with
--     'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.VkPipelineSampleLocationsStateCreateInfoEXT'::@sampleLocationsEnable@
--     set to @VK_TRUE@ and the current subpass has a depth\/stencil
--     attachment, then that attachment /must/ have been created with the
--     @VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT@ bit set
--
-- -   Any 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' created with
--     a 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@flags@
--     containing @VK_IMAGE_CREATE_CORNER_SAMPLED_BIT_NV@ sampled as a
--     result of this command /must/ only be sampled using a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSamplerAddressMode' of
--     @VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE@.
--
-- Unresolved directive in vkCmdDrawMeshTasksIndirectNV.txt -
-- include::..\/validity\/protos\/vkCmdDrawMeshTasksIndirectNV.txt[]
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdDrawMeshTasksIndirectNV" vkCmdDrawMeshTasksIndirectNV :: ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()

#endif
type FN_vkCmdDrawMeshTasksIndirectNV = ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()
type PFN_vkCmdDrawMeshTasksIndirectNV = FunPtr FN_vkCmdDrawMeshTasksIndirectNV
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
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
-- -   @taskCount@ /must/ be less than or equal to
--     @VkPhysicalDeviceMeshShaderPropertiesNV@::@maxDrawMeshTasksCount@
--
-- -   The current render pass /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the @renderPass@ member of the @VkGraphicsPipelineCreateInfo@
--     structure specified when creating the @VkPipeline@ currently bound
--     to @VK_PIPELINE_BIND_POINT_GRAPHICS@.
--
-- -   The subpass index of the current render pass /must/ be equal to the
--     @subpass@ member of the @VkGraphicsPipelineCreateInfo@ structure
--     specified when creating the @VkPipeline@ currently bound to
--     @VK_PIPELINE_BIND_POINT_GRAPHICS@.
--
-- -   For each set /n/ that is statically used by the @VkPipeline@
--     currently bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@, a descriptor
--     set /must/ have been bound to /n/ at
--     @VK_PIPELINE_BIND_POINT_GRAPHICS@, with a @VkPipelineLayout@ that is
--     compatible for set /n/, with the @VkPipelineLayout@ used to create
--     the current @VkPipeline@, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility {html_spec_relative}#descriptorsets-compatibility>
--
-- -   For each push constant that is statically used by the @VkPipeline@
--     currently bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@, a push
--     constant value /must/ have been set for
--     @VK_PIPELINE_BIND_POINT_GRAPHICS@, with a @VkPipelineLayout@ that is
--     compatible for push constants, with the @VkPipelineLayout@ used to
--     create the current @VkPipeline@, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility {html_spec_relative}#descriptorsets-compatibility>
--
-- -   Descriptors in each bound descriptor set, specified via
--     @vkCmdBindDescriptorSets@, /must/ be valid if they are statically
--     used by the currently bound @VkPipeline@ object, specified via
--     @vkCmdBindPipeline@
--
-- -   A valid graphics pipeline /must/ be bound to the current command
--     buffer with @VK_PIPELINE_BIND_POINT_GRAPHICS@
--
-- -   If the @VkPipeline@ object currently bound to
--     @VK_PIPELINE_BIND_POINT_GRAPHICS@ requires any dynamic state, that
--     state /must/ have been set on the current command buffer
--
-- -   Every input attachment used by the current subpass /must/ be bound
--     to the pipeline via a descriptor set
--
-- -   If any @VkSampler@ object that is accessed from a shader by the
--     @VkPipeline@ currently bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@
--     uses unnormalized coordinates, it /must/ not be used to sample from
--     any @VkImage@ with a @VkImageView@ of the type
--     @VK_IMAGE_VIEW_TYPE_3D@, @VK_IMAGE_VIEW_TYPE_CUBE@,
--     @VK_IMAGE_VIEW_TYPE_1D_ARRAY@, @VK_IMAGE_VIEW_TYPE_2D_ARRAY@ or
--     @VK_IMAGE_VIEW_TYPE_CUBE_ARRAY@, in any shader stage
--
-- -   If any @VkSampler@ object that is accessed from a shader by the
--     @VkPipeline@ currently bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@
--     uses unnormalized coordinates, it /must/ not be used with any of the
--     SPIR-V @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name, in any shader stage
--
-- -   If any @VkSampler@ object that is accessed from a shader by the
--     @VkPipeline@ currently bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@
--     uses unnormalized coordinates, it /must/ not be used with any of the
--     SPIR-V @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values, in any shader stage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and any shader stage in the @VkPipeline@
--     object currently bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ accesses
--     a uniform buffer, it /must/ not access values outside of the range
--     of that buffer specified in the currently bound descriptor set
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and any shader stage in the @VkPipeline@
--     object currently bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ accesses
--     a storage buffer, it /must/ not access values outside of the range
--     of that buffer specified in the currently bound descriptor set
--
-- -   Any @VkImageView@ being sampled with @VK_FILTER_LINEAR@ as a result
--     of this command /must/ be of a format which supports linear
--     filtering, as specified by the
--     @VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT@ flag in
--     @VkFormatProperties@::@linearTilingFeatures@ (for a linear image) or
--     @VkFormatProperties@::@optimalTilingFeatures@(for an optimally tiled
--     image) returned by @vkGetPhysicalDeviceFormatProperties@
--
-- -   Image subresources used as attachments in the current render pass
--     /must/ not be accessed in any way other than as an attachment by
--     this command.
--
-- -   Any 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' being sampled
--     with @VK_FILTER_CUBIC_IMG@ as a result of this command /must/ be of
--     a format which supports cubic filtering, as specified by the
--     @VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG@ flag in
--     @VkFormatProperties@::@linearTilingFeatures@ (for a linear image) or
--     @VkFormatProperties@::@optimalTilingFeatures@(for an optimally tiled
--     image) returned by @vkGetPhysicalDeviceFormatProperties@
--
-- -   Any 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' being sampled
--     with @VK_FILTER_CUBIC_IMG@ as a result of this command /must/ not
--     have a 'Graphics.Vulkan.C.Core10.ImageView.VkImageViewType' of
--     @VK_IMAGE_VIEW_TYPE_3D@, @VK_IMAGE_VIEW_TYPE_CUBE@, or
--     @VK_IMAGE_VIEW_TYPE_CUBE_ARRAY@
--
-- -   If the draw is recorded in a render pass instance with multiview
--     enabled, the maximum instance index /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VkPhysicalDeviceMultiviewProperties'::@maxMultiviewInstanceIndex@.
--
-- -   If the currently bound graphics pipeline was created with
--     'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.VkPipelineSampleLocationsStateCreateInfoEXT'::@sampleLocationsEnable@
--     set to @VK_TRUE@ and the current subpass has a depth\/stencil
--     attachment, then that attachment /must/ have been created with the
--     @VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT@ bit set
--
-- -   Any 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' created with
--     a 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@flags@
--     containing @VK_IMAGE_CREATE_CORNER_SAMPLED_BIT_NV@ sampled as a
--     result of this command /must/ only be sampled using a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSamplerAddressMode' of
--     @VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE@.
--
-- Unresolved directive in vkCmdDrawMeshTasksNV.txt -
-- include::..\/validity\/protos\/vkCmdDrawMeshTasksNV.txt[]
--
-- = See Also
--
-- No cross-references are available
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
-- | @VK_SHADER_STAGE_MESH_BIT_NV@ specifies the mesh stage.
pattern VK_SHADER_STAGE_MESH_BIT_NV :: VkShaderStageFlagBits
pattern VK_SHADER_STAGE_MESH_BIT_NV = VkShaderStageFlagBits 0x00000080
-- | @VK_SHADER_STAGE_TASK_BIT_NV@ specifies the task stage.
pattern VK_SHADER_STAGE_TASK_BIT_NV :: VkShaderStageFlagBits
pattern VK_SHADER_STAGE_TASK_BIT_NV = VkShaderStageFlagBits 0x00000040
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV = VkStructureType 1000202000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV = VkStructureType 1000202001
