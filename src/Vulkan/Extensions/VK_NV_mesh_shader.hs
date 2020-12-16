{-# language CPP #-}
-- | = Name
--
-- VK_NV_mesh_shader - device extension
--
-- == VK_NV_mesh_shader
--
-- [__Name String__]
--     @VK_NV_mesh_shader@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     203
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Contact__]
--
--     -   Christoph Kubisch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_NV_mesh_shader:%20&body=@pixeljetstream%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-07-19
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_mesh_shader.html SPV_NV_mesh_shader>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/nv/GLSL_NV_mesh_shader.txt GLSL_NV_mesh_shader>
--
-- [__Contributors__]
--
--     -   Pat Brown, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
--     -   Piers Daniell, NVIDIA
--
--     -   Pierre Boudier, NVIDIA
--
-- == Description
--
-- This extension provides a new mechanism allowing applications to
-- generate collections of geometric primitives via programmable mesh
-- shading. It is an alternative to the existing programmable primitive
-- shading pipeline, which relied on generating input primitives by a fixed
-- function assembler as well as fixed function vertex fetch.
--
-- There are new programmable shader types — the task and mesh shader — to
-- generate these collections to be processed by fixed-function primitive
-- assembly and rasterization logic. When the task and mesh shaders are
-- dispatched, they replace the standard programmable vertex processing
-- pipeline, including vertex array attribute fetching, vertex shader
-- processing, tessellation, and the geometry shader processing.
--
-- This extension also adds support for the following SPIR-V extension in
-- Vulkan:
--
-- -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_mesh_shader.html SPV_NV_mesh_shader>
--
-- == New Commands
--
-- -   'cmdDrawMeshTasksIndirectCountNV'
--
-- -   'cmdDrawMeshTasksIndirectNV'
--
-- -   'cmdDrawMeshTasksNV'
--
-- == New Structures
--
-- -   'DrawMeshTasksIndirectCommandNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceMeshShaderFeaturesNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceMeshShaderPropertiesNV'
--
-- == New Enum Constants
--
-- -   'NV_MESH_SHADER_EXTENSION_NAME'
--
-- -   'NV_MESH_SHADER_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_MESH_SHADER_BIT_NV'
--
--     -   'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TASK_SHADER_BIT_NV'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits':
--
--     -   'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_NV'
--
--     -   'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV'
--
-- == New or Modified Built-In Variables
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-taskcount TaskCountNV>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-primitivecount PrimitiveCountNV>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-primitiveindices PrimitiveIndicesNV>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-clipdistancepv ClipDistancePerViewNV>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-culldistancepv CullDistancePerViewNV>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-layerpv LayerPerViewNV>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-meshviewcount MeshViewCountNV>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-meshviewindices MeshViewIndicesNV>
--
-- -   (modified)@Position@
--
-- -   (modified)@PointSize@
--
-- -   (modified)@ClipDistance@
--
-- -   (modified)@CullDistance@
--
-- -   (modified)@PrimitiveId@
--
-- -   (modified)@Layer@
--
-- -   (modified)@ViewportIndex@
--
-- -   (modified)@WorkgroupSize@
--
-- -   (modified)@WorkgroupId@
--
-- -   (modified)@LocalInvocationId@
--
-- -   (modified)@GlobalInvocationId@
--
-- -   (modified)@LocalInvocationIndex@
--
-- -   (modified)@DrawIndex@
--
-- -   (modified)@ViewportMaskNV@
--
-- -   (modified)@PositionPerViewNV@
--
-- -   (modified)@ViewportMaskPerViewNV@
--
-- == New SPIR-V Capability
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-MeshShadingNV MeshShadingNV>
--
-- == Issues
--
-- 1.  How to name this extension?
--
--     RESOLVED: VK_NV_mesh_shader
--
--     Other options considered:
--
--     -   VK_NV_mesh_shading
--
--     -   VK_NV_programmable_mesh_shading
--
--     -   VK_NV_primitive_group_shading
--
--     -   VK_NV_grouped_drawing
--
-- 2.  Do we need a new VkPrimitiveTopology?
--
--     RESOLVED: NO, we skip the InputAssembler stage
--
-- 3.  Should we allow Instancing?
--
--     RESOLVED: NO, there is no fixed function input, other than the IDs.
--     However, allow offsetting with a \"first\" value.
--
-- 4.  Should we use existing vkCmdDraw or introduce new functions?
--
--     RESOLVED: Introduce new functions.
--
--     New functions make it easier to separate from \"programmable
--     primitive shading\" chapter, less \"dual use\" language about
--     existing functions having alternative behavior. The text around the
--     existing \"draws\" is heavily based around emitting vertices.
--
-- 5.  If new functions, how to name?
--
--     RESOLVED: CmdDrawMeshTasks*
--
--     Other options considered:
--
--     -   CmdDrawMeshed
--
--     -   CmdDrawTasked
--
--     -   CmdDrawGrouped
--
-- 6.  Should VK_SHADER_STAGE_ALL_GRAPHICS be updated to include the new
--     stages?
--
--     RESOLVED: No. If an application were to be recompiled with headers
--     that include additional shader stage bits in
--     VK_SHADER_STAGE_ALL_GRAPHICS, then the previously valid application
--     would no longer be valid on implementations that don’t support mesh
--     or task shaders. This means the change would not be backwards
--     compatible. It’s too bad VkShaderStageFlagBits doesn’t have a
--     dedicated \"all supported graphics stages\" bit like
--     VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT, which would have avoided this
--     problem.
--
-- == Version History
--
-- -   Revision 1, 2018-07-19 (Christoph Kubisch, Daniel Koch)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'DrawMeshTasksIndirectCommandNV', 'PhysicalDeviceMeshShaderFeaturesNV',
-- 'PhysicalDeviceMeshShaderPropertiesNV',
-- 'cmdDrawMeshTasksIndirectCountNV', 'cmdDrawMeshTasksIndirectNV',
-- 'cmdDrawMeshTasksNV'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_mesh_shader Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_mesh_shader  ( cmdDrawMeshTasksNV
                                            , cmdDrawMeshTasksIndirectNV
                                            , cmdDrawMeshTasksIndirectCountNV
                                            , PhysicalDeviceMeshShaderFeaturesNV(..)
                                            , PhysicalDeviceMeshShaderPropertiesNV(..)
                                            , DrawMeshTasksIndirectCommandNV(..)
                                            , NV_MESH_SHADER_SPEC_VERSION
                                            , pattern NV_MESH_SHADER_SPEC_VERSION
                                            , NV_MESH_SHADER_EXTENSION_NAME
                                            , pattern NV_MESH_SHADER_EXTENSION_NAME
                                            ) where

import Vulkan.CStruct.Utils (FixedArray)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Buffer)
import Vulkan.Core10.Handles (Buffer(..))
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdDrawMeshTasksIndirectCountNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdDrawMeshTasksIndirectNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdDrawMeshTasksNV))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDrawMeshTasksNV
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> IO ()

-- | vkCmdDrawMeshTasksNV - Draw mesh task work items
--
-- = Description
--
-- When the command is executed, a global workgroup consisting of
-- @taskCount@ local workgroups is assembled.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdDrawMeshTasksNV-magFilter-04553# If a
--     'Vulkan.Core10.Handles.Sampler' created with @magFilter@ or
--     @minFilter@ equal to 'Vulkan.Core10.Enums.Filter.FILTER_LINEAR' and
--     @compareEnable@ equal to 'Vulkan.Core10.FundamentalTypes.FALSE' is
--     used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-vkCmdDrawMeshTasksNV-None-02691# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using atomic
--     operations as a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   #VUID-vkCmdDrawMeshTasksNV-None-02692# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   #VUID-vkCmdDrawMeshTasksNV-filterCubic-02694# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering, as specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdDrawMeshTasksNV-filterCubicMinmax-02695# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' with a
--     reduction mode of either
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MIN'
--     or
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MAX'
--     as a result of this command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering together with minmax filtering, as
--     specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubicMinmax@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdDrawMeshTasksNV-flags-02696# Any
--     'Vulkan.Core10.Handles.Image' created with a
--     'Vulkan.Core10.Image.ImageCreateInfo'::@flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode' of
--     'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'
--
-- -   #VUID-vkCmdDrawMeshTasksNV-None-02697# For each set /n/ that is
--     statically used by the 'Vulkan.Core10.Handles.Pipeline' bound to the
--     pipeline bind point used by this command, a descriptor set /must/
--     have been bound to /n/ at the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is compatible for set
--     /n/, with the 'Vulkan.Core10.Handles.PipelineLayout' used to create
--     the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdDrawMeshTasksNV-None-02698# For each push constant that
--     is statically used by the 'Vulkan.Core10.Handles.Pipeline' bound to
--     the pipeline bind point used by this command, a push constant value
--     /must/ have been set for the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is compatible for push
--     constants, with the 'Vulkan.Core10.Handles.PipelineLayout' used to
--     create the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdDrawMeshTasksNV-None-02699# Descriptors in each bound
--     descriptor set, specified via
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets', /must/
--     be valid if they are statically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command
--
-- -   #VUID-vkCmdDrawMeshTasksNV-None-02700# A valid pipeline /must/ be
--     bound to the pipeline bind point used by this command
--
-- -   #VUID-vkCmdDrawMeshTasksNV-commandBuffer-02701# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command requires any dynamic state, that state
--     /must/ have been set for @commandBuffer@, and done so after any
--     previously bound pipeline with the corresponding state not specified
--     as dynamic
--
-- -   #VUID-vkCmdDrawMeshTasksNV-None-02859# There /must/ not have been
--     any calls to dynamic state setting commands for any state not
--     specified as dynamic in the 'Vulkan.Core10.Handles.Pipeline' object
--     bound to the pipeline bind point used by this command, since that
--     pipeline was bound
--
-- -   #VUID-vkCmdDrawMeshTasksNV-None-02702# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used to sample from any
--     'Vulkan.Core10.Handles.Image' with a
--     'Vulkan.Core10.Handles.ImageView' of the type
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_3D',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_1D_ARRAY',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY' or
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY', in
--     any shader stage
--
-- -   #VUID-vkCmdDrawMeshTasksNV-None-02703# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name, in any shader stage
--
-- -   #VUID-vkCmdDrawMeshTasksNV-None-02704# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values, in any shader stage
--
-- -   #VUID-vkCmdDrawMeshTasksNV-None-02705# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a uniform buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   #VUID-vkCmdDrawMeshTasksNV-None-02706# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a storage buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   #VUID-vkCmdDrawMeshTasksNV-commandBuffer-02707# If @commandBuffer@
--     is an unprotected command buffer, any resource accessed by the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command /must/ not be a protected resource
--
-- -   #VUID-vkCmdDrawMeshTasksNV-None-04115# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     image view’s format.
--
-- -   #VUID-vkCmdDrawMeshTasksNV-OpImageWrite-04469# If a
--     'Vulkan.Core10.Handles.BufferView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     image view’s format.
--
-- -   #VUID-vkCmdDrawMeshTasksNV-SampledType-04470# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit channel width
--     is accessed as a result of this command, the @SampledType@ of the
--     @OpTypeImage@ operand of that instruction /must/ have a @Width@ of
--     64.
--
-- -   #VUID-vkCmdDrawMeshTasksNV-SampledType-04471# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a channel width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32.
--
-- -   #VUID-vkCmdDrawMeshTasksNV-SampledType-04472# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit channel width
--     is accessed as a result of this command, the @SampledType@ of the
--     @OpTypeImage@ operand of that instruction /must/ have a @Width@ of
--     64.
--
-- -   #VUID-vkCmdDrawMeshTasksNV-SampledType-04473# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a channel width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32.
--
-- -   #VUID-vkCmdDrawMeshTasksNV-sparseImageInt64Atomics-04474# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Image' objects
--     created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command.
--
-- -   #VUID-vkCmdDrawMeshTasksNV-sparseImageInt64Atomics-04475# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Buffer' objects
--     created with the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command.
--
-- -   #VUID-vkCmdDrawMeshTasksNV-renderPass-02684# The current render pass
--     /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the @renderPass@ member of the
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo' structure
--     specified when creating the 'Vulkan.Core10.Handles.Pipeline' bound
--     to
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
--
-- -   #VUID-vkCmdDrawMeshTasksNV-subpass-02685# The subpass index of the
--     current render pass /must/ be equal to the @subpass@ member of the
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo' structure
--     specified when creating the 'Vulkan.Core10.Handles.Pipeline' bound
--     to
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
--
-- -   #VUID-vkCmdDrawMeshTasksNV-None-02686# Every input attachment used
--     by the current subpass /must/ be bound to the pipeline via a
--     descriptor set
--
-- -   #VUID-vkCmdDrawMeshTasksNV-None-04584# Image subresources used as
--     attachments in the current render pass /must/ not be accessed in any
--     way other than as an attachment by this command, except for cases
--     involving read-only access to depth\/stencil attachments as
--     described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-attachment-nonattachment Render Pass>
--     chapter
--
-- -   #VUID-vkCmdDrawMeshTasksNV-maxMultiviewInstanceIndex-02688# If the
--     draw is recorded in a render pass instance with multiview enabled,
--     the maximum instance index /must/ be less than or equal to
--     'Vulkan.Core11.Promoted_From_VK_KHR_multiview.PhysicalDeviceMultiviewProperties'::@maxMultiviewInstanceIndex@
--
-- -   #VUID-vkCmdDrawMeshTasksNV-sampleLocationsEnable-02689# If the bound
--     graphics pipeline was created with
--     'Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'::@sampleLocationsEnable@
--     set to 'Vulkan.Core10.FundamentalTypes.TRUE' and the current subpass
--     has a depth\/stencil attachment, then that attachment /must/ have
--     been created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT'
--     bit set
--
-- -   #VUID-vkCmdDrawMeshTasksNV-viewportCount-03417# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @viewportCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ match the
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'::@scissorCount@
--     of the pipeline
--
-- -   #VUID-vkCmdDrawMeshTasksNV-scissorCount-03418# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @scissorCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT'
--     /must/ match the
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'::@viewportCount@
--     of the pipeline
--
-- -   #VUID-vkCmdDrawMeshTasksNV-viewportCount-03419# If the bound
--     graphics pipeline state was created with both the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic states enabled then both
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     and
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @viewportCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ match the @scissorCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT'
--
-- -   #VUID-vkCmdDrawMeshTasksNV-viewportCount-04137# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_NV'
--     dynamic state enabled, then the bound graphics pipeline /must/ have
--     been created with
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.PipelineViewportWScalingStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawMeshTasksNV-viewportCount-04138# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_NV'
--     dynamic states enabled then the @viewportCount@ parameter in the
--     last call to
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.cmdSetViewportWScalingNV'
--     /must/ be greater than or equal to the @viewportCount@ parameter in
--     the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawMeshTasksNV-viewportCount-04139# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV'
--     dynamic state enabled, then the bound graphics pipeline /must/ have
--     been created with
--     'Vulkan.Extensions.VK_NV_shading_rate_image.PipelineViewportShadingRateImageStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawMeshTasksNV-viewportCount-04140# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV'
--     dynamic states enabled then the @viewportCount@ parameter in the
--     last call to
--     'Vulkan.Extensions.VK_NV_shading_rate_image.cmdSetViewportShadingRatePaletteNV'
--     /must/ be greater than or equal to the @viewportCount@ parameter in
--     the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawMeshTasksNV-VkPipelineVieportCreateInfo-04141# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled and an instance of
--     'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV'
--     chained from @VkPipelineVieportCreateInfo@, then the bound graphics
--     pipeline /must/ have been created with
--     'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawMeshTasksNV-VkPipelineVieportCreateInfo-04142# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled and an instance of
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV'
--     chained from @VkPipelineVieportCreateInfo@, then the bound graphics
--     pipeline /must/ have been created with
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV'::@exclusiveScissorCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawMeshTasksNV-primitiveTopology-03420# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PRIMITIVE_TOPOLOGY_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopologyEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @primitiveTopology@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopologyEXT'
--     /must/ be of the same
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-primitive-topology-class topology class>
--     as the pipeline
--     'Vulkan.Core10.Pipeline.PipelineInputAssemblyStateCreateInfo'::@topology@
--     state
--
-- -   #VUID-vkCmdDrawMeshTasksNV-primitiveFragmentShadingRateWithMultipleViewports-04552#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-primitiveFragmentShadingRateWithMultipleViewports primitiveFragmentShadingRateWithMultipleViewports>
--     limit is not supported, the bound graphics pipeline was created with
--     the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, and any of the shader stages of the bound
--     graphics pipeline write to the @PrimitiveShadingRateKHR@ built-in,
--     then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @viewportCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ be @1@
--
-- -   #VUID-vkCmdDrawMeshTasksNV-taskCount-02119# @taskCount@ /must/ be
--     less than or equal to
--     'PhysicalDeviceMeshShaderPropertiesNV'::@maxDrawMeshTasksCount@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdDrawMeshTasksNV-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdDrawMeshTasksNV-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdDrawMeshTasksNV-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdDrawMeshTasksNV-renderpass# This command /must/ only be
--     called inside of a render pass instance
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Inside                                                                                                                 | Graphics                                                                                                              | Graphics                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdDrawMeshTasksNV :: forall io
                    . (MonadIO io)
                   => -- | @commandBuffer@ is the command buffer into which the command will be
                      -- recorded.
                      CommandBuffer
                   -> -- | @taskCount@ is the number of local workgroups to dispatch in the X
                      -- dimension. Y and Z dimension are implicitly set to one.
                      ("taskCount" ::: Word32)
                   -> -- | @firstTask@ is the X component of the first workgroup ID.
                      ("firstTask" ::: Word32)
                   -> io ()
cmdDrawMeshTasksNV commandBuffer taskCount firstTask = liftIO $ do
  let vkCmdDrawMeshTasksNVPtr = pVkCmdDrawMeshTasksNV (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdDrawMeshTasksNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDrawMeshTasksNV is null" Nothing Nothing
  let vkCmdDrawMeshTasksNV' = mkVkCmdDrawMeshTasksNV vkCmdDrawMeshTasksNVPtr
  traceAroundEvent "vkCmdDrawMeshTasksNV" (vkCmdDrawMeshTasksNV' (commandBufferHandle (commandBuffer)) (taskCount) (firstTask))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDrawMeshTasksIndirectNV
  :: FunPtr (Ptr CommandBuffer_T -> Buffer -> DeviceSize -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Buffer -> DeviceSize -> Word32 -> Word32 -> IO ()

-- | vkCmdDrawMeshTasksIndirectNV - Issue an indirect mesh tasks draw into a
-- command buffer
--
-- = Description
--
-- 'cmdDrawMeshTasksIndirectNV' behaves similarly to 'cmdDrawMeshTasksNV'
-- except that the parameters are read by the device from a buffer during
-- execution. @drawCount@ draws are executed by the command, with
-- parameters taken from @buffer@ starting at @offset@ and increasing by
-- @stride@ bytes for each successive draw. The parameters of each draw are
-- encoded in an array of 'DrawMeshTasksIndirectCommandNV' structures. If
-- @drawCount@ is less than or equal to one, @stride@ is ignored.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-magFilter-04553# If a
--     'Vulkan.Core10.Handles.Sampler' created with @magFilter@ or
--     @minFilter@ equal to 'Vulkan.Core10.Enums.Filter.FILTER_LINEAR' and
--     @compareEnable@ equal to 'Vulkan.Core10.FundamentalTypes.FALSE' is
--     used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-None-02691# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using atomic
--     operations as a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-None-02692# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-filterCubic-02694# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering, as specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-filterCubicMinmax-02695# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' with a
--     reduction mode of either
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MIN'
--     or
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MAX'
--     as a result of this command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering together with minmax filtering, as
--     specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubicMinmax@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-flags-02696# Any
--     'Vulkan.Core10.Handles.Image' created with a
--     'Vulkan.Core10.Image.ImageCreateInfo'::@flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode' of
--     'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-None-02697# For each set /n/ that
--     is statically used by the 'Vulkan.Core10.Handles.Pipeline' bound to
--     the pipeline bind point used by this command, a descriptor set
--     /must/ have been bound to /n/ at the same pipeline bind point, with
--     a 'Vulkan.Core10.Handles.PipelineLayout' that is compatible for set
--     /n/, with the 'Vulkan.Core10.Handles.PipelineLayout' used to create
--     the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-None-02698# For each push
--     constant that is statically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command, a push constant value /must/ have been set for
--     the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is compatible for push
--     constants, with the 'Vulkan.Core10.Handles.PipelineLayout' used to
--     create the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-None-02699# Descriptors in each
--     bound descriptor set, specified via
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets', /must/
--     be valid if they are statically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-None-02700# A valid pipeline
--     /must/ be bound to the pipeline bind point used by this command
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-commandBuffer-02701# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command requires any dynamic state, that state
--     /must/ have been set for @commandBuffer@, and done so after any
--     previously bound pipeline with the corresponding state not specified
--     as dynamic
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-None-02859# There /must/ not have
--     been any calls to dynamic state setting commands for any state not
--     specified as dynamic in the 'Vulkan.Core10.Handles.Pipeline' object
--     bound to the pipeline bind point used by this command, since that
--     pipeline was bound
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-None-02702# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used to sample from any
--     'Vulkan.Core10.Handles.Image' with a
--     'Vulkan.Core10.Handles.ImageView' of the type
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_3D',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_1D_ARRAY',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY' or
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY', in
--     any shader stage
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-None-02703# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name, in any shader stage
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-None-02704# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values, in any shader stage
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-None-02705# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a uniform buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-None-02706# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a storage buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-commandBuffer-02707# If
--     @commandBuffer@ is an unprotected command buffer, any resource
--     accessed by the 'Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command /must/ not be a protected
--     resource
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-None-04115# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     image view’s format.
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-OpImageWrite-04469# If a
--     'Vulkan.Core10.Handles.BufferView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     image view’s format.
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-SampledType-04470# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit channel width
--     is accessed as a result of this command, the @SampledType@ of the
--     @OpTypeImage@ operand of that instruction /must/ have a @Width@ of
--     64.
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-SampledType-04471# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a channel width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32.
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-SampledType-04472# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit channel width
--     is accessed as a result of this command, the @SampledType@ of the
--     @OpTypeImage@ operand of that instruction /must/ have a @Width@ of
--     64.
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-SampledType-04473# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a channel width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32.
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-sparseImageInt64Atomics-04474# If
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Image' objects
--     created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command.
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-sparseImageInt64Atomics-04475# If
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Buffer' objects
--     created with the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command.
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-renderPass-02684# The current
--     render pass /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the @renderPass@ member of the
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo' structure
--     specified when creating the 'Vulkan.Core10.Handles.Pipeline' bound
--     to
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-subpass-02685# The subpass index
--     of the current render pass /must/ be equal to the @subpass@ member
--     of the 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo' structure
--     specified when creating the 'Vulkan.Core10.Handles.Pipeline' bound
--     to
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-None-02686# Every input
--     attachment used by the current subpass /must/ be bound to the
--     pipeline via a descriptor set
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-None-04584# Image subresources
--     used as attachments in the current render pass /must/ not be
--     accessed in any way other than as an attachment by this command,
--     except for cases involving read-only access to depth\/stencil
--     attachments as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-attachment-nonattachment Render Pass>
--     chapter
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-maxMultiviewInstanceIndex-02688#
--     If the draw is recorded in a render pass instance with multiview
--     enabled, the maximum instance index /must/ be less than or equal to
--     'Vulkan.Core11.Promoted_From_VK_KHR_multiview.PhysicalDeviceMultiviewProperties'::@maxMultiviewInstanceIndex@
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-sampleLocationsEnable-02689# If
--     the bound graphics pipeline was created with
--     'Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'::@sampleLocationsEnable@
--     set to 'Vulkan.Core10.FundamentalTypes.TRUE' and the current subpass
--     has a depth\/stencil attachment, then that attachment /must/ have
--     been created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT'
--     bit set
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-viewportCount-03417# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @viewportCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ match the
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'::@scissorCount@
--     of the pipeline
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-scissorCount-03418# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @scissorCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT'
--     /must/ match the
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'::@viewportCount@
--     of the pipeline
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-viewportCount-03419# If the bound
--     graphics pipeline state was created with both the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic states enabled then both
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     and
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @viewportCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ match the @scissorCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT'
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-viewportCount-04137# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_NV'
--     dynamic state enabled, then the bound graphics pipeline /must/ have
--     been created with
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.PipelineViewportWScalingStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-viewportCount-04138# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_NV'
--     dynamic states enabled then the @viewportCount@ parameter in the
--     last call to
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.cmdSetViewportWScalingNV'
--     /must/ be greater than or equal to the @viewportCount@ parameter in
--     the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-viewportCount-04139# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV'
--     dynamic state enabled, then the bound graphics pipeline /must/ have
--     been created with
--     'Vulkan.Extensions.VK_NV_shading_rate_image.PipelineViewportShadingRateImageStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-viewportCount-04140# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV'
--     dynamic states enabled then the @viewportCount@ parameter in the
--     last call to
--     'Vulkan.Extensions.VK_NV_shading_rate_image.cmdSetViewportShadingRatePaletteNV'
--     /must/ be greater than or equal to the @viewportCount@ parameter in
--     the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-VkPipelineVieportCreateInfo-04141#
--     If the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled and an instance of
--     'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV'
--     chained from @VkPipelineVieportCreateInfo@, then the bound graphics
--     pipeline /must/ have been created with
--     'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-VkPipelineVieportCreateInfo-04142#
--     If the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled and an instance of
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV'
--     chained from @VkPipelineVieportCreateInfo@, then the bound graphics
--     pipeline /must/ have been created with
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV'::@exclusiveScissorCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-primitiveTopology-03420# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PRIMITIVE_TOPOLOGY_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopologyEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @primitiveTopology@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopologyEXT'
--     /must/ be of the same
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-primitive-topology-class topology class>
--     as the pipeline
--     'Vulkan.Core10.Pipeline.PipelineInputAssemblyStateCreateInfo'::@topology@
--     state
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-primitiveFragmentShadingRateWithMultipleViewports-04552#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-primitiveFragmentShadingRateWithMultipleViewports primitiveFragmentShadingRateWithMultipleViewports>
--     limit is not supported, the bound graphics pipeline was created with
--     the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, and any of the shader stages of the bound
--     graphics pipeline write to the @PrimitiveShadingRateKHR@ built-in,
--     then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @viewportCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ be @1@
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-buffer-02708# If @buffer@ is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-buffer-02709# @buffer@ /must/
--     have been created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-offset-02710# @offset@ /must/ be
--     a multiple of @4@
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-commandBuffer-02711#
--     @commandBuffer@ /must/ not be a protected command buffer
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-drawCount-02718# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiDrawIndirect multi-draw indirect>
--     feature is not enabled, @drawCount@ /must/ be @0@ or @1@
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-drawCount-02719# @drawCount@
--     /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxDrawIndirectCount@
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-drawCount-02146# If @drawCount@
--     is greater than @1@, @stride@ /must/ be a multiple of @4@ and /must/
--     be greater than or equal to
--     @sizeof@('DrawMeshTasksIndirectCommandNV')
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-drawCount-02156# If @drawCount@
--     is equal to @1@, (@offset@ +
--     @sizeof@('DrawMeshTasksIndirectCommandNV')) /must/ be less than or
--     equal to the size of @buffer@
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-drawCount-02157# If @drawCount@
--     is greater than @1@, (@stride@ × (@drawCount@ - 1) + @offset@ +
--     @sizeof@('DrawMeshTasksIndirectCommandNV')) /must/ be less than or
--     equal to the size of @buffer@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-buffer-parameter# @buffer@ /must/
--     be a valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-renderpass# This command /must/
--     only be called inside of a render pass instance
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectNV-commonparent# Both of @buffer@,
--     and @commandBuffer@ /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Inside                                                                                                                 | Graphics                                                                                                              | Graphics                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Buffer', 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize'
cmdDrawMeshTasksIndirectNV :: forall io
                            . (MonadIO io)
                           => -- | @commandBuffer@ is the command buffer into which the command is
                              -- recorded.
                              CommandBuffer
                           -> -- | @buffer@ is the buffer containing draw parameters.
                              Buffer
                           -> -- | @offset@ is the byte offset into @buffer@ where parameters begin.
                              ("offset" ::: DeviceSize)
                           -> -- | @drawCount@ is the number of draws to execute, and /can/ be zero.
                              ("drawCount" ::: Word32)
                           -> -- | @stride@ is the byte stride between successive sets of draw parameters.
                              ("stride" ::: Word32)
                           -> io ()
cmdDrawMeshTasksIndirectNV commandBuffer buffer offset drawCount stride = liftIO $ do
  let vkCmdDrawMeshTasksIndirectNVPtr = pVkCmdDrawMeshTasksIndirectNV (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdDrawMeshTasksIndirectNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDrawMeshTasksIndirectNV is null" Nothing Nothing
  let vkCmdDrawMeshTasksIndirectNV' = mkVkCmdDrawMeshTasksIndirectNV vkCmdDrawMeshTasksIndirectNVPtr
  traceAroundEvent "vkCmdDrawMeshTasksIndirectNV" (vkCmdDrawMeshTasksIndirectNV' (commandBufferHandle (commandBuffer)) (buffer) (offset) (drawCount) (stride))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDrawMeshTasksIndirectCountNV
  :: FunPtr (Ptr CommandBuffer_T -> Buffer -> DeviceSize -> Buffer -> DeviceSize -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Buffer -> DeviceSize -> Buffer -> DeviceSize -> Word32 -> Word32 -> IO ()

-- | vkCmdDrawMeshTasksIndirectCountNV - Perform an indirect mesh tasks draw
-- with the draw count sourced from a buffer
--
-- = Description
--
-- 'cmdDrawMeshTasksIndirectCountNV' behaves similarly to
-- 'cmdDrawMeshTasksIndirectNV' except that the draw count is read by the
-- device from a buffer during execution. The command will read an unsigned
-- 32-bit integer from @countBuffer@ located at @countBufferOffset@ and use
-- this as the draw count.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-magFilter-04553# If a
--     'Vulkan.Core10.Handles.Sampler' created with @magFilter@ or
--     @minFilter@ equal to 'Vulkan.Core10.Enums.Filter.FILTER_LINEAR' and
--     @compareEnable@ equal to 'Vulkan.Core10.FundamentalTypes.FALSE' is
--     used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-None-02691# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using atomic
--     operations as a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-None-02692# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-filterCubic-02694# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering, as specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-filterCubicMinmax-02695# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' with a
--     reduction mode of either
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MIN'
--     or
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MAX'
--     as a result of this command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering together with minmax filtering, as
--     specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubicMinmax@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-flags-02696# Any
--     'Vulkan.Core10.Handles.Image' created with a
--     'Vulkan.Core10.Image.ImageCreateInfo'::@flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode' of
--     'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-None-02697# For each set /n/
--     that is statically used by the 'Vulkan.Core10.Handles.Pipeline'
--     bound to the pipeline bind point used by this command, a descriptor
--     set /must/ have been bound to /n/ at the same pipeline bind point,
--     with a 'Vulkan.Core10.Handles.PipelineLayout' that is compatible for
--     set /n/, with the 'Vulkan.Core10.Handles.PipelineLayout' used to
--     create the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-None-02698# For each push
--     constant that is statically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command, a push constant value /must/ have been set for
--     the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is compatible for push
--     constants, with the 'Vulkan.Core10.Handles.PipelineLayout' used to
--     create the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-None-02699# Descriptors in
--     each bound descriptor set, specified via
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets', /must/
--     be valid if they are statically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-None-02700# A valid pipeline
--     /must/ be bound to the pipeline bind point used by this command
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-commandBuffer-02701# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command requires any dynamic state, that state
--     /must/ have been set for @commandBuffer@, and done so after any
--     previously bound pipeline with the corresponding state not specified
--     as dynamic
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-None-02859# There /must/ not
--     have been any calls to dynamic state setting commands for any state
--     not specified as dynamic in the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command, since
--     that pipeline was bound
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-None-02702# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used to sample from any
--     'Vulkan.Core10.Handles.Image' with a
--     'Vulkan.Core10.Handles.ImageView' of the type
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_3D',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_1D_ARRAY',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY' or
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY', in
--     any shader stage
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-None-02703# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name, in any shader stage
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-None-02704# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values, in any shader stage
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-None-02705# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a uniform buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-None-02706# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a storage buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-commandBuffer-02707# If
--     @commandBuffer@ is an unprotected command buffer, any resource
--     accessed by the 'Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command /must/ not be a protected
--     resource
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-None-04115# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     image view’s format.
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-OpImageWrite-04469# If a
--     'Vulkan.Core10.Handles.BufferView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     image view’s format.
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-SampledType-04470# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit channel width
--     is accessed as a result of this command, the @SampledType@ of the
--     @OpTypeImage@ operand of that instruction /must/ have a @Width@ of
--     64.
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-SampledType-04471# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a channel width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32.
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-SampledType-04472# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit channel width
--     is accessed as a result of this command, the @SampledType@ of the
--     @OpTypeImage@ operand of that instruction /must/ have a @Width@ of
--     64.
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-SampledType-04473# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a channel width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32.
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-sparseImageInt64Atomics-04474#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Image' objects
--     created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command.
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-sparseImageInt64Atomics-04475#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Buffer' objects
--     created with the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command.
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-renderPass-02684# The
--     current render pass /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the @renderPass@ member of the
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo' structure
--     specified when creating the 'Vulkan.Core10.Handles.Pipeline' bound
--     to
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-subpass-02685# The subpass
--     index of the current render pass /must/ be equal to the @subpass@
--     member of the 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'
--     structure specified when creating the
--     'Vulkan.Core10.Handles.Pipeline' bound to
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-None-02686# Every input
--     attachment used by the current subpass /must/ be bound to the
--     pipeline via a descriptor set
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-None-04584# Image
--     subresources used as attachments in the current render pass /must/
--     not be accessed in any way other than as an attachment by this
--     command, except for cases involving read-only access to
--     depth\/stencil attachments as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-attachment-nonattachment Render Pass>
--     chapter
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-maxMultiviewInstanceIndex-02688#
--     If the draw is recorded in a render pass instance with multiview
--     enabled, the maximum instance index /must/ be less than or equal to
--     'Vulkan.Core11.Promoted_From_VK_KHR_multiview.PhysicalDeviceMultiviewProperties'::@maxMultiviewInstanceIndex@
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-sampleLocationsEnable-02689#
--     If the bound graphics pipeline was created with
--     'Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'::@sampleLocationsEnable@
--     set to 'Vulkan.Core10.FundamentalTypes.TRUE' and the current subpass
--     has a depth\/stencil attachment, then that attachment /must/ have
--     been created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT'
--     bit set
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-viewportCount-03417# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @viewportCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ match the
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'::@scissorCount@
--     of the pipeline
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-scissorCount-03418# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @scissorCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT'
--     /must/ match the
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'::@viewportCount@
--     of the pipeline
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-viewportCount-03419# If the
--     bound graphics pipeline state was created with both the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic states enabled then both
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     and
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @viewportCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ match the @scissorCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT'
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-viewportCount-04137# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_NV'
--     dynamic state enabled, then the bound graphics pipeline /must/ have
--     been created with
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.PipelineViewportWScalingStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-viewportCount-04138# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_NV'
--     dynamic states enabled then the @viewportCount@ parameter in the
--     last call to
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.cmdSetViewportWScalingNV'
--     /must/ be greater than or equal to the @viewportCount@ parameter in
--     the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-viewportCount-04139# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV'
--     dynamic state enabled, then the bound graphics pipeline /must/ have
--     been created with
--     'Vulkan.Extensions.VK_NV_shading_rate_image.PipelineViewportShadingRateImageStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-viewportCount-04140# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV'
--     dynamic states enabled then the @viewportCount@ parameter in the
--     last call to
--     'Vulkan.Extensions.VK_NV_shading_rate_image.cmdSetViewportShadingRatePaletteNV'
--     /must/ be greater than or equal to the @viewportCount@ parameter in
--     the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-VkPipelineVieportCreateInfo-04141#
--     If the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled and an instance of
--     'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV'
--     chained from @VkPipelineVieportCreateInfo@, then the bound graphics
--     pipeline /must/ have been created with
--     'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-VkPipelineVieportCreateInfo-04142#
--     If the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled and an instance of
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV'
--     chained from @VkPipelineVieportCreateInfo@, then the bound graphics
--     pipeline /must/ have been created with
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV'::@exclusiveScissorCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-primitiveTopology-03420# If
--     the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PRIMITIVE_TOPOLOGY_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopologyEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @primitiveTopology@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopologyEXT'
--     /must/ be of the same
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-primitive-topology-class topology class>
--     as the pipeline
--     'Vulkan.Core10.Pipeline.PipelineInputAssemblyStateCreateInfo'::@topology@
--     state
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-primitiveFragmentShadingRateWithMultipleViewports-04552#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-primitiveFragmentShadingRateWithMultipleViewports primitiveFragmentShadingRateWithMultipleViewports>
--     limit is not supported, the bound graphics pipeline was created with
--     the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, and any of the shader stages of the bound
--     graphics pipeline write to the @PrimitiveShadingRateKHR@ built-in,
--     then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @viewportCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ be @1@
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-buffer-02708# If @buffer@ is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-buffer-02709# @buffer@
--     /must/ have been created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-offset-02710# @offset@
--     /must/ be a multiple of @4@
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-commandBuffer-02711#
--     @commandBuffer@ /must/ not be a protected command buffer
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-countBuffer-02714# If
--     @countBuffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-countBuffer-02715#
--     @countBuffer@ /must/ have been created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-countBufferOffset-02716#
--     @countBufferOffset@ /must/ be a multiple of @4@
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-countBuffer-02717# The count
--     stored in @countBuffer@ /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxDrawIndirectCount@
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-countBufferOffset-04129#
--     (@countBufferOffset@ + @sizeof@(uint32_t)) /must/ be less than or
--     equal to the size of @countBuffer@
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-None-04445# If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-drawIndirectCount drawIndirectCount>
--     is not enabled this function /must/ not be used
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-stride-02182# @stride@
--     /must/ be a multiple of @4@ and /must/ be greater than or equal to
--     @sizeof@('DrawMeshTasksIndirectCommandNV')
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-maxDrawCount-02183# If
--     @maxDrawCount@ is greater than or equal to @1@, (@stride@ ×
--     (@maxDrawCount@ - 1) + @offset@ +
--     @sizeof@('DrawMeshTasksIndirectCommandNV')) /must/ be less than or
--     equal to the size of @buffer@
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-countBuffer-02191# If the
--     count stored in @countBuffer@ is equal to @1@, (@offset@ +
--     @sizeof@('DrawMeshTasksIndirectCommandNV')) /must/ be less than or
--     equal to the size of @buffer@
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-countBuffer-02192# If the
--     count stored in @countBuffer@ is greater than @1@, (@stride@ ×
--     (@drawCount@ - 1) + @offset@ +
--     @sizeof@('DrawMeshTasksIndirectCommandNV')) /must/ be less than or
--     equal to the size of @buffer@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-buffer-parameter# @buffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-countBuffer-parameter#
--     @countBuffer@ /must/ be a valid 'Vulkan.Core10.Handles.Buffer'
--     handle
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-renderpass# This command
--     /must/ only be called inside of a render pass instance
--
-- -   #VUID-vkCmdDrawMeshTasksIndirectCountNV-commonparent# Each of
--     @buffer@, @commandBuffer@, and @countBuffer@ /must/ have been
--     created, allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Inside                                                                                                                 | Graphics                                                                                                              | Graphics                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Buffer', 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize'
cmdDrawMeshTasksIndirectCountNV :: forall io
                                 . (MonadIO io)
                                => -- | @commandBuffer@ is the command buffer into which the command is
                                   -- recorded.
                                   CommandBuffer
                                -> -- | @buffer@ is the buffer containing draw parameters.
                                   Buffer
                                -> -- | @offset@ is the byte offset into @buffer@ where parameters begin.
                                   ("offset" ::: DeviceSize)
                                -> -- | @countBuffer@ is the buffer containing the draw count.
                                   ("countBuffer" ::: Buffer)
                                -> -- | @countBufferOffset@ is the byte offset into @countBuffer@ where the draw
                                   -- count begins.
                                   ("countBufferOffset" ::: DeviceSize)
                                -> -- | @maxDrawCount@ specifies the maximum number of draws that will be
                                   -- executed. The actual number of executed draw calls is the minimum of the
                                   -- count specified in @countBuffer@ and @maxDrawCount@.
                                   ("maxDrawCount" ::: Word32)
                                -> -- | @stride@ is the byte stride between successive sets of draw parameters.
                                   ("stride" ::: Word32)
                                -> io ()
cmdDrawMeshTasksIndirectCountNV commandBuffer buffer offset countBuffer countBufferOffset maxDrawCount stride = liftIO $ do
  let vkCmdDrawMeshTasksIndirectCountNVPtr = pVkCmdDrawMeshTasksIndirectCountNV (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdDrawMeshTasksIndirectCountNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDrawMeshTasksIndirectCountNV is null" Nothing Nothing
  let vkCmdDrawMeshTasksIndirectCountNV' = mkVkCmdDrawMeshTasksIndirectCountNV vkCmdDrawMeshTasksIndirectCountNVPtr
  traceAroundEvent "vkCmdDrawMeshTasksIndirectCountNV" (vkCmdDrawMeshTasksIndirectCountNV' (commandBufferHandle (commandBuffer)) (buffer) (offset) (countBuffer) (countBufferOffset) (maxDrawCount) (stride))
  pure $ ()


-- | VkPhysicalDeviceMeshShaderFeaturesNV - Structure describing mesh shading
-- features that can be supported by an implementation
--
-- = Description
--
-- If the 'PhysicalDeviceMeshShaderFeaturesNV' structure is included in the
-- @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with a value indicating whether the feature is supported.
-- 'PhysicalDeviceMeshShaderFeaturesNV' /can/ also be included in @pNext@
-- chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to enable the features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMeshShaderFeaturesNV = PhysicalDeviceMeshShaderFeaturesNV
  { -- | #features-taskShader# @taskShader@ indicates whether the task shader
    -- stage is supported.
    taskShader :: Bool
  , -- | #features-meshShader# @meshShader@ indicates whether the mesh shader
    -- stage is supported.
    meshShader :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMeshShaderFeaturesNV)
#endif
deriving instance Show PhysicalDeviceMeshShaderFeaturesNV

instance ToCStruct PhysicalDeviceMeshShaderFeaturesNV where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMeshShaderFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (taskShader))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (meshShader))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceMeshShaderFeaturesNV where
  peekCStruct p = do
    taskShader <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    meshShader <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceMeshShaderFeaturesNV
             (bool32ToBool taskShader) (bool32ToBool meshShader)

instance Storable PhysicalDeviceMeshShaderFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMeshShaderFeaturesNV where
  zero = PhysicalDeviceMeshShaderFeaturesNV
           zero
           zero


-- | VkPhysicalDeviceMeshShaderPropertiesNV - Structure describing mesh
-- shading properties
--
-- = Members
--
-- The members of the 'PhysicalDeviceMeshShaderPropertiesNV' structure
-- describe the following implementation-dependent limits:
--
-- = Description
--
-- If the 'PhysicalDeviceMeshShaderPropertiesNV' structure is included in
-- the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMeshShaderPropertiesNV = PhysicalDeviceMeshShaderPropertiesNV
  { -- | @maxDrawMeshTasksCount@ is the maximum number of local workgroups that
    -- /can/ be launched by a single draw mesh tasks command. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-mesh-shading>.
    maxDrawMeshTasksCount :: Word32
  , -- | @maxTaskWorkGroupInvocations@ is the maximum total number of task shader
    -- invocations in a single local workgroup. The product of the X, Y, and Z
    -- sizes, as specified by the @LocalSize@ execution mode in shader modules
    -- or by the object decorated by the @WorkgroupSize@ decoration, /must/ be
    -- less than or equal to this limit.
    maxTaskWorkGroupInvocations :: Word32
  , -- | @maxTaskWorkGroupSize@[3] is the maximum size of a local task workgroup.
    -- These three values represent the maximum local workgroup size in the X,
    -- Y, and Z dimensions, respectively. The @x@, @y@, and @z@ sizes, as
    -- specified by the @LocalSize@ execution mode or by the object decorated
    -- by the @WorkgroupSize@ decoration in shader modules, /must/ be less than
    -- or equal to the corresponding limit.
    maxTaskWorkGroupSize :: (Word32, Word32, Word32)
  , -- | @maxTaskTotalMemorySize@ is the maximum number of bytes that the task
    -- shader can use in total for shared and output memory combined.
    maxTaskTotalMemorySize :: Word32
  , -- | @maxTaskOutputCount@ is the maximum number of output tasks a single task
    -- shader workgroup can emit.
    maxTaskOutputCount :: Word32
  , -- | @maxMeshWorkGroupInvocations@ is the maximum total number of mesh shader
    -- invocations in a single local workgroup. The product of the X, Y, and Z
    -- sizes, as specified by the @LocalSize@ execution mode in shader modules
    -- or by the object decorated by the @WorkgroupSize@ decoration, /must/ be
    -- less than or equal to this limit.
    maxMeshWorkGroupInvocations :: Word32
  , -- | @maxMeshWorkGroupSize@[3] is the maximum size of a local mesh workgroup.
    -- These three values represent the maximum local workgroup size in the X,
    -- Y, and Z dimensions, respectively. The @x@, @y@, and @z@ sizes, as
    -- specified by the @LocalSize@ execution mode or by the object decorated
    -- by the @WorkgroupSize@ decoration in shader modules, /must/ be less than
    -- or equal to the corresponding limit.
    maxMeshWorkGroupSize :: (Word32, Word32, Word32)
  , -- | @maxMeshTotalMemorySize@ is the maximum number of bytes that the mesh
    -- shader can use in total for shared and output memory combined.
    maxMeshTotalMemorySize :: Word32
  , -- | @maxMeshOutputVertices@ is the maximum number of vertices a mesh shader
    -- output can store.
    maxMeshOutputVertices :: Word32
  , -- | @maxMeshOutputPrimitives@ is the maximum number of primitives a mesh
    -- shader output can store.
    maxMeshOutputPrimitives :: Word32
  , -- | @maxMeshMultiviewViewCount@ is the maximum number of multi-view views a
    -- mesh shader can use.
    maxMeshMultiviewViewCount :: Word32
  , -- | @meshOutputPerVertexGranularity@ is the granularity with which mesh
    -- vertex outputs are allocated. The value can be used to compute the
    -- memory size used by the mesh shader, which must be less than or equal to
    -- @maxMeshTotalMemorySize@.
    meshOutputPerVertexGranularity :: Word32
  , -- | @meshOutputPerPrimitiveGranularity@ is the granularity with which mesh
    -- outputs qualified as per-primitive are allocated. The value can be used
    -- to compute the memory size used by the mesh shader, which must be less
    -- than or equal to @maxMeshTotalMemorySize@.
    meshOutputPerPrimitiveGranularity :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMeshShaderPropertiesNV)
#endif
deriving instance Show PhysicalDeviceMeshShaderPropertiesNV

instance ToCStruct PhysicalDeviceMeshShaderPropertiesNV where
  withCStruct x f = allocaBytesAligned 88 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMeshShaderPropertiesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxDrawMeshTasksCount)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxTaskWorkGroupInvocations)
    let pMaxTaskWorkGroupSize' = lowerArrayPtr ((p `plusPtr` 24 :: Ptr (FixedArray 3 Word32)))
    case (maxTaskWorkGroupSize) of
      (e0, e1, e2) -> do
        poke (pMaxTaskWorkGroupSize' :: Ptr Word32) (e0)
        poke (pMaxTaskWorkGroupSize' `plusPtr` 4 :: Ptr Word32) (e1)
        poke (pMaxTaskWorkGroupSize' `plusPtr` 8 :: Ptr Word32) (e2)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (maxTaskTotalMemorySize)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (maxTaskOutputCount)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (maxMeshWorkGroupInvocations)
    let pMaxMeshWorkGroupSize' = lowerArrayPtr ((p `plusPtr` 48 :: Ptr (FixedArray 3 Word32)))
    case (maxMeshWorkGroupSize) of
      (e0, e1, e2) -> do
        poke (pMaxMeshWorkGroupSize' :: Ptr Word32) (e0)
        poke (pMaxMeshWorkGroupSize' `plusPtr` 4 :: Ptr Word32) (e1)
        poke (pMaxMeshWorkGroupSize' `plusPtr` 8 :: Ptr Word32) (e2)
    poke ((p `plusPtr` 60 :: Ptr Word32)) (maxMeshTotalMemorySize)
    poke ((p `plusPtr` 64 :: Ptr Word32)) (maxMeshOutputVertices)
    poke ((p `plusPtr` 68 :: Ptr Word32)) (maxMeshOutputPrimitives)
    poke ((p `plusPtr` 72 :: Ptr Word32)) (maxMeshMultiviewViewCount)
    poke ((p `plusPtr` 76 :: Ptr Word32)) (meshOutputPerVertexGranularity)
    poke ((p `plusPtr` 80 :: Ptr Word32)) (meshOutputPerPrimitiveGranularity)
    f
  cStructSize = 88
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    let pMaxTaskWorkGroupSize' = lowerArrayPtr ((p `plusPtr` 24 :: Ptr (FixedArray 3 Word32)))
    case ((zero, zero, zero)) of
      (e0, e1, e2) -> do
        poke (pMaxTaskWorkGroupSize' :: Ptr Word32) (e0)
        poke (pMaxTaskWorkGroupSize' `plusPtr` 4 :: Ptr Word32) (e1)
        poke (pMaxTaskWorkGroupSize' `plusPtr` 8 :: Ptr Word32) (e2)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (zero)
    let pMaxMeshWorkGroupSize' = lowerArrayPtr ((p `plusPtr` 48 :: Ptr (FixedArray 3 Word32)))
    case ((zero, zero, zero)) of
      (e0, e1, e2) -> do
        poke (pMaxMeshWorkGroupSize' :: Ptr Word32) (e0)
        poke (pMaxMeshWorkGroupSize' `plusPtr` 4 :: Ptr Word32) (e1)
        poke (pMaxMeshWorkGroupSize' `plusPtr` 8 :: Ptr Word32) (e2)
    poke ((p `plusPtr` 60 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 64 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 68 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 72 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 76 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 80 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceMeshShaderPropertiesNV where
  peekCStruct p = do
    maxDrawMeshTasksCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxTaskWorkGroupInvocations <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    let pmaxTaskWorkGroupSize = lowerArrayPtr @Word32 ((p `plusPtr` 24 :: Ptr (FixedArray 3 Word32)))
    maxTaskWorkGroupSize0 <- peek @Word32 ((pmaxTaskWorkGroupSize `advancePtrBytes` 0 :: Ptr Word32))
    maxTaskWorkGroupSize1 <- peek @Word32 ((pmaxTaskWorkGroupSize `advancePtrBytes` 4 :: Ptr Word32))
    maxTaskWorkGroupSize2 <- peek @Word32 ((pmaxTaskWorkGroupSize `advancePtrBytes` 8 :: Ptr Word32))
    maxTaskTotalMemorySize <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    maxTaskOutputCount <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    maxMeshWorkGroupInvocations <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    let pmaxMeshWorkGroupSize = lowerArrayPtr @Word32 ((p `plusPtr` 48 :: Ptr (FixedArray 3 Word32)))
    maxMeshWorkGroupSize0 <- peek @Word32 ((pmaxMeshWorkGroupSize `advancePtrBytes` 0 :: Ptr Word32))
    maxMeshWorkGroupSize1 <- peek @Word32 ((pmaxMeshWorkGroupSize `advancePtrBytes` 4 :: Ptr Word32))
    maxMeshWorkGroupSize2 <- peek @Word32 ((pmaxMeshWorkGroupSize `advancePtrBytes` 8 :: Ptr Word32))
    maxMeshTotalMemorySize <- peek @Word32 ((p `plusPtr` 60 :: Ptr Word32))
    maxMeshOutputVertices <- peek @Word32 ((p `plusPtr` 64 :: Ptr Word32))
    maxMeshOutputPrimitives <- peek @Word32 ((p `plusPtr` 68 :: Ptr Word32))
    maxMeshMultiviewViewCount <- peek @Word32 ((p `plusPtr` 72 :: Ptr Word32))
    meshOutputPerVertexGranularity <- peek @Word32 ((p `plusPtr` 76 :: Ptr Word32))
    meshOutputPerPrimitiveGranularity <- peek @Word32 ((p `plusPtr` 80 :: Ptr Word32))
    pure $ PhysicalDeviceMeshShaderPropertiesNV
             maxDrawMeshTasksCount maxTaskWorkGroupInvocations ((maxTaskWorkGroupSize0, maxTaskWorkGroupSize1, maxTaskWorkGroupSize2)) maxTaskTotalMemorySize maxTaskOutputCount maxMeshWorkGroupInvocations ((maxMeshWorkGroupSize0, maxMeshWorkGroupSize1, maxMeshWorkGroupSize2)) maxMeshTotalMemorySize maxMeshOutputVertices maxMeshOutputPrimitives maxMeshMultiviewViewCount meshOutputPerVertexGranularity meshOutputPerPrimitiveGranularity

instance Storable PhysicalDeviceMeshShaderPropertiesNV where
  sizeOf ~_ = 88
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMeshShaderPropertiesNV where
  zero = PhysicalDeviceMeshShaderPropertiesNV
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


-- | VkDrawMeshTasksIndirectCommandNV - Structure specifying a mesh tasks
-- draw indirect command
--
-- = Description
--
-- The members of 'DrawMeshTasksIndirectCommandNV' have the same meaning as
-- the similarly named parameters of 'cmdDrawMeshTasksNV'.
--
-- == Valid Usage
--
-- = See Also
--
-- 'cmdDrawMeshTasksIndirectNV'
data DrawMeshTasksIndirectCommandNV = DrawMeshTasksIndirectCommandNV
  { -- | @taskCount@ is the number of local workgroups to dispatch in the X
    -- dimension. Y and Z dimension are implicitly set to one.
    --
    -- #VUID-VkDrawMeshTasksIndirectCommandNV-taskCount-02175# @taskCount@
    -- /must/ be less than or equal to
    -- 'PhysicalDeviceMeshShaderPropertiesNV'::@maxDrawMeshTasksCount@
    taskCount :: Word32
  , -- | @firstTask@ is the X component of the first workgroup ID.
    firstTask :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DrawMeshTasksIndirectCommandNV)
#endif
deriving instance Show DrawMeshTasksIndirectCommandNV

instance ToCStruct DrawMeshTasksIndirectCommandNV where
  withCStruct x f = allocaBytesAligned 8 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DrawMeshTasksIndirectCommandNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (taskCount)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (firstTask)
    f
  cStructSize = 8
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    f

instance FromCStruct DrawMeshTasksIndirectCommandNV where
  peekCStruct p = do
    taskCount <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    firstTask <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    pure $ DrawMeshTasksIndirectCommandNV
             taskCount firstTask

instance Storable DrawMeshTasksIndirectCommandNV where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DrawMeshTasksIndirectCommandNV where
  zero = DrawMeshTasksIndirectCommandNV
           zero
           zero


type NV_MESH_SHADER_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_MESH_SHADER_SPEC_VERSION"
pattern NV_MESH_SHADER_SPEC_VERSION :: forall a . Integral a => a
pattern NV_MESH_SHADER_SPEC_VERSION = 1


type NV_MESH_SHADER_EXTENSION_NAME = "VK_NV_mesh_shader"

-- No documentation found for TopLevel "VK_NV_MESH_SHADER_EXTENSION_NAME"
pattern NV_MESH_SHADER_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_MESH_SHADER_EXTENSION_NAME = "VK_NV_mesh_shader"

