{-# language CPP #-}
-- | = Name
--
-- VK_AMDX_shader_enqueue - device extension
--
-- = VK_AMDX_shader_enqueue
--
-- [__Name String__]
--     @VK_AMDX_shader_enqueue@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     135
--
-- [__Revision__]
--     2
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--             
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>
--              and
--             
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_spirv_1_4 VK_KHR_spirv_1_4>
--              and
--             
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state VK_EXT_extended_dynamic_state>
--          or
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Vulkan Version 1.3>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_pipeline_library VK_KHR_pipeline_library>
--
--     -   __This is a /provisional/ extension and /must/ be used with
--         caution. See the
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#boilerplate-provisional-header description>
--         of provisional header files for enablement and stability
--         details.__
--
-- [__API Interactions__]
--
--     -   Interacts with VK_VERSION_1_4
--
--     -   Interacts with VK_EXT_mesh_shader
--
--     -   Interacts with VK_KHR_maintenance5
--
-- [__Contact__]
--
--     -   Tobias Hector
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_AMDX_shader_enqueue] @tobski%0A*Here describe the issue or question you have about the VK_AMDX_shader_enqueue extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_AMDX_shader_enqueue.adoc VK_AMDX_shader_enqueue>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-07-17
--
-- [__Provisional__]
--     __This extension is /provisional/ and /should/ not be used in
--     production applications. The functionality /may/ change in ways that
--     break backwards compatibility between revisions, and before final
--     release.__
--
-- [__Contributors__]
--
--     -   Tobias Hector, AMD
--
--     -   Matthaeus Chajdas, AMD
--
--     -   Maciej Jesionowski, AMD
--
--     -   Robert Martin, AMD
--
--     -   Qun Lin, AMD
--
--     -   Rex Xu, AMD
--
--     -   Dominik Witczak, AMD
--
--     -   Karthik Srinivasan, AMD
--
--     -   Nicolai Haehnle, AMD
--
--     -   Stuart Smith, AMD
--
-- == Description
--
-- This extension adds the ability for developers to enqueue mesh and
-- compute shader workgroups from other compute shaders.
--
-- == New Commands
--
-- -   'cmdDispatchGraphAMDX'
--
-- -   'cmdDispatchGraphIndirectAMDX'
--
-- -   'cmdDispatchGraphIndirectCountAMDX'
--
-- -   'cmdInitializeGraphScratchMemoryAMDX'
--
-- -   'createExecutionGraphPipelinesAMDX'
--
-- -   'getExecutionGraphPipelineNodeIndexAMDX'
--
-- -   'getExecutionGraphPipelineScratchSizeAMDX'
--
-- == New Structures
--
-- -   'DispatchGraphCountInfoAMDX'
--
-- -   'DispatchGraphInfoAMDX'
--
-- -   'ExecutionGraphPipelineCreateInfoAMDX'
--
-- -   'ExecutionGraphPipelineScratchSizeAMDX'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderEnqueueFeaturesAMDX'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceShaderEnqueuePropertiesAMDX'
--
-- -   Extending
--     'Vulkan.Core10.ComputePipeline.PipelineShaderStageCreateInfo':
--
--     -   'PipelineShaderStageNodeCreateInfoAMDX'
--
-- == New Unions
--
-- -   'DeviceOrHostAddressConstAMDX'
--
-- == New Enum Constants
--
-- -   'AMDX_SHADER_ENQUEUE_EXTENSION_NAME'
--
-- -   'AMDX_SHADER_ENQUEUE_SPEC_VERSION'
--
-- -   'Vulkan.Core10.APIConstants.SHADER_INDEX_UNUSED_AMDX'
--
-- -   Extending
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BufferUsageFlagBits':
--
--     -   'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_EXECUTION_GRAPH_SCRATCH_BIT_AMDX'
--
-- -   Extending 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint':
--
--     -   'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_EXECUTION_GRAPH_AMDX'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXECUTION_GRAPH_PIPELINE_CREATE_INFO_AMDX'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXECUTION_GRAPH_PIPELINE_SCRATCH_SIZE_AMDX'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ENQUEUE_FEATURES_AMDX'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ENQUEUE_PROPERTIES_AMDX'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_NODE_CREATE_INFO_AMDX'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.4 Vulkan Version 1.4>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core14.Enums.BufferUsageFlags2.BufferUsageFlagBits2':
--
--     -   'Vulkan.Core14.Enums.BufferUsageFlags2.BUFFER_USAGE_2_EXECUTION_GRAPH_SCRATCH_BIT_AMDX'
--
-- -   Extending
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PipelineCreateFlagBits2':
--
--     -   'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_EXECUTION_GRAPH_BIT_AMDX'
--
-- == Version History
--
-- -   Revision 2, 2024-07-17 (Tobias Hector)
--
--     -   Add mesh nodes
--
-- -   Revision 1, 2021-07-22 (Tobias Hector)
--
--     -   Initial revision
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_AMDX_shader_enqueue Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_AMDX_shader_enqueue  ( DispatchGraphCountInfoAMDX
                                                 , DispatchGraphInfoAMDX
                                                 , ExecutionGraphPipelineCreateInfoAMDX
                                                 , ExecutionGraphPipelineScratchSizeAMDX
                                                 , PhysicalDeviceShaderEnqueueFeaturesAMDX
                                                 , PhysicalDeviceShaderEnqueuePropertiesAMDX
                                                 , PipelineShaderStageNodeCreateInfoAMDX
                                                 ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
data DispatchGraphCountInfoAMDX

instance ToCStruct DispatchGraphCountInfoAMDX
instance Show DispatchGraphCountInfoAMDX


data DispatchGraphInfoAMDX

instance ToCStruct DispatchGraphInfoAMDX
instance Show DispatchGraphInfoAMDX


type role ExecutionGraphPipelineCreateInfoAMDX nominal
data ExecutionGraphPipelineCreateInfoAMDX (es :: [Type])

instance ( Extendss ExecutionGraphPipelineCreateInfoAMDX es
         , PokeChain es ) => ToCStruct (ExecutionGraphPipelineCreateInfoAMDX es)
instance Show (Chain es) => Show (ExecutionGraphPipelineCreateInfoAMDX es)

instance ( Extendss ExecutionGraphPipelineCreateInfoAMDX es
         , PeekChain es ) => FromCStruct (ExecutionGraphPipelineCreateInfoAMDX es)


data ExecutionGraphPipelineScratchSizeAMDX

instance ToCStruct ExecutionGraphPipelineScratchSizeAMDX
instance Show ExecutionGraphPipelineScratchSizeAMDX

instance FromCStruct ExecutionGraphPipelineScratchSizeAMDX


data PhysicalDeviceShaderEnqueueFeaturesAMDX

instance ToCStruct PhysicalDeviceShaderEnqueueFeaturesAMDX
instance Show PhysicalDeviceShaderEnqueueFeaturesAMDX

instance FromCStruct PhysicalDeviceShaderEnqueueFeaturesAMDX


data PhysicalDeviceShaderEnqueuePropertiesAMDX

instance ToCStruct PhysicalDeviceShaderEnqueuePropertiesAMDX
instance Show PhysicalDeviceShaderEnqueuePropertiesAMDX

instance FromCStruct PhysicalDeviceShaderEnqueuePropertiesAMDX


data PipelineShaderStageNodeCreateInfoAMDX

instance ToCStruct PipelineShaderStageNodeCreateInfoAMDX
instance Show PipelineShaderStageNodeCreateInfoAMDX

instance FromCStruct PipelineShaderStageNodeCreateInfoAMDX

