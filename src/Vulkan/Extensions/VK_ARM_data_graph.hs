{-# language CPP #-}
-- | = Name
--
-- VK_ARM_data_graph - device extension
--
-- = VK_ARM_data_graph
--
-- [__Name String__]
--     @VK_ARM_data_graph@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     508
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Vulkan Version 1.3>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_deferred_host_operations VK_KHR_deferred_host_operations>
--
-- [__API Interactions__]
--
--     -   Interacts with VK_ARM_tensors
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/ARM/SPV_ARM_graph.html SPV_ARM_graph>
--
-- [__Contact__]
--
--     -   Kevin Petit
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_ARM_data_graph] @kpet%0A*Here describe the issue or question you have about the VK_ARM_data_graph extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-06-18
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_ARM_graph.html SPV_ARM_graph>
--
--     -   This extension interacts with @VK_EXT_mutable_descriptor_type@
--
--     -   This extension interacts with @VK_EXT_pipeline_protected_access@
--
--     -   This extension interacts with @VK_ARM_tensors@
--
--     -   This extension interacts with @VK_EXT_descriptor_buffer@
--
--     -   This extension interacts with @VK_KHR_maintenance6@
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Kévin Petit, Arm Ltd.
--
--     -   Emma Ben Yossef, Arm Ltd.
--
--     -   Stefano Bucciarelli, Arm Ltd.
--
--     -   Marco Cattani, Arm Ltd.
--
--     -   Aaron DeBattista, Arm Ltd.
--
--     -   Jan-Harald Fredriksen, Arm Ltd.
--
--     -   Einar Hov, Arm Ltd.
--
--     -   Robert Hughes, Arm Ltd.
--
--     -   Oualid Khelifi, Arm Ltd.
--
--     -   Derek Lamberti, Arm Ltd.
--
--     -   Chetan Mistry, Arm Ltd.
--
--     -   Georgios Teneketzis, Arm Ltd.
--
--     -   Matthew Netsch, Qualcomm Technologies, Inc
--
-- == Description
--
-- This extension adds support for a new type of pipeline, data graph
-- pipelines, that provide an encapsulation construct for computational
-- graphs operating on full resources (e.g. ML\/AI graphs, image processing
-- pipelines, etc). This extension only supports tensor resources and does
-- not define any operations that can be used within those graphs. These
-- operations will be defined by separate extensions.
--
-- == New Object Types
--
-- -   'Vulkan.Extensions.Handles.DataGraphPipelineSessionARM'
--
-- == New Commands
--
-- -   'bindDataGraphPipelineSessionMemoryARM'
--
-- -   'cmdDispatchDataGraphARM'
--
-- -   'createDataGraphPipelineSessionARM'
--
-- -   'createDataGraphPipelinesARM'
--
-- -   'destroyDataGraphPipelineSessionARM'
--
-- -   'getDataGraphPipelineAvailablePropertiesARM'
--
-- -   'getDataGraphPipelinePropertiesARM'
--
-- -   'getDataGraphPipelineSessionBindPointRequirementsARM'
--
-- -   'getDataGraphPipelineSessionMemoryRequirementsARM'
--
-- -   'getPhysicalDeviceQueueFamilyDataGraphProcessingEnginePropertiesARM'
--
-- -   'getPhysicalDeviceQueueFamilyDataGraphPropertiesARM'
--
-- == New Structures
--
-- -   'BindDataGraphPipelineSessionMemoryInfoARM'
--
-- -   'DataGraphPipelineConstantARM'
--
-- -   'DataGraphPipelineCreateInfoARM'
--
-- -   'DataGraphPipelineDispatchInfoARM'
--
-- -   'DataGraphPipelineInfoARM'
--
-- -   'DataGraphPipelinePropertyQueryResultARM'
--
-- -   'DataGraphPipelineResourceInfoARM'
--
-- -   'DataGraphPipelineSessionBindPointRequirementARM'
--
-- -   'DataGraphPipelineSessionBindPointRequirementsInfoARM'
--
-- -   'DataGraphPipelineSessionCreateInfoARM'
--
-- -   'DataGraphPipelineSessionMemoryRequirementsInfoARM'
--
-- -   'PhysicalDeviceDataGraphOperationSupportARM'
--
-- -   'PhysicalDeviceDataGraphProcessingEngineARM'
--
-- -   'PhysicalDeviceQueueFamilyDataGraphProcessingEngineInfoARM'
--
-- -   'QueueFamilyDataGraphProcessingEnginePropertiesARM'
--
-- -   'QueueFamilyDataGraphPropertiesARM'
--
-- -   Extending 'DataGraphPipelineCreateInfoARM':
--
--     -   'DataGraphPipelineCompilerControlCreateInfoARM'
--
--     -   'DataGraphPipelineIdentifierCreateInfoARM'
--
--     -   'DataGraphPipelineShaderModuleCreateInfoARM'
--
-- -   Extending 'DataGraphPipelineCreateInfoARM',
--     'Vulkan.Core10.DescriptorSet.DescriptorPoolCreateInfo',
--     'Vulkan.Core10.CommandPool.CommandPoolCreateInfo':
--
--     -   'DataGraphProcessingEngineCreateInfoARM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDataGraphFeaturesARM'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>
-- is supported:
--
-- -   Extending 'DataGraphPipelineConstantARM':
--
--     -   'DataGraphPipelineConstantTensorSemiStructuredSparsityInfoARM'
--
-- == New Enums
--
-- -   'DataGraphPipelineDispatchFlagBitsARM'
--
-- -   'DataGraphPipelinePropertyARM'
--
-- -   'DataGraphPipelineSessionBindPointARM'
--
-- -   'DataGraphPipelineSessionBindPointTypeARM'
--
-- -   'DataGraphPipelineSessionCreateFlagBitsARM'
--
-- -   'PhysicalDeviceDataGraphOperationTypeARM'
--
-- -   'PhysicalDeviceDataGraphProcessingEngineTypeARM'
--
-- == New Bitmasks
--
-- -   'DataGraphPipelineDispatchFlagsARM'
--
-- -   'DataGraphPipelineSessionCreateFlagsARM'
--
-- == New Enum Constants
--
-- -   'ARM_DATA_GRAPH_EXTENSION_NAME'
--
-- -   'ARM_DATA_GRAPH_SPEC_VERSION'
--
-- -   'Vulkan.Core10.APIConstants.MAX_PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_SET_NAME_SIZE_ARM'
--
-- -   Extending 'Vulkan.Core13.Enums.AccessFlags2.AccessFlagBits2':
--
--     -   'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_DATA_GRAPH_READ_BIT_ARM'
--
--     -   'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_DATA_GRAPH_WRITE_BIT_ARM'
--
-- -   Extending
--     'Vulkan.Core14.Enums.BufferUsageFlags2.BufferUsageFlagBits2':
--
--     -   'Vulkan.Core14.Enums.BufferUsageFlags2.BUFFER_USAGE_2_DATA_GRAPH_FOREIGN_DESCRIPTOR_BIT_ARM'
--
-- -   Extending
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FormatFeatureFlagBits2':
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_TENSOR_DATA_GRAPH_BIT_ARM'
--
-- -   Extending 'Vulkan.Core10.Enums.ObjectType.ObjectType':
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_DATA_GRAPH_PIPELINE_SESSION_ARM'
--
-- -   Extending 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint':
--
--     -   'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_DATA_GRAPH_ARM'
--
-- -   Extending
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2':
--
--     -   'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_DATA_GRAPH_BIT_ARM'
--
-- -   Extending 'Vulkan.Core10.Enums.QueueFlagBits.QueueFlagBits':
--
--     -   'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_DATA_GRAPH_BIT_ARM'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BIND_DATA_GRAPH_PIPELINE_SESSION_MEMORY_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_COMPILER_CONTROL_CREATE_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_CONSTANT_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_CREATE_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_DISPATCH_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_IDENTIFIER_CREATE_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_PROPERTY_QUERY_RESULT_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_RESOURCE_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SESSION_BIND_POINT_REQUIREMENTS_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SESSION_BIND_POINT_REQUIREMENT_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SESSION_CREATE_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SESSION_MEMORY_REQUIREMENTS_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SHADER_MODULE_CREATE_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PROCESSING_ENGINE_CREATE_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DATA_GRAPH_FEATURES_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_QUEUE_FAMILY_DATA_GRAPH_PROCESSING_ENGINE_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_QUEUE_FAMILY_DATA_GRAPH_PROCESSING_ENGINE_PROPERTIES_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_QUEUE_FAMILY_DATA_GRAPH_PROPERTIES_ARM'
--
-- -   Extending 'Vulkan.Extensions.VK_ARM_tensors.TensorUsageFlagBitsARM':
--
--     -   'Vulkan.Extensions.VK_ARM_tensors.TENSOR_USAGE_DATA_GRAPH_BIT_ARM'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_CONSTANT_TENSOR_SEMI_STRUCTURED_SPARSITY_INFO_ARM'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-GraphARM GraphARM>
--
-- == Issues
--
-- 1) Should graph pipeline resource info structures be integrated into
-- pipeline layouts? Would a new graph pipeline layout be a better fit?
--
-- __RESOLVED__: Graph pipeline resource info are passed separately at
-- pipeline creation time.
--
-- 2) Do we need a new shader stage for graph pipelines for use in creating
-- descriptor set layouts?
--
-- __RESOLVED__: Currently using
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_ALL'.
--
-- 3) Should this extension provide applications with a way of knowing
-- which combinations of sparsity information implementations can take
-- advantage of when processing graph constants?
--
-- __RESOLVED__: No. Describing the exact combinations is in some cases
-- complex and it is always valid for implementations to ignore the
-- sparsity information and treat the data as dense. Specific
-- implementations can provide guidance to application writers if they so
-- desire and applications are encouraged to always provide sparsity
-- information that they have.
--
-- == Version History
--
-- -   Revision 1, 2025-06-18 (Kévin Petit)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_ARM_data_graph Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_ARM_data_graph  ( createDataGraphPipelinesARM
                                            , createDataGraphPipelineSessionARM
                                            , withDataGraphPipelineSessionARM
                                            , getDataGraphPipelineSessionBindPointRequirementsARM
                                            , getDataGraphPipelineSessionMemoryRequirementsARM
                                            , bindDataGraphPipelineSessionMemoryARM
                                            , destroyDataGraphPipelineSessionARM
                                            , cmdDispatchDataGraphARM
                                            , getDataGraphPipelineAvailablePropertiesARM
                                            , getDataGraphPipelinePropertiesARM
                                            , getPhysicalDeviceQueueFamilyDataGraphPropertiesARM
                                            , getPhysicalDeviceQueueFamilyDataGraphProcessingEnginePropertiesARM
                                            , PhysicalDeviceDataGraphFeaturesARM(..)
                                            , DataGraphPipelineConstantTensorSemiStructuredSparsityInfoARM(..)
                                            , DataGraphPipelineConstantARM(..)
                                            , DataGraphPipelineResourceInfoARM(..)
                                            , DataGraphPipelineCompilerControlCreateInfoARM(..)
                                            , DataGraphPipelineCreateInfoARM(..)
                                            , DataGraphPipelineShaderModuleCreateInfoARM(..)
                                            , DataGraphPipelineSessionCreateInfoARM(..)
                                            , DataGraphPipelineSessionBindPointRequirementsInfoARM(..)
                                            , DataGraphPipelineSessionBindPointRequirementARM(..)
                                            , DataGraphPipelineSessionMemoryRequirementsInfoARM(..)
                                            , BindDataGraphPipelineSessionMemoryInfoARM(..)
                                            , DataGraphPipelineInfoARM(..)
                                            , DataGraphPipelinePropertyQueryResultARM(..)
                                            , DataGraphPipelineIdentifierCreateInfoARM(..)
                                            , DataGraphPipelineDispatchInfoARM(..)
                                            , PhysicalDeviceDataGraphProcessingEngineARM(..)
                                            , PhysicalDeviceDataGraphOperationSupportARM(..)
                                            , QueueFamilyDataGraphPropertiesARM(..)
                                            , PhysicalDeviceQueueFamilyDataGraphProcessingEngineInfoARM(..)
                                            , QueueFamilyDataGraphProcessingEnginePropertiesARM(..)
                                            , DataGraphProcessingEngineCreateInfoARM(..)
                                            , DataGraphPipelineSessionCreateFlagsARM
                                            , DataGraphPipelineSessionCreateFlagBitsARM( DATA_GRAPH_PIPELINE_SESSION_CREATE_PROTECTED_BIT_ARM
                                                                                       , ..
                                                                                       )
                                            , DataGraphPipelineSessionBindPointARM( DATA_GRAPH_PIPELINE_SESSION_BIND_POINT_TRANSIENT_ARM
                                                                                  , ..
                                                                                  )
                                            , DataGraphPipelineSessionBindPointTypeARM( DATA_GRAPH_PIPELINE_SESSION_BIND_POINT_TYPE_MEMORY_ARM
                                                                                      , ..
                                                                                      )
                                            , DataGraphPipelinePropertyARM( DATA_GRAPH_PIPELINE_PROPERTY_CREATION_LOG_ARM
                                                                          , DATA_GRAPH_PIPELINE_PROPERTY_IDENTIFIER_ARM
                                                                          , ..
                                                                          )
                                            , DataGraphPipelineDispatchFlagsARM
                                            , DataGraphPipelineDispatchFlagBitsARM(..)
                                            , PhysicalDeviceDataGraphProcessingEngineTypeARM( PHYSICAL_DEVICE_DATA_GRAPH_PROCESSING_ENGINE_TYPE_DEFAULT_ARM
                                                                                            , PHYSICAL_DEVICE_DATA_GRAPH_PROCESSING_ENGINE_TYPE_COMPUTE_QCOM
                                                                                            , PHYSICAL_DEVICE_DATA_GRAPH_PROCESSING_ENGINE_TYPE_NEURAL_QCOM
                                                                                            , ..
                                                                                            )
                                            , PhysicalDeviceDataGraphOperationTypeARM( PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_TYPE_SPIRV_EXTENDED_INSTRUCTION_SET_ARM
                                                                                     , PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_TYPE_BUILTIN_MODEL_QCOM
                                                                                     , PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_TYPE_NEURAL_MODEL_QCOM
                                                                                     , ..
                                                                                     )
                                            , ARM_DATA_GRAPH_SPEC_VERSION
                                            , pattern ARM_DATA_GRAPH_SPEC_VERSION
                                            , ARM_DATA_GRAPH_EXTENSION_NAME
                                            , pattern ARM_DATA_GRAPH_EXTENSION_NAME
                                            , DeferredOperationKHR(..)
                                            , DataGraphPipelineSessionARM(..)
                                            , TensorUsageFlagBitsARM(..)
                                            , TensorUsageFlagsARM
                                            , PipelineCreateFlags2KHR
                                            , MAX_PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_SET_NAME_SIZE_ARM
                                            , pattern MAX_PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_SET_NAME_SIZE_ARM
                                            ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.CStruct.Utils (FixedArray)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Utils (maybePeek)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Numeric (showHex)
import Data.ByteString (packCString)
import Data.ByteString (useAsCString)
import Data.Coerce (coerce)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import qualified Data.Vector (null)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.C.Types (CSize)
import Foreign.C.Types (CSize(..))
import Foreign.C.Types (CSize(CSize))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Word (Word8)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.CStruct.Extends (peekSomeCStruct)
import Vulkan.CStruct.Utils (pokeFixedLengthNullTerminatedByteString)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_data_graph_model (DataGraphPipelineBuiltinModelCreateInfoQCOM)
import Vulkan.Extensions.Handles (DataGraphPipelineSessionARM)
import Vulkan.Extensions.Handles (DataGraphPipelineSessionARM(..))
import Vulkan.Extensions.Handles (DeferredOperationKHR)
import Vulkan.Extensions.Handles (DeferredOperationKHR(..))
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkBindDataGraphPipelineSessionMemoryARM))
import Vulkan.Dynamic (DeviceCmds(pVkCmdDispatchDataGraphARM))
import Vulkan.Dynamic (DeviceCmds(pVkCreateDataGraphPipelineSessionARM))
import Vulkan.Dynamic (DeviceCmds(pVkCreateDataGraphPipelinesARM))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyDataGraphPipelineSessionARM))
import Vulkan.Dynamic (DeviceCmds(pVkGetDataGraphPipelineAvailablePropertiesARM))
import Vulkan.Dynamic (DeviceCmds(pVkGetDataGraphPipelinePropertiesARM))
import Vulkan.Dynamic (DeviceCmds(pVkGetDataGraphPipelineSessionBindPointRequirementsARM))
import Vulkan.Dynamic (DeviceCmds(pVkGetDataGraphPipelineSessionMemoryRequirementsARM))
import Vulkan.Core10.Handles (DeviceMemory)
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlags)
import Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits (ExternalSemaphoreHandleTypeFlags)
import Vulkan.Core10.FundamentalTypes (Flags64)
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceQueueFamilyDataGraphProcessingEnginePropertiesARM))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceQueueFamilyDataGraphPropertiesARM))
import Vulkan.Core10.APIConstants (MAX_PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_SET_NAME_SIZE_ARM)
import Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (MemoryRequirements2)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice(PhysicalDevice))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Handles (Pipeline)
import Vulkan.Core10.Handles (Pipeline(..))
import Vulkan.Core10.Handles (PipelineCache)
import Vulkan.Core10.Handles (PipelineCache(..))
import Vulkan.Extensions.VK_KHR_maintenance5 (PipelineCreateFlags2KHR)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_EXT_pipeline_creation_feedback (PipelineCreationFeedbackCreateInfo)
import Vulkan.Core10.Handles (PipelineLayout)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Handles (ShaderModule)
import {-# SOURCE #-} Vulkan.Core10.Shader (ShaderModuleCreateInfo)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.ComputePipeline (SpecializationInfo)
import Vulkan.Core10.Enums.StructureType (StructureType)
import {-# SOURCE #-} Vulkan.Extensions.VK_ARM_tensors (TensorDescriptionARM)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_DATA_GRAPH_PIPELINE_SESSION_MEMORY_INFO_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_COMPILER_CONTROL_CREATE_INFO_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_CONSTANT_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_CONSTANT_TENSOR_SEMI_STRUCTURED_SPARSITY_INFO_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_CREATE_INFO_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_DISPATCH_INFO_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_IDENTIFIER_CREATE_INFO_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_INFO_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_PROPERTY_QUERY_RESULT_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_RESOURCE_INFO_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SESSION_BIND_POINT_REQUIREMENTS_INFO_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SESSION_BIND_POINT_REQUIREMENT_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SESSION_CREATE_INFO_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SESSION_MEMORY_REQUIREMENTS_INFO_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SHADER_MODULE_CREATE_INFO_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DATA_GRAPH_PROCESSING_ENGINE_CREATE_INFO_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DATA_GRAPH_FEATURES_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_QUEUE_FAMILY_DATA_GRAPH_PROCESSING_ENGINE_INFO_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_QUEUE_FAMILY_DATA_GRAPH_PROCESSING_ENGINE_PROPERTIES_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_QUEUE_FAMILY_DATA_GRAPH_PROPERTIES_ARM))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (DataGraphPipelineSessionARM(..))
import Vulkan.Extensions.Handles (DeferredOperationKHR(..))
import Vulkan.Core10.APIConstants (MAX_PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_SET_NAME_SIZE_ARM)
import Vulkan.Extensions.VK_KHR_maintenance5 (PipelineCreateFlags2KHR)
import Vulkan.Extensions.VK_ARM_tensors (TensorUsageFlagBitsARM(..))
import Vulkan.Extensions.VK_ARM_tensors (TensorUsageFlagsARM)
import Vulkan.Core10.APIConstants (pattern MAX_PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_SET_NAME_SIZE_ARM)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDataGraphPipelinesARM
  :: FunPtr (Ptr Device_T -> DeferredOperationKHR -> PipelineCache -> Word32 -> Ptr (SomeStruct DataGraphPipelineCreateInfoARM) -> Ptr AllocationCallbacks -> Ptr Pipeline -> IO Result) -> Ptr Device_T -> DeferredOperationKHR -> PipelineCache -> Word32 -> Ptr (SomeStruct DataGraphPipelineCreateInfoARM) -> Ptr AllocationCallbacks -> Ptr Pipeline -> IO Result

-- | vkCreateDataGraphPipelinesARM - Create data graph pipeline objects
--
-- = Description
--
-- The implementation will create a pipeline in each element of
-- @pPipelines@ from the corresponding element of @pCreateInfos@. If the
-- creation of any pipeline fails, that pipeline will be set to
-- 'Vulkan.Core10.APIConstants.NULL_HANDLE'.
--
-- == Valid Usage
--
-- -   #VUID-vkCreateDataGraphPipelinesARM-dataGraph-09760# The
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-dataGraph dataGraph>
--     feature /must/ be enabled
--
-- -   #VUID-vkCreateDataGraphPipelinesARM-device-09927# @device@ /must/
--     support at least one queue family with the
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_DATA_GRAPH_BIT_ARM'
--     capability
--
-- -   #VUID-vkCreateDataGraphPipelinesARM-deferredOperation-09761#
--     @deferredOperation@ /must/ be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkCreateDataGraphPipelinesARM-deferredOperation-09916# If
--     @deferredOperation@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     the @flags@ member of elements of @pCreateInfos@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT'
--
-- -   #VUID-vkCreateDataGraphPipelinesARM-pNext-09928# If at least one of
--     the 'DataGraphPipelineCreateInfoARM' includes a
--     'DataGraphPipelineIdentifierCreateInfoARM' structure in its @pNext@
--     chain then @pipelineCache@ /must/ not be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkCreateDataGraphPipelinesARM-pipelineCache-09762# If
--     @pipelineCache@ was created with
--     'Vulkan.Core10.Enums.PipelineCacheCreateFlagBits.PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT',
--     host access to @pipelineCache@ /must/ be
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#fundamentals-threadingbehavior externally synchronized>
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateDataGraphPipelinesARM-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateDataGraphPipelinesARM-deferredOperation-parameter# If
--     @deferredOperation@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @deferredOperation@ /must/ be a valid
--     'Vulkan.Extensions.Handles.DeferredOperationKHR' handle
--
-- -   #VUID-vkCreateDataGraphPipelinesARM-pipelineCache-parameter# If
--     @pipelineCache@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @pipelineCache@ /must/ be a valid
--     'Vulkan.Core10.Handles.PipelineCache' handle
--
-- -   #VUID-vkCreateDataGraphPipelinesARM-pCreateInfos-parameter#
--     @pCreateInfos@ /must/ be a valid pointer to an array of
--     @createInfoCount@ valid 'DataGraphPipelineCreateInfoARM' structures
--
-- -   #VUID-vkCreateDataGraphPipelinesARM-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkCreateDataGraphPipelinesARM-pPipelines-parameter#
--     @pPipelines@ /must/ be a valid pointer to an array of
--     @createInfoCount@ 'Vulkan.Core10.Handles.Pipeline' handles
--
-- -   #VUID-vkCreateDataGraphPipelinesARM-device-queuecount# The device
--     /must/ have been created with at least @1@ queue
--
-- -   #VUID-vkCreateDataGraphPipelinesARM-createInfoCount-arraylength#
--     @createInfoCount@ /must/ be greater than @0@
--
-- -   #VUID-vkCreateDataGraphPipelinesARM-deferredOperation-parent# If
--     @deferredOperation@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- -   #VUID-vkCreateDataGraphPipelinesARM-pipelineCache-parent# If
--     @pipelineCache@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Extensions.VK_EXT_pipeline_creation_cache_control.PIPELINE_COMPILE_REQUIRED_EXT'
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'DataGraphPipelineCreateInfoARM',
-- 'Vulkan.Extensions.Handles.DeferredOperationKHR',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Pipeline',
-- 'Vulkan.Core10.Handles.PipelineCache'
createDataGraphPipelinesARM :: forall io
                             . (MonadIO io)
                            => -- | @device@ is the logical device that creates the data graph pipelines.
                               Device
                            -> -- | @deferredOperation@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE' or the
                               -- handle of a valid 'Vulkan.Extensions.Handles.DeferredOperationKHR'
                               -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#deferred-host-operations-requesting request deferral>
                               -- object for this command.
                               DeferredOperationKHR
                            -> -- | @pipelineCache@ is either 'Vulkan.Core10.APIConstants.NULL_HANDLE',
                               -- indicating that pipeline caching is disabled; or the handle of a valid
                               -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-cache pipeline cache>
                               -- object, in which case use of that cache is enabled for the duration of
                               -- the command.
                               PipelineCache
                            -> -- | @pCreateInfos@ is a pointer to an array of
                               -- 'DataGraphPipelineCreateInfoARM' structures.
                               ("createInfos" ::: Vector (SomeStruct DataGraphPipelineCreateInfoARM))
                            -> -- | @pAllocator@ controls host memory allocation as described in the
                               -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#memory-allocation Memory Allocation>
                               -- chapter.
                               ("allocator" ::: Maybe AllocationCallbacks)
                            -> io (Result, ("pipelines" ::: Vector Pipeline))
createDataGraphPipelinesARM device
                              deferredOperation
                              pipelineCache
                              createInfos
                              allocator = liftIO . evalContT $ do
  let vkCreateDataGraphPipelinesARMPtr = pVkCreateDataGraphPipelinesARM (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCreateDataGraphPipelinesARMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateDataGraphPipelinesARM is null" Nothing Nothing
  let vkCreateDataGraphPipelinesARM' = mkVkCreateDataGraphPipelinesARM vkCreateDataGraphPipelinesARMPtr
  pPCreateInfos <- ContT $ allocaBytes @(DataGraphPipelineCreateInfoARM _) ((Data.Vector.length (createInfos)) * 48)
  Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPCreateInfos `plusPtr` (48 * (i)) :: Ptr (DataGraphPipelineCreateInfoARM _))) (e) . ($ ())) (createInfos)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPPipelines <- ContT $ bracket (callocBytes @Pipeline ((fromIntegral ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32))) * 8)) free
  r <- lift $ traceAroundEvent "vkCreateDataGraphPipelinesARM" (vkCreateDataGraphPipelinesARM'
                                                                  (deviceHandle (device))
                                                                  (deferredOperation)
                                                                  (pipelineCache)
                                                                  ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32))
                                                                  (forgetExtensions (pPCreateInfos))
                                                                  pAllocator
                                                                  (pPPipelines))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPipelines <- lift $ generateM (fromIntegral ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32))) (\i -> peek @Pipeline ((pPPipelines `advancePtrBytes` (8 * (i)) :: Ptr Pipeline)))
  pure $ (r, pPipelines)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDataGraphPipelineSessionARM
  :: FunPtr (Ptr Device_T -> Ptr DataGraphPipelineSessionCreateInfoARM -> Ptr AllocationCallbacks -> Ptr DataGraphPipelineSessionARM -> IO Result) -> Ptr Device_T -> Ptr DataGraphPipelineSessionCreateInfoARM -> Ptr AllocationCallbacks -> Ptr DataGraphPipelineSessionARM -> IO Result

-- | vkCreateDataGraphPipelineSessionARM - Create a data graph pipeline
-- session
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateDataGraphPipelineSessionARM-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateDataGraphPipelineSessionARM-pCreateInfo-parameter#
--     @pCreateInfo@ /must/ be a valid pointer to a valid
--     'DataGraphPipelineSessionCreateInfoARM' structure
--
-- -   #VUID-vkCreateDataGraphPipelineSessionARM-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkCreateDataGraphPipelineSessionARM-pSession-parameter#
--     @pSession@ /must/ be a valid pointer to a
--     'Vulkan.Extensions.Handles.DataGraphPipelineSessionARM' handle
--
-- -   #VUID-vkCreateDataGraphPipelineSessionARM-device-queuecount# The
--     device /must/ have been created with at least @1@ queue
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Extensions.Handles.DataGraphPipelineSessionARM',
-- 'DataGraphPipelineSessionCreateInfoARM', 'Vulkan.Core10.Handles.Device'
createDataGraphPipelineSessionARM :: forall io
                                   . (MonadIO io)
                                  => -- | @device@ is the logical device that creates the data graph pipeline
                                     -- session.
                                     Device
                                  -> -- | @pCreateInfo@ is a pointer to a 'DataGraphPipelineSessionCreateInfoARM'
                                     -- structure.
                                     DataGraphPipelineSessionCreateInfoARM
                                  -> -- | @pAllocator@ controls host memory allocation as described in the
                                     -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#memory-allocation Memory Allocation>
                                     -- chapter.
                                     ("allocator" ::: Maybe AllocationCallbacks)
                                  -> io (DataGraphPipelineSessionARM)
createDataGraphPipelineSessionARM device
                                    createInfo
                                    allocator = liftIO . evalContT $ do
  let vkCreateDataGraphPipelineSessionARMPtr = pVkCreateDataGraphPipelineSessionARM (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCreateDataGraphPipelineSessionARMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateDataGraphPipelineSessionARM is null" Nothing Nothing
  let vkCreateDataGraphPipelineSessionARM' = mkVkCreateDataGraphPipelineSessionARM vkCreateDataGraphPipelineSessionARMPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSession <- ContT $ bracket (callocBytes @DataGraphPipelineSessionARM 8) free
  r <- lift $ traceAroundEvent "vkCreateDataGraphPipelineSessionARM" (vkCreateDataGraphPipelineSessionARM'
                                                                        (deviceHandle (device))
                                                                        pCreateInfo
                                                                        pAllocator
                                                                        (pPSession))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSession <- lift $ peek @DataGraphPipelineSessionARM pPSession
  pure $ (pSession)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createDataGraphPipelineSessionARM' and
-- 'destroyDataGraphPipelineSessionARM'
--
-- To ensure that 'destroyDataGraphPipelineSessionARM' is always called:
-- pass 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withDataGraphPipelineSessionARM :: forall io r . MonadIO io => Device -> DataGraphPipelineSessionCreateInfoARM -> Maybe AllocationCallbacks -> (io DataGraphPipelineSessionARM -> (DataGraphPipelineSessionARM -> io ()) -> r) -> r
withDataGraphPipelineSessionARM device pCreateInfo pAllocator b =
  b (createDataGraphPipelineSessionARM device pCreateInfo pAllocator)
    (\(o0) -> destroyDataGraphPipelineSessionARM device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDataGraphPipelineSessionBindPointRequirementsARM
  :: FunPtr (Ptr Device_T -> Ptr DataGraphPipelineSessionBindPointRequirementsInfoARM -> Ptr Word32 -> Ptr DataGraphPipelineSessionBindPointRequirementARM -> IO Result) -> Ptr Device_T -> Ptr DataGraphPipelineSessionBindPointRequirementsInfoARM -> Ptr Word32 -> Ptr DataGraphPipelineSessionBindPointRequirementARM -> IO Result

-- | vkGetDataGraphPipelineSessionBindPointRequirementsARM - Get the bind
-- point requirements of a data graph pipeline session
--
-- = Description
--
-- If @pBindPointRequirements@ is @NULL@, then the number of bind points
-- associated with the data graph pipeline session is returned in
-- @pBindPointRequirementCount@. Otherwise, @pBindPointRequirementCount@
-- /must/ point to a variable set by the user to the number of elements in
-- the @pBindPointRequirements@ array, and on return the variable is
-- overwritten with the number of structures actually written to
-- @pBindPointRequirements@. If @pBindPointRequirementCount@ is less than
-- the number of bind points associated with the data graph pipeline
-- session, at most @pBindPointRequirementCount@ structures will be
-- written, and 'Vulkan.Core10.Enums.Result.INCOMPLETE' will be returned
-- instead of 'Vulkan.Core10.Enums.Result.SUCCESS', to indicate that not
-- all the required bind points were returned.
--
-- == Valid Usage
--
-- -   #VUID-vkGetDataGraphPipelineSessionBindPointRequirementsARM-session-09783#
--     The @session@ member of @pInfo@ /must/ have been created with
--     @device@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetDataGraphPipelineSessionBindPointRequirementsARM-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetDataGraphPipelineSessionBindPointRequirementsARM-pInfo-parameter#
--     @pInfo@ /must/ be a valid pointer to a valid
--     'DataGraphPipelineSessionBindPointRequirementsInfoARM' structure
--
-- -   #VUID-vkGetDataGraphPipelineSessionBindPointRequirementsARM-pBindPointRequirementCount-parameter#
--     @pBindPointRequirementCount@ /must/ be a valid pointer to a
--     @uint32_t@ value
--
-- -   #VUID-vkGetDataGraphPipelineSessionBindPointRequirementsARM-pBindPointRequirements-parameter#
--     If the value referenced by @pBindPointRequirementCount@ is not @0@,
--     and @pBindPointRequirements@ is not @NULL@, @pBindPointRequirements@
--     /must/ be a valid pointer to an array of
--     @pBindPointRequirementCount@
--     'DataGraphPipelineSessionBindPointRequirementARM' structures
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.INCOMPLETE'
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- 'DataGraphPipelineSessionBindPointRequirementARM',
-- 'DataGraphPipelineSessionBindPointRequirementsInfoARM',
-- 'Vulkan.Core10.Handles.Device'
getDataGraphPipelineSessionBindPointRequirementsARM :: forall io
                                                     . (MonadIO io)
                                                    => -- | @device@ is the logical device that owns the data graph pipeline
                                                       -- session.
                                                       Device
                                                    -> -- | @pInfo@ is a pointer to a
                                                       -- 'DataGraphPipelineSessionBindPointRequirementsInfoARM' structure
                                                       -- containing parameters for the bind point requirements query.
                                                       DataGraphPipelineSessionBindPointRequirementsInfoARM
                                                    -> io (Result, ("bindPointRequirements" ::: Vector DataGraphPipelineSessionBindPointRequirementARM))
getDataGraphPipelineSessionBindPointRequirementsARM device
                                                      info = liftIO . evalContT $ do
  let vkGetDataGraphPipelineSessionBindPointRequirementsARMPtr = pVkGetDataGraphPipelineSessionBindPointRequirementsARM (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetDataGraphPipelineSessionBindPointRequirementsARMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDataGraphPipelineSessionBindPointRequirementsARM is null" Nothing Nothing
  let vkGetDataGraphPipelineSessionBindPointRequirementsARM' = mkVkGetDataGraphPipelineSessionBindPointRequirementsARM vkGetDataGraphPipelineSessionBindPointRequirementsARMPtr
  let device' = deviceHandle (device)
  pInfo <- ContT $ withCStruct (info)
  pPBindPointRequirementCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkGetDataGraphPipelineSessionBindPointRequirementsARM" (vkGetDataGraphPipelineSessionBindPointRequirementsARM'
                                                                                          device'
                                                                                          pInfo
                                                                                          (pPBindPointRequirementCount)
                                                                                          (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pBindPointRequirementCount <- lift $ peek @Word32 pPBindPointRequirementCount
  pPBindPointRequirements <- ContT $ bracket (callocBytes @DataGraphPipelineSessionBindPointRequirementARM ((fromIntegral (pBindPointRequirementCount)) * 32)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPBindPointRequirements `advancePtrBytes` (i * 32) :: Ptr DataGraphPipelineSessionBindPointRequirementARM) . ($ ())) [0..(fromIntegral (pBindPointRequirementCount)) - 1]
  r' <- lift $ traceAroundEvent "vkGetDataGraphPipelineSessionBindPointRequirementsARM" (vkGetDataGraphPipelineSessionBindPointRequirementsARM'
                                                                                           device'
                                                                                           pInfo
                                                                                           (pPBindPointRequirementCount)
                                                                                           ((pPBindPointRequirements)))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pBindPointRequirementCount' <- lift $ peek @Word32 pPBindPointRequirementCount
  pBindPointRequirements' <- lift $ generateM (fromIntegral (pBindPointRequirementCount')) (\i -> peekCStruct @DataGraphPipelineSessionBindPointRequirementARM (((pPBindPointRequirements) `advancePtrBytes` (32 * (i)) :: Ptr DataGraphPipelineSessionBindPointRequirementARM)))
  pure $ ((r'), pBindPointRequirements')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDataGraphPipelineSessionMemoryRequirementsARM
  :: FunPtr (Ptr Device_T -> Ptr DataGraphPipelineSessionMemoryRequirementsInfoARM -> Ptr (SomeStruct MemoryRequirements2) -> IO ()) -> Ptr Device_T -> Ptr DataGraphPipelineSessionMemoryRequirementsInfoARM -> Ptr (SomeStruct MemoryRequirements2) -> IO ()

-- | vkGetDataGraphPipelineSessionMemoryRequirementsARM - Get the memory
-- requirements of a data graph pipeline session
--
-- == Valid Usage
--
-- -   #VUID-vkGetDataGraphPipelineSessionMemoryRequirementsARM-session-09950#
--     The @session@ member of @pInfo@ /must/ have been created with
--     @device@
--
-- -   #VUID-vkGetDataGraphPipelineSessionMemoryRequirementsARM-bindPoint-09784#
--     The @bindPoint@ member of @pInfo@ /must/ have been returned as part
--     of a 'DataGraphPipelineSessionBindPointRequirementARM' whose
--     @bindPointType@ member is
--     'DATA_GRAPH_PIPELINE_SESSION_BIND_POINT_TYPE_MEMORY_ARM' by a prior
--     call to 'getDataGraphPipelineSessionBindPointRequirementsARM' for
--     the @session@ member of @pInfo@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetDataGraphPipelineSessionMemoryRequirementsARM-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetDataGraphPipelineSessionMemoryRequirementsARM-pInfo-parameter#
--     @pInfo@ /must/ be a valid pointer to a valid
--     'DataGraphPipelineSessionMemoryRequirementsInfoARM' structure
--
-- -   #VUID-vkGetDataGraphPipelineSessionMemoryRequirementsARM-pMemoryRequirements-parameter#
--     @pMemoryRequirements@ /must/ be a valid pointer to a
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.MemoryRequirements2'
--     structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- 'DataGraphPipelineSessionMemoryRequirementsInfoARM',
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.MemoryRequirements2'
getDataGraphPipelineSessionMemoryRequirementsARM :: forall a io
                                                  . ( Extendss MemoryRequirements2 a
                                                    , PokeChain a
                                                    , PeekChain a
                                                    , MonadIO io )
                                                 => -- | @device@ is the logical device that owns the data graph pipeline
                                                    -- session.
                                                    Device
                                                 -> -- | @pInfo@ is a pointer to a
                                                    -- 'DataGraphPipelineSessionMemoryRequirementsInfoARM' structure containing
                                                    -- parameters for the memory requirements query.
                                                    DataGraphPipelineSessionMemoryRequirementsInfoARM
                                                 -> io (MemoryRequirements2 a)
getDataGraphPipelineSessionMemoryRequirementsARM device
                                                   info = liftIO . evalContT $ do
  let vkGetDataGraphPipelineSessionMemoryRequirementsARMPtr = pVkGetDataGraphPipelineSessionMemoryRequirementsARM (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetDataGraphPipelineSessionMemoryRequirementsARMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDataGraphPipelineSessionMemoryRequirementsARM is null" Nothing Nothing
  let vkGetDataGraphPipelineSessionMemoryRequirementsARM' = mkVkGetDataGraphPipelineSessionMemoryRequirementsARM vkGetDataGraphPipelineSessionMemoryRequirementsARMPtr
  pInfo <- ContT $ withCStruct (info)
  pPMemoryRequirements <- ContT (withZeroCStruct @(MemoryRequirements2 _))
  lift $ traceAroundEvent "vkGetDataGraphPipelineSessionMemoryRequirementsARM" (vkGetDataGraphPipelineSessionMemoryRequirementsARM'
                                                                                  (deviceHandle (device))
                                                                                  pInfo
                                                                                  (forgetExtensions (pPMemoryRequirements)))
  pMemoryRequirements <- lift $ peekCStruct @(MemoryRequirements2 _) pPMemoryRequirements
  pure $ (pMemoryRequirements)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkBindDataGraphPipelineSessionMemoryARM
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr BindDataGraphPipelineSessionMemoryInfoARM -> IO Result) -> Ptr Device_T -> Word32 -> Ptr BindDataGraphPipelineSessionMemoryInfoARM -> IO Result

-- | vkBindDataGraphPipelineSessionMemoryARM - Bind device memory to a data
-- graph pipeline session object
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- 'BindDataGraphPipelineSessionMemoryInfoARM',
-- 'Vulkan.Core10.Handles.Device'
bindDataGraphPipelineSessionMemoryARM :: forall io
                                       . (MonadIO io)
                                      => -- | @device@ is the logical device that owns the data graph pipeline session
                                         -- and memory.
                                         --
                                         -- #VUID-vkBindDataGraphPipelineSessionMemoryARM-device-parameter# @device@
                                         -- /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                                         Device
                                      -> -- | @pBindInfos@ is a pointer to an array of
                                         -- 'BindDataGraphPipelineSessionMemoryInfoARM' structures describing graph
                                         -- pipeline sessions and memory to bind.
                                         --
                                         -- #VUID-vkBindDataGraphPipelineSessionMemoryARM-pBindInfos-parameter#
                                         -- @pBindInfos@ /must/ be a valid pointer to an array of @bindInfoCount@
                                         -- valid 'BindDataGraphPipelineSessionMemoryInfoARM' structures
                                         ("bindInfos" ::: Vector BindDataGraphPipelineSessionMemoryInfoARM)
                                      -> io ()
bindDataGraphPipelineSessionMemoryARM device bindInfos = liftIO . evalContT $ do
  let vkBindDataGraphPipelineSessionMemoryARMPtr = pVkBindDataGraphPipelineSessionMemoryARM (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkBindDataGraphPipelineSessionMemoryARMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkBindDataGraphPipelineSessionMemoryARM is null" Nothing Nothing
  let vkBindDataGraphPipelineSessionMemoryARM' = mkVkBindDataGraphPipelineSessionMemoryARM vkBindDataGraphPipelineSessionMemoryARMPtr
  pPBindInfos <- ContT $ allocaBytes @BindDataGraphPipelineSessionMemoryInfoARM ((Data.Vector.length (bindInfos)) * 48)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPBindInfos `plusPtr` (48 * (i)) :: Ptr BindDataGraphPipelineSessionMemoryInfoARM) (e)) (bindInfos)
  r <- lift $ traceAroundEvent "vkBindDataGraphPipelineSessionMemoryARM" (vkBindDataGraphPipelineSessionMemoryARM'
                                                                            (deviceHandle (device))
                                                                            ((fromIntegral (Data.Vector.length $ (bindInfos)) :: Word32))
                                                                            (pPBindInfos))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyDataGraphPipelineSessionARM
  :: FunPtr (Ptr Device_T -> DataGraphPipelineSessionARM -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> DataGraphPipelineSessionARM -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyDataGraphPipelineSessionARM - Destroy a data graph pipeline
-- session object
--
-- == Valid Usage
--
-- -   #VUID-vkDestroyDataGraphPipelineSessionARM-session-09793# All
--     submitted commands that refer to @session@ /must/ have completed
--     execution
--
-- -   #VUID-vkDestroyDataGraphPipelineSessionARM-session-09794# If
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @session@ was created, a compatible set of callbacks
--     /must/ be provided here
--
-- -   #VUID-vkDestroyDataGraphPipelineSessionARM-session-09795# If no
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @session@ was created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroyDataGraphPipelineSessionARM-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDestroyDataGraphPipelineSessionARM-session-parameter#
--     @session@ /must/ be a valid
--     'Vulkan.Extensions.Handles.DataGraphPipelineSessionARM' handle
--
-- -   #VUID-vkDestroyDataGraphPipelineSessionARM-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkDestroyDataGraphPipelineSessionARM-session-parent# @session@
--     /must/ have been created, allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @session@ /must/ be externally synchronized
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Extensions.Handles.DataGraphPipelineSessionARM',
-- 'Vulkan.Core10.Handles.Device'
destroyDataGraphPipelineSessionARM :: forall io
                                    . (MonadIO io)
                                   => -- | @device@ is the logical device that destroys the data graph pipeline
                                      -- session.
                                      Device
                                   -> -- | @session@ is the handle of the data graph pipeline session to destroy.
                                      DataGraphPipelineSessionARM
                                   -> -- | @pAllocator@ controls host memory allocation as described in the
                                      -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#memory-allocation Memory Allocation>
                                      -- chapter.
                                      ("allocator" ::: Maybe AllocationCallbacks)
                                   -> io ()
destroyDataGraphPipelineSessionARM device
                                     session
                                     allocator = liftIO . evalContT $ do
  let vkDestroyDataGraphPipelineSessionARMPtr = pVkDestroyDataGraphPipelineSessionARM (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkDestroyDataGraphPipelineSessionARMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyDataGraphPipelineSessionARM is null" Nothing Nothing
  let vkDestroyDataGraphPipelineSessionARM' = mkVkDestroyDataGraphPipelineSessionARM vkDestroyDataGraphPipelineSessionARMPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyDataGraphPipelineSessionARM" (vkDestroyDataGraphPipelineSessionARM'
                                                                    (deviceHandle (device))
                                                                    (session)
                                                                    pAllocator)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDispatchDataGraphARM
  :: FunPtr (Ptr CommandBuffer_T -> DataGraphPipelineSessionARM -> Ptr DataGraphPipelineDispatchInfoARM -> IO ()) -> Ptr CommandBuffer_T -> DataGraphPipelineSessionARM -> Ptr DataGraphPipelineDispatchInfoARM -> IO ()

-- | vkCmdDispatchDataGraphARM - Dispatch a data graph pipeline within a
-- session
--
-- == Valid Usage
--
-- -   #VUID-vkCmdDispatchDataGraphARM-session-09796# For each of the
--     session bind point requirements returned by
--     'getDataGraphPipelineSessionBindPointRequirementsARM' for @session@,
--     'DataGraphPipelineSessionBindPointRequirementARM'::@numObjects@
--     objects /must/ have been bound to @session@
--
-- -   #VUID-vkCmdDispatchDataGraphARM-dataGraphPipeline-09951# The
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command /must/ be identical to the @dataGraphPipeline@
--     used to create @session@
--
-- -   #VUID-vkCmdDispatchDataGraphARM-None-09797# For each set /n/ that is
--     statically used by a bound data graph pipeline, a descriptor set
--     /must/ have been bound to /n/ at the same pipeline bind point, with
--     a 'Vulkan.Core10.Handles.PipelineLayout' that is compatible for set
--     /n/, with the 'Vulkan.Core10.Handles.PipelineLayout' used to create
--     the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptorsets-compatibility>
--
-- -   #VUID-vkCmdDispatchDataGraphARM-None-09935# Descriptors in each
--     bound descriptor set, specified via
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets', /must/
--     be valid as described by
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptor-validity descriptor validity>
--     if they are statically used by the 'Vulkan.Core10.Handles.Pipeline'
--     bound to the pipeline bind point used by this command and the bound
--     'Vulkan.Core10.Handles.Pipeline' was not created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdDispatchDataGraphARM-None-09936# If the descriptors used
--     by the 'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind
--     point were specified via
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets', the
--     bound 'Vulkan.Core10.Handles.Pipeline' /must/ have been created
--     without
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdDispatchDataGraphARM-None-09937# Descriptors in bound
--     descriptor buffers, specified via
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.cmdSetDescriptorBufferOffsetsEXT',
--     /must/ be valid if they are dynamically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command and the bound 'Vulkan.Core10.Handles.Pipeline'
--     was created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdDispatchDataGraphARM-None-09938# If the descriptors used
--     by the 'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind
--     point were specified via
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.cmdSetDescriptorBufferOffsetsEXT',
--     the bound 'Vulkan.Core10.Handles.Pipeline' /must/ have been created
--     with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdDispatchDataGraphARM-None-09939# If a descriptor is
--     dynamically used with a 'Vulkan.Core10.Handles.Pipeline' created
--     with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT',
--     the descriptor memory /must/ be resident
--
-- -   #VUID-vkCmdDispatchDataGraphARM-None-09799# A valid data graph
--     pipeline /must/ be bound to the
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_DATA_GRAPH_ARM'
--     pipeline bind point used by this command
--
-- -   #VUID-vkCmdDispatchDataGraphARM-pDescription-09930# If a
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_TENSOR_ARM'
--     descriptor is accessed as a result of this command, then the
--     underlying 'Vulkan.Extensions.Handles.TensorARM' object /must/ have
--     been created with a
--     'Vulkan.Extensions.VK_ARM_tensors.TensorCreateInfoARM'::@pDescription@
--     whose @usage@ member contained
--     'Vulkan.Extensions.VK_ARM_tensors.TENSOR_USAGE_DATA_GRAPH_BIT_ARM'
--
-- -   #VUID-vkCmdDispatchDataGraphARM-pipeline-09940# If the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command was created with a
--     'DataGraphProcessingEngineCreateInfoARM' structure in the @pNext@
--     chain of 'DataGraphPipelineCreateInfoARM' that included a foreign
--     data graph processing engine in its @pProcessingEngines@ member,
--     then all
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_TENSOR_ARM'
--     descriptors accessed as a result of this command /must/ be
--     'Vulkan.Extensions.Handles.TensorARM' objects that have been bound
--     to memory allocated with
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExportMemoryAllocateInfo'::@handleTypes@
--     with set bits that are a subset of the bits in
--     'QueueFamilyDataGraphProcessingEnginePropertiesARM'::@foreignMemoryHandleTypes@
--     structure queried via
--     'getPhysicalDeviceQueueFamilyDataGraphProcessingEnginePropertiesARM'
--     with a @queueFamilyIndex@ matching the one the command pool used to
--     create @commandBuffer@ was created for and an identical
--     @engineType@, for all the foreign data graph processing engines that
--     were part of the 'DataGraphProcessingEngineCreateInfoARM' used to
--     create the 'Vulkan.Core10.Handles.Pipeline'
--
-- -   #VUID-vkCmdDispatchDataGraphARM-pNext-09952# If the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command was created with a
--     'DataGraphProcessingEngineCreateInfoARM' structure in the @pNext@
--     chain of 'DataGraphPipelineCreateInfoARM' that included a foreign
--     data graph processing engine in its @pProcessingEngines@ member,
--     then all @session@ bound memory /must/ have been allocated with
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExportMemoryAllocateInfo'::@handleTypes@
--     with set bits that are a subset of the bits in
--     'QueueFamilyDataGraphProcessingEnginePropertiesARM'::@foreignMemoryHandleTypes@
--     structure queried via
--     'getPhysicalDeviceQueueFamilyDataGraphProcessingEnginePropertiesARM'
--     with a @queueFamilyIndex@ matching the one the command pool used to
--     create @commandBuffer@ was created for and an identical
--     @engineType@, for all the foreign data graph processing engines that
--     were part of the 'DataGraphProcessingEngineCreateInfoARM' used to
--     create the 'Vulkan.Core10.Handles.Pipeline'
--
-- -   #VUID-vkCmdDispatchDataGraphARM-commandBuffer-09800# If
--     @commandBuffer@ is an unprotected command buffer and
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, any resource accessed by bound data graph
--     pipelines /must/ not be a protected resource
--
-- -   #VUID-vkCmdDispatchDataGraphARM-commandBuffer-09801# If
--     @commandBuffer@ is a protected command buffer and
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, any resource written to by the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the bind point used
--     by this command /must/ not be an unprotected resource
--
-- -   #VUID-vkCmdDispatchDataGraphARM-commandBuffer-09941# All the
--     operations used by the bound data graph pipeline /must/ be supported
--     on the queue family for which the command pool out of which
--     @commandBuffer@ was allocated, as reported by
--     'getPhysicalDeviceQueueFamilyDataGraphPropertiesARM'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdDispatchDataGraphARM-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdDispatchDataGraphARM-session-parameter# @session@ /must/
--     be a valid 'Vulkan.Extensions.Handles.DataGraphPipelineSessionARM'
--     handle
--
-- -   #VUID-vkCmdDispatchDataGraphARM-pInfo-parameter# If @pInfo@ is not
--     @NULL@, @pInfo@ /must/ be a valid pointer to a valid
--     'DataGraphPipelineDispatchInfoARM' structure
--
-- -   #VUID-vkCmdDispatchDataGraphARM-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdDispatchDataGraphARM-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_DATA_GRAPH_BIT_ARM'
--     operations
--
-- -   #VUID-vkCmdDispatchDataGraphARM-renderpass# This command /must/ only
--     be called outside of a render pass instance
--
-- -   #VUID-vkCmdDispatchDataGraphARM-suspended# This command /must/ not
--     be called between suspended render pass instances
--
-- -   #VUID-vkCmdDispatchDataGraphARM-videocoding# This command /must/
--     only be called outside of a video coding scope
--
-- -   #VUID-vkCmdDispatchDataGraphARM-commonparent# Both of
--     @commandBuffer@, and @session@ /must/ have been created, allocated,
--     or retrieved from the same 'Vulkan.Core10.Handles.Device'
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | VK_QUEUE_DATA_GRAPH_BIT_ARM                                                                                           | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdDispatchDataGraphARM is affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'DataGraphPipelineDispatchInfoARM',
-- 'Vulkan.Extensions.Handles.DataGraphPipelineSessionARM'
cmdDispatchDataGraphARM :: forall io
                         . (MonadIO io)
                        => -- | @commandBuffer@ is the command buffer into which the command will be
                           -- recorded.
                           CommandBuffer
                        -> -- | @session@ is the 'Vulkan.Extensions.Handles.DataGraphPipelineSessionARM'
                           -- that data graph pipeline being dispatched will use.
                           DataGraphPipelineSessionARM
                        -> -- | @pInfo@ is @NULL@ or a pointer to a 'DataGraphPipelineDispatchInfoARM'
                           -- structure.
                           ("info" ::: Maybe DataGraphPipelineDispatchInfoARM)
                        -> io ()
cmdDispatchDataGraphARM commandBuffer session info = liftIO . evalContT $ do
  let vkCmdDispatchDataGraphARMPtr = pVkCmdDispatchDataGraphARM (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdDispatchDataGraphARMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDispatchDataGraphARM is null" Nothing Nothing
  let vkCmdDispatchDataGraphARM' = mkVkCmdDispatchDataGraphARM vkCmdDispatchDataGraphARMPtr
  pInfo <- case (info) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkCmdDispatchDataGraphARM" (vkCmdDispatchDataGraphARM'
                                                         (commandBufferHandle (commandBuffer))
                                                         (session)
                                                         pInfo)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDataGraphPipelineAvailablePropertiesARM
  :: FunPtr (Ptr Device_T -> Ptr DataGraphPipelineInfoARM -> Ptr Word32 -> Ptr DataGraphPipelinePropertyARM -> IO Result) -> Ptr Device_T -> Ptr DataGraphPipelineInfoARM -> Ptr Word32 -> Ptr DataGraphPipelinePropertyARM -> IO Result

-- | vkGetDataGraphPipelineAvailablePropertiesARM - Query available
-- properties of a data graph pipeline
--
-- = Description
--
-- If @pProperties@ is @NULL@, then the number of properties associated
-- with the data graph pipeline is returned in @pPropertiesCount@.
-- Otherwise, @pPropertiesCount@ /must/ point to a variable set by the user
-- to the number of elements in the @pProperties@ array, and on return the
-- variable is overwritten with the number of enums actually written to
-- @pProperties@. If @pPropertiesCount@ is less than the number of
-- properties associated with the data graph pipeline, at most
-- @pPropertiesCount@ structures will be written, and
-- 'Vulkan.Core10.Enums.Result.INCOMPLETE' will be returned instead of
-- 'Vulkan.Core10.Enums.Result.SUCCESS', to indicate that not all the
-- available properties were returned.
--
-- == Valid Usage
--
-- -   #VUID-vkGetDataGraphPipelineAvailablePropertiesARM-dataGraphPipeline-09888#
--     The @dataGraphPipeline@ member of @pPipelineInfo@ /must/ have been
--     created with @device@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetDataGraphPipelineAvailablePropertiesARM-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetDataGraphPipelineAvailablePropertiesARM-pPipelineInfo-parameter#
--     @pPipelineInfo@ /must/ be a valid pointer to a valid
--     'DataGraphPipelineInfoARM' structure
--
-- -   #VUID-vkGetDataGraphPipelineAvailablePropertiesARM-pPropertiesCount-parameter#
--     @pPropertiesCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   #VUID-vkGetDataGraphPipelineAvailablePropertiesARM-pProperties-parameter#
--     If the value referenced by @pPropertiesCount@ is not @0@, and
--     @pProperties@ is not @NULL@, @pProperties@ /must/ be a valid pointer
--     to an array of @pPropertiesCount@ 'DataGraphPipelinePropertyARM'
--     values
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.INCOMPLETE'
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- 'DataGraphPipelineInfoARM', 'DataGraphPipelinePropertyARM',
-- 'Vulkan.Core10.Handles.Device'
getDataGraphPipelineAvailablePropertiesARM :: forall io
                                            . (MonadIO io)
                                           => -- | @device@ is the logical device that created the data graph pipeline.
                                              Device
                                           -> -- | @pPipelineInfo@ is a 'DataGraphPipelineInfoARM' that describes the
                                              -- 'Vulkan.Core10.Handles.Pipeline' being queried.
                                              DataGraphPipelineInfoARM
                                           -> io (Result, ("properties" ::: Vector DataGraphPipelinePropertyARM))
getDataGraphPipelineAvailablePropertiesARM device
                                             pipelineInfo = liftIO . evalContT $ do
  let vkGetDataGraphPipelineAvailablePropertiesARMPtr = pVkGetDataGraphPipelineAvailablePropertiesARM (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetDataGraphPipelineAvailablePropertiesARMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDataGraphPipelineAvailablePropertiesARM is null" Nothing Nothing
  let vkGetDataGraphPipelineAvailablePropertiesARM' = mkVkGetDataGraphPipelineAvailablePropertiesARM vkGetDataGraphPipelineAvailablePropertiesARMPtr
  let device' = deviceHandle (device)
  pPipelineInfo <- ContT $ withCStruct (pipelineInfo)
  pPPropertiesCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkGetDataGraphPipelineAvailablePropertiesARM" (vkGetDataGraphPipelineAvailablePropertiesARM'
                                                                                 device'
                                                                                 pPipelineInfo
                                                                                 (pPPropertiesCount)
                                                                                 (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPropertiesCount <- lift $ peek @Word32 pPPropertiesCount
  pPProperties <- ContT $ bracket (callocBytes @DataGraphPipelinePropertyARM ((fromIntegral (pPropertiesCount)) * 4)) free
  r' <- lift $ traceAroundEvent "vkGetDataGraphPipelineAvailablePropertiesARM" (vkGetDataGraphPipelineAvailablePropertiesARM'
                                                                                  device'
                                                                                  pPipelineInfo
                                                                                  (pPPropertiesCount)
                                                                                  (pPProperties))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pPropertiesCount' <- lift $ peek @Word32 pPPropertiesCount
  pProperties' <- lift $ generateM (fromIntegral (pPropertiesCount')) (\i -> peek @DataGraphPipelinePropertyARM ((pPProperties `advancePtrBytes` (4 * (i)) :: Ptr DataGraphPipelinePropertyARM)))
  pure $ ((r'), pProperties')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDataGraphPipelinePropertiesARM
  :: FunPtr (Ptr Device_T -> Ptr DataGraphPipelineInfoARM -> Word32 -> Ptr DataGraphPipelinePropertyQueryResultARM -> IO Result) -> Ptr Device_T -> Ptr DataGraphPipelineInfoARM -> Word32 -> Ptr DataGraphPipelinePropertyQueryResultARM -> IO Result

-- | vkGetDataGraphPipelinePropertiesARM - Query properties of a data graph
-- pipeline
--
-- == Valid Usage
--
-- -   #VUID-vkGetDataGraphPipelinePropertiesARM-dataGraphPipeline-09802#
--     The @dataGraphPipeline@ member of @pPipelineInfo@ /must/ have been
--     created with @device@
--
-- -   #VUID-vkGetDataGraphPipelinePropertiesARM-pProperties-09889# There
--     /must/ not be two or more structures in the @pProperties@ array with
--     the same 'DataGraphPipelinePropertyQueryResultARM'::@property@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetDataGraphPipelinePropertiesARM-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetDataGraphPipelinePropertiesARM-pPipelineInfo-parameter#
--     @pPipelineInfo@ /must/ be a valid pointer to a valid
--     'DataGraphPipelineInfoARM' structure
--
-- -   #VUID-vkGetDataGraphPipelinePropertiesARM-pProperties-parameter#
--     @pProperties@ /must/ be a valid pointer to an array of
--     @propertiesCount@ 'DataGraphPipelinePropertyQueryResultARM'
--     structures
--
-- -   #VUID-vkGetDataGraphPipelinePropertiesARM-propertiesCount-arraylength#
--     @propertiesCount@ /must/ be greater than @0@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.INCOMPLETE'
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- 'DataGraphPipelineInfoARM', 'DataGraphPipelinePropertyQueryResultARM',
-- 'Vulkan.Core10.Handles.Device'
getDataGraphPipelinePropertiesARM :: forall io
                                   . (MonadIO io)
                                  => -- | @device@ is the logical device that created the data graph pipeline.
                                     Device
                                  -> -- | @pPipelineInfo@ is a 'DataGraphPipelineInfoARM' that describes the
                                     -- 'Vulkan.Core10.Handles.Pipeline' being queried.
                                     DataGraphPipelineInfoARM
                                  -> -- | @propertiesCount@ is the length of the @pProperties@ array.
                                     ("propertiesCount" ::: Word32)
                                  -> io (Result, ("properties" ::: Vector DataGraphPipelinePropertyQueryResultARM))
getDataGraphPipelinePropertiesARM device
                                    pipelineInfo
                                    propertiesCount = liftIO . evalContT $ do
  let vkGetDataGraphPipelinePropertiesARMPtr = pVkGetDataGraphPipelinePropertiesARM (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetDataGraphPipelinePropertiesARMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDataGraphPipelinePropertiesARM is null" Nothing Nothing
  let vkGetDataGraphPipelinePropertiesARM' = mkVkGetDataGraphPipelinePropertiesARM vkGetDataGraphPipelinePropertiesARMPtr
  pPipelineInfo <- ContT $ withCStruct (pipelineInfo)
  pPProperties <- ContT $ bracket (callocBytes @DataGraphPipelinePropertyQueryResultARM ((fromIntegral (propertiesCount)) * 40)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPProperties `advancePtrBytes` (i * 40) :: Ptr DataGraphPipelinePropertyQueryResultARM) . ($ ())) [0..(fromIntegral (propertiesCount)) - 1]
  r <- lift $ traceAroundEvent "vkGetDataGraphPipelinePropertiesARM" (vkGetDataGraphPipelinePropertiesARM'
                                                                        (deviceHandle (device))
                                                                        pPipelineInfo
                                                                        (propertiesCount)
                                                                        ((pPProperties)))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pProperties <- lift $ generateM (fromIntegral (propertiesCount)) (\i -> peekCStruct @DataGraphPipelinePropertyQueryResultARM (((pPProperties) `advancePtrBytes` (40 * (i)) :: Ptr DataGraphPipelinePropertyQueryResultARM)))
  pure $ (r, pProperties)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceQueueFamilyDataGraphPropertiesARM
  :: FunPtr (Ptr PhysicalDevice_T -> Word32 -> Ptr Word32 -> Ptr QueueFamilyDataGraphPropertiesARM -> IO Result) -> Ptr PhysicalDevice_T -> Word32 -> Ptr Word32 -> Ptr QueueFamilyDataGraphPropertiesARM -> IO Result

-- | vkGetPhysicalDeviceQueueFamilyDataGraphPropertiesARM - Query the data
-- processing engines and the operations they support for a given queue
-- family of a physical device
--
-- = Description
--
-- If @pQueueFamilyDataGraphProperties@ is @NULL@, then the number of
-- properties available is returned in
-- @pQueueFamilyDataGraphPropertyCount@. Otherwise,
-- @pQueueFamilyDataGraphPropertyCount@ /must/ point to a variable set by
-- the application to the number of elements in the
-- @pQueueFamilyDataGraphProperties@ array, and on return the variable is
-- overwritten with the number of structures actually written to
-- @pQueueFamilyDataGraphProperties@. If
-- @pQueueFamilyDataGraphPropertyCount@ is less than the number of
-- properties available, at most @pQueueFamilyDataGraphPropertyCount@
-- structures will be written, and 'Vulkan.Core10.Enums.Result.INCOMPLETE'
-- will be returned instead of 'Vulkan.Core10.Enums.Result.SUCCESS', to
-- indicate that not all the available properties were returned.
--
-- If the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-dataGraphModelQCOM dataGraphModel>
-- feature is supported, the implementation /must/ return at least one
-- property with engine type
-- 'PHYSICAL_DEVICE_DATA_GRAPH_PROCESSING_ENGINE_TYPE_NEURAL_QCOM' or
-- 'PHYSICAL_DEVICE_DATA_GRAPH_PROCESSING_ENGINE_TYPE_COMPUTE_QCOM'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPhysicalDeviceQueueFamilyDataGraphPropertiesARM-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkGetPhysicalDeviceQueueFamilyDataGraphPropertiesARM-pQueueFamilyDataGraphPropertyCount-parameter#
--     @pQueueFamilyDataGraphPropertyCount@ /must/ be a valid pointer to a
--     @uint32_t@ value
--
-- -   #VUID-vkGetPhysicalDeviceQueueFamilyDataGraphPropertiesARM-pQueueFamilyDataGraphProperties-parameter#
--     If the value referenced by @pQueueFamilyDataGraphPropertyCount@ is
--     not @0@, and @pQueueFamilyDataGraphProperties@ is not @NULL@,
--     @pQueueFamilyDataGraphProperties@ /must/ be a valid pointer to an
--     array of @pQueueFamilyDataGraphPropertyCount@
--     'QueueFamilyDataGraphPropertiesARM' structures
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.INCOMPLETE'
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- 'Vulkan.Core10.Handles.PhysicalDevice',
-- 'QueueFamilyDataGraphPropertiesARM'
getPhysicalDeviceQueueFamilyDataGraphPropertiesARM :: forall io
                                                    . (MonadIO io)
                                                   => -- | @physicalDevice@ is the physical device to query.
                                                      PhysicalDevice
                                                   -> -- | @queueFamilyIndex@ is the index of the queue family being queried.
                                                      ("queueFamilyIndex" ::: Word32)
                                                   -> io (Result, ("queueFamilyDataGraphProperties" ::: Vector QueueFamilyDataGraphPropertiesARM))
getPhysicalDeviceQueueFamilyDataGraphPropertiesARM physicalDevice
                                                     queueFamilyIndex = liftIO . evalContT $ do
  let vkGetPhysicalDeviceQueueFamilyDataGraphPropertiesARMPtr = pVkGetPhysicalDeviceQueueFamilyDataGraphPropertiesARM (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  lift $ unless (vkGetPhysicalDeviceQueueFamilyDataGraphPropertiesARMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceQueueFamilyDataGraphPropertiesARM is null" Nothing Nothing
  let vkGetPhysicalDeviceQueueFamilyDataGraphPropertiesARM' = mkVkGetPhysicalDeviceQueueFamilyDataGraphPropertiesARM vkGetPhysicalDeviceQueueFamilyDataGraphPropertiesARMPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPQueueFamilyDataGraphPropertyCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkGetPhysicalDeviceQueueFamilyDataGraphPropertiesARM" (vkGetPhysicalDeviceQueueFamilyDataGraphPropertiesARM'
                                                                                         physicalDevice'
                                                                                         (queueFamilyIndex)
                                                                                         (pPQueueFamilyDataGraphPropertyCount)
                                                                                         (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pQueueFamilyDataGraphPropertyCount <- lift $ peek @Word32 pPQueueFamilyDataGraphPropertyCount
  pPQueueFamilyDataGraphProperties <- ContT $ bracket (callocBytes @QueueFamilyDataGraphPropertiesARM ((fromIntegral (pQueueFamilyDataGraphPropertyCount)) * 160)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPQueueFamilyDataGraphProperties `advancePtrBytes` (i * 160) :: Ptr QueueFamilyDataGraphPropertiesARM) . ($ ())) [0..(fromIntegral (pQueueFamilyDataGraphPropertyCount)) - 1]
  r' <- lift $ traceAroundEvent "vkGetPhysicalDeviceQueueFamilyDataGraphPropertiesARM" (vkGetPhysicalDeviceQueueFamilyDataGraphPropertiesARM'
                                                                                          physicalDevice'
                                                                                          (queueFamilyIndex)
                                                                                          (pPQueueFamilyDataGraphPropertyCount)
                                                                                          ((pPQueueFamilyDataGraphProperties)))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pQueueFamilyDataGraphPropertyCount' <- lift $ peek @Word32 pPQueueFamilyDataGraphPropertyCount
  pQueueFamilyDataGraphProperties' <- lift $ generateM (fromIntegral (pQueueFamilyDataGraphPropertyCount')) (\i -> peekCStruct @QueueFamilyDataGraphPropertiesARM (((pPQueueFamilyDataGraphProperties) `advancePtrBytes` (160 * (i)) :: Ptr QueueFamilyDataGraphPropertiesARM)))
  pure $ ((r'), pQueueFamilyDataGraphProperties')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceQueueFamilyDataGraphProcessingEnginePropertiesARM
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr PhysicalDeviceQueueFamilyDataGraphProcessingEngineInfoARM -> Ptr QueueFamilyDataGraphProcessingEnginePropertiesARM -> IO ()) -> Ptr PhysicalDevice_T -> Ptr PhysicalDeviceQueueFamilyDataGraphProcessingEngineInfoARM -> Ptr QueueFamilyDataGraphProcessingEnginePropertiesARM -> IO ()

-- | vkGetPhysicalDeviceQueueFamilyDataGraphProcessingEnginePropertiesARM -
-- Query the properties of a data graph processing engine for a specific
-- queue family of a physical device
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- 'Vulkan.Core10.Handles.PhysicalDevice',
-- 'PhysicalDeviceQueueFamilyDataGraphProcessingEngineInfoARM',
-- 'QueueFamilyDataGraphProcessingEnginePropertiesARM'
getPhysicalDeviceQueueFamilyDataGraphProcessingEnginePropertiesARM :: forall io
                                                                    . ( MonadIO io )
                                                                   => -- | @physicalDevice@ is the physical device to query.
                                                                      --
                                                                      -- #VUID-vkGetPhysicalDeviceQueueFamilyDataGraphProcessingEnginePropertiesARM-physicalDevice-parameter#
                                                                      -- @physicalDevice@ /must/ be a valid
                                                                      -- 'Vulkan.Core10.Handles.PhysicalDevice' handle
                                                                      PhysicalDevice
                                                                   -> -- | @pQueueFamilyDataGraphProcessingEngineInfo@ is a pointer to a
                                                                      -- 'PhysicalDeviceQueueFamilyDataGraphProcessingEngineInfoARM' structure
                                                                      -- that specifies the data graph processing engine and queue family to
                                                                      -- query.
                                                                      --
                                                                      -- #VUID-vkGetPhysicalDeviceQueueFamilyDataGraphProcessingEnginePropertiesARM-pQueueFamilyDataGraphProcessingEngineInfo-parameter#
                                                                      -- @pQueueFamilyDataGraphProcessingEngineInfo@ /must/ be a valid pointer to
                                                                      -- a valid 'PhysicalDeviceQueueFamilyDataGraphProcessingEngineInfoARM'
                                                                      -- structure
                                                                      PhysicalDeviceQueueFamilyDataGraphProcessingEngineInfoARM
                                                                   -> io (QueueFamilyDataGraphProcessingEnginePropertiesARM)
getPhysicalDeviceQueueFamilyDataGraphProcessingEnginePropertiesARM physicalDevice
                                                                     queueFamilyDataGraphProcessingEngineInfo = liftIO . evalContT $ do
  let vkGetPhysicalDeviceQueueFamilyDataGraphProcessingEnginePropertiesARMPtr = pVkGetPhysicalDeviceQueueFamilyDataGraphProcessingEnginePropertiesARM (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  lift $ unless (vkGetPhysicalDeviceQueueFamilyDataGraphProcessingEnginePropertiesARMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceQueueFamilyDataGraphProcessingEnginePropertiesARM is null" Nothing Nothing
  let vkGetPhysicalDeviceQueueFamilyDataGraphProcessingEnginePropertiesARM' = mkVkGetPhysicalDeviceQueueFamilyDataGraphProcessingEnginePropertiesARM vkGetPhysicalDeviceQueueFamilyDataGraphProcessingEnginePropertiesARMPtr
  pQueueFamilyDataGraphProcessingEngineInfo <- ContT $ withCStruct (queueFamilyDataGraphProcessingEngineInfo)
  pPQueueFamilyDataGraphProcessingEngineProperties <- ContT (withZeroCStruct @QueueFamilyDataGraphProcessingEnginePropertiesARM)
  lift $ traceAroundEvent "vkGetPhysicalDeviceQueueFamilyDataGraphProcessingEnginePropertiesARM" (vkGetPhysicalDeviceQueueFamilyDataGraphProcessingEnginePropertiesARM'
                                                                                                    (physicalDeviceHandle (physicalDevice))
                                                                                                    pQueueFamilyDataGraphProcessingEngineInfo
                                                                                                    (pPQueueFamilyDataGraphProcessingEngineProperties))
  pQueueFamilyDataGraphProcessingEngineProperties <- lift $ peekCStruct @QueueFamilyDataGraphProcessingEnginePropertiesARM pPQueueFamilyDataGraphProcessingEngineProperties
  pure $ (pQueueFamilyDataGraphProcessingEngineProperties)


-- | VkPhysicalDeviceDataGraphFeaturesARM - Structure describing features to
-- control data graph pipelines
--
-- = Description
--
-- If the 'PhysicalDeviceDataGraphFeaturesARM' structure is included in the
-- @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceDataGraphFeaturesARM', it /must/ add an instance of the
-- structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDataGraphFeaturesARM = PhysicalDeviceDataGraphFeaturesARM
  { -- | #features-dataGraph# @dataGraph@ specifies whether data graph pipelines
    -- /can/ be used.
    dataGraph :: Bool
  , -- | #features-dataGraphUpdateAfterBind# @dataGraphUpdateAfterBind@ specifies
    -- whether data graph pipelines /can/ be created with a
    -- 'Vulkan.Core10.Handles.PipelineLayout' that uses one or more
    -- 'Vulkan.Core10.Handles.DescriptorSetLayout' objects created with the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set.
    dataGraphUpdateAfterBind :: Bool
  , -- | #features-dataGraphSpecializationConstants#
    -- @dataGraphSpecializationConstants@ specifies whether data graph
    -- pipelines /can/ be created from shader modules that use specialization
    -- constants.
    dataGraphSpecializationConstants :: Bool
  , -- | #features-dataGraphDescriptorBuffer# @dataGraphDescriptorBuffer@
    -- specifies whether data graph pipelines /can/ use descriptor buffers.
    dataGraphDescriptorBuffer :: Bool
  , -- | #features-dataGraphShaderModule# @dataGraphShaderModule@ specifies
    -- whether data graph pipelines /can/ be created from a shader module.
    dataGraphShaderModule :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDataGraphFeaturesARM)
#endif
deriving instance Show PhysicalDeviceDataGraphFeaturesARM

instance ToCStruct PhysicalDeviceDataGraphFeaturesARM where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDataGraphFeaturesARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DATA_GRAPH_FEATURES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (dataGraph))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (dataGraphUpdateAfterBind))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (dataGraphSpecializationConstants))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (dataGraphDescriptorBuffer))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (dataGraphShaderModule))
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DATA_GRAPH_FEATURES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceDataGraphFeaturesARM where
  peekCStruct p = do
    dataGraph <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    dataGraphUpdateAfterBind <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    dataGraphSpecializationConstants <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    dataGraphDescriptorBuffer <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    dataGraphShaderModule <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    pure $ PhysicalDeviceDataGraphFeaturesARM
             (bool32ToBool dataGraph)
             (bool32ToBool dataGraphUpdateAfterBind)
             (bool32ToBool dataGraphSpecializationConstants)
             (bool32ToBool dataGraphDescriptorBuffer)
             (bool32ToBool dataGraphShaderModule)

instance Storable PhysicalDeviceDataGraphFeaturesARM where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDataGraphFeaturesARM where
  zero = PhysicalDeviceDataGraphFeaturesARM
           zero
           zero
           zero
           zero
           zero


-- | VkDataGraphPipelineConstantTensorSemiStructuredSparsityInfoARM -
-- Structure specifying semi-structured sparsity parameters of a tensor
-- data graph pipeline constant
--
-- = Description
--
-- This extension does not provide applications with a way of knowing which
-- combinations of @dimension@, @zeroCount@, and @groupSize@ an
-- implementation /can/ take advantage of. Providing sparsity information
-- for a graph constant is always valid and recommended, regardless of the
-- specific combinations an implementation /can/ take advantage of. When
-- they /can/ not take advantage of the sparsity information,
-- implementations will ignore it and treat the data as dense.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DataGraphPipelineConstantTensorSemiStructuredSparsityInfoARM = DataGraphPipelineConstantTensorSemiStructuredSparsityInfoARM
  { -- | @dimension@ is the dimension of the tensor along which its data is
    -- sparse.
    dimension :: Word32
  , -- | @zeroCount@ is the number of tensor elements that /must/ be zero in
    -- every group of @groupSize@ elements.
    zeroCount :: Word32
  , -- | @groupSize@ is the number of tensor elements in a group.
    groupSize :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DataGraphPipelineConstantTensorSemiStructuredSparsityInfoARM)
#endif
deriving instance Show DataGraphPipelineConstantTensorSemiStructuredSparsityInfoARM

instance ToCStruct DataGraphPipelineConstantTensorSemiStructuredSparsityInfoARM where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DataGraphPipelineConstantTensorSemiStructuredSparsityInfoARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_CONSTANT_TENSOR_SEMI_STRUCTURED_SPARSITY_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (dimension)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zeroCount)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (groupSize)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_CONSTANT_TENSOR_SEMI_STRUCTURED_SPARSITY_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    f

instance FromCStruct DataGraphPipelineConstantTensorSemiStructuredSparsityInfoARM where
  peekCStruct p = do
    dimension <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    zeroCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    groupSize <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ DataGraphPipelineConstantTensorSemiStructuredSparsityInfoARM
             dimension zeroCount groupSize

instance Storable DataGraphPipelineConstantTensorSemiStructuredSparsityInfoARM where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DataGraphPipelineConstantTensorSemiStructuredSparsityInfoARM where
  zero = DataGraphPipelineConstantTensorSemiStructuredSparsityInfoARM
           zero
           zero
           zero


-- | VkDataGraphPipelineConstantARM - Structure specifying parameters of a
-- data graph pipeline constant
--
-- = Description
--
-- The size and layout of the data pointed to by @pConstantData@ is
-- specified by a specific structure in the @pNext@ chain for each type of
-- graph constant.
--
-- For graph constants of tensor type, the layout of the data is specified
-- by a 'Vulkan.Extensions.VK_ARM_tensors.TensorDescriptionARM' structure.
-- The data /must/ be laid out according to the following members of this
-- structure:
--
-- -   'Vulkan.Extensions.VK_ARM_tensors.TensorDescriptionARM'::@tiling@
--
-- -   'Vulkan.Extensions.VK_ARM_tensors.TensorDescriptionARM'::@format@
--
-- -   'Vulkan.Extensions.VK_ARM_tensors.TensorDescriptionARM'::@dimensionCount@
--
-- -   'Vulkan.Extensions.VK_ARM_tensors.TensorDescriptionARM'::@pDimensions@
--
-- -   'Vulkan.Extensions.VK_ARM_tensors.TensorDescriptionARM'::@pStrides@
--
-- The presence of a
-- 'DataGraphPipelineConstantTensorSemiStructuredSparsityInfoARM' structure
-- in the @pNext@ chain has no impact on the expected layout of the data
-- pointed to by @pConstantData@.
--
-- == Valid Usage
--
-- -   #VUID-VkDataGraphPipelineConstantARM-pNext-09775# If the @pNext@
--     chain of this structure includes one or more
--     'DataGraphPipelineConstantTensorSemiStructuredSparsityInfoARM'
--     structures then it /must/ also include a
--     'Vulkan.Extensions.VK_ARM_tensors.TensorDescriptionARM' structure
--
-- -   #VUID-VkDataGraphPipelineConstantARM-pNext-09776# If the @pNext@
--     chain of this structure includes one or more
--     'DataGraphPipelineConstantTensorSemiStructuredSparsityInfoARM'
--     structures then, for each structure,
--     'DataGraphPipelineConstantTensorSemiStructuredSparsityInfoARM'::@dimension@
--     /must/ be less than
--     'Vulkan.Extensions.VK_ARM_tensors.TensorDescriptionARM'::@dimensionCount@
--
-- -   #VUID-VkDataGraphPipelineConstantARM-pNext-09777# If the @pNext@
--     chain of this structure includes a
--     'DataGraphPipelineConstantTensorSemiStructuredSparsityInfoARM'
--     structure then, for each structure,
--     'Vulkan.Extensions.VK_ARM_tensors.TensorDescriptionARM'::@pDimensions@['DataGraphPipelineConstantTensorSemiStructuredSparsityInfoARM'::@dimension@]
--     /must/ be a multiple of
--     'DataGraphPipelineConstantTensorSemiStructuredSparsityInfoARM'::@groupSize@
--
-- -   #VUID-VkDataGraphPipelineConstantARM-pNext-09870# If the @pNext@
--     chain of this structure includes multiple
--     'DataGraphPipelineConstantTensorSemiStructuredSparsityInfoARM'
--     structures then no two structures /may/ have their
--     'DataGraphPipelineConstantTensorSemiStructuredSparsityInfoARM'::@dimension@
--     member set to the same value
--
-- -   #VUID-VkDataGraphPipelineConstantARM-id-09850# If the @pNext@ chain
--     of this structure includes a
--     'Vulkan.Extensions.VK_ARM_tensors.TensorDescriptionARM' structure,
--     then its @usage@ member /must/ contain
--     'Vulkan.Extensions.VK_ARM_tensors.TENSOR_USAGE_DATA_GRAPH_BIT_ARM'
--
-- -   #VUID-VkDataGraphPipelineConstantARM-pNext-09917# If the @pNext@
--     chain of this structure includes a
--     'Vulkan.Extensions.VK_ARM_tensors.TensorDescriptionARM' structure,
--     then its @tiling@ member /must/ be
--     'Vulkan.Extensions.VK_ARM_tensors.TENSOR_TILING_LINEAR_ARM'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDataGraphPipelineConstantARM-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_CONSTANT_ARM'
--
-- -   #VUID-VkDataGraphPipelineConstantARM-pNext-pNext# Each @pNext@
--     member of any structure (including this one) in the @pNext@ chain
--     /must/ be either @NULL@ or a pointer to a valid instance of
--     'DataGraphPipelineConstantTensorSemiStructuredSparsityInfoARM' or
--     'Vulkan.Extensions.VK_ARM_tensors.TensorDescriptionARM'
--
-- -   #VUID-VkDataGraphPipelineConstantARM-sType-unique# The @sType@ value
--     of each structure in the @pNext@ chain /must/ be unique, with the
--     exception of structures of type
--     'DataGraphPipelineConstantTensorSemiStructuredSparsityInfoARM'
--
-- -   #VUID-VkDataGraphPipelineConstantARM-pConstantData-parameter#
--     @pConstantData@ /must/ be a pointer value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- 'DataGraphPipelineShaderModuleCreateInfoARM',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DataGraphPipelineConstantARM (es :: [Type]) = DataGraphPipelineConstantARM
  { -- | @pNext@ is a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @id@ is the unique identifier of the graph constant this structure
    -- describes.
    id' :: Word32
  , -- | @pConstantData@ is a pointer to the data for this graph constant.
    constantData :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DataGraphPipelineConstantARM (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (DataGraphPipelineConstantARM es)

instance Extensible DataGraphPipelineConstantARM where
  extensibleTypeName = "DataGraphPipelineConstantARM"
  setNext DataGraphPipelineConstantARM{..} next' = DataGraphPipelineConstantARM{next = next', ..}
  getNext DataGraphPipelineConstantARM{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends DataGraphPipelineConstantARM e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @DataGraphPipelineConstantTensorSemiStructuredSparsityInfoARM = Just f
    | Just Refl <- eqT @e @TensorDescriptionARM = Just f
    | otherwise = Nothing

instance ( Extendss DataGraphPipelineConstantARM es
         , PokeChain es ) => ToCStruct (DataGraphPipelineConstantARM es) where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DataGraphPipelineConstantARM{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_CONSTANT_ARM)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (id')
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (constantData)
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_CONSTANT_ARM)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (zero)
    lift $ f

instance ( Extendss DataGraphPipelineConstantARM es
         , PeekChain es ) => FromCStruct (DataGraphPipelineConstantARM es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    id' <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pConstantData <- peek @(Ptr ()) ((p `plusPtr` 24 :: Ptr (Ptr ())))
    pure $ DataGraphPipelineConstantARM
             next id' pConstantData

instance es ~ '[] => Zero (DataGraphPipelineConstantARM es) where
  zero = DataGraphPipelineConstantARM
           ()
           zero
           zero


-- | VkDataGraphPipelineResourceInfoARM - Structure specifying parameters of
-- a data graph pipeline resource
--
-- == Valid Usage
--
-- -   #VUID-VkDataGraphPipelineResourceInfoARM-descriptorSet-09851# If the
--     @pNext@ chain of this structure includes a
--     'Vulkan.Extensions.VK_ARM_tensors.TensorDescriptionARM' structure,
--     then its @usage@ /must/ contain
--     'Vulkan.Extensions.VK_ARM_tensors.TENSOR_USAGE_DATA_GRAPH_BIT_ARM'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDataGraphPipelineResourceInfoARM-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_RESOURCE_INFO_ARM'
--
-- -   #VUID-VkDataGraphPipelineResourceInfoARM-pNext-pNext# @pNext@ /must/
--     be @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_ARM_tensors.TensorDescriptionARM'
--
-- -   #VUID-VkDataGraphPipelineResourceInfoARM-sType-unique# The @sType@
--     value of each structure in the @pNext@ chain /must/ be unique
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- 'DataGraphPipelineCreateInfoARM',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DataGraphPipelineResourceInfoARM (es :: [Type]) = DataGraphPipelineResourceInfoARM
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @descriptorSet@ is the descriptor set number of the resource being
    -- described.
    descriptorSet :: Word32
  , -- | @binding@ is the binding number of the resource being described.
    binding :: Word32
  , -- | @arrayElement@ is the element in the resource array if @descriptorSet@
    -- and @binding@ identifies an array of resources or @0@ otherwise.
    arrayElement :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DataGraphPipelineResourceInfoARM (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (DataGraphPipelineResourceInfoARM es)

instance Extensible DataGraphPipelineResourceInfoARM where
  extensibleTypeName = "DataGraphPipelineResourceInfoARM"
  setNext DataGraphPipelineResourceInfoARM{..} next' = DataGraphPipelineResourceInfoARM{next = next', ..}
  getNext DataGraphPipelineResourceInfoARM{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends DataGraphPipelineResourceInfoARM e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @TensorDescriptionARM = Just f
    | otherwise = Nothing

instance ( Extendss DataGraphPipelineResourceInfoARM es
         , PokeChain es ) => ToCStruct (DataGraphPipelineResourceInfoARM es) where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DataGraphPipelineResourceInfoARM{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_RESOURCE_INFO_ARM)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (descriptorSet)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (binding)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (arrayElement)
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_RESOURCE_INFO_ARM)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    lift $ f

instance ( Extendss DataGraphPipelineResourceInfoARM es
         , PeekChain es ) => FromCStruct (DataGraphPipelineResourceInfoARM es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    descriptorSet <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    binding <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    arrayElement <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ DataGraphPipelineResourceInfoARM
             next descriptorSet binding arrayElement

instance es ~ '[] => Zero (DataGraphPipelineResourceInfoARM es) where
  zero = DataGraphPipelineResourceInfoARM
           ()
           zero
           zero
           zero


-- | VkDataGraphPipelineCompilerControlCreateInfoARM - Structure specifying
-- compiler control parameters of a newly created data graph pipeline
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DataGraphPipelineCompilerControlCreateInfoARM = DataGraphPipelineCompilerControlCreateInfoARM
  { -- | @pVendorOptions@ is a null-terminated UTF-8 string specifying
    -- implementation-specific options that affect the creation of a data graph
    -- pipeline.
    --
    -- #VUID-VkDataGraphPipelineCompilerControlCreateInfoARM-pVendorOptions-parameter#
    -- @pVendorOptions@ /must/ be a null-terminated UTF-8 string
    vendorOptions :: ByteString }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DataGraphPipelineCompilerControlCreateInfoARM)
#endif
deriving instance Show DataGraphPipelineCompilerControlCreateInfoARM

instance ToCStruct DataGraphPipelineCompilerControlCreateInfoARM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DataGraphPipelineCompilerControlCreateInfoARM{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_COMPILER_CONTROL_CREATE_INFO_ARM)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pVendorOptions'' <- ContT $ useAsCString (vendorOptions)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr CChar))) pVendorOptions''
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_COMPILER_CONTROL_CREATE_INFO_ARM)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pVendorOptions'' <- ContT $ useAsCString (mempty)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr CChar))) pVendorOptions''
    lift $ f

instance FromCStruct DataGraphPipelineCompilerControlCreateInfoARM where
  peekCStruct p = do
    pVendorOptions <- packCString =<< peek ((p `plusPtr` 16 :: Ptr (Ptr CChar)))
    pure $ DataGraphPipelineCompilerControlCreateInfoARM
             pVendorOptions

instance Zero DataGraphPipelineCompilerControlCreateInfoARM where
  zero = DataGraphPipelineCompilerControlCreateInfoARM
           mempty


-- | VkDataGraphPipelineCreateInfoARM - Structure specifying parameters of a
-- newly created data graph pipeline
--
-- = Description
--
-- Applications /can/ create a data graph pipeline entirely from data
-- present in a pipeline cache. This is done by including a
-- 'DataGraphPipelineIdentifierCreateInfoARM' structure in the @pNext@
-- chain. If the required data is not found in the pipeline cache, creating
-- the data graph pipeline is not possible and the implementation /must/
-- fail as specified by
-- 'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT'.
--
-- Applications /can/ create a data graph pipeline without providing a
-- pipeline cache or shader module by invoking one of the models provided
-- by the implementation. This is done by including
-- 'Vulkan.Extensions.VK_QCOM_data_graph_model.DataGraphPipelineBuiltinModelCreateInfoQCOM'
-- in the @pNext@ chain.
--
-- When an identifier or built-in model is used to create a data graph
-- pipeline, implementations /may/ fail pipeline creation with
-- 'Vulkan.Core10.Enums.Result.PIPELINE_COMPILE_REQUIRED' for any reason.
--
-- The data graph engines for this pipeline /can/ be selected by including
-- a 'DataGraphProcessingEngineCreateInfoARM' to the @pNext@ chain of this
-- structure. Otherwise,
-- 'PHYSICAL_DEVICE_DATA_GRAPH_PROCESSING_ENGINE_TYPE_DEFAULT_ARM' will be
-- used as the sole data graph engine.
--
-- The data graph operations that this pipeline uses /must/ be supported
-- for the data graph engines selected for this pipeline as retrieved by
-- 'getPhysicalDeviceQueueFamilyDataGraphPropertiesARM'.
--
-- == Valid Usage
--
-- -   #VUID-VkDataGraphPipelineCreateInfoARM-pNext-09763# One and only one
--     of the following structures /must/ be included in the @pNext@ chain:
--
--     -   'DataGraphPipelineShaderModuleCreateInfoARM'
--
--     -   'DataGraphPipelineIdentifierCreateInfoARM'
--
--     -   'Vulkan.Extensions.VK_QCOM_data_graph_model.DataGraphPipelineBuiltinModelCreateInfoQCOM'
--
-- -   #VUID-VkDataGraphPipelineCreateInfoARM-flags-09764# @flags@ /may/
--     only contain
--     'Vulkan.Extensions.VK_KHR_maintenance5.PIPELINE_CREATE_2_NO_PROTECTED_ACCESS_BIT_EXT',
--     'Vulkan.Extensions.VK_KHR_maintenance5.PIPELINE_CREATE_2_PROTECTED_ACCESS_ONLY_BIT_EXT',
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DISABLE_OPTIMIZATION_BIT',
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_BUFFER_BIT_EXT',
--     'Vulkan.Extensions.VK_KHR_maintenance5.PIPELINE_CREATE_2_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_KHR'
--     or
--     'Vulkan.Extensions.VK_KHR_maintenance5.PIPELINE_CREATE_2_EARLY_RETURN_ON_FAILURE_BIT_KHR'
--
-- -   #VUID-VkDataGraphPipelineCreateInfoARM-layout-09767# @layout@ /must/
--     have been created with @pushConstantRangeCount@ equal to 0 and
--     @pPushConstantRanges@ equal to @NULL@
--
-- -   #VUID-VkDataGraphPipelineCreateInfoARM-dataGraphUpdateAfterBind-09768#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-dataGraphUpdateAfterBind dataGraphUpdateAfterBind>
--     feature is not enabled, @layout@ must not use any
--     'Vulkan.Core10.Handles.DescriptorSetLayout' object created with the
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
--     bit set
--
-- -   #VUID-VkDataGraphPipelineCreateInfoARM-dataGraphDescriptorBuffer-09885#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-dataGraphDescriptorBuffer dataGraphDescriptorBuffer>
--     feature is not enabled, @flags@ /must/ not contain
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-VkDataGraphPipelineCreateInfoARM-layout-09769# If a
--     'DataGraphPipelineShaderModuleCreateInfoARM' structure is included
--     in the @pNext@ chain and a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#interfaces-resources resource variable>
--     is declared in the shader module, the corresponding descriptor
--     binding used to create @layout@ /must/ have a @descriptorType@ that
--     corresponds to the type of the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#interfaces-resources resource variable>
--
-- -   #VUID-VkDataGraphPipelineCreateInfoARM-None-11840# If a
--     'DataGraphPipelineIdentifierCreateInfoARM' or a
--     'Vulkan.Extensions.VK_QCOM_data_graph_model.DataGraphPipelineBuiltinModelCreateInfoQCOM'
--     structure is included in the @pNext@ chain, then @flags@ /must/
--     contain
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT'
--
-- -   #VUID-VkDataGraphPipelineCreateInfoARM-None-12363# If a
--     'DataGraphPipelineIdentifierCreateInfoARM' or a
--     'Vulkan.Extensions.VK_QCOM_data_graph_model.DataGraphPipelineBuiltinModelCreateInfoQCOM'
--     structure is included in the @pNext@ chain, then @resourceInfoCount@
--     /must/ be 0
--
-- -   #VUID-VkDataGraphPipelineCreateInfoARM-resourceInfoCount-12364# If
--     @resourceInfoCount@ is equal to 0, then @pResourceInfos@ /must/
--     equal @NULL@
--
-- -   #VUID-VkDataGraphPipelineCreateInfoARM-None-12365# If neither a
--     'DataGraphPipelineIdentifierCreateInfoARM' nor a
--     'Vulkan.Extensions.VK_QCOM_data_graph_model.DataGraphPipelineBuiltinModelCreateInfoQCOM'
--     structure are included in the @pNext@ chain, then
--     @resourceInfoCount@ /must/ be greater than 0
--
-- -   #VUID-VkDataGraphPipelineCreateInfoARM-dataGraphShaderModule-09886#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-dataGraphShaderModule dataGraphShaderModule>
--     feature is not enabled, a
--     'DataGraphPipelineShaderModuleCreateInfoARM' structure /must/ not be
--     included in the @pNext@ chain
--
-- -   #VUID-VkDataGraphPipelineCreateInfoARM-layout-09934# If a
--     'DataGraphPipelineShaderModuleCreateInfoARM' structure is included
--     in the @pNext@ chain and an array
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#interfaces-resources resource variable>
--     is declared in the shader module, the corresponding descriptor
--     binding used to create @layout@ /must/ have a @descriptorCount@ that
--     is greater than or equal to the length of the array
--
-- -   #VUID-VkDataGraphPipelineCreateInfoARM-pipelineCreationCacheControl-09871#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-pipelineCreationCacheControl pipelineCreationCacheControl>
--     feature is not enabled, @flags@ /must/ not include
--     'Vulkan.Extensions.VK_KHR_maintenance5.PIPELINE_CREATE_2_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_KHR'
--     or
--     'Vulkan.Extensions.VK_KHR_maintenance5.PIPELINE_CREATE_2_EARLY_RETURN_ON_FAILURE_BIT_KHR'
--
-- -   #VUID-VkDataGraphPipelineCreateInfoARM-pSetLayouts-09770# The
--     descriptor set layouts in
--     'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo'::@pSetLayouts@
--     used to create @layout@ /must/ not include any
--     'Vulkan.Core10.DescriptorSet.DescriptorSetLayoutBinding' whose
--     descriptor type is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_EXT'
--
-- -   #VUID-VkDataGraphPipelineCreateInfoARM-pipelineProtectedAccess-09772#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-pipelineProtectedAccess pipelineProtectedAccess>
--     feature is not enabled, @flags@ /must/ not include
--     'Vulkan.Extensions.VK_KHR_maintenance5.PIPELINE_CREATE_2_NO_PROTECTED_ACCESS_BIT_EXT'
--     or
--     'Vulkan.Extensions.VK_KHR_maintenance5.PIPELINE_CREATE_2_PROTECTED_ACCESS_ONLY_BIT_EXT'
--
-- -   #VUID-VkDataGraphPipelineCreateInfoARM-flags-09773# @flags@ /must/
--     not include both
--     'Vulkan.Extensions.VK_KHR_maintenance5.PIPELINE_CREATE_2_NO_PROTECTED_ACCESS_BIT_EXT'
--     and
--     'Vulkan.Extensions.VK_KHR_maintenance5.PIPELINE_CREATE_2_PROTECTED_ACCESS_ONLY_BIT_EXT'
--
-- -   #VUID-VkDataGraphPipelineCreateInfoARM-pNext-09804# If the @pNext@
--     chain includes an
--     'Vulkan.Core13.Promoted_From_VK_EXT_pipeline_creation_feedback.PipelineCreationFeedbackCreateInfo'
--     structure, then its @pipelineStageCreationFeedbackCount@ /must/ be 0
--
-- -   #VUID-VkDataGraphPipelineCreateInfoARM-pNext-09948# If a
--     'DataGraphProcessingEngineCreateInfoARM' structure is included in
--     the @pNext@ chain, each member of @pProcessingEngines@ /must/ be
--     identical to an 'QueueFamilyDataGraphPropertiesARM'::@engine@
--     retrieved from 'getPhysicalDeviceQueueFamilyDataGraphPropertiesARM'
--     with the @physicalDevice@ that was used to create @device@
--
-- -   #VUID-VkDataGraphPipelineCreateInfoARM-pNext-09949# If a
--     'DataGraphProcessingEngineCreateInfoARM' structure is not included
--     in the @pNext@ chain,
--     'PHYSICAL_DEVICE_DATA_GRAPH_PROCESSING_ENGINE_TYPE_DEFAULT_ARM'
--     /must/ be set in an 'QueueFamilyDataGraphPropertiesARM'::@engine@
--     retrieved from 'getPhysicalDeviceQueueFamilyDataGraphPropertiesARM'
--     with the @physicalDevice@ that was used to create @device@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDataGraphPipelineCreateInfoARM-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_CREATE_INFO_ARM'
--
-- -   #VUID-VkDataGraphPipelineCreateInfoARM-pNext-pNext# Each @pNext@
--     member of any structure (including this one) in the @pNext@ chain
--     /must/ be either @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_QCOM_data_graph_model.DataGraphPipelineBuiltinModelCreateInfoQCOM',
--     'DataGraphPipelineCompilerControlCreateInfoARM',
--     'DataGraphPipelineIdentifierCreateInfoARM',
--     'DataGraphPipelineShaderModuleCreateInfoARM',
--     'DataGraphProcessingEngineCreateInfoARM',
--     'Vulkan.Core13.Promoted_From_VK_EXT_pipeline_creation_feedback.PipelineCreationFeedbackCreateInfo',
--     or 'Vulkan.Core10.Shader.ShaderModuleCreateInfo'
--
-- -   #VUID-VkDataGraphPipelineCreateInfoARM-sType-unique# The @sType@
--     value of each structure in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkDataGraphPipelineCreateInfoARM-flags-parameter# @flags@
--     /must/ be a valid combination of
--     'Vulkan.Extensions.VK_KHR_maintenance5.PipelineCreateFlagBits2KHR'
--     values
--
-- -   #VUID-VkDataGraphPipelineCreateInfoARM-layout-parameter# @layout@
--     /must/ be a valid 'Vulkan.Core10.Handles.PipelineLayout' handle
--
-- -   #VUID-VkDataGraphPipelineCreateInfoARM-pResourceInfos-parameter# If
--     @resourceInfoCount@ is not @0@, @pResourceInfos@ /must/ be a valid
--     pointer to an array of @resourceInfoCount@ valid
--     'DataGraphPipelineResourceInfoARM' structures
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- 'DataGraphPipelineResourceInfoARM',
-- 'Vulkan.Core14.Enums.PipelineCreateFlags2.PipelineCreateFlags2',
-- 'Vulkan.Core10.Handles.PipelineLayout',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createDataGraphPipelinesARM'
data DataGraphPipelineCreateInfoARM (es :: [Type]) = DataGraphPipelineCreateInfoARM
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is a bitmask of
    -- 'Vulkan.Extensions.VK_KHR_maintenance5.PipelineCreateFlagBits2KHR'
    -- specifying how the pipeline will be generated.
    flags :: PipelineCreateFlags2KHR
  , -- | @layout@ is the description of binding locations used by both the
    -- pipeline and descriptor sets used with the pipeline.
    layout :: PipelineLayout
  , -- | @pResourceInfos@ is a pointer to an array of
    -- 'DataGraphPipelineResourceInfoARM' structures.
    resourceInfos :: Vector (SomeStruct DataGraphPipelineResourceInfoARM)
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DataGraphPipelineCreateInfoARM (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (DataGraphPipelineCreateInfoARM es)

instance Extensible DataGraphPipelineCreateInfoARM where
  extensibleTypeName = "DataGraphPipelineCreateInfoARM"
  setNext DataGraphPipelineCreateInfoARM{..} next' = DataGraphPipelineCreateInfoARM{next = next', ..}
  getNext DataGraphPipelineCreateInfoARM{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends DataGraphPipelineCreateInfoARM e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @DataGraphPipelineBuiltinModelCreateInfoQCOM = Just f
    | Just Refl <- eqT @e @DataGraphProcessingEngineCreateInfoARM = Just f
    | Just Refl <- eqT @e @DataGraphPipelineIdentifierCreateInfoARM = Just f
    | Just Refl <- eqT @e @DataGraphPipelineShaderModuleCreateInfoARM = Just f
    | Just Refl <- eqT @e @DataGraphPipelineCompilerControlCreateInfoARM = Just f
    | Just Refl <- eqT @e @PipelineCreationFeedbackCreateInfo = Just f
    | Just Refl <- eqT @e @(ShaderModuleCreateInfo '[]) = Just f
    | otherwise = Nothing

instance ( Extendss DataGraphPipelineCreateInfoARM es
         , PokeChain es ) => ToCStruct (DataGraphPipelineCreateInfoARM es) where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DataGraphPipelineCreateInfoARM{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_CREATE_INFO_ARM)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineCreateFlags2KHR)) (flags)
    lift $ poke ((p `plusPtr` 24 :: Ptr PipelineLayout)) (layout)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (resourceInfos)) :: Word32))
    pPResourceInfos' <- ContT $ allocaBytes @(DataGraphPipelineResourceInfoARM _) ((Data.Vector.length (resourceInfos)) * 32)
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPResourceInfos' `plusPtr` (32 * (i)) :: Ptr (DataGraphPipelineResourceInfoARM _))) (e) . ($ ())) (resourceInfos)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr (DataGraphPipelineResourceInfoARM _)))) (pPResourceInfos')
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_CREATE_INFO_ARM)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 24 :: Ptr PipelineLayout)) (zero)
    lift $ f

instance ( Extendss DataGraphPipelineCreateInfoARM es
         , PeekChain es ) => FromCStruct (DataGraphPipelineCreateInfoARM es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @PipelineCreateFlags2KHR ((p `plusPtr` 16 :: Ptr PipelineCreateFlags2KHR))
    layout <- peek @PipelineLayout ((p `plusPtr` 24 :: Ptr PipelineLayout))
    resourceInfoCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pResourceInfos <- peek @(Ptr (DataGraphPipelineResourceInfoARM _)) ((p `plusPtr` 40 :: Ptr (Ptr (DataGraphPipelineResourceInfoARM _))))
    pResourceInfos' <- generateM (fromIntegral resourceInfoCount) (\i -> peekSomeCStruct (forgetExtensions ((pResourceInfos `advancePtrBytes` (32 * (i)) :: Ptr (DataGraphPipelineResourceInfoARM _)))))
    pure $ DataGraphPipelineCreateInfoARM
             next flags layout pResourceInfos'

instance es ~ '[] => Zero (DataGraphPipelineCreateInfoARM es) where
  zero = DataGraphPipelineCreateInfoARM
           ()
           zero
           zero
           mempty


-- | VkDataGraphPipelineShaderModuleCreateInfoARM - Structure specifying
-- shader module parameters of a newly created data graph pipeline
--
-- = Description
--
-- If @module@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the
-- pipeline’s graph is defined by @module@. If @module@ is
-- 'Vulkan.Core10.APIConstants.NULL_HANDLE', the pipeline’s graph is
-- defined by the chained 'Vulkan.Core10.Shader.ShaderModuleCreateInfo'.
--
-- == Valid Usage
--
-- -   #VUID-VkDataGraphPipelineShaderModuleCreateInfoARM-dataGraphSpecializationConstants-09849#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-dataGraphSpecializationConstants dataGraphSpecializationConstants>
--     feature is not enabled then @pSpecializationInfo@ /must/ be @NULL@
--     and @module@ /must/ not contain any @OpSpec*@ instructions
--
-- -   #VUID-VkDataGraphPipelineShaderModuleCreateInfoARM-pName-09872#
--     @pName@ /must/ be the name of an @OpGraphEntryPointARM@ in @module@
--
-- -   #VUID-VkDataGraphPipelineShaderModuleCreateInfoARM-pNext-09873# If
--     the @pNext@ chain includes a
--     'Vulkan.Core10.Shader.ShaderModuleCreateInfo' structure, then
--     @module@ /must/ be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkDataGraphPipelineShaderModuleCreateInfoARM-pNext-09874# If
--     the @pNext@ chain does not include a
--     'Vulkan.Core10.Shader.ShaderModuleCreateInfo' structure, then
--     @module@ /must/ be a valid 'Vulkan.Core10.Handles.ShaderModule'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDataGraphPipelineShaderModuleCreateInfoARM-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SHADER_MODULE_CREATE_INFO_ARM'
--
-- -   #VUID-VkDataGraphPipelineShaderModuleCreateInfoARM-module-parameter#
--     If @module@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @module@ /must/ be a valid 'Vulkan.Core10.Handles.ShaderModule'
--     handle
--
-- -   #VUID-VkDataGraphPipelineShaderModuleCreateInfoARM-pName-parameter#
--     @pName@ /must/ be a null-terminated UTF-8 string
--
-- -   #VUID-VkDataGraphPipelineShaderModuleCreateInfoARM-pSpecializationInfo-parameter#
--     If @pSpecializationInfo@ is not @NULL@, @pSpecializationInfo@ /must/
--     be a valid pointer to a valid
--     'Vulkan.Core10.ComputePipeline.SpecializationInfo' structure
--
-- -   #VUID-VkDataGraphPipelineShaderModuleCreateInfoARM-pConstants-parameter#
--     If @constantCount@ is not @0@, and @pConstants@ is not @NULL@,
--     @pConstants@ /must/ be a valid pointer to an array of
--     @constantCount@ valid 'DataGraphPipelineConstantARM' structures
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- 'DataGraphPipelineConstantARM', 'Vulkan.Core10.Handles.ShaderModule',
-- 'Vulkan.Core10.ComputePipeline.SpecializationInfo',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DataGraphPipelineShaderModuleCreateInfoARM = DataGraphPipelineShaderModuleCreateInfoARM
  { -- | @module@ is optionally a 'Vulkan.Core10.Handles.ShaderModule' object
    -- containing the description of the graph.
    module' :: ShaderModule
  , -- | @pName@ is a pointer to a null-terminated UTF-8 string specifying the
    -- graph entry point name for this pipeline.
    name :: ByteString
  , -- | @pSpecializationInfo@ is a pointer to a
    -- 'Vulkan.Core10.ComputePipeline.SpecializationInfo' structure as
    -- described in
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-specialization-constants Specialization Constants>,
    -- or @NULL@.
    specializationInfo :: Maybe SpecializationInfo
  , -- | @constantCount@ is the length of the @pConstants@ array.
    constantCount :: Word32
  , -- | @pConstants@ is a pointer to an array of 'DataGraphPipelineConstantARM'
    -- structures.
    constants :: Vector (SomeStruct DataGraphPipelineConstantARM)
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DataGraphPipelineShaderModuleCreateInfoARM)
#endif
deriving instance Show DataGraphPipelineShaderModuleCreateInfoARM

instance ToCStruct DataGraphPipelineShaderModuleCreateInfoARM where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DataGraphPipelineShaderModuleCreateInfoARM{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SHADER_MODULE_CREATE_INFO_ARM)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr ShaderModule)) (module')
    pName'' <- ContT $ useAsCString (name)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr CChar))) pName''
    pSpecializationInfo'' <- case (specializationInfo) of
      Nothing -> pure nullPtr
      Just j -> ContT $ withCStruct (j)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr SpecializationInfo))) pSpecializationInfo''
    let pConstantsLength = Data.Vector.length $ (constants)
    constantCount'' <- lift $ if (constantCount) == 0
      then pure $ fromIntegral pConstantsLength
      else do
        unless (fromIntegral pConstantsLength == (constantCount) || pConstantsLength == 0) $
          throwIO $ IOError Nothing InvalidArgument "" "pConstants must be empty or have 'constantCount' elements" Nothing Nothing
        pure (constantCount)
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) (constantCount'')
    pConstants'' <- if Data.Vector.null (constants)
      then pure nullPtr
      else do
        pPConstants <- ContT $ allocaBytes @(DataGraphPipelineConstantARM _) (((Data.Vector.length (constants))) * 32)
        Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPConstants `plusPtr` (32 * (i)) :: Ptr (DataGraphPipelineConstantARM _))) (e) . ($ ())) ((constants))
        pure $ pPConstants
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr (DataGraphPipelineConstantARM _)))) pConstants''
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SHADER_MODULE_CREATE_INFO_ARM)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pName'' <- ContT $ useAsCString (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr CChar))) pName''
    lift $ f

instance FromCStruct DataGraphPipelineShaderModuleCreateInfoARM where
  peekCStruct p = do
    module' <- peek @ShaderModule ((p `plusPtr` 16 :: Ptr ShaderModule))
    pName <- packCString =<< peek ((p `plusPtr` 24 :: Ptr (Ptr CChar)))
    pSpecializationInfo <- peek @(Ptr SpecializationInfo) ((p `plusPtr` 32 :: Ptr (Ptr SpecializationInfo)))
    pSpecializationInfo' <- maybePeek (\j -> peekCStruct @SpecializationInfo (j)) pSpecializationInfo
    constantCount <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    pConstants <- peek @(Ptr (DataGraphPipelineConstantARM _)) ((p `plusPtr` 48 :: Ptr (Ptr (DataGraphPipelineConstantARM _))))
    let pConstantsLength = if pConstants == nullPtr then 0 else (fromIntegral constantCount)
    pConstants' <- generateM pConstantsLength (\i -> peekSomeCStruct (forgetExtensions ((pConstants `advancePtrBytes` (32 * (i)) :: Ptr (DataGraphPipelineConstantARM _)))))
    pure $ DataGraphPipelineShaderModuleCreateInfoARM
             module' pName pSpecializationInfo' constantCount pConstants'

instance Zero DataGraphPipelineShaderModuleCreateInfoARM where
  zero = DataGraphPipelineShaderModuleCreateInfoARM
           zero
           mempty
           Nothing
           zero
           mempty


-- | VkDataGraphPipelineSessionCreateInfoARM - Structure specifying
-- parameters of a newly created data graph pipeline session
--
-- == Valid Usage
--
-- -   #VUID-VkDataGraphPipelineSessionCreateInfoARM-dataGraphPipeline-09781#
--     @dataGraphPipeline@ /must/ have been obtained via a call to
--     'createDataGraphPipelinesARM'
--
-- -   #VUID-VkDataGraphPipelineSessionCreateInfoARM-protectedMemory-09782#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-protectedMemory protectedMemory>
--     feature is not enabled, @flags@ /must/ not contain
--     'DATA_GRAPH_PIPELINE_SESSION_CREATE_PROTECTED_BIT_ARM'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDataGraphPipelineSessionCreateInfoARM-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SESSION_CREATE_INFO_ARM'
--
-- -   #VUID-VkDataGraphPipelineSessionCreateInfoARM-pNext-pNext# @pNext@
--     /must/ be @NULL@
--
-- -   #VUID-VkDataGraphPipelineSessionCreateInfoARM-flags-parameter#
--     @flags@ /must/ be a valid combination of
--     'DataGraphPipelineSessionCreateFlagBitsARM' values
--
-- -   #VUID-VkDataGraphPipelineSessionCreateInfoARM-dataGraphPipeline-parameter#
--     @dataGraphPipeline@ /must/ be a valid
--     'Vulkan.Core10.Handles.Pipeline' handle
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- 'DataGraphPipelineSessionCreateFlagsARM',
-- 'Vulkan.Core10.Handles.Pipeline',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createDataGraphPipelineSessionARM'
data DataGraphPipelineSessionCreateInfoARM = DataGraphPipelineSessionCreateInfoARM
  { -- | @flags@ is a bitmask of 'DataGraphPipelineSessionCreateFlagBitsARM'
    -- describing additional parameters of the session.
    flags :: DataGraphPipelineSessionCreateFlagsARM
  , -- | @dataGraphPipeline@ is the 'Vulkan.Core10.Handles.Pipeline' handle of
    -- the data graph pipeline for which a session is being created.
    dataGraphPipeline :: Pipeline
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DataGraphPipelineSessionCreateInfoARM)
#endif
deriving instance Show DataGraphPipelineSessionCreateInfoARM

instance ToCStruct DataGraphPipelineSessionCreateInfoARM where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DataGraphPipelineSessionCreateInfoARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SESSION_CREATE_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DataGraphPipelineSessionCreateFlagsARM)) (flags)
    poke ((p `plusPtr` 24 :: Ptr Pipeline)) (dataGraphPipeline)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SESSION_CREATE_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr Pipeline)) (zero)
    f

instance FromCStruct DataGraphPipelineSessionCreateInfoARM where
  peekCStruct p = do
    flags <- peek @DataGraphPipelineSessionCreateFlagsARM ((p `plusPtr` 16 :: Ptr DataGraphPipelineSessionCreateFlagsARM))
    dataGraphPipeline <- peek @Pipeline ((p `plusPtr` 24 :: Ptr Pipeline))
    pure $ DataGraphPipelineSessionCreateInfoARM
             flags dataGraphPipeline

instance Storable DataGraphPipelineSessionCreateInfoARM where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DataGraphPipelineSessionCreateInfoARM where
  zero = DataGraphPipelineSessionCreateInfoARM
           zero
           zero


-- | VkDataGraphPipelineSessionBindPointRequirementsInfoARM - Structure
-- specifying info to query the bind point requirements of a data graph
-- pipeline session
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- 'Vulkan.Extensions.Handles.DataGraphPipelineSessionARM',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getDataGraphPipelineSessionBindPointRequirementsARM'
data DataGraphPipelineSessionBindPointRequirementsInfoARM = DataGraphPipelineSessionBindPointRequirementsInfoARM
  { -- | @session@ is a 'Vulkan.Extensions.Handles.DataGraphPipelineSessionARM'
    -- specifying the data graph pipeline session whose bind point requirements
    -- are being queried.
    --
    -- #VUID-VkDataGraphPipelineSessionBindPointRequirementsInfoARM-session-parameter#
    -- @session@ /must/ be a valid
    -- 'Vulkan.Extensions.Handles.DataGraphPipelineSessionARM' handle
    session :: DataGraphPipelineSessionARM }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DataGraphPipelineSessionBindPointRequirementsInfoARM)
#endif
deriving instance Show DataGraphPipelineSessionBindPointRequirementsInfoARM

instance ToCStruct DataGraphPipelineSessionBindPointRequirementsInfoARM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DataGraphPipelineSessionBindPointRequirementsInfoARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SESSION_BIND_POINT_REQUIREMENTS_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DataGraphPipelineSessionARM)) (session)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SESSION_BIND_POINT_REQUIREMENTS_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DataGraphPipelineSessionARM)) (zero)
    f

instance FromCStruct DataGraphPipelineSessionBindPointRequirementsInfoARM where
  peekCStruct p = do
    session <- peek @DataGraphPipelineSessionARM ((p `plusPtr` 16 :: Ptr DataGraphPipelineSessionARM))
    pure $ DataGraphPipelineSessionBindPointRequirementsInfoARM
             session

instance Storable DataGraphPipelineSessionBindPointRequirementsInfoARM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DataGraphPipelineSessionBindPointRequirementsInfoARM where
  zero = DataGraphPipelineSessionBindPointRequirementsInfoARM
           zero


-- | VkDataGraphPipelineSessionBindPointRequirementARM - Structure specifying
-- the requirements of a bind point of a data graph pipeline session
--
-- = Description
--
-- Implementations /must/ always return 1 for @numObjects@ if @bindPoint@
-- is one of the following bind points:
--
-- -   'DATA_GRAPH_PIPELINE_SESSION_BIND_POINT_TRANSIENT_ARM'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDataGraphPipelineSessionBindPointRequirementARM-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SESSION_BIND_POINT_REQUIREMENT_ARM'
--
-- -   #VUID-VkDataGraphPipelineSessionBindPointRequirementARM-pNext-pNext#
--     @pNext@ /must/ be @NULL@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- 'DataGraphPipelineSessionBindPointARM',
-- 'DataGraphPipelineSessionBindPointTypeARM',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getDataGraphPipelineSessionBindPointRequirementsARM'
data DataGraphPipelineSessionBindPointRequirementARM = DataGraphPipelineSessionBindPointRequirementARM
  { -- | @bindPoint@ is a 'DataGraphPipelineSessionBindPointARM' specifying the
    -- data graph pipeline session bind point being required.
    bindPoint :: DataGraphPipelineSessionBindPointARM
  , -- | @bindPointType@ is a 'DataGraphPipelineSessionBindPointTypeARM'
    -- specifying the type of object required for @bindPoint@.
    bindPointType :: DataGraphPipelineSessionBindPointTypeARM
  , -- | @numObjects@ is the number of objects required for @bindPoint@.
    numObjects :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DataGraphPipelineSessionBindPointRequirementARM)
#endif
deriving instance Show DataGraphPipelineSessionBindPointRequirementARM

instance ToCStruct DataGraphPipelineSessionBindPointRequirementARM where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DataGraphPipelineSessionBindPointRequirementARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SESSION_BIND_POINT_REQUIREMENT_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DataGraphPipelineSessionBindPointARM)) (bindPoint)
    poke ((p `plusPtr` 20 :: Ptr DataGraphPipelineSessionBindPointTypeARM)) (bindPointType)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (numObjects)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SESSION_BIND_POINT_REQUIREMENT_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DataGraphPipelineSessionBindPointARM)) (zero)
    poke ((p `plusPtr` 20 :: Ptr DataGraphPipelineSessionBindPointTypeARM)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    f

instance FromCStruct DataGraphPipelineSessionBindPointRequirementARM where
  peekCStruct p = do
    bindPoint <- peek @DataGraphPipelineSessionBindPointARM ((p `plusPtr` 16 :: Ptr DataGraphPipelineSessionBindPointARM))
    bindPointType <- peek @DataGraphPipelineSessionBindPointTypeARM ((p `plusPtr` 20 :: Ptr DataGraphPipelineSessionBindPointTypeARM))
    numObjects <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ DataGraphPipelineSessionBindPointRequirementARM
             bindPoint bindPointType numObjects

instance Storable DataGraphPipelineSessionBindPointRequirementARM where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DataGraphPipelineSessionBindPointRequirementARM where
  zero = DataGraphPipelineSessionBindPointRequirementARM
           zero
           zero
           zero


-- | VkDataGraphPipelineSessionMemoryRequirementsInfoARM - Structure
-- specifying parameters to query the memory requirements of a data graph
-- pipeline session
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- 'Vulkan.Extensions.Handles.DataGraphPipelineSessionARM',
-- 'DataGraphPipelineSessionBindPointARM',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getDataGraphPipelineSessionMemoryRequirementsARM'
data DataGraphPipelineSessionMemoryRequirementsInfoARM = DataGraphPipelineSessionMemoryRequirementsInfoARM
  { -- | @session@ is the data graph pipeline session to query.
    --
    -- #VUID-VkDataGraphPipelineSessionMemoryRequirementsInfoARM-session-parameter#
    -- @session@ /must/ be a valid
    -- 'Vulkan.Extensions.Handles.DataGraphPipelineSessionARM' handle
    session :: DataGraphPipelineSessionARM
  , -- | @bindPoint@ is the bind point of a data graph pipeline session for which
    -- memory requirements are being queried.
    --
    -- #VUID-VkDataGraphPipelineSessionMemoryRequirementsInfoARM-bindPoint-parameter#
    -- @bindPoint@ /must/ be a valid 'DataGraphPipelineSessionBindPointARM'
    -- value
    bindPoint :: DataGraphPipelineSessionBindPointARM
  , -- | @objectIndex@ is the index of the object whose memory requirements are
    -- being queried.
    --
    -- #VUID-VkDataGraphPipelineSessionMemoryRequirementsInfoARM-objectIndex-09855#
    -- @objectIndex@ /must/ be less than the number of objects returned by
    -- 'getDataGraphPipelineSessionBindPointRequirementsARM' via
    -- 'DataGraphPipelineSessionBindPointRequirementARM'::@numObjects@ with
    -- 'DataGraphPipelineSessionMemoryRequirementsInfoARM'::@bindPoint@ equal
    -- to @bindPoint@
    objectIndex :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DataGraphPipelineSessionMemoryRequirementsInfoARM)
#endif
deriving instance Show DataGraphPipelineSessionMemoryRequirementsInfoARM

instance ToCStruct DataGraphPipelineSessionMemoryRequirementsInfoARM where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DataGraphPipelineSessionMemoryRequirementsInfoARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SESSION_MEMORY_REQUIREMENTS_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DataGraphPipelineSessionARM)) (session)
    poke ((p `plusPtr` 24 :: Ptr DataGraphPipelineSessionBindPointARM)) (bindPoint)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (objectIndex)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SESSION_MEMORY_REQUIREMENTS_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DataGraphPipelineSessionARM)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DataGraphPipelineSessionBindPointARM)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    f

instance FromCStruct DataGraphPipelineSessionMemoryRequirementsInfoARM where
  peekCStruct p = do
    session <- peek @DataGraphPipelineSessionARM ((p `plusPtr` 16 :: Ptr DataGraphPipelineSessionARM))
    bindPoint <- peek @DataGraphPipelineSessionBindPointARM ((p `plusPtr` 24 :: Ptr DataGraphPipelineSessionBindPointARM))
    objectIndex <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    pure $ DataGraphPipelineSessionMemoryRequirementsInfoARM
             session bindPoint objectIndex

instance Storable DataGraphPipelineSessionMemoryRequirementsInfoARM where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DataGraphPipelineSessionMemoryRequirementsInfoARM where
  zero = DataGraphPipelineSessionMemoryRequirementsInfoARM
           zero
           zero
           zero


-- | VkBindDataGraphPipelineSessionMemoryInfoARM - Structure describing how
-- to bind a data graph pipeline session to memory
--
-- == Valid Usage
--
-- -   #VUID-VkBindDataGraphPipelineSessionMemoryInfoARM-session-09785#
--     @session@ /must/ not have been bound to a memory object for
--     @bindPoint@
--
-- -   #VUID-VkBindDataGraphPipelineSessionMemoryInfoARM-bindPoint-09786#
--     @bindPoint@ /must/ have been returned as part of a
--     'DataGraphPipelineSessionBindPointRequirementARM' whose
--     @bindPointType@ member is
--     'DATA_GRAPH_PIPELINE_SESSION_BIND_POINT_TYPE_MEMORY_ARM' by a prior
--     call to 'getDataGraphPipelineSessionMemoryRequirementsARM' for
--     @session@
--
-- -   #VUID-VkBindDataGraphPipelineSessionMemoryInfoARM-memoryOffset-09787#
--     @memoryOffset@ /must/ be less than the size of @memory@
--
-- -   #VUID-VkBindDataGraphPipelineSessionMemoryInfoARM-memory-09788#
--     @memory@ must have been allocated using one of the memory types
--     allowed in the @memoryTypeBits@ member of the
--     'Vulkan.Core10.MemoryManagement.MemoryRequirements' structure
--     returned from a call to
--     'getDataGraphPipelineSessionMemoryRequirementsARM' with @session@
--
-- -   #VUID-VkBindDataGraphPipelineSessionMemoryInfoARM-memoryOffset-09789#
--     @memoryOffset@ /must/ be an integer multiple of the @alignment@
--     member of the 'Vulkan.Core10.MemoryManagement.MemoryRequirements'
--     structure returned from a call to
--     'getDataGraphPipelineSessionMemoryRequirementsARM' with @session@
--
-- -   #VUID-VkBindDataGraphPipelineSessionMemoryInfoARM-size-09790# The
--     @size@ member of the
--     'Vulkan.Core10.MemoryManagement.MemoryRequirements' structure
--     returned from a call to
--     'getDataGraphPipelineSessionMemoryRequirementsARM' with @session@
--     /must/ be less than or equal to the size of @memory@ minus
--     @memoryOffset@
--
-- -   #VUID-VkBindDataGraphPipelineSessionMemoryInfoARM-session-09791# If
--     @session@ was created with the
--     'DATA_GRAPH_PIPELINE_SESSION_CREATE_PROTECTED_BIT_ARM' bit set, the
--     session /must/ be bound to a memory object allocated with a memory
--     type that reports
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_PROTECTED_BIT'
--
-- -   #VUID-VkBindDataGraphPipelineSessionMemoryInfoARM-session-09792# If
--     @session@ was created with the
--     'DATA_GRAPH_PIPELINE_SESSION_CREATE_PROTECTED_BIT_ARM' bit not set,
--     the session /must/ not be bound to a memory object allocated with a
--     memory type that reports
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_PROTECTED_BIT'
--
-- -   #VUID-VkBindDataGraphPipelineSessionMemoryInfoARM-objectIndex-09805#
--     @objectIndex@ /must/ be less than the value of @numObjects@ returned
--     by 'getDataGraphPipelineSessionBindPointRequirementsARM' for
--     @bindPoint@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkBindDataGraphPipelineSessionMemoryInfoARM-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BIND_DATA_GRAPH_PIPELINE_SESSION_MEMORY_INFO_ARM'
--
-- -   #VUID-VkBindDataGraphPipelineSessionMemoryInfoARM-pNext-pNext#
--     @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkBindDataGraphPipelineSessionMemoryInfoARM-session-parameter#
--     @session@ /must/ be a valid
--     'Vulkan.Extensions.Handles.DataGraphPipelineSessionARM' handle
--
-- -   #VUID-VkBindDataGraphPipelineSessionMemoryInfoARM-bindPoint-parameter#
--     @bindPoint@ /must/ be a valid 'DataGraphPipelineSessionBindPointARM'
--     value
--
-- -   #VUID-VkBindDataGraphPipelineSessionMemoryInfoARM-memory-parameter#
--     @memory@ /must/ be a valid 'Vulkan.Core10.Handles.DeviceMemory'
--     handle
--
-- -   #VUID-VkBindDataGraphPipelineSessionMemoryInfoARM-commonparent# Both
--     of @memory@, and @session@ /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- 'Vulkan.Extensions.Handles.DataGraphPipelineSessionARM',
-- 'DataGraphPipelineSessionBindPointARM',
-- 'Vulkan.Core10.Handles.DeviceMemory',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'bindDataGraphPipelineSessionMemoryARM'
data BindDataGraphPipelineSessionMemoryInfoARM = BindDataGraphPipelineSessionMemoryInfoARM
  { -- | @session@ is the data graph pipeline session to be attached to memory.
    session :: DataGraphPipelineSessionARM
  , -- | @bindPoint@ is the data graph pipeline session bind point to which
    -- @memory@ is to be attached.
    bindPoint :: DataGraphPipelineSessionBindPointARM
  , -- | @objectIndex@ is the index of the object for @bindPoint@ at which
    -- @memory@ is to be attached.
    objectIndex :: Word32
  , -- | @memory@ is a 'Vulkan.Core10.Handles.DeviceMemory' object describing the
    -- device memory to attach.
    memory :: DeviceMemory
  , -- | @memoryOffset@ is the start offset of the resion of @memory@ which is to
    -- be bound to the data graph pipeline session.
    memoryOffset :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BindDataGraphPipelineSessionMemoryInfoARM)
#endif
deriving instance Show BindDataGraphPipelineSessionMemoryInfoARM

instance ToCStruct BindDataGraphPipelineSessionMemoryInfoARM where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BindDataGraphPipelineSessionMemoryInfoARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_DATA_GRAPH_PIPELINE_SESSION_MEMORY_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DataGraphPipelineSessionARM)) (session)
    poke ((p `plusPtr` 24 :: Ptr DataGraphPipelineSessionBindPointARM)) (bindPoint)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (objectIndex)
    poke ((p `plusPtr` 32 :: Ptr DeviceMemory)) (memory)
    poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (memoryOffset)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_DATA_GRAPH_PIPELINE_SESSION_MEMORY_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DataGraphPipelineSessionARM)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DataGraphPipelineSessionBindPointARM)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr DeviceMemory)) (zero)
    poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct BindDataGraphPipelineSessionMemoryInfoARM where
  peekCStruct p = do
    session <- peek @DataGraphPipelineSessionARM ((p `plusPtr` 16 :: Ptr DataGraphPipelineSessionARM))
    bindPoint <- peek @DataGraphPipelineSessionBindPointARM ((p `plusPtr` 24 :: Ptr DataGraphPipelineSessionBindPointARM))
    objectIndex <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    memory <- peek @DeviceMemory ((p `plusPtr` 32 :: Ptr DeviceMemory))
    memoryOffset <- peek @DeviceSize ((p `plusPtr` 40 :: Ptr DeviceSize))
    pure $ BindDataGraphPipelineSessionMemoryInfoARM
             session bindPoint objectIndex memory memoryOffset

instance Storable BindDataGraphPipelineSessionMemoryInfoARM where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BindDataGraphPipelineSessionMemoryInfoARM where
  zero = BindDataGraphPipelineSessionMemoryInfoARM
           zero
           zero
           zero
           zero
           zero


-- | VkDataGraphPipelineInfoARM - Structure describing a data graph pipeline
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- 'Vulkan.Core10.Handles.Pipeline',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getDataGraphPipelineAvailablePropertiesARM',
-- 'getDataGraphPipelinePropertiesARM'
data DataGraphPipelineInfoARM = DataGraphPipelineInfoARM
  { -- | @dataGraphPipeline@ is a 'Vulkan.Core10.Handles.Pipeline' handle.
    --
    -- #VUID-VkDataGraphPipelineInfoARM-dataGraphPipeline-09803#
    -- @dataGraphPipeline@ /must/ have been created with
    -- 'createDataGraphPipelinesARM'
    --
    -- #VUID-VkDataGraphPipelineInfoARM-dataGraphPipeline-parameter#
    -- @dataGraphPipeline@ /must/ be a valid 'Vulkan.Core10.Handles.Pipeline'
    -- handle
    dataGraphPipeline :: Pipeline }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DataGraphPipelineInfoARM)
#endif
deriving instance Show DataGraphPipelineInfoARM

instance ToCStruct DataGraphPipelineInfoARM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DataGraphPipelineInfoARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Pipeline)) (dataGraphPipeline)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Pipeline)) (zero)
    f

instance FromCStruct DataGraphPipelineInfoARM where
  peekCStruct p = do
    dataGraphPipeline <- peek @Pipeline ((p `plusPtr` 16 :: Ptr Pipeline))
    pure $ DataGraphPipelineInfoARM
             dataGraphPipeline

instance Storable DataGraphPipelineInfoARM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DataGraphPipelineInfoARM where
  zero = DataGraphPipelineInfoARM
           zero


-- | VkDataGraphPipelinePropertyQueryResultARM - Structure describing a data
-- graph pipeline property query or result
--
-- = Description
--
-- If @pData@ is @NULL@, then the size, in bytes, of the property data is
-- returned in @dataSize@. Otherwise, @dataSize@ must be the size of the
-- buffer, in bytes, pointed to by @pData@ and on return @dataSize@ is
-- overwritten with the number of bytes of data actually written to @pData@
-- including any trailing NUL character. If @dataSize@ is less than the
-- size, in bytes, of the property data, at most @dataSize@ bytes of data
-- will be written to @pData@, and 'Vulkan.Core10.Enums.Result.INCOMPLETE'
-- will be returned by 'getDataGraphPipelinePropertiesARM' instead of
-- 'Vulkan.Core10.Enums.Result.SUCCESS', to indicate that not all the
-- available property data was returned. If @isText@ is
-- 'Vulkan.Core10.FundamentalTypes.TRUE' and @pData@ is not @NULL@ and
-- @dataSize@ is not zero, the last byte written to @pData@ will be a NUL
-- character.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDataGraphPipelinePropertyQueryResultARM-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_PROPERTY_QUERY_RESULT_ARM'
--
-- -   #VUID-VkDataGraphPipelinePropertyQueryResultARM-pNext-pNext# @pNext@
--     /must/ be @NULL@
--
-- -   #VUID-VkDataGraphPipelinePropertyQueryResultARM-property-parameter#
--     @property@ /must/ be a valid 'DataGraphPipelinePropertyARM' value
--
-- -   #VUID-VkDataGraphPipelinePropertyQueryResultARM-pData-parameter# If
--     @dataSize@ is not @0@, and @pData@ is not @NULL@, @pData@ /must/ be
--     a valid pointer to an array of @dataSize@ bytes
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32', 'DataGraphPipelinePropertyARM',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getDataGraphPipelinePropertiesARM'
data DataGraphPipelinePropertyQueryResultARM = DataGraphPipelinePropertyQueryResultARM
  { -- | @property@ is a 'DataGraphPipelinePropertyARM' specifying the property
    -- of the data graph pipeline being queried.
    property :: DataGraphPipelinePropertyARM
  , -- | @isText@ specifies whether the returned data is text or opaque data. If
    -- @isText@ is 'Vulkan.Core10.FundamentalTypes.TRUE' then the data returned
    -- in @pData@ is text and guaranteed to be a null-terminated UTF-8 string.
    isText :: Bool
  , -- | @dataSize@ is an integer related to the size, in bytes, of the data, as
    -- described below.
    dataSize :: Word64
  , -- | @pData@ is either @NULL@ or a pointer to a block of memory into which
    -- the implementation will return the property data.
    data' :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DataGraphPipelinePropertyQueryResultARM)
#endif
deriving instance Show DataGraphPipelinePropertyQueryResultARM

instance ToCStruct DataGraphPipelinePropertyQueryResultARM where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DataGraphPipelinePropertyQueryResultARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_PROPERTY_QUERY_RESULT_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DataGraphPipelinePropertyARM)) (property)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (isText))
    poke ((p `plusPtr` 24 :: Ptr CSize)) (CSize (dataSize))
    poke ((p `plusPtr` 32 :: Ptr (Ptr ()))) (data')
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_PROPERTY_QUERY_RESULT_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DataGraphPipelinePropertyARM)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct DataGraphPipelinePropertyQueryResultARM where
  peekCStruct p = do
    property <- peek @DataGraphPipelinePropertyARM ((p `plusPtr` 16 :: Ptr DataGraphPipelinePropertyARM))
    isText <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    dataSize <- peek @CSize ((p `plusPtr` 24 :: Ptr CSize))
    pData <- peek @(Ptr ()) ((p `plusPtr` 32 :: Ptr (Ptr ())))
    pure $ DataGraphPipelinePropertyQueryResultARM
             property
             (bool32ToBool isText)
             (coerce @CSize @Word64 dataSize)
             pData

instance Storable DataGraphPipelinePropertyQueryResultARM where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DataGraphPipelinePropertyQueryResultARM where
  zero = DataGraphPipelinePropertyQueryResultARM
           zero
           zero
           zero
           zero


-- | VkDataGraphPipelineIdentifierCreateInfoARM - Structure specifying an
-- identifier for the newly created data graph pipeline
--
-- = Description
--
-- The @pIdentifier@ /can/ be retrieved from the device by calling
-- 'getDataGraphPipelinePropertiesARM' and searching the results for a
-- 'DataGraphPipelinePropertyQueryResultARM' structure with @property@ set
-- to 'DATA_GRAPH_PIPELINE_PROPERTY_IDENTIFIER_ARM'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DataGraphPipelineIdentifierCreateInfoARM = DataGraphPipelineIdentifierCreateInfoARM
  { -- | @pIdentifier@ is a pointer to @identifierSize@ bytes of data that
    -- describe the pipeline being created.
    --
    -- #VUID-VkDataGraphPipelineIdentifierCreateInfoARM-pIdentifier-parameter#
    -- @pIdentifier@ /must/ be a valid pointer to an array of @identifierSize@
    -- @uint8_t@ values
    identifier :: Vector Word8 }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DataGraphPipelineIdentifierCreateInfoARM)
#endif
deriving instance Show DataGraphPipelineIdentifierCreateInfoARM

instance ToCStruct DataGraphPipelineIdentifierCreateInfoARM where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DataGraphPipelineIdentifierCreateInfoARM{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_IDENTIFIER_CREATE_INFO_ARM)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (identifier)) :: Word32))
    pPIdentifier' <- ContT $ allocaBytes @Word8 (Data.Vector.length (identifier))
    lift $ Data.Vector.imapM_ (\i e -> poke (pPIdentifier' `plusPtr` (1 * (i)) :: Ptr Word8) (e)) (identifier)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Word8))) (pPIdentifier')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_IDENTIFIER_CREATE_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct DataGraphPipelineIdentifierCreateInfoARM where
  peekCStruct p = do
    identifierSize <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pIdentifier <- peek @(Ptr Word8) ((p `plusPtr` 24 :: Ptr (Ptr Word8)))
    pIdentifier' <- generateM (fromIntegral identifierSize) (\i -> peek @Word8 ((pIdentifier `advancePtrBytes` (1 * (i)) :: Ptr Word8)))
    pure $ DataGraphPipelineIdentifierCreateInfoARM
             pIdentifier'

instance Zero DataGraphPipelineIdentifierCreateInfoARM where
  zero = DataGraphPipelineIdentifierCreateInfoARM
           mempty


-- | VkDataGraphPipelineDispatchInfoARM - Structure specifying parameters of
-- a data graph pipeline dispatch
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- 'DataGraphPipelineDispatchFlagsARM',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdDispatchDataGraphARM'
data DataGraphPipelineDispatchInfoARM = DataGraphPipelineDispatchInfoARM
  { -- | @flags@ is a bitmask of 'DataGraphPipelineDispatchFlagBitsARM'
    -- describing additional parameters of the dispatch.
    --
    -- #VUID-VkDataGraphPipelineDispatchInfoARM-flags-zerobitmask# @flags@
    -- /must/ be @0@
    flags :: DataGraphPipelineDispatchFlagsARM }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DataGraphPipelineDispatchInfoARM)
#endif
deriving instance Show DataGraphPipelineDispatchInfoARM

instance ToCStruct DataGraphPipelineDispatchInfoARM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DataGraphPipelineDispatchInfoARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_DISPATCH_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DataGraphPipelineDispatchFlagsARM)) (flags)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_DISPATCH_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct DataGraphPipelineDispatchInfoARM where
  peekCStruct p = do
    flags <- peek @DataGraphPipelineDispatchFlagsARM ((p `plusPtr` 16 :: Ptr DataGraphPipelineDispatchFlagsARM))
    pure $ DataGraphPipelineDispatchInfoARM
             flags

instance Storable DataGraphPipelineDispatchInfoARM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DataGraphPipelineDispatchInfoARM where
  zero = DataGraphPipelineDispatchInfoARM
           zero


-- | VkPhysicalDeviceDataGraphProcessingEngineARM - Structure describing a
-- data graph processing engine supported by a physical device
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'DataGraphProcessingEngineCreateInfoARM',
-- 'PhysicalDeviceDataGraphProcessingEngineTypeARM',
-- 'QueueFamilyDataGraphPropertiesARM'
data PhysicalDeviceDataGraphProcessingEngineARM = PhysicalDeviceDataGraphProcessingEngineARM
  { -- | @type@ is a 'PhysicalDeviceDataGraphProcessingEngineTypeARM' that
    -- specifies the type of the processing engine.
    type' :: PhysicalDeviceDataGraphProcessingEngineTypeARM
  , -- | @isForeign@ specifies whether the processing engine is foreign.
    isForeign :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDataGraphProcessingEngineARM)
#endif
deriving instance Show PhysicalDeviceDataGraphProcessingEngineARM

instance ToCStruct PhysicalDeviceDataGraphProcessingEngineARM where
  withCStruct x f = allocaBytes 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDataGraphProcessingEngineARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr PhysicalDeviceDataGraphProcessingEngineTypeARM)) (type')
    poke ((p `plusPtr` 4 :: Ptr Bool32)) (boolToBool32 (isForeign))
    f
  cStructSize = 8
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr PhysicalDeviceDataGraphProcessingEngineTypeARM)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceDataGraphProcessingEngineARM where
  peekCStruct p = do
    type' <- peek @PhysicalDeviceDataGraphProcessingEngineTypeARM ((p `plusPtr` 0 :: Ptr PhysicalDeviceDataGraphProcessingEngineTypeARM))
    isForeign <- peek @Bool32 ((p `plusPtr` 4 :: Ptr Bool32))
    pure $ PhysicalDeviceDataGraphProcessingEngineARM
             type' (bool32ToBool isForeign)

instance Storable PhysicalDeviceDataGraphProcessingEngineARM where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDataGraphProcessingEngineARM where
  zero = PhysicalDeviceDataGraphProcessingEngineARM
           zero
           zero


-- | VkPhysicalDeviceDataGraphOperationSupportARM - Structure describing an
-- operation or set of operations supported by a data graph processing
-- engine
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- 'Vulkan.Extensions.VK_QCOM_data_graph_model.DataGraphPipelineBuiltinModelCreateInfoQCOM',
-- 'PhysicalDeviceDataGraphOperationTypeARM',
-- 'QueueFamilyDataGraphPropertiesARM'
data PhysicalDeviceDataGraphOperationSupportARM = PhysicalDeviceDataGraphOperationSupportARM
  { -- | @operationType@ is a 'PhysicalDeviceDataGraphOperationTypeARM' enum
    -- specifying the type of the operation whose support is being described.
    operationType :: PhysicalDeviceDataGraphOperationTypeARM
  , -- | @name@ is a pointer to a null-terminated UTF-8 string specifying the
    -- name of the operation whose support is being described.
    name :: ByteString
  , -- | @version@ is an integer specifying the version of the operation whose
    -- support is being described.
    version :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDataGraphOperationSupportARM)
#endif
deriving instance Show PhysicalDeviceDataGraphOperationSupportARM

instance ToCStruct PhysicalDeviceDataGraphOperationSupportARM where
  withCStruct x f = allocaBytes 136 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDataGraphOperationSupportARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr PhysicalDeviceDataGraphOperationTypeARM)) (operationType)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 4 :: Ptr (FixedArray MAX_PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_SET_NAME_SIZE_ARM CChar))) (name)
    poke ((p `plusPtr` 132 :: Ptr Word32)) (version)
    f
  cStructSize = 136
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr PhysicalDeviceDataGraphOperationTypeARM)) (zero)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 4 :: Ptr (FixedArray MAX_PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_SET_NAME_SIZE_ARM CChar))) (mempty)
    poke ((p `plusPtr` 132 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceDataGraphOperationSupportARM where
  peekCStruct p = do
    operationType <- peek @PhysicalDeviceDataGraphOperationTypeARM ((p `plusPtr` 0 :: Ptr PhysicalDeviceDataGraphOperationTypeARM))
    name <- packCString (lowerArrayPtr ((p `plusPtr` 4 :: Ptr (FixedArray MAX_PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_SET_NAME_SIZE_ARM CChar))))
    version <- peek @Word32 ((p `plusPtr` 132 :: Ptr Word32))
    pure $ PhysicalDeviceDataGraphOperationSupportARM
             operationType name version

instance Storable PhysicalDeviceDataGraphOperationSupportARM where
  sizeOf ~_ = 136
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDataGraphOperationSupportARM where
  zero = PhysicalDeviceDataGraphOperationSupportARM
           zero
           mempty
           zero


-- | VkQueueFamilyDataGraphPropertiesARM - Structure describing a data graph
-- processing engine and operation it supports
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- 'PhysicalDeviceDataGraphOperationSupportARM',
-- 'PhysicalDeviceDataGraphProcessingEngineARM',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPhysicalDeviceQueueFamilyDataGraphPropertiesARM'
data QueueFamilyDataGraphPropertiesARM = QueueFamilyDataGraphPropertiesARM
  { -- | @engine@ is a 'PhysicalDeviceDataGraphProcessingEngineARM' structure
    -- describing a data graph processing engine.
    engine :: PhysicalDeviceDataGraphProcessingEngineARM
  , -- | @operation@ is a 'PhysicalDeviceDataGraphOperationSupportARM' structure
    -- describing one or more operations supported by a data graph processing
    -- engine.
    operation :: PhysicalDeviceDataGraphOperationSupportARM
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (QueueFamilyDataGraphPropertiesARM)
#endif
deriving instance Show QueueFamilyDataGraphPropertiesARM

instance ToCStruct QueueFamilyDataGraphPropertiesARM where
  withCStruct x f = allocaBytes 160 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p QueueFamilyDataGraphPropertiesARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_QUEUE_FAMILY_DATA_GRAPH_PROPERTIES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PhysicalDeviceDataGraphProcessingEngineARM)) (engine)
    poke ((p `plusPtr` 24 :: Ptr PhysicalDeviceDataGraphOperationSupportARM)) (operation)
    f
  cStructSize = 160
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_QUEUE_FAMILY_DATA_GRAPH_PROPERTIES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PhysicalDeviceDataGraphProcessingEngineARM)) (zero)
    poke ((p `plusPtr` 24 :: Ptr PhysicalDeviceDataGraphOperationSupportARM)) (zero)
    f

instance FromCStruct QueueFamilyDataGraphPropertiesARM where
  peekCStruct p = do
    engine <- peekCStruct @PhysicalDeviceDataGraphProcessingEngineARM ((p `plusPtr` 16 :: Ptr PhysicalDeviceDataGraphProcessingEngineARM))
    operation <- peekCStruct @PhysicalDeviceDataGraphOperationSupportARM ((p `plusPtr` 24 :: Ptr PhysicalDeviceDataGraphOperationSupportARM))
    pure $ QueueFamilyDataGraphPropertiesARM
             engine operation

instance Storable QueueFamilyDataGraphPropertiesARM where
  sizeOf ~_ = 160
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero QueueFamilyDataGraphPropertiesARM where
  zero = QueueFamilyDataGraphPropertiesARM
           zero
           zero


-- | VkPhysicalDeviceQueueFamilyDataGraphProcessingEngineInfoARM - Structure
-- specifying a data graph processing engine type and queue family to query
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- 'PhysicalDeviceDataGraphProcessingEngineTypeARM',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPhysicalDeviceQueueFamilyDataGraphProcessingEnginePropertiesARM'
data PhysicalDeviceQueueFamilyDataGraphProcessingEngineInfoARM = PhysicalDeviceQueueFamilyDataGraphProcessingEngineInfoARM
  { -- | @queueFamilyIndex@ specifies the queue family being queried.
    queueFamilyIndex :: Word32
  , -- | @engineType@ is a 'PhysicalDeviceDataGraphProcessingEngineTypeARM'
    -- specifying the engine type whose properties are being queried.
    --
    -- #VUID-VkPhysicalDeviceQueueFamilyDataGraphProcessingEngineInfoARM-engineType-parameter#
    -- @engineType@ /must/ be a valid
    -- 'PhysicalDeviceDataGraphProcessingEngineTypeARM' value
    engineType :: PhysicalDeviceDataGraphProcessingEngineTypeARM
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceQueueFamilyDataGraphProcessingEngineInfoARM)
#endif
deriving instance Show PhysicalDeviceQueueFamilyDataGraphProcessingEngineInfoARM

instance ToCStruct PhysicalDeviceQueueFamilyDataGraphProcessingEngineInfoARM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceQueueFamilyDataGraphProcessingEngineInfoARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_QUEUE_FAMILY_DATA_GRAPH_PROCESSING_ENGINE_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (queueFamilyIndex)
    poke ((p `plusPtr` 20 :: Ptr PhysicalDeviceDataGraphProcessingEngineTypeARM)) (engineType)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_QUEUE_FAMILY_DATA_GRAPH_PROCESSING_ENGINE_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr PhysicalDeviceDataGraphProcessingEngineTypeARM)) (zero)
    f

instance FromCStruct PhysicalDeviceQueueFamilyDataGraphProcessingEngineInfoARM where
  peekCStruct p = do
    queueFamilyIndex <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    engineType <- peek @PhysicalDeviceDataGraphProcessingEngineTypeARM ((p `plusPtr` 20 :: Ptr PhysicalDeviceDataGraphProcessingEngineTypeARM))
    pure $ PhysicalDeviceQueueFamilyDataGraphProcessingEngineInfoARM
             queueFamilyIndex engineType

instance Storable PhysicalDeviceQueueFamilyDataGraphProcessingEngineInfoARM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceQueueFamilyDataGraphProcessingEngineInfoARM where
  zero = PhysicalDeviceQueueFamilyDataGraphProcessingEngineInfoARM
           zero
           zero


-- | VkQueueFamilyDataGraphProcessingEnginePropertiesARM - Structure
-- describing the properties of a data graph processing engine type for a
-- given queue family
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlags',
-- 'Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.ExternalSemaphoreHandleTypeFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPhysicalDeviceQueueFamilyDataGraphProcessingEnginePropertiesARM'
data QueueFamilyDataGraphProcessingEnginePropertiesARM = QueueFamilyDataGraphProcessingEnginePropertiesARM
  { -- | @foreignSemaphoreHandleTypes@ is a
    -- 'Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.ExternalSemaphoreHandleTypeFlags'
    -- that describes the external semaphore handle types supported by a
    -- foreign data graph processing engine.
    foreignSemaphoreHandleTypes :: ExternalSemaphoreHandleTypeFlags
  , -- | @foreignMemoryHandleTypes@ is a
    -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlags'
    -- that describes the external memory handle types supported by a foreign
    -- data graph processing engine.
    foreignMemoryHandleTypes :: ExternalMemoryHandleTypeFlags
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (QueueFamilyDataGraphProcessingEnginePropertiesARM)
#endif
deriving instance Show QueueFamilyDataGraphProcessingEnginePropertiesARM

instance ToCStruct QueueFamilyDataGraphProcessingEnginePropertiesARM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p QueueFamilyDataGraphProcessingEnginePropertiesARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_QUEUE_FAMILY_DATA_GRAPH_PROCESSING_ENGINE_PROPERTIES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalSemaphoreHandleTypeFlags)) (foreignSemaphoreHandleTypes)
    poke ((p `plusPtr` 20 :: Ptr ExternalMemoryHandleTypeFlags)) (foreignMemoryHandleTypes)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_QUEUE_FAMILY_DATA_GRAPH_PROCESSING_ENGINE_PROPERTIES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalSemaphoreHandleTypeFlags)) (zero)
    poke ((p `plusPtr` 20 :: Ptr ExternalMemoryHandleTypeFlags)) (zero)
    f

instance FromCStruct QueueFamilyDataGraphProcessingEnginePropertiesARM where
  peekCStruct p = do
    foreignSemaphoreHandleTypes <- peek @ExternalSemaphoreHandleTypeFlags ((p `plusPtr` 16 :: Ptr ExternalSemaphoreHandleTypeFlags))
    foreignMemoryHandleTypes <- peek @ExternalMemoryHandleTypeFlags ((p `plusPtr` 20 :: Ptr ExternalMemoryHandleTypeFlags))
    pure $ QueueFamilyDataGraphProcessingEnginePropertiesARM
             foreignSemaphoreHandleTypes foreignMemoryHandleTypes

instance Storable QueueFamilyDataGraphProcessingEnginePropertiesARM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero QueueFamilyDataGraphProcessingEnginePropertiesARM where
  zero = QueueFamilyDataGraphProcessingEnginePropertiesARM
           zero
           zero


-- | VkDataGraphProcessingEngineCreateInfoARM - Structure describing a
-- collection of data graph processing engines for which the object being
-- created is specialized
--
-- == Valid Usage
--
-- -   #VUID-VkDataGraphProcessingEngineCreateInfoARM-dataGraph-09953# The
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-dataGraph dataGraph>
--     feature /must/ be enabled
--
-- -   #VUID-VkDataGraphProcessingEngineCreateInfoARM-pProcessingEngines-09918#
--     @pProcessingEngines@ /must/ not contain identical
--     'PhysicalDeviceDataGraphProcessingEngineARM' structures
--
-- -   #VUID-VkDataGraphProcessingEngineCreateInfoARM-pProcessingEngines-09956#
--     For each element of @pProcessingEngines@, its @type@ member /must/
--     be a valid 'PhysicalDeviceDataGraphProcessingEngineTypeARM' value
--
-- -   #VUID-VkDataGraphProcessingEngineCreateInfoARM-pProcessingEngines-11843#
--     If any element of @pProcessingEngines@ has a @type@ of
--     'PHYSICAL_DEVICE_DATA_GRAPH_PROCESSING_ENGINE_TYPE_NEURAL_QCOM' or
--     'PHYSICAL_DEVICE_DATA_GRAPH_PROCESSING_ENGINE_TYPE_COMPUTE_QCOM' and
--     @isForeign@ set to 'Vulkan.Core10.FundamentalTypes.TRUE',
--     @processingEngineCount@ /must/ equal @1@
--
-- -   #VUID-VkDataGraphProcessingEngineCreateInfoARM-pProcessingEngines-11844#
--     If any element of @pProcessingEngines@ has a @type@ of
--     'PHYSICAL_DEVICE_DATA_GRAPH_PROCESSING_ENGINE_TYPE_NEURAL_QCOM' or
--     'PHYSICAL_DEVICE_DATA_GRAPH_PROCESSING_ENGINE_TYPE_COMPUTE_QCOM',
--     the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-dataGraphModelQCOM dataGraphModel>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDataGraphProcessingEngineCreateInfoARM-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PROCESSING_ENGINE_CREATE_INFO_ARM'
--
-- -   #VUID-VkDataGraphProcessingEngineCreateInfoARM-pProcessingEngines-parameter#
--     @pProcessingEngines@ /must/ be a valid pointer to an array of
--     @processingEngineCount@ 'PhysicalDeviceDataGraphProcessingEngineARM'
--     structures
--
-- -   #VUID-VkDataGraphProcessingEngineCreateInfoARM-processingEngineCount-arraylength#
--     @processingEngineCount@ /must/ be greater than @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- 'PhysicalDeviceDataGraphProcessingEngineARM',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DataGraphProcessingEngineCreateInfoARM = DataGraphProcessingEngineCreateInfoARM
  { -- | @processingEngineCount@ is the number of elements in
    -- @pProcessingEngines@.
    processingEngineCount :: Word32
  , -- | @pProcessingEngines@ is a pointer to an array of @processingEngineCount@
    -- 'PhysicalDeviceDataGraphProcessingEngineARM' structures.
    processingEngines :: Ptr PhysicalDeviceDataGraphProcessingEngineARM
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DataGraphProcessingEngineCreateInfoARM)
#endif
deriving instance Show DataGraphProcessingEngineCreateInfoARM

instance ToCStruct DataGraphProcessingEngineCreateInfoARM where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DataGraphProcessingEngineCreateInfoARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PROCESSING_ENGINE_CREATE_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (processingEngineCount)
    poke ((p `plusPtr` 24 :: Ptr (Ptr PhysicalDeviceDataGraphProcessingEngineARM))) (processingEngines)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PROCESSING_ENGINE_CREATE_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr (Ptr PhysicalDeviceDataGraphProcessingEngineARM))) (zero)
    f

instance FromCStruct DataGraphProcessingEngineCreateInfoARM where
  peekCStruct p = do
    processingEngineCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pProcessingEngines <- peek @(Ptr PhysicalDeviceDataGraphProcessingEngineARM) ((p `plusPtr` 24 :: Ptr (Ptr PhysicalDeviceDataGraphProcessingEngineARM)))
    pure $ DataGraphProcessingEngineCreateInfoARM
             processingEngineCount pProcessingEngines

instance Storable DataGraphProcessingEngineCreateInfoARM where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DataGraphProcessingEngineCreateInfoARM where
  zero = DataGraphProcessingEngineCreateInfoARM
           zero
           zero


type DataGraphPipelineSessionCreateFlagsARM = DataGraphPipelineSessionCreateFlagBitsARM

-- | VkDataGraphPipelineSessionCreateFlagBitsARM - Bitmask specifying
-- additional parameters of a data graph pipeline session
--
-- = Description
--
-- -   'DATA_GRAPH_PIPELINE_SESSION_CREATE_PROTECTED_BIT_ARM' specifies
--     that the data graph pipeline session is backed by protected memory.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- 'DataGraphPipelineSessionCreateFlagsARM'
newtype DataGraphPipelineSessionCreateFlagBitsARM = DataGraphPipelineSessionCreateFlagBitsARM Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkDataGraphPipelineSessionCreateFlagBitsARM" "VK_DATA_GRAPH_PIPELINE_SESSION_CREATE_PROTECTED_BIT_ARM"
pattern DATA_GRAPH_PIPELINE_SESSION_CREATE_PROTECTED_BIT_ARM = DataGraphPipelineSessionCreateFlagBitsARM 0x0000000000000001

conNameDataGraphPipelineSessionCreateFlagBitsARM :: String
conNameDataGraphPipelineSessionCreateFlagBitsARM = "DataGraphPipelineSessionCreateFlagBitsARM"

enumPrefixDataGraphPipelineSessionCreateFlagBitsARM :: String
enumPrefixDataGraphPipelineSessionCreateFlagBitsARM = "DATA_GRAPH_PIPELINE_SESSION_CREATE_PROTECTED_BIT_ARM"

showTableDataGraphPipelineSessionCreateFlagBitsARM :: [(DataGraphPipelineSessionCreateFlagBitsARM, String)]
showTableDataGraphPipelineSessionCreateFlagBitsARM =
  [
    ( DATA_GRAPH_PIPELINE_SESSION_CREATE_PROTECTED_BIT_ARM
    , ""
    )
  ]

instance Show DataGraphPipelineSessionCreateFlagBitsARM where
  showsPrec =
    enumShowsPrec
      enumPrefixDataGraphPipelineSessionCreateFlagBitsARM
      showTableDataGraphPipelineSessionCreateFlagBitsARM
      conNameDataGraphPipelineSessionCreateFlagBitsARM
      (\(DataGraphPipelineSessionCreateFlagBitsARM x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read DataGraphPipelineSessionCreateFlagBitsARM where
  readPrec =
    enumReadPrec
      enumPrefixDataGraphPipelineSessionCreateFlagBitsARM
      showTableDataGraphPipelineSessionCreateFlagBitsARM
      conNameDataGraphPipelineSessionCreateFlagBitsARM
      DataGraphPipelineSessionCreateFlagBitsARM

-- | VkDataGraphPipelineSessionBindPointARM - Enumeration describing the bind
-- points of a data graph pipeline session
--
-- = Description
--
-- -   'DATA_GRAPH_PIPELINE_SESSION_BIND_POINT_TRANSIENT_ARM' corresponds
--     to the transient data produced and consumed during one dispatch of a
--     data graph pipeline in a data graph pipeline session. This transient
--     data is never reused by subsequent dispatches and can safely be
--     clobbered once a 'cmdDispatchDataGraphARM' command completes
--     execution.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- 'BindDataGraphPipelineSessionMemoryInfoARM',
-- 'DataGraphPipelineSessionBindPointRequirementARM',
-- 'DataGraphPipelineSessionMemoryRequirementsInfoARM'
newtype DataGraphPipelineSessionBindPointARM = DataGraphPipelineSessionBindPointARM Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkDataGraphPipelineSessionBindPointARM" "VK_DATA_GRAPH_PIPELINE_SESSION_BIND_POINT_TRANSIENT_ARM"
pattern DATA_GRAPH_PIPELINE_SESSION_BIND_POINT_TRANSIENT_ARM = DataGraphPipelineSessionBindPointARM 0

{-# COMPLETE DATA_GRAPH_PIPELINE_SESSION_BIND_POINT_TRANSIENT_ARM :: DataGraphPipelineSessionBindPointARM #-}

conNameDataGraphPipelineSessionBindPointARM :: String
conNameDataGraphPipelineSessionBindPointARM = "DataGraphPipelineSessionBindPointARM"

enumPrefixDataGraphPipelineSessionBindPointARM :: String
enumPrefixDataGraphPipelineSessionBindPointARM = "DATA_GRAPH_PIPELINE_SESSION_BIND_POINT_TRANSIENT_ARM"

showTableDataGraphPipelineSessionBindPointARM :: [(DataGraphPipelineSessionBindPointARM, String)]
showTableDataGraphPipelineSessionBindPointARM =
  [
    ( DATA_GRAPH_PIPELINE_SESSION_BIND_POINT_TRANSIENT_ARM
    , ""
    )
  ]

instance Show DataGraphPipelineSessionBindPointARM where
  showsPrec =
    enumShowsPrec
      enumPrefixDataGraphPipelineSessionBindPointARM
      showTableDataGraphPipelineSessionBindPointARM
      conNameDataGraphPipelineSessionBindPointARM
      (\(DataGraphPipelineSessionBindPointARM x) -> x)
      (showsPrec 11)

instance Read DataGraphPipelineSessionBindPointARM where
  readPrec =
    enumReadPrec
      enumPrefixDataGraphPipelineSessionBindPointARM
      showTableDataGraphPipelineSessionBindPointARM
      conNameDataGraphPipelineSessionBindPointARM
      DataGraphPipelineSessionBindPointARM

-- | VkDataGraphPipelineSessionBindPointTypeARM - Enumeration describing the
-- type of bind points of a data graph pipeline session
--
-- = Description
--
-- -   'DATA_GRAPH_PIPELINE_SESSION_BIND_POINT_TYPE_MEMORY_ARM' corresponds
--     to a memory allocation.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- 'DataGraphPipelineSessionBindPointRequirementARM'
newtype DataGraphPipelineSessionBindPointTypeARM = DataGraphPipelineSessionBindPointTypeARM Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkDataGraphPipelineSessionBindPointTypeARM" "VK_DATA_GRAPH_PIPELINE_SESSION_BIND_POINT_TYPE_MEMORY_ARM"
pattern DATA_GRAPH_PIPELINE_SESSION_BIND_POINT_TYPE_MEMORY_ARM = DataGraphPipelineSessionBindPointTypeARM 0

{-# COMPLETE DATA_GRAPH_PIPELINE_SESSION_BIND_POINT_TYPE_MEMORY_ARM :: DataGraphPipelineSessionBindPointTypeARM #-}

conNameDataGraphPipelineSessionBindPointTypeARM :: String
conNameDataGraphPipelineSessionBindPointTypeARM = "DataGraphPipelineSessionBindPointTypeARM"

enumPrefixDataGraphPipelineSessionBindPointTypeARM :: String
enumPrefixDataGraphPipelineSessionBindPointTypeARM = "DATA_GRAPH_PIPELINE_SESSION_BIND_POINT_TYPE_MEMORY_ARM"

showTableDataGraphPipelineSessionBindPointTypeARM :: [(DataGraphPipelineSessionBindPointTypeARM, String)]
showTableDataGraphPipelineSessionBindPointTypeARM =
  [
    ( DATA_GRAPH_PIPELINE_SESSION_BIND_POINT_TYPE_MEMORY_ARM
    , ""
    )
  ]

instance Show DataGraphPipelineSessionBindPointTypeARM where
  showsPrec =
    enumShowsPrec
      enumPrefixDataGraphPipelineSessionBindPointTypeARM
      showTableDataGraphPipelineSessionBindPointTypeARM
      conNameDataGraphPipelineSessionBindPointTypeARM
      (\(DataGraphPipelineSessionBindPointTypeARM x) -> x)
      (showsPrec 11)

instance Read DataGraphPipelineSessionBindPointTypeARM where
  readPrec =
    enumReadPrec
      enumPrefixDataGraphPipelineSessionBindPointTypeARM
      showTableDataGraphPipelineSessionBindPointTypeARM
      conNameDataGraphPipelineSessionBindPointTypeARM
      DataGraphPipelineSessionBindPointTypeARM

-- | VkDataGraphPipelinePropertyARM - Enumeration describing the properties
-- of a data graph pipeline that can be queried
--
-- = Description
--
-- -   'DATA_GRAPH_PIPELINE_PROPERTY_CREATION_LOG_ARM' corresponds to a
--     human-readable log produced during the creation of a data graph
--     pipeline. It /may/ contain information about errors encountered
--     during the creation or other information generally useful for
--     debugging. This property /can/ be queried for any data graph
--     pipeline.
--
-- -   'DATA_GRAPH_PIPELINE_PROPERTY_IDENTIFIER_ARM' corresponds to an
--     opaque identifier for the data graph pipeline. It /can/ be used to
--     create a graph pipeline from a pipeline cache without the need to
--     provide any creation data beyond the identifier, using a
--     'DataGraphPipelineIdentifierCreateInfoARM' structure.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- 'DataGraphPipelinePropertyQueryResultARM',
-- 'getDataGraphPipelineAvailablePropertiesARM'
newtype DataGraphPipelinePropertyARM = DataGraphPipelinePropertyARM Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkDataGraphPipelinePropertyARM" "VK_DATA_GRAPH_PIPELINE_PROPERTY_CREATION_LOG_ARM"
pattern DATA_GRAPH_PIPELINE_PROPERTY_CREATION_LOG_ARM = DataGraphPipelinePropertyARM 0

-- No documentation found for Nested "VkDataGraphPipelinePropertyARM" "VK_DATA_GRAPH_PIPELINE_PROPERTY_IDENTIFIER_ARM"
pattern DATA_GRAPH_PIPELINE_PROPERTY_IDENTIFIER_ARM = DataGraphPipelinePropertyARM 1

{-# COMPLETE
  DATA_GRAPH_PIPELINE_PROPERTY_CREATION_LOG_ARM
  , DATA_GRAPH_PIPELINE_PROPERTY_IDENTIFIER_ARM ::
    DataGraphPipelinePropertyARM
  #-}

conNameDataGraphPipelinePropertyARM :: String
conNameDataGraphPipelinePropertyARM = "DataGraphPipelinePropertyARM"

enumPrefixDataGraphPipelinePropertyARM :: String
enumPrefixDataGraphPipelinePropertyARM = "DATA_GRAPH_PIPELINE_PROPERTY_"

showTableDataGraphPipelinePropertyARM :: [(DataGraphPipelinePropertyARM, String)]
showTableDataGraphPipelinePropertyARM =
  [
    ( DATA_GRAPH_PIPELINE_PROPERTY_CREATION_LOG_ARM
    , "CREATION_LOG_ARM"
    )
  ,
    ( DATA_GRAPH_PIPELINE_PROPERTY_IDENTIFIER_ARM
    , "IDENTIFIER_ARM"
    )
  ]

instance Show DataGraphPipelinePropertyARM where
  showsPrec =
    enumShowsPrec
      enumPrefixDataGraphPipelinePropertyARM
      showTableDataGraphPipelinePropertyARM
      conNameDataGraphPipelinePropertyARM
      (\(DataGraphPipelinePropertyARM x) -> x)
      (showsPrec 11)

instance Read DataGraphPipelinePropertyARM where
  readPrec =
    enumReadPrec
      enumPrefixDataGraphPipelinePropertyARM
      showTableDataGraphPipelinePropertyARM
      conNameDataGraphPipelinePropertyARM
      DataGraphPipelinePropertyARM

type DataGraphPipelineDispatchFlagsARM = DataGraphPipelineDispatchFlagBitsARM

-- | VkDataGraphPipelineDispatchFlagBitsARM - Bitmask specifying additional
-- parameters of a data graph pipeline dispatch
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- 'DataGraphPipelineDispatchFlagsARM'
newtype DataGraphPipelineDispatchFlagBitsARM = DataGraphPipelineDispatchFlagBitsARM Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

conNameDataGraphPipelineDispatchFlagBitsARM :: String
conNameDataGraphPipelineDispatchFlagBitsARM = "DataGraphPipelineDispatchFlagBitsARM"

enumPrefixDataGraphPipelineDispatchFlagBitsARM :: String
enumPrefixDataGraphPipelineDispatchFlagBitsARM = ""

showTableDataGraphPipelineDispatchFlagBitsARM :: [(DataGraphPipelineDispatchFlagBitsARM, String)]
showTableDataGraphPipelineDispatchFlagBitsARM = []

instance Show DataGraphPipelineDispatchFlagBitsARM where
  showsPrec =
    enumShowsPrec
      enumPrefixDataGraphPipelineDispatchFlagBitsARM
      showTableDataGraphPipelineDispatchFlagBitsARM
      conNameDataGraphPipelineDispatchFlagBitsARM
      (\(DataGraphPipelineDispatchFlagBitsARM x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read DataGraphPipelineDispatchFlagBitsARM where
  readPrec =
    enumReadPrec
      enumPrefixDataGraphPipelineDispatchFlagBitsARM
      showTableDataGraphPipelineDispatchFlagBitsARM
      conNameDataGraphPipelineDispatchFlagBitsARM
      DataGraphPipelineDispatchFlagBitsARM

-- | VkPhysicalDeviceDataGraphProcessingEngineTypeARM - Enumeration
-- describing data graph processing engines
--
-- = Description
--
-- -   'PHYSICAL_DEVICE_DATA_GRAPH_PROCESSING_ENGINE_TYPE_DEFAULT_ARM'
--     corresponds to the default data graph processing engine.
--
-- -   'PHYSICAL_DEVICE_DATA_GRAPH_PROCESSING_ENGINE_TYPE_NEURAL_QCOM'
--     specifies an engine that specializes in neural processing.
--
-- -   'PHYSICAL_DEVICE_DATA_GRAPH_PROCESSING_ENGINE_TYPE_COMPUTE_QCOM'
--     specifies an engine that uses compute processing to execute data
--     graphs.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- 'PhysicalDeviceDataGraphProcessingEngineARM',
-- 'PhysicalDeviceQueueFamilyDataGraphProcessingEngineInfoARM'
newtype PhysicalDeviceDataGraphProcessingEngineTypeARM = PhysicalDeviceDataGraphProcessingEngineTypeARM Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkPhysicalDeviceDataGraphProcessingEngineTypeARM" "VK_PHYSICAL_DEVICE_DATA_GRAPH_PROCESSING_ENGINE_TYPE_DEFAULT_ARM"
pattern PHYSICAL_DEVICE_DATA_GRAPH_PROCESSING_ENGINE_TYPE_DEFAULT_ARM = PhysicalDeviceDataGraphProcessingEngineTypeARM 0

-- No documentation found for Nested "VkPhysicalDeviceDataGraphProcessingEngineTypeARM" "VK_PHYSICAL_DEVICE_DATA_GRAPH_PROCESSING_ENGINE_TYPE_COMPUTE_QCOM"
pattern PHYSICAL_DEVICE_DATA_GRAPH_PROCESSING_ENGINE_TYPE_COMPUTE_QCOM = PhysicalDeviceDataGraphProcessingEngineTypeARM 1000629001

-- No documentation found for Nested "VkPhysicalDeviceDataGraphProcessingEngineTypeARM" "VK_PHYSICAL_DEVICE_DATA_GRAPH_PROCESSING_ENGINE_TYPE_NEURAL_QCOM"
pattern PHYSICAL_DEVICE_DATA_GRAPH_PROCESSING_ENGINE_TYPE_NEURAL_QCOM = PhysicalDeviceDataGraphProcessingEngineTypeARM 1000629000

{-# COMPLETE
  PHYSICAL_DEVICE_DATA_GRAPH_PROCESSING_ENGINE_TYPE_DEFAULT_ARM
  , PHYSICAL_DEVICE_DATA_GRAPH_PROCESSING_ENGINE_TYPE_COMPUTE_QCOM
  , PHYSICAL_DEVICE_DATA_GRAPH_PROCESSING_ENGINE_TYPE_NEURAL_QCOM ::
    PhysicalDeviceDataGraphProcessingEngineTypeARM
  #-}

conNamePhysicalDeviceDataGraphProcessingEngineTypeARM :: String
conNamePhysicalDeviceDataGraphProcessingEngineTypeARM = "PhysicalDeviceDataGraphProcessingEngineTypeARM"

enumPrefixPhysicalDeviceDataGraphProcessingEngineTypeARM :: String
enumPrefixPhysicalDeviceDataGraphProcessingEngineTypeARM = "PHYSICAL_DEVICE_DATA_GRAPH_PROCESSING_ENGINE_TYPE_"

showTablePhysicalDeviceDataGraphProcessingEngineTypeARM :: [(PhysicalDeviceDataGraphProcessingEngineTypeARM, String)]
showTablePhysicalDeviceDataGraphProcessingEngineTypeARM =
  [
    ( PHYSICAL_DEVICE_DATA_GRAPH_PROCESSING_ENGINE_TYPE_DEFAULT_ARM
    , "DEFAULT_ARM"
    )
  ,
    ( PHYSICAL_DEVICE_DATA_GRAPH_PROCESSING_ENGINE_TYPE_COMPUTE_QCOM
    , "COMPUTE_QCOM"
    )
  ,
    ( PHYSICAL_DEVICE_DATA_GRAPH_PROCESSING_ENGINE_TYPE_NEURAL_QCOM
    , "NEURAL_QCOM"
    )
  ]

instance Show PhysicalDeviceDataGraphProcessingEngineTypeARM where
  showsPrec =
    enumShowsPrec
      enumPrefixPhysicalDeviceDataGraphProcessingEngineTypeARM
      showTablePhysicalDeviceDataGraphProcessingEngineTypeARM
      conNamePhysicalDeviceDataGraphProcessingEngineTypeARM
      (\(PhysicalDeviceDataGraphProcessingEngineTypeARM x) -> x)
      (showsPrec 11)

instance Read PhysicalDeviceDataGraphProcessingEngineTypeARM where
  readPrec =
    enumReadPrec
      enumPrefixPhysicalDeviceDataGraphProcessingEngineTypeARM
      showTablePhysicalDeviceDataGraphProcessingEngineTypeARM
      conNamePhysicalDeviceDataGraphProcessingEngineTypeARM
      PhysicalDeviceDataGraphProcessingEngineTypeARM

-- | VkPhysicalDeviceDataGraphOperationTypeARM - Enumeration describing data
-- graph operations
--
-- = Description
--
-- -   'PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_TYPE_SPIRV_EXTENDED_INSTRUCTION_SET_ARM'
--     corresponds to operations provided by a SPIR-V extended instruction
--     set.
--
-- -   'PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_TYPE_NEURAL_MODEL_QCOM'
--     specifies an operation that executes neural models provided by the
--     application.
--
-- -   'PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_TYPE_BUILTIN_MODEL_QCOM'
--     specifies an operation that executes specialized built-in models
--     provided by the implementation.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>,
-- 'PhysicalDeviceDataGraphOperationSupportARM'
newtype PhysicalDeviceDataGraphOperationTypeARM = PhysicalDeviceDataGraphOperationTypeARM Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkPhysicalDeviceDataGraphOperationTypeARM" "VK_PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_TYPE_SPIRV_EXTENDED_INSTRUCTION_SET_ARM"
pattern PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_TYPE_SPIRV_EXTENDED_INSTRUCTION_SET_ARM = PhysicalDeviceDataGraphOperationTypeARM 0

-- No documentation found for Nested "VkPhysicalDeviceDataGraphOperationTypeARM" "VK_PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_TYPE_BUILTIN_MODEL_QCOM"
pattern PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_TYPE_BUILTIN_MODEL_QCOM = PhysicalDeviceDataGraphOperationTypeARM 1000629001

-- No documentation found for Nested "VkPhysicalDeviceDataGraphOperationTypeARM" "VK_PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_TYPE_NEURAL_MODEL_QCOM"
pattern PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_TYPE_NEURAL_MODEL_QCOM = PhysicalDeviceDataGraphOperationTypeARM 1000629000

{-# COMPLETE
  PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_TYPE_SPIRV_EXTENDED_INSTRUCTION_SET_ARM
  , PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_TYPE_BUILTIN_MODEL_QCOM
  , PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_TYPE_NEURAL_MODEL_QCOM ::
    PhysicalDeviceDataGraphOperationTypeARM
  #-}

conNamePhysicalDeviceDataGraphOperationTypeARM :: String
conNamePhysicalDeviceDataGraphOperationTypeARM = "PhysicalDeviceDataGraphOperationTypeARM"

enumPrefixPhysicalDeviceDataGraphOperationTypeARM :: String
enumPrefixPhysicalDeviceDataGraphOperationTypeARM = "PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_TYPE_"

showTablePhysicalDeviceDataGraphOperationTypeARM :: [(PhysicalDeviceDataGraphOperationTypeARM, String)]
showTablePhysicalDeviceDataGraphOperationTypeARM =
  [
    ( PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_TYPE_SPIRV_EXTENDED_INSTRUCTION_SET_ARM
    , "SPIRV_EXTENDED_INSTRUCTION_SET_ARM"
    )
  ,
    ( PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_TYPE_BUILTIN_MODEL_QCOM
    , "BUILTIN_MODEL_QCOM"
    )
  ,
    ( PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_TYPE_NEURAL_MODEL_QCOM
    , "NEURAL_MODEL_QCOM"
    )
  ]

instance Show PhysicalDeviceDataGraphOperationTypeARM where
  showsPrec =
    enumShowsPrec
      enumPrefixPhysicalDeviceDataGraphOperationTypeARM
      showTablePhysicalDeviceDataGraphOperationTypeARM
      conNamePhysicalDeviceDataGraphOperationTypeARM
      (\(PhysicalDeviceDataGraphOperationTypeARM x) -> x)
      (showsPrec 11)

instance Read PhysicalDeviceDataGraphOperationTypeARM where
  readPrec =
    enumReadPrec
      enumPrefixPhysicalDeviceDataGraphOperationTypeARM
      showTablePhysicalDeviceDataGraphOperationTypeARM
      conNamePhysicalDeviceDataGraphOperationTypeARM
      PhysicalDeviceDataGraphOperationTypeARM

type ARM_DATA_GRAPH_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_ARM_DATA_GRAPH_SPEC_VERSION"
pattern ARM_DATA_GRAPH_SPEC_VERSION :: forall a . Integral a => a
pattern ARM_DATA_GRAPH_SPEC_VERSION = 1


type ARM_DATA_GRAPH_EXTENSION_NAME = "VK_ARM_data_graph"

-- No documentation found for TopLevel "VK_ARM_DATA_GRAPH_EXTENSION_NAME"
pattern ARM_DATA_GRAPH_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern ARM_DATA_GRAPH_EXTENSION_NAME = "VK_ARM_data_graph"

