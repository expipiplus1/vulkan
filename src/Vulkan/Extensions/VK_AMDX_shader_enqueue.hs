{-# language CPP #-}
-- | = Name
--
-- VK_AMDX_shader_enqueue - device extension
--
-- == VK_AMDX_shader_enqueue
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
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_pipeline_library VK_KHR_pipeline_library>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_spirv_1_4 VK_KHR_spirv_1_4>
--
--     -   __This is a /provisional/ extension and /must/ be used with
--         caution. See the
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#boilerplate-provisional-header description>
--         of provisional header files for enablement and stability
--         details.__
--
-- [__API Interactions__]
--
--     -   Interacts with VK_KHR_maintenance5
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/AMDX/SPV_AMDX_shader_enqueue.html SPV_AMDX_shader_enqueue>
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
--     2021-07-22
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
-- This extension adds the ability for developers to enqueue compute shader
-- workgroups from other compute shaders.
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
-- -   Extending 'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo':
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
-- is supported:
--
-- -   Extending 'BufferUsageFlagBits2KHR':
--
--     -   'BUFFER_USAGE_2_EXECUTION_GRAPH_SCRATCH_BIT_AMDX'
--
-- == Version History
--
-- -   Revision 1, 2021-07-22 (Tobias Hector)
--
--     -   Initial revision
--
-- == See Also
--
-- 'Vulkan.Core10.APIConstants.SHADER_INDEX_UNUSED_AMDX',
-- 'DeviceOrHostAddressConstAMDX', 'DispatchGraphCountInfoAMDX',
-- 'DispatchGraphInfoAMDX', 'ExecutionGraphPipelineCreateInfoAMDX',
-- 'ExecutionGraphPipelineScratchSizeAMDX',
-- 'PhysicalDeviceShaderEnqueueFeaturesAMDX',
-- 'PhysicalDeviceShaderEnqueuePropertiesAMDX',
-- 'PipelineShaderStageNodeCreateInfoAMDX', 'cmdDispatchGraphAMDX',
-- 'cmdDispatchGraphIndirectAMDX', 'cmdDispatchGraphIndirectCountAMDX',
-- 'cmdInitializeGraphScratchMemoryAMDX',
-- 'createExecutionGraphPipelinesAMDX',
-- 'getExecutionGraphPipelineNodeIndexAMDX',
-- 'getExecutionGraphPipelineScratchSizeAMDX'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_AMDX_shader_enqueue Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_AMDX_shader_enqueue  ( getExecutionGraphPipelineScratchSizeAMDX
                                                 , getExecutionGraphPipelineNodeIndexAMDX
                                                 , createExecutionGraphPipelinesAMDX
                                                 , cmdInitializeGraphScratchMemoryAMDX
                                                 , cmdDispatchGraphAMDX
                                                 , cmdDispatchGraphIndirectAMDX
                                                 , cmdDispatchGraphIndirectCountAMDX
                                                 , PhysicalDeviceShaderEnqueuePropertiesAMDX(..)
                                                 , PhysicalDeviceShaderEnqueueFeaturesAMDX(..)
                                                 , ExecutionGraphPipelineCreateInfoAMDX(..)
                                                 , PipelineShaderStageNodeCreateInfoAMDX(..)
                                                 , ExecutionGraphPipelineScratchSizeAMDX(..)
                                                 , DispatchGraphInfoAMDX(..)
                                                 , DispatchGraphCountInfoAMDX(..)
                                                 , DeviceOrHostAddressConstAMDX(..)
                                                 , BufferUsageFlags2KHR
                                                 , BufferUsageFlagBits2KHR( BUFFER_USAGE_2_TRANSFER_SRC_BIT_KHR
                                                                          , BUFFER_USAGE_2_TRANSFER_DST_BIT_KHR
                                                                          , BUFFER_USAGE_2_UNIFORM_TEXEL_BUFFER_BIT_KHR
                                                                          , BUFFER_USAGE_2_STORAGE_TEXEL_BUFFER_BIT_KHR
                                                                          , BUFFER_USAGE_2_UNIFORM_BUFFER_BIT_KHR
                                                                          , BUFFER_USAGE_2_STORAGE_BUFFER_BIT_KHR
                                                                          , BUFFER_USAGE_2_INDEX_BUFFER_BIT_KHR
                                                                          , BUFFER_USAGE_2_VERTEX_BUFFER_BIT_KHR
                                                                          , BUFFER_USAGE_2_INDIRECT_BUFFER_BIT_KHR
                                                                          , BUFFER_USAGE_2_MICROMAP_STORAGE_BIT_EXT
                                                                          , BUFFER_USAGE_2_MICROMAP_BUILD_INPUT_READ_ONLY_BIT_EXT
                                                                          , BUFFER_USAGE_2_PUSH_DESCRIPTORS_DESCRIPTOR_BUFFER_BIT_EXT
                                                                          , BUFFER_USAGE_2_RESOURCE_DESCRIPTOR_BUFFER_BIT_EXT
                                                                          , BUFFER_USAGE_2_SAMPLER_DESCRIPTOR_BUFFER_BIT_EXT
                                                                          , BUFFER_USAGE_2_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR
                                                                          , BUFFER_USAGE_2_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR
                                                                          , BUFFER_USAGE_2_SHADER_DEVICE_ADDRESS_BIT_KHR
                                                                          , BUFFER_USAGE_2_VIDEO_ENCODE_SRC_BIT_KHR
                                                                          , BUFFER_USAGE_2_VIDEO_ENCODE_DST_BIT_KHR
                                                                          , BUFFER_USAGE_2_VIDEO_DECODE_DST_BIT_KHR
                                                                          , BUFFER_USAGE_2_VIDEO_DECODE_SRC_BIT_KHR
                                                                          , BUFFER_USAGE_2_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT
                                                                          , BUFFER_USAGE_2_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT
                                                                          , BUFFER_USAGE_2_SHADER_BINDING_TABLE_BIT_KHR
                                                                          , BUFFER_USAGE_2_CONDITIONAL_RENDERING_BIT_EXT
                                                                          , BUFFER_USAGE_2_EXECUTION_GRAPH_SCRATCH_BIT_AMDX
                                                                          , ..
                                                                          )
                                                 , AMDX_SHADER_ENQUEUE_SPEC_VERSION
                                                 , pattern AMDX_SHADER_ENQUEUE_SPEC_VERSION
                                                 , AMDX_SHADER_ENQUEUE_EXTENSION_NAME
                                                 , pattern AMDX_SHADER_ENQUEUE_EXTENSION_NAME
                                                 , PipelineLibraryCreateInfoKHR(..)
                                                 , SHADER_INDEX_UNUSED_AMDX
                                                 , pattern SHADER_INDEX_UNUSED_AMDX
                                                 ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
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
import Numeric (showHex)
import Data.ByteString (packCString)
import Data.ByteString (useAsCString)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.Trans.Cont (runContT)
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
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Extends (peekSomeCStruct)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Core10.FundamentalTypes (DeviceAddress)
import Vulkan.Dynamic (DeviceCmds(pVkCmdDispatchGraphAMDX))
import Vulkan.Dynamic (DeviceCmds(pVkCmdDispatchGraphIndirectAMDX))
import Vulkan.Dynamic (DeviceCmds(pVkCmdDispatchGraphIndirectCountAMDX))
import Vulkan.Dynamic (DeviceCmds(pVkCmdInitializeGraphScratchMemoryAMDX))
import Vulkan.Dynamic (DeviceCmds(pVkCreateExecutionGraphPipelinesAMDX))
import Vulkan.Dynamic (DeviceCmds(pVkGetExecutionGraphPipelineNodeIndexAMDX))
import Vulkan.Dynamic (DeviceCmds(pVkGetExecutionGraphPipelineScratchSizeAMDX))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.FundamentalTypes (Flags64)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.Core10.Handles (Pipeline)
import Vulkan.Core10.Handles (Pipeline(..))
import Vulkan.Core10.Handles (PipelineCache)
import Vulkan.Core10.Handles (PipelineCache(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_pipeline_compiler_control (PipelineCompilerControlCreateInfoAMD)
import Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlags)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_EXT_pipeline_creation_feedback (PipelineCreationFeedbackCreateInfo)
import Vulkan.Core10.Handles (PipelineLayout)
import Vulkan.Extensions.VK_KHR_pipeline_library (PipelineLibraryCreateInfoKHR)
import Vulkan.Core10.Pipeline (PipelineShaderStageCreateInfo)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXECUTION_GRAPH_PIPELINE_CREATE_INFO_AMDX))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXECUTION_GRAPH_PIPELINE_SCRATCH_SIZE_AMDX))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ENQUEUE_FEATURES_AMDX))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ENQUEUE_PROPERTIES_AMDX))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_NODE_CREATE_INFO_AMDX))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_KHR_pipeline_library (PipelineLibraryCreateInfoKHR(..))
import Vulkan.Core10.APIConstants (SHADER_INDEX_UNUSED_AMDX)
import Vulkan.Core10.APIConstants (pattern SHADER_INDEX_UNUSED_AMDX)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetExecutionGraphPipelineScratchSizeAMDX
  :: FunPtr (Ptr Device_T -> Pipeline -> Ptr ExecutionGraphPipelineScratchSizeAMDX -> IO Result) -> Ptr Device_T -> Pipeline -> Ptr ExecutionGraphPipelineScratchSizeAMDX -> IO Result

-- | vkGetExecutionGraphPipelineScratchSizeAMDX - Query scratch space
-- required to dispatch an execution graph
--
-- = Description
--
-- After this function returns, information about the scratch space
-- required will be returned in @pSizeInfo@.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMDX_shader_enqueue VK_AMDX_shader_enqueue>,
-- 'Vulkan.Core10.Handles.Device', 'ExecutionGraphPipelineScratchSizeAMDX',
-- 'Vulkan.Core10.Handles.Pipeline'
getExecutionGraphPipelineScratchSizeAMDX :: forall io
                                          . (MonadIO io)
                                         => -- | @device@ is the that @executionGraph@ was created on.
                                            --
                                            -- #VUID-vkGetExecutionGraphPipelineScratchSizeAMDX-device-parameter#
                                            -- @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                                            Device
                                         -> -- | @executionGraph@ is the execution graph pipeline to query the scratch
                                            -- space for.
                                            --
                                            -- #VUID-vkGetExecutionGraphPipelineScratchSizeAMDX-executionGraph-parameter#
                                            -- @executionGraph@ /must/ be a valid 'Vulkan.Core10.Handles.Pipeline'
                                            -- handle
                                            --
                                            -- #VUID-vkGetExecutionGraphPipelineScratchSizeAMDX-executionGraph-parent#
                                            -- @executionGraph@ /must/ have been created, allocated, or retrieved from
                                            -- @device@
                                            ("executionGraph" ::: Pipeline)
                                         -> io (("sizeInfo" ::: ExecutionGraphPipelineScratchSizeAMDX))
getExecutionGraphPipelineScratchSizeAMDX device
                                           executionGraph = liftIO . evalContT $ do
  let vkGetExecutionGraphPipelineScratchSizeAMDXPtr = pVkGetExecutionGraphPipelineScratchSizeAMDX (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetExecutionGraphPipelineScratchSizeAMDXPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetExecutionGraphPipelineScratchSizeAMDX is null" Nothing Nothing
  let vkGetExecutionGraphPipelineScratchSizeAMDX' = mkVkGetExecutionGraphPipelineScratchSizeAMDX vkGetExecutionGraphPipelineScratchSizeAMDXPtr
  pPSizeInfo <- ContT (withZeroCStruct @ExecutionGraphPipelineScratchSizeAMDX)
  r <- lift $ traceAroundEvent "vkGetExecutionGraphPipelineScratchSizeAMDX" (vkGetExecutionGraphPipelineScratchSizeAMDX'
                                                                               (deviceHandle (device))
                                                                               (executionGraph)
                                                                               (pPSizeInfo))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSizeInfo <- lift $ peekCStruct @ExecutionGraphPipelineScratchSizeAMDX pPSizeInfo
  pure $ (pSizeInfo)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetExecutionGraphPipelineNodeIndexAMDX
  :: FunPtr (Ptr Device_T -> Pipeline -> Ptr PipelineShaderStageNodeCreateInfoAMDX -> Ptr Word32 -> IO Result) -> Ptr Device_T -> Pipeline -> Ptr PipelineShaderStageNodeCreateInfoAMDX -> Ptr Word32 -> IO Result

-- | vkGetExecutionGraphPipelineNodeIndexAMDX - Query internal id of a node
-- in an execution graph
--
-- = Description
--
-- Once this function returns, the contents of @pNodeIndex@ contain the
-- internal node index of the identified node.
--
-- == Valid Usage
--
-- -   #VUID-vkGetExecutionGraphPipelineNodeIndexAMDX-pNodeInfo-09140#
--     @pNodeInfo->pName@ /must/ not be @NULL@
--
-- -   #VUID-vkGetExecutionGraphPipelineNodeIndexAMDX-pNodeInfo-09141#
--     @pNodeInfo->index@ /must/ not be
--     'Vulkan.Core10.APIConstants.SHADER_INDEX_UNUSED_AMDX'
--
-- -   #VUID-vkGetExecutionGraphPipelineNodeIndexAMDX-executionGraph-09142#
--     There /must/ be a node in @executionGraph@ with a shader name and
--     index equal to @pNodeInfo->pName@ and @pNodeInfo->index@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetExecutionGraphPipelineNodeIndexAMDX-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetExecutionGraphPipelineNodeIndexAMDX-executionGraph-parameter#
--     @executionGraph@ /must/ be a valid 'Vulkan.Core10.Handles.Pipeline'
--     handle
--
-- -   #VUID-vkGetExecutionGraphPipelineNodeIndexAMDX-pNodeInfo-parameter#
--     @pNodeInfo@ /must/ be a valid pointer to a valid
--     'PipelineShaderStageNodeCreateInfoAMDX' structure
--
-- -   #VUID-vkGetExecutionGraphPipelineNodeIndexAMDX-pNodeIndex-parameter#
--     @pNodeIndex@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   #VUID-vkGetExecutionGraphPipelineNodeIndexAMDX-executionGraph-parent#
--     @executionGraph@ /must/ have been created, allocated, or retrieved
--     from @device@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMDX_shader_enqueue VK_AMDX_shader_enqueue>,
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Pipeline',
-- 'PipelineShaderStageNodeCreateInfoAMDX'
getExecutionGraphPipelineNodeIndexAMDX :: forall io
                                        . (MonadIO io)
                                       => -- | @device@ is the that @executionGraph@ was created on.
                                          Device
                                       -> -- | @executionGraph@ is the execution graph pipeline to query the internal
                                          -- node index for.
                                          ("executionGraph" ::: Pipeline)
                                       -> -- | @pNodeInfo@ is a pointer to a 'PipelineShaderStageNodeCreateInfoAMDX'
                                          -- structure identifying the name and index of the node to query.
                                          ("nodeInfo" ::: PipelineShaderStageNodeCreateInfoAMDX)
                                       -> io (("nodeIndex" ::: Word32))
getExecutionGraphPipelineNodeIndexAMDX device
                                         executionGraph
                                         nodeInfo = liftIO . evalContT $ do
  let vkGetExecutionGraphPipelineNodeIndexAMDXPtr = pVkGetExecutionGraphPipelineNodeIndexAMDX (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetExecutionGraphPipelineNodeIndexAMDXPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetExecutionGraphPipelineNodeIndexAMDX is null" Nothing Nothing
  let vkGetExecutionGraphPipelineNodeIndexAMDX' = mkVkGetExecutionGraphPipelineNodeIndexAMDX vkGetExecutionGraphPipelineNodeIndexAMDXPtr
  pNodeInfo <- ContT $ withCStruct (nodeInfo)
  pPNodeIndex <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkGetExecutionGraphPipelineNodeIndexAMDX" (vkGetExecutionGraphPipelineNodeIndexAMDX'
                                                                             (deviceHandle (device))
                                                                             (executionGraph)
                                                                             pNodeInfo
                                                                             (pPNodeIndex))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pNodeIndex <- lift $ peek @Word32 pPNodeIndex
  pure $ (pNodeIndex)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateExecutionGraphPipelinesAMDX
  :: FunPtr (Ptr Device_T -> PipelineCache -> Word32 -> Ptr (SomeStruct ExecutionGraphPipelineCreateInfoAMDX) -> Ptr AllocationCallbacks -> Ptr Pipeline -> IO Result) -> Ptr Device_T -> PipelineCache -> Word32 -> Ptr (SomeStruct ExecutionGraphPipelineCreateInfoAMDX) -> Ptr AllocationCallbacks -> Ptr Pipeline -> IO Result

-- | vkCreateExecutionGraphPipelinesAMDX - Creates a new execution graph
-- pipeline object
--
-- = Description
--
-- Pipelines are created and returned as described for
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#pipelines-multiple Multiple Pipeline Creation>.
--
-- == Valid Usage
--
-- -   #VUID-vkCreateExecutionGraphPipelinesAMDX-shaderEnqueue-09124# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-shaderEnqueue shaderEnqueue feature>
--     /must/ be enabled
--
-- -   #VUID-vkCreateExecutionGraphPipelinesAMDX-flags-09125# If the
--     @flags@ member of any element of @pCreateInfos@ contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and the @basePipelineIndex@ member of that same element is not
--     @-1@, @basePipelineIndex@ /must/ be less than the index into
--     @pCreateInfos@ that corresponds to that element
--
-- -   #VUID-vkCreateExecutionGraphPipelinesAMDX-flags-09126# If the
--     @flags@ member of any element of @pCreateInfos@ contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, the base pipeline /must/ have been created with the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT'
--     flag set
--
-- -   #VUID-vkCreateExecutionGraphPipelinesAMDX-pipelineCache-09127# If
--     @pipelineCache@ was created with
--     'Vulkan.Core10.Enums.PipelineCacheCreateFlagBits.PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT',
--     host access to @pipelineCache@ /must/ be
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#fundamentals-threadingbehavior externally synchronized>
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateExecutionGraphPipelinesAMDX-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateExecutionGraphPipelinesAMDX-pipelineCache-parameter#
--     If @pipelineCache@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @pipelineCache@ /must/ be a valid
--     'Vulkan.Core10.Handles.PipelineCache' handle
--
-- -   #VUID-vkCreateExecutionGraphPipelinesAMDX-pCreateInfos-parameter#
--     @pCreateInfos@ /must/ be a valid pointer to an array of
--     @createInfoCount@ valid 'ExecutionGraphPipelineCreateInfoAMDX'
--     structures
--
-- -   #VUID-vkCreateExecutionGraphPipelinesAMDX-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkCreateExecutionGraphPipelinesAMDX-pPipelines-parameter#
--     @pPipelines@ /must/ be a valid pointer to an array of
--     @createInfoCount@ 'Vulkan.Core10.Handles.Pipeline' handles
--
-- -   #VUID-vkCreateExecutionGraphPipelinesAMDX-createInfoCount-arraylength#
--     @createInfoCount@ /must/ be greater than @0@
--
-- -   #VUID-vkCreateExecutionGraphPipelinesAMDX-pipelineCache-parent# If
--     @pipelineCache@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Extensions.VK_EXT_pipeline_creation_cache_control.PIPELINE_COMPILE_REQUIRED_EXT'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMDX_shader_enqueue VK_AMDX_shader_enqueue>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'ExecutionGraphPipelineCreateInfoAMDX',
-- 'Vulkan.Core10.Handles.Pipeline', 'Vulkan.Core10.Handles.PipelineCache'
createExecutionGraphPipelinesAMDX :: forall io
                                   . (MonadIO io)
                                  => -- | @device@ is the logical device that creates the execution graph
                                     -- pipelines.
                                     Device
                                  -> -- | @pipelineCache@ is either 'Vulkan.Core10.APIConstants.NULL_HANDLE',
                                     -- indicating that pipeline caching is disabled; or the handle of a valid
                                     -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#pipelines-cache pipeline cache>
                                     -- object, in which case use of that cache is enabled for the duration of
                                     -- the command.
                                     PipelineCache
                                  -> -- | @pCreateInfos@ is a pointer to an array of
                                     -- 'ExecutionGraphPipelineCreateInfoAMDX' structures.
                                     ("createInfos" ::: Vector (SomeStruct ExecutionGraphPipelineCreateInfoAMDX))
                                  -> -- | @pAllocator@ controls host memory allocation as described in the
                                     -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                                     -- chapter.
                                     ("allocator" ::: Maybe AllocationCallbacks)
                                  -> io (Result, ("pipelines" ::: Vector Pipeline))
createExecutionGraphPipelinesAMDX device
                                    pipelineCache
                                    createInfos
                                    allocator = liftIO . evalContT $ do
  let vkCreateExecutionGraphPipelinesAMDXPtr = pVkCreateExecutionGraphPipelinesAMDX (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCreateExecutionGraphPipelinesAMDXPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateExecutionGraphPipelinesAMDX is null" Nothing Nothing
  let vkCreateExecutionGraphPipelinesAMDX' = mkVkCreateExecutionGraphPipelinesAMDX vkCreateExecutionGraphPipelinesAMDXPtr
  pPCreateInfos <- ContT $ allocaBytes @(ExecutionGraphPipelineCreateInfoAMDX _) ((Data.Vector.length (createInfos)) * 64)
  Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPCreateInfos `plusPtr` (64 * (i)) :: Ptr (ExecutionGraphPipelineCreateInfoAMDX _))) (e) . ($ ())) (createInfos)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPPipelines <- ContT $ bracket (callocBytes @Pipeline ((fromIntegral ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32))) * 8)) free
  r <- lift $ traceAroundEvent "vkCreateExecutionGraphPipelinesAMDX" (vkCreateExecutionGraphPipelinesAMDX'
                                                                        (deviceHandle (device))
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
  "dynamic" mkVkCmdInitializeGraphScratchMemoryAMDX
  :: FunPtr (Ptr CommandBuffer_T -> DeviceAddress -> IO ()) -> Ptr CommandBuffer_T -> DeviceAddress -> IO ()

-- | vkCmdInitializeGraphScratchMemoryAMDX - Initialize scratch memory for an
-- execution graph
--
-- = Description
--
-- This command /must/ be called before using @scratch@ to dispatch the
-- currently bound execution graph pipeline.
--
-- Execution of this command /may/ modify any memory locations in the range
-- [@scratch@,@scratch@ + @size@), where @size@ is the value returned in
-- 'ExecutionGraphPipelineScratchSizeAMDX'::@size@ by
-- 'ExecutionGraphPipelineScratchSizeAMDX' for the currently bound
-- execution graph pipeline. Accesses to this memory range are performed in
-- the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COMPUTE_SHADER_BIT'
-- pipeline stage with the
-- 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_STORAGE_READ_BIT' and
-- 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_STORAGE_WRITE_BIT'
-- access flags.
--
-- If any portion of @scratch@ is modified by any command other than
-- 'cmdDispatchGraphAMDX', 'cmdDispatchGraphIndirectAMDX',
-- 'cmdDispatchGraphIndirectCountAMDX', or
-- 'cmdInitializeGraphScratchMemoryAMDX' with the same execution graph, it
-- /must/ be reinitialized for the execution graph again before dispatching
-- against it.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdInitializeGraphScratchMemoryAMDX-scratch-09143# @scratch@
--     /must/ be the device address of an allocated memory range at least
--     as large as the value of
--     'ExecutionGraphPipelineScratchSizeAMDX'::@size@ returned by
--     'ExecutionGraphPipelineScratchSizeAMDX' for the currently bound
--     execution graph pipeline.
--
-- -   #VUID-vkCmdInitializeGraphScratchMemoryAMDX-scratch-09144# @scratch@
--     /must/ be a multiple of 64
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdInitializeGraphScratchMemoryAMDX-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdInitializeGraphScratchMemoryAMDX-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdInitializeGraphScratchMemoryAMDX-commandBuffer-cmdpool#
--     The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   #VUID-vkCmdInitializeGraphScratchMemoryAMDX-renderpass# This command
--     /must/ only be called outside of a render pass instance
--
-- -   #VUID-vkCmdInitializeGraphScratchMemoryAMDX-videocoding# This
--     command /must/ only be called outside of a video coding scope
--
-- -   #VUID-vkCmdInitializeGraphScratchMemoryAMDX-bufferlevel#
--     @commandBuffer@ /must/ be a primary
--     'Vulkan.Core10.Handles.CommandBuffer'
--
-- == Host Synchronization
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | Graphics                                                                                                              | Action                                                                                                                                 |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMDX_shader_enqueue VK_AMDX_shader_enqueue>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress'
cmdInitializeGraphScratchMemoryAMDX :: forall io
                                     . (MonadIO io)
                                    => -- | @commandBuffer@ is the command buffer into which the command will be
                                       -- recorded.
                                       CommandBuffer
                                    -> -- | @scratch@ is a pointer to the scratch memory to be initialized.
                                       ("scratch" ::: DeviceAddress)
                                    -> io ()
cmdInitializeGraphScratchMemoryAMDX commandBuffer scratch = liftIO $ do
  let vkCmdInitializeGraphScratchMemoryAMDXPtr = pVkCmdInitializeGraphScratchMemoryAMDX (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdInitializeGraphScratchMemoryAMDXPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdInitializeGraphScratchMemoryAMDX is null" Nothing Nothing
  let vkCmdInitializeGraphScratchMemoryAMDX' = mkVkCmdInitializeGraphScratchMemoryAMDX vkCmdInitializeGraphScratchMemoryAMDXPtr
  traceAroundEvent "vkCmdInitializeGraphScratchMemoryAMDX" (vkCmdInitializeGraphScratchMemoryAMDX'
                                                              (commandBufferHandle (commandBuffer))
                                                              (scratch))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDispatchGraphAMDX
  :: FunPtr (Ptr CommandBuffer_T -> DeviceAddress -> Ptr DispatchGraphCountInfoAMDX -> IO ()) -> Ptr CommandBuffer_T -> DeviceAddress -> Ptr DispatchGraphCountInfoAMDX -> IO ()

-- | vkCmdDispatchGraphAMDX - Dispatch an execution graph
--
-- = Description
--
-- When this command is executed, the nodes specified in @pCountInfo@ are
-- executed. Nodes executed as part of this command are not implicitly
-- synchronized in any way against each other once they are dispatched.
--
-- For this command, all device\/host pointers in substructures are treated
-- as host pointers and read only during host execution of this command.
-- Once this command returns, no reference to the original pointers is
-- retained.
--
-- Execution of this command /may/ modify any memory locations in the range
-- [@scratch@,@scratch@ + @size@), where @size@ is the value returned in
-- 'ExecutionGraphPipelineScratchSizeAMDX'::@size@ by
-- 'ExecutionGraphPipelineScratchSizeAMDX' for the currently bound
-- execution graph pipeline Accesses to this memory range are performed in
-- the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COMPUTE_SHADER_BIT'
-- pipeline stage with the
-- 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_STORAGE_READ_BIT' and
-- 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_STORAGE_WRITE_BIT'
-- access flags.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdDispatchGraphAMDX-magFilter-04553# If a
--     'Vulkan.Core10.Handles.Sampler' created with @magFilter@ or
--     @minFilter@ equal to 'Vulkan.Core10.Enums.Filter.FILTER_LINEAR' and
--     @compareEnable@ equal to 'Vulkan.Core10.FundamentalTypes.FALSE' is
--     used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-vkCmdDispatchGraphAMDX-mipmapMode-04770# If a
--     'Vulkan.Core10.Handles.Sampler' created with @mipmapMode@ equal to
--     'Vulkan.Core10.Enums.SamplerMipmapMode.SAMPLER_MIPMAP_MODE_LINEAR'
--     and @compareEnable@ equal to 'Vulkan.Core10.FundamentalTypes.FALSE'
--     is used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-vkCmdDispatchGraphAMDX-None-06479# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-depth-compare-operation depth comparison>,
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_SAMPLED_IMAGE_DEPTH_COMPARISON_BIT'
--
-- -   #VUID-vkCmdDispatchGraphAMDX-None-02691# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using atomic
--     operations as a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   #VUID-vkCmdDispatchGraphAMDX-None-07888# If a
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
--     descriptor is accessed using atomic operations as a result of this
--     command, then the storage texel buffer’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-buffer-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT'
--
-- -   #VUID-vkCmdDispatchGraphAMDX-None-02692# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a result of this
--     command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   #VUID-vkCmdDispatchGraphAMDX-None-02693# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_filter_cubic VK_EXT_filter_cubic>
--     extension is not enabled and any 'Vulkan.Core10.Handles.ImageView'
--     is sampled with 'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a
--     result of this command, it /must/ not have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' of
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_3D',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE', or
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY'
--
-- -   #VUID-vkCmdDispatchGraphAMDX-filterCubic-02694# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a result of this
--     command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering, as specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdDispatchGraphAMDX-filterCubicMinmax-02695# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' with a reduction mode
--     of either
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
-- -   #VUID-vkCmdDispatchGraphAMDX-cubicRangeClamp-09212# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-filter-cubic-range-clamp cubicRangeClamp>
--     feature is not enabled, then any 'Vulkan.Core10.Handles.ImageView'
--     being sampled with 'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as
--     a result of this command /must/ not have a
--     'Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax.SamplerReductionModeCreateInfo'::@reductionMode@
--     equal to
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_RANGECLAMP_QCOM'
--
-- -   #VUID-vkCmdDispatchGraphAMDX-reductionMode-09213# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with a
--     'Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax.SamplerReductionModeCreateInfo'::@reductionMode@
--     equal to
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_RANGECLAMP_QCOM'
--     as a result of this command /must/ sample with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT'
--
-- -   #VUID-vkCmdDispatchGraphAMDX-selectableCubicWeights-09214# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-filter-cubic-weight-selection selectableCubicWeights>
--     feature is not enabled, then any 'Vulkan.Core10.Handles.ImageView'
--     being sampled with 'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as
--     a result of this command /must/ have
--     'Vulkan.Extensions.VK_QCOM_filter_cubic_weights.SamplerCubicWeightsCreateInfoQCOM'::@cubicWeights@
--     equal to
--     'Vulkan.Extensions.VK_QCOM_filter_cubic_weights.CUBIC_FILTER_WEIGHTS_CATMULL_ROM_QCOM'
--
-- -   #VUID-vkCmdDispatchGraphAMDX-flags-02696# Any
--     'Vulkan.Core10.Handles.Image' created with a
--     'Vulkan.Core10.Image.ImageCreateInfo'::@flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode' of
--     'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'
--
-- -   #VUID-vkCmdDispatchGraphAMDX-OpTypeImage-07027# For any
--     'Vulkan.Core10.Handles.ImageView' being written as a storage image
--     where the image format field of the @OpTypeImage@ is @Unknown@, the
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDispatchGraphAMDX-OpTypeImage-07028# For any
--     'Vulkan.Core10.Handles.ImageView' being read as a storage image
--     where the image format field of the @OpTypeImage@ is @Unknown@, the
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDispatchGraphAMDX-OpTypeImage-07029# For any
--     'Vulkan.Core10.Handles.BufferView' being written as a storage texel
--     buffer where the image format field of the @OpTypeImage@ is
--     @Unknown@, the view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkFormatProperties3 buffer features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDispatchGraphAMDX-OpTypeImage-07030# Any
--     'Vulkan.Core10.Handles.BufferView' being read as a storage texel
--     buffer where the image format field of the @OpTypeImage@ is
--     @Unknown@ then the view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkFormatProperties3 buffer features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDispatchGraphAMDX-None-08600# For each set /n/ that is
--     statically used by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>,
--     a descriptor set /must/ have been bound to /n/ at the same pipeline
--     bind point, with a 'Vulkan.Core10.Handles.PipelineLayout' that is
--     compatible for set /n/, with the
--     'Vulkan.Core10.Handles.PipelineLayout' used to create the current
--     'Vulkan.Core10.Handles.Pipeline' or the
--     'Vulkan.Core10.Handles.DescriptorSetLayout' array used to create the
--     current 'Vulkan.Extensions.Handles.ShaderEXT' , as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdDispatchGraphAMDX-None-08601# For each push constant that
--     is statically used by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>,
--     a push constant value /must/ have been set for the same pipeline
--     bind point, with a 'Vulkan.Core10.Handles.PipelineLayout' that is
--     compatible for push constants, with the
--     'Vulkan.Core10.Handles.PipelineLayout' used to create the current
--     'Vulkan.Core10.Handles.Pipeline' or the
--     'Vulkan.Core10.Handles.DescriptorSetLayout' array used to create the
--     current 'Vulkan.Extensions.Handles.ShaderEXT' , as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdDispatchGraphAMDX-maintenance4-08602# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-maintenance4 maintenance4>
--     feature is not enabled, then for each push constant that is
--     statically used by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>,
--     a push constant value /must/ have been set for the same pipeline
--     bind point, with a 'Vulkan.Core10.Handles.PipelineLayout' that is
--     compatible for push constants, with the
--     'Vulkan.Core10.Handles.PipelineLayout' used to create the current
--     'Vulkan.Core10.Handles.Pipeline' or the
--     'Vulkan.Core10.Handles.DescriptorSetLayout' and
--     'Vulkan.Core10.PipelineLayout.PushConstantRange' arrays used to
--     create the current 'Vulkan.Extensions.Handles.ShaderEXT' , as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdDispatchGraphAMDX-None-08114# Descriptors in each bound
--     descriptor set, specified via
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets', /must/
--     be valid as described by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptor-validity descriptor validity>
--     if they are statically used by the 'Vulkan.Core10.Handles.Pipeline'
--     bound to the pipeline bind point used by this command and the bound
--     'Vulkan.Core10.Handles.Pipeline' was not created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdDispatchGraphAMDX-None-08115# If the descriptors used by
--     the 'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind
--     point were specified via
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets', the
--     bound 'Vulkan.Core10.Handles.Pipeline' /must/ have been created
--     without
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdDispatchGraphAMDX-None-08116# Descriptors in bound
--     descriptor buffers, specified via
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.cmdSetDescriptorBufferOffsetsEXT',
--     /must/ be valid if they are dynamically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command and the bound 'Vulkan.Core10.Handles.Pipeline'
--     was created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdDispatchGraphAMDX-None-08604# Descriptors in bound
--     descriptor buffers, specified via
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.cmdSetDescriptorBufferOffsetsEXT',
--     /must/ be valid if they are dynamically used by any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command
--
-- -   #VUID-vkCmdDispatchGraphAMDX-None-08117# If the descriptors used by
--     the 'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind
--     point were specified via
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.cmdSetDescriptorBufferOffsetsEXT',
--     the bound 'Vulkan.Core10.Handles.Pipeline' /must/ have been created
--     with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdDispatchGraphAMDX-None-08119# If a descriptor is
--     dynamically used with a 'Vulkan.Core10.Handles.Pipeline' created
--     with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT',
--     the descriptor memory /must/ be resident
--
-- -   #VUID-vkCmdDispatchGraphAMDX-None-08605# If a descriptor is
--     dynamically used with a 'Vulkan.Extensions.Handles.ShaderEXT'
--     created with a 'Vulkan.Core10.Handles.DescriptorSetLayout' that was
--     created with
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_DESCRIPTOR_BUFFER_BIT_EXT',
--     the descriptor memory /must/ be resident
--
-- -   #VUID-vkCmdDispatchGraphAMDX-None-08606# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--     feature is not enabled, a valid pipeline /must/ be bound to the
--     pipeline bind point used by this command
--
-- -   #VUID-vkCmdDispatchGraphAMDX-None-08608# If a pipeline is bound to
--     the pipeline bind point used by this command, there /must/ not have
--     been any calls to dynamic state setting commands for any state not
--     specified as dynamic in the 'Vulkan.Core10.Handles.Pipeline' object
--     bound to the pipeline bind point used by this command, since that
--     pipeline was bound
--
-- -   #VUID-vkCmdDispatchGraphAMDX-None-08609# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command or any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command accesses a
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
-- -   #VUID-vkCmdDispatchGraphAMDX-None-08610# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command or any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name, in any shader stage
--
-- -   #VUID-vkCmdDispatchGraphAMDX-None-08611# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command or any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values, in any shader stage
--
-- -   #VUID-vkCmdDispatchGraphAMDX-None-08607# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--     is enabled, either a valid pipeline /must/ be bound to the pipeline
--     bind point used by this command, or a valid combination of valid and
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' shader objects /must/ be
--     bound to every supported shader stage corresponding to the pipeline
--     bind point used by this command
--
-- -   #VUID-vkCmdDispatchGraphAMDX-uniformBuffers-06935# If any stage of
--     the 'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline
--     bind point used by this command accesses a uniform buffer, and that
--     stage was created without enabling either
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2_EXT'
--     for @uniformBuffers@, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, that stage /must/ not access values outside
--     of the range of the buffer as specified in the descriptor set bound
--     to the same pipeline bind point
--
-- -   #VUID-vkCmdDispatchGraphAMDX-None-08612# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, and any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command accesses a uniform
--     buffer, it /must/ not access values outside of the range of the
--     buffer as specified in the descriptor set bound to the same pipeline
--     bind point
--
-- -   #VUID-vkCmdDispatchGraphAMDX-storageBuffers-06936# If any stage of
--     the 'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline
--     bind point used by this command accesses a storage buffer, and that
--     stage was created without enabling either
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2_EXT'
--     for @storageBuffers@, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, that stage /must/ not access values outside
--     of the range of the buffer as specified in the descriptor set bound
--     to the same pipeline bind point
--
-- -   #VUID-vkCmdDispatchGraphAMDX-None-08613# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, and any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command accesses a storage
--     buffer, it /must/ not access values outside of the range of the
--     buffer as specified in the descriptor set bound to the same pipeline
--     bind point
--
-- -   #VUID-vkCmdDispatchGraphAMDX-commandBuffer-02707# If @commandBuffer@
--     is an unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, any resource accessed by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding bound shaders>
--     /must/ not be a protected resource
--
-- -   #VUID-vkCmdDispatchGraphAMDX-None-06550# If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>
--     accesses a 'Vulkan.Core10.Handles.Sampler' or
--     'Vulkan.Core10.Handles.ImageView' object that enables
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>,
--     that object /must/ only be used with @OpImageSample*@ or
--     @OpImageSparseSample*@ instructions
--
-- -   #VUID-vkCmdDispatchGraphAMDX-ConstOffset-06551# If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>
--     accesses a 'Vulkan.Core10.Handles.Sampler' or
--     'Vulkan.Core10.Handles.ImageView' object that enables
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>,
--     that object /must/ not use the @ConstOffset@ and @Offset@ operands
--
-- -   #VUID-vkCmdDispatchGraphAMDX-viewType-07752# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed as a result of this
--     command, then the image view’s @viewType@ /must/ match the @Dim@
--     operand of the @OpTypeImage@ as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-operation-validation ???>
--
-- -   #VUID-vkCmdDispatchGraphAMDX-format-07753# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed as a result of this
--     command, then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-numericformat numeric type>
--     of the image view’s @format@ and the @Sampled@ @Type@ operand of the
--     @OpTypeImage@ /must/ match
--
-- -   #VUID-vkCmdDispatchGraphAMDX-OpImageWrite-08795# If a
--     'Vulkan.Core10.Handles.ImageView' created with a format other than
--     'Vulkan.Core10.Enums.Format.FORMAT_A8_UNORM_KHR' is accessed using
--     @OpImageWrite@ as a result of this command, then the @Type@ of the
--     @Texel@ operand of that instruction /must/ have at least as many
--     components as the image view’s format
--
-- -   #VUID-vkCmdDispatchGraphAMDX-OpImageWrite-08796# If a
--     'Vulkan.Core10.Handles.ImageView' created with the format
--     'Vulkan.Core10.Enums.Format.FORMAT_A8_UNORM_KHR' is accessed using
--     @OpImageWrite@ as a result of this command, then the @Type@ of the
--     @Texel@ operand of that instruction /must/ have four components
--
-- -   #VUID-vkCmdDispatchGraphAMDX-OpImageWrite-04469# If a
--     'Vulkan.Core10.Handles.BufferView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     buffer view’s format
--
-- -   #VUID-vkCmdDispatchGraphAMDX-SampledType-04470# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit component
--     width is accessed as a result of this command, the @SampledType@ of
--     the @OpTypeImage@ operand of that instruction /must/ have a @Width@
--     of 64
--
-- -   #VUID-vkCmdDispatchGraphAMDX-SampledType-04471# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a component width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32
--
-- -   #VUID-vkCmdDispatchGraphAMDX-SampledType-04472# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit component
--     width is accessed as a result of this command, the @SampledType@ of
--     the @OpTypeImage@ operand of that instruction /must/ have a @Width@
--     of 64
--
-- -   #VUID-vkCmdDispatchGraphAMDX-SampledType-04473# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a component width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32
--
-- -   #VUID-vkCmdDispatchGraphAMDX-sparseImageInt64Atomics-04474# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Image' objects
--     created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command
--
-- -   #VUID-vkCmdDispatchGraphAMDX-sparseImageInt64Atomics-04475# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Buffer' objects
--     created with the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command
--
-- -   #VUID-vkCmdDispatchGraphAMDX-OpImageWeightedSampleQCOM-06971# If
--     @OpImageWeightedSampleQCOM@ is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_WEIGHT_SAMPLED_IMAGE_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchGraphAMDX-OpImageWeightedSampleQCOM-06972# If
--     @OpImageWeightedSampleQCOM@ uses a 'Vulkan.Core10.Handles.ImageView'
--     as a sample weight image as a result of this command, then the image
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_WEIGHT_IMAGE_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchGraphAMDX-OpImageBoxFilterQCOM-06973# If
--     @OpImageBoxFilterQCOM@ is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BOX_FILTER_SAMPLED_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchGraphAMDX-OpImageBlockMatchSSDQCOM-06974# If
--     @OpImageBlockMatchSSDQCOM@ is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchGraphAMDX-OpImageBlockMatchSADQCOM-06975# If
--     @OpImageBlockMatchSADQCOM@ is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchGraphAMDX-OpImageBlockMatchSADQCOM-06976# If
--     @OpImageBlockMatchSADQCOM@ or OpImageBlockMatchSSDQCOM is used to
--     read from a reference image as result of this command, then the
--     specified reference coordinates /must/ not fail
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-integer-coordinate-validation integer texel coordinate validation>
--
-- -   #VUID-vkCmdDispatchGraphAMDX-OpImageWeightedSampleQCOM-06977# If
--     @OpImageWeightedSampleQCOM@, @OpImageBoxFilterQCOM@,
--     @OpImageBlockMatchWindowSSDQCOM@, @OpImageBlockMatchWindowSADQCOM@,
--     @OpImageBlockMatchGatherSSDQCOM@, @OpImageBlockMatchGatherSADQCOM@,
--     @OpImageBlockMatchSSDQCOM@, or @OpImageBlockMatchSADQCOM@ uses a
--     'Vulkan.Core10.Handles.Sampler' as a result of this command, then
--     the sampler /must/ have been created with
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_IMAGE_PROCESSING_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchGraphAMDX-OpImageWeightedSampleQCOM-06978# If any
--     command other than @OpImageWeightedSampleQCOM@,
--     @OpImageBoxFilterQCOM@, @OpImageBlockMatchWindowSSDQCOM@,
--     @OpImageBlockMatchWindowSADQCOM@, @OpImageBlockMatchGatherSSDQCOM@,
--     @OpImageBlockMatchGatherSADQCOM@, @OpImageBlockMatchSSDQCOM@, or
--     @OpImageBlockMatchSADQCOM@ uses a 'Vulkan.Core10.Handles.Sampler' as
--     a result of this command, then the sampler /must/ not have been
--     created with
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_IMAGE_PROCESSING_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchGraphAMDX-OpImageBlockMatchWindow-09215# If a
--     @OpImageBlockMatchWindow*QCOM@ or @OpImageBlockMatchGather*QCOM@
--     instruction is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchGraphAMDX-OpImageBlockMatchWindow-09216# If a
--     @OpImageBlockMatchWindow*QCOM@ or @OpImageBlockMatchGather*QCOM@
--     instruction is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s format /must/ be a single-component format.
--
-- -   #VUID-vkCmdDispatchGraphAMDX-OpImageBlockMatchWindow-09217# If a
--     @OpImageBlockMatchWindow*QCOM@ or @OpImageBlockMatchGather*QCOM@
--     read from a reference image as result of this command, then the
--     specified reference coordinates /must/ not fail
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-integer-coordinate-validation integer texel coordinate validation>
--
-- -   #VUID-vkCmdDispatchGraphAMDX-None-07288# Any shader invocation
--     executed by this command /must/
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-termination terminate>
--
-- -   #VUID-vkCmdDispatchGraphAMDX-commandBuffer-09181# @commandBuffer@
--     /must/ not be a protected command buffer
--
-- -   #VUID-vkCmdDispatchGraphAMDX-commandBuffer-09182# @commandBuffer@
--     /must/ be a primary command buffer
--
-- -   #VUID-vkCmdDispatchGraphAMDX-scratch-09183# @scratch@ /must/ be the
--     device address of an allocated memory range at least as large as the
--     value of 'ExecutionGraphPipelineScratchSizeAMDX'::@size@ returned by
--     'ExecutionGraphPipelineScratchSizeAMDX' for the currently bound
--     execution graph pipeline
--
-- -   #VUID-vkCmdDispatchGraphAMDX-scratch-09184# @scratch@ /must/ be a
--     device address within a 'Vulkan.Core10.Handles.Buffer' created with
--     the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_EXECUTION_GRAPH_SCRATCH_BIT_AMDX'
--     or 'BUFFER_USAGE_2_EXECUTION_GRAPH_SCRATCH_BIT_AMDX' flag
--
-- -   #VUID-vkCmdDispatchGraphAMDX-scratch-09185# Device memory in the
--     range [@scratch@,@scratch@
--     'ExecutionGraphPipelineScratchSizeAMDX'::@size@) /must/ have been
--     initialized with 'cmdInitializeGraphScratchMemoryAMDX' using the
--     currently bound execution graph pipeline, and not modified after
--     that by anything other than another execution graph dispatch command
--
-- -   #VUID-vkCmdDispatchGraphAMDX-maxComputeWorkGroupCount-09186#
--     Execution of this command /must/ not cause a node to be dispatched
--     with a larger number of workgroups than that specified by either a
--     @MaxNumWorkgroupsAMDX@ decoration in the dispatched node or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxComputeWorkGroupCount maxComputeWorkGroupCount>
--
-- -   #VUID-vkCmdDispatchGraphAMDX-maxExecutionGraphShaderPayloadCount-09187#
--     Execution of this command /must/ not cause any shader to initialize
--     more than
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxExecutionGraphShaderPayloadCount maxExecutionGraphShaderPayloadCount>
--     output payloads
--
-- -   #VUID-vkCmdDispatchGraphAMDX-NodeMaxPayloadsAMDX-09188# Execution of
--     this command /must/ not cause any shader that declares
--     @NodeMaxPayloadsAMDX@ to initialize more output payloads than
--     specified by the max number of payloads for that decoration. This
--     requirement applies to each @NodeMaxPayloadsAMDX@ decoration
--     separately
--
-- -   #VUID-vkCmdDispatchGraphAMDX-pCountInfo-09145# @pCountInfo->infos@
--     /must/ be a host pointer to a memory allocation at least as large as
--     the product of @count@ and @stride@
--
-- -   #VUID-vkCmdDispatchGraphAMDX-infos-09146# Host memory locations at
--     indexes in the range [@infos@, @infos@ + (@count@*@stride@)), at a
--     granularity of @stride@ /must/ contain valid 'DispatchGraphInfoAMDX'
--     structures in the first 24 bytes
--
-- -   #VUID-vkCmdDispatchGraphAMDX-pCountInfo-09147# For each
--     'DispatchGraphInfoAMDX' structure in @pCountInfo->infos@, @payloads@
--     /must/ be a host pointer to a memory allocation at least as large as
--     the product of @payloadCount@ and @payloadStride@
--
-- -   #VUID-vkCmdDispatchGraphAMDX-pCountInfo-09148# For each
--     'DispatchGraphInfoAMDX' structure in @pCountInfo->infos@,
--     @nodeIndex@ /must/ be a valid node index in the currently bound
--     execution graph pipeline, as returned by
--     'getExecutionGraphPipelineNodeIndexAMDX'
--
-- -   #VUID-vkCmdDispatchGraphAMDX-pCountInfo-09149# For each
--     'DispatchGraphInfoAMDX' structure in @pCountInfo->infos@, host
--     memory locations at indexes in the range [@payloads@, @payloads@ +
--     (@payloadCount@ * @payloadStride@)), at a granularity of
--     @payloadStride@ /must/ contain a payload matching the size of the
--     input payload expected by the node in @nodeIndex@ in the first bytes
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdDispatchGraphAMDX-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdDispatchGraphAMDX-pCountInfo-parameter# @pCountInfo@
--     /must/ be a valid pointer to a valid 'DispatchGraphCountInfoAMDX'
--     structure
--
-- -   #VUID-vkCmdDispatchGraphAMDX-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdDispatchGraphAMDX-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   #VUID-vkCmdDispatchGraphAMDX-renderpass# This command /must/ only be
--     called outside of a render pass instance
--
-- -   #VUID-vkCmdDispatchGraphAMDX-videocoding# This command /must/ only
--     be called outside of a video coding scope
--
-- -   #VUID-vkCmdDispatchGraphAMDX-bufferlevel# @commandBuffer@ /must/ be
--     a primary 'Vulkan.Core10.Handles.CommandBuffer'
--
-- == Host Synchronization
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | Graphics                                                                                                              | Action                                                                                                                                 |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMDX_shader_enqueue VK_AMDX_shader_enqueue>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress',
-- 'DispatchGraphCountInfoAMDX'
cmdDispatchGraphAMDX :: forall io
                      . (MonadIO io)
                     => -- | @commandBuffer@ is the command buffer into which the command will be
                        -- recorded.
                        CommandBuffer
                     -> -- | @scratch@ is a pointer to the scratch memory to be used.
                        ("scratch" ::: DeviceAddress)
                     -> -- | @pCountInfo@ is a host pointer to a 'DispatchGraphCountInfoAMDX'
                        -- structure defining the nodes which will be initially executed.
                        DispatchGraphCountInfoAMDX
                     -> io ()
cmdDispatchGraphAMDX commandBuffer scratch countInfo = liftIO . evalContT $ do
  let vkCmdDispatchGraphAMDXPtr = pVkCmdDispatchGraphAMDX (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdDispatchGraphAMDXPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDispatchGraphAMDX is null" Nothing Nothing
  let vkCmdDispatchGraphAMDX' = mkVkCmdDispatchGraphAMDX vkCmdDispatchGraphAMDXPtr
  pCountInfo <- ContT $ withCStruct (countInfo)
  lift $ traceAroundEvent "vkCmdDispatchGraphAMDX" (vkCmdDispatchGraphAMDX'
                                                      (commandBufferHandle (commandBuffer))
                                                      (scratch)
                                                      pCountInfo)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDispatchGraphIndirectAMDX
  :: FunPtr (Ptr CommandBuffer_T -> DeviceAddress -> Ptr DispatchGraphCountInfoAMDX -> IO ()) -> Ptr CommandBuffer_T -> DeviceAddress -> Ptr DispatchGraphCountInfoAMDX -> IO ()

-- | vkCmdDispatchGraphIndirectAMDX - Dispatch an execution graph with node
-- and payload parameters read on the device
--
-- = Description
--
-- When this command is executed, the nodes specified in @pCountInfo@ are
-- executed. Nodes executed as part of this command are not implicitly
-- synchronized in any way against each other once they are dispatched.
--
-- For this command, all device\/host pointers in substructures are treated
-- as device pointers and read during device execution of this command. The
-- allocation and contents of these pointers only needs to be valid during
-- device execution. All of these addresses will be read in the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COMPUTE_SHADER_BIT'
-- pipeline stage with the
-- 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_STORAGE_READ_BIT'
-- access flag.
--
-- Execution of this command /may/ modify any memory locations in the range
-- [@scratch@,@scratch@ + @size@), where @size@ is the value returned in
-- 'ExecutionGraphPipelineScratchSizeAMDX'::@size@ by
-- 'ExecutionGraphPipelineScratchSizeAMDX' for the currently bound
-- execution graph pipeline. Accesses to this memory range are performed in
-- the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COMPUTE_SHADER_BIT'
-- pipeline stage with the
-- 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_STORAGE_READ_BIT' and
-- 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_STORAGE_WRITE_BIT'
-- access flags.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-magFilter-04553# If a
--     'Vulkan.Core10.Handles.Sampler' created with @magFilter@ or
--     @minFilter@ equal to 'Vulkan.Core10.Enums.Filter.FILTER_LINEAR' and
--     @compareEnable@ equal to 'Vulkan.Core10.FundamentalTypes.FALSE' is
--     used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-mipmapMode-04770# If a
--     'Vulkan.Core10.Handles.Sampler' created with @mipmapMode@ equal to
--     'Vulkan.Core10.Enums.SamplerMipmapMode.SAMPLER_MIPMAP_MODE_LINEAR'
--     and @compareEnable@ equal to 'Vulkan.Core10.FundamentalTypes.FALSE'
--     is used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-None-06479# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-depth-compare-operation depth comparison>,
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_SAMPLED_IMAGE_DEPTH_COMPARISON_BIT'
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-None-02691# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using atomic
--     operations as a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-None-07888# If a
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
--     descriptor is accessed using atomic operations as a result of this
--     command, then the storage texel buffer’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-buffer-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT'
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-None-02692# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a result of this
--     command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-None-02693# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_filter_cubic VK_EXT_filter_cubic>
--     extension is not enabled and any 'Vulkan.Core10.Handles.ImageView'
--     is sampled with 'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a
--     result of this command, it /must/ not have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' of
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_3D',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE', or
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY'
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-filterCubic-02694# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a result of this
--     command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering, as specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-filterCubicMinmax-02695# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' with a reduction mode
--     of either
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
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-cubicRangeClamp-09212# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-filter-cubic-range-clamp cubicRangeClamp>
--     feature is not enabled, then any 'Vulkan.Core10.Handles.ImageView'
--     being sampled with 'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as
--     a result of this command /must/ not have a
--     'Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax.SamplerReductionModeCreateInfo'::@reductionMode@
--     equal to
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_RANGECLAMP_QCOM'
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-reductionMode-09213# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with a
--     'Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax.SamplerReductionModeCreateInfo'::@reductionMode@
--     equal to
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_RANGECLAMP_QCOM'
--     as a result of this command /must/ sample with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT'
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-selectableCubicWeights-09214#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-filter-cubic-weight-selection selectableCubicWeights>
--     feature is not enabled, then any 'Vulkan.Core10.Handles.ImageView'
--     being sampled with 'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as
--     a result of this command /must/ have
--     'Vulkan.Extensions.VK_QCOM_filter_cubic_weights.SamplerCubicWeightsCreateInfoQCOM'::@cubicWeights@
--     equal to
--     'Vulkan.Extensions.VK_QCOM_filter_cubic_weights.CUBIC_FILTER_WEIGHTS_CATMULL_ROM_QCOM'
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-flags-02696# Any
--     'Vulkan.Core10.Handles.Image' created with a
--     'Vulkan.Core10.Image.ImageCreateInfo'::@flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode' of
--     'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-OpTypeImage-07027# For any
--     'Vulkan.Core10.Handles.ImageView' being written as a storage image
--     where the image format field of the @OpTypeImage@ is @Unknown@, the
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-OpTypeImage-07028# For any
--     'Vulkan.Core10.Handles.ImageView' being read as a storage image
--     where the image format field of the @OpTypeImage@ is @Unknown@, the
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-OpTypeImage-07029# For any
--     'Vulkan.Core10.Handles.BufferView' being written as a storage texel
--     buffer where the image format field of the @OpTypeImage@ is
--     @Unknown@, the view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkFormatProperties3 buffer features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-OpTypeImage-07030# Any
--     'Vulkan.Core10.Handles.BufferView' being read as a storage texel
--     buffer where the image format field of the @OpTypeImage@ is
--     @Unknown@ then the view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkFormatProperties3 buffer features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-None-08600# For each set /n/
--     that is statically used by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>,
--     a descriptor set /must/ have been bound to /n/ at the same pipeline
--     bind point, with a 'Vulkan.Core10.Handles.PipelineLayout' that is
--     compatible for set /n/, with the
--     'Vulkan.Core10.Handles.PipelineLayout' used to create the current
--     'Vulkan.Core10.Handles.Pipeline' or the
--     'Vulkan.Core10.Handles.DescriptorSetLayout' array used to create the
--     current 'Vulkan.Extensions.Handles.ShaderEXT' , as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-None-08601# For each push
--     constant that is statically used by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>,
--     a push constant value /must/ have been set for the same pipeline
--     bind point, with a 'Vulkan.Core10.Handles.PipelineLayout' that is
--     compatible for push constants, with the
--     'Vulkan.Core10.Handles.PipelineLayout' used to create the current
--     'Vulkan.Core10.Handles.Pipeline' or the
--     'Vulkan.Core10.Handles.DescriptorSetLayout' array used to create the
--     current 'Vulkan.Extensions.Handles.ShaderEXT' , as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-maintenance4-08602# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-maintenance4 maintenance4>
--     feature is not enabled, then for each push constant that is
--     statically used by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>,
--     a push constant value /must/ have been set for the same pipeline
--     bind point, with a 'Vulkan.Core10.Handles.PipelineLayout' that is
--     compatible for push constants, with the
--     'Vulkan.Core10.Handles.PipelineLayout' used to create the current
--     'Vulkan.Core10.Handles.Pipeline' or the
--     'Vulkan.Core10.Handles.DescriptorSetLayout' and
--     'Vulkan.Core10.PipelineLayout.PushConstantRange' arrays used to
--     create the current 'Vulkan.Extensions.Handles.ShaderEXT' , as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-None-08114# Descriptors in each
--     bound descriptor set, specified via
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets', /must/
--     be valid as described by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptor-validity descriptor validity>
--     if they are statically used by the 'Vulkan.Core10.Handles.Pipeline'
--     bound to the pipeline bind point used by this command and the bound
--     'Vulkan.Core10.Handles.Pipeline' was not created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-None-08115# If the descriptors
--     used by the 'Vulkan.Core10.Handles.Pipeline' bound to the pipeline
--     bind point were specified via
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets', the
--     bound 'Vulkan.Core10.Handles.Pipeline' /must/ have been created
--     without
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-None-08116# Descriptors in
--     bound descriptor buffers, specified via
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.cmdSetDescriptorBufferOffsetsEXT',
--     /must/ be valid if they are dynamically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command and the bound 'Vulkan.Core10.Handles.Pipeline'
--     was created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-None-08604# Descriptors in
--     bound descriptor buffers, specified via
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.cmdSetDescriptorBufferOffsetsEXT',
--     /must/ be valid if they are dynamically used by any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-None-08117# If the descriptors
--     used by the 'Vulkan.Core10.Handles.Pipeline' bound to the pipeline
--     bind point were specified via
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.cmdSetDescriptorBufferOffsetsEXT',
--     the bound 'Vulkan.Core10.Handles.Pipeline' /must/ have been created
--     with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-None-08119# If a descriptor is
--     dynamically used with a 'Vulkan.Core10.Handles.Pipeline' created
--     with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT',
--     the descriptor memory /must/ be resident
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-None-08605# If a descriptor is
--     dynamically used with a 'Vulkan.Extensions.Handles.ShaderEXT'
--     created with a 'Vulkan.Core10.Handles.DescriptorSetLayout' that was
--     created with
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_DESCRIPTOR_BUFFER_BIT_EXT',
--     the descriptor memory /must/ be resident
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-None-08606# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--     feature is not enabled, a valid pipeline /must/ be bound to the
--     pipeline bind point used by this command
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-None-08608# If a pipeline is
--     bound to the pipeline bind point used by this command, there /must/
--     not have been any calls to dynamic state setting commands for any
--     state not specified as dynamic in the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command, since that pipeline was bound
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-None-08609# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command or any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command accesses a
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
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-None-08610# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command or any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name, in any shader stage
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-None-08611# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command or any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values, in any shader stage
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-None-08607# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--     is enabled, either a valid pipeline /must/ be bound to the pipeline
--     bind point used by this command, or a valid combination of valid and
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' shader objects /must/ be
--     bound to every supported shader stage corresponding to the pipeline
--     bind point used by this command
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-uniformBuffers-06935# If any
--     stage of the 'Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a uniform buffer,
--     and that stage was created without enabling either
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2_EXT'
--     for @uniformBuffers@, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, that stage /must/ not access values outside
--     of the range of the buffer as specified in the descriptor set bound
--     to the same pipeline bind point
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-None-08612# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, and any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command accesses a uniform
--     buffer, it /must/ not access values outside of the range of the
--     buffer as specified in the descriptor set bound to the same pipeline
--     bind point
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-storageBuffers-06936# If any
--     stage of the 'Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a storage buffer,
--     and that stage was created without enabling either
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2_EXT'
--     for @storageBuffers@, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, that stage /must/ not access values outside
--     of the range of the buffer as specified in the descriptor set bound
--     to the same pipeline bind point
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-None-08613# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, and any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command accesses a storage
--     buffer, it /must/ not access values outside of the range of the
--     buffer as specified in the descriptor set bound to the same pipeline
--     bind point
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-commandBuffer-02707# If
--     @commandBuffer@ is an unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, any resource accessed by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding bound shaders>
--     /must/ not be a protected resource
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-None-06550# If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>
--     accesses a 'Vulkan.Core10.Handles.Sampler' or
--     'Vulkan.Core10.Handles.ImageView' object that enables
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>,
--     that object /must/ only be used with @OpImageSample*@ or
--     @OpImageSparseSample*@ instructions
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-ConstOffset-06551# If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>
--     accesses a 'Vulkan.Core10.Handles.Sampler' or
--     'Vulkan.Core10.Handles.ImageView' object that enables
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>,
--     that object /must/ not use the @ConstOffset@ and @Offset@ operands
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-viewType-07752# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed as a result of this
--     command, then the image view’s @viewType@ /must/ match the @Dim@
--     operand of the @OpTypeImage@ as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-operation-validation ???>
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-format-07753# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed as a result of this
--     command, then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-numericformat numeric type>
--     of the image view’s @format@ and the @Sampled@ @Type@ operand of the
--     @OpTypeImage@ /must/ match
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-OpImageWrite-08795# If a
--     'Vulkan.Core10.Handles.ImageView' created with a format other than
--     'Vulkan.Core10.Enums.Format.FORMAT_A8_UNORM_KHR' is accessed using
--     @OpImageWrite@ as a result of this command, then the @Type@ of the
--     @Texel@ operand of that instruction /must/ have at least as many
--     components as the image view’s format
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-OpImageWrite-08796# If a
--     'Vulkan.Core10.Handles.ImageView' created with the format
--     'Vulkan.Core10.Enums.Format.FORMAT_A8_UNORM_KHR' is accessed using
--     @OpImageWrite@ as a result of this command, then the @Type@ of the
--     @Texel@ operand of that instruction /must/ have four components
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-OpImageWrite-04469# If a
--     'Vulkan.Core10.Handles.BufferView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     buffer view’s format
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-SampledType-04470# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit component
--     width is accessed as a result of this command, the @SampledType@ of
--     the @OpTypeImage@ operand of that instruction /must/ have a @Width@
--     of 64
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-SampledType-04471# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a component width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-SampledType-04472# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit component
--     width is accessed as a result of this command, the @SampledType@ of
--     the @OpTypeImage@ operand of that instruction /must/ have a @Width@
--     of 64
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-SampledType-04473# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a component width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-sparseImageInt64Atomics-04474#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Image' objects
--     created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-sparseImageInt64Atomics-04475#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Buffer' objects
--     created with the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-OpImageWeightedSampleQCOM-06971#
--     If @OpImageWeightedSampleQCOM@ is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_WEIGHT_SAMPLED_IMAGE_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-OpImageWeightedSampleQCOM-06972#
--     If @OpImageWeightedSampleQCOM@ uses a
--     'Vulkan.Core10.Handles.ImageView' as a sample weight image as a
--     result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_WEIGHT_IMAGE_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-OpImageBoxFilterQCOM-06973# If
--     @OpImageBoxFilterQCOM@ is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BOX_FILTER_SAMPLED_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-OpImageBlockMatchSSDQCOM-06974#
--     If @OpImageBlockMatchSSDQCOM@ is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-OpImageBlockMatchSADQCOM-06975#
--     If @OpImageBlockMatchSADQCOM@ is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-OpImageBlockMatchSADQCOM-06976#
--     If @OpImageBlockMatchSADQCOM@ or OpImageBlockMatchSSDQCOM is used to
--     read from a reference image as result of this command, then the
--     specified reference coordinates /must/ not fail
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-integer-coordinate-validation integer texel coordinate validation>
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-OpImageWeightedSampleQCOM-06977#
--     If @OpImageWeightedSampleQCOM@, @OpImageBoxFilterQCOM@,
--     @OpImageBlockMatchWindowSSDQCOM@, @OpImageBlockMatchWindowSADQCOM@,
--     @OpImageBlockMatchGatherSSDQCOM@, @OpImageBlockMatchGatherSADQCOM@,
--     @OpImageBlockMatchSSDQCOM@, or @OpImageBlockMatchSADQCOM@ uses a
--     'Vulkan.Core10.Handles.Sampler' as a result of this command, then
--     the sampler /must/ have been created with
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_IMAGE_PROCESSING_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-OpImageWeightedSampleQCOM-06978#
--     If any command other than @OpImageWeightedSampleQCOM@,
--     @OpImageBoxFilterQCOM@, @OpImageBlockMatchWindowSSDQCOM@,
--     @OpImageBlockMatchWindowSADQCOM@, @OpImageBlockMatchGatherSSDQCOM@,
--     @OpImageBlockMatchGatherSADQCOM@, @OpImageBlockMatchSSDQCOM@, or
--     @OpImageBlockMatchSADQCOM@ uses a 'Vulkan.Core10.Handles.Sampler' as
--     a result of this command, then the sampler /must/ not have been
--     created with
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_IMAGE_PROCESSING_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-OpImageBlockMatchWindow-09215#
--     If a @OpImageBlockMatchWindow*QCOM@ or
--     @OpImageBlockMatchGather*QCOM@ instruction is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-OpImageBlockMatchWindow-09216#
--     If a @OpImageBlockMatchWindow*QCOM@ or
--     @OpImageBlockMatchGather*QCOM@ instruction is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s format /must/ be a single-component format.
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-OpImageBlockMatchWindow-09217#
--     If a @OpImageBlockMatchWindow*QCOM@ or
--     @OpImageBlockMatchGather*QCOM@ read from a reference image as result
--     of this command, then the specified reference coordinates /must/ not
--     fail
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-integer-coordinate-validation integer texel coordinate validation>
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-None-07288# Any shader
--     invocation executed by this command /must/
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-termination terminate>
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-commandBuffer-09181#
--     @commandBuffer@ /must/ not be a protected command buffer
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-commandBuffer-09182#
--     @commandBuffer@ /must/ be a primary command buffer
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-scratch-09183# @scratch@ /must/
--     be the device address of an allocated memory range at least as large
--     as the value of 'ExecutionGraphPipelineScratchSizeAMDX'::@size@
--     returned by 'ExecutionGraphPipelineScratchSizeAMDX' for the
--     currently bound execution graph pipeline
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-scratch-09184# @scratch@ /must/
--     be a device address within a 'Vulkan.Core10.Handles.Buffer' created
--     with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_EXECUTION_GRAPH_SCRATCH_BIT_AMDX'
--     or 'BUFFER_USAGE_2_EXECUTION_GRAPH_SCRATCH_BIT_AMDX' flag
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-scratch-09185# Device memory in
--     the range [@scratch@,@scratch@
--     'ExecutionGraphPipelineScratchSizeAMDX'::@size@) /must/ have been
--     initialized with 'cmdInitializeGraphScratchMemoryAMDX' using the
--     currently bound execution graph pipeline, and not modified after
--     that by anything other than another execution graph dispatch command
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-maxComputeWorkGroupCount-09186#
--     Execution of this command /must/ not cause a node to be dispatched
--     with a larger number of workgroups than that specified by either a
--     @MaxNumWorkgroupsAMDX@ decoration in the dispatched node or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxComputeWorkGroupCount maxComputeWorkGroupCount>
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-maxExecutionGraphShaderPayloadCount-09187#
--     Execution of this command /must/ not cause any shader to initialize
--     more than
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxExecutionGraphShaderPayloadCount maxExecutionGraphShaderPayloadCount>
--     output payloads
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-NodeMaxPayloadsAMDX-09188#
--     Execution of this command /must/ not cause any shader that declares
--     @NodeMaxPayloadsAMDX@ to initialize more output payloads than
--     specified by the max number of payloads for that decoration. This
--     requirement applies to each @NodeMaxPayloadsAMDX@ decoration
--     separately
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-pCountInfo-09150#
--     @pCountInfo->infos@ /must/ be a device pointer to a memory
--     allocation at least as large as the product of @count@ and @stride@
--     when this command is executed on the device
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-pCountInfo-09151#
--     @pCountInfo->infos@ /must/ be a device address within a
--     'Vulkan.Core10.Handles.Buffer' created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     flag
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-pCountInfo-09152#
--     @pCountInfo->infos@ /must/ be a multiple of
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-executionGraphDispatchAddressAlignment executionGraphDispatchAddressAlignment>
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-infos-09153# Device memory
--     locations at indexes in the range [@infos@, @infos@ +
--     (@count@*@stride@)), at a granularity of @stride@ /must/ contain
--     valid 'DispatchGraphInfoAMDX' structures in the first 24 bytes when
--     this command is executed on the device
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-pCountInfo-09154# For each
--     'DispatchGraphInfoAMDX' structure in @pCountInfo->infos@, @payloads@
--     /must/ be a device pointer to a memory allocation at least as large
--     as the product of @payloadCount@ and @payloadStride@ when this
--     command is executed on the device
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-pCountInfo-09155# For each
--     'DispatchGraphInfoAMDX' structure in @pCountInfo->infos@, @payloads@
--     /must/ be a device address within a 'Vulkan.Core10.Handles.Buffer'
--     created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     flag
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-pCountInfo-09156# For each
--     'DispatchGraphInfoAMDX' structure in @pCountInfo->infos@, @payloads@
--     /must/ be a multiple of
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-executionGraphDispatchAddressAlignment executionGraphDispatchAddressAlignment>
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-pCountInfo-09157# For each
--     'DispatchGraphInfoAMDX' structure in @pCountInfo->infos@,
--     @nodeIndex@ /must/ be a valid node index in the currently bound
--     execution graph pipeline, as returned by
--     'getExecutionGraphPipelineNodeIndexAMDX' when this command is
--     executed on the device
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-pCountInfo-09158# For each
--     'DispatchGraphInfoAMDX' structure in @pCountInfo->infos@, device
--     memory locations at indexes in the range [@payloads@, @payloads@ +
--     (@payloadCount@ * @payloadStride@)), at a granularity of
--     @payloadStride@ /must/ contain a payload matching the size of the
--     input payload expected by the node in @nodeIndex@ in the first bytes
--     when this command is executed on the device
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-pCountInfo-parameter#
--     @pCountInfo@ /must/ be a valid pointer to a valid
--     'DispatchGraphCountInfoAMDX' structure
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-renderpass# This command /must/
--     only be called outside of a render pass instance
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-videocoding# This command
--     /must/ only be called outside of a video coding scope
--
-- -   #VUID-vkCmdDispatchGraphIndirectAMDX-bufferlevel# @commandBuffer@
--     /must/ be a primary 'Vulkan.Core10.Handles.CommandBuffer'
--
-- == Host Synchronization
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | Graphics                                                                                                              | Action                                                                                                                                 |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMDX_shader_enqueue VK_AMDX_shader_enqueue>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress',
-- 'DispatchGraphCountInfoAMDX'
cmdDispatchGraphIndirectAMDX :: forall io
                              . (MonadIO io)
                             => -- | @commandBuffer@ is the command buffer into which the command will be
                                -- recorded.
                                CommandBuffer
                             -> -- | @scratch@ is a pointer to the scratch memory to be used.
                                ("scratch" ::: DeviceAddress)
                             -> -- | @pCountInfo@ is a host pointer to a 'DispatchGraphCountInfoAMDX'
                                -- structure defining the nodes which will be initially executed.
                                DispatchGraphCountInfoAMDX
                             -> io ()
cmdDispatchGraphIndirectAMDX commandBuffer
                               scratch
                               countInfo = liftIO . evalContT $ do
  let vkCmdDispatchGraphIndirectAMDXPtr = pVkCmdDispatchGraphIndirectAMDX (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdDispatchGraphIndirectAMDXPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDispatchGraphIndirectAMDX is null" Nothing Nothing
  let vkCmdDispatchGraphIndirectAMDX' = mkVkCmdDispatchGraphIndirectAMDX vkCmdDispatchGraphIndirectAMDXPtr
  pCountInfo <- ContT $ withCStruct (countInfo)
  lift $ traceAroundEvent "vkCmdDispatchGraphIndirectAMDX" (vkCmdDispatchGraphIndirectAMDX'
                                                              (commandBufferHandle (commandBuffer))
                                                              (scratch)
                                                              pCountInfo)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDispatchGraphIndirectCountAMDX
  :: FunPtr (Ptr CommandBuffer_T -> DeviceAddress -> DeviceAddress -> IO ()) -> Ptr CommandBuffer_T -> DeviceAddress -> DeviceAddress -> IO ()

-- | vkCmdDispatchGraphIndirectCountAMDX - Dispatch an execution graph with
-- all parameters read on the device
--
-- = Description
--
-- When this command is executed, the nodes specified in @countInfo@ are
-- executed. Nodes executed as part of this command are not implicitly
-- synchronized in any way against each other once they are dispatched.
--
-- For this command, all pointers in substructures are treated as device
-- pointers and read during device execution of this command. The
-- allocation and contents of these pointers only needs to be valid during
-- device execution. All of these addresses will be read in the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COMPUTE_SHADER_BIT'
-- pipeline stage with the
-- 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_STORAGE_READ_BIT'
-- access flag.
--
-- Execution of this command /may/ modify any memory locations in the range
-- [@scratch@,@scratch@ + @size@), where @size@ is the value returned in
-- 'ExecutionGraphPipelineScratchSizeAMDX'::@size@ by
-- 'ExecutionGraphPipelineScratchSizeAMDX' for the currently bound
-- execution graph pipeline. Accesses to this memory range are performed in
-- the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COMPUTE_SHADER_BIT'
-- pipeline stage with the
-- 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_STORAGE_READ_BIT' and
-- 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_STORAGE_WRITE_BIT'
-- access flags.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-magFilter-04553# If a
--     'Vulkan.Core10.Handles.Sampler' created with @magFilter@ or
--     @minFilter@ equal to 'Vulkan.Core10.Enums.Filter.FILTER_LINEAR' and
--     @compareEnable@ equal to 'Vulkan.Core10.FundamentalTypes.FALSE' is
--     used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-mipmapMode-04770# If a
--     'Vulkan.Core10.Handles.Sampler' created with @mipmapMode@ equal to
--     'Vulkan.Core10.Enums.SamplerMipmapMode.SAMPLER_MIPMAP_MODE_LINEAR'
--     and @compareEnable@ equal to 'Vulkan.Core10.FundamentalTypes.FALSE'
--     is used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-None-06479# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-depth-compare-operation depth comparison>,
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_SAMPLED_IMAGE_DEPTH_COMPARISON_BIT'
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-None-02691# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using atomic
--     operations as a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-None-07888# If a
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
--     descriptor is accessed using atomic operations as a result of this
--     command, then the storage texel buffer’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-buffer-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT'
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-None-02692# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a result of this
--     command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-None-02693# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_filter_cubic VK_EXT_filter_cubic>
--     extension is not enabled and any 'Vulkan.Core10.Handles.ImageView'
--     is sampled with 'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a
--     result of this command, it /must/ not have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' of
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_3D',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE', or
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY'
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-filterCubic-02694# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a result of this
--     command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering, as specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-filterCubicMinmax-02695#
--     Any 'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' with a reduction mode
--     of either
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
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-cubicRangeClamp-09212# If
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-filter-cubic-range-clamp cubicRangeClamp>
--     feature is not enabled, then any 'Vulkan.Core10.Handles.ImageView'
--     being sampled with 'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as
--     a result of this command /must/ not have a
--     'Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax.SamplerReductionModeCreateInfo'::@reductionMode@
--     equal to
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_RANGECLAMP_QCOM'
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-reductionMode-09213# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with a
--     'Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax.SamplerReductionModeCreateInfo'::@reductionMode@
--     equal to
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_RANGECLAMP_QCOM'
--     as a result of this command /must/ sample with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT'
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-selectableCubicWeights-09214#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-filter-cubic-weight-selection selectableCubicWeights>
--     feature is not enabled, then any 'Vulkan.Core10.Handles.ImageView'
--     being sampled with 'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as
--     a result of this command /must/ have
--     'Vulkan.Extensions.VK_QCOM_filter_cubic_weights.SamplerCubicWeightsCreateInfoQCOM'::@cubicWeights@
--     equal to
--     'Vulkan.Extensions.VK_QCOM_filter_cubic_weights.CUBIC_FILTER_WEIGHTS_CATMULL_ROM_QCOM'
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-flags-02696# Any
--     'Vulkan.Core10.Handles.Image' created with a
--     'Vulkan.Core10.Image.ImageCreateInfo'::@flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode' of
--     'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-OpTypeImage-07027# For any
--     'Vulkan.Core10.Handles.ImageView' being written as a storage image
--     where the image format field of the @OpTypeImage@ is @Unknown@, the
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-OpTypeImage-07028# For any
--     'Vulkan.Core10.Handles.ImageView' being read as a storage image
--     where the image format field of the @OpTypeImage@ is @Unknown@, the
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-OpTypeImage-07029# For any
--     'Vulkan.Core10.Handles.BufferView' being written as a storage texel
--     buffer where the image format field of the @OpTypeImage@ is
--     @Unknown@, the view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkFormatProperties3 buffer features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-OpTypeImage-07030# Any
--     'Vulkan.Core10.Handles.BufferView' being read as a storage texel
--     buffer where the image format field of the @OpTypeImage@ is
--     @Unknown@ then the view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkFormatProperties3 buffer features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-None-08600# For each set
--     /n/ that is statically used by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>,
--     a descriptor set /must/ have been bound to /n/ at the same pipeline
--     bind point, with a 'Vulkan.Core10.Handles.PipelineLayout' that is
--     compatible for set /n/, with the
--     'Vulkan.Core10.Handles.PipelineLayout' used to create the current
--     'Vulkan.Core10.Handles.Pipeline' or the
--     'Vulkan.Core10.Handles.DescriptorSetLayout' array used to create the
--     current 'Vulkan.Extensions.Handles.ShaderEXT' , as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-None-08601# For each push
--     constant that is statically used by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>,
--     a push constant value /must/ have been set for the same pipeline
--     bind point, with a 'Vulkan.Core10.Handles.PipelineLayout' that is
--     compatible for push constants, with the
--     'Vulkan.Core10.Handles.PipelineLayout' used to create the current
--     'Vulkan.Core10.Handles.Pipeline' or the
--     'Vulkan.Core10.Handles.DescriptorSetLayout' array used to create the
--     current 'Vulkan.Extensions.Handles.ShaderEXT' , as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-maintenance4-08602# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-maintenance4 maintenance4>
--     feature is not enabled, then for each push constant that is
--     statically used by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>,
--     a push constant value /must/ have been set for the same pipeline
--     bind point, with a 'Vulkan.Core10.Handles.PipelineLayout' that is
--     compatible for push constants, with the
--     'Vulkan.Core10.Handles.PipelineLayout' used to create the current
--     'Vulkan.Core10.Handles.Pipeline' or the
--     'Vulkan.Core10.Handles.DescriptorSetLayout' and
--     'Vulkan.Core10.PipelineLayout.PushConstantRange' arrays used to
--     create the current 'Vulkan.Extensions.Handles.ShaderEXT' , as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-None-08114# Descriptors in
--     each bound descriptor set, specified via
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets', /must/
--     be valid as described by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptor-validity descriptor validity>
--     if they are statically used by the 'Vulkan.Core10.Handles.Pipeline'
--     bound to the pipeline bind point used by this command and the bound
--     'Vulkan.Core10.Handles.Pipeline' was not created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-None-08115# If the
--     descriptors used by the 'Vulkan.Core10.Handles.Pipeline' bound to
--     the pipeline bind point were specified via
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets', the
--     bound 'Vulkan.Core10.Handles.Pipeline' /must/ have been created
--     without
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-None-08116# Descriptors in
--     bound descriptor buffers, specified via
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.cmdSetDescriptorBufferOffsetsEXT',
--     /must/ be valid if they are dynamically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command and the bound 'Vulkan.Core10.Handles.Pipeline'
--     was created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-None-08604# Descriptors in
--     bound descriptor buffers, specified via
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.cmdSetDescriptorBufferOffsetsEXT',
--     /must/ be valid if they are dynamically used by any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-None-08117# If the
--     descriptors used by the 'Vulkan.Core10.Handles.Pipeline' bound to
--     the pipeline bind point were specified via
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.cmdSetDescriptorBufferOffsetsEXT',
--     the bound 'Vulkan.Core10.Handles.Pipeline' /must/ have been created
--     with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-None-08119# If a
--     descriptor is dynamically used with a
--     'Vulkan.Core10.Handles.Pipeline' created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT',
--     the descriptor memory /must/ be resident
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-None-08605# If a
--     descriptor is dynamically used with a
--     'Vulkan.Extensions.Handles.ShaderEXT' created with a
--     'Vulkan.Core10.Handles.DescriptorSetLayout' that was created with
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_DESCRIPTOR_BUFFER_BIT_EXT',
--     the descriptor memory /must/ be resident
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-None-08606# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--     feature is not enabled, a valid pipeline /must/ be bound to the
--     pipeline bind point used by this command
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-None-08608# If a pipeline
--     is bound to the pipeline bind point used by this command, there
--     /must/ not have been any calls to dynamic state setting commands for
--     any state not specified as dynamic in the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command, since that pipeline was bound
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-None-08609# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command or any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command accesses a
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
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-None-08610# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command or any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name, in any shader stage
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-None-08611# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command or any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values, in any shader stage
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-None-08607# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--     is enabled, either a valid pipeline /must/ be bound to the pipeline
--     bind point used by this command, or a valid combination of valid and
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' shader objects /must/ be
--     bound to every supported shader stage corresponding to the pipeline
--     bind point used by this command
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-uniformBuffers-06935# If
--     any stage of the 'Vulkan.Core10.Handles.Pipeline' object bound to
--     the pipeline bind point used by this command accesses a uniform
--     buffer, and that stage was created without enabling either
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2_EXT'
--     for @uniformBuffers@, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, that stage /must/ not access values outside
--     of the range of the buffer as specified in the descriptor set bound
--     to the same pipeline bind point
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-None-08612# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, and any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command accesses a uniform
--     buffer, it /must/ not access values outside of the range of the
--     buffer as specified in the descriptor set bound to the same pipeline
--     bind point
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-storageBuffers-06936# If
--     any stage of the 'Vulkan.Core10.Handles.Pipeline' object bound to
--     the pipeline bind point used by this command accesses a storage
--     buffer, and that stage was created without enabling either
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2_EXT'
--     for @storageBuffers@, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, that stage /must/ not access values outside
--     of the range of the buffer as specified in the descriptor set bound
--     to the same pipeline bind point
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-None-08613# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, and any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command accesses a storage
--     buffer, it /must/ not access values outside of the range of the
--     buffer as specified in the descriptor set bound to the same pipeline
--     bind point
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-commandBuffer-02707# If
--     @commandBuffer@ is an unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, any resource accessed by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding bound shaders>
--     /must/ not be a protected resource
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-None-06550# If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>
--     accesses a 'Vulkan.Core10.Handles.Sampler' or
--     'Vulkan.Core10.Handles.ImageView' object that enables
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>,
--     that object /must/ only be used with @OpImageSample*@ or
--     @OpImageSparseSample*@ instructions
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-ConstOffset-06551# If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>
--     accesses a 'Vulkan.Core10.Handles.Sampler' or
--     'Vulkan.Core10.Handles.ImageView' object that enables
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>,
--     that object /must/ not use the @ConstOffset@ and @Offset@ operands
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-viewType-07752# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed as a result of this
--     command, then the image view’s @viewType@ /must/ match the @Dim@
--     operand of the @OpTypeImage@ as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-operation-validation ???>
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-format-07753# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed as a result of this
--     command, then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-numericformat numeric type>
--     of the image view’s @format@ and the @Sampled@ @Type@ operand of the
--     @OpTypeImage@ /must/ match
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-OpImageWrite-08795# If a
--     'Vulkan.Core10.Handles.ImageView' created with a format other than
--     'Vulkan.Core10.Enums.Format.FORMAT_A8_UNORM_KHR' is accessed using
--     @OpImageWrite@ as a result of this command, then the @Type@ of the
--     @Texel@ operand of that instruction /must/ have at least as many
--     components as the image view’s format
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-OpImageWrite-08796# If a
--     'Vulkan.Core10.Handles.ImageView' created with the format
--     'Vulkan.Core10.Enums.Format.FORMAT_A8_UNORM_KHR' is accessed using
--     @OpImageWrite@ as a result of this command, then the @Type@ of the
--     @Texel@ operand of that instruction /must/ have four components
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-OpImageWrite-04469# If a
--     'Vulkan.Core10.Handles.BufferView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     buffer view’s format
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-SampledType-04470# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit component
--     width is accessed as a result of this command, the @SampledType@ of
--     the @OpTypeImage@ operand of that instruction /must/ have a @Width@
--     of 64
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-SampledType-04471# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a component width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-SampledType-04472# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit component
--     width is accessed as a result of this command, the @SampledType@ of
--     the @OpTypeImage@ operand of that instruction /must/ have a @Width@
--     of 64
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-SampledType-04473# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a component width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-sparseImageInt64Atomics-04474#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Image' objects
--     created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-sparseImageInt64Atomics-04475#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Buffer' objects
--     created with the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-OpImageWeightedSampleQCOM-06971#
--     If @OpImageWeightedSampleQCOM@ is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_WEIGHT_SAMPLED_IMAGE_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-OpImageWeightedSampleQCOM-06972#
--     If @OpImageWeightedSampleQCOM@ uses a
--     'Vulkan.Core10.Handles.ImageView' as a sample weight image as a
--     result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_WEIGHT_IMAGE_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-OpImageBoxFilterQCOM-06973#
--     If @OpImageBoxFilterQCOM@ is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BOX_FILTER_SAMPLED_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-OpImageBlockMatchSSDQCOM-06974#
--     If @OpImageBlockMatchSSDQCOM@ is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-OpImageBlockMatchSADQCOM-06975#
--     If @OpImageBlockMatchSADQCOM@ is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-OpImageBlockMatchSADQCOM-06976#
--     If @OpImageBlockMatchSADQCOM@ or OpImageBlockMatchSSDQCOM is used to
--     read from a reference image as result of this command, then the
--     specified reference coordinates /must/ not fail
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-integer-coordinate-validation integer texel coordinate validation>
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-OpImageWeightedSampleQCOM-06977#
--     If @OpImageWeightedSampleQCOM@, @OpImageBoxFilterQCOM@,
--     @OpImageBlockMatchWindowSSDQCOM@, @OpImageBlockMatchWindowSADQCOM@,
--     @OpImageBlockMatchGatherSSDQCOM@, @OpImageBlockMatchGatherSADQCOM@,
--     @OpImageBlockMatchSSDQCOM@, or @OpImageBlockMatchSADQCOM@ uses a
--     'Vulkan.Core10.Handles.Sampler' as a result of this command, then
--     the sampler /must/ have been created with
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_IMAGE_PROCESSING_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-OpImageWeightedSampleQCOM-06978#
--     If any command other than @OpImageWeightedSampleQCOM@,
--     @OpImageBoxFilterQCOM@, @OpImageBlockMatchWindowSSDQCOM@,
--     @OpImageBlockMatchWindowSADQCOM@, @OpImageBlockMatchGatherSSDQCOM@,
--     @OpImageBlockMatchGatherSADQCOM@, @OpImageBlockMatchSSDQCOM@, or
--     @OpImageBlockMatchSADQCOM@ uses a 'Vulkan.Core10.Handles.Sampler' as
--     a result of this command, then the sampler /must/ not have been
--     created with
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_IMAGE_PROCESSING_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-OpImageBlockMatchWindow-09215#
--     If a @OpImageBlockMatchWindow*QCOM@ or
--     @OpImageBlockMatchGather*QCOM@ instruction is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-OpImageBlockMatchWindow-09216#
--     If a @OpImageBlockMatchWindow*QCOM@ or
--     @OpImageBlockMatchGather*QCOM@ instruction is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s format /must/ be a single-component format.
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-OpImageBlockMatchWindow-09217#
--     If a @OpImageBlockMatchWindow*QCOM@ or
--     @OpImageBlockMatchGather*QCOM@ read from a reference image as result
--     of this command, then the specified reference coordinates /must/ not
--     fail
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-integer-coordinate-validation integer texel coordinate validation>
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-None-07288# Any shader
--     invocation executed by this command /must/
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-termination terminate>
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-commandBuffer-09181#
--     @commandBuffer@ /must/ not be a protected command buffer
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-commandBuffer-09182#
--     @commandBuffer@ /must/ be a primary command buffer
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-scratch-09183# @scratch@
--     /must/ be the device address of an allocated memory range at least
--     as large as the value of
--     'ExecutionGraphPipelineScratchSizeAMDX'::@size@ returned by
--     'ExecutionGraphPipelineScratchSizeAMDX' for the currently bound
--     execution graph pipeline
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-scratch-09184# @scratch@
--     /must/ be a device address within a 'Vulkan.Core10.Handles.Buffer'
--     created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_EXECUTION_GRAPH_SCRATCH_BIT_AMDX'
--     or 'BUFFER_USAGE_2_EXECUTION_GRAPH_SCRATCH_BIT_AMDX' flag
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-scratch-09185# Device
--     memory in the range [@scratch@,@scratch@
--     'ExecutionGraphPipelineScratchSizeAMDX'::@size@) /must/ have been
--     initialized with 'cmdInitializeGraphScratchMemoryAMDX' using the
--     currently bound execution graph pipeline, and not modified after
--     that by anything other than another execution graph dispatch command
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-maxComputeWorkGroupCount-09186#
--     Execution of this command /must/ not cause a node to be dispatched
--     with a larger number of workgroups than that specified by either a
--     @MaxNumWorkgroupsAMDX@ decoration in the dispatched node or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxComputeWorkGroupCount maxComputeWorkGroupCount>
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-maxExecutionGraphShaderPayloadCount-09187#
--     Execution of this command /must/ not cause any shader to initialize
--     more than
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxExecutionGraphShaderPayloadCount maxExecutionGraphShaderPayloadCount>
--     output payloads
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-NodeMaxPayloadsAMDX-09188#
--     Execution of this command /must/ not cause any shader that declares
--     @NodeMaxPayloadsAMDX@ to initialize more output payloads than
--     specified by the max number of payloads for that decoration. This
--     requirement applies to each @NodeMaxPayloadsAMDX@ decoration
--     separately
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-countInfo-09159#
--     @countInfo@ /must/ be a device pointer to a memory allocation
--     containing a valid 'DispatchGraphCountInfoAMDX' structure when this
--     command is executed on the device
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-countInfo-09160#
--     @countInfo@ /must/ be a device address within a
--     'Vulkan.Core10.Handles.Buffer' created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     flag
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-countInfo-09161#
--     @countInfo@ /must/ be a multiple of
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-executionGraphDispatchAddressAlignment executionGraphDispatchAddressAlignment>
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-countInfo-09162#
--     @countInfo->infos@ /must/ be a device pointer to a memory allocation
--     at least as large as the product of @count@ and @stride@ when this
--     command is executed on the device
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-countInfo-09163#
--     @countInfo->infos@ /must/ be a device address within a
--     'Vulkan.Core10.Handles.Buffer' created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     flag
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-countInfo-09164#
--     @countInfo->infos@ /must/ be a multiple of
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-executionGraphDispatchAddressAlignment executionGraphDispatchAddressAlignment>
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-infos-09165# Device memory
--     locations at indexes in the range [@infos@, @infos@ +
--     (@count@*@stride@)), at a granularity of @stride@ /must/ contain
--     valid 'DispatchGraphInfoAMDX' structures in the first 24 bytes when
--     this command is executed on the device
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-countInfo-09166# For each
--     'DispatchGraphInfoAMDX' structure in @countInfo->infos@, @payloads@
--     /must/ be a device pointer to a memory allocation at least as large
--     as the product of @payloadCount@ and @payloadStride@ when this
--     command is executed on the device
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-countInfo-09167# For each
--     'DispatchGraphInfoAMDX' structure in @countInfo->infos@, @payloads@
--     /must/ be a device address within a 'Vulkan.Core10.Handles.Buffer'
--     created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     flag
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-countInfo-09168# For each
--     'DispatchGraphInfoAMDX' structure in @countInfo->infos@, @payloads@
--     /must/ be a multiple of
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-executionGraphDispatchAddressAlignment executionGraphDispatchAddressAlignment>
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-countInfo-09169# For each
--     'DispatchGraphInfoAMDX' structure in @countInfo->infos@, @nodeIndex@
--     /must/ be a valid node index in the currently bound execution graph
--     pipeline, as returned by 'getExecutionGraphPipelineNodeIndexAMDX'
--     when this command is executed on the device
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-countInfo-09170# For each
--     'DispatchGraphInfoAMDX' structure in @countInfo->infos@, device
--     memory locations at indexes in the range [@payloads@, @payloads@ +
--     (@payloadCount@ * @payloadStride@)), at a granularity of
--     @payloadStride@ /must/ contain a payload matching the size of the
--     input payload expected by the node in @nodeIndex@ in the first bytes
--     when this command is executed on the device
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-renderpass# This command
--     /must/ only be called outside of a render pass instance
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-videocoding# This command
--     /must/ only be called outside of a video coding scope
--
-- -   #VUID-vkCmdDispatchGraphIndirectCountAMDX-bufferlevel#
--     @commandBuffer@ /must/ be a primary
--     'Vulkan.Core10.Handles.CommandBuffer'
--
-- == Host Synchronization
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | Graphics                                                                                                              | Action                                                                                                                                 |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMDX_shader_enqueue VK_AMDX_shader_enqueue>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress'
cmdDispatchGraphIndirectCountAMDX :: forall io
                                   . (MonadIO io)
                                  => -- | @commandBuffer@ is the command buffer into which the command will be
                                     -- recorded.
                                     CommandBuffer
                                  -> -- | @scratch@ is a pointer to the scratch memory to be used.
                                     ("scratch" ::: DeviceAddress)
                                  -> -- | @countInfo@ is a device address of a 'DispatchGraphCountInfoAMDX'
                                     -- structure defining the nodes which will be initially executed.
                                     ("countInfo" ::: DeviceAddress)
                                  -> io ()
cmdDispatchGraphIndirectCountAMDX commandBuffer scratch countInfo = liftIO $ do
  let vkCmdDispatchGraphIndirectCountAMDXPtr = pVkCmdDispatchGraphIndirectCountAMDX (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdDispatchGraphIndirectCountAMDXPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDispatchGraphIndirectCountAMDX is null" Nothing Nothing
  let vkCmdDispatchGraphIndirectCountAMDX' = mkVkCmdDispatchGraphIndirectCountAMDX vkCmdDispatchGraphIndirectCountAMDXPtr
  traceAroundEvent "vkCmdDispatchGraphIndirectCountAMDX" (vkCmdDispatchGraphIndirectCountAMDX'
                                                            (commandBufferHandle (commandBuffer))
                                                            (scratch)
                                                            (countInfo))
  pure $ ()


-- | VkPhysicalDeviceShaderEnqueuePropertiesAMDX - Structure describing
-- shader enqueue limits of an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceShaderEnqueuePropertiesAMDX' structure
-- describe the following limits:
--
-- = Description
--
-- If the 'PhysicalDeviceShaderEnqueuePropertiesAMDX' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMDX_shader_enqueue VK_AMDX_shader_enqueue>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderEnqueuePropertiesAMDX = PhysicalDeviceShaderEnqueuePropertiesAMDX
  { -- | #limits-maxExecutionGraphDepth# @maxExecutionGraphDepth@ defines the
    -- maximum node chain depth in the graph. The dispatched node is at depth 1
    -- and the node enqueued by it is at depth 2, and so on. If a node enqueues
    -- itself, each recursive enqueue increases the depth by 1 as well.
    maxExecutionGraphDepth :: Word32
  , -- | #limits-maxExecutionGraphShaderOutputNodes#
    -- @maxExecutionGraphShaderOutputNodes@ specifies the maximum number of
    -- unique nodes that can be dispatched from a single shader, and must be at
    -- least 256.
    maxExecutionGraphShaderOutputNodes :: Word32
  , -- | #limits-maxExecutionGraphShaderPayloadSize#
    -- @maxExecutionGraphShaderPayloadSize@ specifies the maximum total size of
    -- payload declarations in a shader. For any payload declarations that
    -- share resources, indicated by @NodeSharesPayloadLimitsWithAMDX@
    -- decorations, the maximum size of each set of shared payload declarations
    -- is taken. The sum of each shared set’s maximum size and the size of each
    -- unshared payload is counted against this limit.
    maxExecutionGraphShaderPayloadSize :: Word32
  , -- | #limits-maxExecutionGraphShaderPayloadCount#
    -- @maxExecutionGraphShaderPayloadCount@ specifies the maximum number of
    -- output payloads that can be initialized in a single workgroup.
    maxExecutionGraphShaderPayloadCount :: Word32
  , -- | #limits-executionGraphDispatchAddressAlignment#
    -- @executionGraphDispatchAddressAlignment@ specifies the alignment of
    -- non-scratch 'Vulkan.Core10.FundamentalTypes.DeviceAddress' arguments
    -- consumed by graph dispatch commands.
    executionGraphDispatchAddressAlignment :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderEnqueuePropertiesAMDX)
#endif
deriving instance Show PhysicalDeviceShaderEnqueuePropertiesAMDX

instance ToCStruct PhysicalDeviceShaderEnqueuePropertiesAMDX where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderEnqueuePropertiesAMDX{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ENQUEUE_PROPERTIES_AMDX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxExecutionGraphDepth)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxExecutionGraphShaderOutputNodes)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (maxExecutionGraphShaderPayloadSize)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (maxExecutionGraphShaderPayloadCount)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (executionGraphDispatchAddressAlignment)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ENQUEUE_PROPERTIES_AMDX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceShaderEnqueuePropertiesAMDX where
  peekCStruct p = do
    maxExecutionGraphDepth <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxExecutionGraphShaderOutputNodes <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    maxExecutionGraphShaderPayloadSize <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    maxExecutionGraphShaderPayloadCount <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    executionGraphDispatchAddressAlignment <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pure $ PhysicalDeviceShaderEnqueuePropertiesAMDX
             maxExecutionGraphDepth
             maxExecutionGraphShaderOutputNodes
             maxExecutionGraphShaderPayloadSize
             maxExecutionGraphShaderPayloadCount
             executionGraphDispatchAddressAlignment

instance Storable PhysicalDeviceShaderEnqueuePropertiesAMDX where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderEnqueuePropertiesAMDX where
  zero = PhysicalDeviceShaderEnqueuePropertiesAMDX
           zero
           zero
           zero
           zero
           zero


-- | VkPhysicalDeviceShaderEnqueueFeaturesAMDX - Structure describing whether
-- shader enqueue within execution graphs are supported by the
-- implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceShaderEnqueueFeaturesAMDX' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceShaderEnqueueFeaturesAMDX' /can/ also be used
-- in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMDX_shader_enqueue VK_AMDX_shader_enqueue>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderEnqueueFeaturesAMDX = PhysicalDeviceShaderEnqueueFeaturesAMDX
  { -- | #features-shaderEnqueue# @shaderEnqueue@ indicates whether the
    -- implementation supports
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#executiongraphs execution graphs>.
    shaderEnqueue :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderEnqueueFeaturesAMDX)
#endif
deriving instance Show PhysicalDeviceShaderEnqueueFeaturesAMDX

instance ToCStruct PhysicalDeviceShaderEnqueueFeaturesAMDX where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderEnqueueFeaturesAMDX{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ENQUEUE_FEATURES_AMDX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderEnqueue))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ENQUEUE_FEATURES_AMDX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderEnqueueFeaturesAMDX where
  peekCStruct p = do
    shaderEnqueue <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderEnqueueFeaturesAMDX
             (bool32ToBool shaderEnqueue)

instance Storable PhysicalDeviceShaderEnqueueFeaturesAMDX where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderEnqueueFeaturesAMDX where
  zero = PhysicalDeviceShaderEnqueueFeaturesAMDX
           zero


-- | VkExecutionGraphPipelineCreateInfoAMDX - Structure specifying parameters
-- of a newly created execution graph pipeline
--
-- = Description
--
-- The parameters @basePipelineHandle@ and @basePipelineIndex@ are
-- described in more detail in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#pipelines-pipeline-derivatives Pipeline Derivatives>.
--
-- Each shader stage provided when creating an execution graph pipeline
-- (including those in libraries) is associated with a name and an index,
-- determined by the inclusion or omission of a
-- 'PipelineShaderStageNodeCreateInfoAMDX' structure in its @pNext@ chain.
--
-- In addition to the shader name and index, an internal \"node index\" is
-- also generated for each node, which can be queried with
-- 'getExecutionGraphPipelineNodeIndexAMDX', and is used exclusively for
-- initial dispatch of an execution graph.
--
-- == Valid Usage
--
-- -   #VUID-VkExecutionGraphPipelineCreateInfoAMDX-flags-07984# If @flags@
--     contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineIndex@ is -1, @basePipelineHandle@ /must/ be
--     a valid execution graph 'Vulkan.Core10.Handles.Pipeline' handle
--
-- -   #VUID-VkExecutionGraphPipelineCreateInfoAMDX-flags-07985# If @flags@
--     contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineHandle@ is
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @basePipelineIndex@ /must/
--     be a valid index into the calling command’s @pCreateInfos@ parameter
--
-- -   #VUID-VkExecutionGraphPipelineCreateInfoAMDX-flags-07986# If @flags@
--     contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, @basePipelineIndex@ /must/ be -1 or @basePipelineHandle@
--     /must/ be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkExecutionGraphPipelineCreateInfoAMDX-layout-07987# If a push
--     constant block is declared in a shader, a push constant range in
--     @layout@ /must/ match both the shader stage and range
--
-- -   #VUID-VkExecutionGraphPipelineCreateInfoAMDX-layout-07988# If a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-resources resource variables>
--     is declared in a shader, a descriptor slot in @layout@ /must/ match
--     the shader stage
--
-- -   #VUID-VkExecutionGraphPipelineCreateInfoAMDX-layout-07990# If a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-resources resource variables>
--     is declared in a shader, and the descriptor type is not
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_EXT', a
--     descriptor slot in @layout@ /must/ match the descriptor type
--
-- -   #VUID-VkExecutionGraphPipelineCreateInfoAMDX-layout-07991# If a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-resources resource variables>
--     is declared in a shader as an array, a descriptor slot in @layout@
--     /must/ match the descriptor count
--
-- -   #VUID-VkExecutionGraphPipelineCreateInfoAMDX-flags-03365# @flags@
--     /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR'
--
-- -   #VUID-VkExecutionGraphPipelineCreateInfoAMDX-flags-03366# @flags@
--     /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR'
--
-- -   #VUID-VkExecutionGraphPipelineCreateInfoAMDX-flags-03367# @flags@
--     /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR'
--
-- -   #VUID-VkExecutionGraphPipelineCreateInfoAMDX-flags-03368# @flags@
--     /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR'
--
-- -   #VUID-VkExecutionGraphPipelineCreateInfoAMDX-flags-03369# @flags@
--     /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR'
--
-- -   #VUID-VkExecutionGraphPipelineCreateInfoAMDX-flags-03370# @flags@
--     /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_SKIP_AABBS_BIT_KHR'
--
-- -   #VUID-VkExecutionGraphPipelineCreateInfoAMDX-flags-03576# @flags@
--     /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR'
--
-- -   #VUID-VkExecutionGraphPipelineCreateInfoAMDX-flags-04945# @flags@
--     /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_ALLOW_MOTION_BIT_NV'
--
-- -   #VUID-VkExecutionGraphPipelineCreateInfoAMDX-flags-09007# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-deviceGeneratedComputePipelines ::deviceGeneratedComputePipelines>
--     is not enabled, @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV'
--
-- -   #VUID-VkExecutionGraphPipelineCreateInfoAMDX-flags-09008# If @flags@
--     includes
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV',
--     then the @pNext@ chain /must/ include a pointer to a valid instance
--     of
--     'Vulkan.Extensions.VK_NV_device_generated_commands_compute.ComputePipelineIndirectBufferInfoNV'
--     specifying the address where the pipeline’s metadata will be saved
--
-- -   #VUID-VkExecutionGraphPipelineCreateInfoAMDX-pipelineCreationCacheControl-02875#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-pipelineCreationCacheControl pipelineCreationCacheControl>
--     feature is not enabled, @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT'
--     or
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT'
--
-- -   #VUID-VkExecutionGraphPipelineCreateInfoAMDX-stage-09128# The
--     @stage@ member of any element of @pStages@ /must/ be
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_COMPUTE_BIT'
--
-- -   #VUID-VkExecutionGraphPipelineCreateInfoAMDX-pStages-09129# The
--     shader code for the entry point identified by each element of
--     @pStages@ and the rest of the state identified by this structure
--     /must/ adhere to the pipeline linking rules described in the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces Shader Interfaces>
--     chapter
--
-- -   #VUID-VkExecutionGraphPipelineCreateInfoAMDX-layout-09130# @layout@
--     /must/ be
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-pipelinelayout-consistency consistent>
--     with the layout of the shaders specified in @pStages@
--
-- -   #VUID-VkExecutionGraphPipelineCreateInfoAMDX-pLibraryInfo-09131# If
--     @pLibraryInfo@ is not @NULL@, each element of its @pLibraries@
--     member /must/ have been created with a @layout@ that is compatible
--     with the @layout@ in this pipeline
--
-- -   #VUID-VkExecutionGraphPipelineCreateInfoAMDX-layout-09132# The
--     number of resources in @layout@ accessible to each shader stage that
--     is used by the pipeline /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxPerStageResources@
--
-- -   #VUID-VkExecutionGraphPipelineCreateInfoAMDX-pLibraryInfo-09133# If
--     @pLibraryInfo@ is not @NULL@, each element of
--     @pLibraryInfo->libraries@ /must/ be either a compute pipeline or an
--     execution graph pipeline
--
-- -   #VUID-VkExecutionGraphPipelineCreateInfoAMDX-None-09134# There
--     /must/ be no two nodes in the pipeline that share both the same
--     shader name and index, as specified by
--     'PipelineShaderStageNodeCreateInfoAMDX'
--
-- -   #VUID-VkExecutionGraphPipelineCreateInfoAMDX-None-09135# There
--     /must/ be no two nodes in the pipeline that share the same shader
--     name and have input payload declarations with different sizes
--
-- -   #VUID-VkExecutionGraphPipelineCreateInfoAMDX-None-09136# There
--     /must/ be no two nodes in the pipeline that share the same name but
--     have different execution models
--
-- -   #VUID-VkExecutionGraphPipelineCreateInfoAMDX-CoalescedInputCountAMDX-09137#
--     There /must/ be no two nodes in the pipeline that share the same
--     name where one includes @CoalescedInputCountAMDX@ and the other does
--     not
--
-- -   #VUID-VkExecutionGraphPipelineCreateInfoAMDX-StaticNumWorkgroupsAMDX-09138#
--     There /must/ be no two nodes in the pipeline that share the same
--     name where one includes @StaticNumWorkgroupsAMDX@ and the other does
--     not
--
-- -   #VUID-VkExecutionGraphPipelineCreateInfoAMDX-PayloadNodeNameAMDX-09139#
--     If an output payload declared in any shader in the pipeline has a
--     @PayloadNodeNameAMDX@ decoration with a @Node@ @Name@ that matches
--     the shader name of any other node in the graph, the size of the
--     output payload /must/ match the size of the input payload in the
--     matching node
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkExecutionGraphPipelineCreateInfoAMDX-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXECUTION_GRAPH_PIPELINE_CREATE_INFO_AMDX'
--
-- -   #VUID-VkExecutionGraphPipelineCreateInfoAMDX-pNext-pNext# Each
--     @pNext@ member of any structure (including this one) in the @pNext@
--     chain /must/ be either @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_AMD_pipeline_compiler_control.PipelineCompilerControlCreateInfoAMD'
--     or
--     'Vulkan.Core13.Promoted_From_VK_EXT_pipeline_creation_feedback.PipelineCreationFeedbackCreateInfo'
--
-- -   #VUID-VkExecutionGraphPipelineCreateInfoAMDX-sType-unique# The
--     @sType@ value of each struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkExecutionGraphPipelineCreateInfoAMDX-flags-parameter#
--     @flags@ /must/ be a valid combination of
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits'
--     values
--
-- -   #VUID-VkExecutionGraphPipelineCreateInfoAMDX-pStages-parameter# If
--     @stageCount@ is not @0@, and @pStages@ is not @NULL@, @pStages@
--     /must/ be a valid pointer to an array of @stageCount@ valid
--     'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo' structures
--
-- -   #VUID-VkExecutionGraphPipelineCreateInfoAMDX-pLibraryInfo-parameter#
--     If @pLibraryInfo@ is not @NULL@, @pLibraryInfo@ /must/ be a valid
--     pointer to a valid
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'
--     structure
--
-- -   #VUID-VkExecutionGraphPipelineCreateInfoAMDX-layout-parameter#
--     @layout@ /must/ be a valid 'Vulkan.Core10.Handles.PipelineLayout'
--     handle
--
-- -   #VUID-VkExecutionGraphPipelineCreateInfoAMDX-commonparent# Both of
--     @basePipelineHandle@, and @layout@ that are valid handles of
--     non-ignored parameters /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMDX_shader_enqueue VK_AMDX_shader_enqueue>,
-- 'Vulkan.Core10.Handles.Pipeline',
-- 'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlags',
-- 'Vulkan.Core10.Handles.PipelineLayout',
-- 'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR',
-- 'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createExecutionGraphPipelinesAMDX'
data ExecutionGraphPipelineCreateInfoAMDX (es :: [Type]) = ExecutionGraphPipelineCreateInfoAMDX
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits'
    -- specifying how the pipeline will be generated.
    flags :: PipelineCreateFlags
  , -- | @stageCount@ is the number of entries in the @pStages@ array.
    stageCount :: Word32
  , -- | @pStages@ is a pointer to an array of @stageCount@
    -- 'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo' structures
    -- describing the set of the shader stages to be included in the execution
    -- graph pipeline.
    stages :: Vector (SomeStruct PipelineShaderStageCreateInfo)
  , -- | @pLibraryInfo@ is a pointer to a
    -- 'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'
    -- structure defining pipeline libraries to include.
    libraryInfo :: Maybe PipelineLibraryCreateInfoKHR
  , -- | @layout@ is the description of binding locations used by both the
    -- pipeline and descriptor sets used with the pipeline.
    layout :: PipelineLayout
  , -- | @basePipelineHandle@ is a pipeline to derive from
    basePipelineHandle :: Pipeline
  , -- | @basePipelineIndex@ is an index into the @pCreateInfos@ parameter to use
    -- as a pipeline to derive from
    basePipelineIndex :: Int32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExecutionGraphPipelineCreateInfoAMDX (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (ExecutionGraphPipelineCreateInfoAMDX es)

instance Extensible ExecutionGraphPipelineCreateInfoAMDX where
  extensibleTypeName = "ExecutionGraphPipelineCreateInfoAMDX"
  setNext ExecutionGraphPipelineCreateInfoAMDX{..} next' = ExecutionGraphPipelineCreateInfoAMDX{next = next', ..}
  getNext ExecutionGraphPipelineCreateInfoAMDX{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends ExecutionGraphPipelineCreateInfoAMDX e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PipelineCompilerControlCreateInfoAMD = Just f
    | Just Refl <- eqT @e @PipelineCreationFeedbackCreateInfo = Just f
    | otherwise = Nothing

instance ( Extendss ExecutionGraphPipelineCreateInfoAMDX es
         , PokeChain es ) => ToCStruct (ExecutionGraphPipelineCreateInfoAMDX es) where
  withCStruct x f = allocaBytes 64 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExecutionGraphPipelineCreateInfoAMDX{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXECUTION_GRAPH_PIPELINE_CREATE_INFO_AMDX)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineCreateFlags)) (flags)
    let pStagesLength = Data.Vector.length $ (stages)
    stageCount'' <- lift $ if (stageCount) == 0
      then pure $ fromIntegral pStagesLength
      else do
        unless (fromIntegral pStagesLength == (stageCount) || pStagesLength == 0) $
          throwIO $ IOError Nothing InvalidArgument "" "pStages must be empty or have 'stageCount' elements" Nothing Nothing
        pure (stageCount)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (stageCount'')
    pStages'' <- if Data.Vector.null (stages)
      then pure nullPtr
      else do
        pPStages <- ContT $ allocaBytes @(PipelineShaderStageCreateInfo _) (((Data.Vector.length (stages))) * 48)
        Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPStages `plusPtr` (48 * (i)) :: Ptr (PipelineShaderStageCreateInfo _))) (e) . ($ ())) ((stages))
        pure $ pPStages
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr (PipelineShaderStageCreateInfo _)))) pStages''
    pLibraryInfo'' <- case (libraryInfo) of
      Nothing -> pure nullPtr
      Just j -> ContT $ withCStruct (j)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr PipelineLibraryCreateInfoKHR))) pLibraryInfo''
    lift $ poke ((p `plusPtr` 40 :: Ptr PipelineLayout)) (layout)
    lift $ poke ((p `plusPtr` 48 :: Ptr Pipeline)) (basePipelineHandle)
    lift $ poke ((p `plusPtr` 56 :: Ptr Int32)) (basePipelineIndex)
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXECUTION_GRAPH_PIPELINE_CREATE_INFO_AMDX)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 40 :: Ptr PipelineLayout)) (zero)
    lift $ poke ((p `plusPtr` 56 :: Ptr Int32)) (zero)
    lift $ f

instance ( Extendss ExecutionGraphPipelineCreateInfoAMDX es
         , PeekChain es ) => FromCStruct (ExecutionGraphPipelineCreateInfoAMDX es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @PipelineCreateFlags ((p `plusPtr` 16 :: Ptr PipelineCreateFlags))
    stageCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pStages <- peek @(Ptr (PipelineShaderStageCreateInfo _)) ((p `plusPtr` 24 :: Ptr (Ptr (PipelineShaderStageCreateInfo _))))
    let pStagesLength = if pStages == nullPtr then 0 else (fromIntegral stageCount)
    pStages' <- generateM pStagesLength (\i -> peekSomeCStruct (forgetExtensions ((pStages `advancePtrBytes` (48 * (i)) :: Ptr (PipelineShaderStageCreateInfo _)))))
    pLibraryInfo <- peek @(Ptr PipelineLibraryCreateInfoKHR) ((p `plusPtr` 32 :: Ptr (Ptr PipelineLibraryCreateInfoKHR)))
    pLibraryInfo' <- maybePeek (\j -> peekCStruct @PipelineLibraryCreateInfoKHR (j)) pLibraryInfo
    layout <- peek @PipelineLayout ((p `plusPtr` 40 :: Ptr PipelineLayout))
    basePipelineHandle <- peek @Pipeline ((p `plusPtr` 48 :: Ptr Pipeline))
    basePipelineIndex <- peek @Int32 ((p `plusPtr` 56 :: Ptr Int32))
    pure $ ExecutionGraphPipelineCreateInfoAMDX
             next
             flags
             stageCount
             pStages'
             pLibraryInfo'
             layout
             basePipelineHandle
             basePipelineIndex

instance es ~ '[] => Zero (ExecutionGraphPipelineCreateInfoAMDX es) where
  zero = ExecutionGraphPipelineCreateInfoAMDX
           ()
           zero
           zero
           mempty
           Nothing
           zero
           zero
           zero


-- | VkPipelineShaderStageNodeCreateInfoAMDX - Structure specifying the
-- shader name and index with an execution graph
--
-- = Description
--
-- When included in the @pNext@ chain of a
-- 'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo' structure, this
-- structure specifies the shader name and shader index of a node when
-- creating an execution graph pipeline. If this structure is omitted, the
-- shader name is set to the name of the entry point in SPIR-V and the
-- shader index is set to @0@.
--
-- When dispatching a node from another shader, the name is fixed at
-- pipeline creation, but the index /can/ be set dynamically. By
-- associating multiple shaders with the same name but different indexes,
-- applications can dynamically select different nodes to execute.
-- Applications /must/ ensure each node has a unique name and index.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineShaderStageNodeCreateInfoAMDX-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_NODE_CREATE_INFO_AMDX'
--
-- -   #VUID-VkPipelineShaderStageNodeCreateInfoAMDX-pName-parameter# If
--     @pName@ is not @NULL@, @pName@ /must/ be a null-terminated UTF-8
--     string
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMDX_shader_enqueue VK_AMDX_shader_enqueue>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getExecutionGraphPipelineNodeIndexAMDX'
data PipelineShaderStageNodeCreateInfoAMDX = PipelineShaderStageNodeCreateInfoAMDX
  { -- | @pName@ is the shader name to use when creating a node in an execution
    -- graph. If @pName@ is @NULL@, the name of the entry point specified in
    -- SPIR-V is used as the shader name.
    name :: Maybe ByteString
  , -- | @index@ is the shader index to use when creating a node in an execution
    -- graph. If @index@ is
    -- 'Vulkan.Core10.APIConstants.SHADER_INDEX_UNUSED_AMDX' then the original
    -- index is used, either as specified by the @ShaderIndexAMDX@ execution
    -- mode, or @0@ if that too is not specified.
    index :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineShaderStageNodeCreateInfoAMDX)
#endif
deriving instance Show PipelineShaderStageNodeCreateInfoAMDX

instance ToCStruct PipelineShaderStageNodeCreateInfoAMDX where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineShaderStageNodeCreateInfoAMDX{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_NODE_CREATE_INFO_AMDX)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pName'' <- case (name) of
      Nothing -> pure nullPtr
      Just j -> ContT $ useAsCString (j)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr CChar))) pName''
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (index)
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_NODE_CREATE_INFO_AMDX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    f

instance FromCStruct PipelineShaderStageNodeCreateInfoAMDX where
  peekCStruct p = do
    pName <- peek @(Ptr CChar) ((p `plusPtr` 16 :: Ptr (Ptr CChar)))
    pName' <- maybePeek (\j -> packCString (j)) pName
    index <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ PipelineShaderStageNodeCreateInfoAMDX
             pName' index

instance Zero PipelineShaderStageNodeCreateInfoAMDX where
  zero = PipelineShaderStageNodeCreateInfoAMDX
           Nothing
           zero


-- | VkExecutionGraphPipelineScratchSizeAMDX - Structure describing the
-- scratch space required to dispatch an execution graph
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMDX_shader_enqueue VK_AMDX_shader_enqueue>,
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getExecutionGraphPipelineScratchSizeAMDX'
data ExecutionGraphPipelineScratchSizeAMDX = ExecutionGraphPipelineScratchSizeAMDX
  { -- | @size@ indicates the scratch space required for dispatch the queried
    -- execution graph.
    size :: DeviceSize }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExecutionGraphPipelineScratchSizeAMDX)
#endif
deriving instance Show ExecutionGraphPipelineScratchSizeAMDX

instance ToCStruct ExecutionGraphPipelineScratchSizeAMDX where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExecutionGraphPipelineScratchSizeAMDX{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXECUTION_GRAPH_PIPELINE_SCRATCH_SIZE_AMDX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (size)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXECUTION_GRAPH_PIPELINE_SCRATCH_SIZE_AMDX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct ExecutionGraphPipelineScratchSizeAMDX where
  peekCStruct p = do
    size <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    pure $ ExecutionGraphPipelineScratchSizeAMDX
             size

instance Storable ExecutionGraphPipelineScratchSizeAMDX where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExecutionGraphPipelineScratchSizeAMDX where
  zero = ExecutionGraphPipelineScratchSizeAMDX
           zero


-- | VkDispatchGraphInfoAMDX - Structure specifying node parameters for
-- execution graph dispatch
--
-- = Description
--
-- Whether @payloads@ is consumed as a device or host pointer is defined by
-- the command this structure is used in.
--
-- == Valid Usage
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMDX_shader_enqueue VK_AMDX_shader_enqueue>,
-- 'DeviceOrHostAddressConstAMDX', 'DispatchGraphCountInfoAMDX'
data DispatchGraphInfoAMDX = DispatchGraphInfoAMDX
  { -- | @nodeIndex@ is the index of a node in an execution graph to be
    -- dispatched.
    nodeIndex :: Word32
  , -- | @payloadCount@ is the number of payloads to dispatch for the specified
    -- node.
    --
    -- #VUID-VkDispatchGraphInfoAMDX-payloadCount-09171# @payloadCount@ /must/
    -- be no greater than
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-maxExecutionGraphShaderPayloadCount maxExecutionGraphShaderPayloadCount>
    payloadCount :: Word32
  , -- | @payloads@ is a device or host address pointer to a flat array of
    -- payloads with size equal to the product of @payloadCount@ and
    -- @payloadStride@
    payloads :: DeviceOrHostAddressConstAMDX
  , -- | @payloadStride@ is the byte stride between successive payloads in
    -- @payloads@
    payloadStride :: Word64
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DispatchGraphInfoAMDX)
#endif
deriving instance Show DispatchGraphInfoAMDX

instance ToCStruct DispatchGraphInfoAMDX where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DispatchGraphInfoAMDX{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr Word32)) (nodeIndex)
    lift $ poke ((p `plusPtr` 4 :: Ptr Word32)) (payloadCount)
    ContT $ pokeCStruct ((p `plusPtr` 8 :: Ptr DeviceOrHostAddressConstAMDX)) (payloads) . ($ ())
    lift $ poke ((p `plusPtr` 16 :: Ptr Word64)) (payloadStride)
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 8 :: Ptr DeviceOrHostAddressConstAMDX)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 16 :: Ptr Word64)) (zero)
    lift $ f

instance Zero DispatchGraphInfoAMDX where
  zero = DispatchGraphInfoAMDX
           zero
           zero
           zero
           zero


-- | VkDispatchGraphCountInfoAMDX - Structure specifying count parameters for
-- execution graph dispatch
--
-- = Description
--
-- Whether @infos@ is consumed as a device or host pointer is defined by
-- the command this structure is used in.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMDX_shader_enqueue VK_AMDX_shader_enqueue>,
-- 'DeviceOrHostAddressConstAMDX', 'cmdDispatchGraphAMDX',
-- 'cmdDispatchGraphIndirectAMDX', 'cmdDispatchGraphIndirectCountAMDX'
data DispatchGraphCountInfoAMDX = DispatchGraphCountInfoAMDX
  { -- | @count@ is the number of dispatches to perform.
    count :: Word32
  , -- | @infos@ is the device or host address of a flat array of
    -- 'DispatchGraphInfoAMDX' structures
    infos :: DeviceOrHostAddressConstAMDX
  , -- | @stride@ is the byte stride between successive 'DispatchGraphInfoAMDX'
    -- structures in @infos@
    stride :: Word64
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DispatchGraphCountInfoAMDX)
#endif
deriving instance Show DispatchGraphCountInfoAMDX

instance ToCStruct DispatchGraphCountInfoAMDX where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DispatchGraphCountInfoAMDX{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr Word32)) (count)
    ContT $ pokeCStruct ((p `plusPtr` 8 :: Ptr DeviceOrHostAddressConstAMDX)) (infos) . ($ ())
    lift $ poke ((p `plusPtr` 16 :: Ptr Word64)) (stride)
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    ContT $ pokeCStruct ((p `plusPtr` 8 :: Ptr DeviceOrHostAddressConstAMDX)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 16 :: Ptr Word64)) (zero)
    lift $ f

instance Zero DispatchGraphCountInfoAMDX where
  zero = DispatchGraphCountInfoAMDX
           zero
           zero
           zero


data DeviceOrHostAddressConstAMDX
  = DeviceAddressConstAMDX DeviceAddress
  | HostAddressConstAMDX (Ptr ())
  deriving (Show)

instance ToCStruct DeviceOrHostAddressConstAMDX where
  withCStruct x f = allocaBytes 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct :: Ptr DeviceOrHostAddressConstAMDX -> DeviceOrHostAddressConstAMDX -> IO a -> IO a
  pokeCStruct p = (. const) . runContT .  \case
    DeviceAddressConstAMDX v -> lift $ poke (castPtr @_ @DeviceAddress p) (v)
    HostAddressConstAMDX v -> lift $ poke (castPtr @_ @(Ptr ()) p) (v)
  pokeZeroCStruct :: Ptr DeviceOrHostAddressConstAMDX -> IO b -> IO b
  pokeZeroCStruct _ f = f
  cStructSize = 8
  cStructAlignment = 8

instance Zero DeviceOrHostAddressConstAMDX where
  zero = DeviceAddressConstAMDX zero


type BufferUsageFlags2KHR = BufferUsageFlagBits2KHR

-- | VkBufferUsageFlagBits2KHR - Bitmask controlling how a pipeline is
-- created
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>
newtype BufferUsageFlagBits2KHR = BufferUsageFlagBits2KHR Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'BUFFER_USAGE_2_TRANSFER_SRC_BIT_KHR' specifies that the buffer /can/ be
-- used as the source of a /transfer command/ (see the definition of
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages-transfer >).
pattern BUFFER_USAGE_2_TRANSFER_SRC_BIT_KHR = BufferUsageFlagBits2KHR 0x0000000000000001

-- | 'BUFFER_USAGE_2_TRANSFER_DST_BIT_KHR' specifies that the buffer /can/ be
-- used as the destination of a transfer command.
pattern BUFFER_USAGE_2_TRANSFER_DST_BIT_KHR = BufferUsageFlagBits2KHR 0x0000000000000002

-- | 'BUFFER_USAGE_2_UNIFORM_TEXEL_BUFFER_BIT_KHR' specifies that the buffer
-- /can/ be used to create a 'Vulkan.Core10.Handles.BufferView' suitable
-- for occupying a 'Vulkan.Core10.Handles.DescriptorSet' slot of type
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'.
pattern BUFFER_USAGE_2_UNIFORM_TEXEL_BUFFER_BIT_KHR = BufferUsageFlagBits2KHR 0x0000000000000004

-- | 'BUFFER_USAGE_2_STORAGE_TEXEL_BUFFER_BIT_KHR' specifies that the buffer
-- /can/ be used to create a 'Vulkan.Core10.Handles.BufferView' suitable
-- for occupying a 'Vulkan.Core10.Handles.DescriptorSet' slot of type
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'.
pattern BUFFER_USAGE_2_STORAGE_TEXEL_BUFFER_BIT_KHR = BufferUsageFlagBits2KHR 0x0000000000000008

-- | 'BUFFER_USAGE_2_UNIFORM_BUFFER_BIT_KHR' specifies that the buffer /can/
-- be used in a 'Vulkan.Core10.DescriptorSet.DescriptorBufferInfo' suitable
-- for occupying a 'Vulkan.Core10.Handles.DescriptorSet' slot either of
-- type 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER'
-- or
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'.
pattern BUFFER_USAGE_2_UNIFORM_BUFFER_BIT_KHR = BufferUsageFlagBits2KHR 0x0000000000000010

-- | 'BUFFER_USAGE_2_STORAGE_BUFFER_BIT_KHR' specifies that the buffer /can/
-- be used in a 'Vulkan.Core10.DescriptorSet.DescriptorBufferInfo' suitable
-- for occupying a 'Vulkan.Core10.Handles.DescriptorSet' slot either of
-- type 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER'
-- or
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'.
pattern BUFFER_USAGE_2_STORAGE_BUFFER_BIT_KHR = BufferUsageFlagBits2KHR 0x0000000000000020

-- | 'BUFFER_USAGE_2_INDEX_BUFFER_BIT_KHR' specifies that the buffer is
-- suitable for passing as the @buffer@ parameter to
-- 'Vulkan.Extensions.VK_KHR_maintenance5.cmdBindIndexBuffer2KHR' and
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBindIndexBuffer'.
pattern BUFFER_USAGE_2_INDEX_BUFFER_BIT_KHR = BufferUsageFlagBits2KHR 0x0000000000000040

-- | 'BUFFER_USAGE_2_VERTEX_BUFFER_BIT_KHR' specifies that the buffer is
-- suitable for passing as an element of the @pBuffers@ array to
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBindVertexBuffers'.
pattern BUFFER_USAGE_2_VERTEX_BUFFER_BIT_KHR = BufferUsageFlagBits2KHR 0x0000000000000080

-- | 'BUFFER_USAGE_2_INDIRECT_BUFFER_BIT_KHR' specifies that the buffer is
-- suitable for passing as the @buffer@ parameter to
-- 'Vulkan.Core10.CommandBufferBuilding.cmdDrawIndirect',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdDrawIndexedIndirect',
-- 'Vulkan.Extensions.VK_NV_mesh_shader.cmdDrawMeshTasksIndirectNV',
-- 'Vulkan.Extensions.VK_NV_mesh_shader.cmdDrawMeshTasksIndirectCountNV',
-- 'Vulkan.Extensions.VK_EXT_mesh_shader.cmdDrawMeshTasksIndirectEXT',
-- 'Vulkan.Extensions.VK_EXT_mesh_shader.cmdDrawMeshTasksIndirectCountEXT',
-- 'Vulkan.Extensions.VK_HUAWEI_cluster_culling_shader.cmdDrawClusterIndirectHUAWEI',
-- or 'Vulkan.Core10.CommandBufferBuilding.cmdDispatchIndirect'. It is also
-- suitable for passing as the @buffer@ member of
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.IndirectCommandsStreamNV',
-- or @sequencesCountBuffer@ or @sequencesIndexBuffer@ or
-- @preprocessedBuffer@ member of
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.GeneratedCommandsInfoNV'
pattern BUFFER_USAGE_2_INDIRECT_BUFFER_BIT_KHR = BufferUsageFlagBits2KHR 0x0000000000000100

-- No documentation found for Nested "VkBufferUsageFlagBits2KHR" "VK_BUFFER_USAGE_2_MICROMAP_STORAGE_BIT_EXT"
pattern BUFFER_USAGE_2_MICROMAP_STORAGE_BIT_EXT = BufferUsageFlagBits2KHR 0x0000000001000000

-- No documentation found for Nested "VkBufferUsageFlagBits2KHR" "VK_BUFFER_USAGE_2_MICROMAP_BUILD_INPUT_READ_ONLY_BIT_EXT"
pattern BUFFER_USAGE_2_MICROMAP_BUILD_INPUT_READ_ONLY_BIT_EXT = BufferUsageFlagBits2KHR 0x0000000000800000

-- | 'BUFFER_USAGE_2_PUSH_DESCRIPTORS_DESCRIPTOR_BUFFER_BIT_EXT' specifies
-- that the buffer, when bound, /can/ be used by the implementation to
-- support push descriptors when using descriptor buffers.
pattern BUFFER_USAGE_2_PUSH_DESCRIPTORS_DESCRIPTOR_BUFFER_BIT_EXT = BufferUsageFlagBits2KHR 0x0000000004000000

-- | 'BUFFER_USAGE_2_RESOURCE_DESCRIPTOR_BUFFER_BIT_EXT' specifies that the
-- buffer is suitable to contain resource descriptors when bound as a
-- descriptor buffer.
pattern BUFFER_USAGE_2_RESOURCE_DESCRIPTOR_BUFFER_BIT_EXT = BufferUsageFlagBits2KHR 0x0000000000400000

-- | 'BUFFER_USAGE_2_SAMPLER_DESCRIPTOR_BUFFER_BIT_EXT' specifies that the
-- buffer is suitable to contain sampler and combined image sampler
-- descriptors when bound as a descriptor buffer. Buffers containing
-- combined image sampler descriptors /must/ also specify
-- 'BUFFER_USAGE_2_RESOURCE_DESCRIPTOR_BUFFER_BIT_EXT'.
pattern BUFFER_USAGE_2_SAMPLER_DESCRIPTOR_BUFFER_BIT_EXT = BufferUsageFlagBits2KHR 0x0000000000200000

-- | 'BUFFER_USAGE_2_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR' specifies that
-- the buffer is suitable for storage space for a
-- 'Vulkan.Extensions.Handles.AccelerationStructureKHR'.
pattern BUFFER_USAGE_2_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR = BufferUsageFlagBits2KHR 0x0000000000100000

-- | 'BUFFER_USAGE_2_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR'
-- specifies that the buffer is suitable for use as a read-only input to an
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#acceleration-structure-building acceleration structure build>.
pattern BUFFER_USAGE_2_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR = BufferUsageFlagBits2KHR 0x0000000000080000

-- | 'BUFFER_USAGE_2_SHADER_DEVICE_ADDRESS_BIT_KHR' specifies that the buffer
-- /can/ be used to retrieve a buffer device address via
-- 'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.getBufferDeviceAddress'
-- and use that address to access the buffer’s memory from a shader.
pattern BUFFER_USAGE_2_SHADER_DEVICE_ADDRESS_BIT_KHR = BufferUsageFlagBits2KHR 0x0000000000020000

-- | 'BUFFER_USAGE_2_VIDEO_ENCODE_SRC_BIT_KHR' is reserved for future use.
pattern BUFFER_USAGE_2_VIDEO_ENCODE_SRC_BIT_KHR = BufferUsageFlagBits2KHR 0x0000000000010000

-- | 'BUFFER_USAGE_2_VIDEO_ENCODE_DST_BIT_KHR' specifies that the buffer
-- /can/ be used as the destination video bitstream buffer in a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#video-encode-operations video encode operation>.
pattern BUFFER_USAGE_2_VIDEO_ENCODE_DST_BIT_KHR = BufferUsageFlagBits2KHR 0x0000000000008000

-- | 'BUFFER_USAGE_2_VIDEO_DECODE_DST_BIT_KHR' is reserved for future use.
pattern BUFFER_USAGE_2_VIDEO_DECODE_DST_BIT_KHR = BufferUsageFlagBits2KHR 0x0000000000004000

-- | 'BUFFER_USAGE_2_VIDEO_DECODE_SRC_BIT_KHR' specifies that the buffer
-- /can/ be used as the source video bitstream buffer in a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#video-decode-operations video decode operation>.
pattern BUFFER_USAGE_2_VIDEO_DECODE_SRC_BIT_KHR = BufferUsageFlagBits2KHR 0x0000000000002000

-- | 'BUFFER_USAGE_2_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT' specifies
-- that the buffer is suitable for using as a counter buffer with
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdBeginTransformFeedbackEXT'
-- and
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdEndTransformFeedbackEXT'.
pattern BUFFER_USAGE_2_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT = BufferUsageFlagBits2KHR 0x0000000000001000

-- | 'BUFFER_USAGE_2_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT' specifies that the
-- buffer is suitable for using for binding as a transform feedback buffer
-- with
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdBindTransformFeedbackBuffersEXT'.
pattern BUFFER_USAGE_2_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT = BufferUsageFlagBits2KHR 0x0000000000000800

-- | 'BUFFER_USAGE_2_SHADER_BINDING_TABLE_BIT_KHR' specifies that the buffer
-- is suitable for use as a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shader-binding-table Shader Binding Table>.
pattern BUFFER_USAGE_2_SHADER_BINDING_TABLE_BIT_KHR = BufferUsageFlagBits2KHR 0x0000000000000400

-- | 'BUFFER_USAGE_2_CONDITIONAL_RENDERING_BIT_EXT' specifies that the buffer
-- is suitable for passing as the @buffer@ parameter to
-- 'Vulkan.Extensions.VK_EXT_conditional_rendering.cmdBeginConditionalRenderingEXT'.
pattern BUFFER_USAGE_2_CONDITIONAL_RENDERING_BIT_EXT = BufferUsageFlagBits2KHR 0x0000000000000200

-- | 'BUFFER_USAGE_2_EXECUTION_GRAPH_SCRATCH_BIT_AMDX' specifies that the
-- buffer /can/ be used for as scratch memory for
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#executiongraphs execution graph dispatch>.
pattern BUFFER_USAGE_2_EXECUTION_GRAPH_SCRATCH_BIT_AMDX = BufferUsageFlagBits2KHR 0x0000000002000000

conNameBufferUsageFlagBits2KHR :: String
conNameBufferUsageFlagBits2KHR = "BufferUsageFlagBits2KHR"

enumPrefixBufferUsageFlagBits2KHR :: String
enumPrefixBufferUsageFlagBits2KHR = "BUFFER_USAGE_2_"

showTableBufferUsageFlagBits2KHR :: [(BufferUsageFlagBits2KHR, String)]
showTableBufferUsageFlagBits2KHR =
  [
    ( BUFFER_USAGE_2_TRANSFER_SRC_BIT_KHR
    , "TRANSFER_SRC_BIT_KHR"
    )
  ,
    ( BUFFER_USAGE_2_TRANSFER_DST_BIT_KHR
    , "TRANSFER_DST_BIT_KHR"
    )
  ,
    ( BUFFER_USAGE_2_UNIFORM_TEXEL_BUFFER_BIT_KHR
    , "UNIFORM_TEXEL_BUFFER_BIT_KHR"
    )
  ,
    ( BUFFER_USAGE_2_STORAGE_TEXEL_BUFFER_BIT_KHR
    , "STORAGE_TEXEL_BUFFER_BIT_KHR"
    )
  ,
    ( BUFFER_USAGE_2_UNIFORM_BUFFER_BIT_KHR
    , "UNIFORM_BUFFER_BIT_KHR"
    )
  ,
    ( BUFFER_USAGE_2_STORAGE_BUFFER_BIT_KHR
    , "STORAGE_BUFFER_BIT_KHR"
    )
  ,
    ( BUFFER_USAGE_2_INDEX_BUFFER_BIT_KHR
    , "INDEX_BUFFER_BIT_KHR"
    )
  ,
    ( BUFFER_USAGE_2_VERTEX_BUFFER_BIT_KHR
    , "VERTEX_BUFFER_BIT_KHR"
    )
  ,
    ( BUFFER_USAGE_2_INDIRECT_BUFFER_BIT_KHR
    , "INDIRECT_BUFFER_BIT_KHR"
    )
  ,
    ( BUFFER_USAGE_2_MICROMAP_STORAGE_BIT_EXT
    , "MICROMAP_STORAGE_BIT_EXT"
    )
  ,
    ( BUFFER_USAGE_2_MICROMAP_BUILD_INPUT_READ_ONLY_BIT_EXT
    , "MICROMAP_BUILD_INPUT_READ_ONLY_BIT_EXT"
    )
  ,
    ( BUFFER_USAGE_2_PUSH_DESCRIPTORS_DESCRIPTOR_BUFFER_BIT_EXT
    , "PUSH_DESCRIPTORS_DESCRIPTOR_BUFFER_BIT_EXT"
    )
  ,
    ( BUFFER_USAGE_2_RESOURCE_DESCRIPTOR_BUFFER_BIT_EXT
    , "RESOURCE_DESCRIPTOR_BUFFER_BIT_EXT"
    )
  ,
    ( BUFFER_USAGE_2_SAMPLER_DESCRIPTOR_BUFFER_BIT_EXT
    , "SAMPLER_DESCRIPTOR_BUFFER_BIT_EXT"
    )
  ,
    ( BUFFER_USAGE_2_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR
    , "ACCELERATION_STRUCTURE_STORAGE_BIT_KHR"
    )
  ,
    ( BUFFER_USAGE_2_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR
    , "ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR"
    )
  ,
    ( BUFFER_USAGE_2_SHADER_DEVICE_ADDRESS_BIT_KHR
    , "SHADER_DEVICE_ADDRESS_BIT_KHR"
    )
  ,
    ( BUFFER_USAGE_2_VIDEO_ENCODE_SRC_BIT_KHR
    , "VIDEO_ENCODE_SRC_BIT_KHR"
    )
  ,
    ( BUFFER_USAGE_2_VIDEO_ENCODE_DST_BIT_KHR
    , "VIDEO_ENCODE_DST_BIT_KHR"
    )
  ,
    ( BUFFER_USAGE_2_VIDEO_DECODE_DST_BIT_KHR
    , "VIDEO_DECODE_DST_BIT_KHR"
    )
  ,
    ( BUFFER_USAGE_2_VIDEO_DECODE_SRC_BIT_KHR
    , "VIDEO_DECODE_SRC_BIT_KHR"
    )
  ,
    ( BUFFER_USAGE_2_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT
    , "TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT"
    )
  ,
    ( BUFFER_USAGE_2_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT
    , "TRANSFORM_FEEDBACK_BUFFER_BIT_EXT"
    )
  ,
    ( BUFFER_USAGE_2_SHADER_BINDING_TABLE_BIT_KHR
    , "SHADER_BINDING_TABLE_BIT_KHR"
    )
  ,
    ( BUFFER_USAGE_2_CONDITIONAL_RENDERING_BIT_EXT
    , "CONDITIONAL_RENDERING_BIT_EXT"
    )
  ,
    ( BUFFER_USAGE_2_EXECUTION_GRAPH_SCRATCH_BIT_AMDX
    , "EXECUTION_GRAPH_SCRATCH_BIT_AMDX"
    )
  ]

instance Show BufferUsageFlagBits2KHR where
  showsPrec =
    enumShowsPrec
      enumPrefixBufferUsageFlagBits2KHR
      showTableBufferUsageFlagBits2KHR
      conNameBufferUsageFlagBits2KHR
      (\(BufferUsageFlagBits2KHR x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read BufferUsageFlagBits2KHR where
  readPrec =
    enumReadPrec
      enumPrefixBufferUsageFlagBits2KHR
      showTableBufferUsageFlagBits2KHR
      conNameBufferUsageFlagBits2KHR
      BufferUsageFlagBits2KHR

type AMDX_SHADER_ENQUEUE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_AMDX_SHADER_ENQUEUE_SPEC_VERSION"
pattern AMDX_SHADER_ENQUEUE_SPEC_VERSION :: forall a . Integral a => a
pattern AMDX_SHADER_ENQUEUE_SPEC_VERSION = 1


type AMDX_SHADER_ENQUEUE_EXTENSION_NAME = "VK_AMDX_shader_enqueue"

-- No documentation found for TopLevel "VK_AMDX_SHADER_ENQUEUE_EXTENSION_NAME"
pattern AMDX_SHADER_ENQUEUE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern AMDX_SHADER_ENQUEUE_EXTENSION_NAME = "VK_AMDX_shader_enqueue"

