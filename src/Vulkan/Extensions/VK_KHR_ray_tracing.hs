{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_ray_tracing  ( destroyAccelerationStructureKHR
                                             , getAccelerationStructureMemoryRequirementsKHR
                                             , bindAccelerationStructureMemoryKHR
                                             , cmdCopyAccelerationStructureKHR
                                             , copyAccelerationStructureKHR
                                             , cmdCopyAccelerationStructureToMemoryKHR
                                             , copyAccelerationStructureToMemoryKHR
                                             , cmdCopyMemoryToAccelerationStructureKHR
                                             , copyMemoryToAccelerationStructureKHR
                                             , cmdWriteAccelerationStructuresPropertiesKHR
                                             , writeAccelerationStructuresPropertiesKHR
                                             , cmdTraceRaysKHR
                                             , getRayTracingShaderGroupHandlesKHR
                                             , getRayTracingCaptureReplayShaderGroupHandlesKHR
                                             , createRayTracingPipelinesKHR
                                             , cmdTraceRaysIndirectKHR
                                             , getDeviceAccelerationStructureCompatibilityKHR
                                             , createAccelerationStructureKHR
                                             , withAccelerationStructureKHR
                                             , cmdBuildAccelerationStructureKHR
                                             , cmdBuildAccelerationStructureIndirectKHR
                                             , buildAccelerationStructureKHR
                                             , getAccelerationStructureDeviceAddressKHR
                                             , RayTracingShaderGroupCreateInfoKHR(..)
                                             , RayTracingPipelineCreateInfoKHR(..)
                                             , BindAccelerationStructureMemoryInfoKHR(..)
                                             , WriteDescriptorSetAccelerationStructureKHR(..)
                                             , AccelerationStructureMemoryRequirementsInfoKHR(..)
                                             , PhysicalDeviceRayTracingFeaturesKHR(..)
                                             , PhysicalDeviceRayTracingPropertiesKHR(..)
                                             , StridedBufferRegionKHR(..)
                                             , TraceRaysIndirectCommandKHR(..)
                                             , AccelerationStructureGeometryTrianglesDataKHR(..)
                                             , AccelerationStructureGeometryAabbsDataKHR(..)
                                             , AccelerationStructureGeometryInstancesDataKHR(..)
                                             , AccelerationStructureGeometryKHR(..)
                                             , AccelerationStructureBuildGeometryInfoKHR(..)
                                             , AccelerationStructureBuildOffsetInfoKHR(..)
                                             , AccelerationStructureCreateGeometryTypeInfoKHR(..)
                                             , AccelerationStructureCreateInfoKHR(..)
                                             , AabbPositionsKHR(..)
                                             , TransformMatrixKHR(..)
                                             , AccelerationStructureInstanceKHR(..)
                                             , AccelerationStructureDeviceAddressInfoKHR(..)
                                             , AccelerationStructureVersionKHR(..)
                                             , CopyAccelerationStructureInfoKHR(..)
                                             , CopyAccelerationStructureToMemoryInfoKHR(..)
                                             , CopyMemoryToAccelerationStructureInfoKHR(..)
                                             , RayTracingPipelineInterfaceCreateInfoKHR(..)
                                             , DeviceOrHostAddressKHR(..)
                                             , DeviceOrHostAddressConstKHR(..)
                                             , AccelerationStructureGeometryDataKHR(..)
                                             , GeometryInstanceFlagBitsKHR( GEOMETRY_INSTANCE_TRIANGLE_FACING_CULL_DISABLE_BIT_KHR
                                                                          , GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_KHR
                                                                          , GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_KHR
                                                                          , GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_KHR
                                                                          , ..
                                                                          )
                                             , GeometryInstanceFlagsKHR
                                             , GeometryFlagBitsKHR( GEOMETRY_OPAQUE_BIT_KHR
                                                                  , GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_KHR
                                                                  , ..
                                                                  )
                                             , GeometryFlagsKHR
                                             , BuildAccelerationStructureFlagBitsKHR( BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR
                                                                                    , BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR
                                                                                    , BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR
                                                                                    , BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_KHR
                                                                                    , BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_KHR
                                                                                    , ..
                                                                                    )
                                             , BuildAccelerationStructureFlagsKHR
                                             , CopyAccelerationStructureModeKHR( COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR
                                                                               , COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR
                                                                               , COPY_ACCELERATION_STRUCTURE_MODE_SERIALIZE_KHR
                                                                               , COPY_ACCELERATION_STRUCTURE_MODE_DESERIALIZE_KHR
                                                                               , ..
                                                                               )
                                             , AccelerationStructureTypeKHR( ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR
                                                                           , ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR
                                                                           , ..
                                                                           )
                                             , GeometryTypeKHR( GEOMETRY_TYPE_TRIANGLES_KHR
                                                              , GEOMETRY_TYPE_AABBS_KHR
                                                              , GEOMETRY_TYPE_INSTANCES_KHR
                                                              , ..
                                                              )
                                             , AccelerationStructureMemoryRequirementsTypeKHR( ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_KHR
                                                                                             , ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_KHR
                                                                                             , ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_KHR
                                                                                             , ..
                                                                                             )
                                             , AccelerationStructureBuildTypeKHR( ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_KHR
                                                                                , ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR
                                                                                , ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_OR_DEVICE_KHR
                                                                                , ..
                                                                                )
                                             , RayTracingShaderGroupTypeKHR( RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR
                                                                           , RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR
                                                                           , RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR
                                                                           , ..
                                                                           )
                                             , KHR_RAY_TRACING_SPEC_VERSION
                                             , pattern KHR_RAY_TRACING_SPEC_VERSION
                                             , KHR_RAY_TRACING_EXTENSION_NAME
                                             , pattern KHR_RAY_TRACING_EXTENSION_NAME
                                             , AccelerationStructureKHR(..)
                                             , PipelineLibraryCreateInfoKHR(..)
                                             , DebugReportObjectTypeEXT(..)
                                             , SHADER_UNUSED_KHR
                                             , pattern SHADER_UNUSED_KHR
                                             ) where

import Vulkan.CStruct.Utils (FixedArray)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Bits ((.&.))
import Data.Bits ((.|.))
import Data.Bits (shiftL)
import Data.Bits (shiftR)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Utils (maybePeek)
import Foreign.Marshal.Utils (with)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Numeric (showHex)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import qualified Data.ByteString (length)
import Data.ByteString (packCStringLen)
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Data.Coerce (coerce)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.Trans.Cont (runContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Foreign.C.Types (CSize(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Bits (Bits)
import Data.String (IsString)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(CFloat))
import Foreign.C.Types (CSize)
import Foreign.C.Types (CSize(CSize))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Word (Word8)
import Text.Read.Lex (Lexeme(Ident))
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.BaseType (bool32ToBool)
import Vulkan.Core10.BaseType (boolToBool32)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.CStruct.Extends (peekSomeCStruct)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.Extensions.Handles (AccelerationStructureKHR)
import Vulkan.Extensions.Handles (AccelerationStructureKHR(..))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.BaseType (Bool32)
import Vulkan.Core10.Handles (Buffer)
import Vulkan.Core10.Handles (Buffer(..))
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_deferred_host_operations (DeferredOperationInfoKHR)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.BaseType (DeviceAddress)
import Vulkan.Dynamic (DeviceCmds(pVkBindAccelerationStructureMemoryKHR))
import Vulkan.Dynamic (DeviceCmds(pVkBuildAccelerationStructureKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBuildAccelerationStructureIndirectKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBuildAccelerationStructureKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyAccelerationStructureKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyAccelerationStructureToMemoryKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyMemoryToAccelerationStructureKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdTraceRaysIndirectKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdTraceRaysKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdWriteAccelerationStructuresPropertiesKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCopyAccelerationStructureKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCopyAccelerationStructureToMemoryKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCopyMemoryToAccelerationStructureKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCreateAccelerationStructureKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCreateRayTracingPipelinesKHR))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyAccelerationStructureKHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetAccelerationStructureDeviceAddressKHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetAccelerationStructureMemoryRequirementsKHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetDeviceAccelerationStructureCompatibilityKHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetRayTracingCaptureReplayShaderGroupHandlesKHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetRayTracingShaderGroupHandlesKHR))
import Vulkan.Dynamic (DeviceCmds(pVkWriteAccelerationStructuresPropertiesKHR))
import Vulkan.Core10.Handles (DeviceMemory)
import Vulkan.Core10.BaseType (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.BaseType (Flags)
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.IndexType (IndexType)
import Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (MemoryRequirements2)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.Core10.Handles (Pipeline)
import Vulkan.Core10.Handles (Pipeline(..))
import Vulkan.Core10.Handles (PipelineCache)
import Vulkan.Core10.Handles (PipelineCache(..))
import Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_pipeline_creation_feedback (PipelineCreationFeedbackCreateInfoEXT)
import Vulkan.Core10.Handles (PipelineLayout)
import Vulkan.Extensions.VK_KHR_pipeline_library (PipelineLibraryCreateInfoKHR)
import Vulkan.Core10.Pipeline (PipelineShaderStageCreateInfo)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Handles (QueryPool)
import Vulkan.Core10.Handles (QueryPool(..))
import Vulkan.Core10.Enums.QueryType (QueryType)
import Vulkan.Core10.Enums.QueryType (QueryType(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.BaseType (Bool32(FALSE))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_GEOMETRY_TYPE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DEVICE_ADDRESS_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_AABBS_DATA_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_INSTANCES_DATA_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_TRIANGLES_DATA_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_VERSION_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_TO_MEMORY_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_MEMORY_TO_ACCELERATION_STRUCTURE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RAY_TRACING_PIPELINE_INTERFACE_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.APIConstants (pattern UUID_SIZE)
import Vulkan.Extensions.Handles (AccelerationStructureKHR(..))
import Vulkan.Extensions.VK_EXT_debug_report (DebugReportObjectTypeEXT(..))
import Vulkan.Extensions.VK_KHR_pipeline_library (PipelineLibraryCreateInfoKHR(..))
import Vulkan.Core10.APIConstants (SHADER_UNUSED_KHR)
import Vulkan.Core10.APIConstants (pattern SHADER_UNUSED_KHR)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyAccelerationStructureKHR
  :: FunPtr (Ptr Device_T -> AccelerationStructureKHR -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> AccelerationStructureKHR -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyAccelerationStructureKHR - Destroy an acceleration structure
-- object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the buffer.
--
-- -   @accelerationStructure@ is the acceleration structure to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @accelerationStructure@ /must/
--     have completed execution
--
-- -   If 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @accelerationStructure@ was created, a compatible set
--     of callbacks /must/ be provided here
--
-- -   If no 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @accelerationStructure@ was created, @pAllocator@
--     /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   @accelerationStructure@ /must/ be a valid
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR' handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   @accelerationStructure@ /must/ have been created, allocated, or
--     retrieved from @device@
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.AccelerationStructureKHR',
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device'
destroyAccelerationStructureKHR :: forall io . MonadIO io => Device -> AccelerationStructureKHR -> ("allocator" ::: Maybe AllocationCallbacks) -> io ()
destroyAccelerationStructureKHR device accelerationStructure allocator = liftIO . evalContT $ do
  let vkDestroyAccelerationStructureKHR' = mkVkDestroyAccelerationStructureKHR (pVkDestroyAccelerationStructureKHR (deviceCmds (device :: Device)))
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroyAccelerationStructureKHR' (deviceHandle (device)) (accelerationStructure) pAllocator
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetAccelerationStructureMemoryRequirementsKHR
  :: FunPtr (Ptr Device_T -> Ptr AccelerationStructureMemoryRequirementsInfoKHR -> Ptr (MemoryRequirements2 a) -> IO ()) -> Ptr Device_T -> Ptr AccelerationStructureMemoryRequirementsInfoKHR -> Ptr (MemoryRequirements2 a) -> IO ()

-- | vkGetAccelerationStructureMemoryRequirementsKHR - Get acceleration
-- structure memory requirements
--
-- = Parameters
--
-- -   @device@ is the logical device on which the acceleration structure
--     was created.
--
-- -   @pInfo@ specifies the acceleration structure to get memory
--     requirements for.
--
-- -   @pMemoryRequirements@ returns the requested acceleration structure
--     memory requirements.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'AccelerationStructureMemoryRequirementsInfoKHR',
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.MemoryRequirements2'
getAccelerationStructureMemoryRequirementsKHR :: forall a io . (PokeChain a, PeekChain a, MonadIO io) => Device -> AccelerationStructureMemoryRequirementsInfoKHR -> io (MemoryRequirements2 a)
getAccelerationStructureMemoryRequirementsKHR device info = liftIO . evalContT $ do
  let vkGetAccelerationStructureMemoryRequirementsKHR' = mkVkGetAccelerationStructureMemoryRequirementsKHR (pVkGetAccelerationStructureMemoryRequirementsKHR (deviceCmds (device :: Device)))
  pInfo <- ContT $ withCStruct (info)
  pPMemoryRequirements <- ContT (withZeroCStruct @(MemoryRequirements2 _))
  lift $ vkGetAccelerationStructureMemoryRequirementsKHR' (deviceHandle (device)) pInfo (pPMemoryRequirements)
  pMemoryRequirements <- lift $ peekCStruct @(MemoryRequirements2 _) pPMemoryRequirements
  pure $ (pMemoryRequirements)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkBindAccelerationStructureMemoryKHR
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr BindAccelerationStructureMemoryInfoKHR -> IO Result) -> Ptr Device_T -> Word32 -> Ptr BindAccelerationStructureMemoryInfoKHR -> IO Result

-- | vkBindAccelerationStructureMemoryKHR - Bind acceleration structure
-- memory
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the acceleration structures
--     and memory.
--
-- -   @bindInfoCount@ is the number of elements in @pBindInfos@.
--
-- -   @pBindInfos@ is a pointer to an array of
--     'BindAccelerationStructureMemoryInfoKHR' structures describing
--     acceleration structures and memory to bind.
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'BindAccelerationStructureMemoryInfoKHR', 'Vulkan.Core10.Handles.Device'
bindAccelerationStructureMemoryKHR :: forall io . MonadIO io => Device -> ("bindInfos" ::: Vector BindAccelerationStructureMemoryInfoKHR) -> io ()
bindAccelerationStructureMemoryKHR device bindInfos = liftIO . evalContT $ do
  let vkBindAccelerationStructureMemoryKHR' = mkVkBindAccelerationStructureMemoryKHR (pVkBindAccelerationStructureMemoryKHR (deviceCmds (device :: Device)))
  pPBindInfos <- ContT $ allocaBytesAligned @BindAccelerationStructureMemoryInfoKHR ((Data.Vector.length (bindInfos)) * 56) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPBindInfos `plusPtr` (56 * (i)) :: Ptr BindAccelerationStructureMemoryInfoKHR) (e) . ($ ())) (bindInfos)
  r <- lift $ vkBindAccelerationStructureMemoryKHR' (deviceHandle (device)) ((fromIntegral (Data.Vector.length $ (bindInfos)) :: Word32)) (pPBindInfos)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyAccelerationStructureKHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr (CopyAccelerationStructureInfoKHR a) -> IO ()) -> Ptr CommandBuffer_T -> Ptr (CopyAccelerationStructureInfoKHR a) -> IO ()

-- | vkCmdCopyAccelerationStructureKHR - Copy an acceleration structure
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @pInfo@ is a pointer to a 'CopyAccelerationStructureInfoKHR'
--     structure defining the copy operation.
--
-- == Valid Usage
--
-- -   All 'Vulkan.Extensions.Handles.AccelerationStructureKHR' objects
--     referenced by this command /must/ be bound to device memory
--
-- -   The
--     'Vulkan.Extensions.VK_KHR_deferred_host_operations.DeferredOperationInfoKHR'
--     structure /must/ not be included in the @pNext@ chain of the
--     'CopyAccelerationStructureInfoKHR' structure
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @pInfo@ /must/ be a valid pointer to a valid
--     'CopyAccelerationStructureInfoKHR' structure
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Compute                                                                                                               |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'CopyAccelerationStructureInfoKHR'
cmdCopyAccelerationStructureKHR :: forall a io . (PokeChain a, MonadIO io) => CommandBuffer -> CopyAccelerationStructureInfoKHR a -> io ()
cmdCopyAccelerationStructureKHR commandBuffer info = liftIO . evalContT $ do
  let vkCmdCopyAccelerationStructureKHR' = mkVkCmdCopyAccelerationStructureKHR (pVkCmdCopyAccelerationStructureKHR (deviceCmds (commandBuffer :: CommandBuffer)))
  pInfo <- ContT $ withCStruct (info)
  lift $ vkCmdCopyAccelerationStructureKHR' (commandBufferHandle (commandBuffer)) pInfo
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCopyAccelerationStructureKHR
  :: FunPtr (Ptr Device_T -> Ptr (CopyAccelerationStructureInfoKHR a) -> IO Result) -> Ptr Device_T -> Ptr (CopyAccelerationStructureInfoKHR a) -> IO Result

-- | vkCopyAccelerationStructureKHR - Copy an acceleration structure on the
-- host
--
-- = Parameters
--
-- This command fulfills the same task as 'cmdCopyAccelerationStructureKHR'
-- but executed by the host.
--
-- = Description
--
-- -   @device@ is the device which owns the acceleration structures.
--
-- -   @pInfo@ is a pointer to a 'CopyAccelerationStructureInfoKHR'
--     structure defining the copy operation.
--
-- If the
-- 'Vulkan.Extensions.VK_KHR_deferred_host_operations.DeferredOperationInfoKHR'
-- structure is included in the @pNext@ chain of the
-- 'CopyAccelerationStructureInfoKHR' structure, the operation of this
-- command is /deferred/, as defined in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#deferred-host-operations Deferred Host Operations>
-- chapter.
--
-- == Valid Usage
--
-- -   All 'Vulkan.Extensions.Handles.AccelerationStructureKHR' objects
--     referenced by this command /must/ be bound to host-visible memory
--
-- -   the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#feature-raytracing-hostascmds ::rayTracingHostAccelerationStructureCommands>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   @pInfo@ /must/ be a valid pointer to a valid
--     'CopyAccelerationStructureInfoKHR' structure
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.OPERATION_DEFERRED_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.OPERATION_NOT_DEFERRED_KHR'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'CopyAccelerationStructureInfoKHR', 'Vulkan.Core10.Handles.Device'
copyAccelerationStructureKHR :: forall a io . (PokeChain a, MonadIO io) => Device -> CopyAccelerationStructureInfoKHR a -> io (Result)
copyAccelerationStructureKHR device info = liftIO . evalContT $ do
  let vkCopyAccelerationStructureKHR' = mkVkCopyAccelerationStructureKHR (pVkCopyAccelerationStructureKHR (deviceCmds (device :: Device)))
  pInfo <- ContT $ withCStruct (info)
  r <- lift $ vkCopyAccelerationStructureKHR' (deviceHandle (device)) pInfo
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyAccelerationStructureToMemoryKHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr (CopyAccelerationStructureToMemoryInfoKHR a) -> IO ()) -> Ptr CommandBuffer_T -> Ptr (CopyAccelerationStructureToMemoryInfoKHR a) -> IO ()

-- | vkCmdCopyAccelerationStructureToMemoryKHR - Copy an acceleration
-- structure to device memory
--
-- = Parameters
--
-- This command produces the same results as
-- 'copyAccelerationStructureToMemoryKHR', but writes its result to a
-- device address, and is executed on the device rather than the host. The
-- output /may/ not necessarily be bit-for-bit identical, but it can be
-- equally used by either 'cmdCopyMemoryToAccelerationStructureKHR' or
-- 'copyMemoryToAccelerationStructureKHR'.
--
-- = Description
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @pInfo@ is an a pointer to a
--     'CopyAccelerationStructureToMemoryInfoKHR' structure defining the
--     copy operation.
--
-- The defined header structure for the serialized data consists of:
--
-- -   'Vulkan.Core10.APIConstants.UUID_SIZE' bytes of data matching
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.PhysicalDeviceIDProperties'::@driverUUID@
--
-- -   'Vulkan.Core10.APIConstants.UUID_SIZE' bytes of data identifying the
--     compatibility for comparison using
--     'getDeviceAccelerationStructureCompatibilityKHR'
--
-- -   A 64-bit integer of the total size matching the value queried using
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_SIZE_KHR'
--
-- -   A 64-bit integer of the deserialized size to be passed in to
--     'AccelerationStructureCreateInfoKHR'::@compactedSize@
--
-- -   A 64-bit integer of the count of the number of acceleration
--     structure handles following. This will be zero for a bottom-level
--     acceleration structure.
--
-- The corresponding handles matching the values returned by
-- 'getAccelerationStructureDeviceAddressKHR' or
-- 'Vulkan.Extensions.VK_NV_ray_tracing.getAccelerationStructureHandleNV'
-- are tightly packed in the buffer following the count. The application is
-- expected to store a mapping between those handles and the original
-- application-generated bottom-level acceleration structures to provide
-- when deserializing.
--
-- == Valid Usage
--
-- -   All 'DeviceOrHostAddressConstKHR' referenced by this command /must/
--     contain valid device addresses
--
-- -   All 'Vulkan.Extensions.Handles.AccelerationStructureKHR' objects
--     referenced by this command /must/ be bound to device memory
--
-- -   The
--     'Vulkan.Extensions.VK_KHR_deferred_host_operations.DeferredOperationInfoKHR'
--     structure /must/ not be included in the @pNext@ chain of the
--     'CopyAccelerationStructureToMemoryInfoKHR' structure
--
-- -   [[VUID-{refpage}-mode-03412]] @mode@ /must/ be
--     'COPY_ACCELERATION_STRUCTURE_MODE_SERIALIZE_KHR'
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @pInfo@ /must/ be a valid pointer to a valid
--     'CopyAccelerationStructureToMemoryInfoKHR' structure
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Compute                                                                                                               |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'CopyAccelerationStructureToMemoryInfoKHR'
cmdCopyAccelerationStructureToMemoryKHR :: forall a io . (PokeChain a, MonadIO io) => CommandBuffer -> CopyAccelerationStructureToMemoryInfoKHR a -> io ()
cmdCopyAccelerationStructureToMemoryKHR commandBuffer info = liftIO . evalContT $ do
  let vkCmdCopyAccelerationStructureToMemoryKHR' = mkVkCmdCopyAccelerationStructureToMemoryKHR (pVkCmdCopyAccelerationStructureToMemoryKHR (deviceCmds (commandBuffer :: CommandBuffer)))
  pInfo <- ContT $ withCStruct (info)
  lift $ vkCmdCopyAccelerationStructureToMemoryKHR' (commandBufferHandle (commandBuffer)) pInfo
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCopyAccelerationStructureToMemoryKHR
  :: FunPtr (Ptr Device_T -> Ptr (CopyAccelerationStructureToMemoryInfoKHR a) -> IO Result) -> Ptr Device_T -> Ptr (CopyAccelerationStructureToMemoryInfoKHR a) -> IO Result

-- | vkCopyAccelerationStructureToMemoryKHR - Serialize an acceleration
-- structure on the host
--
-- = Parameters
--
-- This command fulfills the same task as
-- 'cmdCopyAccelerationStructureToMemoryKHR' but executed by the host.
--
-- = Description
--
-- This command produces the same results as
-- 'cmdCopyAccelerationStructureToMemoryKHR', but writes its result
-- directly to a host pointer, and is executed on the host rather than the
-- device. The output /may/ not necessarily be bit-for-bit identical, but
-- it can be equally used by either
-- 'cmdCopyMemoryToAccelerationStructureKHR' or
-- 'copyMemoryToAccelerationStructureKHR'.
--
-- -   @device@ is the device which owns @pInfo->src@.
--
-- -   @pInfo@ is a pointer to a 'CopyAccelerationStructureToMemoryInfoKHR'
--     structure defining the copy operation.
--
-- If the
-- 'Vulkan.Extensions.VK_KHR_deferred_host_operations.DeferredOperationInfoKHR'
-- structure is included in the @pNext@ chain of the
-- 'CopyAccelerationStructureToMemoryInfoKHR' structure, the operation of
-- this command is /deferred/, as defined in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#deferred-host-operations>
-- chapter.
--
-- == Valid Usage
--
-- -   All 'Vulkan.Extensions.Handles.AccelerationStructureKHR' objects
--     referenced by this command /must/ be bound to host-visible memory
--
-- -   All 'DeviceOrHostAddressKHR' referenced by this command /must/
--     contain valid host pointers
--
-- -   the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#feature-raytracing-hostascmds ::rayTracingHostAccelerationStructureCommands>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   @pInfo@ /must/ be a valid pointer to a valid
--     'CopyAccelerationStructureToMemoryInfoKHR' structure
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.OPERATION_DEFERRED_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.OPERATION_NOT_DEFERRED_KHR'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'CopyAccelerationStructureToMemoryInfoKHR',
-- 'Vulkan.Core10.Handles.Device'
copyAccelerationStructureToMemoryKHR :: forall a io . (PokeChain a, MonadIO io) => Device -> CopyAccelerationStructureToMemoryInfoKHR a -> io (Result)
copyAccelerationStructureToMemoryKHR device info = liftIO . evalContT $ do
  let vkCopyAccelerationStructureToMemoryKHR' = mkVkCopyAccelerationStructureToMemoryKHR (pVkCopyAccelerationStructureToMemoryKHR (deviceCmds (device :: Device)))
  pInfo <- ContT $ withCStruct (info)
  r <- lift $ vkCopyAccelerationStructureToMemoryKHR' (deviceHandle (device)) pInfo
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyMemoryToAccelerationStructureKHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr (CopyMemoryToAccelerationStructureInfoKHR a) -> IO ()) -> Ptr CommandBuffer_T -> Ptr (CopyMemoryToAccelerationStructureInfoKHR a) -> IO ()

-- | vkCmdCopyMemoryToAccelerationStructureKHR - Copy device memory to an
-- acceleration structure
--
-- = Parameters
--
-- This command can accept acceleration structures produced by either
-- 'cmdCopyAccelerationStructureToMemoryKHR' or
-- 'copyAccelerationStructureToMemoryKHR'.
--
-- = Description
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @pInfo@ is a pointer to a 'CopyMemoryToAccelerationStructureInfoKHR'
--     structure defining the copy operation.
--
-- The structure provided as input to deserialize is as described in
-- 'cmdCopyAccelerationStructureToMemoryKHR', with any acceleration
-- structure handles filled in with the newly-queried handles to bottom
-- level acceleration structures created before deserialization. These do
-- not need to be built at deserialize time, but /must/ be created.
--
-- == Valid Usage
--
-- -   All 'DeviceOrHostAddressKHR' referenced by this command /must/
--     contain valid device addresses
--
-- -   All 'Vulkan.Extensions.Handles.AccelerationStructureKHR' objects
--     referenced by this command /must/ be bound to device memory
--
-- -   The
--     'Vulkan.Extensions.VK_KHR_deferred_host_operations.DeferredOperationInfoKHR'
--     structure /must/ not be included in the @pNext@ chain of the
--     'CopyMemoryToAccelerationStructureInfoKHR' structure
--
-- -   [[VUID-{refpage}-mode-03413]] @mode@ /must/ be
--     'COPY_ACCELERATION_STRUCTURE_MODE_DESERIALIZE_KHR'
--
-- -   [[VUID-{refpage}-pInfo-03414]] The data in @pInfo->src@ /must/ have
--     a format compatible with the destination physical device as returned
--     by 'getDeviceAccelerationStructureCompatibilityKHR'
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @pInfo@ /must/ be a valid pointer to a valid
--     'CopyMemoryToAccelerationStructureInfoKHR' structure
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Compute                                                                                                               |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'CopyMemoryToAccelerationStructureInfoKHR'
cmdCopyMemoryToAccelerationStructureKHR :: forall a io . (PokeChain a, MonadIO io) => CommandBuffer -> CopyMemoryToAccelerationStructureInfoKHR a -> io ()
cmdCopyMemoryToAccelerationStructureKHR commandBuffer info = liftIO . evalContT $ do
  let vkCmdCopyMemoryToAccelerationStructureKHR' = mkVkCmdCopyMemoryToAccelerationStructureKHR (pVkCmdCopyMemoryToAccelerationStructureKHR (deviceCmds (commandBuffer :: CommandBuffer)))
  pInfo <- ContT $ withCStruct (info)
  lift $ vkCmdCopyMemoryToAccelerationStructureKHR' (commandBufferHandle (commandBuffer)) pInfo
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCopyMemoryToAccelerationStructureKHR
  :: FunPtr (Ptr Device_T -> Ptr (CopyMemoryToAccelerationStructureInfoKHR a) -> IO Result) -> Ptr Device_T -> Ptr (CopyMemoryToAccelerationStructureInfoKHR a) -> IO Result

-- | vkCopyMemoryToAccelerationStructureKHR - Deserialize an acceleration
-- structure on the host
--
-- = Parameters
--
-- This command fulfills the same task as
-- 'cmdCopyMemoryToAccelerationStructureKHR' but is executed by the host.
--
-- = Description
--
-- This command can accept acceleration structures produced by either
-- 'cmdCopyAccelerationStructureToMemoryKHR' or
-- 'copyAccelerationStructureToMemoryKHR'.
--
-- -   @device@ is the device which owns @pInfo->dst@.
--
-- -   @pInfo@ is a pointer to a 'CopyMemoryToAccelerationStructureInfoKHR'
--     structure defining the copy operation.
--
-- If the
-- 'Vulkan.Extensions.VK_KHR_deferred_host_operations.DeferredOperationInfoKHR'
-- structure is included in the @pNext@ chain of the
-- 'CopyMemoryToAccelerationStructureInfoKHR' structure, the operation of
-- this command is /deferred/, as defined in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#deferred-host-operations Deferred Host Operations>
-- chapter.
--
-- == Valid Usage
--
-- -   All 'Vulkan.Extensions.Handles.AccelerationStructureKHR' objects
--     referenced by this command /must/ be bound to host-visible memory
--
-- -   All 'DeviceOrHostAddressConstKHR' referenced by this command /must/
--     contain valid host pointers
--
-- -   the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#feature-raytracing-hostascmds ::rayTracingHostAccelerationStructureCommands>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   @pInfo@ /must/ be a valid pointer to a valid
--     'CopyMemoryToAccelerationStructureInfoKHR' structure
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.OPERATION_DEFERRED_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.OPERATION_NOT_DEFERRED_KHR'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'CopyMemoryToAccelerationStructureInfoKHR',
-- 'Vulkan.Core10.Handles.Device'
copyMemoryToAccelerationStructureKHR :: forall a io . (PokeChain a, MonadIO io) => Device -> CopyMemoryToAccelerationStructureInfoKHR a -> io (Result)
copyMemoryToAccelerationStructureKHR device info = liftIO . evalContT $ do
  let vkCopyMemoryToAccelerationStructureKHR' = mkVkCopyMemoryToAccelerationStructureKHR (pVkCopyMemoryToAccelerationStructureKHR (deviceCmds (device :: Device)))
  pInfo <- ContT $ withCStruct (info)
  r <- lift $ vkCopyMemoryToAccelerationStructureKHR' (deviceHandle (device)) pInfo
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdWriteAccelerationStructuresPropertiesKHR
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr AccelerationStructureKHR -> QueryType -> QueryPool -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr AccelerationStructureKHR -> QueryType -> QueryPool -> Word32 -> IO ()

-- | vkCmdWriteAccelerationStructuresPropertiesKHR - Write acceleration
-- structure result parameters to query results.
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @accelerationStructureCount@ is the count of acceleration structures
--     for which to query the property.
--
-- -   @pAccelerationStructures@ is a pointer to an array of existing
--     previously built acceleration structures.
--
-- -   @queryType@ is a 'Vulkan.Core10.Enums.QueryType.QueryType' value
--     specifying the type of queries managed by the pool.
--
-- -   @queryPool@ is the query pool that will manage the results of the
--     query.
--
-- -   @firstQuery@ is the first query index within the query pool that
--     will contain the @accelerationStructureCount@ number of results.
--
-- == Valid Usage
--
-- -   @queryPool@ /must/ have been created with a @queryType@ matching
--     @queryType@
--
-- -   The queries identified by @queryPool@ and @firstQuery@ /must/ be
--     /unavailable/
--
-- -   [[VUID-{refpage}-accelerationStructures-03431]] All acceleration
--     structures in @accelerationStructures@ /must/ have been built with
--     'BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR' if
--     @queryType@ is
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR'
--
-- -   [[VUID-{refpage}-queryType-03432]] @queryType@ /must/ be
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR'
--     or
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_SIZE_KHR'
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @pAccelerationStructures@ /must/ be a valid pointer to an array of
--     @accelerationStructureCount@ valid
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR' handles
--
-- -   @queryType@ /must/ be a valid
--     'Vulkan.Core10.Enums.QueryType.QueryType' value
--
-- -   @queryPool@ /must/ be a valid 'Vulkan.Core10.Handles.QueryPool'
--     handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   @accelerationStructureCount@ /must/ be greater than @0@
--
-- -   Each of @commandBuffer@, @queryPool@, and the elements of
--     @pAccelerationStructures@ /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Device'
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Compute                                                                                                               |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.AccelerationStructureKHR',
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Handles.QueryPool',
-- 'Vulkan.Core10.Enums.QueryType.QueryType'
cmdWriteAccelerationStructuresPropertiesKHR :: forall io . MonadIO io => CommandBuffer -> ("accelerationStructures" ::: Vector AccelerationStructureKHR) -> QueryType -> QueryPool -> ("firstQuery" ::: Word32) -> io ()
cmdWriteAccelerationStructuresPropertiesKHR commandBuffer accelerationStructures queryType queryPool firstQuery = liftIO . evalContT $ do
  let vkCmdWriteAccelerationStructuresPropertiesKHR' = mkVkCmdWriteAccelerationStructuresPropertiesKHR (pVkCmdWriteAccelerationStructuresPropertiesKHR (deviceCmds (commandBuffer :: CommandBuffer)))
  pPAccelerationStructures <- ContT $ allocaBytesAligned @AccelerationStructureKHR ((Data.Vector.length (accelerationStructures)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPAccelerationStructures `plusPtr` (8 * (i)) :: Ptr AccelerationStructureKHR) (e)) (accelerationStructures)
  lift $ vkCmdWriteAccelerationStructuresPropertiesKHR' (commandBufferHandle (commandBuffer)) ((fromIntegral (Data.Vector.length $ (accelerationStructures)) :: Word32)) (pPAccelerationStructures) (queryType) (queryPool) (firstQuery)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkWriteAccelerationStructuresPropertiesKHR
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr AccelerationStructureKHR -> QueryType -> CSize -> Ptr () -> CSize -> IO Result) -> Ptr Device_T -> Word32 -> Ptr AccelerationStructureKHR -> QueryType -> CSize -> Ptr () -> CSize -> IO Result

-- | vkWriteAccelerationStructuresPropertiesKHR - Query acceleration
-- structure meta-data on the host
--
-- = Parameters
--
-- This command fulfills the same task as
-- 'cmdWriteAccelerationStructuresPropertiesKHR' but executed by the host.
--
-- = Description
--
-- -   @device@ is the device which owns the acceleration structures in
--     @pAccelerationStructures@.
--
-- -   @accelerationStructureCount@ is the count of acceleration structures
--     for which to query the property.
--
-- -   @pAccelerationStructures@ points to an array of existing previously
--     built acceleration structures.
--
-- -   @queryType@ is a 'Vulkan.Core10.Enums.QueryType.QueryType' value
--     specifying the property to be queried.
--
-- -   @dataSize@ is the size in bytes of the buffer pointed to by @pData@.
--
-- -   @pData@ is a pointer to a user-allocated buffer where the results
--     will be written.
--
-- -   @stride@ is the stride in bytes between results for individual
--     queries within @pData@.
--
-- == Valid Usage
--
-- -   If @queryType@ is
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR',
--     then @stride@ /must/ be a multiple of the size of
--     'Vulkan.Core10.BaseType.DeviceSize'
--
-- -   If @queryType@ is
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR',
--     then @data@ /must/ point to a 'Vulkan.Core10.BaseType.DeviceSize'
--
-- -   If @queryType@ is
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_SIZE_KHR',
--     then @stride@ /must/ be a multiple of the size of
--     'Vulkan.Core10.BaseType.DeviceSize'
--
-- -   If @queryType@ is
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_SIZE_KHR',
--     then @data@ /must/ point to a 'Vulkan.Core10.BaseType.DeviceSize'
--
-- -   @dataSize@ /must/ be greater than or equal to
--     @accelerationStructureCount@*@stride@
--
-- -   The acceleration structures referenced by @pAccelerationStructures@
--     /must/ be bound to host-visible memory
--
-- -   [[VUID-{refpage}-accelerationStructures-03431]] All acceleration
--     structures in @accelerationStructures@ /must/ have been built with
--     'BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR' if
--     @queryType@ is
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR'
--
-- -   [[VUID-{refpage}-queryType-03432]] @queryType@ /must/ be
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR'
--     or
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_SIZE_KHR'
--
-- -   the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#feature-raytracing-hostascmds ::rayTracingHostAccelerationStructureCommands>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   @pAccelerationStructures@ /must/ be a valid pointer to an array of
--     @accelerationStructureCount@ valid
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR' handles
--
-- -   @queryType@ /must/ be a valid
--     'Vulkan.Core10.Enums.QueryType.QueryType' value
--
-- -   @pData@ /must/ be a valid pointer to an array of @dataSize@ bytes
--
-- -   @accelerationStructureCount@ /must/ be greater than @0@
--
-- -   @dataSize@ /must/ be greater than @0@
--
-- -   Each element of @pAccelerationStructures@ /must/ have been created,
--     allocated, or retrieved from @device@
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.AccelerationStructureKHR',
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Core10.Enums.QueryType.QueryType'
writeAccelerationStructuresPropertiesKHR :: forall io . MonadIO io => Device -> ("accelerationStructures" ::: Vector AccelerationStructureKHR) -> QueryType -> ("dataSize" ::: Word64) -> ("data" ::: Ptr ()) -> ("stride" ::: Word64) -> io ()
writeAccelerationStructuresPropertiesKHR device accelerationStructures queryType dataSize data' stride = liftIO . evalContT $ do
  let vkWriteAccelerationStructuresPropertiesKHR' = mkVkWriteAccelerationStructuresPropertiesKHR (pVkWriteAccelerationStructuresPropertiesKHR (deviceCmds (device :: Device)))
  pPAccelerationStructures <- ContT $ allocaBytesAligned @AccelerationStructureKHR ((Data.Vector.length (accelerationStructures)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPAccelerationStructures `plusPtr` (8 * (i)) :: Ptr AccelerationStructureKHR) (e)) (accelerationStructures)
  r <- lift $ vkWriteAccelerationStructuresPropertiesKHR' (deviceHandle (device)) ((fromIntegral (Data.Vector.length $ (accelerationStructures)) :: Word32)) (pPAccelerationStructures) (queryType) (CSize (dataSize)) (data') (CSize (stride))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdTraceRaysKHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr StridedBufferRegionKHR -> Ptr StridedBufferRegionKHR -> Ptr StridedBufferRegionKHR -> Ptr StridedBufferRegionKHR -> Word32 -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Ptr StridedBufferRegionKHR -> Ptr StridedBufferRegionKHR -> Ptr StridedBufferRegionKHR -> Ptr StridedBufferRegionKHR -> Word32 -> Word32 -> Word32 -> IO ()

-- | vkCmdTraceRaysKHR - Initialize a ray tracing dispatch
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @pRaygenShaderBindingTable@ is the strided buffer range that holds
--     the shader binding table data for the ray generation shader stage.
--
-- -   @pMissShaderBindingTable@ is the strided buffer range that holds the
--     shader binding table data for the miss shader stage.
--
-- -   @pHitShaderBindingTable@ is the strided buffer range that holds the
--     shader binding table data for the hit shader stage.
--
-- -   @pCallableShaderBindingTable@ is the strided buffer range that holds
--     the shader binding table data for the callable shader stage.
--
-- -   @width@ is the width of the ray trace query dimensions.
--
-- -   @height@ is height of the ray trace query dimensions.
--
-- -   @depth@ is depth of the ray trace query dimensions.
--
-- = Description
--
-- When the command is executed, a ray generation group of @width@ ×
-- @height@ × @depth@ rays is assembled.
--
-- == Valid Usage
--
-- -   If a 'Vulkan.Core10.Handles.ImageView' is sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_LINEAR' as a result of this
--     command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   If a 'Vulkan.Core10.Handles.ImageView' is accessed using atomic
--     operations as a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   If a 'Vulkan.Core10.Handles.ImageView' is sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   Any 'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering, as specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   Any 'Vulkan.Core10.Handles.ImageView' being sampled with
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
-- -   Any 'Vulkan.Core10.Handles.Image' created with a
--     'Vulkan.Core10.Image.ImageCreateInfo'::@flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode' of
--     'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'
--
-- -   For each set /n/ that is statically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command, a descriptor set /must/ have been bound to /n/
--     at the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is compatible for set
--     /n/, with the 'Vulkan.Core10.Handles.PipelineLayout' used to create
--     the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   For each push constant that is statically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command, a push constant value /must/ have been set for
--     the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is compatible for push
--     constants, with the 'Vulkan.Core10.Handles.PipelineLayout' used to
--     create the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   Descriptors in each bound descriptor set, specified via
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets', /must/
--     be valid if they are statically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command
--
-- -   A valid pipeline /must/ be bound to the pipeline bind point used by
--     this command
--
-- -   If the 'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline
--     bind point used by this command requires any dynamic state, that
--     state /must/ have been set for @commandBuffer@, and done so after
--     any previously bound pipeline with the corresponding state not
--     specified as dynamic
--
-- -   There /must/ not have been any calls to dynamic state setting
--     commands for any state not specified as dynamic in the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command, since that pipeline was bound
--
-- -   If the 'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline
--     bind point used by this command accesses a
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
-- -   If the 'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline
--     bind point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name, in any shader stage
--
-- -   If the 'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline
--     bind point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values, in any shader stage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a uniform buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a storage buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   If @commandBuffer@ is an unprotected command buffer, any resource
--     accessed by the 'Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command /must/ not be a protected
--     resource
--
-- -   @raygenShaderBindingOffset@ /must/ be less than the size of
--     @raygenShaderBindingTableBuffer@
--
-- -   @raygenShaderBindingOffset@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPropertiesKHR'::@shaderGroupBaseAlignment@
--
-- -   @missShaderBindingOffset@ /must/ be less than the size of
--     @missShaderBindingTableBuffer@
--
-- -   @missShaderBindingOffset@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPropertiesKHR'::@shaderGroupBaseAlignment@
--
-- -   @hitShaderBindingOffset@ /must/ be less than the size of
--     @hitShaderBindingTableBuffer@
--
-- -   @hitShaderBindingOffset@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPropertiesKHR'::@shaderGroupBaseAlignment@
--
-- -   @callableShaderBindingOffset@ /must/ be less than the size of
--     @callableShaderBindingTableBuffer@
--
-- -   @callableShaderBindingOffset@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPropertiesKHR'::@shaderGroupBaseAlignment@
--
-- -   @missShaderBindingStride@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPropertiesKHR'::@shaderGroupHandleSize@
--
-- -   @hitShaderBindingStride@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPropertiesKHR'::@shaderGroupHandleSize@
--
-- -   @callableShaderBindingStride@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPropertiesKHR'::@shaderGroupHandleSize@
--
-- -   @missShaderBindingStride@ /must/ be less than or equal to
--     'PhysicalDeviceRayTracingPropertiesKHR'::@maxShaderGroupStride@
--
-- -   @hitShaderBindingStride@ /must/ be less than or equal to
--     'PhysicalDeviceRayTracingPropertiesKHR'::@maxShaderGroupStride@
--
-- -   @callableShaderBindingStride@ /must/ be less than or equal to
--     'PhysicalDeviceRayTracingPropertiesKHR'::@maxShaderGroupStride@
--
-- -   Any shader group handle referenced by this call /must/ have been
--     queried from the currently bound ray tracing shader pipeline
--
-- -   This command /must/ not cause a shader call instruction to be
--     executed from a shader invocation with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#ray-tracing-recursion-depth recursion depth>
--     greater than the value of @maxRecursionDepth@ used to create the
--     bound ray tracing pipeline
--
-- -   If @commandBuffer@ is a protected command buffer, any resource
--     written to by the 'Vulkan.Core10.Handles.Pipeline' object bound to
--     the pipeline bind point used by this command /must/ not be an
--     unprotected resource
--
-- -   If @commandBuffer@ is a protected command buffer, pipeline stages
--     other than the framebuffer-space and compute stages in the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point /must/ not write to any resource
--
-- -   @width@ /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[0]
--
-- -   @height@ /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[1]
--
-- -   @depth@ /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[2]
--
-- -   If the currently bound ray tracing pipeline was created with @flags@
--     that included
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR',
--     the @buffer@ member of @hitShaderBindingTable@ /must/ not be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   If the currently bound ray tracing pipeline was created with @flags@
--     that included
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR',
--     the @buffer@ member of @hitShaderBindingTable@ /must/ not be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   If the currently bound ray tracing pipeline was created with @flags@
--     that included
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR',
--     the @buffer@ member of @hitShaderBindingTable@ /must/ not be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   If the currently bound ray tracing pipeline was created with @flags@
--     that included
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR',
--     the shader group handle identified by @missShaderBindingTable@
--     /must/ contain a valid miss shader
--
-- -   If the currently bound ray tracing pipeline was created with @flags@
--     that included
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR',
--     entries in @hitShaderBindingTable@ accessed as a result of this
--     command in order to execute an any hit shader /must/ not be set to
--     zero
--
-- -   If the currently bound ray tracing pipeline was created with @flags@
--     that included
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR',
--     entries in @hitShaderBindingTable@ accessed as a result of this
--     command in order to execute a closest hit shader /must/ not be set
--     to zero
--
-- -   If the currently bound ray tracing pipeline was created with @flags@
--     that included
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR',
--     entries in @hitShaderBindingTable@ accessed as a result of this
--     command in order to execute an intersection shader /must/ not be set
--     to zero
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @pRaygenShaderBindingTable@ /must/ be a valid pointer to a valid
--     'StridedBufferRegionKHR' structure
--
-- -   @pMissShaderBindingTable@ /must/ be a valid pointer to a valid
--     'StridedBufferRegionKHR' structure
--
-- -   @pHitShaderBindingTable@ /must/ be a valid pointer to a valid
--     'StridedBufferRegionKHR' structure
--
-- -   @pCallableShaderBindingTable@ /must/ be a valid pointer to a valid
--     'StridedBufferRegionKHR' structure
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Compute                                                                                                               |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer', 'StridedBufferRegionKHR'
cmdTraceRaysKHR :: forall io . MonadIO io => CommandBuffer -> ("raygenShaderBindingTable" ::: StridedBufferRegionKHR) -> ("missShaderBindingTable" ::: StridedBufferRegionKHR) -> ("hitShaderBindingTable" ::: StridedBufferRegionKHR) -> ("callableShaderBindingTable" ::: StridedBufferRegionKHR) -> ("width" ::: Word32) -> ("height" ::: Word32) -> ("depth" ::: Word32) -> io ()
cmdTraceRaysKHR commandBuffer raygenShaderBindingTable missShaderBindingTable hitShaderBindingTable callableShaderBindingTable width height depth = liftIO . evalContT $ do
  let vkCmdTraceRaysKHR' = mkVkCmdTraceRaysKHR (pVkCmdTraceRaysKHR (deviceCmds (commandBuffer :: CommandBuffer)))
  pRaygenShaderBindingTable <- ContT $ withCStruct (raygenShaderBindingTable)
  pMissShaderBindingTable <- ContT $ withCStruct (missShaderBindingTable)
  pHitShaderBindingTable <- ContT $ withCStruct (hitShaderBindingTable)
  pCallableShaderBindingTable <- ContT $ withCStruct (callableShaderBindingTable)
  lift $ vkCmdTraceRaysKHR' (commandBufferHandle (commandBuffer)) pRaygenShaderBindingTable pMissShaderBindingTable pHitShaderBindingTable pCallableShaderBindingTable (width) (height) (depth)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetRayTracingShaderGroupHandlesKHR
  :: FunPtr (Ptr Device_T -> Pipeline -> Word32 -> Word32 -> CSize -> Ptr () -> IO Result) -> Ptr Device_T -> Pipeline -> Word32 -> Word32 -> CSize -> Ptr () -> IO Result

-- | vkGetRayTracingShaderGroupHandlesKHR - Query ray tracing pipeline shader
-- group handles
--
-- = Parameters
--
-- -   @device@ is the logical device containing the ray tracing pipeline.
--
-- -   @pipeline@ is the ray tracing pipeline object containing the
--     shaders.
--
-- -   @firstGroup@ is the index of the first group to retrieve a handle
--     for from the 'RayTracingPipelineCreateInfoKHR'::@pGroups@ or
--     'Vulkan.Extensions.VK_NV_ray_tracing.RayTracingPipelineCreateInfoNV'::@pGroups@
--     array.
--
-- -   @groupCount@ is the number of shader handles to retrieve.
--
-- -   @dataSize@ is the size in bytes of the buffer pointed to by @pData@.
--
-- -   @pData@ is a pointer to a user-allocated buffer where the results
--     will be written.
--
-- == Valid Usage
--
-- -   The sum of @firstGroup@ and @groupCount@ /must/ be less than the
--     number of shader groups in @pipeline@
--
-- -   @dataSize@ /must/ be at least
--     'PhysicalDeviceRayTracingPropertiesKHR'::@shaderGroupHandleSize@ ×
--     @groupCount@
--
-- -   @pipeline@ /must/ have not been created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_LIBRARY_BIT_KHR'
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   @pipeline@ /must/ be a valid 'Vulkan.Core10.Handles.Pipeline' handle
--
-- -   @pData@ /must/ be a valid pointer to an array of @dataSize@ bytes
--
-- -   @dataSize@ /must/ be greater than @0@
--
-- -   @pipeline@ /must/ have been created, allocated, or retrieved from
--     @device@
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Pipeline'
getRayTracingShaderGroupHandlesKHR :: forall io . MonadIO io => Device -> Pipeline -> ("firstGroup" ::: Word32) -> ("groupCount" ::: Word32) -> ("dataSize" ::: Word64) -> ("data" ::: Ptr ()) -> io ()
getRayTracingShaderGroupHandlesKHR device pipeline firstGroup groupCount dataSize data' = liftIO $ do
  let vkGetRayTracingShaderGroupHandlesKHR' = mkVkGetRayTracingShaderGroupHandlesKHR (pVkGetRayTracingShaderGroupHandlesKHR (deviceCmds (device :: Device)))
  r <- vkGetRayTracingShaderGroupHandlesKHR' (deviceHandle (device)) (pipeline) (firstGroup) (groupCount) (CSize (dataSize)) (data')
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetRayTracingCaptureReplayShaderGroupHandlesKHR
  :: FunPtr (Ptr Device_T -> Pipeline -> Word32 -> Word32 -> CSize -> Ptr () -> IO Result) -> Ptr Device_T -> Pipeline -> Word32 -> Word32 -> CSize -> Ptr () -> IO Result

-- | vkGetRayTracingCaptureReplayShaderGroupHandlesKHR - Query ray tracing
-- capture replay pipeline shader group handles
--
-- = Parameters
--
-- -   @device@ is the logical device containing the ray tracing pipeline.
--
-- -   @pipeline@ is the ray tracing pipeline object containing the
--     shaders.
--
-- -   @firstGroup@ is the index of the first group to retrieve a handle
--     for from the 'RayTracingPipelineCreateInfoKHR'::@pGroups@ array.
--
-- -   @groupCount@ is the number of shader handles to retrieve.
--
-- -   @dataSize@ is the size in bytes of the buffer pointed to by @pData@.
--
-- -   @pData@ is a pointer to a user-allocated buffer where the results
--     will be written.
--
-- == Valid Usage
--
-- -   The sum of @firstGroup@ and @groupCount@ /must/ be less than the
--     number of shader groups in @pipeline@
--
-- -   @dataSize@ /must/ be at least
--     'PhysicalDeviceRayTracingPropertiesKHR'::@shaderGroupHandleCaptureReplaySize@
--     × @groupCount@
--
-- -   'PhysicalDeviceRayTracingFeaturesKHR'::@rayTracingShaderGroupHandleCaptureReplay@
--     /must/ be enabled to call this function
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   @pipeline@ /must/ be a valid 'Vulkan.Core10.Handles.Pipeline' handle
--
-- -   @pData@ /must/ be a valid pointer to an array of @dataSize@ bytes
--
-- -   @dataSize@ /must/ be greater than @0@
--
-- -   @pipeline@ /must/ have been created, allocated, or retrieved from
--     @device@
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Pipeline'
getRayTracingCaptureReplayShaderGroupHandlesKHR :: forall io . MonadIO io => Device -> Pipeline -> ("firstGroup" ::: Word32) -> ("groupCount" ::: Word32) -> ("dataSize" ::: Word64) -> ("data" ::: Ptr ()) -> io ()
getRayTracingCaptureReplayShaderGroupHandlesKHR device pipeline firstGroup groupCount dataSize data' = liftIO $ do
  let vkGetRayTracingCaptureReplayShaderGroupHandlesKHR' = mkVkGetRayTracingCaptureReplayShaderGroupHandlesKHR (pVkGetRayTracingCaptureReplayShaderGroupHandlesKHR (deviceCmds (device :: Device)))
  r <- vkGetRayTracingCaptureReplayShaderGroupHandlesKHR' (deviceHandle (device)) (pipeline) (firstGroup) (groupCount) (CSize (dataSize)) (data')
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateRayTracingPipelinesKHR
  :: FunPtr (Ptr Device_T -> PipelineCache -> Word32 -> Ptr (RayTracingPipelineCreateInfoKHR a) -> Ptr AllocationCallbacks -> Ptr Pipeline -> IO Result) -> Ptr Device_T -> PipelineCache -> Word32 -> Ptr (RayTracingPipelineCreateInfoKHR a) -> Ptr AllocationCallbacks -> Ptr Pipeline -> IO Result

-- | vkCreateRayTracingPipelinesKHR - Creates a new ray tracing pipeline
-- object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the ray tracing
--     pipelines.
--
-- -   @pipelineCache@ is either 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     indicating that pipeline caching is disabled, or the handle of a
--     valid
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#pipelines-cache pipeline cache>
--     object, in which case use of that cache is enabled for the duration
--     of the command.
--
-- -   @createInfoCount@ is the length of the @pCreateInfos@ and
--     @pPipelines@ arrays.
--
-- -   @pCreateInfos@ is a pointer to an array of
--     'RayTracingPipelineCreateInfoKHR' structures.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pPipelines@ is a pointer to an array in which the resulting ray
--     tracing pipeline objects are returned.
--
-- = Description
--
-- The 'Vulkan.Core10.Enums.Result.ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS'
-- error is returned if the implementation is unable to re-use the shader
-- group handles provided in
-- 'RayTracingShaderGroupCreateInfoKHR'::@pShaderGroupCaptureReplayHandle@
-- when
-- 'PhysicalDeviceRayTracingFeaturesKHR'::@rayTracingShaderGroupHandleCaptureReplay@
-- is enabled.
--
-- == Valid Usage
--
-- -   If the @flags@ member of any element of @pCreateInfos@ contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and the @basePipelineIndex@ member of that same element is not
--     @-1@, @basePipelineIndex@ /must/ be less than the index into
--     @pCreateInfos@ that corresponds to that element
--
-- -   If the @flags@ member of any element of @pCreateInfos@ contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, the base pipeline /must/ have been created with the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT'
--     flag set
--
-- -   If @pipelineCache@ was created with
--     'Vulkan.Core10.Enums.PipelineCacheCreateFlagBits.PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT_EXT',
--     host access to @pipelineCache@ /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-threadingbehavior externally synchronized>
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-raytracing rayTracing>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   If @pipelineCache@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @pipelineCache@ /must/ be a valid
--     'Vulkan.Core10.Handles.PipelineCache' handle
--
-- -   @pCreateInfos@ /must/ be a valid pointer to an array of
--     @createInfoCount@ valid 'RayTracingPipelineCreateInfoKHR' structures
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   @pPipelines@ /must/ be a valid pointer to an array of
--     @createInfoCount@ 'Vulkan.Core10.Handles.Pipeline' handles
--
-- -   @createInfoCount@ /must/ be greater than @0@
--
-- -   If @pipelineCache@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.OPERATION_DEFERRED_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.OPERATION_NOT_DEFERRED_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.PIPELINE_COMPILE_REQUIRED_EXT'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS'
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Pipeline',
-- 'Vulkan.Core10.Handles.PipelineCache', 'RayTracingPipelineCreateInfoKHR'
createRayTracingPipelinesKHR :: forall a io . (PokeChain a, MonadIO io) => Device -> PipelineCache -> ("createInfos" ::: Vector (RayTracingPipelineCreateInfoKHR a)) -> ("allocator" ::: Maybe AllocationCallbacks) -> io (Result, ("pipelines" ::: Vector Pipeline))
createRayTracingPipelinesKHR device pipelineCache createInfos allocator = liftIO . evalContT $ do
  let vkCreateRayTracingPipelinesKHR' = mkVkCreateRayTracingPipelinesKHR (pVkCreateRayTracingPipelinesKHR (deviceCmds (device :: Device)))
  pPCreateInfos <- ContT $ allocaBytesAligned @(RayTracingPipelineCreateInfoKHR _) ((Data.Vector.length (createInfos)) * 120) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPCreateInfos `plusPtr` (120 * (i)) :: Ptr (RayTracingPipelineCreateInfoKHR _)) (e) . ($ ())) (createInfos)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPPipelines <- ContT $ bracket (callocBytes @Pipeline ((fromIntegral ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32))) * 8)) free
  r <- lift $ vkCreateRayTracingPipelinesKHR' (deviceHandle (device)) (pipelineCache) ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32)) (pPCreateInfos) pAllocator (pPPipelines)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPipelines <- lift $ generateM (fromIntegral ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32))) (\i -> peek @Pipeline ((pPPipelines `advancePtrBytes` (8 * (i)) :: Ptr Pipeline)))
  pure $ (r, pPipelines)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdTraceRaysIndirectKHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr StridedBufferRegionKHR -> Ptr StridedBufferRegionKHR -> Ptr StridedBufferRegionKHR -> Ptr StridedBufferRegionKHR -> Buffer -> DeviceSize -> IO ()) -> Ptr CommandBuffer_T -> Ptr StridedBufferRegionKHR -> Ptr StridedBufferRegionKHR -> Ptr StridedBufferRegionKHR -> Ptr StridedBufferRegionKHR -> Buffer -> DeviceSize -> IO ()

-- | vkCmdTraceRaysIndirectKHR - Initialize an indirect ray tracing dispatch
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @pRaygenShaderBindingTable@ is the strided buffer range that holds
--     the shader binding table data for the ray generation shader stage.
--
-- -   @pMissShaderBindingTable@ is the strided buffer range that holds the
--     shader binding table data for the miss shader stage.
--
-- -   @pHitShaderBindingTable@ is the strided buffer range that holds the
--     shader binding table data for the hit shader stage.
--
-- -   @pCallableShaderBindingTable@ is the strided buffer range that holds
--     the shader binding table data for the callable shader stage.
--
-- -   @buffer@ is the buffer containing the trace ray parameters.
--
-- -   @offset@ is the byte offset into @buffer@ where parameters begin.
--
-- = Description
--
-- 'cmdTraceRaysIndirectKHR' behaves similarly to 'cmdTraceRaysKHR' except
-- that the ray trace query dimensions are read by the device from a buffer
-- during execution. The parameters of trace ray are encoded in the
-- 'TraceRaysIndirectCommandKHR' structure.
--
-- == Valid Usage
--
-- -   If a 'Vulkan.Core10.Handles.ImageView' is sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_LINEAR' as a result of this
--     command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   If a 'Vulkan.Core10.Handles.ImageView' is accessed using atomic
--     operations as a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   If a 'Vulkan.Core10.Handles.ImageView' is sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   Any 'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering, as specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   Any 'Vulkan.Core10.Handles.ImageView' being sampled with
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
-- -   Any 'Vulkan.Core10.Handles.Image' created with a
--     'Vulkan.Core10.Image.ImageCreateInfo'::@flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode' of
--     'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'
--
-- -   For each set /n/ that is statically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command, a descriptor set /must/ have been bound to /n/
--     at the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is compatible for set
--     /n/, with the 'Vulkan.Core10.Handles.PipelineLayout' used to create
--     the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   For each push constant that is statically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command, a push constant value /must/ have been set for
--     the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is compatible for push
--     constants, with the 'Vulkan.Core10.Handles.PipelineLayout' used to
--     create the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   Descriptors in each bound descriptor set, specified via
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets', /must/
--     be valid if they are statically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command
--
-- -   A valid pipeline /must/ be bound to the pipeline bind point used by
--     this command
--
-- -   If the 'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline
--     bind point used by this command requires any dynamic state, that
--     state /must/ have been set for @commandBuffer@, and done so after
--     any previously bound pipeline with the corresponding state not
--     specified as dynamic
--
-- -   There /must/ not have been any calls to dynamic state setting
--     commands for any state not specified as dynamic in the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command, since that pipeline was bound
--
-- -   If the 'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline
--     bind point used by this command accesses a
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
-- -   If the 'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline
--     bind point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name, in any shader stage
--
-- -   If the 'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline
--     bind point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values, in any shader stage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a uniform buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a storage buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   If @commandBuffer@ is an unprotected command buffer, any resource
--     accessed by the 'Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command /must/ not be a protected
--     resource
--
-- -   @raygenShaderBindingOffset@ /must/ be less than the size of
--     @raygenShaderBindingTableBuffer@
--
-- -   @raygenShaderBindingOffset@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPropertiesKHR'::@shaderGroupBaseAlignment@
--
-- -   @missShaderBindingOffset@ /must/ be less than the size of
--     @missShaderBindingTableBuffer@
--
-- -   @missShaderBindingOffset@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPropertiesKHR'::@shaderGroupBaseAlignment@
--
-- -   @hitShaderBindingOffset@ /must/ be less than the size of
--     @hitShaderBindingTableBuffer@
--
-- -   @hitShaderBindingOffset@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPropertiesKHR'::@shaderGroupBaseAlignment@
--
-- -   @callableShaderBindingOffset@ /must/ be less than the size of
--     @callableShaderBindingTableBuffer@
--
-- -   @callableShaderBindingOffset@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPropertiesKHR'::@shaderGroupBaseAlignment@
--
-- -   @missShaderBindingStride@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPropertiesKHR'::@shaderGroupHandleSize@
--
-- -   @hitShaderBindingStride@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPropertiesKHR'::@shaderGroupHandleSize@
--
-- -   @callableShaderBindingStride@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPropertiesKHR'::@shaderGroupHandleSize@
--
-- -   @missShaderBindingStride@ /must/ be less than or equal to
--     'PhysicalDeviceRayTracingPropertiesKHR'::@maxShaderGroupStride@
--
-- -   @hitShaderBindingStride@ /must/ be less than or equal to
--     'PhysicalDeviceRayTracingPropertiesKHR'::@maxShaderGroupStride@
--
-- -   @callableShaderBindingStride@ /must/ be less than or equal to
--     'PhysicalDeviceRayTracingPropertiesKHR'::@maxShaderGroupStride@
--
-- -   Any shader group handle referenced by this call /must/ have been
--     queried from the currently bound ray tracing shader pipeline
--
-- -   This command /must/ not cause a shader call instruction to be
--     executed from a shader invocation with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#ray-tracing-recursion-depth recursion depth>
--     greater than the value of @maxRecursionDepth@ used to create the
--     bound ray tracing pipeline
--
-- -   If @buffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   @buffer@ /must/ have been created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   @offset@ /must/ be a multiple of @4@
--
-- -   @commandBuffer@ /must/ not be a protected command buffer
--
-- -   (@offset@ + @sizeof@('TraceRaysIndirectCommandKHR')) /must/ be less
--     than or equal to the size of @buffer@
--
-- -   the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-raytracing-indirecttraceray ::rayTracingIndirectTraceRays>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @pRaygenShaderBindingTable@ /must/ be a valid pointer to a valid
--     'StridedBufferRegionKHR' structure
--
-- -   @pMissShaderBindingTable@ /must/ be a valid pointer to a valid
--     'StridedBufferRegionKHR' structure
--
-- -   @pHitShaderBindingTable@ /must/ be a valid pointer to a valid
--     'StridedBufferRegionKHR' structure
--
-- -   @pCallableShaderBindingTable@ /must/ be a valid pointer to a valid
--     'StridedBufferRegionKHR' structure
--
-- -   @buffer@ /must/ be a valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   Both of @buffer@, and @commandBuffer@ /must/ have been created,
--     allocated, or retrieved from the same 'Vulkan.Core10.Handles.Device'
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Compute                                                                                                               |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Buffer', 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.BaseType.DeviceSize', 'StridedBufferRegionKHR'
cmdTraceRaysIndirectKHR :: forall io . MonadIO io => CommandBuffer -> ("raygenShaderBindingTable" ::: StridedBufferRegionKHR) -> ("missShaderBindingTable" ::: StridedBufferRegionKHR) -> ("hitShaderBindingTable" ::: StridedBufferRegionKHR) -> ("callableShaderBindingTable" ::: StridedBufferRegionKHR) -> Buffer -> ("offset" ::: DeviceSize) -> io ()
cmdTraceRaysIndirectKHR commandBuffer raygenShaderBindingTable missShaderBindingTable hitShaderBindingTable callableShaderBindingTable buffer offset = liftIO . evalContT $ do
  let vkCmdTraceRaysIndirectKHR' = mkVkCmdTraceRaysIndirectKHR (pVkCmdTraceRaysIndirectKHR (deviceCmds (commandBuffer :: CommandBuffer)))
  pRaygenShaderBindingTable <- ContT $ withCStruct (raygenShaderBindingTable)
  pMissShaderBindingTable <- ContT $ withCStruct (missShaderBindingTable)
  pHitShaderBindingTable <- ContT $ withCStruct (hitShaderBindingTable)
  pCallableShaderBindingTable <- ContT $ withCStruct (callableShaderBindingTable)
  lift $ vkCmdTraceRaysIndirectKHR' (commandBufferHandle (commandBuffer)) pRaygenShaderBindingTable pMissShaderBindingTable pHitShaderBindingTable pCallableShaderBindingTable (buffer) (offset)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceAccelerationStructureCompatibilityKHR
  :: FunPtr (Ptr Device_T -> Ptr AccelerationStructureVersionKHR -> IO Result) -> Ptr Device_T -> Ptr AccelerationStructureVersionKHR -> IO Result

-- | vkGetDeviceAccelerationStructureCompatibilityKHR - Check if a serialized
-- acceleration structure is compatible with the current device
--
-- = Parameters
--
-- -   @device@ is the device to check the version against.
--
-- -   @version@ points to the 'AccelerationStructureVersionKHR' version
--     information to check against the device.
--
-- = Description
--
-- This possible return values for
-- 'getDeviceAccelerationStructureCompatibilityKHR' are:
--
-- -   'Vulkan.Core10.Enums.Result.SUCCESS' is returned if an acceleration
--     structure serialized with @version@ as the version information is
--     compatible with @device@.
--
-- -   'Vulkan.Core10.Enums.Result.ERROR_INCOMPATIBLE_VERSION_KHR' is
--     returned if an acceleration structure serialized with @version@ as
--     the version information is not compatible with @device@.
--
-- == Valid Usage
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-raytracing rayTracing>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-rayQuery rayQuery>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   @version@ /must/ be a valid pointer to a valid
--     'AccelerationStructureVersionKHR' structure
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INCOMPATIBLE_VERSION_KHR'
--
-- = See Also
--
-- 'AccelerationStructureVersionKHR', 'Vulkan.Core10.Handles.Device'
getDeviceAccelerationStructureCompatibilityKHR :: forall io . MonadIO io => Device -> AccelerationStructureVersionKHR -> io ()
getDeviceAccelerationStructureCompatibilityKHR device version = liftIO . evalContT $ do
  let vkGetDeviceAccelerationStructureCompatibilityKHR' = mkVkGetDeviceAccelerationStructureCompatibilityKHR (pVkGetDeviceAccelerationStructureCompatibilityKHR (deviceCmds (device :: Device)))
  version' <- ContT $ withCStruct (version)
  r <- lift $ vkGetDeviceAccelerationStructureCompatibilityKHR' (deviceHandle (device)) version'
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateAccelerationStructureKHR
  :: FunPtr (Ptr Device_T -> Ptr AccelerationStructureCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr AccelerationStructureKHR -> IO Result) -> Ptr Device_T -> Ptr AccelerationStructureCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr AccelerationStructureKHR -> IO Result

-- | vkCreateAccelerationStructureKHR - Create a new acceleration structure
-- object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the buffer object.
--
-- -   @pCreateInfo@ is a pointer to a 'AccelerationStructureCreateInfoKHR'
--     structure containing parameters affecting creation of the
--     acceleration structure.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pAccelerationStructure@ is a pointer to a
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR' handle in which
--     the resulting acceleration structure object is returned.
--
-- = Description
--
-- Similar to other objects in Vulkan, the acceleration structure creation
-- merely creates an object with a specific “shape”. The type and quantity
-- of geometry that can be built into an acceleration structure is
-- determined by the parameters of 'AccelerationStructureCreateInfoKHR'.
--
-- Populating the data in the object after allocating and binding memory is
-- done with commands such as 'cmdBuildAccelerationStructureKHR',
-- 'buildAccelerationStructureKHR', 'cmdCopyAccelerationStructureKHR', and
-- 'copyAccelerationStructureKHR'.
--
-- == Valid Usage
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-raytracing rayTracing>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-rayQuery rayQuery>
--     feature /must/ be enabled
--
-- -   If 'AccelerationStructureCreateInfoKHR'::@deviceAddress@ is not
--     zero, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-raytracing-ascapturereplay rayTracingAccelerationStructureCaptureReplay>
--     feature /must/ be enabled
--
-- -   If @device@ was created with multiple physical devices, then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-bufferDeviceAddressMultiDevice bufferDeviceAddressMultiDevice>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'AccelerationStructureCreateInfoKHR' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   @pAccelerationStructure@ /must/ be a valid pointer to a
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR' handle
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
--     -   'Vulkan.Extensions.VK_KHR_buffer_device_address.ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS_KHR'
--
-- = See Also
--
-- 'AccelerationStructureCreateInfoKHR',
-- 'Vulkan.Extensions.Handles.AccelerationStructureKHR',
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device'
createAccelerationStructureKHR :: forall io . MonadIO io => Device -> AccelerationStructureCreateInfoKHR -> ("allocator" ::: Maybe AllocationCallbacks) -> io (AccelerationStructureKHR)
createAccelerationStructureKHR device createInfo allocator = liftIO . evalContT $ do
  let vkCreateAccelerationStructureKHR' = mkVkCreateAccelerationStructureKHR (pVkCreateAccelerationStructureKHR (deviceCmds (device :: Device)))
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPAccelerationStructure <- ContT $ bracket (callocBytes @AccelerationStructureKHR 8) free
  r <- lift $ vkCreateAccelerationStructureKHR' (deviceHandle (device)) pCreateInfo pAllocator (pPAccelerationStructure)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pAccelerationStructure <- lift $ peek @AccelerationStructureKHR pPAccelerationStructure
  pure $ (pAccelerationStructure)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createAccelerationStructureKHR' and 'destroyAccelerationStructureKHR'
--
-- To ensure that 'destroyAccelerationStructureKHR' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the first argument.
-- To just extract the pair pass '(,)' as the first argument.
--
withAccelerationStructureKHR :: forall io r . MonadIO io => (io (AccelerationStructureKHR) -> ((AccelerationStructureKHR) -> io ()) -> r) -> Device -> AccelerationStructureCreateInfoKHR -> Maybe AllocationCallbacks -> r
withAccelerationStructureKHR b device pCreateInfo pAllocator =
  b (createAccelerationStructureKHR device pCreateInfo pAllocator)
    (\(o0) -> destroyAccelerationStructureKHR device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBuildAccelerationStructureKHR
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr (AccelerationStructureBuildGeometryInfoKHR a) -> Ptr (Ptr AccelerationStructureBuildOffsetInfoKHR) -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr (AccelerationStructureBuildGeometryInfoKHR a) -> Ptr (Ptr AccelerationStructureBuildOffsetInfoKHR) -> IO ()

-- | vkCmdBuildAccelerationStructureKHR - Build an acceleration structure
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @infoCount@ is the number of acceleration structures to build. It
--     specifies the number of the @pInfos@ structures and @ppOffsetInfos@
--     pointers that /must/ be provided.
--
-- -   @pInfos@ is an array of @infoCount@
--     'AccelerationStructureBuildGeometryInfoKHR' structures defining the
--     geometry used to build each acceleration structure.
--
-- -   @ppOffsetInfos@ is an array of @infoCount@ pointers to arrays of
--     'AccelerationStructureBuildOffsetInfoKHR' structures. Each
--     @ppOffsetInfos@[i] is an array of @pInfos@[i].@geometryCount@
--     'AccelerationStructureBuildOffsetInfoKHR' structures defining
--     dynamic offsets to the addresses where geometry data is stored, as
--     defined by @pInfos@[i].
--
-- = Description
--
-- The 'cmdBuildAccelerationStructureKHR' command provides the ability to
-- initiate multiple acceleration structures builds, however there is no
-- ordering or synchronization implied between any of the individual
-- acceleration structure builds.
--
-- Note
--
-- This means that an application /cannot/ build a top-level acceleration
-- structure in the same 'cmdBuildAccelerationStructureKHR' call as the
-- associated bottom-level or instance acceleration structures are being
-- built. There also /cannot/ be any memory aliasing between any
-- acceleration structure memories or scratch memories being used by any of
-- the builds.
--
-- == Valid Usage
--
-- -   [[VUID-{refpage}-pOffsetInfos-03402]] Each element of
--     @ppOffsetInfos@[i] /must/ be a valid pointer to an array of
--     @pInfos@[i].@geometryCount@
--     'AccelerationStructureBuildOffsetInfoKHR' structures
--
-- -   [[VUID-{refpage}-pInfos-03403]] Each
--     @pInfos@[i].@srcAccelerationStructure@ /must/ not refer to the same
--     acceleration structure as any @pInfos@[i].@dstAccelerationStructure@
--     that is provided to the same build command unless it is identical
--     for an update
--
-- -   [[VUID-{refpage}-pInfos-03404]] For each @pInfos@[i],
--     @dstAccelerationStructure@ /must/ have been created with compatible
--     'AccelerationStructureCreateInfoKHR' where
--     'AccelerationStructureCreateInfoKHR'::@type@ and
--     'AccelerationStructureCreateInfoKHR'::@flags@ are identical to
--     'AccelerationStructureBuildGeometryInfoKHR'::@type@ and
--     'AccelerationStructureBuildGeometryInfoKHR'::@flags@ respectively,
--     'AccelerationStructureBuildGeometryInfoKHR'::@geometryCount@ for
--     @dstAccelerationStructure@ are greater than or equal to the build
--     size, and each geometry in
--     'AccelerationStructureBuildGeometryInfoKHR'::@ppGeometries@ for
--     @dstAccelerationStructure@ has greater than or equal to the number
--     of vertices, indices, and AABBs,
--     'AccelerationStructureGeometryTrianglesDataKHR'::@transformData@ is
--     both 0 or both non-zero, and all other parameters are the same
--
-- -   [[VUID-{refpage}-pInfos-03405]] For each @pInfos@[i], if @update@ is
--     'Vulkan.Core10.BaseType.TRUE', then objects that were previously
--     active for that acceleration structure /must/ not be made inactive
--     as per
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#acceleration-structure-inactive-prims ???>
--
-- -   [[VUID-{refpage}-pInfos-03406]] For each @pInfos@[i], if @update@ is
--     'Vulkan.Core10.BaseType.TRUE', then objects that were previously
--     inactive for that acceleration structure /must/ not be made active
--     as per
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#acceleration-structure-inactive-prims ???>
--
-- -   [[VUID-{refpage}-None-03407]] Any acceleration structure instance in
--     any top level build in this command /must/ not reference any bottom
--     level acceleration structure built by this command
--
-- -   [[VUID-{refpage}-pInfos-03408]] There /must/ not be any
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-memory-aliasing memory aliasing>
--     between the scratch memories that are provided in all the
--     @pInfos@[i].@scratchData@ memories for the acceleration structure
--     builds
--
-- -   [[VUID-{refpage}-None-03409]] There /must/ not be any
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-memory-aliasing memory aliasing>
--     between memory bound to any top level, bottom level, or instance
--     acceleration structure accessed by this command
--
-- -   If @update@ is 'Vulkan.Core10.BaseType.FALSE', all addresses between
--     @pInfos@[i].@scratchData@ and @pInfos@[i].@scratchData@ + N - 1
--     /must/ be in the buffer device address range of the same buffer,
--     where N is given by the @size@ member of the
--     'Vulkan.Core10.MemoryManagement.MemoryRequirements' structure
--     returned from a call to
--     'getAccelerationStructureMemoryRequirementsKHR' with
--     'AccelerationStructureMemoryRequirementsInfoKHR'::@accelerationStructure@
--     set to @pInfos@[i].@dstAccelerationStructure@ and
--     'AccelerationStructureMemoryRequirementsInfoKHR'::@type@ set to
--     'ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_KHR'
--
-- -   If @update@ is 'Vulkan.Core10.BaseType.TRUE', all addresses between
--     @pInfos@[i].@scratchData@ and @pInfos@[i].@scratchData@ + N - 1
--     /must/ be in the buffer device address range of the same buffer,
--     where N is given by the @size@ member of the
--     'Vulkan.Core10.MemoryManagement.MemoryRequirements' structure
--     returned from a call to
--     'getAccelerationStructureMemoryRequirementsKHR' with
--     'AccelerationStructureMemoryRequirementsInfoKHR'::@accelerationStructure@
--     set to @pInfos@[i].@dstAccelerationStructure@ and
--     'AccelerationStructureMemoryRequirementsInfoKHR'::@type@ set to
--     'ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_KHR'
--
-- -   The buffer from which the buffer device address
--     @pInfos@[i].@scratchData@ is queried /must/ have been created with
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_RAY_TRACING_BIT_KHR'
--     usage flag
--
-- -   All 'DeviceOrHostAddressKHR' or 'DeviceOrHostAddressConstKHR'
--     referenced by this command /must/ contain valid device addresses
--
-- -   All 'Vulkan.Extensions.Handles.AccelerationStructureKHR' objects
--     referenced by this command /must/ be bound to device memory
--
-- -   The
--     'Vulkan.Extensions.VK_KHR_deferred_host_operations.DeferredOperationInfoKHR'
--     structure /must/ not be included in the @pNext@ chain of any of the
--     provided 'AccelerationStructureBuildGeometryInfoKHR' structures
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @pInfos@ /must/ be a valid pointer to an array of @infoCount@ valid
--     'AccelerationStructureBuildGeometryInfoKHR' structures
--
-- -   @ppOffsetInfos@ /must/ be a valid pointer to an array of @infoCount@
--     'AccelerationStructureBuildOffsetInfoKHR' structures
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   @infoCount@ /must/ be greater than @0@
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Compute                                                                                                               |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'AccelerationStructureBuildGeometryInfoKHR',
-- 'AccelerationStructureBuildOffsetInfoKHR',
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdBuildAccelerationStructureKHR :: forall a io . (PokeChain a, MonadIO io) => CommandBuffer -> ("infos" ::: Vector (AccelerationStructureBuildGeometryInfoKHR a)) -> ("offsetInfos" ::: Vector AccelerationStructureBuildOffsetInfoKHR) -> io ()
cmdBuildAccelerationStructureKHR commandBuffer infos offsetInfos = liftIO . evalContT $ do
  let vkCmdBuildAccelerationStructureKHR' = mkVkCmdBuildAccelerationStructureKHR (pVkCmdBuildAccelerationStructureKHR (deviceCmds (commandBuffer :: CommandBuffer)))
  let pInfosLength = Data.Vector.length $ (infos)
  let ppOffsetInfosLength = Data.Vector.length $ (offsetInfos)
  lift $ unless (ppOffsetInfosLength == pInfosLength) $
    throwIO $ IOError Nothing InvalidArgument "" "ppOffsetInfos and pInfos must have the same length" Nothing Nothing
  pPInfos <- ContT $ allocaBytesAligned @(AccelerationStructureBuildGeometryInfoKHR _) ((Data.Vector.length (infos)) * 72) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPInfos `plusPtr` (72 * (i)) :: Ptr (AccelerationStructureBuildGeometryInfoKHR _)) (e) . ($ ())) (infos)
  pPpOffsetInfos <- ContT $ allocaBytesAligned @(Ptr AccelerationStructureBuildOffsetInfoKHR) ((Data.Vector.length (offsetInfos)) * 8) 8
  Data.Vector.imapM_ (\i e -> do
    ppOffsetInfos <- ContT $ withCStruct (e)
    lift $ poke (pPpOffsetInfos `plusPtr` (8 * (i)) :: Ptr (Ptr AccelerationStructureBuildOffsetInfoKHR)) ppOffsetInfos) (offsetInfos)
  lift $ vkCmdBuildAccelerationStructureKHR' (commandBufferHandle (commandBuffer)) ((fromIntegral pInfosLength :: Word32)) (pPInfos) (pPpOffsetInfos)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBuildAccelerationStructureIndirectKHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr (AccelerationStructureBuildGeometryInfoKHR a) -> Buffer -> DeviceSize -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Ptr (AccelerationStructureBuildGeometryInfoKHR a) -> Buffer -> DeviceSize -> Word32 -> IO ()

-- | vkCmdBuildAccelerationStructureIndirectKHR - Build an acceleration
-- structure with some parameters provided on the device
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @pInfo@ is a pointer to a
--     'AccelerationStructureBuildGeometryInfoKHR' structure defining the
--     geometry used to build the acceleration structure.
--
-- -   @indirectBuffer@ is the 'Vulkan.Core10.Handles.Buffer' containing
--     @pInfo->geometryCount@ 'AccelerationStructureBuildOffsetInfoKHR'
--     structures defining dynamic offsets to the addresses where geometry
--     data is stored, as defined by @pInfo@.
--
-- -   @indirectOffset@ is the byte offset into @indirectBuffer@ where
--     offset parameters begin.
--
-- -   @stride@ is the byte stride between successive sets of offset
--     parameters.
--
-- == Valid Usage
--
-- -   All 'DeviceOrHostAddressKHR' or 'DeviceOrHostAddressConstKHR'
--     referenced by this command /must/ contain valid device addresses
--
-- -   All 'Vulkan.Extensions.Handles.AccelerationStructureKHR' objects
--     referenced by this command /must/ be bound to device memory
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-raytracing-indirectasbuild ::rayTracingIndirectAccelerationStructureBuild>
--     feature /must/ be enabled
--
-- -   The
--     'Vulkan.Extensions.VK_KHR_deferred_host_operations.DeferredOperationInfoKHR'
--     structure /must/ not be included in the @pNext@ chain of any of the
--     provided 'AccelerationStructureBuildGeometryInfoKHR' structures
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @pInfo@ /must/ be a valid pointer to a valid
--     'AccelerationStructureBuildGeometryInfoKHR' structure
--
-- -   @indirectBuffer@ /must/ be a valid 'Vulkan.Core10.Handles.Buffer'
--     handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   Both of @commandBuffer@, and @indirectBuffer@ /must/ have been
--     created, allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Device'
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Compute                                                                                                               |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'AccelerationStructureBuildGeometryInfoKHR',
-- 'Vulkan.Core10.Handles.Buffer', 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.BaseType.DeviceSize'
cmdBuildAccelerationStructureIndirectKHR :: forall a io . (PokeChain a, MonadIO io) => CommandBuffer -> AccelerationStructureBuildGeometryInfoKHR a -> ("indirectBuffer" ::: Buffer) -> ("indirectOffset" ::: DeviceSize) -> ("indirectStride" ::: Word32) -> io ()
cmdBuildAccelerationStructureIndirectKHR commandBuffer info indirectBuffer indirectOffset indirectStride = liftIO . evalContT $ do
  let vkCmdBuildAccelerationStructureIndirectKHR' = mkVkCmdBuildAccelerationStructureIndirectKHR (pVkCmdBuildAccelerationStructureIndirectKHR (deviceCmds (commandBuffer :: CommandBuffer)))
  pInfo <- ContT $ withCStruct (info)
  lift $ vkCmdBuildAccelerationStructureIndirectKHR' (commandBufferHandle (commandBuffer)) pInfo (indirectBuffer) (indirectOffset) (indirectStride)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkBuildAccelerationStructureKHR
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr (AccelerationStructureBuildGeometryInfoKHR a) -> Ptr (Ptr AccelerationStructureBuildOffsetInfoKHR) -> IO Result) -> Ptr Device_T -> Word32 -> Ptr (AccelerationStructureBuildGeometryInfoKHR a) -> Ptr (Ptr AccelerationStructureBuildOffsetInfoKHR) -> IO Result

-- | vkBuildAccelerationStructureKHR - Build an acceleration structure on the
-- host
--
-- = Parameters
--
-- This command fulfills the same task as
-- 'cmdBuildAccelerationStructureKHR' but executed by the host.
--
-- = Description
--
-- -   @device@ is the 'Vulkan.Core10.Handles.Device' for which the
--     acceleration structures are being built.
--
-- -   @infoCount@ is the number of acceleration structures to build. It
--     specifies the number of the @pInfos@ structures and @ppOffsetInfos@
--     pointers that /must/ be provided.
--
-- -   @pInfos@ is a pointer to an array of @infoCount@
--     'AccelerationStructureBuildGeometryInfoKHR' structures defining the
--     geometry used to build each acceleration structure.
--
-- -   @ppOffsetInfos@ is an array of @infoCount@ pointers to arrays of
--     'AccelerationStructureBuildOffsetInfoKHR' structures. Each
--     @ppOffsetInfos@[i] is an array of @pInfos@[i].@geometryCount@
--     'AccelerationStructureBuildOffsetInfoKHR' structures defining
--     dynamic offsets to the addresses where geometry data is stored, as
--     defined by @pInfos@[i].
--
-- The 'buildAccelerationStructureKHR' command provides the ability to
-- initiate multiple acceleration structures builds, however there is no
-- ordering or synchronization implied between any of the individual
-- acceleration structure builds.
--
-- Note
--
-- This means that an application /cannot/ build a top-level acceleration
-- structure in the same 'buildAccelerationStructureKHR' call as the
-- associated bottom-level or instance acceleration structures are being
-- built. There also /cannot/ be any memory aliasing between any
-- acceleration structure memories or scratch memories being used by any of
-- the builds.
--
-- If the
-- 'Vulkan.Extensions.VK_KHR_deferred_host_operations.DeferredOperationInfoKHR'
-- structure is included in the @pNext@ chain of any
-- 'AccelerationStructureBuildGeometryInfoKHR' structure, the operation of
-- this command is /deferred/, as defined in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#deferred-host-operations Deferred Host Operations>
-- chapter.
--
-- == Valid Usage
--
-- -   [[VUID-{refpage}-pOffsetInfos-03402]] Each element of
--     @ppOffsetInfos@[i] /must/ be a valid pointer to an array of
--     @pInfos@[i].@geometryCount@
--     'AccelerationStructureBuildOffsetInfoKHR' structures
--
-- -   [[VUID-{refpage}-pInfos-03403]] Each
--     @pInfos@[i].@srcAccelerationStructure@ /must/ not refer to the same
--     acceleration structure as any @pInfos@[i].@dstAccelerationStructure@
--     that is provided to the same build command unless it is identical
--     for an update
--
-- -   [[VUID-{refpage}-pInfos-03404]] For each @pInfos@[i],
--     @dstAccelerationStructure@ /must/ have been created with compatible
--     'AccelerationStructureCreateInfoKHR' where
--     'AccelerationStructureCreateInfoKHR'::@type@ and
--     'AccelerationStructureCreateInfoKHR'::@flags@ are identical to
--     'AccelerationStructureBuildGeometryInfoKHR'::@type@ and
--     'AccelerationStructureBuildGeometryInfoKHR'::@flags@ respectively,
--     'AccelerationStructureBuildGeometryInfoKHR'::@geometryCount@ for
--     @dstAccelerationStructure@ are greater than or equal to the build
--     size, and each geometry in
--     'AccelerationStructureBuildGeometryInfoKHR'::@ppGeometries@ for
--     @dstAccelerationStructure@ has greater than or equal to the number
--     of vertices, indices, and AABBs,
--     'AccelerationStructureGeometryTrianglesDataKHR'::@transformData@ is
--     both 0 or both non-zero, and all other parameters are the same
--
-- -   [[VUID-{refpage}-pInfos-03405]] For each @pInfos@[i], if @update@ is
--     'Vulkan.Core10.BaseType.TRUE', then objects that were previously
--     active for that acceleration structure /must/ not be made inactive
--     as per
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#acceleration-structure-inactive-prims ???>
--
-- -   [[VUID-{refpage}-pInfos-03406]] For each @pInfos@[i], if @update@ is
--     'Vulkan.Core10.BaseType.TRUE', then objects that were previously
--     inactive for that acceleration structure /must/ not be made active
--     as per
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#acceleration-structure-inactive-prims ???>
--
-- -   [[VUID-{refpage}-None-03407]] Any acceleration structure instance in
--     any top level build in this command /must/ not reference any bottom
--     level acceleration structure built by this command
--
-- -   [[VUID-{refpage}-pInfos-03408]] There /must/ not be any
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-memory-aliasing memory aliasing>
--     between the scratch memories that are provided in all the
--     @pInfos@[i].@scratchData@ memories for the acceleration structure
--     builds
--
-- -   [[VUID-{refpage}-None-03409]] There /must/ not be any
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-memory-aliasing memory aliasing>
--     between memory bound to any top level, bottom level, or instance
--     acceleration structure accessed by this command
--
-- -   All 'DeviceOrHostAddressKHR' or 'DeviceOrHostAddressConstKHR'
--     referenced by this command /must/ contain valid host addresses
--
-- -   All 'Vulkan.Extensions.Handles.AccelerationStructureKHR' objects
--     referenced by this command /must/ be bound to host-visible memory
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#feature-raytracing-hostascmds ::rayTracingHostAccelerationStructureCommands>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   @pInfos@ /must/ be a valid pointer to an array of @infoCount@ valid
--     'AccelerationStructureBuildGeometryInfoKHR' structures
--
-- -   @ppOffsetInfos@ /must/ be a valid pointer to an array of @infoCount@
--     'AccelerationStructureBuildOffsetInfoKHR' structures
--
-- -   @infoCount@ /must/ be greater than @0@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.OPERATION_DEFERRED_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.OPERATION_NOT_DEFERRED_KHR'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'AccelerationStructureBuildGeometryInfoKHR',
-- 'AccelerationStructureBuildOffsetInfoKHR',
-- 'Vulkan.Core10.Handles.Device'
buildAccelerationStructureKHR :: forall a io . (PokeChain a, MonadIO io) => Device -> ("infos" ::: Vector (AccelerationStructureBuildGeometryInfoKHR a)) -> ("offsetInfos" ::: Vector AccelerationStructureBuildOffsetInfoKHR) -> io (Result)
buildAccelerationStructureKHR device infos offsetInfos = liftIO . evalContT $ do
  let vkBuildAccelerationStructureKHR' = mkVkBuildAccelerationStructureKHR (pVkBuildAccelerationStructureKHR (deviceCmds (device :: Device)))
  let pInfosLength = Data.Vector.length $ (infos)
  let ppOffsetInfosLength = Data.Vector.length $ (offsetInfos)
  lift $ unless (ppOffsetInfosLength == pInfosLength) $
    throwIO $ IOError Nothing InvalidArgument "" "ppOffsetInfos and pInfos must have the same length" Nothing Nothing
  pPInfos <- ContT $ allocaBytesAligned @(AccelerationStructureBuildGeometryInfoKHR _) ((Data.Vector.length (infos)) * 72) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPInfos `plusPtr` (72 * (i)) :: Ptr (AccelerationStructureBuildGeometryInfoKHR _)) (e) . ($ ())) (infos)
  pPpOffsetInfos <- ContT $ allocaBytesAligned @(Ptr AccelerationStructureBuildOffsetInfoKHR) ((Data.Vector.length (offsetInfos)) * 8) 8
  Data.Vector.imapM_ (\i e -> do
    ppOffsetInfos <- ContT $ withCStruct (e)
    lift $ poke (pPpOffsetInfos `plusPtr` (8 * (i)) :: Ptr (Ptr AccelerationStructureBuildOffsetInfoKHR)) ppOffsetInfos) (offsetInfos)
  r <- lift $ vkBuildAccelerationStructureKHR' (deviceHandle (device)) ((fromIntegral pInfosLength :: Word32)) (pPInfos) (pPpOffsetInfos)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetAccelerationStructureDeviceAddressKHR
  :: FunPtr (Ptr Device_T -> Ptr AccelerationStructureDeviceAddressInfoKHR -> IO DeviceAddress) -> Ptr Device_T -> Ptr AccelerationStructureDeviceAddressInfoKHR -> IO DeviceAddress

-- | vkGetAccelerationStructureDeviceAddressKHR - Query an address of a
-- acceleration structure
--
-- = Parameters
--
-- -   @device@ is the logical device that the accelerationStructure was
--     created on.
--
-- -   @pInfo@ is a pointer to a
--     'AccelerationStructureDeviceAddressInfoKHR' structure specifying the
--     acceleration structure to retrieve an address for.
--
-- = Description
--
-- The 64-bit return value is an address of the acceleration structure,
-- which can be used for device and shader operations that involve
-- acceleration structures, such as ray traversal and acceleration
-- structure building.
--
-- If the acceleration structure was created with a non-zero value of
-- 'AccelerationStructureCreateInfoKHR'::@deviceAddress@ the return value
-- will be the same address.
--
-- == Valid Usage
--
-- -   If @device@ was created with multiple physical devices, then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-bufferDeviceAddressMultiDevice bufferDeviceAddressMultiDevice>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   @pInfo@ /must/ be a valid pointer to a valid
--     'AccelerationStructureDeviceAddressInfoKHR' structure
--
-- = See Also
--
-- 'AccelerationStructureDeviceAddressInfoKHR',
-- 'Vulkan.Core10.Handles.Device'
getAccelerationStructureDeviceAddressKHR :: forall io . MonadIO io => Device -> AccelerationStructureDeviceAddressInfoKHR -> io (DeviceAddress)
getAccelerationStructureDeviceAddressKHR device info = liftIO . evalContT $ do
  let vkGetAccelerationStructureDeviceAddressKHR' = mkVkGetAccelerationStructureDeviceAddressKHR (pVkGetAccelerationStructureDeviceAddressKHR (deviceCmds (device :: Device)))
  pInfo <- ContT $ withCStruct (info)
  r <- lift $ vkGetAccelerationStructureDeviceAddressKHR' (deviceHandle (device)) pInfo
  pure $ (r)


-- | VkRayTracingShaderGroupCreateInfoKHR - Structure specifying shaders in a
-- shader group
--
-- == Valid Usage
--
-- -   If @type@ is 'RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR' then
--     @generalShader@ /must/ be a valid index into
--     'RayTracingPipelineCreateInfoKHR'::@pStages@ referring to a shader
--     of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_RAYGEN_BIT_KHR',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MISS_BIT_KHR',
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_CALLABLE_BIT_KHR'
--
-- -   If @type@ is 'RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR' then
--     @closestHitShader@, @anyHitShader@, and @intersectionShader@ /must/
--     be 'Vulkan.Core10.APIConstants.SHADER_UNUSED_KHR'
--
-- -   If @type@ is
--     'RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR' then
--     @intersectionShader@ /must/ be a valid index into
--     'RayTracingPipelineCreateInfoKHR'::@pStages@ referring to a shader
--     of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_INTERSECTION_BIT_KHR'
--
-- -   If @type@ is 'RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR'
--     then @intersectionShader@ /must/ be
--     'Vulkan.Core10.APIConstants.SHADER_UNUSED_KHR'
--
-- -   @closestHitShader@ /must/ be either
--     'Vulkan.Core10.APIConstants.SHADER_UNUSED_KHR' or a valid index into
--     'RayTracingPipelineCreateInfoKHR'::@pStages@ referring to a shader
--     of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_CLOSEST_HIT_BIT_KHR'
--
-- -   @anyHitShader@ /must/ be either
--     'Vulkan.Core10.APIConstants.SHADER_UNUSED_KHR' or a valid index into
--     'RayTracingPipelineCreateInfoKHR'::@pStages@ referring to a shader
--     of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_ANY_HIT_BIT_KHR'
--
-- -   If
--     'PhysicalDeviceRayTracingFeaturesKHR'::@rayTracingShaderGroupHandleCaptureReplayMixed@
--     is 'Vulkan.Core10.BaseType.FALSE' then
--     @pShaderGroupCaptureReplayHandle@ /must/ not be provided if it has
--     not been provided on a previous call to ray tracing pipeline
--     creation
--
-- -   If
--     'PhysicalDeviceRayTracingFeaturesKHR'::@rayTracingShaderGroupHandleCaptureReplayMixed@
--     is 'Vulkan.Core10.BaseType.FALSE' then the caller /must/ guarantee
--     that no ray tracing pipeline creation commands with
--     @pShaderGroupCaptureReplayHandle@ provided execute simultaneously
--     with ray tracing pipeline creation commands without
--     @pShaderGroupCaptureReplayHandle@ provided
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_KHR'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @type@ /must/ be a valid 'RayTracingShaderGroupTypeKHR' value
--
-- = See Also
--
-- 'RayTracingPipelineCreateInfoKHR', 'RayTracingShaderGroupTypeKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data RayTracingShaderGroupCreateInfoKHR = RayTracingShaderGroupCreateInfoKHR
  { -- | @type@ is the type of hit group specified in this structure.
    type' :: RayTracingShaderGroupTypeKHR
  , -- | @generalShader@ is the index of the ray generation, miss, or callable
    -- shader from 'RayTracingPipelineCreateInfoKHR'::@pStages@ in the group if
    -- the shader group has @type@ of
    -- 'RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR', and
    -- 'Vulkan.Core10.APIConstants.SHADER_UNUSED_KHR' otherwise.
    generalShader :: Word32
  , -- | @closestHitShader@ is the optional index of the closest hit shader from
    -- 'RayTracingPipelineCreateInfoKHR'::@pStages@ in the group if the shader
    -- group has @type@ of
    -- 'RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR' or
    -- 'RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR', and
    -- 'Vulkan.Core10.APIConstants.SHADER_UNUSED_KHR' otherwise.
    closestHitShader :: Word32
  , -- | @anyHitShader@ is the optional index of the any-hit shader from
    -- 'RayTracingPipelineCreateInfoKHR'::@pStages@ in the group if the shader
    -- group has @type@ of
    -- 'RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR' or
    -- 'RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR', and
    -- 'Vulkan.Core10.APIConstants.SHADER_UNUSED_KHR' otherwise.
    anyHitShader :: Word32
  , -- | @intersectionShader@ is the index of the intersection shader from
    -- 'RayTracingPipelineCreateInfoKHR'::@pStages@ in the group if the shader
    -- group has @type@ of
    -- 'RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR', and
    -- 'Vulkan.Core10.APIConstants.SHADER_UNUSED_KHR' otherwise.
    intersectionShader :: Word32
  , -- | @pShaderGroupCaptureReplayHandle@ is an optional pointer to replay
    -- information for this shader group. Ignored if
    -- 'PhysicalDeviceRayTracingFeaturesKHR'::@rayTracingShaderGroupHandleCaptureReplay@
    -- is 'Vulkan.Core10.BaseType.FALSE'.
    shaderGroupCaptureReplayHandle :: Ptr ()
  }
  deriving (Typeable)
deriving instance Show RayTracingShaderGroupCreateInfoKHR

instance ToCStruct RayTracingShaderGroupCreateInfoKHR where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RayTracingShaderGroupCreateInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr RayTracingShaderGroupTypeKHR)) (type')
    poke ((p `plusPtr` 20 :: Ptr Word32)) (generalShader)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (closestHitShader)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (anyHitShader)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (intersectionShader)
    poke ((p `plusPtr` 40 :: Ptr (Ptr ()))) (shaderGroupCaptureReplayHandle)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr RayTracingShaderGroupTypeKHR)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    f

instance FromCStruct RayTracingShaderGroupCreateInfoKHR where
  peekCStruct p = do
    type' <- peek @RayTracingShaderGroupTypeKHR ((p `plusPtr` 16 :: Ptr RayTracingShaderGroupTypeKHR))
    generalShader <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    closestHitShader <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    anyHitShader <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    intersectionShader <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pShaderGroupCaptureReplayHandle <- peek @(Ptr ()) ((p `plusPtr` 40 :: Ptr (Ptr ())))
    pure $ RayTracingShaderGroupCreateInfoKHR
             type' generalShader closestHitShader anyHitShader intersectionShader pShaderGroupCaptureReplayHandle

instance Storable RayTracingShaderGroupCreateInfoKHR where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero RayTracingShaderGroupCreateInfoKHR where
  zero = RayTracingShaderGroupCreateInfoKHR
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkRayTracingPipelineCreateInfoKHR - Structure specifying parameters of a
-- newly created ray tracing pipeline
--
-- = Description
--
-- The parameters @basePipelineHandle@ and @basePipelineIndex@ are
-- described in more detail in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#pipelines-pipeline-derivatives Pipeline Derivatives>.
--
-- When
-- 'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_LIBRARY_BIT_KHR'
-- is specified, this pipeline defines a /pipeline library/ which /cannot/
-- be bound as a ray tracing pipeline directly. Instead, pipeline libraries
-- define common shaders and shader groups which /can/ be included in
-- future pipeline creation.
--
-- If pipeline libraries are included in @libraries@, shaders defined in
-- those libraries are treated as if they were defined as additional
-- entries in @pStages@, appended in the order they appear in the
-- @pLibraries@ array and in the @pStages@ array when those libraries were
-- defined.
--
-- When referencing shader groups in order to obtain a shader group handle,
-- groups defined in those libraries are treated as if they were defined as
-- additional entries in @pGroups@, appended in the order they appear in
-- the @pLibraries@ array and in the @pGroups@ array when those libraries
-- were defined. The shaders these groups reference are set when the
-- pipeline library is created, referencing those specified in the pipeline
-- library, not in the pipeline that includes it.
--
-- If the
-- 'Vulkan.Extensions.VK_KHR_deferred_host_operations.DeferredOperationInfoKHR'
-- structure is included in the @pNext@ chain of
-- 'RayTracingPipelineCreateInfoKHR', the operation of this pipeline
-- creation is /deferred/, as defined in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#deferred-host-operations Deferred Host Operations>
-- chapter.
--
-- == Valid Usage
--
-- -   If @flags@ contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineIndex@ is @-1@, @basePipelineHandle@ /must/
--     be a valid handle to a ray tracing 'Vulkan.Core10.Handles.Pipeline'
--
-- -   If @flags@ contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineHandle@ is
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @basePipelineIndex@ /must/
--     be a valid index into the calling command’s @pCreateInfos@ parameter
--
-- -   If @flags@ contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineIndex@ is not @-1@, @basePipelineHandle@
--     /must/ be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   If @flags@ contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineHandle@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @basePipelineIndex@ /must/
--     be @-1@
--
-- -   The @stage@ member of at least one element of @pStages@ /must/ be
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_RAYGEN_BIT_KHR'
--
-- -   The shader code for the entry points identified by @pStages@, and
--     the rest of the state identified by this structure /must/ adhere to
--     the pipeline linking rules described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces Shader Interfaces>
--     chapter
--
-- -   @layout@ /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-pipelinelayout-consistency consistent>
--     with all shaders specified in @pStages@
--
-- -   The number of resources in @layout@ accessible to each shader stage
--     that is used by the pipeline /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxPerStageResources@
--
-- -   @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-pipelineCreationCacheControl pipelineCreationCacheControl>
--     feature is not enabled, @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_EXT'
--     or
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT_EXT'
--
-- -   @maxRecursionDepth@ /must/ be less than or equal to
--     'PhysicalDeviceRayTracingPropertiesKHR'::@maxRecursionDepth@
--
-- -   If @flags@ includes
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_LIBRARY_BIT_KHR',
--     @pLibraryInterface@ /must/ not be @NULL@
--
-- -   If the @libraryCount@ member of @libraries@ is greater than @0@,
--     @pLibraryInterface@ /must/ not be @NULL@
--
-- -   Each element of the @pLibraries@ member of @libraries@ /must/ have
--     been created with the value of @maxRecursionDepth@ equal to that in
--     this pipeline
--
-- -   Each element of the @pLibraries@ member of @libraries@ /must/ have
--     been created with a @layout@ that is compatible with the @layout@ in
--     this pipeline
--
-- -   Each element of the @pLibraries@ member of @libraries@ /must/ have
--     been created with values of the @maxPayloadSize@,
--     @maxAttributeSize@, and @maxCallableSize@ members of
--     @pLibraryInterface@ equal to those in this pipeline
--
-- -   If @flags@ includes
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR',
--     for any element of @pGroups@ with a @type@ of
--     'RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR' or
--     'RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR', the
--     @anyHitShader@ of that element /must/ not be
--     'Vulkan.Core10.APIConstants.SHADER_UNUSED_KHR'
--
-- -   If @flags@ includes
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR',
--     for any element of @pGroups@ with a @type@ of
--     'RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR' or
--     'RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR', the
--     @closestHitShader@ of that element /must/ not be
--     'Vulkan.Core10.APIConstants.SHADER_UNUSED_KHR'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-rayTracingPrimitiveCulling rayTracingPrimitiveCulling>
--     feature is not enabled, @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_SKIP_AABBS_BIT_KHR'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-rayTracingPrimitiveCulling rayTracingPrimitiveCulling>
--     feature is not enabled, @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR'
--
-- -   If @libraries.libraryCount@ is zero, then @stageCount@ /must/ not be
--     zero
--
-- -   If @libraries.libraryCount@ is zero, then @groupCount@ /must/ not be
--     zero
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_KHR'
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Vulkan.Extensions.VK_KHR_deferred_host_operations.DeferredOperationInfoKHR'
--     or
--     'Vulkan.Extensions.VK_EXT_pipeline_creation_feedback.PipelineCreationFeedbackCreateInfoEXT'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   @flags@ /must/ be a valid combination of
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits'
--     values
--
-- -   If @stageCount@ is not @0@, @pStages@ /must/ be a valid pointer to
--     an array of @stageCount@ valid
--     'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo' structures
--
-- -   If @groupCount@ is not @0@, @pGroups@ /must/ be a valid pointer to
--     an array of @groupCount@ valid 'RayTracingShaderGroupCreateInfoKHR'
--     structures
--
-- -   @libraries@ /must/ be a valid
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'
--     structure
--
-- -   If @pLibraryInterface@ is not @NULL@, @pLibraryInterface@ /must/ be
--     a valid pointer to a valid
--     'RayTracingPipelineInterfaceCreateInfoKHR' structure
--
-- -   @layout@ /must/ be a valid 'Vulkan.Core10.Handles.PipelineLayout'
--     handle
--
-- -   Both of @basePipelineHandle@, and @layout@ that are valid handles of
--     non-ignored parameters /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Pipeline',
-- 'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlags',
-- 'Vulkan.Core10.Handles.PipelineLayout',
-- 'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR',
-- 'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo',
-- 'RayTracingPipelineInterfaceCreateInfoKHR',
-- 'RayTracingShaderGroupCreateInfoKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createRayTracingPipelinesKHR'
data RayTracingPipelineCreateInfoKHR (es :: [Type]) = RayTracingPipelineCreateInfoKHR
  { -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
    next :: Chain es
  , -- | @flags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits'
    -- specifying how the pipeline will be generated.
    flags :: PipelineCreateFlags
  , -- | @pStages@ is a pointer to an array of @stageCount@
    -- 'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo' structures
    -- describing the set of the shader stages to be included in the ray
    -- tracing pipeline.
    stages :: Vector (SomeStruct PipelineShaderStageCreateInfo)
  , -- | @pGroups@ is a pointer to an array of @groupCount@
    -- 'RayTracingShaderGroupCreateInfoKHR' structures describing the set of
    -- the shader stages to be included in each shader group in the ray tracing
    -- pipeline.
    groups :: Vector RayTracingShaderGroupCreateInfoKHR
  , -- | @maxRecursionDepth@ is the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#ray-tracing-recursion-depth maximum recursion depth>
    -- of shaders executed by this pipeline.
    maxRecursionDepth :: Word32
  , -- | @libraries@ is a
    -- 'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'
    -- structure defining pipeline libraries to include.
    libraries :: PipelineLibraryCreateInfoKHR
  , -- | @pLibraryInterface@ is a pointer to a
    -- 'RayTracingPipelineInterfaceCreateInfoKHR' structure defining additional
    -- information when using pipeline libraries.
    libraryInterface :: Maybe RayTracingPipelineInterfaceCreateInfoKHR
  , -- | @layout@ is the description of binding locations used by both the
    -- pipeline and descriptor sets used with the pipeline.
    layout :: PipelineLayout
  , -- | @basePipelineHandle@ is a pipeline to derive from.
    basePipelineHandle :: Pipeline
  , -- | @basePipelineIndex@ is an index into the @pCreateInfos@ parameter to use
    -- as a pipeline to derive from.
    basePipelineIndex :: Int32
  }
  deriving (Typeable)
deriving instance Show (Chain es) => Show (RayTracingPipelineCreateInfoKHR es)

instance Extensible RayTracingPipelineCreateInfoKHR where
  extensibleType = STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_KHR
  setNext x next = x{next = next}
  getNext RayTracingPipelineCreateInfoKHR{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends RayTracingPipelineCreateInfoKHR e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @DeferredOperationInfoKHR = Just f
    | Just Refl <- eqT @e @PipelineCreationFeedbackCreateInfoEXT = Just f
    | otherwise = Nothing

instance PokeChain es => ToCStruct (RayTracingPipelineCreateInfoKHR es) where
  withCStruct x f = allocaBytesAligned 120 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RayTracingPipelineCreateInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_KHR)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (stages)) :: Word32))
    pPStages' <- ContT $ allocaBytesAligned @(PipelineShaderStageCreateInfo _) ((Data.Vector.length (stages)) * 48) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPStages' `plusPtr` (48 * (i)) :: Ptr (PipelineShaderStageCreateInfo _))) (e) . ($ ())) (stages)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr (PipelineShaderStageCreateInfo _)))) (pPStages')
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (groups)) :: Word32))
    pPGroups' <- ContT $ allocaBytesAligned @RayTracingShaderGroupCreateInfoKHR ((Data.Vector.length (groups)) * 48) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPGroups' `plusPtr` (48 * (i)) :: Ptr RayTracingShaderGroupCreateInfoKHR) (e) . ($ ())) (groups)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr RayTracingShaderGroupCreateInfoKHR))) (pPGroups')
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) (maxRecursionDepth)
    ContT $ pokeCStruct ((p `plusPtr` 56 :: Ptr PipelineLibraryCreateInfoKHR)) (libraries) . ($ ())
    pLibraryInterface'' <- case (libraryInterface) of
      Nothing -> pure nullPtr
      Just j -> ContT $ withCStruct (j)
    lift $ poke ((p `plusPtr` 88 :: Ptr (Ptr RayTracingPipelineInterfaceCreateInfoKHR))) pLibraryInterface''
    lift $ poke ((p `plusPtr` 96 :: Ptr PipelineLayout)) (layout)
    lift $ poke ((p `plusPtr` 104 :: Ptr Pipeline)) (basePipelineHandle)
    lift $ poke ((p `plusPtr` 112 :: Ptr Int32)) (basePipelineIndex)
    lift $ f
  cStructSize = 120
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_KHR)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    pPStages' <- ContT $ allocaBytesAligned @(PipelineShaderStageCreateInfo _) ((Data.Vector.length (mempty)) * 48) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPStages' `plusPtr` (48 * (i)) :: Ptr (PipelineShaderStageCreateInfo _))) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr (PipelineShaderStageCreateInfo _)))) (pPStages')
    pPGroups' <- ContT $ allocaBytesAligned @RayTracingShaderGroupCreateInfoKHR ((Data.Vector.length (mempty)) * 48) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPGroups' `plusPtr` (48 * (i)) :: Ptr RayTracingShaderGroupCreateInfoKHR) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr RayTracingShaderGroupCreateInfoKHR))) (pPGroups')
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 56 :: Ptr PipelineLibraryCreateInfoKHR)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 96 :: Ptr PipelineLayout)) (zero)
    lift $ poke ((p `plusPtr` 112 :: Ptr Int32)) (zero)
    lift $ f

instance PeekChain es => FromCStruct (RayTracingPipelineCreateInfoKHR es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @PipelineCreateFlags ((p `plusPtr` 16 :: Ptr PipelineCreateFlags))
    stageCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pStages <- peek @(Ptr (PipelineShaderStageCreateInfo _)) ((p `plusPtr` 24 :: Ptr (Ptr (PipelineShaderStageCreateInfo a))))
    pStages' <- generateM (fromIntegral stageCount) (\i -> peekSomeCStruct (forgetExtensions ((pStages `advancePtrBytes` (48 * (i)) :: Ptr (PipelineShaderStageCreateInfo _)))))
    groupCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pGroups <- peek @(Ptr RayTracingShaderGroupCreateInfoKHR) ((p `plusPtr` 40 :: Ptr (Ptr RayTracingShaderGroupCreateInfoKHR)))
    pGroups' <- generateM (fromIntegral groupCount) (\i -> peekCStruct @RayTracingShaderGroupCreateInfoKHR ((pGroups `advancePtrBytes` (48 * (i)) :: Ptr RayTracingShaderGroupCreateInfoKHR)))
    maxRecursionDepth <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    libraries <- peekCStruct @PipelineLibraryCreateInfoKHR ((p `plusPtr` 56 :: Ptr PipelineLibraryCreateInfoKHR))
    pLibraryInterface <- peek @(Ptr RayTracingPipelineInterfaceCreateInfoKHR) ((p `plusPtr` 88 :: Ptr (Ptr RayTracingPipelineInterfaceCreateInfoKHR)))
    pLibraryInterface' <- maybePeek (\j -> peekCStruct @RayTracingPipelineInterfaceCreateInfoKHR (j)) pLibraryInterface
    layout <- peek @PipelineLayout ((p `plusPtr` 96 :: Ptr PipelineLayout))
    basePipelineHandle <- peek @Pipeline ((p `plusPtr` 104 :: Ptr Pipeline))
    basePipelineIndex <- peek @Int32 ((p `plusPtr` 112 :: Ptr Int32))
    pure $ RayTracingPipelineCreateInfoKHR
             next flags pStages' pGroups' maxRecursionDepth libraries pLibraryInterface' layout basePipelineHandle basePipelineIndex

instance es ~ '[] => Zero (RayTracingPipelineCreateInfoKHR es) where
  zero = RayTracingPipelineCreateInfoKHR
           ()
           zero
           mempty
           mempty
           zero
           zero
           Nothing
           zero
           zero
           zero


-- | VkBindAccelerationStructureMemoryInfoKHR - Structure specifying
-- acceleration structure memory binding
--
-- == Valid Usage
--
-- -   @accelerationStructure@ /must/ not already be backed by a memory
--     object
--
-- -   @memoryOffset@ /must/ be less than the size of @memory@
--
-- -   @memory@ /must/ have been allocated using one of the memory types
--     allowed in the @memoryTypeBits@ member of the
--     'Vulkan.Core10.MemoryManagement.MemoryRequirements' structure
--     returned from a call to
--     'getAccelerationStructureMemoryRequirementsKHR' with
--     @accelerationStructure@ and @type@ of
--     'ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_KHR'
--
-- -   @memoryOffset@ /must/ be an integer multiple of the @alignment@
--     member of the 'Vulkan.Core10.MemoryManagement.MemoryRequirements'
--     structure returned from a call to
--     'getAccelerationStructureMemoryRequirementsKHR' with
--     @accelerationStructure@ and @type@ of
--     'ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_KHR'
--
-- -   The @size@ member of the
--     'Vulkan.Core10.MemoryManagement.MemoryRequirements' structure
--     returned from a call to
--     'getAccelerationStructureMemoryRequirementsKHR' with
--     @accelerationStructure@ and @type@ of
--     'ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_KHR' /must/
--     be less than or equal to the size of @memory@ minus @memoryOffset@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_KHR'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @accelerationStructure@ /must/ be a valid
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR' handle
--
-- -   @memory@ /must/ be a valid 'Vulkan.Core10.Handles.DeviceMemory'
--     handle
--
-- -   If @deviceIndexCount@ is not @0@, @pDeviceIndices@ /must/ be a valid
--     pointer to an array of @deviceIndexCount@ @uint32_t@ values
--
-- -   Both of @accelerationStructure@, and @memory@ /must/ have been
--     created, allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.AccelerationStructureKHR',
-- 'Vulkan.Core10.Handles.DeviceMemory',
-- 'Vulkan.Core10.BaseType.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'bindAccelerationStructureMemoryKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.bindAccelerationStructureMemoryNV'
data BindAccelerationStructureMemoryInfoKHR = BindAccelerationStructureMemoryInfoKHR
  { -- | @accelerationStructure@ is the acceleration structure to be attached to
    -- memory.
    accelerationStructure :: AccelerationStructureKHR
  , -- | @memory@ is a 'Vulkan.Core10.Handles.DeviceMemory' object describing the
    -- device memory to attach.
    memory :: DeviceMemory
  , -- | @memoryOffset@ is the start offset of the region of memory that is to be
    -- bound to the acceleration structure. The number of bytes returned in the
    -- 'Vulkan.Core10.MemoryManagement.MemoryRequirements'::@size@ member in
    -- @memory@, starting from @memoryOffset@ bytes, will be bound to the
    -- specified acceleration structure.
    memoryOffset :: DeviceSize
  , -- | @pDeviceIndices@ is a pointer to an array of device indices.
    deviceIndices :: Vector Word32
  }
  deriving (Typeable)
deriving instance Show BindAccelerationStructureMemoryInfoKHR

instance ToCStruct BindAccelerationStructureMemoryInfoKHR where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BindAccelerationStructureMemoryInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr AccelerationStructureKHR)) (accelerationStructure)
    lift $ poke ((p `plusPtr` 24 :: Ptr DeviceMemory)) (memory)
    lift $ poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (memoryOffset)
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (deviceIndices)) :: Word32))
    pPDeviceIndices' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (deviceIndices)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDeviceIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (deviceIndices)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr Word32))) (pPDeviceIndices')
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr AccelerationStructureKHR)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr DeviceMemory)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    pPDeviceIndices' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDeviceIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (mempty)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr Word32))) (pPDeviceIndices')
    lift $ f

instance FromCStruct BindAccelerationStructureMemoryInfoKHR where
  peekCStruct p = do
    accelerationStructure <- peek @AccelerationStructureKHR ((p `plusPtr` 16 :: Ptr AccelerationStructureKHR))
    memory <- peek @DeviceMemory ((p `plusPtr` 24 :: Ptr DeviceMemory))
    memoryOffset <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    deviceIndexCount <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    pDeviceIndices <- peek @(Ptr Word32) ((p `plusPtr` 48 :: Ptr (Ptr Word32)))
    pDeviceIndices' <- generateM (fromIntegral deviceIndexCount) (\i -> peek @Word32 ((pDeviceIndices `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pure $ BindAccelerationStructureMemoryInfoKHR
             accelerationStructure memory memoryOffset pDeviceIndices'

instance Zero BindAccelerationStructureMemoryInfoKHR where
  zero = BindAccelerationStructureMemoryInfoKHR
           zero
           zero
           zero
           mempty


-- | VkWriteDescriptorSetAccelerationStructureKHR - Structure specifying
-- acceleration structure descriptor info
--
-- == Valid Usage
--
-- -   @accelerationStructureCount@ /must/ be equal to @descriptorCount@ in
--     the extended structure
--
-- -   Each acceleration structure in @pAccelerationStructures@ /must/ have
--     been created with 'ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_KHR'
--
-- -   @pAccelerationStructures@ /must/ be a valid pointer to an array of
--     @accelerationStructureCount@ valid
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR' handles
--
-- -   @accelerationStructureCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.AccelerationStructureKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data WriteDescriptorSetAccelerationStructureKHR = WriteDescriptorSetAccelerationStructureKHR
  { -- | @pAccelerationStructures@ are the acceleration structures to update.
    accelerationStructures :: Vector AccelerationStructureKHR }
  deriving (Typeable)
deriving instance Show WriteDescriptorSetAccelerationStructureKHR

instance ToCStruct WriteDescriptorSetAccelerationStructureKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p WriteDescriptorSetAccelerationStructureKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (accelerationStructures)) :: Word32))
    pPAccelerationStructures' <- ContT $ allocaBytesAligned @AccelerationStructureKHR ((Data.Vector.length (accelerationStructures)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPAccelerationStructures' `plusPtr` (8 * (i)) :: Ptr AccelerationStructureKHR) (e)) (accelerationStructures)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr AccelerationStructureKHR))) (pPAccelerationStructures')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pPAccelerationStructures' <- ContT $ allocaBytesAligned @AccelerationStructureKHR ((Data.Vector.length (mempty)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPAccelerationStructures' `plusPtr` (8 * (i)) :: Ptr AccelerationStructureKHR) (e)) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr AccelerationStructureKHR))) (pPAccelerationStructures')
    lift $ f

instance FromCStruct WriteDescriptorSetAccelerationStructureKHR where
  peekCStruct p = do
    accelerationStructureCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pAccelerationStructures <- peek @(Ptr AccelerationStructureKHR) ((p `plusPtr` 24 :: Ptr (Ptr AccelerationStructureKHR)))
    pAccelerationStructures' <- generateM (fromIntegral accelerationStructureCount) (\i -> peek @AccelerationStructureKHR ((pAccelerationStructures `advancePtrBytes` (8 * (i)) :: Ptr AccelerationStructureKHR)))
    pure $ WriteDescriptorSetAccelerationStructureKHR
             pAccelerationStructures'

instance Zero WriteDescriptorSetAccelerationStructureKHR where
  zero = WriteDescriptorSetAccelerationStructureKHR
           mempty


-- | VkAccelerationStructureMemoryRequirementsInfoKHR - Structure specifying
-- acceleration to query for memory requirements
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'AccelerationStructureBuildTypeKHR',
-- 'Vulkan.Extensions.Handles.AccelerationStructureKHR',
-- 'AccelerationStructureMemoryRequirementsTypeKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getAccelerationStructureMemoryRequirementsKHR'
data AccelerationStructureMemoryRequirementsInfoKHR = AccelerationStructureMemoryRequirementsInfoKHR
  { -- | @type@ /must/ be a valid
    -- 'AccelerationStructureMemoryRequirementsTypeKHR' value
    type' :: AccelerationStructureMemoryRequirementsTypeKHR
  , -- | @buildType@ /must/ be a valid 'AccelerationStructureBuildTypeKHR' value
    buildType :: AccelerationStructureBuildTypeKHR
  , -- | @accelerationStructure@ /must/ be a valid
    -- 'Vulkan.Extensions.Handles.AccelerationStructureKHR' handle
    accelerationStructure :: AccelerationStructureKHR
  }
  deriving (Typeable)
deriving instance Show AccelerationStructureMemoryRequirementsInfoKHR

instance ToCStruct AccelerationStructureMemoryRequirementsInfoKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureMemoryRequirementsInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AccelerationStructureMemoryRequirementsTypeKHR)) (type')
    poke ((p `plusPtr` 20 :: Ptr AccelerationStructureBuildTypeKHR)) (buildType)
    poke ((p `plusPtr` 24 :: Ptr AccelerationStructureKHR)) (accelerationStructure)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AccelerationStructureMemoryRequirementsTypeKHR)) (zero)
    poke ((p `plusPtr` 20 :: Ptr AccelerationStructureBuildTypeKHR)) (zero)
    poke ((p `plusPtr` 24 :: Ptr AccelerationStructureKHR)) (zero)
    f

instance FromCStruct AccelerationStructureMemoryRequirementsInfoKHR where
  peekCStruct p = do
    type' <- peek @AccelerationStructureMemoryRequirementsTypeKHR ((p `plusPtr` 16 :: Ptr AccelerationStructureMemoryRequirementsTypeKHR))
    buildType <- peek @AccelerationStructureBuildTypeKHR ((p `plusPtr` 20 :: Ptr AccelerationStructureBuildTypeKHR))
    accelerationStructure <- peek @AccelerationStructureKHR ((p `plusPtr` 24 :: Ptr AccelerationStructureKHR))
    pure $ AccelerationStructureMemoryRequirementsInfoKHR
             type' buildType accelerationStructure

instance Storable AccelerationStructureMemoryRequirementsInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AccelerationStructureMemoryRequirementsInfoKHR where
  zero = AccelerationStructureMemoryRequirementsInfoKHR
           zero
           zero
           zero


-- | VkPhysicalDeviceRayTracingFeaturesKHR - Structure describing the ray
-- tracing features that can be supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceRayTracingFeaturesKHR' structure
-- describe the following features:
--
-- = Description
--
-- -   @rayTracing@ indicates whether the implementation supports ray
--     tracing functionality. See
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#ray-tracing Ray Tracing>.
--
-- -   @rayTracingShaderGroupHandleCaptureReplay@ indicates whether the
--     implementation supports saving and reusing shader group handles,
--     e.g. for trace capture and replay.
--
-- -   @rayTracingShaderGroupHandleCaptureReplayMixed@ indicates whether
--     the implementation supports reuse of shader group handles being
--     arbitrarily mixed with creation of non-reused shader group handles.
--     If this is 'Vulkan.Core10.BaseType.FALSE', all reused shader group
--     handles /must/ be specified before any non-reused handles /may/ be
--     created.
--
-- -   @rayTracingAccelerationStructureCaptureReplay@ indicates whether the
--     implementation supports saving and reusing acceleration structure
--     device addresses, e.g. for trace capture and replay.
--
-- -   @rayTracingIndirectTraceRays@ indicates whether the implementation
--     supports indirect trace ray commands, e.g.
--     'cmdTraceRaysIndirectKHR'.
--
-- -   @rayTracingIndirectAccelerationStructureBuild@ indicates whether the
--     implementation supports indirect acceleration structure build
--     commands, e.g. 'cmdBuildAccelerationStructureIndirectKHR'.
--
-- -   @rayTracingHostAccelerationStructureCommands@ indicates whether the
--     implementation supports host side acceleration structure commands,
--     e.g. 'buildAccelerationStructureKHR',
--     'copyAccelerationStructureKHR',
--     'copyAccelerationStructureToMemoryKHR',
--     'copyMemoryToAccelerationStructureKHR',
--     'writeAccelerationStructuresPropertiesKHR'.
--
-- -   @rayQuery@ indicates whether the implementation supports ray query
--     (@OpRayQueryProceedKHR@) functionality.
--
-- -   @rayTracingPrimitiveCulling@ indicates whether the implementation
--     supports
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#ray-traversal-culling-primitive primitive culling during ray traversal>.
--
-- If the 'PhysicalDeviceRayTracingFeaturesKHR' structure is included in
-- the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether the feature is supported.
-- 'PhysicalDeviceRayTracingFeaturesKHR' /can/ also be used in the @pNext@
-- chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to enable the features.
--
-- == Valid Usage
--
-- -   If @rayTracingShaderGroupHandleCaptureReplayMixed@ is
--     'Vulkan.Core10.BaseType.TRUE',
--     @rayTracingShaderGroupHandleCaptureReplay@ /must/ also be
--     'Vulkan.Core10.BaseType.TRUE'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_FEATURES_KHR'
--
-- = See Also
--
-- 'Vulkan.Core10.BaseType.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceRayTracingFeaturesKHR = PhysicalDeviceRayTracingFeaturesKHR
  { -- No documentation found for Nested "VkPhysicalDeviceRayTracingFeaturesKHR" "rayTracing"
    rayTracing :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceRayTracingFeaturesKHR" "rayTracingShaderGroupHandleCaptureReplay"
    rayTracingShaderGroupHandleCaptureReplay :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceRayTracingFeaturesKHR" "rayTracingShaderGroupHandleCaptureReplayMixed"
    rayTracingShaderGroupHandleCaptureReplayMixed :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceRayTracingFeaturesKHR" "rayTracingAccelerationStructureCaptureReplay"
    rayTracingAccelerationStructureCaptureReplay :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceRayTracingFeaturesKHR" "rayTracingIndirectTraceRays"
    rayTracingIndirectTraceRays :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceRayTracingFeaturesKHR" "rayTracingIndirectAccelerationStructureBuild"
    rayTracingIndirectAccelerationStructureBuild :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceRayTracingFeaturesKHR" "rayTracingHostAccelerationStructureCommands"
    rayTracingHostAccelerationStructureCommands :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceRayTracingFeaturesKHR" "rayQuery"
    rayQuery :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceRayTracingFeaturesKHR" "rayTracingPrimitiveCulling"
    rayTracingPrimitiveCulling :: Bool
  }
  deriving (Typeable)
deriving instance Show PhysicalDeviceRayTracingFeaturesKHR

instance ToCStruct PhysicalDeviceRayTracingFeaturesKHR where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceRayTracingFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (rayTracing))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (rayTracingShaderGroupHandleCaptureReplay))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (rayTracingShaderGroupHandleCaptureReplayMixed))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (rayTracingAccelerationStructureCaptureReplay))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (rayTracingIndirectTraceRays))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (rayTracingIndirectAccelerationStructureBuild))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (rayTracingHostAccelerationStructureCommands))
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (rayQuery))
    poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (rayTracingPrimitiveCulling))
    f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceRayTracingFeaturesKHR where
  peekCStruct p = do
    rayTracing <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    rayTracingShaderGroupHandleCaptureReplay <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    rayTracingShaderGroupHandleCaptureReplayMixed <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    rayTracingAccelerationStructureCaptureReplay <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    rayTracingIndirectTraceRays <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    rayTracingIndirectAccelerationStructureBuild <- peek @Bool32 ((p `plusPtr` 36 :: Ptr Bool32))
    rayTracingHostAccelerationStructureCommands <- peek @Bool32 ((p `plusPtr` 40 :: Ptr Bool32))
    rayQuery <- peek @Bool32 ((p `plusPtr` 44 :: Ptr Bool32))
    rayTracingPrimitiveCulling <- peek @Bool32 ((p `plusPtr` 48 :: Ptr Bool32))
    pure $ PhysicalDeviceRayTracingFeaturesKHR
             (bool32ToBool rayTracing) (bool32ToBool rayTracingShaderGroupHandleCaptureReplay) (bool32ToBool rayTracingShaderGroupHandleCaptureReplayMixed) (bool32ToBool rayTracingAccelerationStructureCaptureReplay) (bool32ToBool rayTracingIndirectTraceRays) (bool32ToBool rayTracingIndirectAccelerationStructureBuild) (bool32ToBool rayTracingHostAccelerationStructureCommands) (bool32ToBool rayQuery) (bool32ToBool rayTracingPrimitiveCulling)

instance Storable PhysicalDeviceRayTracingFeaturesKHR where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceRayTracingFeaturesKHR where
  zero = PhysicalDeviceRayTracingFeaturesKHR
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkPhysicalDeviceRayTracingPropertiesKHR - Properties of the physical
-- device for ray tracing
--
-- = Description
--
-- If the 'PhysicalDeviceRayTracingPropertiesKHR' structure is included in
-- the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- Limits specified by this structure /must/ match those specified with the
-- same name in
-- 'Vulkan.Extensions.VK_NV_ray_tracing.PhysicalDeviceRayTracingPropertiesNV'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceRayTracingPropertiesKHR = PhysicalDeviceRayTracingPropertiesKHR
  { -- | @shaderGroupHandleSize@ size in bytes of the shader header.
    shaderGroupHandleSize :: Word32
  , -- | @maxRecursionDepth@ is the maximum number of levels of recursion allowed
    -- in a trace command.
    maxRecursionDepth :: Word32
  , -- | @maxShaderGroupStride@ is the maximum stride in bytes allowed between
    -- shader groups in the SBT.
    maxShaderGroupStride :: Word32
  , -- | @shaderGroupBaseAlignment@ is the required alignment in bytes for the
    -- base of the SBTs.
    shaderGroupBaseAlignment :: Word32
  , -- | @maxGeometryCount@ is the maximum number of geometries in the bottom
    -- level acceleration structure.
    maxGeometryCount :: Word64
  , -- | @maxInstanceCount@ is the maximum number of instances in the top level
    -- acceleration structure.
    maxInstanceCount :: Word64
  , -- | @maxPrimitiveCount@ is the maximum number of triangles or AABBs in all
    -- geometries in the bottom level acceleration structure.
    maxPrimitiveCount :: Word64
  , -- | @maxDescriptorSetAccelerationStructures@ is the maximum number of
    -- acceleration structure descriptors that are allowed in a descriptor set.
    maxDescriptorSetAccelerationStructures :: Word32
  , -- | @shaderGroupHandleCaptureReplaySize@ is the number of bytes for the
    -- information required to do capture and replay for shader group handles.
    shaderGroupHandleCaptureReplaySize :: Word32
  }
  deriving (Typeable)
deriving instance Show PhysicalDeviceRayTracingPropertiesKHR

instance ToCStruct PhysicalDeviceRayTracingPropertiesKHR where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceRayTracingPropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (shaderGroupHandleSize)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxRecursionDepth)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (maxShaderGroupStride)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (shaderGroupBaseAlignment)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (maxGeometryCount)
    poke ((p `plusPtr` 40 :: Ptr Word64)) (maxInstanceCount)
    poke ((p `plusPtr` 48 :: Ptr Word64)) (maxPrimitiveCount)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (maxDescriptorSetAccelerationStructures)
    poke ((p `plusPtr` 60 :: Ptr Word32)) (shaderGroupHandleCaptureReplaySize)
    f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 60 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceRayTracingPropertiesKHR where
  peekCStruct p = do
    shaderGroupHandleSize <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxRecursionDepth <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    maxShaderGroupStride <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    shaderGroupBaseAlignment <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    maxGeometryCount <- peek @Word64 ((p `plusPtr` 32 :: Ptr Word64))
    maxInstanceCount <- peek @Word64 ((p `plusPtr` 40 :: Ptr Word64))
    maxPrimitiveCount <- peek @Word64 ((p `plusPtr` 48 :: Ptr Word64))
    maxDescriptorSetAccelerationStructures <- peek @Word32 ((p `plusPtr` 56 :: Ptr Word32))
    shaderGroupHandleCaptureReplaySize <- peek @Word32 ((p `plusPtr` 60 :: Ptr Word32))
    pure $ PhysicalDeviceRayTracingPropertiesKHR
             shaderGroupHandleSize maxRecursionDepth maxShaderGroupStride shaderGroupBaseAlignment maxGeometryCount maxInstanceCount maxPrimitiveCount maxDescriptorSetAccelerationStructures shaderGroupHandleCaptureReplaySize

instance Storable PhysicalDeviceRayTracingPropertiesKHR where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceRayTracingPropertiesKHR where
  zero = PhysicalDeviceRayTracingPropertiesKHR
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkStridedBufferRegionKHR - Structure specifying a region of a VkBuffer
-- with a stride
--
-- == Valid Usage
--
-- -   If @buffer@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @size@
--     plus @offset@ /must/ be less than or equal to the size of @buffer@
--
-- -   If @buffer@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @stride@ /must/ be less than the size of @buffer@
--
-- == Valid Usage (Implicit)
--
-- -   If @buffer@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @buffer@ /must/ be a valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Buffer', 'Vulkan.Core10.BaseType.DeviceSize',
-- 'cmdTraceRaysIndirectKHR', 'cmdTraceRaysKHR'
data StridedBufferRegionKHR = StridedBufferRegionKHR
  { -- | @buffer@ is the buffer containing this region.
    buffer :: Buffer
  , -- | @offset@ is the byte offset in @buffer@ at which the region starts.
    offset :: DeviceSize
  , -- | @stride@ is the byte stride between consecutive elements.
    stride :: DeviceSize
  , -- | @size@ is the size in bytes of the region starting at @offset@.
    size :: DeviceSize
  }
  deriving (Typeable)
deriving instance Show StridedBufferRegionKHR

instance ToCStruct StridedBufferRegionKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p StridedBufferRegionKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Buffer)) (buffer)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (offset)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (stride)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (size)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct StridedBufferRegionKHR where
  peekCStruct p = do
    buffer <- peek @Buffer ((p `plusPtr` 0 :: Ptr Buffer))
    offset <- peek @DeviceSize ((p `plusPtr` 8 :: Ptr DeviceSize))
    stride <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    size <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    pure $ StridedBufferRegionKHR
             buffer offset stride size

instance Storable StridedBufferRegionKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero StridedBufferRegionKHR where
  zero = StridedBufferRegionKHR
           zero
           zero
           zero
           zero


-- | VkTraceRaysIndirectCommandKHR - Structure specifying the parameters of
-- an indirect trace ray command
--
-- = Description
--
-- The members of 'TraceRaysIndirectCommandKHR' have the same meaning as
-- the similarly named parameters of 'cmdTraceRaysKHR'.
--
-- == Valid Usage
--
-- = See Also
--
-- No cross-references are available
data TraceRaysIndirectCommandKHR = TraceRaysIndirectCommandKHR
  { -- | @width@ /must/ be less than or equal to
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[0]
    width :: Word32
  , -- | @height@ /must/ be less than or equal to
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[1]
    height :: Word32
  , -- | @depth@ /must/ be less than or equal to
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[2]
    depth :: Word32
  }
  deriving (Typeable)
deriving instance Show TraceRaysIndirectCommandKHR

instance ToCStruct TraceRaysIndirectCommandKHR where
  withCStruct x f = allocaBytesAligned 12 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p TraceRaysIndirectCommandKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (width)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (height)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (depth)
    f
  cStructSize = 12
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    f

instance FromCStruct TraceRaysIndirectCommandKHR where
  peekCStruct p = do
    width <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    height <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    depth <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    pure $ TraceRaysIndirectCommandKHR
             width height depth

instance Storable TraceRaysIndirectCommandKHR where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero TraceRaysIndirectCommandKHR where
  zero = TraceRaysIndirectCommandKHR
           zero
           zero
           zero


-- | VkAccelerationStructureGeometryTrianglesDataKHR - Structure specifying a
-- triangle geometry in a bottom-level acceleration structure
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_TRIANGLES_DATA_KHR'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @vertexFormat@ /must/ be a valid 'Vulkan.Core10.Enums.Format.Format'
--     value
--
-- -   @vertexData@ /must/ be a valid 'DeviceOrHostAddressConstKHR' union
--
-- -   @indexType@ /must/ be a valid
--     'Vulkan.Core10.Enums.IndexType.IndexType' value
--
-- -   If @indexData@ is not @0@, @indexData@ /must/ be a valid
--     'DeviceOrHostAddressConstKHR' union
--
-- -   If @transformData@ is not @0@, @transformData@ /must/ be a valid
--     'DeviceOrHostAddressConstKHR' union
--
-- = See Also
--
-- 'AccelerationStructureGeometryDataKHR', 'DeviceOrHostAddressConstKHR',
-- 'Vulkan.Core10.BaseType.DeviceSize',
-- 'Vulkan.Core10.Enums.Format.Format',
-- 'Vulkan.Core10.Enums.IndexType.IndexType',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data AccelerationStructureGeometryTrianglesDataKHR = AccelerationStructureGeometryTrianglesDataKHR
  { -- | @vertexFormat@ is the 'Vulkan.Core10.Enums.Format.Format' of each vertex
    -- element.
    vertexFormat :: Format
  , -- | @vertexData@ is a device or host address to memory containing vertex
    -- data for this geometry.
    vertexData :: DeviceOrHostAddressConstKHR
  , -- | @vertexStride@ is the stride in bytes between each vertex.
    vertexStride :: DeviceSize
  , -- | @indexType@ is the 'Vulkan.Core10.Enums.IndexType.IndexType' of each
    -- index element.
    indexType :: IndexType
  , -- | @indexData@ is the device or host address to memory containing index
    -- data for this geometry.
    indexData :: DeviceOrHostAddressConstKHR
  , -- | @transformData@ is a device or host address to memory containing an
    -- optional reference to a 'TransformMatrixKHR' structure defining a
    -- transformation that should be applied to vertices in this geometry.
    transformData :: DeviceOrHostAddressConstKHR
  }
  deriving (Typeable)
deriving instance Show AccelerationStructureGeometryTrianglesDataKHR

instance ToCStruct AccelerationStructureGeometryTrianglesDataKHR where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureGeometryTrianglesDataKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_TRIANGLES_DATA_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Format)) (vertexFormat)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr DeviceOrHostAddressConstKHR)) (vertexData) . ($ ())
    lift $ poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (vertexStride)
    lift $ poke ((p `plusPtr` 40 :: Ptr IndexType)) (indexType)
    ContT $ pokeCStruct ((p `plusPtr` 48 :: Ptr DeviceOrHostAddressConstKHR)) (indexData) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 56 :: Ptr DeviceOrHostAddressConstKHR)) (transformData) . ($ ())
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_TRIANGLES_DATA_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Format)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr DeviceOrHostAddressConstKHR)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr IndexType)) (zero)
    lift $ f

instance Zero AccelerationStructureGeometryTrianglesDataKHR where
  zero = AccelerationStructureGeometryTrianglesDataKHR
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkAccelerationStructureGeometryAabbsDataKHR - Structure specifying
-- axis-aligned bounding box geometry in a bottom-level acceleration
-- structure
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'AccelerationStructureGeometryDataKHR', 'DeviceOrHostAddressConstKHR',
-- 'Vulkan.Core10.BaseType.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data AccelerationStructureGeometryAabbsDataKHR = AccelerationStructureGeometryAabbsDataKHR
  { -- | @data@ /must/ be a valid 'DeviceOrHostAddressConstKHR' union
    data' :: DeviceOrHostAddressConstKHR
  , -- | @stride@ /must/ be a multiple of @8@
    stride :: DeviceSize
  }
  deriving (Typeable)
deriving instance Show AccelerationStructureGeometryAabbsDataKHR

instance ToCStruct AccelerationStructureGeometryAabbsDataKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureGeometryAabbsDataKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_AABBS_DATA_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr DeviceOrHostAddressConstKHR)) (data') . ($ ())
    lift $ poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (stride)
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_AABBS_DATA_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr DeviceOrHostAddressConstKHR)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    lift $ f

instance Zero AccelerationStructureGeometryAabbsDataKHR where
  zero = AccelerationStructureGeometryAabbsDataKHR
           zero
           zero


-- | VkAccelerationStructureGeometryInstancesDataKHR - Structure specifying a
-- geometry consisting of instances of other acceleration structures
--
-- == Valid Usage
--
-- -   @data@ /must/ be aligned to @16@ bytes
--
-- -   If @arrayOfPointers@ is true, each pointer /must/ be aligned to @16@
--     bytes
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_INSTANCES_DATA_KHR'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @data@ /must/ be a valid 'DeviceOrHostAddressConstKHR' union
--
-- = See Also
--
-- 'AccelerationStructureGeometryDataKHR', 'Vulkan.Core10.BaseType.Bool32',
-- 'DeviceOrHostAddressConstKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data AccelerationStructureGeometryInstancesDataKHR = AccelerationStructureGeometryInstancesDataKHR
  { -- | @arrayOfPointers@ specifies whether @data@ is used as an array of
    -- addresses or just an array.
    arrayOfPointers :: Bool
  , -- | @data@ is either the address of an array of device or host addresses
    -- referencing individual 'AccelerationStructureInstanceKHR' structures if
    -- @arrayOfPointers@ is 'Vulkan.Core10.BaseType.TRUE', or the address of an
    -- array of 'AccelerationStructureInstanceKHR' structures.
    data' :: DeviceOrHostAddressConstKHR
  }
  deriving (Typeable)
deriving instance Show AccelerationStructureGeometryInstancesDataKHR

instance ToCStruct AccelerationStructureGeometryInstancesDataKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureGeometryInstancesDataKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_INSTANCES_DATA_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (arrayOfPointers))
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr DeviceOrHostAddressConstKHR)) (data') . ($ ())
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_INSTANCES_DATA_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr DeviceOrHostAddressConstKHR)) (zero) . ($ ())
    lift $ f

instance Zero AccelerationStructureGeometryInstancesDataKHR where
  zero = AccelerationStructureGeometryInstancesDataKHR
           zero
           zero


-- | VkAccelerationStructureGeometryKHR - Structure specifying geometries to
-- be built into an acceleration structure
--
-- == Valid Usage
--
-- -   If @geometryType@ is 'GEOMETRY_TYPE_AABBS_KHR', the @aabbs@ member
--     of @geometry@ /must/ be a valid
--     'AccelerationStructureGeometryAabbsDataKHR' structure
--
-- -   If @geometryType@ is 'GEOMETRY_TYPE_TRIANGLES_KHR', the @triangles@
--     member of @geometry@ /must/ be a valid
--     'AccelerationStructureGeometryTrianglesDataKHR' structure
--
-- -   If @geometryType@ is 'GEOMETRY_TYPE_INSTANCES_KHR', the @instances@
--     member of @geometry@ /must/ be a valid
--     'AccelerationStructureGeometryInstancesDataKHR' structure
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @geometryType@ /must/ be a valid 'GeometryTypeKHR' value
--
-- -   @geometry@ /must/ be a valid 'AccelerationStructureGeometryDataKHR'
--     union
--
-- -   @flags@ /must/ be a valid combination of 'GeometryFlagBitsKHR'
--     values
--
-- = See Also
--
-- 'AccelerationStructureBuildGeometryInfoKHR',
-- 'AccelerationStructureGeometryDataKHR', 'GeometryFlagsKHR',
-- 'GeometryTypeKHR', 'Vulkan.Core10.Enums.StructureType.StructureType'
data AccelerationStructureGeometryKHR = AccelerationStructureGeometryKHR
  { -- | @geometryType@ describes which type of geometry this
    -- 'AccelerationStructureGeometryKHR' refers to.
    geometryType :: GeometryTypeKHR
  , -- | @geometry@ is a 'AccelerationStructureGeometryDataKHR' union describing
    -- the geometry data for the relevant geometry type.
    geometry :: AccelerationStructureGeometryDataKHR
  , -- | @flags@ is a bitmask of 'GeometryFlagBitsKHR' values describing
    -- additional properties of how the geometry should be built.
    flags :: GeometryFlagsKHR
  }
  deriving (Typeable)
deriving instance Show AccelerationStructureGeometryKHR

instance ToCStruct AccelerationStructureGeometryKHR where
  withCStruct x f = allocaBytesAligned 96 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureGeometryKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr GeometryTypeKHR)) (geometryType)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr AccelerationStructureGeometryDataKHR)) (geometry) . ($ ())
    lift $ poke ((p `plusPtr` 88 :: Ptr GeometryFlagsKHR)) (flags)
    lift $ f
  cStructSize = 96
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr GeometryTypeKHR)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr AccelerationStructureGeometryDataKHR)) (zero) . ($ ())
    lift $ f

instance Zero AccelerationStructureGeometryKHR where
  zero = AccelerationStructureGeometryKHR
           zero
           zero
           zero


-- | VkAccelerationStructureBuildGeometryInfoKHR - Structure specifying the
-- geometry data used to build an acceleration structure
--
-- = Description
--
-- Note
--
-- Elements of @ppGeometries@ are accessed as follows, based on
-- @geometryArrayOfPointers@:
--
-- > if (geometryArrayOfPointers) {
-- >     use *(ppGeometries[i]);
-- > } else {
-- >     use (*ppGeometries)[i];
-- > }
--
-- == Valid Usage
--
-- -   If @update@ is 'Vulkan.Core10.BaseType.TRUE',
--     @srcAccelerationStructure@ /must/ not be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   If @update@ is 'Vulkan.Core10.BaseType.TRUE',
--     @srcAccelerationStructure@ /must/ have been built before with
--     'BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR' set in
--     'AccelerationStructureBuildGeometryInfoKHR'::@flags@
--
-- -   @scratchData@ /must/ have been created with
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_RAY_TRACING_BIT_KHR'
--     usage flag
--
-- -   If @update@ is 'Vulkan.Core10.BaseType.TRUE', the
--     @srcAccelerationStructure@ and @dstAccelerationStructure@ objects
--     /must/ either be the same object or not have any
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-memory-aliasing memory aliasing>
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR'
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_KHR_deferred_host_operations.DeferredOperationInfoKHR'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   @type@ /must/ be a valid 'AccelerationStructureTypeKHR' value
--
-- -   @flags@ /must/ be a valid combination of
--     'BuildAccelerationStructureFlagBitsKHR' values
--
-- -   If @srcAccelerationStructure@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @srcAccelerationStructure@
--     /must/ be a valid
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR' handle
--
-- -   @dstAccelerationStructure@ /must/ be a valid
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR' handle
--
-- -   @scratchData@ /must/ be a valid 'DeviceOrHostAddressKHR' union
--
-- -   Both of @dstAccelerationStructure@, and @srcAccelerationStructure@
--     that are valid handles of non-ignored parameters /must/ have been
--     created, allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'AccelerationStructureGeometryKHR',
-- 'Vulkan.Extensions.Handles.AccelerationStructureKHR',
-- 'AccelerationStructureTypeKHR', 'Vulkan.Core10.BaseType.Bool32',
-- 'BuildAccelerationStructureFlagsKHR', 'DeviceOrHostAddressKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'buildAccelerationStructureKHR',
-- 'cmdBuildAccelerationStructureIndirectKHR',
-- 'cmdBuildAccelerationStructureKHR'
data AccelerationStructureBuildGeometryInfoKHR (es :: [Type]) = AccelerationStructureBuildGeometryInfoKHR
  { -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
    next :: Chain es
  , -- | @type@ is a 'AccelerationStructureTypeKHR' value specifying the type of
    -- acceleration structure being built.
    type' :: AccelerationStructureTypeKHR
  , -- | @flags@ is a bitmask of 'BuildAccelerationStructureFlagBitsKHR'
    -- specifying additional parameters of the acceleration structure.
    flags :: BuildAccelerationStructureFlagsKHR
  , -- | @update@ specifies whether to update @dstAccelerationStructure@ with the
    -- data in @srcAccelerationStructure@ or not.
    update :: Bool
  , -- | @srcAccelerationStructure@ points to an existing acceleration structure
    -- that is to be used to update the @dst@ acceleration structure when
    -- @update@ is 'Vulkan.Core10.BaseType.TRUE'.
    srcAccelerationStructure :: AccelerationStructureKHR
  , -- | @dstAccelerationStructure@ points to the target acceleration structure
    -- for the build.
    dstAccelerationStructure :: AccelerationStructureKHR
  , -- | @ppGeometries@ is either a pointer to an array of pointers to
    -- 'AccelerationStructureGeometryKHR' structures if
    -- @geometryArrayOfPointers@ is 'Vulkan.Core10.BaseType.TRUE', or a pointer
    -- to a pointer to an array of 'AccelerationStructureGeometryKHR'
    -- structures if it is 'Vulkan.Core10.BaseType.FALSE'. Each element of the
    -- array describes the data used to build each acceleration structure
    -- geometry.
    geometries :: Vector AccelerationStructureGeometryKHR
  , -- | @scratchData@ is the device or host address to memory that will be used
    -- as scratch memory for the build.
    scratchData :: DeviceOrHostAddressKHR
  }
  deriving (Typeable)
deriving instance Show (Chain es) => Show (AccelerationStructureBuildGeometryInfoKHR es)

instance Extensible AccelerationStructureBuildGeometryInfoKHR where
  extensibleType = STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR
  setNext x next = x{next = next}
  getNext AccelerationStructureBuildGeometryInfoKHR{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends AccelerationStructureBuildGeometryInfoKHR e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @DeferredOperationInfoKHR = Just f
    | otherwise = Nothing

instance PokeChain es => ToCStruct (AccelerationStructureBuildGeometryInfoKHR es) where
  withCStruct x f = allocaBytesAligned 72 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureBuildGeometryInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr AccelerationStructureTypeKHR)) (type')
    lift $ poke ((p `plusPtr` 20 :: Ptr BuildAccelerationStructureFlagsKHR)) (flags)
    lift $ poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (update))
    lift $ poke ((p `plusPtr` 32 :: Ptr AccelerationStructureKHR)) (srcAccelerationStructure)
    lift $ poke ((p `plusPtr` 40 :: Ptr AccelerationStructureKHR)) (dstAccelerationStructure)
    lift $ poke ((p `plusPtr` 48 :: Ptr Bool32)) (FALSE)
    lift $ poke ((p `plusPtr` 52 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (geometries)) :: Word32))
    pPpGeometries' <- ContT $ allocaBytesAligned @AccelerationStructureGeometryKHR ((Data.Vector.length (geometries)) * 96) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPpGeometries' `plusPtr` (96 * (i)) :: Ptr AccelerationStructureGeometryKHR) (e) . ($ ())) (geometries)
    ppGeometries'' <- ContT $ with (pPpGeometries')
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr (Ptr AccelerationStructureGeometryKHR)))) ppGeometries''
    ContT $ pokeCStruct ((p `plusPtr` 64 :: Ptr DeviceOrHostAddressKHR)) (scratchData) . ($ ())
    lift $ f
  cStructSize = 72
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr AccelerationStructureTypeKHR)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 40 :: Ptr AccelerationStructureKHR)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 64 :: Ptr DeviceOrHostAddressKHR)) (zero) . ($ ())
    lift $ f

instance es ~ '[] => Zero (AccelerationStructureBuildGeometryInfoKHR es) where
  zero = AccelerationStructureBuildGeometryInfoKHR
           ()
           zero
           zero
           zero
           zero
           zero
           mempty
           zero


-- | VkAccelerationStructureBuildOffsetInfoKHR - Structure specifying build
-- offsets and counts for acceleration structure builds
--
-- = Description
--
-- The primitive count and primitive offset are interpreted differently
-- depending on the 'GeometryTypeKHR' used:
--
-- -   For geometries of type 'GEOMETRY_TYPE_TRIANGLES_KHR',
--     @primitiveCount@ is the number of triangles to be built, where each
--     triangle is treated as 3 vertices.
--
--     -   If the geometry uses indices, @primitiveCount@ × 3 indices are
--         consumed from
--         'AccelerationStructureGeometryTrianglesDataKHR'::@indexData@,
--         starting at an offset of @primitiveOffset@. The value of
--         @firstVertex@ is added to the index values before fetching
--         vertices.
--
--     -   If the geometry does not use indices, @primitiveCount@ × 3
--         vertices are consumed from
--         'AccelerationStructureGeometryTrianglesDataKHR'::@vertexData@,
--         starting at an offset of @primitiveOffset@ +
--         'AccelerationStructureGeometryTrianglesDataKHR'::@vertexStride@
--         × @firstVertex@.
--
--     -   A single 'TransformMatrixKHR' structure is consumed from
--         'AccelerationStructureGeometryTrianglesDataKHR'::@transformData@,
--         at an offset of @transformOffset@. This transformation matrix is
--         used by all triangles.
--
-- -   For geometries of type 'GEOMETRY_TYPE_AABBS_KHR', @primitiveCount@
--     is the number of axis-aligned bounding boxes. @primitiveCount@
--     'AabbPositionsKHR' structures are consumed from
--     'AccelerationStructureGeometryAabbsDataKHR'::@data@, starting at an
--     offset of @primitiveOffset@.
--
-- -   For geometries of type 'GEOMETRY_TYPE_INSTANCES_KHR',
--     @primitiveCount@ is the number of acceleration structures.
--     @primitiveCount@ 'AccelerationStructureInstanceKHR' structures are
--     consumed from
--     'AccelerationStructureGeometryInstancesDataKHR'::@data@, starting at
--     an offset of @primitiveOffset@.
--
-- == Valid Usage
--
-- -   For geometries of type 'GEOMETRY_TYPE_TRIANGLES_KHR', if the
--     geometry uses indices, the offset @primitiveOffset@ from
--     'AccelerationStructureGeometryTrianglesDataKHR'::@indexData@ /must/
--     be a multiple of the element size of
--     'AccelerationStructureGeometryTrianglesDataKHR'::@indexType@
--
-- -   For geometries of type 'GEOMETRY_TYPE_TRIANGLES_KHR', if the
--     geometry doesn’t use indices, the offset @primitiveOffset@ from
--     'AccelerationStructureGeometryTrianglesDataKHR'::@vertexData@ /must/
--     be a multiple of the component size of
--     'AccelerationStructureGeometryTrianglesDataKHR'::@vertexType@
--
-- -   For geometries of type 'GEOMETRY_TYPE_TRIANGLES_KHR', the offset
--     @transformOffset@ from
--     'AccelerationStructureGeometryTrianglesDataKHR'::@transformData@
--     /must/ be a multiple of 16
--
-- -   For geometries of type 'GEOMETRY_TYPE_AABBS_KHR', the offset
--     @primitiveOffset@ from
--     'AccelerationStructureGeometryAabbsDataKHR'::@data@ /must/ be a
--     multiple of 8
--
-- -   For geometries of type 'GEOMETRY_TYPE_INSTANCES_KHR', the offset
--     @primitiveOffset@ from
--     'AccelerationStructureGeometryInstancesDataKHR'::@data@ /must/ be a
--     multiple of 16 \/\/ TODO - Almost certainly should be more here
--
-- = See Also
--
-- 'buildAccelerationStructureKHR', 'cmdBuildAccelerationStructureKHR'
data AccelerationStructureBuildOffsetInfoKHR = AccelerationStructureBuildOffsetInfoKHR
  { -- | @primitiveCount@ defines the number of primitives for a corresponding
    -- acceleration structure geometry.
    primitiveCount :: Word32
  , -- | @primitiveOffset@ defines an offset in bytes into the memory where
    -- primitive data is defined.
    primitiveOffset :: Word32
  , -- | @firstVertex@ is the index of the first vertex to build from for
    -- triangle geometry.
    firstVertex :: Word32
  , -- | @transformOffset@ defines an offset in bytes into the memory where a
    -- transform matrix is defined.
    transformOffset :: Word32
  }
  deriving (Typeable)
deriving instance Show AccelerationStructureBuildOffsetInfoKHR

instance ToCStruct AccelerationStructureBuildOffsetInfoKHR where
  withCStruct x f = allocaBytesAligned 16 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureBuildOffsetInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (primitiveCount)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (primitiveOffset)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (firstVertex)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (transformOffset)
    f
  cStructSize = 16
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    f

instance FromCStruct AccelerationStructureBuildOffsetInfoKHR where
  peekCStruct p = do
    primitiveCount <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    primitiveOffset <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    firstVertex <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    transformOffset <- peek @Word32 ((p `plusPtr` 12 :: Ptr Word32))
    pure $ AccelerationStructureBuildOffsetInfoKHR
             primitiveCount primitiveOffset firstVertex transformOffset

instance Storable AccelerationStructureBuildOffsetInfoKHR where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AccelerationStructureBuildOffsetInfoKHR where
  zero = AccelerationStructureBuildOffsetInfoKHR
           zero
           zero
           zero
           zero


-- | VkAccelerationStructureCreateGeometryTypeInfoKHR - Structure specifying
-- the shape of geometries that will be built into an acceleration
-- structure
--
-- = Description
--
-- When @geometryType@ is 'GEOMETRY_TYPE_TRIANGLES_KHR':
--
-- -   if @indexType@ is
--     'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_NONE_KHR', then this
--     structure describes a set of triangles.
--
-- -   if @indexType@ is not
--     'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_NONE_KHR', then this
--     structure describes a set of indexed triangles.
--
-- == Valid Usage
--
-- -   If @geometryType@ is 'GEOMETRY_TYPE_TRIANGLES_KHR', @vertexFormat@
--     /must/ support the
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_ACCELERATION_STRUCTURE_VERTEX_BUFFER_BIT_KHR'
--     in
--     'Vulkan.Core10.DeviceInitialization.FormatProperties'::@bufferFeatures@
--     as returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFormatProperties2'
--
-- -   If @geometryType@ is 'GEOMETRY_TYPE_TRIANGLES_KHR', @indexType@
--     /must/ be 'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT16',
--     'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT32', or
--     'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_NONE_KHR'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_GEOMETRY_TYPE_INFO_KHR'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @geometryType@ /must/ be a valid 'GeometryTypeKHR' value
--
-- -   @indexType@ /must/ be a valid
--     'Vulkan.Core10.Enums.IndexType.IndexType' value
--
-- -   If @vertexFormat@ is not @0@, @vertexFormat@ /must/ be a valid
--     'Vulkan.Core10.Enums.Format.Format' value
--
-- = See Also
--
-- 'AccelerationStructureCreateInfoKHR', 'Vulkan.Core10.BaseType.Bool32',
-- 'Vulkan.Core10.Enums.Format.Format', 'GeometryTypeKHR',
-- 'Vulkan.Core10.Enums.IndexType.IndexType',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data AccelerationStructureCreateGeometryTypeInfoKHR = AccelerationStructureCreateGeometryTypeInfoKHR
  { -- | @geometryType@ is a 'GeometryTypeKHR' that describes the type of an
    -- acceleration structure geometry.
    geometryType :: GeometryTypeKHR
  , -- | @maxPrimitiveCount@ describes the maximum number of primitives that
    -- /can/ be built into an acceleration structure geometry.
    maxPrimitiveCount :: Word32
  , -- | @indexType@ is a 'Vulkan.Core10.Enums.IndexType.IndexType' that
    -- describes the index type used to build this geometry when @geometryType@
    -- is 'GEOMETRY_TYPE_TRIANGLES_KHR'.
    indexType :: IndexType
  , -- | @maxVertexCount@ describes the maximum vertex count that /can/ be used
    -- to build an acceleration structure geometry when @geometryType@ is
    -- 'GEOMETRY_TYPE_TRIANGLES_KHR'.
    maxVertexCount :: Word32
  , -- | @vertexFormat@ is a 'Vulkan.Core10.Enums.Format.Format' that describes
    -- the vertex format used to build this geometry when @geometryType@ is
    -- 'GEOMETRY_TYPE_TRIANGLES_KHR'.
    vertexFormat :: Format
  , -- | @allowsTransforms@ indicates whether transform data /can/ be used by
    -- this acceleration structure or not, when @geometryType@ is
    -- 'GEOMETRY_TYPE_TRIANGLES_KHR'.
    allowsTransforms :: Bool
  }
  deriving (Typeable)
deriving instance Show AccelerationStructureCreateGeometryTypeInfoKHR

instance ToCStruct AccelerationStructureCreateGeometryTypeInfoKHR where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureCreateGeometryTypeInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_GEOMETRY_TYPE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr GeometryTypeKHR)) (geometryType)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxPrimitiveCount)
    poke ((p `plusPtr` 24 :: Ptr IndexType)) (indexType)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (maxVertexCount)
    poke ((p `plusPtr` 32 :: Ptr Format)) (vertexFormat)
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (allowsTransforms))
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_GEOMETRY_TYPE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr GeometryTypeKHR)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr IndexType)) (zero)
    f

instance FromCStruct AccelerationStructureCreateGeometryTypeInfoKHR where
  peekCStruct p = do
    geometryType <- peek @GeometryTypeKHR ((p `plusPtr` 16 :: Ptr GeometryTypeKHR))
    maxPrimitiveCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    indexType <- peek @IndexType ((p `plusPtr` 24 :: Ptr IndexType))
    maxVertexCount <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    vertexFormat <- peek @Format ((p `plusPtr` 32 :: Ptr Format))
    allowsTransforms <- peek @Bool32 ((p `plusPtr` 36 :: Ptr Bool32))
    pure $ AccelerationStructureCreateGeometryTypeInfoKHR
             geometryType maxPrimitiveCount indexType maxVertexCount vertexFormat (bool32ToBool allowsTransforms)

instance Storable AccelerationStructureCreateGeometryTypeInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AccelerationStructureCreateGeometryTypeInfoKHR where
  zero = AccelerationStructureCreateGeometryTypeInfoKHR
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkAccelerationStructureCreateInfoKHR - Structure specifying the
-- parameters of a newly created acceleration structure object
--
-- = Description
--
-- If @deviceAddress@ is zero, no specific address is requested.
--
-- If @deviceAddress@ is not zero, @deviceAddress@ /must/ be an address
-- retrieved from an identically created acceleration structure on the same
-- implementation. The acceleration structure /must/ also be bound to an
-- identically created 'Vulkan.Core10.Handles.DeviceMemory' object.
--
-- Apps /should/ avoid creating acceleration structures with app-provided
-- addresses and implementation-provided addresses in the same process, to
-- reduce the likelihood of
-- 'Vulkan.Extensions.VK_KHR_buffer_device_address.ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS_KHR'
-- errors.
--
-- == Valid Usage
--
-- -   If @compactedSize@ is not @0@ then @maxGeometryCount@ /must/ be @0@
--
-- -   If @compactedSize@ is @0@ then @maxGeometryCount@ /must/ not be @0@
--
-- -   If @type@ is 'ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR' then
--     @maxGeometryCount@ /must/ be less than or equal to
--     'PhysicalDeviceRayTracingPropertiesKHR'::@maxGeometryCount@
--
-- -   If @type@ is 'ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR' then the
--     @maxPrimitiveCount@ member of each element of the @pGeometryInfos@
--     array /must/ be less than or equal to
--     'PhysicalDeviceRayTracingPropertiesKHR'::@maxInstanceCount@
--
-- -   The total number of triangles in all geometries /must/ be less than
--     or equal to
--     'PhysicalDeviceRayTracingPropertiesKHR'::@maxPrimitiveCount@
--
-- -   The total number of AABBs in all geometries /must/ be less than or
--     equal to
--     'PhysicalDeviceRayTracingPropertiesKHR'::@maxPrimitiveCount@
--
-- -   If @type@ is 'ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR' and
--     @compactedSize@ is @0@, @maxGeometryCount@ /must/ be @1@
--
-- -   If @type@ is 'ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR' and
--     @compactedSize@ is @0@, the @geometryType@ member of elements of
--     @pGeometryInfos@ /must/ be 'GEOMETRY_TYPE_INSTANCES_KHR'
--
-- -   If @type@ is 'ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR' and
--     @compactedSize@ is @0@, the @geometryType@ member of elements of
--     @pGeometryInfos@ /must/ not be 'GEOMETRY_TYPE_INSTANCES_KHR'
--
-- -   If @type@ is 'ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR' then the
--     @geometryType@ member of each geometry in @pGeometryInfos@ /must/ be
--     the same
--
-- -   If @flags@ has the
--     'BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR' bit set,
--     then it /must/ not have the
--     'BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_KHR' bit set
--
-- -   If @deviceAddress@ is not @0@,
--     'PhysicalDeviceRayTracingFeaturesKHR'::@rayTracingAccelerationStructureCaptureReplay@
--     /must/ be 'Vulkan.Core10.BaseType.TRUE'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_KHR'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @type@ /must/ be a valid 'AccelerationStructureTypeKHR' value
--
-- -   @flags@ /must/ be a valid combination of
--     'BuildAccelerationStructureFlagBitsKHR' values
--
-- -   If @maxGeometryCount@ is not @0@, @pGeometryInfos@ /must/ be a valid
--     pointer to an array of @maxGeometryCount@ valid
--     'AccelerationStructureCreateGeometryTypeInfoKHR' structures
--
-- = See Also
--
-- 'AccelerationStructureCreateGeometryTypeInfoKHR',
-- 'AccelerationStructureTypeKHR', 'BuildAccelerationStructureFlagsKHR',
-- 'Vulkan.Core10.BaseType.DeviceAddress',
-- 'Vulkan.Core10.BaseType.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createAccelerationStructureKHR'
data AccelerationStructureCreateInfoKHR = AccelerationStructureCreateInfoKHR
  { -- | @compactedSize@ is the size from the result of
    -- 'cmdWriteAccelerationStructuresPropertiesKHR' if this acceleration
    -- structure is going to be the target of a compacting copy.
    compactedSize :: DeviceSize
  , -- | @type@ is a 'AccelerationStructureTypeKHR' value specifying the type of
    -- acceleration structure that will be created.
    type' :: AccelerationStructureTypeKHR
  , -- | @flags@ is a bitmask of 'BuildAccelerationStructureFlagBitsKHR'
    -- specifying additional parameters of the acceleration structure.
    flags :: BuildAccelerationStructureFlagsKHR
  , -- | @pGeometryInfos@ is an array of @maxGeometryCount@
    -- 'AccelerationStructureCreateGeometryTypeInfoKHR' structures, which
    -- describe the maximum size and format of the data that will be built into
    -- the acceleration structure.
    geometryInfos :: Vector AccelerationStructureCreateGeometryTypeInfoKHR
  , -- | @deviceAddress@ is the device address requested for the acceleration
    -- structure if the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-raytracing-ascapturereplay rayTracingAccelerationStructureCaptureReplay>
    -- feature is being used.
    deviceAddress :: DeviceAddress
  }
  deriving (Typeable)
deriving instance Show AccelerationStructureCreateInfoKHR

instance ToCStruct AccelerationStructureCreateInfoKHR where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureCreateInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (compactedSize)
    lift $ poke ((p `plusPtr` 24 :: Ptr AccelerationStructureTypeKHR)) (type')
    lift $ poke ((p `plusPtr` 28 :: Ptr BuildAccelerationStructureFlagsKHR)) (flags)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (geometryInfos)) :: Word32))
    pPGeometryInfos' <- ContT $ allocaBytesAligned @AccelerationStructureCreateGeometryTypeInfoKHR ((Data.Vector.length (geometryInfos)) * 40) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPGeometryInfos' `plusPtr` (40 * (i)) :: Ptr AccelerationStructureCreateGeometryTypeInfoKHR) (e) . ($ ())) (geometryInfos)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr AccelerationStructureCreateGeometryTypeInfoKHR))) (pPGeometryInfos')
    lift $ poke ((p `plusPtr` 48 :: Ptr DeviceAddress)) (deviceAddress)
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr AccelerationStructureTypeKHR)) (zero)
    pPGeometryInfos' <- ContT $ allocaBytesAligned @AccelerationStructureCreateGeometryTypeInfoKHR ((Data.Vector.length (mempty)) * 40) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPGeometryInfos' `plusPtr` (40 * (i)) :: Ptr AccelerationStructureCreateGeometryTypeInfoKHR) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr AccelerationStructureCreateGeometryTypeInfoKHR))) (pPGeometryInfos')
    lift $ f

instance FromCStruct AccelerationStructureCreateInfoKHR where
  peekCStruct p = do
    compactedSize <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    type' <- peek @AccelerationStructureTypeKHR ((p `plusPtr` 24 :: Ptr AccelerationStructureTypeKHR))
    flags <- peek @BuildAccelerationStructureFlagsKHR ((p `plusPtr` 28 :: Ptr BuildAccelerationStructureFlagsKHR))
    maxGeometryCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pGeometryInfos <- peek @(Ptr AccelerationStructureCreateGeometryTypeInfoKHR) ((p `plusPtr` 40 :: Ptr (Ptr AccelerationStructureCreateGeometryTypeInfoKHR)))
    pGeometryInfos' <- generateM (fromIntegral maxGeometryCount) (\i -> peekCStruct @AccelerationStructureCreateGeometryTypeInfoKHR ((pGeometryInfos `advancePtrBytes` (40 * (i)) :: Ptr AccelerationStructureCreateGeometryTypeInfoKHR)))
    deviceAddress <- peek @DeviceAddress ((p `plusPtr` 48 :: Ptr DeviceAddress))
    pure $ AccelerationStructureCreateInfoKHR
             compactedSize type' flags pGeometryInfos' deviceAddress

instance Zero AccelerationStructureCreateInfoKHR where
  zero = AccelerationStructureCreateInfoKHR
           zero
           zero
           zero
           mempty
           zero


-- | VkAabbPositionsKHR - Structure specifying two opposing corners of an
-- axis-aligned bounding box
--
-- == Valid Usage
--
-- = See Also
--
-- No cross-references are available
data AabbPositionsKHR = AabbPositionsKHR
  { -- | @minX@ /must/ be less than or equal to @maxX@
    minX :: Float
  , -- | @minY@ /must/ be less than or equal to @maxY@
    minY :: Float
  , -- | @minZ@ /must/ be less than or equal to @maxZ@
    minZ :: Float
  , -- | @maxX@ is the x position of the other opposing corner of a bounding box.
    maxX :: Float
  , -- | @maxY@ is the y position of the other opposing corner of a bounding box.
    maxY :: Float
  , -- | @maxZ@ is the z position of the other opposing corner of a bounding box.
    maxZ :: Float
  }
  deriving (Typeable)
deriving instance Show AabbPositionsKHR

instance ToCStruct AabbPositionsKHR where
  withCStruct x f = allocaBytesAligned 24 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AabbPositionsKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (minX))
    poke ((p `plusPtr` 4 :: Ptr CFloat)) (CFloat (minY))
    poke ((p `plusPtr` 8 :: Ptr CFloat)) (CFloat (minZ))
    poke ((p `plusPtr` 12 :: Ptr CFloat)) (CFloat (maxX))
    poke ((p `plusPtr` 16 :: Ptr CFloat)) (CFloat (maxY))
    poke ((p `plusPtr` 20 :: Ptr CFloat)) (CFloat (maxZ))
    f
  cStructSize = 24
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 4 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 8 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 12 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 16 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 20 :: Ptr CFloat)) (CFloat (zero))
    f

instance FromCStruct AabbPositionsKHR where
  peekCStruct p = do
    minX <- peek @CFloat ((p `plusPtr` 0 :: Ptr CFloat))
    minY <- peek @CFloat ((p `plusPtr` 4 :: Ptr CFloat))
    minZ <- peek @CFloat ((p `plusPtr` 8 :: Ptr CFloat))
    maxX <- peek @CFloat ((p `plusPtr` 12 :: Ptr CFloat))
    maxY <- peek @CFloat ((p `plusPtr` 16 :: Ptr CFloat))
    maxZ <- peek @CFloat ((p `plusPtr` 20 :: Ptr CFloat))
    pure $ AabbPositionsKHR
             ((\(CFloat a) -> a) minX) ((\(CFloat a) -> a) minY) ((\(CFloat a) -> a) minZ) ((\(CFloat a) -> a) maxX) ((\(CFloat a) -> a) maxY) ((\(CFloat a) -> a) maxZ)

instance Storable AabbPositionsKHR where
  sizeOf ~_ = 24
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AabbPositionsKHR where
  zero = AabbPositionsKHR
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkTransformMatrixKHR - Structure specifying a 3x4 affine transformation
-- matrix
--
-- = See Also
--
-- 'AccelerationStructureInstanceKHR'
data TransformMatrixKHR = TransformMatrixKHR
  { -- | @matrix@ is a 3x4 row-major affine transformation matrix.
    matrix :: ((Float, Float, Float, Float), (Float, Float, Float, Float), (Float, Float, Float, Float)) }
  deriving (Typeable)
deriving instance Show TransformMatrixKHR

instance ToCStruct TransformMatrixKHR where
  withCStruct x f = allocaBytesAligned 48 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p TransformMatrixKHR{..} f = do
    let pMatrix' = lowerArrayPtr ((p `plusPtr` 0 :: Ptr (FixedArray 3 (FixedArray 4 CFloat))))
    case (matrix) of
      (e0, e1, e2) -> do
        let pMatrix0 = lowerArrayPtr (pMatrix' :: Ptr (FixedArray 4 CFloat))
        case (e0) of
          (e0', e1', e2', e3) -> do
            poke (pMatrix0 :: Ptr CFloat) (CFloat (e0'))
            poke (pMatrix0 `plusPtr` 4 :: Ptr CFloat) (CFloat (e1'))
            poke (pMatrix0 `plusPtr` 8 :: Ptr CFloat) (CFloat (e2'))
            poke (pMatrix0 `plusPtr` 12 :: Ptr CFloat) (CFloat (e3))
        let pMatrix1 = lowerArrayPtr (pMatrix' `plusPtr` 16 :: Ptr (FixedArray 4 CFloat))
        case (e1) of
          (e0', e1', e2', e3) -> do
            poke (pMatrix1 :: Ptr CFloat) (CFloat (e0'))
            poke (pMatrix1 `plusPtr` 4 :: Ptr CFloat) (CFloat (e1'))
            poke (pMatrix1 `plusPtr` 8 :: Ptr CFloat) (CFloat (e2'))
            poke (pMatrix1 `plusPtr` 12 :: Ptr CFloat) (CFloat (e3))
        let pMatrix2 = lowerArrayPtr (pMatrix' `plusPtr` 32 :: Ptr (FixedArray 4 CFloat))
        case (e2) of
          (e0', e1', e2', e3) -> do
            poke (pMatrix2 :: Ptr CFloat) (CFloat (e0'))
            poke (pMatrix2 `plusPtr` 4 :: Ptr CFloat) (CFloat (e1'))
            poke (pMatrix2 `plusPtr` 8 :: Ptr CFloat) (CFloat (e2'))
            poke (pMatrix2 `plusPtr` 12 :: Ptr CFloat) (CFloat (e3))
        pure $ ()
    f
  cStructSize = 48
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    let pMatrix' = lowerArrayPtr ((p `plusPtr` 0 :: Ptr (FixedArray 3 (FixedArray 4 CFloat))))
    case (((zero, zero, zero, zero), (zero, zero, zero, zero), (zero, zero, zero, zero))) of
      (e0, e1, e2) -> do
        let pMatrix0 = lowerArrayPtr (pMatrix' :: Ptr (FixedArray 4 CFloat))
        case (e0) of
          (e0', e1', e2', e3) -> do
            poke (pMatrix0 :: Ptr CFloat) (CFloat (e0'))
            poke (pMatrix0 `plusPtr` 4 :: Ptr CFloat) (CFloat (e1'))
            poke (pMatrix0 `plusPtr` 8 :: Ptr CFloat) (CFloat (e2'))
            poke (pMatrix0 `plusPtr` 12 :: Ptr CFloat) (CFloat (e3))
        let pMatrix1 = lowerArrayPtr (pMatrix' `plusPtr` 16 :: Ptr (FixedArray 4 CFloat))
        case (e1) of
          (e0', e1', e2', e3) -> do
            poke (pMatrix1 :: Ptr CFloat) (CFloat (e0'))
            poke (pMatrix1 `plusPtr` 4 :: Ptr CFloat) (CFloat (e1'))
            poke (pMatrix1 `plusPtr` 8 :: Ptr CFloat) (CFloat (e2'))
            poke (pMatrix1 `plusPtr` 12 :: Ptr CFloat) (CFloat (e3))
        let pMatrix2 = lowerArrayPtr (pMatrix' `plusPtr` 32 :: Ptr (FixedArray 4 CFloat))
        case (e2) of
          (e0', e1', e2', e3) -> do
            poke (pMatrix2 :: Ptr CFloat) (CFloat (e0'))
            poke (pMatrix2 `plusPtr` 4 :: Ptr CFloat) (CFloat (e1'))
            poke (pMatrix2 `plusPtr` 8 :: Ptr CFloat) (CFloat (e2'))
            poke (pMatrix2 `plusPtr` 12 :: Ptr CFloat) (CFloat (e3))
        pure $ ()
    f

instance FromCStruct TransformMatrixKHR where
  peekCStruct p = do
    let pmatrix = lowerArrayPtr @(FixedArray 4 CFloat) ((p `plusPtr` 0 :: Ptr (FixedArray 3 (FixedArray 4 CFloat))))
    let pmatrix0 = lowerArrayPtr @CFloat ((pmatrix `advancePtrBytes` 0 :: Ptr (FixedArray 4 CFloat)))
    matrix00 <- peek @CFloat ((pmatrix0 `advancePtrBytes` 0 :: Ptr CFloat))
    matrix01 <- peek @CFloat ((pmatrix0 `advancePtrBytes` 4 :: Ptr CFloat))
    matrix02 <- peek @CFloat ((pmatrix0 `advancePtrBytes` 8 :: Ptr CFloat))
    matrix03 <- peek @CFloat ((pmatrix0 `advancePtrBytes` 12 :: Ptr CFloat))
    let pmatrix1 = lowerArrayPtr @CFloat ((pmatrix `advancePtrBytes` 16 :: Ptr (FixedArray 4 CFloat)))
    matrix10 <- peek @CFloat ((pmatrix1 `advancePtrBytes` 0 :: Ptr CFloat))
    matrix11 <- peek @CFloat ((pmatrix1 `advancePtrBytes` 4 :: Ptr CFloat))
    matrix12 <- peek @CFloat ((pmatrix1 `advancePtrBytes` 8 :: Ptr CFloat))
    matrix13 <- peek @CFloat ((pmatrix1 `advancePtrBytes` 12 :: Ptr CFloat))
    let pmatrix2 = lowerArrayPtr @CFloat ((pmatrix `advancePtrBytes` 32 :: Ptr (FixedArray 4 CFloat)))
    matrix20 <- peek @CFloat ((pmatrix2 `advancePtrBytes` 0 :: Ptr CFloat))
    matrix21 <- peek @CFloat ((pmatrix2 `advancePtrBytes` 4 :: Ptr CFloat))
    matrix22 <- peek @CFloat ((pmatrix2 `advancePtrBytes` 8 :: Ptr CFloat))
    matrix23 <- peek @CFloat ((pmatrix2 `advancePtrBytes` 12 :: Ptr CFloat))
    pure $ TransformMatrixKHR
             ((((((\(CFloat a) -> a) matrix00), ((\(CFloat a) -> a) matrix01), ((\(CFloat a) -> a) matrix02), ((\(CFloat a) -> a) matrix03))), ((((\(CFloat a) -> a) matrix10), ((\(CFloat a) -> a) matrix11), ((\(CFloat a) -> a) matrix12), ((\(CFloat a) -> a) matrix13))), ((((\(CFloat a) -> a) matrix20), ((\(CFloat a) -> a) matrix21), ((\(CFloat a) -> a) matrix22), ((\(CFloat a) -> a) matrix23)))))

instance Storable TransformMatrixKHR where
  sizeOf ~_ = 48
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero TransformMatrixKHR where
  zero = TransformMatrixKHR
           ((zero, zero, zero, zero), (zero, zero, zero, zero), (zero, zero, zero, zero))


-- | VkAccelerationStructureInstanceKHR - Structure specifying a single
-- acceleration structure instance for building into an acceleration
-- structure geometry
--
-- = Description
--
-- The C language spec does not define the ordering of bit-fields, but in
-- practice, this struct produces the correct layout with existing
-- compilers. The intended bit pattern is for the following:
--
-- If a compiler produces code that diverges from that pattern,
-- applications /must/ employ another method to set values according to the
-- correct bit pattern.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'GeometryInstanceFlagsKHR', 'TransformMatrixKHR'
data AccelerationStructureInstanceKHR = AccelerationStructureInstanceKHR
  { -- | @transform@ is a 'TransformMatrixKHR' structure describing a
    -- transformation to be applied to the acceleration structure.
    transform :: TransformMatrixKHR
  , -- | @instanceCustomIndex@ and @mask@ occupy the same memory as if a single
    -- @int32_t@ was specified in their place
    --
    -- -   @instanceCustomIndex@ occupies the 24 least significant bits of that
    --     memory
    --
    -- -   @mask@ occupies the 8 most significant bits of that memory
    instanceCustomIndex :: Word32
  , -- | @mask@ is an 8-bit visibility mask for the geometry. The instance /may/
    -- only be hit if @rayMask & instance.mask != 0@
    mask :: Word32
  , -- | @instanceShaderBindingTableRecordOffset@ and @flags@ occupy the same
    -- memory as if a single @int32_t@ was specified in their place
    --
    -- -   @instanceShaderBindingTableRecordOffset@ occupies the 24 least
    --     significant bits of that memory
    --
    -- -   @flags@ occupies the 8 most significant bits of that memory
    instanceShaderBindingTableRecordOffset :: Word32
  , -- | @flags@ /must/ be a valid combination of 'GeometryInstanceFlagBitsKHR'
    -- values
    flags :: GeometryInstanceFlagsKHR
  , -- | @accelerationStructureReference@ is either:
    --
    -- -   a device address containing the value obtained from
    --     'getAccelerationStructureDeviceAddressKHR' or
    --     'Vulkan.Extensions.VK_NV_ray_tracing.getAccelerationStructureHandleNV'
    --     (used by device operations which reference acceleration structures)
    --     or,
    --
    -- -   a 'Vulkan.Extensions.Handles.AccelerationStructureKHR' object (used
    --     by host operations which reference acceleration structures).
    accelerationStructureReference :: Word64
  }
  deriving (Typeable)
deriving instance Show AccelerationStructureInstanceKHR

instance ToCStruct AccelerationStructureInstanceKHR where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureInstanceKHR{..} f = evalContT $ do
    ContT $ pokeCStruct ((p `plusPtr` 0 :: Ptr TransformMatrixKHR)) (transform) . ($ ())
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) (((coerce @_ @Word32 (mask)) `shiftL` 24) .|. (instanceCustomIndex))
    lift $ poke ((p `plusPtr` 52 :: Ptr Word32)) (((coerce @_ @Word32 (flags)) `shiftL` 24) .|. (instanceShaderBindingTableRecordOffset))
    lift $ poke ((p `plusPtr` 56 :: Ptr Word64)) (accelerationStructureReference)
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    ContT $ pokeCStruct ((p `plusPtr` 0 :: Ptr TransformMatrixKHR)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 56 :: Ptr Word64)) (zero)
    lift $ f

instance FromCStruct AccelerationStructureInstanceKHR where
  peekCStruct p = do
    transform <- peekCStruct @TransformMatrixKHR ((p `plusPtr` 0 :: Ptr TransformMatrixKHR))
    instanceCustomIndex <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    let instanceCustomIndex' = ((instanceCustomIndex .&. coerce @Word32 0xffffff))
    mask <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    let mask' = ((((mask `shiftR` 24)) .&. coerce @Word32 0xff))
    instanceShaderBindingTableRecordOffset <- peek @Word32 ((p `plusPtr` 52 :: Ptr Word32))
    let instanceShaderBindingTableRecordOffset' = ((instanceShaderBindingTableRecordOffset .&. coerce @Word32 0xffffff))
    flags <- peek @GeometryInstanceFlagsKHR ((p `plusPtr` 52 :: Ptr GeometryInstanceFlagsKHR))
    let flags' = ((((flags `shiftR` 24)) .&. coerce @Word32 0xff))
    accelerationStructureReference <- peek @Word64 ((p `plusPtr` 56 :: Ptr Word64))
    pure $ AccelerationStructureInstanceKHR
             transform instanceCustomIndex' mask' instanceShaderBindingTableRecordOffset' flags' accelerationStructureReference

instance Zero AccelerationStructureInstanceKHR where
  zero = AccelerationStructureInstanceKHR
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkAccelerationStructureDeviceAddressInfoKHR - Structure specifying the
-- acceleration structure to query an address for
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.AccelerationStructureKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getAccelerationStructureDeviceAddressKHR'
data AccelerationStructureDeviceAddressInfoKHR = AccelerationStructureDeviceAddressInfoKHR
  { -- | @accelerationStructure@ /must/ be a valid
    -- 'Vulkan.Extensions.Handles.AccelerationStructureKHR' handle
    accelerationStructure :: AccelerationStructureKHR }
  deriving (Typeable)
deriving instance Show AccelerationStructureDeviceAddressInfoKHR

instance ToCStruct AccelerationStructureDeviceAddressInfoKHR where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureDeviceAddressInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DEVICE_ADDRESS_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AccelerationStructureKHR)) (accelerationStructure)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DEVICE_ADDRESS_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AccelerationStructureKHR)) (zero)
    f

instance FromCStruct AccelerationStructureDeviceAddressInfoKHR where
  peekCStruct p = do
    accelerationStructure <- peek @AccelerationStructureKHR ((p `plusPtr` 16 :: Ptr AccelerationStructureKHR))
    pure $ AccelerationStructureDeviceAddressInfoKHR
             accelerationStructure

instance Storable AccelerationStructureDeviceAddressInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AccelerationStructureDeviceAddressInfoKHR where
  zero = AccelerationStructureDeviceAddressInfoKHR
           zero


-- | VkAccelerationStructureVersionKHR - Acceleration structure version
-- information
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getDeviceAccelerationStructureCompatibilityKHR'
data AccelerationStructureVersionKHR = AccelerationStructureVersionKHR
  { -- | @versionData@ /must/ be a valid pointer to an array of @2@*VK_UUID_SIZE
    -- @uint8_t@ values
    versionData :: ByteString }
  deriving (Typeable)
deriving instance Show AccelerationStructureVersionKHR

instance ToCStruct AccelerationStructureVersionKHR where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureVersionKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_VERSION_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ unless (Data.ByteString.length (versionData) == 2 * UUID_SIZE) $
      throwIO $ IOError Nothing InvalidArgument "" "AccelerationStructureVersionKHR::versionData must be 2*VK_UUID_SIZE bytes" Nothing Nothing
    versionData'' <- fmap (castPtr @CChar @Word8) . ContT $ unsafeUseAsCString (versionData)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr Word8))) versionData''
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_VERSION_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct AccelerationStructureVersionKHR where
  peekCStruct p = do
    versionData <- peek @(Ptr Word8) ((p `plusPtr` 16 :: Ptr (Ptr Word8)))
    versionData' <- packCStringLen (castPtr @Word8 @CChar versionData, 2 * UUID_SIZE)
    pure $ AccelerationStructureVersionKHR
             versionData'

instance Zero AccelerationStructureVersionKHR where
  zero = AccelerationStructureVersionKHR
           mempty


-- | VkCopyAccelerationStructureInfoKHR - Parameters for copying an
-- acceleration structure
--
-- == Valid Usage
--
-- -   [[VUID-{refpage}-mode-03410]] @mode@ /must/ be
--     'COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR' or
--     'COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR'
--
-- -   [[VUID-{refpage}-src-03411]] @src@ /must/ have been built with
--     'BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR' if @mode@ is
--     'COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_INFO_KHR'
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_KHR_deferred_host_operations.DeferredOperationInfoKHR'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   @src@ /must/ be a valid
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR' handle
--
-- -   @dst@ /must/ be a valid
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR' handle
--
-- -   @mode@ /must/ be a valid 'CopyAccelerationStructureModeKHR' value
--
-- -   Both of @dst@, and @src@ /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.AccelerationStructureKHR',
-- 'CopyAccelerationStructureModeKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdCopyAccelerationStructureKHR', 'copyAccelerationStructureKHR'
data CopyAccelerationStructureInfoKHR (es :: [Type]) = CopyAccelerationStructureInfoKHR
  { -- No documentation found for Nested "VkCopyAccelerationStructureInfoKHR" "pNext"
    next :: Chain es
  , -- | @src@ is the source acceleration structure for the copy.
    src :: AccelerationStructureKHR
  , -- | @dst@ is the target acceleration structure for the copy.
    dst :: AccelerationStructureKHR
  , -- | @mode@ is a 'CopyAccelerationStructureModeKHR' value that specifies
    -- additional operations to perform during the copy.
    mode :: CopyAccelerationStructureModeKHR
  }
  deriving (Typeable)
deriving instance Show (Chain es) => Show (CopyAccelerationStructureInfoKHR es)

instance Extensible CopyAccelerationStructureInfoKHR where
  extensibleType = STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_INFO_KHR
  setNext x next = x{next = next}
  getNext CopyAccelerationStructureInfoKHR{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends CopyAccelerationStructureInfoKHR e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @DeferredOperationInfoKHR = Just f
    | otherwise = Nothing

instance PokeChain es => ToCStruct (CopyAccelerationStructureInfoKHR es) where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyAccelerationStructureInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_INFO_KHR)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr AccelerationStructureKHR)) (src)
    lift $ poke ((p `plusPtr` 24 :: Ptr AccelerationStructureKHR)) (dst)
    lift $ poke ((p `plusPtr` 32 :: Ptr CopyAccelerationStructureModeKHR)) (mode)
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_INFO_KHR)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr AccelerationStructureKHR)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr AccelerationStructureKHR)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr CopyAccelerationStructureModeKHR)) (zero)
    lift $ f

instance PeekChain es => FromCStruct (CopyAccelerationStructureInfoKHR es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    src <- peek @AccelerationStructureKHR ((p `plusPtr` 16 :: Ptr AccelerationStructureKHR))
    dst <- peek @AccelerationStructureKHR ((p `plusPtr` 24 :: Ptr AccelerationStructureKHR))
    mode <- peek @CopyAccelerationStructureModeKHR ((p `plusPtr` 32 :: Ptr CopyAccelerationStructureModeKHR))
    pure $ CopyAccelerationStructureInfoKHR
             next src dst mode

instance es ~ '[] => Zero (CopyAccelerationStructureInfoKHR es) where
  zero = CopyAccelerationStructureInfoKHR
           ()
           zero
           zero
           zero


-- | VkCopyAccelerationStructureToMemoryInfoKHR - Parameters for serializing
-- an acceleration structure
--
-- == Valid Usage
--
-- -   The memory pointed to by @dst@ /must/ be at least as large as the
--     serialization size of @src@, as reported by
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_SIZE_KHR'
--
-- -   [[VUID-{refpage}-mode-03412]] @mode@ /must/ be
--     'COPY_ACCELERATION_STRUCTURE_MODE_SERIALIZE_KHR'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_TO_MEMORY_INFO_KHR'
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_KHR_deferred_host_operations.DeferredOperationInfoKHR'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   @src@ /must/ be a valid
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR' handle
--
-- -   @dst@ /must/ be a valid 'DeviceOrHostAddressKHR' union
--
-- -   @mode@ /must/ be a valid 'CopyAccelerationStructureModeKHR' value
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.AccelerationStructureKHR',
-- 'CopyAccelerationStructureModeKHR', 'DeviceOrHostAddressKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdCopyAccelerationStructureToMemoryKHR',
-- 'copyAccelerationStructureToMemoryKHR'
data CopyAccelerationStructureToMemoryInfoKHR (es :: [Type]) = CopyAccelerationStructureToMemoryInfoKHR
  { -- No documentation found for Nested "VkCopyAccelerationStructureToMemoryInfoKHR" "pNext"
    next :: Chain es
  , -- | @src@ is the source acceleration structure for the copy
    src :: AccelerationStructureKHR
  , -- | @dst@ is the device or host address to memory which is the target for
    -- the copy
    dst :: DeviceOrHostAddressKHR
  , -- | @mode@ is a 'CopyAccelerationStructureModeKHR' value that specifies
    -- additional operations to perform during the copy.
    mode :: CopyAccelerationStructureModeKHR
  }
  deriving (Typeable)
deriving instance Show (Chain es) => Show (CopyAccelerationStructureToMemoryInfoKHR es)

instance Extensible CopyAccelerationStructureToMemoryInfoKHR where
  extensibleType = STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_TO_MEMORY_INFO_KHR
  setNext x next = x{next = next}
  getNext CopyAccelerationStructureToMemoryInfoKHR{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends CopyAccelerationStructureToMemoryInfoKHR e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @DeferredOperationInfoKHR = Just f
    | otherwise = Nothing

instance PokeChain es => ToCStruct (CopyAccelerationStructureToMemoryInfoKHR es) where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyAccelerationStructureToMemoryInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_TO_MEMORY_INFO_KHR)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr AccelerationStructureKHR)) (src)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr DeviceOrHostAddressKHR)) (dst) . ($ ())
    lift $ poke ((p `plusPtr` 32 :: Ptr CopyAccelerationStructureModeKHR)) (mode)
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_TO_MEMORY_INFO_KHR)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr AccelerationStructureKHR)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr DeviceOrHostAddressKHR)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 32 :: Ptr CopyAccelerationStructureModeKHR)) (zero)
    lift $ f

instance es ~ '[] => Zero (CopyAccelerationStructureToMemoryInfoKHR es) where
  zero = CopyAccelerationStructureToMemoryInfoKHR
           ()
           zero
           zero
           zero


-- | VkCopyMemoryToAccelerationStructureInfoKHR - Parameters for
-- deserializing an acceleration structure
--
-- == Valid Usage
--
-- -   [[VUID-{refpage}-mode-03413]] @mode@ /must/ be
--     'COPY_ACCELERATION_STRUCTURE_MODE_DESERIALIZE_KHR'
--
-- -   [[VUID-{refpage}-pInfo-03414]] The data in @pInfo->src@ /must/ have
--     a format compatible with the destination physical device as returned
--     by 'getDeviceAccelerationStructureCompatibilityKHR'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_MEMORY_TO_ACCELERATION_STRUCTURE_INFO_KHR'
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_KHR_deferred_host_operations.DeferredOperationInfoKHR'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   @src@ /must/ be a valid 'DeviceOrHostAddressConstKHR' union
--
-- -   @dst@ /must/ be a valid
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR' handle
--
-- -   @mode@ /must/ be a valid 'CopyAccelerationStructureModeKHR' value
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.AccelerationStructureKHR',
-- 'CopyAccelerationStructureModeKHR', 'DeviceOrHostAddressConstKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdCopyMemoryToAccelerationStructureKHR',
-- 'copyMemoryToAccelerationStructureKHR'
data CopyMemoryToAccelerationStructureInfoKHR (es :: [Type]) = CopyMemoryToAccelerationStructureInfoKHR
  { -- No documentation found for Nested "VkCopyMemoryToAccelerationStructureInfoKHR" "pNext"
    next :: Chain es
  , -- | @src@ is the device or host address to memory containing the source data
    -- for the copy.
    src :: DeviceOrHostAddressConstKHR
  , -- | @dst@ is the target acceleration structure for the copy.
    dst :: AccelerationStructureKHR
  , -- | @mode@ is a 'CopyAccelerationStructureModeKHR' value that specifies
    -- additional operations to perform during the copy.
    mode :: CopyAccelerationStructureModeKHR
  }
  deriving (Typeable)
deriving instance Show (Chain es) => Show (CopyMemoryToAccelerationStructureInfoKHR es)

instance Extensible CopyMemoryToAccelerationStructureInfoKHR where
  extensibleType = STRUCTURE_TYPE_COPY_MEMORY_TO_ACCELERATION_STRUCTURE_INFO_KHR
  setNext x next = x{next = next}
  getNext CopyMemoryToAccelerationStructureInfoKHR{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends CopyMemoryToAccelerationStructureInfoKHR e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @DeferredOperationInfoKHR = Just f
    | otherwise = Nothing

instance PokeChain es => ToCStruct (CopyMemoryToAccelerationStructureInfoKHR es) where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyMemoryToAccelerationStructureInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_MEMORY_TO_ACCELERATION_STRUCTURE_INFO_KHR)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr DeviceOrHostAddressConstKHR)) (src) . ($ ())
    lift $ poke ((p `plusPtr` 24 :: Ptr AccelerationStructureKHR)) (dst)
    lift $ poke ((p `plusPtr` 32 :: Ptr CopyAccelerationStructureModeKHR)) (mode)
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_MEMORY_TO_ACCELERATION_STRUCTURE_INFO_KHR)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr DeviceOrHostAddressConstKHR)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 24 :: Ptr AccelerationStructureKHR)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr CopyAccelerationStructureModeKHR)) (zero)
    lift $ f

instance es ~ '[] => Zero (CopyMemoryToAccelerationStructureInfoKHR es) where
  zero = CopyMemoryToAccelerationStructureInfoKHR
           ()
           zero
           zero
           zero


-- | VkRayTracingPipelineInterfaceCreateInfoKHR - Structure specifying
-- additional interface information when using libraries
--
-- = Description
--
-- @maxPayloadSize@ is calculated as the maximum number of bytes used by
-- any block declared in the @RayPayloadKHR@ or @IncomingRayPayloadKHR@
-- storage classes. @maxAttributeSize@ is calculated as the maximum number
-- of bytes used by any block declared in the @HitAttributeKHR@ storage
-- class. @maxCallableSize@ is calculated as the maximum number of bytes
-- used by any block declred in the @CallableDataKHR@ or
-- @IncomingCallableDataKHR@. As variables in these storage classes do not
-- have explicit offsets, the size should be calculated as if each variable
-- has a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-alignment-requirements scalar alignment>
-- equal to the largest scalar alignment of any of the block’s members.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'RayTracingPipelineCreateInfoKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data RayTracingPipelineInterfaceCreateInfoKHR = RayTracingPipelineInterfaceCreateInfoKHR
  { -- | @maxPayloadSize@ is the maximum payload size in bytes used by any shader
    -- in the pipeline.
    maxPayloadSize :: Word32
  , -- | @maxAttributeSize@ is the maximum attribute structure size in bytes used
    -- by any shader in the pipeline.
    maxAttributeSize :: Word32
  , -- | @maxCallableSize@ is the maximum callable data size in bytes used by any
    -- shader in the pipeline.
    maxCallableSize :: Word32
  }
  deriving (Typeable)
deriving instance Show RayTracingPipelineInterfaceCreateInfoKHR

instance ToCStruct RayTracingPipelineInterfaceCreateInfoKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RayTracingPipelineInterfaceCreateInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RAY_TRACING_PIPELINE_INTERFACE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxPayloadSize)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxAttributeSize)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (maxCallableSize)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RAY_TRACING_PIPELINE_INTERFACE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    f

instance FromCStruct RayTracingPipelineInterfaceCreateInfoKHR where
  peekCStruct p = do
    maxPayloadSize <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxAttributeSize <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    maxCallableSize <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ RayTracingPipelineInterfaceCreateInfoKHR
             maxPayloadSize maxAttributeSize maxCallableSize

instance Storable RayTracingPipelineInterfaceCreateInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero RayTracingPipelineInterfaceCreateInfoKHR where
  zero = RayTracingPipelineInterfaceCreateInfoKHR
           zero
           zero
           zero


data DeviceOrHostAddressKHR
  = DeviceAddress DeviceAddress
  | HostAddress (Ptr ())
  deriving (Show)

instance ToCStruct DeviceOrHostAddressKHR where
  withCStruct x f = allocaBytesAligned 8 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct :: Ptr DeviceOrHostAddressKHR -> DeviceOrHostAddressKHR -> IO a -> IO a
  pokeCStruct p = (. const) . runContT .  \case
    DeviceAddress v -> lift $ poke (castPtr @_ @DeviceAddress p) (v)
    HostAddress v -> lift $ poke (castPtr @_ @(Ptr ()) p) (v)
  pokeZeroCStruct :: Ptr DeviceOrHostAddressKHR -> IO b -> IO b
  pokeZeroCStruct _ f = f
  cStructSize = 8
  cStructAlignment = 8

instance Zero DeviceOrHostAddressKHR where
  zero = DeviceAddress zero


data DeviceOrHostAddressConstKHR
  = DeviceAddressConst DeviceAddress
  | HostAddressConst (Ptr ())
  deriving (Show)

instance ToCStruct DeviceOrHostAddressConstKHR where
  withCStruct x f = allocaBytesAligned 8 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct :: Ptr DeviceOrHostAddressConstKHR -> DeviceOrHostAddressConstKHR -> IO a -> IO a
  pokeCStruct p = (. const) . runContT .  \case
    DeviceAddressConst v -> lift $ poke (castPtr @_ @DeviceAddress p) (v)
    HostAddressConst v -> lift $ poke (castPtr @_ @(Ptr ()) p) (v)
  pokeZeroCStruct :: Ptr DeviceOrHostAddressConstKHR -> IO b -> IO b
  pokeZeroCStruct _ f = f
  cStructSize = 8
  cStructAlignment = 8

instance Zero DeviceOrHostAddressConstKHR where
  zero = DeviceAddressConst zero


data AccelerationStructureGeometryDataKHR
  = Triangles AccelerationStructureGeometryTrianglesDataKHR
  | Aabbs AccelerationStructureGeometryAabbsDataKHR
  | Instances AccelerationStructureGeometryInstancesDataKHR
  deriving (Show)

instance ToCStruct AccelerationStructureGeometryDataKHR where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct :: Ptr AccelerationStructureGeometryDataKHR -> AccelerationStructureGeometryDataKHR -> IO a -> IO a
  pokeCStruct p = (. const) . runContT .  \case
    Triangles v -> ContT $ pokeCStruct (castPtr @_ @AccelerationStructureGeometryTrianglesDataKHR p) (v) . ($ ())
    Aabbs v -> ContT $ pokeCStruct (castPtr @_ @AccelerationStructureGeometryAabbsDataKHR p) (v) . ($ ())
    Instances v -> ContT $ pokeCStruct (castPtr @_ @AccelerationStructureGeometryInstancesDataKHR p) (v) . ($ ())
  pokeZeroCStruct :: Ptr AccelerationStructureGeometryDataKHR -> IO b -> IO b
  pokeZeroCStruct _ f = f
  cStructSize = 64
  cStructAlignment = 8

instance Zero AccelerationStructureGeometryDataKHR where
  zero = Triangles zero


-- | VkGeometryInstanceFlagBitsKHR - Instance flag bits
--
-- = Description
--
-- 'GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_KHR' and
-- 'GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_KHR' /must/ not be used in the same
-- flag.
--
-- = See Also
--
-- 'GeometryInstanceFlagsKHR'
newtype GeometryInstanceFlagBitsKHR = GeometryInstanceFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'GEOMETRY_INSTANCE_TRIANGLE_FACING_CULL_DISABLE_BIT_KHR' disables face
-- culling for this instance.
pattern GEOMETRY_INSTANCE_TRIANGLE_FACING_CULL_DISABLE_BIT_KHR = GeometryInstanceFlagBitsKHR 0x00000001
-- | 'GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_KHR' indicates
-- that the front face of the triangle for culling purposes is the face
-- that is counter clockwise in object space relative to the ray origin.
-- Because the facing is determined in object space, an instance transform
-- matrix does not change the winding, but a geometry transform does.
pattern GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_KHR = GeometryInstanceFlagBitsKHR 0x00000002
-- | 'GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_KHR' causes this instance to act as
-- though 'GEOMETRY_OPAQUE_BIT_KHR' were specified on all geometries
-- referenced by this instance. This behavior /can/ be overridden by the
-- SPIR-V @NoOpaqueKHR@ ray flag.
pattern GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_KHR = GeometryInstanceFlagBitsKHR 0x00000004
-- | 'GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_KHR' causes this instance to act
-- as though 'GEOMETRY_OPAQUE_BIT_KHR' were not specified on all geometries
-- referenced by this instance. This behavior /can/ be overridden by the
-- SPIR-V @OpaqueKHR@ ray flag.
pattern GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_KHR = GeometryInstanceFlagBitsKHR 0x00000008

type GeometryInstanceFlagsKHR = GeometryInstanceFlagBitsKHR

instance Show GeometryInstanceFlagBitsKHR where
  showsPrec p = \case
    GEOMETRY_INSTANCE_TRIANGLE_FACING_CULL_DISABLE_BIT_KHR -> showString "GEOMETRY_INSTANCE_TRIANGLE_FACING_CULL_DISABLE_BIT_KHR"
    GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_KHR -> showString "GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_KHR"
    GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_KHR -> showString "GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_KHR"
    GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_KHR -> showString "GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_KHR"
    GeometryInstanceFlagBitsKHR x -> showParen (p >= 11) (showString "GeometryInstanceFlagBitsKHR 0x" . showHex x)

instance Read GeometryInstanceFlagBitsKHR where
  readPrec = parens (choose [("GEOMETRY_INSTANCE_TRIANGLE_FACING_CULL_DISABLE_BIT_KHR", pure GEOMETRY_INSTANCE_TRIANGLE_FACING_CULL_DISABLE_BIT_KHR)
                            , ("GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_KHR", pure GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_KHR)
                            , ("GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_KHR", pure GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_KHR)
                            , ("GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_KHR", pure GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_KHR)]
                     +++
                     prec 10 (do
                       expectP (Ident "GeometryInstanceFlagBitsKHR")
                       v <- step readPrec
                       pure (GeometryInstanceFlagBitsKHR v)))


-- | VkGeometryFlagBitsKHR - Bitmask specifying additional parameters for a
-- geometry
--
-- = See Also
--
-- 'GeometryFlagsKHR'
newtype GeometryFlagBitsKHR = GeometryFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'GEOMETRY_OPAQUE_BIT_KHR' indicates that this geometry does not invoke
-- the any-hit shaders even if present in a hit group.
pattern GEOMETRY_OPAQUE_BIT_KHR = GeometryFlagBitsKHR 0x00000001
-- | 'GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_KHR' indicates that the
-- implementation /must/ only call the any-hit shader a single time for
-- each primitive in this geometry. If this bit is absent an implementation
-- /may/ invoke the any-hit shader more than once for this geometry.
pattern GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_KHR = GeometryFlagBitsKHR 0x00000002

type GeometryFlagsKHR = GeometryFlagBitsKHR

instance Show GeometryFlagBitsKHR where
  showsPrec p = \case
    GEOMETRY_OPAQUE_BIT_KHR -> showString "GEOMETRY_OPAQUE_BIT_KHR"
    GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_KHR -> showString "GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_KHR"
    GeometryFlagBitsKHR x -> showParen (p >= 11) (showString "GeometryFlagBitsKHR 0x" . showHex x)

instance Read GeometryFlagBitsKHR where
  readPrec = parens (choose [("GEOMETRY_OPAQUE_BIT_KHR", pure GEOMETRY_OPAQUE_BIT_KHR)
                            , ("GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_KHR", pure GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_KHR)]
                     +++
                     prec 10 (do
                       expectP (Ident "GeometryFlagBitsKHR")
                       v <- step readPrec
                       pure (GeometryFlagBitsKHR v)))


-- | VkBuildAccelerationStructureFlagBitsKHR - Bitmask specifying additional
-- parameters for acceleration structure builds
--
-- = Description
--
-- Note
--
-- 'BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR' and
-- 'BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR' /may/ take more
-- time and memory than a normal build, and so /should/ only be used when
-- those features are needed.
--
-- = See Also
--
-- 'BuildAccelerationStructureFlagsKHR'
newtype BuildAccelerationStructureFlagBitsKHR = BuildAccelerationStructureFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR' indicates that the
-- specified acceleration structure /can/ be updated with @update@ of
-- 'Vulkan.Core10.BaseType.TRUE' in 'cmdBuildAccelerationStructureKHR' or
-- 'Vulkan.Extensions.VK_NV_ray_tracing.cmdBuildAccelerationStructureNV' .
pattern BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR = BuildAccelerationStructureFlagBitsKHR 0x00000001
-- | 'BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR' indicates that
-- the specified acceleration structure /can/ act as the source for a copy
-- acceleration structure command with @mode@ of
-- 'COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR' to produce a compacted
-- acceleration structure.
pattern BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR = BuildAccelerationStructureFlagBitsKHR 0x00000002
-- | 'BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR' indicates that
-- the given acceleration structure build /should/ prioritize trace
-- performance over build time.
pattern BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR = BuildAccelerationStructureFlagBitsKHR 0x00000004
-- | 'BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_KHR' indicates that
-- the given acceleration structure build /should/ prioritize build time
-- over trace performance.
pattern BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_KHR = BuildAccelerationStructureFlagBitsKHR 0x00000008
-- | 'BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_KHR' indicates that this
-- acceleration structure /should/ minimize the size of the scratch memory
-- and the final result build, potentially at the expense of build time or
-- trace performance.
pattern BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_KHR = BuildAccelerationStructureFlagBitsKHR 0x00000010

type BuildAccelerationStructureFlagsKHR = BuildAccelerationStructureFlagBitsKHR

instance Show BuildAccelerationStructureFlagBitsKHR where
  showsPrec p = \case
    BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR -> showString "BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR"
    BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR -> showString "BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR"
    BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR -> showString "BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR"
    BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_KHR -> showString "BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_KHR"
    BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_KHR -> showString "BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_KHR"
    BuildAccelerationStructureFlagBitsKHR x -> showParen (p >= 11) (showString "BuildAccelerationStructureFlagBitsKHR 0x" . showHex x)

instance Read BuildAccelerationStructureFlagBitsKHR where
  readPrec = parens (choose [("BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR", pure BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR)
                            , ("BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR", pure BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR)
                            , ("BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR", pure BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR)
                            , ("BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_KHR", pure BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_KHR)
                            , ("BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_KHR", pure BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_KHR)]
                     +++
                     prec 10 (do
                       expectP (Ident "BuildAccelerationStructureFlagBitsKHR")
                       v <- step readPrec
                       pure (BuildAccelerationStructureFlagBitsKHR v)))


-- | VkCopyAccelerationStructureModeKHR - Acceleration structure copy mode
--
-- = See Also
--
-- 'CopyAccelerationStructureInfoKHR',
-- 'CopyAccelerationStructureToMemoryInfoKHR',
-- 'CopyMemoryToAccelerationStructureInfoKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.cmdCopyAccelerationStructureNV'
newtype CopyAccelerationStructureModeKHR = CopyAccelerationStructureModeKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR' creates a direct copy of
-- the acceleration structure specified in @src@ into the one specified by
-- @dst@. The @dst@ acceleration structure /must/ have been created with
-- the same parameters as @src@.
pattern COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR = CopyAccelerationStructureModeKHR 0
-- | 'COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR' creates a more compact
-- version of an acceleration structure @src@ into @dst@. The acceleration
-- structure @dst@ /must/ have been created with a @compactedSize@
-- corresponding to the one returned by
-- 'cmdWriteAccelerationStructuresPropertiesKHR' after the build of the
-- acceleration structure specified by @src@.
pattern COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR = CopyAccelerationStructureModeKHR 1
-- | 'COPY_ACCELERATION_STRUCTURE_MODE_SERIALIZE_KHR' serializes the
-- acceleration structure to a semi-opaque format which can be reloaded on
-- a compatible implementation.
pattern COPY_ACCELERATION_STRUCTURE_MODE_SERIALIZE_KHR = CopyAccelerationStructureModeKHR 2
-- | 'COPY_ACCELERATION_STRUCTURE_MODE_DESERIALIZE_KHR' deserializes the
-- semi-opaque serialization format in the buffer to the acceleration
-- structure.
pattern COPY_ACCELERATION_STRUCTURE_MODE_DESERIALIZE_KHR = CopyAccelerationStructureModeKHR 3
{-# complete COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR,
             COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR,
             COPY_ACCELERATION_STRUCTURE_MODE_SERIALIZE_KHR,
             COPY_ACCELERATION_STRUCTURE_MODE_DESERIALIZE_KHR :: CopyAccelerationStructureModeKHR #-}

instance Show CopyAccelerationStructureModeKHR where
  showsPrec p = \case
    COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR -> showString "COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR"
    COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR -> showString "COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR"
    COPY_ACCELERATION_STRUCTURE_MODE_SERIALIZE_KHR -> showString "COPY_ACCELERATION_STRUCTURE_MODE_SERIALIZE_KHR"
    COPY_ACCELERATION_STRUCTURE_MODE_DESERIALIZE_KHR -> showString "COPY_ACCELERATION_STRUCTURE_MODE_DESERIALIZE_KHR"
    CopyAccelerationStructureModeKHR x -> showParen (p >= 11) (showString "CopyAccelerationStructureModeKHR " . showsPrec 11 x)

instance Read CopyAccelerationStructureModeKHR where
  readPrec = parens (choose [("COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR", pure COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR)
                            , ("COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR", pure COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR)
                            , ("COPY_ACCELERATION_STRUCTURE_MODE_SERIALIZE_KHR", pure COPY_ACCELERATION_STRUCTURE_MODE_SERIALIZE_KHR)
                            , ("COPY_ACCELERATION_STRUCTURE_MODE_DESERIALIZE_KHR", pure COPY_ACCELERATION_STRUCTURE_MODE_DESERIALIZE_KHR)]
                     +++
                     prec 10 (do
                       expectP (Ident "CopyAccelerationStructureModeKHR")
                       v <- step readPrec
                       pure (CopyAccelerationStructureModeKHR v)))


-- | VkAccelerationStructureTypeKHR - Type of acceleration structure
--
-- = See Also
--
-- 'AccelerationStructureBuildGeometryInfoKHR',
-- 'AccelerationStructureCreateInfoKHR'
newtype AccelerationStructureTypeKHR = AccelerationStructureTypeKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR' is a top-level acceleration
-- structure containing instance data referring to bottom-level
-- acceleration structures.
pattern ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR = AccelerationStructureTypeKHR 0
-- | 'ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR' is a bottom-level
-- acceleration structure containing the AABBs or geometry to be
-- intersected.
pattern ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR = AccelerationStructureTypeKHR 1
{-# complete ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR,
             ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR :: AccelerationStructureTypeKHR #-}

instance Show AccelerationStructureTypeKHR where
  showsPrec p = \case
    ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR -> showString "ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR"
    ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR -> showString "ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR"
    AccelerationStructureTypeKHR x -> showParen (p >= 11) (showString "AccelerationStructureTypeKHR " . showsPrec 11 x)

instance Read AccelerationStructureTypeKHR where
  readPrec = parens (choose [("ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR", pure ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR)
                            , ("ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR", pure ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR)]
                     +++
                     prec 10 (do
                       expectP (Ident "AccelerationStructureTypeKHR")
                       v <- step readPrec
                       pure (AccelerationStructureTypeKHR v)))


-- | VkGeometryTypeKHR - Enum specifying which type of geometry is provided
--
-- = See Also
--
-- 'AccelerationStructureCreateGeometryTypeInfoKHR',
-- 'AccelerationStructureGeometryKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.GeometryNV'
newtype GeometryTypeKHR = GeometryTypeKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'GEOMETRY_TYPE_TRIANGLES_KHR' specifies a geometry type consisting of
-- triangles.
pattern GEOMETRY_TYPE_TRIANGLES_KHR = GeometryTypeKHR 0
-- | 'GEOMETRY_TYPE_AABBS_KHR' specifies a geometry type consisting of
-- axis-aligned bounding boxes.
pattern GEOMETRY_TYPE_AABBS_KHR = GeometryTypeKHR 1
-- | 'GEOMETRY_TYPE_INSTANCES_KHR' specifies a geometry type consisting of
-- acceleration structure instances.
pattern GEOMETRY_TYPE_INSTANCES_KHR = GeometryTypeKHR 1000150000
{-# complete GEOMETRY_TYPE_TRIANGLES_KHR,
             GEOMETRY_TYPE_AABBS_KHR,
             GEOMETRY_TYPE_INSTANCES_KHR :: GeometryTypeKHR #-}

instance Show GeometryTypeKHR where
  showsPrec p = \case
    GEOMETRY_TYPE_TRIANGLES_KHR -> showString "GEOMETRY_TYPE_TRIANGLES_KHR"
    GEOMETRY_TYPE_AABBS_KHR -> showString "GEOMETRY_TYPE_AABBS_KHR"
    GEOMETRY_TYPE_INSTANCES_KHR -> showString "GEOMETRY_TYPE_INSTANCES_KHR"
    GeometryTypeKHR x -> showParen (p >= 11) (showString "GeometryTypeKHR " . showsPrec 11 x)

instance Read GeometryTypeKHR where
  readPrec = parens (choose [("GEOMETRY_TYPE_TRIANGLES_KHR", pure GEOMETRY_TYPE_TRIANGLES_KHR)
                            , ("GEOMETRY_TYPE_AABBS_KHR", pure GEOMETRY_TYPE_AABBS_KHR)
                            , ("GEOMETRY_TYPE_INSTANCES_KHR", pure GEOMETRY_TYPE_INSTANCES_KHR)]
                     +++
                     prec 10 (do
                       expectP (Ident "GeometryTypeKHR")
                       v <- step readPrec
                       pure (GeometryTypeKHR v)))


-- | VkAccelerationStructureMemoryRequirementsTypeKHR - Acceleration
-- structure memory requirement type
--
-- = See Also
--
-- 'AccelerationStructureMemoryRequirementsInfoKHR'
newtype AccelerationStructureMemoryRequirementsTypeKHR = AccelerationStructureMemoryRequirementsTypeKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_KHR' requests
-- the memory requirement for the
-- 'Vulkan.Extensions.Handles.AccelerationStructureKHR' backing store.
pattern ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_KHR = AccelerationStructureMemoryRequirementsTypeKHR 0
-- | 'ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_KHR'
-- requests the memory requirement for scratch space during the initial
-- build.
pattern ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_KHR = AccelerationStructureMemoryRequirementsTypeKHR 1
-- | 'ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_KHR'
-- requests the memory requirement for scratch space during an update.
pattern ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_KHR = AccelerationStructureMemoryRequirementsTypeKHR 2
{-# complete ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_KHR,
             ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_KHR,
             ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_KHR :: AccelerationStructureMemoryRequirementsTypeKHR #-}

instance Show AccelerationStructureMemoryRequirementsTypeKHR where
  showsPrec p = \case
    ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_KHR -> showString "ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_KHR"
    ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_KHR -> showString "ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_KHR"
    ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_KHR -> showString "ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_KHR"
    AccelerationStructureMemoryRequirementsTypeKHR x -> showParen (p >= 11) (showString "AccelerationStructureMemoryRequirementsTypeKHR " . showsPrec 11 x)

instance Read AccelerationStructureMemoryRequirementsTypeKHR where
  readPrec = parens (choose [("ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_KHR", pure ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_KHR)
                            , ("ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_KHR", pure ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_KHR)
                            , ("ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_KHR", pure ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_KHR)]
                     +++
                     prec 10 (do
                       expectP (Ident "AccelerationStructureMemoryRequirementsTypeKHR")
                       v <- step readPrec
                       pure (AccelerationStructureMemoryRequirementsTypeKHR v)))


-- | VkAccelerationStructureBuildTypeKHR - Acceleration structure build type
--
-- = See Also
--
-- 'AccelerationStructureMemoryRequirementsInfoKHR'
newtype AccelerationStructureBuildTypeKHR = AccelerationStructureBuildTypeKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_KHR' requests the memory
-- requirement for operations performed by the host.
pattern ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_KHR = AccelerationStructureBuildTypeKHR 0
-- | 'ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR' requests the memory
-- requirement for operations performed by the device.
pattern ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR = AccelerationStructureBuildTypeKHR 1
-- | 'ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_OR_DEVICE_KHR' requests the
-- memory requirement for operations performed by either the host, or the
-- device.
pattern ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_OR_DEVICE_KHR = AccelerationStructureBuildTypeKHR 2
{-# complete ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_KHR,
             ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR,
             ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_OR_DEVICE_KHR :: AccelerationStructureBuildTypeKHR #-}

instance Show AccelerationStructureBuildTypeKHR where
  showsPrec p = \case
    ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_KHR -> showString "ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_KHR"
    ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR -> showString "ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR"
    ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_OR_DEVICE_KHR -> showString "ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_OR_DEVICE_KHR"
    AccelerationStructureBuildTypeKHR x -> showParen (p >= 11) (showString "AccelerationStructureBuildTypeKHR " . showsPrec 11 x)

instance Read AccelerationStructureBuildTypeKHR where
  readPrec = parens (choose [("ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_KHR", pure ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_KHR)
                            , ("ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR", pure ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR)
                            , ("ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_OR_DEVICE_KHR", pure ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_OR_DEVICE_KHR)]
                     +++
                     prec 10 (do
                       expectP (Ident "AccelerationStructureBuildTypeKHR")
                       v <- step readPrec
                       pure (AccelerationStructureBuildTypeKHR v)))


-- | VkRayTracingShaderGroupTypeKHR - Shader group types
--
-- = Description
--
-- Note
--
-- For current group types, the hit group type could be inferred from the
-- presence or absence of the intersection shader, but we provide the type
-- explicitly for future hit groups that do not have that property.
--
-- = See Also
--
-- 'RayTracingShaderGroupCreateInfoKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.RayTracingShaderGroupCreateInfoNV'
newtype RayTracingShaderGroupTypeKHR = RayTracingShaderGroupTypeKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR' indicates a shader group
-- with a single
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_RAYGEN_BIT_KHR',
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MISS_BIT_KHR', or
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_CALLABLE_BIT_KHR'
-- shader in it.
pattern RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR = RayTracingShaderGroupTypeKHR 0
-- | 'RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR' specifies a
-- shader group that only hits triangles and /must/ not contain an
-- intersection shader, only closest hit and any-hit shaders.
pattern RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR = RayTracingShaderGroupTypeKHR 1
-- | 'RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR' specifies a
-- shader group that only intersects with custom geometry and /must/
-- contain an intersection shader and /may/ contain closest hit and any-hit
-- shaders.
pattern RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR = RayTracingShaderGroupTypeKHR 2
{-# complete RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR,
             RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR,
             RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR :: RayTracingShaderGroupTypeKHR #-}

instance Show RayTracingShaderGroupTypeKHR where
  showsPrec p = \case
    RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR -> showString "RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR"
    RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR -> showString "RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR"
    RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR -> showString "RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR"
    RayTracingShaderGroupTypeKHR x -> showParen (p >= 11) (showString "RayTracingShaderGroupTypeKHR " . showsPrec 11 x)

instance Read RayTracingShaderGroupTypeKHR where
  readPrec = parens (choose [("RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR", pure RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR)
                            , ("RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR", pure RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR)
                            , ("RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR", pure RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR)]
                     +++
                     prec 10 (do
                       expectP (Ident "RayTracingShaderGroupTypeKHR")
                       v <- step readPrec
                       pure (RayTracingShaderGroupTypeKHR v)))


type KHR_RAY_TRACING_SPEC_VERSION = 8

-- No documentation found for TopLevel "VK_KHR_RAY_TRACING_SPEC_VERSION"
pattern KHR_RAY_TRACING_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_RAY_TRACING_SPEC_VERSION = 8


type KHR_RAY_TRACING_EXTENSION_NAME = "VK_KHR_ray_tracing"

-- No documentation found for TopLevel "VK_KHR_RAY_TRACING_EXTENSION_NAME"
pattern KHR_RAY_TRACING_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_RAY_TRACING_EXTENSION_NAME = "VK_KHR_ray_tracing"

