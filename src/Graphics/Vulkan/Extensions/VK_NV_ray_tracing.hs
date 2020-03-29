{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_NV_ray_tracing  ( compileDeferredNV
                                                     , createAccelerationStructureNV
                                                     , withAccelerationStructureNV
                                                     , destroyAccelerationStructureNV
                                                     , getAccelerationStructureMemoryRequirementsNV
                                                     , bindAccelerationStructureMemoryNV
                                                     , cmdCopyAccelerationStructureNV
                                                     , cmdWriteAccelerationStructuresPropertiesNV
                                                     , cmdBuildAccelerationStructureNV
                                                     , cmdTraceRaysNV
                                                     , getRayTracingShaderGroupHandlesNV
                                                     , getAccelerationStructureHandleNV
                                                     , createRayTracingPipelinesNV
                                                     , RayTracingShaderGroupCreateInfoNV(..)
                                                     , RayTracingPipelineCreateInfoNV(..)
                                                     , GeometryTrianglesNV(..)
                                                     , GeometryAABBNV(..)
                                                     , GeometryDataNV(..)
                                                     , GeometryNV(..)
                                                     , AccelerationStructureInfoNV(..)
                                                     , AccelerationStructureCreateInfoNV(..)
                                                     , BindAccelerationStructureMemoryInfoNV(..)
                                                     , WriteDescriptorSetAccelerationStructureNV(..)
                                                     , AccelerationStructureMemoryRequirementsInfoNV(..)
                                                     , PhysicalDeviceRayTracingPropertiesNV(..)
                                                     , GeometryInstanceFlagBitsNV( GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV
                                                                                 , GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV
                                                                                 , GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV
                                                                                 , GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV
                                                                                 , ..
                                                                                 )
                                                     , GeometryInstanceFlagsNV
                                                     , GeometryFlagBitsNV( GEOMETRY_OPAQUE_BIT_NV
                                                                         , GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV
                                                                         , ..
                                                                         )
                                                     , GeometryFlagsNV
                                                     , BuildAccelerationStructureFlagBitsNV( BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV
                                                                                           , BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV
                                                                                           , BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV
                                                                                           , BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV
                                                                                           , BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV
                                                                                           , ..
                                                                                           )
                                                     , BuildAccelerationStructureFlagsNV
                                                     , CopyAccelerationStructureModeNV( COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV
                                                                                      , COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV
                                                                                      , ..
                                                                                      )
                                                     , AccelerationStructureTypeNV( ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV
                                                                                  , ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV
                                                                                  , ..
                                                                                  )
                                                     , GeometryTypeNV( GEOMETRY_TYPE_TRIANGLES_NV
                                                                     , GEOMETRY_TYPE_AABBS_NV
                                                                     , ..
                                                                     )
                                                     , AccelerationStructureMemoryRequirementsTypeNV( ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV
                                                                                                    , ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV
                                                                                                    , ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV
                                                                                                    , ..
                                                                                                    )
                                                     , RayTracingShaderGroupTypeNV( RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV
                                                                                  , RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV
                                                                                  , RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV
                                                                                  , ..
                                                                                  )
                                                     , NV_RAY_TRACING_SPEC_VERSION
                                                     , pattern NV_RAY_TRACING_SPEC_VERSION
                                                     , NV_RAY_TRACING_EXTENSION_NAME
                                                     , pattern NV_RAY_TRACING_EXTENSION_NAME
                                                     , AccelerationStructureNV(..)
                                                     , DebugReportObjectTypeEXT(..)
                                                     , SHADER_UNUSED_NV
                                                     , pattern SHADER_UNUSED_NV
                                                     ) where

import Control.Exception.Base (bracket)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
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
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Data.Bits (Bits)
import Data.String (IsString)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.C.Types (CSize)
import Foreign.C.Types (CSize(..))
import Foreign.C.Types (CSize(CSize))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import Data.Word (Word32)
import Data.Word (Word64)
import Text.Read.Lex (Lexeme(Ident))
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Graphics.Vulkan.CStruct.Utils (advancePtrBytes)
import Graphics.Vulkan.Core10.BaseType (boolToBool32)
import Graphics.Vulkan.CStruct.Extends (forgetExtensions)
import Graphics.Vulkan.CStruct.Extends (peekSomeCStruct)
import Graphics.Vulkan.CStruct.Extends (pokeSomeCStruct)
import Graphics.Vulkan.NamedType ((:::))
import Graphics.Vulkan.Extensions.Handles (AccelerationStructureNV)
import Graphics.Vulkan.Extensions.Handles (AccelerationStructureNV(..))
import Graphics.Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Graphics.Vulkan.Core10.BaseType (Bool32)
import Graphics.Vulkan.Core10.BaseType (Bool32(..))
import Graphics.Vulkan.Core10.Handles (Buffer)
import Graphics.Vulkan.Core10.Handles (Buffer(..))
import Graphics.Vulkan.CStruct.Extends (Chain)
import Graphics.Vulkan.Core10.Handles (CommandBuffer)
import Graphics.Vulkan.Core10.Handles (CommandBuffer(..))
import Graphics.Vulkan.Core10.Handles (CommandBuffer_T)
import Graphics.Vulkan.Core10.Handles (Device)
import Graphics.Vulkan.Core10.Handles (Device(..))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkBindAccelerationStructureMemoryNV))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdBuildAccelerationStructureNV))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdCopyAccelerationStructureNV))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdTraceRaysNV))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdWriteAccelerationStructuresPropertiesNV))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCompileDeferredNV))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCreateAccelerationStructureNV))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCreateRayTracingPipelinesNV))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkDestroyAccelerationStructureNV))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkGetAccelerationStructureHandleNV))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkGetAccelerationStructureMemoryRequirementsNV))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkGetRayTracingShaderGroupHandlesNV))
import Graphics.Vulkan.Core10.Handles (DeviceMemory)
import Graphics.Vulkan.Core10.BaseType (DeviceSize)
import Graphics.Vulkan.Core10.Handles (Device_T)
import Graphics.Vulkan.CStruct.Extends (Extends)
import Graphics.Vulkan.CStruct.Extends (Extensible(..))
import Graphics.Vulkan.Core10.BaseType (Flags)
import Graphics.Vulkan.Core10.Enums.Format (Format)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Enums.IndexType (IndexType)
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (MemoryRequirements2KHR)
import Graphics.Vulkan.CStruct.Extends (PeekChain)
import Graphics.Vulkan.CStruct.Extends (PeekChain(..))
import Graphics.Vulkan.Core10.Handles (Pipeline)
import Graphics.Vulkan.Core10.Handles (Pipeline(..))
import Graphics.Vulkan.Core10.Handles (PipelineCache)
import Graphics.Vulkan.Core10.Handles (PipelineCache(..))
import Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlags)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_EXT_pipeline_creation_feedback (PipelineCreationFeedbackCreateInfoEXT)
import Graphics.Vulkan.Core10.Handles (PipelineLayout)
import Graphics.Vulkan.Core10.Pipeline (PipelineShaderStageCreateInfo)
import Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct.Extends (PokeChain(..))
import Graphics.Vulkan.Core10.Handles (QueryPool)
import Graphics.Vulkan.Core10.Handles (QueryPool(..))
import Graphics.Vulkan.Core10.Enums.QueryType (QueryType)
import Graphics.Vulkan.Core10.Enums.QueryType (QueryType(..))
import Graphics.Vulkan.Core10.Enums.Result (Result)
import Graphics.Vulkan.Core10.Enums.Result (Result(..))
import Graphics.Vulkan.CStruct.Extends (SomeStruct)
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Exception (VulkanException(..))
import Graphics.Vulkan.Zero (Zero)
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_GEOMETRY_AABB_NV))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_GEOMETRY_NV))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV))
import Graphics.Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Graphics.Vulkan.Extensions.Handles (AccelerationStructureNV(..))
import Graphics.Vulkan.Extensions.VK_EXT_debug_report (DebugReportObjectTypeEXT(..))
import Graphics.Vulkan.Core10.APIConstants (SHADER_UNUSED_NV)
import Graphics.Vulkan.Core10.APIConstants (pattern SHADER_UNUSED_NV)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCompileDeferredNV
  :: FunPtr (Ptr Device_T -> Pipeline -> Word32 -> IO Result) -> Ptr Device_T -> Pipeline -> Word32 -> IO Result

-- | vkCompileDeferredNV - Deferred compilation of shaders
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' is the logical device
--     containing the ray tracing pipeline.
--
-- -   'Graphics.Vulkan.Core10.Handles.Pipeline' is the ray tracing
--     pipeline object containing the shaders.
--
-- -   @shader@ is the index of the shader to compile.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Device',
-- 'Graphics.Vulkan.Core10.Handles.Pipeline'
compileDeferredNV :: Device -> Pipeline -> ("shader" ::: Word32) -> IO ()
compileDeferredNV device pipeline shader = do
  let vkCompileDeferredNV' = mkVkCompileDeferredNV (pVkCompileDeferredNV (deviceCmds (device :: Device)))
  r <- vkCompileDeferredNV' (deviceHandle (device)) (pipeline) (shader)
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateAccelerationStructureNV
  :: FunPtr (Ptr Device_T -> Ptr AccelerationStructureCreateInfoNV -> Ptr AllocationCallbacks -> Ptr AccelerationStructureNV -> IO Result) -> Ptr Device_T -> Ptr AccelerationStructureCreateInfoNV -> Ptr AllocationCallbacks -> Ptr AccelerationStructureNV -> IO Result

-- | vkCreateAccelerationStructureNV - Create a new acceleration structure
-- object
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' is the logical device that
--     creates the buffer object.
--
-- -   @pCreateInfo@ is a pointer to a 'AccelerationStructureCreateInfoNV'
--     structure containing parameters affecting creation of the
--     acceleration structure.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pAccelerationStructure@ is a pointer to a
--     'Graphics.Vulkan.Extensions.Handles.AccelerationStructureNV' handle
--     in which the resulting acceleration structure object is returned.
--
-- = Description
--
-- Similar to other objects in Vulkan, the acceleration structure creation
-- merely creates an object with a specific “shape” as specified by the
-- information in 'AccelerationStructureInfoNV' and @compactedSize@ in
-- @pCreateInfo@. Populating the data in the object after allocating and
-- binding memory is done with 'cmdBuildAccelerationStructureNV' and
-- 'cmdCopyAccelerationStructureNV'.
--
-- Acceleration structure creation uses the count and type information from
-- the geometries, but does not use the data references in the structures.
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Device' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'AccelerationStructureCreateInfoNV' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   @pAccelerationStructure@ /must/ be a valid pointer to a
--     'Graphics.Vulkan.Extensions.Handles.AccelerationStructureNV' handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
-- = See Also
--
-- 'AccelerationStructureCreateInfoNV',
-- 'Graphics.Vulkan.Extensions.Handles.AccelerationStructureNV',
-- 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Graphics.Vulkan.Core10.Handles.Device'
createAccelerationStructureNV :: Device -> AccelerationStructureCreateInfoNV -> ("allocator" ::: Maybe AllocationCallbacks) -> IO (AccelerationStructureNV)
createAccelerationStructureNV device createInfo allocator = evalContT $ do
  let vkCreateAccelerationStructureNV' = mkVkCreateAccelerationStructureNV (pVkCreateAccelerationStructureNV (deviceCmds (device :: Device)))
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPAccelerationStructure <- ContT $ bracket (callocBytes @AccelerationStructureNV 8) free
  r <- lift $ vkCreateAccelerationStructureNV' (deviceHandle (device)) pCreateInfo pAllocator (pPAccelerationStructure)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pAccelerationStructure <- lift $ peek @AccelerationStructureNV pPAccelerationStructure
  pure $ (pAccelerationStructure)

-- | A safe wrapper for 'createAccelerationStructureNV' and
-- 'destroyAccelerationStructureNV' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withAccelerationStructureNV :: Device -> AccelerationStructureCreateInfoNV -> Maybe AllocationCallbacks -> (AccelerationStructureNV -> IO r) -> IO r
withAccelerationStructureNV device accelerationStructureCreateInfoNV allocationCallbacks =
  bracket
    (createAccelerationStructureNV device accelerationStructureCreateInfoNV allocationCallbacks)
    (\o -> destroyAccelerationStructureNV device o allocationCallbacks)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyAccelerationStructureNV
  :: FunPtr (Ptr Device_T -> AccelerationStructureNV -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> AccelerationStructureNV -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyAccelerationStructureNV - Destroy an acceleration structure
-- object
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' is the logical device that
--     destroys the buffer.
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
-- -   If 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     were provided when @accelerationStructure@ was created, a compatible
--     set of callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     were provided when @accelerationStructure@ was created, @pAllocator@
--     /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Device' handle
--
-- -   @accelerationStructure@ /must/ be a valid
--     'Graphics.Vulkan.Extensions.Handles.AccelerationStructureNV' handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   @accelerationStructure@ /must/ have been created, allocated, or
--     retrieved from 'Graphics.Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.Handles.AccelerationStructureNV',
-- 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Graphics.Vulkan.Core10.Handles.Device'
destroyAccelerationStructureNV :: Device -> AccelerationStructureNV -> ("allocator" ::: Maybe AllocationCallbacks) -> IO ()
destroyAccelerationStructureNV device accelerationStructure allocator = evalContT $ do
  let vkDestroyAccelerationStructureNV' = mkVkDestroyAccelerationStructureNV (pVkDestroyAccelerationStructureNV (deviceCmds (device :: Device)))
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroyAccelerationStructureNV' (deviceHandle (device)) (accelerationStructure) pAllocator
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetAccelerationStructureMemoryRequirementsNV
  :: FunPtr (Ptr Device_T -> Ptr AccelerationStructureMemoryRequirementsInfoNV -> Ptr (MemoryRequirements2KHR a) -> IO ()) -> Ptr Device_T -> Ptr AccelerationStructureMemoryRequirementsInfoNV -> Ptr (MemoryRequirements2KHR a) -> IO ()

-- | vkGetAccelerationStructureMemoryRequirementsNV - Get acceleration
-- structure memory requirements
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' is the logical device on
--     which the acceleration structure was created.
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
-- 'AccelerationStructureMemoryRequirementsInfoNV',
-- 'Graphics.Vulkan.Core10.Handles.Device',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.MemoryRequirements2KHR'
getAccelerationStructureMemoryRequirementsNV :: (PokeChain a, PeekChain a) => Device -> AccelerationStructureMemoryRequirementsInfoNV -> IO (MemoryRequirements2KHR a)
getAccelerationStructureMemoryRequirementsNV device info = evalContT $ do
  let vkGetAccelerationStructureMemoryRequirementsNV' = mkVkGetAccelerationStructureMemoryRequirementsNV (pVkGetAccelerationStructureMemoryRequirementsNV (deviceCmds (device :: Device)))
  pInfo <- ContT $ withCStruct (info)
  pPMemoryRequirements <- ContT (withZeroCStruct @(MemoryRequirements2KHR _))
  lift $ vkGetAccelerationStructureMemoryRequirementsNV' (deviceHandle (device)) pInfo (pPMemoryRequirements)
  pMemoryRequirements <- lift $ peekCStruct @(MemoryRequirements2KHR _) pPMemoryRequirements
  pure $ (pMemoryRequirements)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkBindAccelerationStructureMemoryNV
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr BindAccelerationStructureMemoryInfoNV -> IO Result) -> Ptr Device_T -> Word32 -> Ptr BindAccelerationStructureMemoryInfoNV -> IO Result

-- | vkBindAccelerationStructureMemoryNV - Bind acceleration structure memory
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' is the logical device that
--     owns the acceleration structures and memory.
--
-- -   @bindInfoCount@ is the number of elements in @pBindInfos@.
--
-- -   @pBindInfos@ is a pointer to an array of
--     'BindAccelerationStructureMemoryInfoNV' structures describing images
--     and memory to bind.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'BindAccelerationStructureMemoryInfoNV',
-- 'Graphics.Vulkan.Core10.Handles.Device'
bindAccelerationStructureMemoryNV :: Device -> ("bindInfos" ::: Vector BindAccelerationStructureMemoryInfoNV) -> IO ()
bindAccelerationStructureMemoryNV device bindInfos = evalContT $ do
  let vkBindAccelerationStructureMemoryNV' = mkVkBindAccelerationStructureMemoryNV (pVkBindAccelerationStructureMemoryNV (deviceCmds (device :: Device)))
  pPBindInfos <- ContT $ allocaBytesAligned @BindAccelerationStructureMemoryInfoNV ((Data.Vector.length (bindInfos)) * 56) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPBindInfos `plusPtr` (56 * (i)) :: Ptr BindAccelerationStructureMemoryInfoNV) (e) . ($ ())) (bindInfos)
  r <- lift $ vkBindAccelerationStructureMemoryNV' (deviceHandle (device)) ((fromIntegral (Data.Vector.length $ (bindInfos)) :: Word32)) (pPBindInfos)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyAccelerationStructureNV
  :: FunPtr (Ptr CommandBuffer_T -> AccelerationStructureNV -> AccelerationStructureNV -> CopyAccelerationStructureModeNV -> IO ()) -> Ptr CommandBuffer_T -> AccelerationStructureNV -> AccelerationStructureNV -> CopyAccelerationStructureModeNV -> IO ()

-- | vkCmdCopyAccelerationStructureNV - Copy an acceleration structure
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command will be recorded.
--
-- -   @dst@ is a pointer to the target acceleration structure for the
--     copy.
--
-- -   @src@ is a pointer to the source acceleration structure for the
--     copy.
--
-- -   @mode@ is a 'CopyAccelerationStructureModeNV' value specifying
--     additional operations to perform during the copy.
--
-- == Valid Usage
--
-- -   @mode@ /must/ be 'COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV' or
--     'COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV'
--
-- -   @src@ /must/ have been built with
--     'BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV' if @mode@ is
--     'COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV'
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @dst@ /must/ be a valid
--     'Graphics.Vulkan.Extensions.Handles.AccelerationStructureNV' handle
--
-- -   @src@ /must/ be a valid
--     'Graphics.Vulkan.Extensions.Handles.AccelerationStructureNV' handle
--
-- -   @mode@ /must/ be a valid 'CopyAccelerationStructureModeNV' value
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   Each of 'Graphics.Vulkan.Core10.Handles.CommandBuffer', @dst@, and
--     @src@ /must/ have been created, allocated, or retrieved from the
--     same 'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
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
-- 'Graphics.Vulkan.Extensions.Handles.AccelerationStructureNV',
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'CopyAccelerationStructureModeNV'
cmdCopyAccelerationStructureNV :: CommandBuffer -> ("dst" ::: AccelerationStructureNV) -> ("src" ::: AccelerationStructureNV) -> CopyAccelerationStructureModeNV -> IO ()
cmdCopyAccelerationStructureNV commandBuffer dst src mode = do
  let vkCmdCopyAccelerationStructureNV' = mkVkCmdCopyAccelerationStructureNV (pVkCmdCopyAccelerationStructureNV (deviceCmds (commandBuffer :: CommandBuffer)))
  vkCmdCopyAccelerationStructureNV' (commandBufferHandle (commandBuffer)) (dst) (src) (mode)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdWriteAccelerationStructuresPropertiesNV
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr AccelerationStructureNV -> QueryType -> QueryPool -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr AccelerationStructureNV -> QueryType -> QueryPool -> Word32 -> IO ()

-- | vkCmdWriteAccelerationStructuresPropertiesNV - Write acceleration
-- structure result parameters to query results.
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command will be recorded.
--
-- -   @accelerationStructureCount@ is the count of acceleration structures
--     for which to query the property.
--
-- -   @pAccelerationStructures@ is a pointer to an array of existing
--     previously built acceleration structures.
--
-- -   'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' is a
--     'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' value specifying
--     the type of queries managed by the pool.
--
-- -   'Graphics.Vulkan.Core10.Handles.QueryPool' is the query pool that
--     will manage the results of the query.
--
-- -   @firstQuery@ is the first query index within the query pool that
--     will contain the @accelerationStructureCount@ number of results.
--
-- == Valid Usage
--
-- -   'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' /must/ be
--     'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV'
--
-- -   'Graphics.Vulkan.Core10.Handles.QueryPool' /must/ have been created
--     with a 'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' matching
--     'Graphics.Vulkan.Core10.Enums.QueryType.QueryType'
--
-- -   The queries identified by 'Graphics.Vulkan.Core10.Handles.QueryPool'
--     and @firstQuery@ /must/ be /unavailable/
--
-- -   All acceleration structures in @accelerationStructures@ /must/ have
--     been built with
--     'BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV' if
--     'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' is
--     'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV'
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @pAccelerationStructures@ /must/ be a valid pointer to an array of
--     @accelerationStructureCount@ valid
--     'Graphics.Vulkan.Extensions.Handles.AccelerationStructureNV' handles
--
-- -   'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' value
--
-- -   'Graphics.Vulkan.Core10.Handles.QueryPool' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.QueryPool' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   @accelerationStructureCount@ /must/ be greater than @0@
--
-- -   Each of 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
--     'Graphics.Vulkan.Core10.Handles.QueryPool', and the elements of
--     @pAccelerationStructures@ /must/ have been created, allocated, or
--     retrieved from the same 'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
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
-- 'Graphics.Vulkan.Extensions.Handles.AccelerationStructureNV',
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.Handles.QueryPool',
-- 'Graphics.Vulkan.Core10.Enums.QueryType.QueryType'
cmdWriteAccelerationStructuresPropertiesNV :: CommandBuffer -> ("accelerationStructures" ::: Vector AccelerationStructureNV) -> QueryType -> QueryPool -> ("firstQuery" ::: Word32) -> IO ()
cmdWriteAccelerationStructuresPropertiesNV commandBuffer accelerationStructures queryType queryPool firstQuery = evalContT $ do
  let vkCmdWriteAccelerationStructuresPropertiesNV' = mkVkCmdWriteAccelerationStructuresPropertiesNV (pVkCmdWriteAccelerationStructuresPropertiesNV (deviceCmds (commandBuffer :: CommandBuffer)))
  pPAccelerationStructures <- ContT $ allocaBytesAligned @AccelerationStructureNV ((Data.Vector.length (accelerationStructures)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPAccelerationStructures `plusPtr` (8 * (i)) :: Ptr AccelerationStructureNV) (e)) (accelerationStructures)
  lift $ vkCmdWriteAccelerationStructuresPropertiesNV' (commandBufferHandle (commandBuffer)) ((fromIntegral (Data.Vector.length $ (accelerationStructures)) :: Word32)) (pPAccelerationStructures) (queryType) (queryPool) (firstQuery)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBuildAccelerationStructureNV
  :: FunPtr (Ptr CommandBuffer_T -> Ptr AccelerationStructureInfoNV -> Buffer -> DeviceSize -> Bool32 -> AccelerationStructureNV -> AccelerationStructureNV -> Buffer -> DeviceSize -> IO ()) -> Ptr CommandBuffer_T -> Ptr AccelerationStructureInfoNV -> Buffer -> DeviceSize -> Bool32 -> AccelerationStructureNV -> AccelerationStructureNV -> Buffer -> DeviceSize -> IO ()

-- | vkCmdBuildAccelerationStructureNV - Build an acceleration structure
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command will be recorded.
--
-- -   @pInfo@ contains the shared information for the acceleration
--     structure’s structure.
--
-- -   @instanceData@ is the buffer containing instance data that will be
--     used to build the acceleration structure as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#acceleration-structure-instance Accelerator structure instances.>
--     This parameter /must/ be @NULL@ for bottom level acceleration
--     structures.
--
-- -   @instanceOffset@ is the offset in bytes (relative to the start of
--     @instanceData@) at which the instance data is located.
--
-- -   @update@ specifies whether to update the @dst@ acceleration
--     structure with the data in @src@.
--
-- -   @dst@ is a pointer to the target acceleration structure for the
--     build.
--
-- -   @src@ is a pointer to an existing acceleration structure that is to
--     be used to update the @dst@ acceleration structure.
--
-- -   @scratch@ is the 'Graphics.Vulkan.Core10.Handles.Buffer' that will
--     be used as scratch memory for the build.
--
-- -   @scratchOffset@ is the offset in bytes relative to the start of
--     @scratch@ that will be used as a scratch memory.
--
-- == Valid Usage
--
-- -   @geometryCount@ /must/ be less than or equal to
--     'PhysicalDeviceRayTracingPropertiesNV'::@maxGeometryCount@
--
-- -   @dst@ /must/ have been created with compatible
--     'AccelerationStructureInfoNV' where
--     'AccelerationStructureInfoNV'::@type@ and
--     'AccelerationStructureInfoNV'::'Graphics.Vulkan.Core10.BaseType.Flags'
--     are identical, 'AccelerationStructureInfoNV'::@instanceCount@ and
--     'AccelerationStructureInfoNV'::@geometryCount@ for @dst@ are greater
--     than or equal to the build size and each geometry in
--     'AccelerationStructureInfoNV'::@pGeometries@ for @dst@ has greater
--     than or equal to the number of vertices, indices, and AABBs.
--
-- -   If @update@ is 'Graphics.Vulkan.Core10.BaseType.TRUE', @src@ /must/
--     not be 'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   If @update@ is 'Graphics.Vulkan.Core10.BaseType.TRUE', @src@ /must/
--     have been built before with
--     'BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV' set in
--     'AccelerationStructureInfoNV'::'Graphics.Vulkan.Core10.BaseType.Flags'
--
-- -   If @update@ is 'Graphics.Vulkan.Core10.BaseType.FALSE', The @size@
--     member of the
--     'Graphics.Vulkan.Core10.MemoryManagement.MemoryRequirements'
--     structure returned from a call to
--     'getAccelerationStructureMemoryRequirementsNV' with
--     'AccelerationStructureMemoryRequirementsInfoNV'::@accelerationStructure@
--     set to @dst@ and
--     'AccelerationStructureMemoryRequirementsInfoNV'::@type@ set to
--     'ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV'
--     /must/ be less than or equal to the size of @scratch@ minus
--     @scratchOffset@
--
-- -   If @update@ is 'Graphics.Vulkan.Core10.BaseType.TRUE', The @size@
--     member of the
--     'Graphics.Vulkan.Core10.MemoryManagement.MemoryRequirements'
--     structure returned from a call to
--     'getAccelerationStructureMemoryRequirementsNV' with
--     'AccelerationStructureMemoryRequirementsInfoNV'::@accelerationStructure@
--     set to @dst@ and
--     'AccelerationStructureMemoryRequirementsInfoNV'::@type@ set to
--     'ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV'
--     /must/ be less than or equal to the size of @scratch@ minus
--     @scratchOffset@
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @pInfo@ /must/ be a valid pointer to a valid
--     'AccelerationStructureInfoNV' structure
--
-- -   If @instanceData@ is not
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE', @instanceData@
--     /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Buffer' handle
--
-- -   @dst@ /must/ be a valid
--     'Graphics.Vulkan.Extensions.Handles.AccelerationStructureNV' handle
--
-- -   If @src@ is not 'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @src@ /must/ be a valid
--     'Graphics.Vulkan.Extensions.Handles.AccelerationStructureNV' handle
--
-- -   @scratch@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Buffer'
--     handle
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   Each of 'Graphics.Vulkan.Core10.Handles.CommandBuffer', @dst@,
--     @instanceData@, @scratch@, and @src@ that are valid handles of
--     non-ignored parameters /must/ have been created, allocated, or
--     retrieved from the same 'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
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
-- 'AccelerationStructureInfoNV',
-- 'Graphics.Vulkan.Extensions.Handles.AccelerationStructureNV',
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.Handles.Buffer',
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.BaseType.DeviceSize'
cmdBuildAccelerationStructureNV :: CommandBuffer -> AccelerationStructureInfoNV -> ("instanceData" ::: Buffer) -> ("instanceOffset" ::: DeviceSize) -> ("update" ::: Bool) -> ("dst" ::: AccelerationStructureNV) -> ("src" ::: AccelerationStructureNV) -> ("scratch" ::: Buffer) -> ("scratchOffset" ::: DeviceSize) -> IO ()
cmdBuildAccelerationStructureNV commandBuffer info instanceData instanceOffset update dst src scratch scratchOffset = evalContT $ do
  let vkCmdBuildAccelerationStructureNV' = mkVkCmdBuildAccelerationStructureNV (pVkCmdBuildAccelerationStructureNV (deviceCmds (commandBuffer :: CommandBuffer)))
  pInfo <- ContT $ withCStruct (info)
  lift $ vkCmdBuildAccelerationStructureNV' (commandBufferHandle (commandBuffer)) pInfo (instanceData) (instanceOffset) (boolToBool32 (update)) (dst) (src) (scratch) (scratchOffset)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdTraceRaysNV
  :: FunPtr (Ptr CommandBuffer_T -> Buffer -> DeviceSize -> Buffer -> DeviceSize -> DeviceSize -> Buffer -> DeviceSize -> DeviceSize -> Buffer -> DeviceSize -> DeviceSize -> Word32 -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Buffer -> DeviceSize -> Buffer -> DeviceSize -> DeviceSize -> Buffer -> DeviceSize -> DeviceSize -> Buffer -> DeviceSize -> DeviceSize -> Word32 -> Word32 -> Word32 -> IO ()

-- | vkCmdTraceRaysNV - Initialize a ray tracing dispatch
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command will be recorded.
--
-- -   @raygenShaderBindingTableBuffer@ is the buffer object that holds the
--     shader binding table data for the ray generation shader stage.
--
-- -   @raygenShaderBindingOffset@ is the offset in bytes (relative to
--     @raygenShaderBindingTableBuffer@) of the ray generation shader being
--     used for the trace.
--
-- -   @missShaderBindingTableBuffer@ is the buffer object that holds the
--     shader binding table data for the miss shader stage.
--
-- -   @missShaderBindingOffset@ is the offset in bytes (relative to
--     @missShaderBindingTableBuffer@) of the miss shader being used for
--     the trace.
--
-- -   @missShaderBindingStride@ is the size in bytes of each shader
--     binding table record in @missShaderBindingTableBuffer@.
--
-- -   @hitShaderBindingTableBuffer@ is the buffer object that holds the
--     shader binding table data for the hit shader stages.
--
-- -   @hitShaderBindingOffset@ is the offset in bytes (relative to
--     @hitShaderBindingTableBuffer@) of the hit shader group being used
--     for the trace.
--
-- -   @hitShaderBindingStride@ is the size in bytes of each shader binding
--     table record in @hitShaderBindingTableBuffer@.
--
-- -   @callableShaderBindingTableBuffer@ is the buffer object that holds
--     the shader binding table data for the callable shader stage.
--
-- -   @callableShaderBindingOffset@ is the offset in bytes (relative to
--     @callableShaderBindingTableBuffer@) of the callable shader being
--     used for the trace.
--
-- -   @callableShaderBindingStride@ is the size in bytes of each shader
--     binding table record in @callableShaderBindingTableBuffer@.
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
-- -   If a 'Graphics.Vulkan.Core10.Handles.ImageView' is sampled with
--     'Graphics.Vulkan.Core10.Enums.Filter.FILTER_LINEAR' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   If a 'Graphics.Vulkan.Core10.Handles.ImageView' is accessed using
--     atomic operations as a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   If a 'Graphics.Vulkan.Core10.Handles.ImageView' is sampled with
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as
--     a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   Any 'Graphics.Vulkan.Core10.Handles.ImageView' being sampled with
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as
--     a result of this command /must/ have a
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.ImageViewType' and
--     format that supports cubic filtering, as specified by
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   Any 'Graphics.Vulkan.Core10.Handles.ImageView' being sampled with
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT'
--     with a reduction mode of either
--     'Graphics.Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MIN'
--     or
--     'Graphics.Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MAX'
--     as a result of this command /must/ have a
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.ImageViewType' and
--     format that supports cubic filtering together with minmax filtering,
--     as specified by
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubicMinmax@
--     returned by
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   Any 'Graphics.Vulkan.Core10.Handles.Image' created with a
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo'::'Graphics.Vulkan.Core10.BaseType.Flags'
--     containing
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Graphics.Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode'
--     of
--     'Graphics.Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'.
--
-- -   For each set /n/ that is statically used by the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind
--     point used by this command, a descriptor set /must/ have been bound
--     to /n/ at the same pipeline bind point, with a
--     'Graphics.Vulkan.Core10.Handles.PipelineLayout' that is compatible
--     for set /n/, with the
--     'Graphics.Vulkan.Core10.Handles.PipelineLayout' used to create the
--     current 'Graphics.Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   For each push constant that is statically used by the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind
--     point used by this command, a push constant value /must/ have been
--     set for the same pipeline bind point, with a
--     'Graphics.Vulkan.Core10.Handles.PipelineLayout' that is compatible
--     for push constants, with the
--     'Graphics.Vulkan.Core10.Handles.PipelineLayout' used to create the
--     current 'Graphics.Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   Descriptors in each bound descriptor set, specified via
--     'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets',
--     /must/ be valid if they are statically used by the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind
--     point used by this command
--
-- -   A valid pipeline /must/ be bound to the pipeline bind point used by
--     this command
--
-- -   If the 'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command requires any dynamic state,
--     that state /must/ have been set for
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer', and done so after
--     any previously bound pipeline with the corresponding state not
--     specified as dynamic
--
-- -   There /must/ not have been any calls to dynamic state setting
--     commands for any state not specified as dynamic in the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command, since that pipeline was
--     bound
--
-- -   If the 'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.Core10.Handles.Sampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used to sample
--     from any 'Graphics.Vulkan.Core10.Handles.Image' with a
--     'Graphics.Vulkan.Core10.Handles.ImageView' of the type
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_3D',
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE',
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_1D_ARRAY',
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY'
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY',
--     in any shader stage
--
-- -   If the 'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.Core10.Handles.Sampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions with @ImplicitLod@, @Dref@ or @Proj@ in their name, in
--     any shader stage
--
-- -   If the 'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.Core10.Handles.Sampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions that includes a LOD bias or any offset values, in any
--     shader stage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a uniform buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a storage buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is an unprotected
--     command buffer, any resource accessed by the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command /must/ not be a protected
--     resource
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is a protected
--     command buffer, any resource written to by the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command /must/ not be an
--     unprotected resource
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is a protected
--     command buffer, pipeline stages other than the framebuffer-space and
--     compute stages in the 'Graphics.Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point /must/ not write to any
--     resource
--
-- -   @raygenShaderBindingOffset@ /must/ be less than the size of
--     @raygenShaderBindingTableBuffer@
--
-- -   @raygenShaderBindingOffset@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPropertiesNV'::@shaderGroupBaseAlignment@
--
-- -   @missShaderBindingOffset@ /must/ be less than the size of
--     @missShaderBindingTableBuffer@
--
-- -   @missShaderBindingOffset@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPropertiesNV'::@shaderGroupBaseAlignment@
--
-- -   @hitShaderBindingOffset@ /must/ be less than the size of
--     @hitShaderBindingTableBuffer@
--
-- -   @hitShaderBindingOffset@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPropertiesNV'::@shaderGroupBaseAlignment@
--
-- -   @callableShaderBindingOffset@ /must/ be less than the size of
--     @callableShaderBindingTableBuffer@
--
-- -   @callableShaderBindingOffset@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPropertiesNV'::@shaderGroupBaseAlignment@
--
-- -   @missShaderBindingStride@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPropertiesNV'::@shaderGroupHandleSize@
--
-- -   @hitShaderBindingStride@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPropertiesNV'::@shaderGroupHandleSize@
--
-- -   @callableShaderBindingStride@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPropertiesNV'::@shaderGroupHandleSize@
--
-- -   @missShaderBindingStride@ /must/ be a less than or equal to
--     'PhysicalDeviceRayTracingPropertiesNV'::@maxShaderGroupStride@
--
-- -   @hitShaderBindingStride@ /must/ be a less than or equal to
--     'PhysicalDeviceRayTracingPropertiesNV'::@maxShaderGroupStride@
--
-- -   @callableShaderBindingStride@ /must/ be a less than or equal to
--     'PhysicalDeviceRayTracingPropertiesNV'::@maxShaderGroupStride@
--
-- -   @width@ /must/ be less than or equal to
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[0]
--
-- -   @height@ /must/ be less than or equal to
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[1]
--
-- -   @depth@ /must/ be less than or equal to
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[2]
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @raygenShaderBindingTableBuffer@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Buffer' handle
--
-- -   If @missShaderBindingTableBuffer@ is not
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @missShaderBindingTableBuffer@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Buffer' handle
--
-- -   If @hitShaderBindingTableBuffer@ is not
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @hitShaderBindingTableBuffer@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Buffer' handle
--
-- -   If @callableShaderBindingTableBuffer@ is not
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @callableShaderBindingTableBuffer@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Buffer' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   Each of @callableShaderBindingTableBuffer@,
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer',
--     @hitShaderBindingTableBuffer@, @missShaderBindingTableBuffer@, and
--     @raygenShaderBindingTableBuffer@ that are valid handles of
--     non-ignored parameters /must/ have been created, allocated, or
--     retrieved from the same 'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
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
-- 'Graphics.Vulkan.Core10.Handles.Buffer',
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.BaseType.DeviceSize'
cmdTraceRaysNV :: CommandBuffer -> ("raygenShaderBindingTableBuffer" ::: Buffer) -> ("raygenShaderBindingOffset" ::: DeviceSize) -> ("missShaderBindingTableBuffer" ::: Buffer) -> ("missShaderBindingOffset" ::: DeviceSize) -> ("missShaderBindingStride" ::: DeviceSize) -> ("hitShaderBindingTableBuffer" ::: Buffer) -> ("hitShaderBindingOffset" ::: DeviceSize) -> ("hitShaderBindingStride" ::: DeviceSize) -> ("callableShaderBindingTableBuffer" ::: Buffer) -> ("callableShaderBindingOffset" ::: DeviceSize) -> ("callableShaderBindingStride" ::: DeviceSize) -> ("width" ::: Word32) -> ("height" ::: Word32) -> ("depth" ::: Word32) -> IO ()
cmdTraceRaysNV commandBuffer raygenShaderBindingTableBuffer raygenShaderBindingOffset missShaderBindingTableBuffer missShaderBindingOffset missShaderBindingStride hitShaderBindingTableBuffer hitShaderBindingOffset hitShaderBindingStride callableShaderBindingTableBuffer callableShaderBindingOffset callableShaderBindingStride width height depth = do
  let vkCmdTraceRaysNV' = mkVkCmdTraceRaysNV (pVkCmdTraceRaysNV (deviceCmds (commandBuffer :: CommandBuffer)))
  vkCmdTraceRaysNV' (commandBufferHandle (commandBuffer)) (raygenShaderBindingTableBuffer) (raygenShaderBindingOffset) (missShaderBindingTableBuffer) (missShaderBindingOffset) (missShaderBindingStride) (hitShaderBindingTableBuffer) (hitShaderBindingOffset) (hitShaderBindingStride) (callableShaderBindingTableBuffer) (callableShaderBindingOffset) (callableShaderBindingStride) (width) (height) (depth)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetRayTracingShaderGroupHandlesNV
  :: FunPtr (Ptr Device_T -> Pipeline -> Word32 -> Word32 -> CSize -> Ptr () -> IO Result) -> Ptr Device_T -> Pipeline -> Word32 -> Word32 -> CSize -> Ptr () -> IO Result

-- | vkGetRayTracingShaderGroupHandlesNV - Query ray tracing pipeline shader
-- group handles
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' is the logical device
--     containing the ray tracing pipeline.
--
-- -   'Graphics.Vulkan.Core10.Handles.Pipeline' is the ray tracing
--     pipeline object containing the shaders.
--
-- -   @firstGroup@ is the index of the first group to retrieve a handle
--     for from the 'RayTracingShaderGroupCreateInfoNV'::@pGroups@ array.
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
--     number of shader groups in
--     'Graphics.Vulkan.Core10.Handles.Pipeline'.
--
-- -   @dataSize@ /must/ be at least
--     'PhysicalDeviceRayTracingPropertiesNV'::@shaderGroupHandleSize@ ×
--     @groupCount@
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Device' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.Pipeline' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Pipeline' handle
--
-- -   @pData@ /must/ be a valid pointer to an array of @dataSize@ bytes
--
-- -   @dataSize@ /must/ be greater than @0@
--
-- -   'Graphics.Vulkan.Core10.Handles.Pipeline' /must/ have been created,
--     allocated, or retrieved from 'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Device',
-- 'Graphics.Vulkan.Core10.Handles.Pipeline'
getRayTracingShaderGroupHandlesNV :: Device -> Pipeline -> ("firstGroup" ::: Word32) -> ("groupCount" ::: Word32) -> ("dataSize" ::: Word64) -> ("data" ::: Ptr ()) -> IO ()
getRayTracingShaderGroupHandlesNV device pipeline firstGroup groupCount dataSize data' = do
  let vkGetRayTracingShaderGroupHandlesNV' = mkVkGetRayTracingShaderGroupHandlesNV (pVkGetRayTracingShaderGroupHandlesNV (deviceCmds (device :: Device)))
  r <- vkGetRayTracingShaderGroupHandlesNV' (deviceHandle (device)) (pipeline) (firstGroup) (groupCount) (CSize (dataSize)) (data')
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetAccelerationStructureHandleNV
  :: FunPtr (Ptr Device_T -> AccelerationStructureNV -> CSize -> Ptr () -> IO Result) -> Ptr Device_T -> AccelerationStructureNV -> CSize -> Ptr () -> IO Result

-- | vkGetAccelerationStructureHandleNV - Get opaque acceleration structure
-- handle
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' is the logical device that
--     owns the acceleration structures.
--
-- -   @accelerationStructure@ is the acceleration structure.
--
-- -   @dataSize@ is the size in bytes of the buffer pointed to by @pData@.
--
-- -   @pData@ is a pointer to a user-allocated buffer where the results
--     will be written.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.Handles.AccelerationStructureNV',
-- 'Graphics.Vulkan.Core10.Handles.Device'
getAccelerationStructureHandleNV :: Device -> AccelerationStructureNV -> ("dataSize" ::: Word64) -> ("data" ::: Ptr ()) -> IO ()
getAccelerationStructureHandleNV device accelerationStructure dataSize data' = do
  let vkGetAccelerationStructureHandleNV' = mkVkGetAccelerationStructureHandleNV (pVkGetAccelerationStructureHandleNV (deviceCmds (device :: Device)))
  r <- vkGetAccelerationStructureHandleNV' (deviceHandle (device)) (accelerationStructure) (CSize (dataSize)) (data')
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateRayTracingPipelinesNV
  :: FunPtr (Ptr Device_T -> PipelineCache -> Word32 -> Ptr (RayTracingPipelineCreateInfoNV a) -> Ptr AllocationCallbacks -> Ptr Pipeline -> IO Result) -> Ptr Device_T -> PipelineCache -> Word32 -> Ptr (RayTracingPipelineCreateInfoNV a) -> Ptr AllocationCallbacks -> Ptr Pipeline -> IO Result

-- | vkCreateRayTracingPipelinesNV - Creates a new ray tracing pipeline
-- object
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' is the logical device that
--     creates the ray tracing pipelines.
--
-- -   'Graphics.Vulkan.Core10.Handles.PipelineCache' is either
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE', indicating that
--     pipeline caching is disabled, or the handle of a valid
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#pipelines-cache pipeline cache>
--     object, in which case use of that cache is enabled for the duration
--     of the command.
--
-- -   @createInfoCount@ is the length of the @pCreateInfos@ and
--     @pPipelines@ arrays.
--
-- -   @pCreateInfos@ is a pointer to an array of
--     'RayTracingPipelineCreateInfoNV' structures.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pPipelines@ is a pointer to an array in which the resulting ray
--     tracing pipeline objects are returned.
--
-- == Valid Usage
--
-- -   If the 'Graphics.Vulkan.Core10.BaseType.Flags' member of any element
--     of @pCreateInfos@ contains the
--     'Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and the @basePipelineIndex@ member of that same element is not
--     @-1@, @basePipelineIndex@ /must/ be less than the index into
--     @pCreateInfos@ that corresponds to that element
--
-- -   If the 'Graphics.Vulkan.Core10.BaseType.Flags' member of any element
--     of @pCreateInfos@ contains the
--     'Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, the base pipeline /must/ have been created with the
--     'Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT'
--     flag set
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Device' handle
--
-- -   If 'Graphics.Vulkan.Core10.Handles.PipelineCache' is not
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE',
--     'Graphics.Vulkan.Core10.Handles.PipelineCache' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.PipelineCache' handle
--
-- -   @pCreateInfos@ /must/ be a valid pointer to an array of
--     @createInfoCount@ valid 'RayTracingPipelineCreateInfoNV' structures
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   @pPipelines@ /must/ be a valid pointer to an array of
--     @createInfoCount@ 'Graphics.Vulkan.Core10.Handles.Pipeline' handles
--
-- -   @createInfoCount@ /must/ be greater than @0@
--
-- -   If 'Graphics.Vulkan.Core10.Handles.PipelineCache' is a valid handle,
--     it /must/ have been created, allocated, or retrieved from
--     'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_INVALID_SHADER_NV'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Graphics.Vulkan.Core10.Handles.Device',
-- 'Graphics.Vulkan.Core10.Handles.Pipeline',
-- 'Graphics.Vulkan.Core10.Handles.PipelineCache',
-- 'RayTracingPipelineCreateInfoNV'
createRayTracingPipelinesNV :: PokeChain a => Device -> PipelineCache -> ("createInfos" ::: Vector (RayTracingPipelineCreateInfoNV a)) -> ("allocator" ::: Maybe AllocationCallbacks) -> IO (("pipelines" ::: Vector Pipeline))
createRayTracingPipelinesNV device pipelineCache createInfos allocator = evalContT $ do
  let vkCreateRayTracingPipelinesNV' = mkVkCreateRayTracingPipelinesNV (pVkCreateRayTracingPipelinesNV (deviceCmds (device :: Device)))
  pPCreateInfos <- ContT $ allocaBytesAligned @(RayTracingPipelineCreateInfoNV _) ((Data.Vector.length (createInfos)) * 80) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPCreateInfos `plusPtr` (80 * (i)) :: Ptr (RayTracingPipelineCreateInfoNV _)) (e) . ($ ())) (createInfos)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPPipelines <- ContT $ bracket (callocBytes @Pipeline ((fromIntegral ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32))) * 8)) free
  r <- lift $ vkCreateRayTracingPipelinesNV' (deviceHandle (device)) (pipelineCache) ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32)) (pPCreateInfos) pAllocator (pPPipelines)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPipelines <- lift $ generateM (fromIntegral ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32))) (\i -> peek @Pipeline ((pPPipelines `advancePtrBytes` (8 * (i)) :: Ptr Pipeline)))
  pure $ (pPipelines)


-- | VkRayTracingShaderGroupCreateInfoNV - Structure specifying shaders in a
-- shader group
--
-- == Valid Usage
--
-- -   If @type@ is 'RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV' then
--     @generalShader@ /must/ be a valid index into @pStages@ referring to
--     a shader of
--     'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_RAYGEN_BIT_NV',
--     'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MISS_BIT_NV',
--     or
--     'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_CALLABLE_BIT_NV'
--
-- -   If @type@ is 'RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV' then
--     @closestHitShader@, @anyHitShader@, and @intersectionShader@ /must/
--     be 'Graphics.Vulkan.Core10.APIConstants.SHADER_UNUSED_NV'
--
-- -   If @type@ is 'RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV'
--     then @intersectionShader@ /must/ be a valid index into @pStages@
--     referring to a shader of
--     'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_INTERSECTION_BIT_NV'
--
-- -   If @type@ is 'RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV'
--     then @intersectionShader@ /must/ be
--     'Graphics.Vulkan.Core10.APIConstants.SHADER_UNUSED_NV'
--
-- -   @closestHitShader@ /must/ be either
--     'Graphics.Vulkan.Core10.APIConstants.SHADER_UNUSED_NV' or a valid
--     index into @pStages@ referring to a shader of
--     'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_CLOSEST_HIT_BIT_NV'
--
-- -   @anyHitShader@ /must/ be either
--     'Graphics.Vulkan.Core10.APIConstants.SHADER_UNUSED_NV' or a valid
--     index into @pStages@ referring to a shader of
--     'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_ANY_HIT_BIT_NV'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @type@ /must/ be a valid 'RayTracingShaderGroupTypeNV' value
--
-- = See Also
--
-- 'RayTracingPipelineCreateInfoNV', 'RayTracingShaderGroupTypeNV',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data RayTracingShaderGroupCreateInfoNV = RayTracingShaderGroupCreateInfoNV
  { -- | @type@ is the type of hit group specified in this structure.
    type' :: RayTracingShaderGroupTypeNV
  , -- | @generalShader@ is the index of the ray generation, miss, or callable
    -- shader from 'RayTracingPipelineCreateInfoNV'::@pStages@ in the group if
    -- the shader group has @type@ of
    -- 'RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV' and
    -- 'Graphics.Vulkan.Core10.APIConstants.SHADER_UNUSED_NV' otherwise.
    generalShader :: Word32
  , -- | @closestHitShader@ is the optional index of the closest hit shader from
    -- 'RayTracingPipelineCreateInfoNV'::@pStages@ in the group if the shader
    -- group has @type@ of
    -- 'RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV' or
    -- 'RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV' and
    -- 'Graphics.Vulkan.Core10.APIConstants.SHADER_UNUSED_NV' otherwise.
    closestHitShader :: Word32
  , -- | @anyHitShader@ is the optional index of the any-hit shader from
    -- 'RayTracingPipelineCreateInfoNV'::@pStages@ in the group if the shader
    -- group has @type@ of
    -- 'RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV' or
    -- 'RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV' and
    -- 'Graphics.Vulkan.Core10.APIConstants.SHADER_UNUSED_NV' otherwise.
    anyHitShader :: Word32
  , -- | @intersectionShader@ is the index of the intersection shader from
    -- 'RayTracingPipelineCreateInfoNV'::@pStages@ in the group if the shader
    -- group has @type@ of
    -- 'RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV' and
    -- 'Graphics.Vulkan.Core10.APIConstants.SHADER_UNUSED_NV' otherwise.
    intersectionShader :: Word32
  }
  deriving (Typeable)
deriving instance Show RayTracingShaderGroupCreateInfoNV

instance ToCStruct RayTracingShaderGroupCreateInfoNV where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RayTracingShaderGroupCreateInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr RayTracingShaderGroupTypeNV)) (type')
    poke ((p `plusPtr` 20 :: Ptr Word32)) (generalShader)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (closestHitShader)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (anyHitShader)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (intersectionShader)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr RayTracingShaderGroupTypeNV)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    f

instance FromCStruct RayTracingShaderGroupCreateInfoNV where
  peekCStruct p = do
    type' <- peek @RayTracingShaderGroupTypeNV ((p `plusPtr` 16 :: Ptr RayTracingShaderGroupTypeNV))
    generalShader <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    closestHitShader <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    anyHitShader <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    intersectionShader <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pure $ RayTracingShaderGroupCreateInfoNV
             type' generalShader closestHitShader anyHitShader intersectionShader

instance Storable RayTracingShaderGroupCreateInfoNV where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero RayTracingShaderGroupCreateInfoNV where
  zero = RayTracingShaderGroupCreateInfoNV
           zero
           zero
           zero
           zero
           zero


-- | VkRayTracingPipelineCreateInfoNV - Structure specifying parameters of a
-- newly created ray tracing pipeline
--
-- = Description
--
-- The parameters @basePipelineHandle@ and @basePipelineIndex@ are
-- described in more detail in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#pipelines-pipeline-derivatives Pipeline Derivatives>.
--
-- == Valid Usage
--
-- -   If 'Graphics.Vulkan.Core10.BaseType.Flags' contains the
--     'Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineIndex@ is @-1@, @basePipelineHandle@ /must/
--     be a valid handle to a ray tracing
--     'Graphics.Vulkan.Core10.Handles.Pipeline'
--
-- -   If 'Graphics.Vulkan.Core10.BaseType.Flags' contains the
--     'Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineHandle@ is
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @basePipelineIndex@ /must/ be a valid index into the calling
--     command’s @pCreateInfos@ parameter
--
-- -   If 'Graphics.Vulkan.Core10.BaseType.Flags' contains the
--     'Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineIndex@ is not @-1@, @basePipelineHandle@
--     /must/ be 'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   If 'Graphics.Vulkan.Core10.BaseType.Flags' contains the
--     'Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineHandle@ is not
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @basePipelineIndex@ /must/ be @-1@
--
-- -   The @stage@ member of one element of @pStages@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_RAYGEN_BIT_NV'
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
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxPerStageResources@
--
-- -   @maxRecursionDepth@ /must/ be less than or equal to
--     'PhysicalDeviceRayTracingPropertiesNV'::@maxRecursionDepth@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV'
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.Extensions.VK_EXT_pipeline_creation_feedback.PipelineCreationFeedbackCreateInfoEXT'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   'Graphics.Vulkan.Core10.BaseType.Flags' /must/ be a valid
--     combination of
--     'Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits'
--     values
--
-- -   @pStages@ /must/ be a valid pointer to an array of @stageCount@
--     valid
--     'Graphics.Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo'
--     structures
--
-- -   @pGroups@ /must/ be a valid pointer to an array of @groupCount@
--     valid 'RayTracingShaderGroupCreateInfoNV' structures
--
-- -   @layout@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.PipelineLayout' handle
--
-- -   @stageCount@ /must/ be greater than @0@
--
-- -   @groupCount@ /must/ be greater than @0@
--
-- -   Both of @basePipelineHandle@, and @layout@ that are valid handles of
--     non-ignored parameters /must/ have been created, allocated, or
--     retrieved from the same 'Graphics.Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Pipeline',
-- 'Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlags',
-- 'Graphics.Vulkan.Core10.Handles.PipelineLayout',
-- 'Graphics.Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo',
-- 'RayTracingShaderGroupCreateInfoNV',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createRayTracingPipelinesNV'
data RayTracingPipelineCreateInfoNV (es :: [Type]) = RayTracingPipelineCreateInfoNV
  { -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
    next :: Chain es
  , -- | 'Graphics.Vulkan.Core10.BaseType.Flags' is a bitmask of
    -- 'Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits'
    -- specifying how the pipeline will be generated.
    flags :: PipelineCreateFlags
  , -- | @pStages@ is a pointer to an array of @stageCount@
    -- 'Graphics.Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo'
    -- structures describing the set of the shader stages to be included in the
    -- ray tracing pipeline.
    stages :: Vector (SomeStruct PipelineShaderStageCreateInfo)
  , -- | @pGroups@ is a pointer to an array of @groupCount@
    -- 'RayTracingShaderGroupCreateInfoNV' structures describing the set of the
    -- shader stages to be included in each shader group in the ray tracing
    -- pipeline.
    groups :: Vector RayTracingShaderGroupCreateInfoNV
  , -- | @maxRecursionDepth@ is the maximum recursion that will be called from
    -- this pipeline.
    maxRecursionDepth :: Word32
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
deriving instance Show (Chain es) => Show (RayTracingPipelineCreateInfoNV es)

instance Extensible RayTracingPipelineCreateInfoNV where
  extensibleType = STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV
  setNext x next = x{next = next}
  getNext RayTracingPipelineCreateInfoNV{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends RayTracingPipelineCreateInfoNV e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PipelineCreationFeedbackCreateInfoEXT = Just f
    | otherwise = Nothing

instance PokeChain es => ToCStruct (RayTracingPipelineCreateInfoNV es) where
  withCStruct x f = allocaBytesAligned 80 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RayTracingPipelineCreateInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (stages)) :: Word32))
    pPStages' <- ContT $ allocaBytesAligned @(PipelineShaderStageCreateInfo _) ((Data.Vector.length (stages)) * 48) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPStages' `plusPtr` (48 * (i)) :: Ptr (PipelineShaderStageCreateInfo _))) (e) . ($ ())) (stages)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr (PipelineShaderStageCreateInfo _)))) (pPStages')
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (groups)) :: Word32))
    pPGroups' <- ContT $ allocaBytesAligned @RayTracingShaderGroupCreateInfoNV ((Data.Vector.length (groups)) * 40) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPGroups' `plusPtr` (40 * (i)) :: Ptr RayTracingShaderGroupCreateInfoNV) (e) . ($ ())) (groups)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr RayTracingShaderGroupCreateInfoNV))) (pPGroups')
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) (maxRecursionDepth)
    lift $ poke ((p `plusPtr` 56 :: Ptr PipelineLayout)) (layout)
    lift $ poke ((p `plusPtr` 64 :: Ptr Pipeline)) (basePipelineHandle)
    lift $ poke ((p `plusPtr` 72 :: Ptr Int32)) (basePipelineIndex)
    lift $ f
  cStructSize = 80
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    pPStages' <- ContT $ allocaBytesAligned @(PipelineShaderStageCreateInfo _) ((Data.Vector.length (mempty)) * 48) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPStages' `plusPtr` (48 * (i)) :: Ptr (PipelineShaderStageCreateInfo _))) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr (PipelineShaderStageCreateInfo _)))) (pPStages')
    pPGroups' <- ContT $ allocaBytesAligned @RayTracingShaderGroupCreateInfoNV ((Data.Vector.length (mempty)) * 40) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPGroups' `plusPtr` (40 * (i)) :: Ptr RayTracingShaderGroupCreateInfoNV) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr RayTracingShaderGroupCreateInfoNV))) (pPGroups')
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 56 :: Ptr PipelineLayout)) (zero)
    lift $ poke ((p `plusPtr` 72 :: Ptr Int32)) (zero)
    lift $ f

instance PeekChain es => FromCStruct (RayTracingPipelineCreateInfoNV es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @PipelineCreateFlags ((p `plusPtr` 16 :: Ptr PipelineCreateFlags))
    stageCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pStages <- peek @(Ptr (PipelineShaderStageCreateInfo _)) ((p `plusPtr` 24 :: Ptr (Ptr (PipelineShaderStageCreateInfo a))))
    pStages' <- generateM (fromIntegral stageCount) (\i -> peekSomeCStruct (forgetExtensions ((pStages `advancePtrBytes` (48 * (i)) :: Ptr (PipelineShaderStageCreateInfo _)))))
    groupCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pGroups <- peek @(Ptr RayTracingShaderGroupCreateInfoNV) ((p `plusPtr` 40 :: Ptr (Ptr RayTracingShaderGroupCreateInfoNV)))
    pGroups' <- generateM (fromIntegral groupCount) (\i -> peekCStruct @RayTracingShaderGroupCreateInfoNV ((pGroups `advancePtrBytes` (40 * (i)) :: Ptr RayTracingShaderGroupCreateInfoNV)))
    maxRecursionDepth <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    layout <- peek @PipelineLayout ((p `plusPtr` 56 :: Ptr PipelineLayout))
    basePipelineHandle <- peek @Pipeline ((p `plusPtr` 64 :: Ptr Pipeline))
    basePipelineIndex <- peek @Int32 ((p `plusPtr` 72 :: Ptr Int32))
    pure $ RayTracingPipelineCreateInfoNV
             next flags pStages' pGroups' maxRecursionDepth layout basePipelineHandle basePipelineIndex

instance es ~ '[] => Zero (RayTracingPipelineCreateInfoNV es) where
  zero = RayTracingPipelineCreateInfoNV
           ()
           zero
           mempty
           mempty
           zero
           zero
           zero
           zero


-- | VkGeometryTrianglesNV - Structure specifying a triangle geometry in a
-- bottom-level acceleration structure
--
-- = Description
--
-- If 'Graphics.Vulkan.Core10.Enums.IndexType.IndexType' is
-- 'Graphics.Vulkan.Core10.Enums.IndexType.INDEX_TYPE_NONE_NV', then this
-- structure describes a set of triangles determined by @vertexCount@.
-- Otherwise, this structure describes a set of indexed triangles
-- determined by @indexCount@.
--
-- == Valid Usage
--
-- -   @vertexOffset@ /must/ be less than the size of @vertexData@
--
-- -   @vertexOffset@ /must/ be a multiple of the component size of
--     @vertexFormat@
--
-- -   @vertexFormat@ /must/ be one of
--     'Graphics.Vulkan.Core10.Enums.Format.FORMAT_R32G32B32_SFLOAT',
--     'Graphics.Vulkan.Core10.Enums.Format.FORMAT_R32G32_SFLOAT',
--     'Graphics.Vulkan.Core10.Enums.Format.FORMAT_R16G16B16_SFLOAT',
--     'Graphics.Vulkan.Core10.Enums.Format.FORMAT_R16G16_SFLOAT',
--     'Graphics.Vulkan.Core10.Enums.Format.FORMAT_R16G16_SNORM', or
--     'Graphics.Vulkan.Core10.Enums.Format.FORMAT_R16G16B16_SNORM'
--
-- -   @indexOffset@ /must/ be less than the size of @indexData@
--
-- -   @indexOffset@ /must/ be a multiple of the element size of
--     'Graphics.Vulkan.Core10.Enums.IndexType.IndexType'
--
-- -   'Graphics.Vulkan.Core10.Enums.IndexType.IndexType' /must/ be
--     'Graphics.Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT16',
--     'Graphics.Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT32', or
--     'Graphics.Vulkan.Core10.Enums.IndexType.INDEX_TYPE_NONE_NV'
--
-- -   @indexData@ /must/ be
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE' if
--     'Graphics.Vulkan.Core10.Enums.IndexType.IndexType' is
--     'Graphics.Vulkan.Core10.Enums.IndexType.INDEX_TYPE_NONE_NV'
--
-- -   @indexData@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Buffer' handle if
--     'Graphics.Vulkan.Core10.Enums.IndexType.IndexType' is not
--     'Graphics.Vulkan.Core10.Enums.IndexType.INDEX_TYPE_NONE_NV'
--
-- -   @indexCount@ /must/ be @0@ if
--     'Graphics.Vulkan.Core10.Enums.IndexType.IndexType' is
--     'Graphics.Vulkan.Core10.Enums.IndexType.INDEX_TYPE_NONE_NV'
--
-- -   @transformOffset@ /must/ be less than the size of @transformData@
--
-- -   @transformOffset@ /must/ be a multiple of @16@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   If @vertexData@ is not
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE', @vertexData@
--     /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Buffer' handle
--
-- -   @vertexFormat@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.Format.Format' value
--
-- -   If @indexData@ is not
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE', @indexData@
--     /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Buffer' handle
--
-- -   'Graphics.Vulkan.Core10.Enums.IndexType.IndexType' /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.IndexType.IndexType' value
--
-- -   If @transformData@ is not
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE', @transformData@
--     /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Buffer' handle
--
-- -   Each of @indexData@, @transformData@, and @vertexData@ that are
--     valid handles of non-ignored parameters /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Buffer',
-- 'Graphics.Vulkan.Core10.BaseType.DeviceSize',
-- 'Graphics.Vulkan.Core10.Enums.Format.Format', 'GeometryDataNV',
-- 'Graphics.Vulkan.Core10.Enums.IndexType.IndexType',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data GeometryTrianglesNV = GeometryTrianglesNV
  { -- | @vertexData@ is the buffer containing vertex data for this geometry.
    vertexData :: Buffer
  , -- | @vertexOffset@ is the offset in bytes within @vertexData@ containing
    -- vertex data for this geometry.
    vertexOffset :: DeviceSize
  , -- | @vertexCount@ is the number of valid vertices.
    vertexCount :: Word32
  , -- | @vertexStride@ is the stride in bytes between each vertex.
    vertexStride :: DeviceSize
  , -- | @vertexFormat@ is the format of each vertex element.
    vertexFormat :: Format
  , -- | @indexData@ is the buffer containing index data for this geometry.
    indexData :: Buffer
  , -- | @indexOffset@ is the offset in bytes within @indexData@ containing index
    -- data for this geometry.
    indexOffset :: DeviceSize
  , -- | @indexCount@ is the number of indices to include in this geometry.
    indexCount :: Word32
  , -- | 'Graphics.Vulkan.Core10.Enums.IndexType.IndexType' is the format of each
    -- index.
    indexType :: IndexType
  , -- | @transformData@ is a buffer containing optional reference to an array of
    -- 32-bit floats representing a 3x4 row major affine transformation matrix
    -- for this geometry.
    transformData :: Buffer
  , -- | @transformOffset@ is the offset in bytes in @transformData@ of the
    -- transform information described above.
    transformOffset :: DeviceSize
  }
  deriving (Typeable)
deriving instance Show GeometryTrianglesNV

instance ToCStruct GeometryTrianglesNV where
  withCStruct x f = allocaBytesAligned 96 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GeometryTrianglesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Buffer)) (vertexData)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (vertexOffset)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (vertexCount)
    poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (vertexStride)
    poke ((p `plusPtr` 48 :: Ptr Format)) (vertexFormat)
    poke ((p `plusPtr` 56 :: Ptr Buffer)) (indexData)
    poke ((p `plusPtr` 64 :: Ptr DeviceSize)) (indexOffset)
    poke ((p `plusPtr` 72 :: Ptr Word32)) (indexCount)
    poke ((p `plusPtr` 76 :: Ptr IndexType)) (indexType)
    poke ((p `plusPtr` 80 :: Ptr Buffer)) (transformData)
    poke ((p `plusPtr` 88 :: Ptr DeviceSize)) (transformOffset)
    f
  cStructSize = 96
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Format)) (zero)
    poke ((p `plusPtr` 64 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 72 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 76 :: Ptr IndexType)) (zero)
    poke ((p `plusPtr` 88 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct GeometryTrianglesNV where
  peekCStruct p = do
    vertexData <- peek @Buffer ((p `plusPtr` 16 :: Ptr Buffer))
    vertexOffset <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    vertexCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    vertexStride <- peek @DeviceSize ((p `plusPtr` 40 :: Ptr DeviceSize))
    vertexFormat <- peek @Format ((p `plusPtr` 48 :: Ptr Format))
    indexData <- peek @Buffer ((p `plusPtr` 56 :: Ptr Buffer))
    indexOffset <- peek @DeviceSize ((p `plusPtr` 64 :: Ptr DeviceSize))
    indexCount <- peek @Word32 ((p `plusPtr` 72 :: Ptr Word32))
    indexType <- peek @IndexType ((p `plusPtr` 76 :: Ptr IndexType))
    transformData <- peek @Buffer ((p `plusPtr` 80 :: Ptr Buffer))
    transformOffset <- peek @DeviceSize ((p `plusPtr` 88 :: Ptr DeviceSize))
    pure $ GeometryTrianglesNV
             vertexData vertexOffset vertexCount vertexStride vertexFormat indexData indexOffset indexCount indexType transformData transformOffset

instance Storable GeometryTrianglesNV where
  sizeOf ~_ = 96
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero GeometryTrianglesNV where
  zero = GeometryTrianglesNV
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


-- | VkGeometryAABBNV - Structure specifying axis-aligned bounding box
-- geometry in a bottom-level acceleration structure
--
-- = Description
--
-- The AABB data in memory is six 32-bit floats consisting of the minimum
-- x, y, and z values followed by the maximum x, y, and z values.
--
-- == Valid Usage
--
-- -   @offset@ /must/ be less than the size of @aabbData@
--
-- -   @offset@ /must/ be a multiple of @8@
--
-- -   @stride@ /must/ be a multiple of @8@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GEOMETRY_AABB_NV'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   If @aabbData@ is not
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE', @aabbData@ /must/
--     be a valid 'Graphics.Vulkan.Core10.Handles.Buffer' handle
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Buffer',
-- 'Graphics.Vulkan.Core10.BaseType.DeviceSize', 'GeometryDataNV',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data GeometryAABBNV = GeometryAABBNV
  { -- | @aabbData@ is the buffer containing axis-aligned bounding box data.
    aabbData :: Buffer
  , -- | @numAABBs@ is the number of AABBs in this geometry.
    numAABBs :: Word32
  , -- | @stride@ is the stride in bytes between AABBs in @aabbData@.
    stride :: Word32
  , -- | @offset@ is the offset in bytes of the first AABB in @aabbData@.
    offset :: DeviceSize
  }
  deriving (Typeable)
deriving instance Show GeometryAABBNV

instance ToCStruct GeometryAABBNV where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GeometryAABBNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GEOMETRY_AABB_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Buffer)) (aabbData)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (numAABBs)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (stride)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (offset)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GEOMETRY_AABB_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct GeometryAABBNV where
  peekCStruct p = do
    aabbData <- peek @Buffer ((p `plusPtr` 16 :: Ptr Buffer))
    numAABBs <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    stride <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    offset <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    pure $ GeometryAABBNV
             aabbData numAABBs stride offset

instance Storable GeometryAABBNV where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero GeometryAABBNV where
  zero = GeometryAABBNV
           zero
           zero
           zero
           zero


-- | VkGeometryDataNV - Structure specifying geometry in a bottom-level
-- acceleration structure
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'GeometryAABBNV', 'GeometryNV', 'GeometryTrianglesNV'
data GeometryDataNV = GeometryDataNV
  { -- | @triangles@ /must/ be a valid 'GeometryTrianglesNV' structure
    triangles :: GeometryTrianglesNV
  , -- | @aabbs@ /must/ be a valid 'GeometryAABBNV' structure
    aabbs :: GeometryAABBNV
  }
  deriving (Typeable)
deriving instance Show GeometryDataNV

instance ToCStruct GeometryDataNV where
  withCStruct x f = allocaBytesAligned 136 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GeometryDataNV{..} f = evalContT $ do
    ContT $ pokeCStruct ((p `plusPtr` 0 :: Ptr GeometryTrianglesNV)) (triangles) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 96 :: Ptr GeometryAABBNV)) (aabbs) . ($ ())
    lift $ f
  cStructSize = 136
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    ContT $ pokeCStruct ((p `plusPtr` 0 :: Ptr GeometryTrianglesNV)) (zero) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 96 :: Ptr GeometryAABBNV)) (zero) . ($ ())
    lift $ f

instance FromCStruct GeometryDataNV where
  peekCStruct p = do
    triangles <- peekCStruct @GeometryTrianglesNV ((p `plusPtr` 0 :: Ptr GeometryTrianglesNV))
    aabbs <- peekCStruct @GeometryAABBNV ((p `plusPtr` 96 :: Ptr GeometryAABBNV))
    pure $ GeometryDataNV
             triangles aabbs

instance Zero GeometryDataNV where
  zero = GeometryDataNV
           zero
           zero


-- | VkGeometryNV - Structure specifying a geometry in a bottom-level
-- acceleration structure
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'AccelerationStructureInfoNV', 'GeometryDataNV', 'GeometryFlagsNV',
-- 'GeometryTypeNV',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data GeometryNV = GeometryNV
  { -- | @geometryType@ /must/ be a valid 'GeometryTypeNV' value
    geometryType :: GeometryTypeNV
  , -- | @geometry@ /must/ be a valid 'GeometryDataNV' structure
    geometry :: GeometryDataNV
  , -- | 'Graphics.Vulkan.Core10.BaseType.Flags' /must/ be a valid combination of
    -- 'GeometryFlagBitsNV' values
    flags :: GeometryFlagsNV
  }
  deriving (Typeable)
deriving instance Show GeometryNV

instance ToCStruct GeometryNV where
  withCStruct x f = allocaBytesAligned 168 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GeometryNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GEOMETRY_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr GeometryTypeNV)) (geometryType)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr GeometryDataNV)) (geometry) . ($ ())
    lift $ poke ((p `plusPtr` 160 :: Ptr GeometryFlagsNV)) (flags)
    lift $ f
  cStructSize = 168
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GEOMETRY_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr GeometryTypeNV)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr GeometryDataNV)) (zero) . ($ ())
    lift $ f

instance FromCStruct GeometryNV where
  peekCStruct p = do
    geometryType <- peek @GeometryTypeNV ((p `plusPtr` 16 :: Ptr GeometryTypeNV))
    geometry <- peekCStruct @GeometryDataNV ((p `plusPtr` 24 :: Ptr GeometryDataNV))
    flags <- peek @GeometryFlagsNV ((p `plusPtr` 160 :: Ptr GeometryFlagsNV))
    pure $ GeometryNV
             geometryType geometry flags

instance Zero GeometryNV where
  zero = GeometryNV
           zero
           zero
           zero


-- | VkAccelerationStructureInfoNV - Structure specifying the parameters of
-- acceleration structure object
--
-- = Description
--
-- 'AccelerationStructureInfoNV' contains information that is used both for
-- acceleration structure creation with 'createAccelerationStructureNV' and
-- in combination with the actual geometric data to build the acceleration
-- structure with 'cmdBuildAccelerationStructureNV'.
--
-- == Valid Usage
--
-- -   @geometryCount@ /must/ be less than or equal to
--     'PhysicalDeviceRayTracingPropertiesNV'::@maxGeometryCount@
--
-- -   @instanceCount@ /must/ be less than or equal to
--     'PhysicalDeviceRayTracingPropertiesNV'::@maxInstanceCount@
--
-- -   The total number of triangles in all geometries /must/ be less than
--     or equal to
--     'PhysicalDeviceRayTracingPropertiesNV'::@maxTriangleCount@
--
-- -   If @type@ is 'ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV' then
--     @geometryCount@ /must/ be @0@
--
-- -   If @type@ is 'ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV' then
--     @instanceCount@ /must/ be @0@
--
-- -   If @type@ is 'ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV' then the
--     @geometryType@ member of each geometry in @pGeometries@ /must/ be
--     the same
--
-- -   If 'Graphics.Vulkan.Core10.BaseType.Flags' has the
--     'BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV' bit set,
--     then it /must/ not have the
--     'BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV' bit set
--
-- -   @scratch@ /must/ have been created with
--     'Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_RAY_TRACING_BIT_NV'
--     usage flag
--
-- -   If @instanceData@ is not
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE', @instanceData@
--     /must/ have been created with
--     'Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_RAY_TRACING_BIT_NV'
--     usage flag
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @type@ /must/ be a valid 'AccelerationStructureTypeNV' value
--
-- -   'Graphics.Vulkan.Core10.BaseType.Flags' /must/ be a valid
--     combination of 'BuildAccelerationStructureFlagBitsNV' values
--
-- -   If @geometryCount@ is not @0@, @pGeometries@ /must/ be a valid
--     pointer to an array of @geometryCount@ valid 'GeometryNV' structures
--
-- = See Also
--
-- 'AccelerationStructureCreateInfoNV', 'AccelerationStructureTypeNV',
-- 'BuildAccelerationStructureFlagsNV', 'GeometryNV',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdBuildAccelerationStructureNV'
data AccelerationStructureInfoNV = AccelerationStructureInfoNV
  { -- | @type@ is a 'AccelerationStructureTypeNV' value specifying the type of
    -- acceleration structure that will be created.
    type' :: AccelerationStructureTypeNV
  , -- | 'Graphics.Vulkan.Core10.BaseType.Flags' is a bitmask of
    -- 'BuildAccelerationStructureFlagBitsNV' specifying additional parameters
    -- of the acceleration structure.
    flags :: BuildAccelerationStructureFlagsNV
  , -- | @instanceCount@ specifies the number of instances that will be in the
    -- new acceleration structure.
    instanceCount :: Word32
  , -- | @pGeometries@ is a pointer to an array of @geometryCount@ 'GeometryNV'
    -- structures containing the scene data being passed into the acceleration
    -- structure.
    geometries :: Vector GeometryNV
  }
  deriving (Typeable)
deriving instance Show AccelerationStructureInfoNV

instance ToCStruct AccelerationStructureInfoNV where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr AccelerationStructureTypeNV)) (type')
    lift $ poke ((p `plusPtr` 20 :: Ptr BuildAccelerationStructureFlagsNV)) (flags)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (instanceCount)
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (geometries)) :: Word32))
    pPGeometries' <- ContT $ allocaBytesAligned @GeometryNV ((Data.Vector.length (geometries)) * 168) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPGeometries' `plusPtr` (168 * (i)) :: Ptr GeometryNV) (e) . ($ ())) (geometries)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr GeometryNV))) (pPGeometries')
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr AccelerationStructureTypeNV)) (zero)
    pPGeometries' <- ContT $ allocaBytesAligned @GeometryNV ((Data.Vector.length (mempty)) * 168) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPGeometries' `plusPtr` (168 * (i)) :: Ptr GeometryNV) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr GeometryNV))) (pPGeometries')
    lift $ f

instance FromCStruct AccelerationStructureInfoNV where
  peekCStruct p = do
    type' <- peek @AccelerationStructureTypeNV ((p `plusPtr` 16 :: Ptr AccelerationStructureTypeNV))
    flags <- peek @BuildAccelerationStructureFlagsNV ((p `plusPtr` 20 :: Ptr BuildAccelerationStructureFlagsNV))
    instanceCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    geometryCount <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    pGeometries <- peek @(Ptr GeometryNV) ((p `plusPtr` 32 :: Ptr (Ptr GeometryNV)))
    pGeometries' <- generateM (fromIntegral geometryCount) (\i -> peekCStruct @GeometryNV ((pGeometries `advancePtrBytes` (168 * (i)) :: Ptr GeometryNV)))
    pure $ AccelerationStructureInfoNV
             type' flags instanceCount pGeometries'

instance Zero AccelerationStructureInfoNV where
  zero = AccelerationStructureInfoNV
           zero
           zero
           zero
           mempty


-- | VkAccelerationStructureCreateInfoNV - Structure specifying the
-- parameters of a newly created acceleration structure object
--
-- == Valid Usage
--
-- -   If @compactedSize@ is not @0@ then both @info.geometryCount@ and
--     @info.instanceCount@ /must/ be @0@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @info@ /must/ be a valid 'AccelerationStructureInfoNV' structure
--
-- = See Also
--
-- 'AccelerationStructureInfoNV',
-- 'Graphics.Vulkan.Core10.BaseType.DeviceSize',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createAccelerationStructureNV'
data AccelerationStructureCreateInfoNV = AccelerationStructureCreateInfoNV
  { -- | @compactedSize@ is the size from the result of
    -- 'cmdWriteAccelerationStructuresPropertiesNV' if this acceleration
    -- structure is going to be the target of a compacting copy.
    compactedSize :: DeviceSize
  , -- | @info@ is the 'AccelerationStructureInfoNV' structure specifying further
    -- parameters of the created acceleration structure.
    info :: AccelerationStructureInfoNV
  }
  deriving (Typeable)
deriving instance Show AccelerationStructureCreateInfoNV

instance ToCStruct AccelerationStructureCreateInfoNV where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureCreateInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (compactedSize)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr AccelerationStructureInfoNV)) (info) . ($ ())
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr AccelerationStructureInfoNV)) (zero) . ($ ())
    lift $ f

instance FromCStruct AccelerationStructureCreateInfoNV where
  peekCStruct p = do
    compactedSize <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    info <- peekCStruct @AccelerationStructureInfoNV ((p `plusPtr` 24 :: Ptr AccelerationStructureInfoNV))
    pure $ AccelerationStructureCreateInfoNV
             compactedSize info

instance Zero AccelerationStructureCreateInfoNV where
  zero = AccelerationStructureCreateInfoNV
           zero
           zero


-- | VkBindAccelerationStructureMemoryInfoNV - Structure specifying
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
--     'Graphics.Vulkan.Core10.MemoryManagement.MemoryRequirements'
--     structure returned from a call to
--     'getAccelerationStructureMemoryRequirementsNV' with
--     @accelerationStructure@ and @type@ of
--     'ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV'
--
-- -   @memoryOffset@ /must/ be an integer multiple of the @alignment@
--     member of the
--     'Graphics.Vulkan.Core10.MemoryManagement.MemoryRequirements'
--     structure returned from a call to
--     'getAccelerationStructureMemoryRequirementsNV' with
--     @accelerationStructure@ and @type@ of
--     'ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV'
--
-- -   The @size@ member of the
--     'Graphics.Vulkan.Core10.MemoryManagement.MemoryRequirements'
--     structure returned from a call to
--     'getAccelerationStructureMemoryRequirementsNV' with
--     @accelerationStructure@ and @type@ of
--     'ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV' /must/
--     be less than or equal to the size of @memory@ minus @memoryOffset@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @accelerationStructure@ /must/ be a valid
--     'Graphics.Vulkan.Extensions.Handles.AccelerationStructureNV' handle
--
-- -   @memory@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.DeviceMemory' handle
--
-- -   If @deviceIndexCount@ is not @0@, @pDeviceIndices@ /must/ be a valid
--     pointer to an array of @deviceIndexCount@ @uint32_t@ values
--
-- -   Both of @accelerationStructure@, and @memory@ /must/ have been
--     created, allocated, or retrieved from the same
--     'Graphics.Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.Handles.AccelerationStructureNV',
-- 'Graphics.Vulkan.Core10.Handles.DeviceMemory',
-- 'Graphics.Vulkan.Core10.BaseType.DeviceSize',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'bindAccelerationStructureMemoryNV'
data BindAccelerationStructureMemoryInfoNV = BindAccelerationStructureMemoryInfoNV
  { -- | @accelerationStructure@ is the acceleration structure to be attached to
    -- memory.
    accelerationStructure :: AccelerationStructureNV
  , -- | @memory@ is a 'Graphics.Vulkan.Core10.Handles.DeviceMemory' object
    -- describing the device memory to attach.
    memory :: DeviceMemory
  , -- | @memoryOffset@ is the start offset of the region of memory that is to be
    -- bound to the acceleration structure. The number of bytes returned in the
    -- 'Graphics.Vulkan.Core10.MemoryManagement.MemoryRequirements'::@size@
    -- member in @memory@, starting from @memoryOffset@ bytes, will be bound to
    -- the specified acceleration structure.
    memoryOffset :: DeviceSize
  , -- | @pDeviceIndices@ is a pointer to an array of device indices.
    deviceIndices :: Vector Word32
  }
  deriving (Typeable)
deriving instance Show BindAccelerationStructureMemoryInfoNV

instance ToCStruct BindAccelerationStructureMemoryInfoNV where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BindAccelerationStructureMemoryInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr AccelerationStructureNV)) (accelerationStructure)
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
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr AccelerationStructureNV)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr DeviceMemory)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    pPDeviceIndices' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDeviceIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (mempty)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr Word32))) (pPDeviceIndices')
    lift $ f

instance FromCStruct BindAccelerationStructureMemoryInfoNV where
  peekCStruct p = do
    accelerationStructure <- peek @AccelerationStructureNV ((p `plusPtr` 16 :: Ptr AccelerationStructureNV))
    memory <- peek @DeviceMemory ((p `plusPtr` 24 :: Ptr DeviceMemory))
    memoryOffset <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    deviceIndexCount <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    pDeviceIndices <- peek @(Ptr Word32) ((p `plusPtr` 48 :: Ptr (Ptr Word32)))
    pDeviceIndices' <- generateM (fromIntegral deviceIndexCount) (\i -> peek @Word32 ((pDeviceIndices `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pure $ BindAccelerationStructureMemoryInfoNV
             accelerationStructure memory memoryOffset pDeviceIndices'

instance Zero BindAccelerationStructureMemoryInfoNV where
  zero = BindAccelerationStructureMemoryInfoNV
           zero
           zero
           zero
           mempty


-- | VkWriteDescriptorSetAccelerationStructureNV - Structure specifying
-- acceleration structure descriptor info
--
-- == Valid Usage
--
-- -   @accelerationStructureCount@ /must/ be equal to @descriptorCount@ in
--     the extended structure
--
-- -   Each acceleration structure in @pAccelerationStructures@ must have
--     been created with 'ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV'
--
-- -   @pAccelerationStructures@ /must/ be a valid pointer to an array of
--     @accelerationStructureCount@ valid
--     'Graphics.Vulkan.Extensions.Handles.AccelerationStructureNV' handles
--
-- -   @accelerationStructureCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.Handles.AccelerationStructureNV',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data WriteDescriptorSetAccelerationStructureNV = WriteDescriptorSetAccelerationStructureNV
  { -- | @pAccelerationStructures@ are the acceleration structures to update.
    accelerationStructures :: Vector AccelerationStructureNV }
  deriving (Typeable)
deriving instance Show WriteDescriptorSetAccelerationStructureNV

instance ToCStruct WriteDescriptorSetAccelerationStructureNV where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p WriteDescriptorSetAccelerationStructureNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (accelerationStructures)) :: Word32))
    pPAccelerationStructures' <- ContT $ allocaBytesAligned @AccelerationStructureNV ((Data.Vector.length (accelerationStructures)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPAccelerationStructures' `plusPtr` (8 * (i)) :: Ptr AccelerationStructureNV) (e)) (accelerationStructures)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr AccelerationStructureNV))) (pPAccelerationStructures')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pPAccelerationStructures' <- ContT $ allocaBytesAligned @AccelerationStructureNV ((Data.Vector.length (mempty)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPAccelerationStructures' `plusPtr` (8 * (i)) :: Ptr AccelerationStructureNV) (e)) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr AccelerationStructureNV))) (pPAccelerationStructures')
    lift $ f

instance FromCStruct WriteDescriptorSetAccelerationStructureNV where
  peekCStruct p = do
    accelerationStructureCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pAccelerationStructures <- peek @(Ptr AccelerationStructureNV) ((p `plusPtr` 24 :: Ptr (Ptr AccelerationStructureNV)))
    pAccelerationStructures' <- generateM (fromIntegral accelerationStructureCount) (\i -> peek @AccelerationStructureNV ((pAccelerationStructures `advancePtrBytes` (8 * (i)) :: Ptr AccelerationStructureNV)))
    pure $ WriteDescriptorSetAccelerationStructureNV
             pAccelerationStructures'

instance Zero WriteDescriptorSetAccelerationStructureNV where
  zero = WriteDescriptorSetAccelerationStructureNV
           mempty


-- | VkAccelerationStructureMemoryRequirementsInfoNV - Structure specifying
-- acceleration to query for memory requirements
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'AccelerationStructureMemoryRequirementsTypeNV',
-- 'Graphics.Vulkan.Extensions.Handles.AccelerationStructureNV',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getAccelerationStructureMemoryRequirementsNV'
data AccelerationStructureMemoryRequirementsInfoNV = AccelerationStructureMemoryRequirementsInfoNV
  { -- | @type@ /must/ be a valid 'AccelerationStructureMemoryRequirementsTypeNV'
    -- value
    type' :: AccelerationStructureMemoryRequirementsTypeNV
  , -- | @accelerationStructure@ /must/ be a valid
    -- 'Graphics.Vulkan.Extensions.Handles.AccelerationStructureNV' handle
    accelerationStructure :: AccelerationStructureNV
  }
  deriving (Typeable)
deriving instance Show AccelerationStructureMemoryRequirementsInfoNV

instance ToCStruct AccelerationStructureMemoryRequirementsInfoNV where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureMemoryRequirementsInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AccelerationStructureMemoryRequirementsTypeNV)) (type')
    poke ((p `plusPtr` 24 :: Ptr AccelerationStructureNV)) (accelerationStructure)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AccelerationStructureMemoryRequirementsTypeNV)) (zero)
    poke ((p `plusPtr` 24 :: Ptr AccelerationStructureNV)) (zero)
    f

instance FromCStruct AccelerationStructureMemoryRequirementsInfoNV where
  peekCStruct p = do
    type' <- peek @AccelerationStructureMemoryRequirementsTypeNV ((p `plusPtr` 16 :: Ptr AccelerationStructureMemoryRequirementsTypeNV))
    accelerationStructure <- peek @AccelerationStructureNV ((p `plusPtr` 24 :: Ptr AccelerationStructureNV))
    pure $ AccelerationStructureMemoryRequirementsInfoNV
             type' accelerationStructure

instance Storable AccelerationStructureMemoryRequirementsInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AccelerationStructureMemoryRequirementsInfoNV where
  zero = AccelerationStructureMemoryRequirementsInfoNV
           zero
           zero


-- | VkPhysicalDeviceRayTracingPropertiesNV - Properties of the physical
-- device for ray tracing
--
-- = Description
--
-- If the 'PhysicalDeviceRayTracingPropertiesNV' structure is included in
-- the @pNext@ chain of
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceRayTracingPropertiesNV = PhysicalDeviceRayTracingPropertiesNV
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
  , -- | @maxTriangleCount@ is the maximum number of triangles in all geometries
    -- in the bottom level acceleration structure.
    maxTriangleCount :: Word64
  , -- | @maxDescriptorSetAccelerationStructures@ is the maximum number of
    -- acceleration structure descriptors that are allowed in a descriptor set.
    maxDescriptorSetAccelerationStructures :: Word32
  }
  deriving (Typeable)
deriving instance Show PhysicalDeviceRayTracingPropertiesNV

instance ToCStruct PhysicalDeviceRayTracingPropertiesNV where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceRayTracingPropertiesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (shaderGroupHandleSize)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxRecursionDepth)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (maxShaderGroupStride)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (shaderGroupBaseAlignment)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (maxGeometryCount)
    poke ((p `plusPtr` 40 :: Ptr Word64)) (maxInstanceCount)
    poke ((p `plusPtr` 48 :: Ptr Word64)) (maxTriangleCount)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (maxDescriptorSetAccelerationStructures)
    f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceRayTracingPropertiesNV where
  peekCStruct p = do
    shaderGroupHandleSize <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxRecursionDepth <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    maxShaderGroupStride <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    shaderGroupBaseAlignment <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    maxGeometryCount <- peek @Word64 ((p `plusPtr` 32 :: Ptr Word64))
    maxInstanceCount <- peek @Word64 ((p `plusPtr` 40 :: Ptr Word64))
    maxTriangleCount <- peek @Word64 ((p `plusPtr` 48 :: Ptr Word64))
    maxDescriptorSetAccelerationStructures <- peek @Word32 ((p `plusPtr` 56 :: Ptr Word32))
    pure $ PhysicalDeviceRayTracingPropertiesNV
             shaderGroupHandleSize maxRecursionDepth maxShaderGroupStride shaderGroupBaseAlignment maxGeometryCount maxInstanceCount maxTriangleCount maxDescriptorSetAccelerationStructures

instance Storable PhysicalDeviceRayTracingPropertiesNV where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceRayTracingPropertiesNV where
  zero = PhysicalDeviceRayTracingPropertiesNV
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkGeometryInstanceFlagBitsNV - Instance flag bits
--
-- = Description
--
-- 'GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV' and
-- 'GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV' /must/ not be used in the same
-- flag.
--
-- = See Also
--
-- 'GeometryInstanceFlagsNV'
newtype GeometryInstanceFlagBitsNV = GeometryInstanceFlagBitsNV Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV' disables face culling
-- for this instance.
pattern GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV = GeometryInstanceFlagBitsNV 0x00000001
-- | 'GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV' indicates
-- that the front face of the triangle for culling purposes is the face
-- that is counter clockwise in object space relative to the ray origin.
-- Because the facing is determined in object space, an instance transform
-- matrix does not change the winding, but a geometry transform does.
pattern GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV = GeometryInstanceFlagBitsNV 0x00000002
-- | 'GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV' causes this instance to act as
-- though 'GEOMETRY_OPAQUE_BIT_NV' were specified on all geometries
-- referenced by this instance. This behavior /can/ be overridden by the
-- ray flag @gl_RayFlagsNoOpaqueNV@.
pattern GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV = GeometryInstanceFlagBitsNV 0x00000004
-- | 'GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV' causes this instance to act
-- as though 'GEOMETRY_OPAQUE_BIT_NV' were not specified on all geometries
-- referenced by this instance. This behavior /can/ be overridden by the
-- ray flag @gl_RayFlagsOpaqueNV@.
pattern GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV = GeometryInstanceFlagBitsNV 0x00000008

type GeometryInstanceFlagsNV = GeometryInstanceFlagBitsNV

instance Show GeometryInstanceFlagBitsNV where
  showsPrec p = \case
    GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV -> showString "GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV"
    GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV -> showString "GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV"
    GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV -> showString "GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV"
    GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV -> showString "GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV"
    GeometryInstanceFlagBitsNV x -> showParen (p >= 11) (showString "GeometryInstanceFlagBitsNV 0x" . showHex x)

instance Read GeometryInstanceFlagBitsNV where
  readPrec = parens (choose [("GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV", pure GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV)
                            , ("GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV", pure GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV)
                            , ("GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV", pure GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV)
                            , ("GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV", pure GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV)]
                     +++
                     prec 10 (do
                       expectP (Ident "GeometryInstanceFlagBitsNV")
                       v <- step readPrec
                       pure (GeometryInstanceFlagBitsNV v)))


-- | VkGeometryFlagBitsNV - Bitmask specifying additional parameters for a
-- geometry
--
-- = See Also
--
-- 'GeometryFlagsNV'
newtype GeometryFlagBitsNV = GeometryFlagBitsNV Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'GEOMETRY_OPAQUE_BIT_NV' indicates that this geometry does not invoke
-- the any-hit shaders even if present in a hit group.
pattern GEOMETRY_OPAQUE_BIT_NV = GeometryFlagBitsNV 0x00000001
-- | 'GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV' indicates that the
-- implementation /must/ only call the any-hit shader a single time for
-- each primitive in this geometry. If this bit is absent an implementation
-- /may/ invoke the any-hit shader more than once for this geometry.
pattern GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV = GeometryFlagBitsNV 0x00000002

type GeometryFlagsNV = GeometryFlagBitsNV

instance Show GeometryFlagBitsNV where
  showsPrec p = \case
    GEOMETRY_OPAQUE_BIT_NV -> showString "GEOMETRY_OPAQUE_BIT_NV"
    GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV -> showString "GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV"
    GeometryFlagBitsNV x -> showParen (p >= 11) (showString "GeometryFlagBitsNV 0x" . showHex x)

instance Read GeometryFlagBitsNV where
  readPrec = parens (choose [("GEOMETRY_OPAQUE_BIT_NV", pure GEOMETRY_OPAQUE_BIT_NV)
                            , ("GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV", pure GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV)]
                     +++
                     prec 10 (do
                       expectP (Ident "GeometryFlagBitsNV")
                       v <- step readPrec
                       pure (GeometryFlagBitsNV v)))


-- | VkBuildAccelerationStructureFlagBitsNV - Bitmask specifying additional
-- parameters for acceleration structure builds
--
-- = Description
--
-- Note
--
-- 'BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV' and
-- 'BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV' /may/ take more
-- time and memory than a normal build, and so /should/ only be used when
-- those features are used.
--
-- = See Also
--
-- 'BuildAccelerationStructureFlagsNV'
newtype BuildAccelerationStructureFlagBitsNV = BuildAccelerationStructureFlagBitsNV Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV' indicates that the
-- specified acceleration structure /can/ be updated with @update@ of
-- 'Graphics.Vulkan.Core10.BaseType.TRUE' in
-- 'cmdBuildAccelerationStructureNV'.
pattern BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV = BuildAccelerationStructureFlagBitsNV 0x00000001
-- | 'BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV' indicates that
-- the specified acceleration structure /can/ act as the source for
-- 'cmdCopyAccelerationStructureNV' with @mode@ of
-- 'COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV' to produce a compacted
-- acceleration structure.
pattern BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV = BuildAccelerationStructureFlagBitsNV 0x00000002
-- | 'BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV' indicates that
-- the given acceleration structure build /should/ prioritize trace
-- performance over build time.
pattern BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV = BuildAccelerationStructureFlagBitsNV 0x00000004
-- | 'BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV' indicates that
-- the given acceleration structure build /should/ prioritize build time
-- over trace performance.
pattern BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV = BuildAccelerationStructureFlagBitsNV 0x00000008
-- | 'BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV' indicates that this
-- acceleration structure /should/ minimize the size of the scratch memory
-- and the final result build, potentially at the expense of build time or
-- trace performance.
pattern BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV = BuildAccelerationStructureFlagBitsNV 0x00000010

type BuildAccelerationStructureFlagsNV = BuildAccelerationStructureFlagBitsNV

instance Show BuildAccelerationStructureFlagBitsNV where
  showsPrec p = \case
    BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV -> showString "BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV"
    BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV -> showString "BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV"
    BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV -> showString "BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV"
    BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV -> showString "BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV"
    BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV -> showString "BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV"
    BuildAccelerationStructureFlagBitsNV x -> showParen (p >= 11) (showString "BuildAccelerationStructureFlagBitsNV 0x" . showHex x)

instance Read BuildAccelerationStructureFlagBitsNV where
  readPrec = parens (choose [("BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV", pure BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV)
                            , ("BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV", pure BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV)
                            , ("BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV", pure BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV)
                            , ("BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV", pure BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV)
                            , ("BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV", pure BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV)]
                     +++
                     prec 10 (do
                       expectP (Ident "BuildAccelerationStructureFlagBitsNV")
                       v <- step readPrec
                       pure (BuildAccelerationStructureFlagBitsNV v)))


-- | VkCopyAccelerationStructureModeNV - Acceleration structure copy mode
--
-- = See Also
--
-- 'cmdCopyAccelerationStructureNV'
newtype CopyAccelerationStructureModeNV = CopyAccelerationStructureModeNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV' creates a direct copy of the
-- acceleration structure specified in @src@ into the one specified by
-- @dst@. The @dst@ acceleration structure /must/ have been created with
-- the same parameters as @src@.
pattern COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV = CopyAccelerationStructureModeNV 0
-- | 'COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV' creates a more compact
-- version of an acceleration structure @src@ into @dst@. The acceleration
-- structure @dst@ /must/ have been created with a @compactedSize@
-- corresponding to the one returned by
-- 'cmdWriteAccelerationStructuresPropertiesNV' after the build of the
-- acceleration structure specified by @src@.
pattern COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV = CopyAccelerationStructureModeNV 1
{-# complete COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV,
             COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV :: CopyAccelerationStructureModeNV #-}

instance Show CopyAccelerationStructureModeNV where
  showsPrec p = \case
    COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV -> showString "COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV"
    COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV -> showString "COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV"
    CopyAccelerationStructureModeNV x -> showParen (p >= 11) (showString "CopyAccelerationStructureModeNV " . showsPrec 11 x)

instance Read CopyAccelerationStructureModeNV where
  readPrec = parens (choose [("COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV", pure COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV)
                            , ("COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV", pure COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV)]
                     +++
                     prec 10 (do
                       expectP (Ident "CopyAccelerationStructureModeNV")
                       v <- step readPrec
                       pure (CopyAccelerationStructureModeNV v)))


-- | VkAccelerationStructureTypeNV - Type of acceleration structure
--
-- = See Also
--
-- 'AccelerationStructureInfoNV'
newtype AccelerationStructureTypeNV = AccelerationStructureTypeNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV' is a top-level acceleration
-- structure containing instance data referring to bottom-level level
-- acceleration structures.
pattern ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV = AccelerationStructureTypeNV 0
-- | 'ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV' is a bottom-level
-- acceleration structure containing the AABBs or geometry to be
-- intersected.
pattern ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV = AccelerationStructureTypeNV 1
{-# complete ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV,
             ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV :: AccelerationStructureTypeNV #-}

instance Show AccelerationStructureTypeNV where
  showsPrec p = \case
    ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV -> showString "ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV"
    ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV -> showString "ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV"
    AccelerationStructureTypeNV x -> showParen (p >= 11) (showString "AccelerationStructureTypeNV " . showsPrec 11 x)

instance Read AccelerationStructureTypeNV where
  readPrec = parens (choose [("ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV", pure ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV)
                            , ("ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV", pure ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV)]
                     +++
                     prec 10 (do
                       expectP (Ident "AccelerationStructureTypeNV")
                       v <- step readPrec
                       pure (AccelerationStructureTypeNV v)))


-- | VkGeometryTypeNV - Enum specifying which type of geometry is provided
--
-- = See Also
--
-- 'GeometryNV'
newtype GeometryTypeNV = GeometryTypeNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'GEOMETRY_TYPE_TRIANGLES_NV' indicates that the @triangles@ of
-- 'GeometryDataNV' contains valid data.
pattern GEOMETRY_TYPE_TRIANGLES_NV = GeometryTypeNV 0
-- | 'GEOMETRY_TYPE_AABBS_NV' indicates that the @aabbs@ of 'GeometryDataNV'
-- contains valid data.
pattern GEOMETRY_TYPE_AABBS_NV = GeometryTypeNV 1
{-# complete GEOMETRY_TYPE_TRIANGLES_NV,
             GEOMETRY_TYPE_AABBS_NV :: GeometryTypeNV #-}

instance Show GeometryTypeNV where
  showsPrec p = \case
    GEOMETRY_TYPE_TRIANGLES_NV -> showString "GEOMETRY_TYPE_TRIANGLES_NV"
    GEOMETRY_TYPE_AABBS_NV -> showString "GEOMETRY_TYPE_AABBS_NV"
    GeometryTypeNV x -> showParen (p >= 11) (showString "GeometryTypeNV " . showsPrec 11 x)

instance Read GeometryTypeNV where
  readPrec = parens (choose [("GEOMETRY_TYPE_TRIANGLES_NV", pure GEOMETRY_TYPE_TRIANGLES_NV)
                            , ("GEOMETRY_TYPE_AABBS_NV", pure GEOMETRY_TYPE_AABBS_NV)]
                     +++
                     prec 10 (do
                       expectP (Ident "GeometryTypeNV")
                       v <- step readPrec
                       pure (GeometryTypeNV v)))


-- | VkAccelerationStructureMemoryRequirementsTypeNV - Acceleration structure
-- memory requirement type
--
-- = See Also
--
-- 'AccelerationStructureMemoryRequirementsInfoNV'
newtype AccelerationStructureMemoryRequirementsTypeNV = AccelerationStructureMemoryRequirementsTypeNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV' requests the
-- memory requirement for the
-- 'Graphics.Vulkan.Extensions.Handles.AccelerationStructureNV' backing
-- store.
pattern ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV = AccelerationStructureMemoryRequirementsTypeNV 0
-- | 'ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV'
-- requests the memory requirement for scratch space during the initial
-- build.
pattern ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV = AccelerationStructureMemoryRequirementsTypeNV 1
-- | 'ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV'
-- requests the memory requirement for scratch space during an update.
pattern ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV = AccelerationStructureMemoryRequirementsTypeNV 2
{-# complete ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV,
             ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV,
             ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV :: AccelerationStructureMemoryRequirementsTypeNV #-}

instance Show AccelerationStructureMemoryRequirementsTypeNV where
  showsPrec p = \case
    ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV -> showString "ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV"
    ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV -> showString "ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV"
    ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV -> showString "ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV"
    AccelerationStructureMemoryRequirementsTypeNV x -> showParen (p >= 11) (showString "AccelerationStructureMemoryRequirementsTypeNV " . showsPrec 11 x)

instance Read AccelerationStructureMemoryRequirementsTypeNV where
  readPrec = parens (choose [("ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV", pure ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV)
                            , ("ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV", pure ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV)
                            , ("ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV", pure ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV)]
                     +++
                     prec 10 (do
                       expectP (Ident "AccelerationStructureMemoryRequirementsTypeNV")
                       v <- step readPrec
                       pure (AccelerationStructureMemoryRequirementsTypeNV v)))


-- | VkRayTracingShaderGroupTypeNV - Shader group types
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
-- 'RayTracingShaderGroupCreateInfoNV'
newtype RayTracingShaderGroupTypeNV = RayTracingShaderGroupTypeNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV' indicates a shader group with
-- a single
-- 'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_RAYGEN_BIT_NV',
-- 'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MISS_BIT_NV',
-- or
-- 'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_CALLABLE_BIT_NV'
-- shader in it.
pattern RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV = RayTracingShaderGroupTypeNV 0
-- | 'RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV' specifies a
-- shader group that only hits triangles and /must/ not contain an
-- intersection shader, only closest hit and any-hit.
pattern RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV = RayTracingShaderGroupTypeNV 1
-- | 'RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV' specifies a
-- shader group that only intersects with custom geometry and /must/
-- contain an intersection shader and /may/ contain closest hit and any-hit
-- shaders.
pattern RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV = RayTracingShaderGroupTypeNV 2
{-# complete RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV,
             RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV,
             RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV :: RayTracingShaderGroupTypeNV #-}

instance Show RayTracingShaderGroupTypeNV where
  showsPrec p = \case
    RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV -> showString "RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV"
    RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV -> showString "RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV"
    RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV -> showString "RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV"
    RayTracingShaderGroupTypeNV x -> showParen (p >= 11) (showString "RayTracingShaderGroupTypeNV " . showsPrec 11 x)

instance Read RayTracingShaderGroupTypeNV where
  readPrec = parens (choose [("RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV", pure RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV)
                            , ("RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV", pure RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV)
                            , ("RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV", pure RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV)]
                     +++
                     prec 10 (do
                       expectP (Ident "RayTracingShaderGroupTypeNV")
                       v <- step readPrec
                       pure (RayTracingShaderGroupTypeNV v)))


type NV_RAY_TRACING_SPEC_VERSION = 3

-- No documentation found for TopLevel "VK_NV_RAY_TRACING_SPEC_VERSION"
pattern NV_RAY_TRACING_SPEC_VERSION :: forall a . Integral a => a
pattern NV_RAY_TRACING_SPEC_VERSION = 3


type NV_RAY_TRACING_EXTENSION_NAME = "VK_NV_ray_tracing"

-- No documentation found for TopLevel "VK_NV_RAY_TRACING_EXTENSION_NAME"
pattern NV_RAY_TRACING_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_RAY_TRACING_EXTENSION_NAME = "VK_NV_ray_tracing"

