{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_ray_tracing_pipeline"
module Vulkan.Extensions.VK_KHR_ray_tracing_pipeline  ( cmdTraceRaysKHR
                                                      , getRayTracingShaderGroupHandlesKHR
                                                      , getRayTracingCaptureReplayShaderGroupHandlesKHR
                                                      , createRayTracingPipelinesKHR
                                                      , withRayTracingPipelinesKHR
                                                      , cmdTraceRaysIndirectKHR
                                                      , getRayTracingShaderGroupStackSizeKHR
                                                      , cmdSetRayTracingPipelineStackSizeKHR
                                                      , RayTracingShaderGroupCreateInfoKHR(..)
                                                      , RayTracingPipelineCreateInfoKHR(..)
                                                      , PhysicalDeviceRayTracingPipelineFeaturesKHR(..)
                                                      , PhysicalDeviceRayTracingPipelinePropertiesKHR(..)
                                                      , StridedDeviceAddressRegionKHR(..)
                                                      , TraceRaysIndirectCommandKHR(..)
                                                      , RayTracingPipelineInterfaceCreateInfoKHR(..)
                                                      , RayTracingShaderGroupTypeKHR( RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR
                                                                                    , RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR
                                                                                    , RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR
                                                                                    , ..
                                                                                    )
                                                      , ShaderGroupShaderKHR( SHADER_GROUP_SHADER_GENERAL_KHR
                                                                            , SHADER_GROUP_SHADER_CLOSEST_HIT_KHR
                                                                            , SHADER_GROUP_SHADER_ANY_HIT_KHR
                                                                            , SHADER_GROUP_SHADER_INTERSECTION_KHR
                                                                            , ..
                                                                            )
                                                      , KHR_RAY_TRACING_PIPELINE_SPEC_VERSION
                                                      , pattern KHR_RAY_TRACING_PIPELINE_SPEC_VERSION
                                                      , KHR_RAY_TRACING_PIPELINE_EXTENSION_NAME
                                                      , pattern KHR_RAY_TRACING_PIPELINE_EXTENSION_NAME
                                                      , DeferredOperationKHR(..)
                                                      , PipelineLibraryCreateInfoKHR(..)
                                                      , SHADER_UNUSED_KHR
                                                      , pattern SHADER_UNUSED_KHR
                                                      ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Utils (maybePeek)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Foreign.C.Types (CSize(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.C.Types (CSize)
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
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.Pipeline (destroyPipeline)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Extends (peekSomeCStruct)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Extensions.Handles (DeferredOperationKHR)
import Vulkan.Extensions.Handles (DeferredOperationKHR(..))
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.FundamentalTypes (DeviceAddress)
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetRayTracingPipelineStackSizeKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdTraceRaysIndirectKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdTraceRaysKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCreateRayTracingPipelinesKHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetRayTracingCaptureReplayShaderGroupHandlesKHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetRayTracingShaderGroupHandlesKHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetRayTracingShaderGroupStackSizeKHR))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.Core10.Handles (Pipeline)
import Vulkan.Core10.Handles (Pipeline(..))
import Vulkan.Core10.Handles (PipelineCache)
import Vulkan.Core10.Handles (PipelineCache(..))
import Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_pipeline_creation_feedback (PipelineCreationFeedbackCreateInfoEXT)
import Vulkan.Core10.Pipeline (PipelineDynamicStateCreateInfo)
import Vulkan.Core10.Handles (PipelineLayout)
import Vulkan.Extensions.VK_KHR_pipeline_library (PipelineLibraryCreateInfoKHR)
import Vulkan.Core10.Pipeline (PipelineShaderStageCreateInfo)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PIPELINE_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PIPELINE_PROPERTIES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RAY_TRACING_PIPELINE_INTERFACE_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (DeferredOperationKHR(..))
import Vulkan.Extensions.VK_KHR_pipeline_library (PipelineLibraryCreateInfoKHR(..))
import Vulkan.Core10.APIConstants (SHADER_UNUSED_KHR)
import Vulkan.Core10.APIConstants (pattern SHADER_UNUSED_KHR)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdTraceRaysKHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr StridedDeviceAddressRegionKHR -> Ptr StridedDeviceAddressRegionKHR -> Ptr StridedDeviceAddressRegionKHR -> Ptr StridedDeviceAddressRegionKHR -> Word32 -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Ptr StridedDeviceAddressRegionKHR -> Ptr StridedDeviceAddressRegionKHR -> Ptr StridedDeviceAddressRegionKHR -> Ptr StridedDeviceAddressRegionKHR -> Word32 -> Word32 -> Word32 -> IO ()

-- No documentation found for TopLevel "vkCmdTraceRaysKHR"
cmdTraceRaysKHR :: forall io
                 . (MonadIO io)
                => -- No documentation found for Nested "vkCmdTraceRaysKHR" "commandBuffer"
                   CommandBuffer
                -> -- No documentation found for Nested "vkCmdTraceRaysKHR" "pRaygenShaderBindingTable"
                   ("raygenShaderBindingTable" ::: StridedDeviceAddressRegionKHR)
                -> -- No documentation found for Nested "vkCmdTraceRaysKHR" "pMissShaderBindingTable"
                   ("missShaderBindingTable" ::: StridedDeviceAddressRegionKHR)
                -> -- No documentation found for Nested "vkCmdTraceRaysKHR" "pHitShaderBindingTable"
                   ("hitShaderBindingTable" ::: StridedDeviceAddressRegionKHR)
                -> -- No documentation found for Nested "vkCmdTraceRaysKHR" "pCallableShaderBindingTable"
                   ("callableShaderBindingTable" ::: StridedDeviceAddressRegionKHR)
                -> -- No documentation found for Nested "vkCmdTraceRaysKHR" "width"
                   ("width" ::: Word32)
                -> -- No documentation found for Nested "vkCmdTraceRaysKHR" "height"
                   ("height" ::: Word32)
                -> -- No documentation found for Nested "vkCmdTraceRaysKHR" "depth"
                   ("depth" ::: Word32)
                -> io ()
cmdTraceRaysKHR commandBuffer raygenShaderBindingTable missShaderBindingTable hitShaderBindingTable callableShaderBindingTable width height depth = liftIO . evalContT $ do
  let vkCmdTraceRaysKHRPtr = pVkCmdTraceRaysKHR (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdTraceRaysKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdTraceRaysKHR is null" Nothing Nothing
  let vkCmdTraceRaysKHR' = mkVkCmdTraceRaysKHR vkCmdTraceRaysKHRPtr
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

-- No documentation found for TopLevel "vkGetRayTracingShaderGroupHandlesKHR"
getRayTracingShaderGroupHandlesKHR :: forall io
                                    . (MonadIO io)
                                   => -- No documentation found for Nested "vkGetRayTracingShaderGroupHandlesKHR" "device"
                                      Device
                                   -> -- No documentation found for Nested "vkGetRayTracingShaderGroupHandlesKHR" "pipeline"
                                      Pipeline
                                   -> -- No documentation found for Nested "vkGetRayTracingShaderGroupHandlesKHR" "firstGroup"
                                      ("firstGroup" ::: Word32)
                                   -> -- No documentation found for Nested "vkGetRayTracingShaderGroupHandlesKHR" "groupCount"
                                      ("groupCount" ::: Word32)
                                   -> -- No documentation found for Nested "vkGetRayTracingShaderGroupHandlesKHR" "dataSize"
                                      ("dataSize" ::: Word64)
                                   -> -- No documentation found for Nested "vkGetRayTracingShaderGroupHandlesKHR" "pData"
                                      ("data" ::: Ptr ())
                                   -> io ()
getRayTracingShaderGroupHandlesKHR device pipeline firstGroup groupCount dataSize data' = liftIO $ do
  let vkGetRayTracingShaderGroupHandlesKHRPtr = pVkGetRayTracingShaderGroupHandlesKHR (deviceCmds (device :: Device))
  unless (vkGetRayTracingShaderGroupHandlesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetRayTracingShaderGroupHandlesKHR is null" Nothing Nothing
  let vkGetRayTracingShaderGroupHandlesKHR' = mkVkGetRayTracingShaderGroupHandlesKHR vkGetRayTracingShaderGroupHandlesKHRPtr
  r <- vkGetRayTracingShaderGroupHandlesKHR' (deviceHandle (device)) (pipeline) (firstGroup) (groupCount) (CSize (dataSize)) (data')
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetRayTracingCaptureReplayShaderGroupHandlesKHR
  :: FunPtr (Ptr Device_T -> Pipeline -> Word32 -> Word32 -> CSize -> Ptr () -> IO Result) -> Ptr Device_T -> Pipeline -> Word32 -> Word32 -> CSize -> Ptr () -> IO Result

-- No documentation found for TopLevel "vkGetRayTracingCaptureReplayShaderGroupHandlesKHR"
getRayTracingCaptureReplayShaderGroupHandlesKHR :: forall io
                                                 . (MonadIO io)
                                                => -- No documentation found for Nested "vkGetRayTracingCaptureReplayShaderGroupHandlesKHR" "device"
                                                   Device
                                                -> -- No documentation found for Nested "vkGetRayTracingCaptureReplayShaderGroupHandlesKHR" "pipeline"
                                                   Pipeline
                                                -> -- No documentation found for Nested "vkGetRayTracingCaptureReplayShaderGroupHandlesKHR" "firstGroup"
                                                   ("firstGroup" ::: Word32)
                                                -> -- No documentation found for Nested "vkGetRayTracingCaptureReplayShaderGroupHandlesKHR" "groupCount"
                                                   ("groupCount" ::: Word32)
                                                -> -- No documentation found for Nested "vkGetRayTracingCaptureReplayShaderGroupHandlesKHR" "dataSize"
                                                   ("dataSize" ::: Word64)
                                                -> -- No documentation found for Nested "vkGetRayTracingCaptureReplayShaderGroupHandlesKHR" "pData"
                                                   ("data" ::: Ptr ())
                                                -> io ()
getRayTracingCaptureReplayShaderGroupHandlesKHR device pipeline firstGroup groupCount dataSize data' = liftIO $ do
  let vkGetRayTracingCaptureReplayShaderGroupHandlesKHRPtr = pVkGetRayTracingCaptureReplayShaderGroupHandlesKHR (deviceCmds (device :: Device))
  unless (vkGetRayTracingCaptureReplayShaderGroupHandlesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetRayTracingCaptureReplayShaderGroupHandlesKHR is null" Nothing Nothing
  let vkGetRayTracingCaptureReplayShaderGroupHandlesKHR' = mkVkGetRayTracingCaptureReplayShaderGroupHandlesKHR vkGetRayTracingCaptureReplayShaderGroupHandlesKHRPtr
  r <- vkGetRayTracingCaptureReplayShaderGroupHandlesKHR' (deviceHandle (device)) (pipeline) (firstGroup) (groupCount) (CSize (dataSize)) (data')
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateRayTracingPipelinesKHR
  :: FunPtr (Ptr Device_T -> DeferredOperationKHR -> PipelineCache -> Word32 -> Ptr (SomeStruct RayTracingPipelineCreateInfoKHR) -> Ptr AllocationCallbacks -> Ptr Pipeline -> IO Result) -> Ptr Device_T -> DeferredOperationKHR -> PipelineCache -> Word32 -> Ptr (SomeStruct RayTracingPipelineCreateInfoKHR) -> Ptr AllocationCallbacks -> Ptr Pipeline -> IO Result

-- No documentation found for TopLevel "vkCreateRayTracingPipelinesKHR"
createRayTracingPipelinesKHR :: forall io
                              . (MonadIO io)
                             => -- No documentation found for Nested "vkCreateRayTracingPipelinesKHR" "device"
                                Device
                             -> -- No documentation found for Nested "vkCreateRayTracingPipelinesKHR" "deferredOperation"
                                DeferredOperationKHR
                             -> -- No documentation found for Nested "vkCreateRayTracingPipelinesKHR" "pipelineCache"
                                PipelineCache
                             -> -- No documentation found for Nested "vkCreateRayTracingPipelinesKHR" "pCreateInfos"
                                ("createInfos" ::: Vector (SomeStruct RayTracingPipelineCreateInfoKHR))
                             -> -- No documentation found for Nested "vkCreateRayTracingPipelinesKHR" "pAllocator"
                                ("allocator" ::: Maybe AllocationCallbacks)
                             -> io (Result, ("pipelines" ::: Vector Pipeline))
createRayTracingPipelinesKHR device deferredOperation pipelineCache createInfos allocator = liftIO . evalContT $ do
  let vkCreateRayTracingPipelinesKHRPtr = pVkCreateRayTracingPipelinesKHR (deviceCmds (device :: Device))
  lift $ unless (vkCreateRayTracingPipelinesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateRayTracingPipelinesKHR is null" Nothing Nothing
  let vkCreateRayTracingPipelinesKHR' = mkVkCreateRayTracingPipelinesKHR vkCreateRayTracingPipelinesKHRPtr
  pPCreateInfos <- ContT $ allocaBytesAligned @(RayTracingPipelineCreateInfoKHR _) ((Data.Vector.length (createInfos)) * 104) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPCreateInfos `plusPtr` (104 * (i)) :: Ptr (RayTracingPipelineCreateInfoKHR _))) (e) . ($ ())) (createInfos)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPPipelines <- ContT $ bracket (callocBytes @Pipeline ((fromIntegral ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32))) * 8)) free
  r <- lift $ vkCreateRayTracingPipelinesKHR' (deviceHandle (device)) (deferredOperation) (pipelineCache) ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32)) (forgetExtensions (pPCreateInfos)) pAllocator (pPPipelines)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPipelines <- lift $ generateM (fromIntegral ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32))) (\i -> peek @Pipeline ((pPPipelines `advancePtrBytes` (8 * (i)) :: Ptr Pipeline)))
  pure $ (r, pPipelines)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createRayTracingPipelinesKHR' and 'destroyPipeline'
--
-- To ensure that 'destroyPipeline' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withRayTracingPipelinesKHR :: forall io r . MonadIO io => Device -> DeferredOperationKHR -> PipelineCache -> Vector (SomeStruct RayTracingPipelineCreateInfoKHR) -> Maybe AllocationCallbacks -> (io (Result, Vector Pipeline) -> ((Result, Vector Pipeline) -> io ()) -> r) -> r
withRayTracingPipelinesKHR device deferredOperation pipelineCache pCreateInfos pAllocator b =
  b (createRayTracingPipelinesKHR device deferredOperation pipelineCache pCreateInfos pAllocator)
    (\(_, o1) -> traverse_ (\o1Elem -> destroyPipeline device o1Elem pAllocator) o1)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdTraceRaysIndirectKHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr StridedDeviceAddressRegionKHR -> Ptr StridedDeviceAddressRegionKHR -> Ptr StridedDeviceAddressRegionKHR -> Ptr StridedDeviceAddressRegionKHR -> DeviceAddress -> IO ()) -> Ptr CommandBuffer_T -> Ptr StridedDeviceAddressRegionKHR -> Ptr StridedDeviceAddressRegionKHR -> Ptr StridedDeviceAddressRegionKHR -> Ptr StridedDeviceAddressRegionKHR -> DeviceAddress -> IO ()

-- No documentation found for TopLevel "vkCmdTraceRaysIndirectKHR"
cmdTraceRaysIndirectKHR :: forall io
                         . (MonadIO io)
                        => -- No documentation found for Nested "vkCmdTraceRaysIndirectKHR" "commandBuffer"
                           CommandBuffer
                        -> -- No documentation found for Nested "vkCmdTraceRaysIndirectKHR" "pRaygenShaderBindingTable"
                           ("raygenShaderBindingTable" ::: StridedDeviceAddressRegionKHR)
                        -> -- No documentation found for Nested "vkCmdTraceRaysIndirectKHR" "pMissShaderBindingTable"
                           ("missShaderBindingTable" ::: StridedDeviceAddressRegionKHR)
                        -> -- No documentation found for Nested "vkCmdTraceRaysIndirectKHR" "pHitShaderBindingTable"
                           ("hitShaderBindingTable" ::: StridedDeviceAddressRegionKHR)
                        -> -- No documentation found for Nested "vkCmdTraceRaysIndirectKHR" "pCallableShaderBindingTable"
                           ("callableShaderBindingTable" ::: StridedDeviceAddressRegionKHR)
                        -> -- No documentation found for Nested "vkCmdTraceRaysIndirectKHR" "indirectDeviceAddress"
                           ("indirectDeviceAddress" ::: DeviceAddress)
                        -> io ()
cmdTraceRaysIndirectKHR commandBuffer raygenShaderBindingTable missShaderBindingTable hitShaderBindingTable callableShaderBindingTable indirectDeviceAddress = liftIO . evalContT $ do
  let vkCmdTraceRaysIndirectKHRPtr = pVkCmdTraceRaysIndirectKHR (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdTraceRaysIndirectKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdTraceRaysIndirectKHR is null" Nothing Nothing
  let vkCmdTraceRaysIndirectKHR' = mkVkCmdTraceRaysIndirectKHR vkCmdTraceRaysIndirectKHRPtr
  pRaygenShaderBindingTable <- ContT $ withCStruct (raygenShaderBindingTable)
  pMissShaderBindingTable <- ContT $ withCStruct (missShaderBindingTable)
  pHitShaderBindingTable <- ContT $ withCStruct (hitShaderBindingTable)
  pCallableShaderBindingTable <- ContT $ withCStruct (callableShaderBindingTable)
  lift $ vkCmdTraceRaysIndirectKHR' (commandBufferHandle (commandBuffer)) pRaygenShaderBindingTable pMissShaderBindingTable pHitShaderBindingTable pCallableShaderBindingTable (indirectDeviceAddress)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetRayTracingShaderGroupStackSizeKHR
  :: FunPtr (Ptr Device_T -> Pipeline -> Word32 -> ShaderGroupShaderKHR -> IO DeviceSize) -> Ptr Device_T -> Pipeline -> Word32 -> ShaderGroupShaderKHR -> IO DeviceSize

-- No documentation found for TopLevel "vkGetRayTracingShaderGroupStackSizeKHR"
getRayTracingShaderGroupStackSizeKHR :: forall io
                                      . (MonadIO io)
                                     => -- No documentation found for Nested "vkGetRayTracingShaderGroupStackSizeKHR" "device"
                                        Device
                                     -> -- No documentation found for Nested "vkGetRayTracingShaderGroupStackSizeKHR" "pipeline"
                                        Pipeline
                                     -> -- No documentation found for Nested "vkGetRayTracingShaderGroupStackSizeKHR" "group"
                                        ("group" ::: Word32)
                                     -> -- No documentation found for Nested "vkGetRayTracingShaderGroupStackSizeKHR" "groupShader"
                                        ShaderGroupShaderKHR
                                     -> io (DeviceSize)
getRayTracingShaderGroupStackSizeKHR device pipeline group groupShader = liftIO $ do
  let vkGetRayTracingShaderGroupStackSizeKHRPtr = pVkGetRayTracingShaderGroupStackSizeKHR (deviceCmds (device :: Device))
  unless (vkGetRayTracingShaderGroupStackSizeKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetRayTracingShaderGroupStackSizeKHR is null" Nothing Nothing
  let vkGetRayTracingShaderGroupStackSizeKHR' = mkVkGetRayTracingShaderGroupStackSizeKHR vkGetRayTracingShaderGroupStackSizeKHRPtr
  r <- vkGetRayTracingShaderGroupStackSizeKHR' (deviceHandle (device)) (pipeline) (group) (groupShader)
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetRayTracingPipelineStackSizeKHR
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> IO ()

-- No documentation found for TopLevel "vkCmdSetRayTracingPipelineStackSizeKHR"
cmdSetRayTracingPipelineStackSizeKHR :: forall io
                                      . (MonadIO io)
                                     => -- No documentation found for Nested "vkCmdSetRayTracingPipelineStackSizeKHR" "commandBuffer"
                                        CommandBuffer
                                     -> -- No documentation found for Nested "vkCmdSetRayTracingPipelineStackSizeKHR" "pipelineStackSize"
                                        ("pipelineStackSize" ::: Word32)
                                     -> io ()
cmdSetRayTracingPipelineStackSizeKHR commandBuffer pipelineStackSize = liftIO $ do
  let vkCmdSetRayTracingPipelineStackSizeKHRPtr = pVkCmdSetRayTracingPipelineStackSizeKHR (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetRayTracingPipelineStackSizeKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetRayTracingPipelineStackSizeKHR is null" Nothing Nothing
  let vkCmdSetRayTracingPipelineStackSizeKHR' = mkVkCmdSetRayTracingPipelineStackSizeKHR vkCmdSetRayTracingPipelineStackSizeKHRPtr
  vkCmdSetRayTracingPipelineStackSizeKHR' (commandBufferHandle (commandBuffer)) (pipelineStackSize)
  pure $ ()



-- No documentation found for TopLevel "VkRayTracingShaderGroupCreateInfoKHR"
data RayTracingShaderGroupCreateInfoKHR = RayTracingShaderGroupCreateInfoKHR
  { -- No documentation found for Nested "VkRayTracingShaderGroupCreateInfoKHR" "type"
    type' :: RayTracingShaderGroupTypeKHR
  , -- No documentation found for Nested "VkRayTracingShaderGroupCreateInfoKHR" "generalShader"
    generalShader :: Word32
  , -- No documentation found for Nested "VkRayTracingShaderGroupCreateInfoKHR" "closestHitShader"
    closestHitShader :: Word32
  , -- No documentation found for Nested "VkRayTracingShaderGroupCreateInfoKHR" "anyHitShader"
    anyHitShader :: Word32
  , -- No documentation found for Nested "VkRayTracingShaderGroupCreateInfoKHR" "intersectionShader"
    intersectionShader :: Word32
  , -- No documentation found for Nested "VkRayTracingShaderGroupCreateInfoKHR" "pShaderGroupCaptureReplayHandle"
    shaderGroupCaptureReplayHandle :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RayTracingShaderGroupCreateInfoKHR)
#endif
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



-- No documentation found for TopLevel "VkRayTracingPipelineCreateInfoKHR"
data RayTracingPipelineCreateInfoKHR (es :: [Type]) = RayTracingPipelineCreateInfoKHR
  { -- No documentation found for Nested "VkRayTracingPipelineCreateInfoKHR" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkRayTracingPipelineCreateInfoKHR" "flags"
    flags :: PipelineCreateFlags
  , -- No documentation found for Nested "VkRayTracingPipelineCreateInfoKHR" "pStages"
    stages :: Vector (SomeStruct PipelineShaderStageCreateInfo)
  , -- No documentation found for Nested "VkRayTracingPipelineCreateInfoKHR" "pGroups"
    groups :: Vector RayTracingShaderGroupCreateInfoKHR
  , -- No documentation found for Nested "VkRayTracingPipelineCreateInfoKHR" "maxPipelineRayRecursionDepth"
    maxPipelineRayRecursionDepth :: Word32
  , -- No documentation found for Nested "VkRayTracingPipelineCreateInfoKHR" "pLibraryInfo"
    libraryInfo :: Maybe PipelineLibraryCreateInfoKHR
  , -- No documentation found for Nested "VkRayTracingPipelineCreateInfoKHR" "pLibraryInterface"
    libraryInterface :: Maybe RayTracingPipelineInterfaceCreateInfoKHR
  , -- No documentation found for Nested "VkRayTracingPipelineCreateInfoKHR" "pDynamicState"
    dynamicState :: Maybe PipelineDynamicStateCreateInfo
  , -- No documentation found for Nested "VkRayTracingPipelineCreateInfoKHR" "layout"
    layout :: PipelineLayout
  , -- No documentation found for Nested "VkRayTracingPipelineCreateInfoKHR" "basePipelineHandle"
    basePipelineHandle :: Pipeline
  , -- No documentation found for Nested "VkRayTracingPipelineCreateInfoKHR" "basePipelineIndex"
    basePipelineIndex :: Int32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RayTracingPipelineCreateInfoKHR (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (RayTracingPipelineCreateInfoKHR es)

instance Extensible RayTracingPipelineCreateInfoKHR where
  extensibleType = STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_KHR
  setNext x next = x{next = next}
  getNext RayTracingPipelineCreateInfoKHR{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends RayTracingPipelineCreateInfoKHR e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PipelineCreationFeedbackCreateInfoEXT = Just f
    | otherwise = Nothing

instance (Extendss RayTracingPipelineCreateInfoKHR es, PokeChain es) => ToCStruct (RayTracingPipelineCreateInfoKHR es) where
  withCStruct x f = allocaBytesAligned 104 8 $ \p -> pokeCStruct p x (f p)
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
    lift $ Data.Vector.imapM_ (\i e -> poke (pPGroups' `plusPtr` (48 * (i)) :: Ptr RayTracingShaderGroupCreateInfoKHR) (e)) (groups)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr RayTracingShaderGroupCreateInfoKHR))) (pPGroups')
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) (maxPipelineRayRecursionDepth)
    pLibraryInfo'' <- case (libraryInfo) of
      Nothing -> pure nullPtr
      Just j -> ContT $ withCStruct (j)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr PipelineLibraryCreateInfoKHR))) pLibraryInfo''
    pLibraryInterface'' <- case (libraryInterface) of
      Nothing -> pure nullPtr
      Just j -> ContT $ withCStruct (j)
    lift $ poke ((p `plusPtr` 64 :: Ptr (Ptr RayTracingPipelineInterfaceCreateInfoKHR))) pLibraryInterface''
    pDynamicState'' <- case (dynamicState) of
      Nothing -> pure nullPtr
      Just j -> ContT $ withCStruct (j)
    lift $ poke ((p `plusPtr` 72 :: Ptr (Ptr PipelineDynamicStateCreateInfo))) pDynamicState''
    lift $ poke ((p `plusPtr` 80 :: Ptr PipelineLayout)) (layout)
    lift $ poke ((p `plusPtr` 88 :: Ptr Pipeline)) (basePipelineHandle)
    lift $ poke ((p `plusPtr` 96 :: Ptr Int32)) (basePipelineIndex)
    lift $ f
  cStructSize = 104
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_KHR)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    pPStages' <- ContT $ allocaBytesAligned @(PipelineShaderStageCreateInfo _) ((Data.Vector.length (mempty)) * 48) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPStages' `plusPtr` (48 * (i)) :: Ptr (PipelineShaderStageCreateInfo _))) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr (PipelineShaderStageCreateInfo _)))) (pPStages')
    pPGroups' <- ContT $ allocaBytesAligned @RayTracingShaderGroupCreateInfoKHR ((Data.Vector.length (mempty)) * 48) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPGroups' `plusPtr` (48 * (i)) :: Ptr RayTracingShaderGroupCreateInfoKHR) (e)) (mempty)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr RayTracingShaderGroupCreateInfoKHR))) (pPGroups')
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 80 :: Ptr PipelineLayout)) (zero)
    lift $ poke ((p `plusPtr` 96 :: Ptr Int32)) (zero)
    lift $ f

instance (Extendss RayTracingPipelineCreateInfoKHR es, PeekChain es) => FromCStruct (RayTracingPipelineCreateInfoKHR es) where
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
    maxPipelineRayRecursionDepth <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    pLibraryInfo <- peek @(Ptr PipelineLibraryCreateInfoKHR) ((p `plusPtr` 56 :: Ptr (Ptr PipelineLibraryCreateInfoKHR)))
    pLibraryInfo' <- maybePeek (\j -> peekCStruct @PipelineLibraryCreateInfoKHR (j)) pLibraryInfo
    pLibraryInterface <- peek @(Ptr RayTracingPipelineInterfaceCreateInfoKHR) ((p `plusPtr` 64 :: Ptr (Ptr RayTracingPipelineInterfaceCreateInfoKHR)))
    pLibraryInterface' <- maybePeek (\j -> peekCStruct @RayTracingPipelineInterfaceCreateInfoKHR (j)) pLibraryInterface
    pDynamicState <- peek @(Ptr PipelineDynamicStateCreateInfo) ((p `plusPtr` 72 :: Ptr (Ptr PipelineDynamicStateCreateInfo)))
    pDynamicState' <- maybePeek (\j -> peekCStruct @PipelineDynamicStateCreateInfo (j)) pDynamicState
    layout <- peek @PipelineLayout ((p `plusPtr` 80 :: Ptr PipelineLayout))
    basePipelineHandle <- peek @Pipeline ((p `plusPtr` 88 :: Ptr Pipeline))
    basePipelineIndex <- peek @Int32 ((p `plusPtr` 96 :: Ptr Int32))
    pure $ RayTracingPipelineCreateInfoKHR
             next flags pStages' pGroups' maxPipelineRayRecursionDepth pLibraryInfo' pLibraryInterface' pDynamicState' layout basePipelineHandle basePipelineIndex

instance es ~ '[] => Zero (RayTracingPipelineCreateInfoKHR es) where
  zero = RayTracingPipelineCreateInfoKHR
           ()
           zero
           mempty
           mempty
           zero
           Nothing
           Nothing
           Nothing
           zero
           zero
           zero



-- No documentation found for TopLevel "VkPhysicalDeviceRayTracingPipelineFeaturesKHR"
data PhysicalDeviceRayTracingPipelineFeaturesKHR = PhysicalDeviceRayTracingPipelineFeaturesKHR
  { -- No documentation found for Nested "VkPhysicalDeviceRayTracingPipelineFeaturesKHR" "rayTracingPipeline"
    rayTracingPipeline :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceRayTracingPipelineFeaturesKHR" "rayTracingPipelineShaderGroupHandleCaptureReplay"
    rayTracingPipelineShaderGroupHandleCaptureReplay :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceRayTracingPipelineFeaturesKHR" "rayTracingPipelineShaderGroupHandleCaptureReplayMixed"
    rayTracingPipelineShaderGroupHandleCaptureReplayMixed :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceRayTracingPipelineFeaturesKHR" "rayTracingPipelineTraceRaysIndirect"
    rayTracingPipelineTraceRaysIndirect :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceRayTracingPipelineFeaturesKHR" "rayTraversalPrimitiveCulling"
    rayTraversalPrimitiveCulling :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceRayTracingPipelineFeaturesKHR)
#endif
deriving instance Show PhysicalDeviceRayTracingPipelineFeaturesKHR

instance ToCStruct PhysicalDeviceRayTracingPipelineFeaturesKHR where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceRayTracingPipelineFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PIPELINE_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (rayTracingPipeline))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (rayTracingPipelineShaderGroupHandleCaptureReplay))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (rayTracingPipelineShaderGroupHandleCaptureReplayMixed))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (rayTracingPipelineTraceRaysIndirect))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (rayTraversalPrimitiveCulling))
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PIPELINE_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceRayTracingPipelineFeaturesKHR where
  peekCStruct p = do
    rayTracingPipeline <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    rayTracingPipelineShaderGroupHandleCaptureReplay <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    rayTracingPipelineShaderGroupHandleCaptureReplayMixed <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    rayTracingPipelineTraceRaysIndirect <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    rayTraversalPrimitiveCulling <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    pure $ PhysicalDeviceRayTracingPipelineFeaturesKHR
             (bool32ToBool rayTracingPipeline) (bool32ToBool rayTracingPipelineShaderGroupHandleCaptureReplay) (bool32ToBool rayTracingPipelineShaderGroupHandleCaptureReplayMixed) (bool32ToBool rayTracingPipelineTraceRaysIndirect) (bool32ToBool rayTraversalPrimitiveCulling)


instance Storable PhysicalDeviceRayTracingPipelineFeaturesKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceRayTracingPipelineFeaturesKHR where
  zero = PhysicalDeviceRayTracingPipelineFeaturesKHR
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkPhysicalDeviceRayTracingPipelinePropertiesKHR"
data PhysicalDeviceRayTracingPipelinePropertiesKHR = PhysicalDeviceRayTracingPipelinePropertiesKHR
  { -- No documentation found for Nested "VkPhysicalDeviceRayTracingPipelinePropertiesKHR" "shaderGroupHandleSize"
    shaderGroupHandleSize :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceRayTracingPipelinePropertiesKHR" "maxRayRecursionDepth"
    maxRayRecursionDepth :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceRayTracingPipelinePropertiesKHR" "maxShaderGroupStride"
    maxShaderGroupStride :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceRayTracingPipelinePropertiesKHR" "shaderGroupBaseAlignment"
    shaderGroupBaseAlignment :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceRayTracingPipelinePropertiesKHR" "shaderGroupHandleCaptureReplaySize"
    shaderGroupHandleCaptureReplaySize :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceRayTracingPipelinePropertiesKHR" "maxRayDispatchInvocationCount"
    maxRayDispatchInvocationCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceRayTracingPipelinePropertiesKHR" "shaderGroupHandleAlignment"
    shaderGroupHandleAlignment :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceRayTracingPipelinePropertiesKHR" "maxRayHitAttributeSize"
    maxRayHitAttributeSize :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceRayTracingPipelinePropertiesKHR)
#endif
deriving instance Show PhysicalDeviceRayTracingPipelinePropertiesKHR

instance ToCStruct PhysicalDeviceRayTracingPipelinePropertiesKHR where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceRayTracingPipelinePropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PIPELINE_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (shaderGroupHandleSize)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxRayRecursionDepth)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (maxShaderGroupStride)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (shaderGroupBaseAlignment)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (shaderGroupHandleCaptureReplaySize)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (maxRayDispatchInvocationCount)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (shaderGroupHandleAlignment)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (maxRayHitAttributeSize)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PIPELINE_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceRayTracingPipelinePropertiesKHR where
  peekCStruct p = do
    shaderGroupHandleSize <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxRayRecursionDepth <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    maxShaderGroupStride <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    shaderGroupBaseAlignment <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    shaderGroupHandleCaptureReplaySize <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    maxRayDispatchInvocationCount <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    shaderGroupHandleAlignment <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    maxRayHitAttributeSize <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    pure $ PhysicalDeviceRayTracingPipelinePropertiesKHR
             shaderGroupHandleSize maxRayRecursionDepth maxShaderGroupStride shaderGroupBaseAlignment shaderGroupHandleCaptureReplaySize maxRayDispatchInvocationCount shaderGroupHandleAlignment maxRayHitAttributeSize


instance Storable PhysicalDeviceRayTracingPipelinePropertiesKHR where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceRayTracingPipelinePropertiesKHR where
  zero = PhysicalDeviceRayTracingPipelinePropertiesKHR
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkStridedDeviceAddressRegionKHR"
data StridedDeviceAddressRegionKHR = StridedDeviceAddressRegionKHR
  { -- No documentation found for Nested "VkStridedDeviceAddressRegionKHR" "deviceAddress"
    deviceAddress :: DeviceAddress
  , -- No documentation found for Nested "VkStridedDeviceAddressRegionKHR" "stride"
    stride :: DeviceSize
  , -- No documentation found for Nested "VkStridedDeviceAddressRegionKHR" "size"
    size :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (StridedDeviceAddressRegionKHR)
#endif
deriving instance Show StridedDeviceAddressRegionKHR

instance ToCStruct StridedDeviceAddressRegionKHR where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p StridedDeviceAddressRegionKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceAddress)) (deviceAddress)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (stride)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (size)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct StridedDeviceAddressRegionKHR where
  peekCStruct p = do
    deviceAddress <- peek @DeviceAddress ((p `plusPtr` 0 :: Ptr DeviceAddress))
    stride <- peek @DeviceSize ((p `plusPtr` 8 :: Ptr DeviceSize))
    size <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    pure $ StridedDeviceAddressRegionKHR
             deviceAddress stride size


instance Storable StridedDeviceAddressRegionKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero StridedDeviceAddressRegionKHR where
  zero = StridedDeviceAddressRegionKHR
           zero
           zero
           zero



-- No documentation found for TopLevel "VkTraceRaysIndirectCommandKHR"
data TraceRaysIndirectCommandKHR = TraceRaysIndirectCommandKHR
  { -- No documentation found for Nested "VkTraceRaysIndirectCommandKHR" "width"
    width :: Word32
  , -- No documentation found for Nested "VkTraceRaysIndirectCommandKHR" "height"
    height :: Word32
  , -- No documentation found for Nested "VkTraceRaysIndirectCommandKHR" "depth"
    depth :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (TraceRaysIndirectCommandKHR)
#endif
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



-- No documentation found for TopLevel "VkRayTracingPipelineInterfaceCreateInfoKHR"
data RayTracingPipelineInterfaceCreateInfoKHR = RayTracingPipelineInterfaceCreateInfoKHR
  { -- No documentation found for Nested "VkRayTracingPipelineInterfaceCreateInfoKHR" "maxPipelineRayPayloadSize"
    maxPipelineRayPayloadSize :: Word32
  , -- No documentation found for Nested "VkRayTracingPipelineInterfaceCreateInfoKHR" "maxPipelineRayHitAttributeSize"
    maxPipelineRayHitAttributeSize :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RayTracingPipelineInterfaceCreateInfoKHR)
#endif
deriving instance Show RayTracingPipelineInterfaceCreateInfoKHR

instance ToCStruct RayTracingPipelineInterfaceCreateInfoKHR where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RayTracingPipelineInterfaceCreateInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RAY_TRACING_PIPELINE_INTERFACE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxPipelineRayPayloadSize)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxPipelineRayHitAttributeSize)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RAY_TRACING_PIPELINE_INTERFACE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    f

instance FromCStruct RayTracingPipelineInterfaceCreateInfoKHR where
  peekCStruct p = do
    maxPipelineRayPayloadSize <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxPipelineRayHitAttributeSize <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pure $ RayTracingPipelineInterfaceCreateInfoKHR
             maxPipelineRayPayloadSize maxPipelineRayHitAttributeSize


instance Storable RayTracingPipelineInterfaceCreateInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero RayTracingPipelineInterfaceCreateInfoKHR where
  zero = RayTracingPipelineInterfaceCreateInfoKHR
           zero
           zero


-- No documentation found for TopLevel "VkRayTracingShaderGroupTypeKHR"
newtype RayTracingShaderGroupTypeKHR = RayTracingShaderGroupTypeKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkRayTracingShaderGroupTypeKHR" "VK_RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR"
pattern RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR              = RayTracingShaderGroupTypeKHR 0
-- No documentation found for Nested "VkRayTracingShaderGroupTypeKHR" "VK_RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR"
pattern RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR  = RayTracingShaderGroupTypeKHR 1
-- No documentation found for Nested "VkRayTracingShaderGroupTypeKHR" "VK_RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR"
pattern RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR = RayTracingShaderGroupTypeKHR 2
{-# complete RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR,
             RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR,
             RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR :: RayTracingShaderGroupTypeKHR #-}

conNameRayTracingShaderGroupTypeKHR :: String
conNameRayTracingShaderGroupTypeKHR = "RayTracingShaderGroupTypeKHR"

enumPrefixRayTracingShaderGroupTypeKHR :: String
enumPrefixRayTracingShaderGroupTypeKHR = "RAY_TRACING_SHADER_GROUP_TYPE_"

showTableRayTracingShaderGroupTypeKHR :: [(RayTracingShaderGroupTypeKHR, String)]
showTableRayTracingShaderGroupTypeKHR =
  [ (RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR             , "GENERAL_KHR")
  , (RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR , "TRIANGLES_HIT_GROUP_KHR")
  , (RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR, "PROCEDURAL_HIT_GROUP_KHR")
  ]


instance Show RayTracingShaderGroupTypeKHR where
showsPrec = enumShowsPrec enumPrefixRayTracingShaderGroupTypeKHR
                          showTableRayTracingShaderGroupTypeKHR
                          conNameRayTracingShaderGroupTypeKHR
                          (\(RayTracingShaderGroupTypeKHR x) -> x)
                          (showsPrec 11)


instance Read RayTracingShaderGroupTypeKHR where
  readPrec = enumReadPrec enumPrefixRayTracingShaderGroupTypeKHR
                          showTableRayTracingShaderGroupTypeKHR
                          conNameRayTracingShaderGroupTypeKHR
                          RayTracingShaderGroupTypeKHR


-- No documentation found for TopLevel "VkShaderGroupShaderKHR"
newtype ShaderGroupShaderKHR = ShaderGroupShaderKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkShaderGroupShaderKHR" "VK_SHADER_GROUP_SHADER_GENERAL_KHR"
pattern SHADER_GROUP_SHADER_GENERAL_KHR      = ShaderGroupShaderKHR 0
-- No documentation found for Nested "VkShaderGroupShaderKHR" "VK_SHADER_GROUP_SHADER_CLOSEST_HIT_KHR"
pattern SHADER_GROUP_SHADER_CLOSEST_HIT_KHR  = ShaderGroupShaderKHR 1
-- No documentation found for Nested "VkShaderGroupShaderKHR" "VK_SHADER_GROUP_SHADER_ANY_HIT_KHR"
pattern SHADER_GROUP_SHADER_ANY_HIT_KHR      = ShaderGroupShaderKHR 2
-- No documentation found for Nested "VkShaderGroupShaderKHR" "VK_SHADER_GROUP_SHADER_INTERSECTION_KHR"
pattern SHADER_GROUP_SHADER_INTERSECTION_KHR = ShaderGroupShaderKHR 3
{-# complete SHADER_GROUP_SHADER_GENERAL_KHR,
             SHADER_GROUP_SHADER_CLOSEST_HIT_KHR,
             SHADER_GROUP_SHADER_ANY_HIT_KHR,
             SHADER_GROUP_SHADER_INTERSECTION_KHR :: ShaderGroupShaderKHR #-}

conNameShaderGroupShaderKHR :: String
conNameShaderGroupShaderKHR = "ShaderGroupShaderKHR"

enumPrefixShaderGroupShaderKHR :: String
enumPrefixShaderGroupShaderKHR = "SHADER_GROUP_SHADER_"

showTableShaderGroupShaderKHR :: [(ShaderGroupShaderKHR, String)]
showTableShaderGroupShaderKHR =
  [ (SHADER_GROUP_SHADER_GENERAL_KHR     , "GENERAL_KHR")
  , (SHADER_GROUP_SHADER_CLOSEST_HIT_KHR , "CLOSEST_HIT_KHR")
  , (SHADER_GROUP_SHADER_ANY_HIT_KHR     , "ANY_HIT_KHR")
  , (SHADER_GROUP_SHADER_INTERSECTION_KHR, "INTERSECTION_KHR")
  ]


instance Show ShaderGroupShaderKHR where
showsPrec = enumShowsPrec enumPrefixShaderGroupShaderKHR
                          showTableShaderGroupShaderKHR
                          conNameShaderGroupShaderKHR
                          (\(ShaderGroupShaderKHR x) -> x)
                          (showsPrec 11)


instance Read ShaderGroupShaderKHR where
  readPrec = enumReadPrec enumPrefixShaderGroupShaderKHR
                          showTableShaderGroupShaderKHR
                          conNameShaderGroupShaderKHR
                          ShaderGroupShaderKHR


type KHR_RAY_TRACING_PIPELINE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_RAY_TRACING_PIPELINE_SPEC_VERSION"
pattern KHR_RAY_TRACING_PIPELINE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_RAY_TRACING_PIPELINE_SPEC_VERSION = 1


type KHR_RAY_TRACING_PIPELINE_EXTENSION_NAME = "VK_KHR_ray_tracing_pipeline"

-- No documentation found for TopLevel "VK_KHR_RAY_TRACING_PIPELINE_EXTENSION_NAME"
pattern KHR_RAY_TRACING_PIPELINE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_RAY_TRACING_PIPELINE_EXTENSION_NAME = "VK_KHR_ray_tracing_pipeline"

