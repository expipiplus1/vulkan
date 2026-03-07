{-# language CPP #-}
-- | = Name
--
-- VK_KHR_pipeline_binary - device extension
--
-- = VK_KHR_pipeline_binary
--
-- [__Name String__]
--     @VK_KHR_pipeline_binary@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     484
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>
--
-- [__Contact__]
--
--     -   Stu Smith
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_pipeline_binary] @stu-s%0A*Here describe the issue or question you have about the VK_KHR_pipeline_binary extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_pipeline_binary.adoc VK_KHR_pipeline_binary>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-07-01
--
-- [__Contributors__]
--
--     -   Stu Smith, AMD
--
--     -   Tobias Hector, AMD
--
--     -   Alan Harrison, AMD
--
--     -   Maciej Jesionowski, AMD
--
--     -   Younggwan Kim, Arm
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Ting Wei, Arm
--
--     -   Chris Glover, Google
--
--     -   Shahbaz Youssefi, Google
--
--     -   Jakub Kuderski, Google
--
--     -   Piotr Byszewski, Mobica
--
--     -   Piers Daniell, NVIDIA
--
--     -   Ralph Potter, Samsung
--
--     -   Matthew Netsch, Qualcomm
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Samuel Pitoiset, Valve
--
--     -   Tatsuyuki Ishi, Valve
--
-- == Description
--
-- This extension provides a method to obtain binary data associated with
-- individual pipelines such that applications can manage caching
-- themselves instead of using VkPipelineCache objects.
--
-- == New Object Types
--
-- -   'Vulkan.Extensions.Handles.PipelineBinaryKHR'
--
-- == New Commands
--
-- -   'createPipelineBinariesKHR'
--
-- -   'destroyPipelineBinaryKHR'
--
-- -   'getPipelineBinaryDataKHR'
--
-- -   'getPipelineKeyKHR'
--
-- -   'releaseCapturedPipelineDataKHR'
--
-- == New Structures
--
-- -   'PipelineBinaryCreateInfoKHR'
--
-- -   'PipelineBinaryDataInfoKHR'
--
-- -   'PipelineBinaryDataKHR'
--
-- -   'PipelineBinaryHandlesInfoKHR'
--
-- -   'PipelineBinaryKeyKHR'
--
-- -   'PipelineBinaryKeysAndDataKHR'
--
-- -   'PipelineCreateInfoKHR'
--
-- -   'ReleaseCapturedPipelineDataInfoKHR'
--
-- -   Extending 'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'DevicePipelineBinaryInternalCacheControlKHR'
--
-- -   Extending 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo',
--     'Vulkan.Core10.Pipeline.ComputePipelineCreateInfo',
--     'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.RayTracingPipelineCreateInfoKHR':
--
--     -   'PipelineBinaryInfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePipelineBinaryFeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDevicePipelineBinaryPropertiesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_PIPELINE_BINARY_EXTENSION_NAME'
--
-- -   'KHR_PIPELINE_BINARY_SPEC_VERSION'
--
-- -   'Vulkan.Core10.APIConstants.MAX_PIPELINE_BINARY_KEY_SIZE_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.ObjectType.ObjectType':
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_PIPELINE_BINARY_KHR'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_maintenance5.PipelineCreateFlagBits2KHR':
--
--     -   'Vulkan.Extensions.VK_KHR_maintenance5.PIPELINE_CREATE_2_CAPTURE_DATA_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.Result.Result':
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_NOT_ENOUGH_SPACE_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.PIPELINE_BINARY_MISSING_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_PIPELINE_BINARY_INTERNAL_CACHE_CONTROL_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_BINARY_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_BINARY_PROPERTIES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_BINARY_CREATE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_BINARY_DATA_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_BINARY_HANDLES_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_BINARY_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_BINARY_KEY_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_CREATE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RELEASE_CAPTURED_PIPELINE_DATA_INFO_KHR'
--
-- == Version History
--
-- -   Revision 1, 2021-12-10 (Chris Glover)
--
--     -   Initial draft.
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_pipeline_binary Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_pipeline_binary  ( createPipelineBinariesKHR
                                                 , destroyPipelineBinaryKHR
                                                 , getPipelineKeyKHR
                                                 , getPipelineBinaryDataKHR
                                                 , releaseCapturedPipelineDataKHR
                                                 , PipelineBinaryCreateInfoKHR(..)
                                                 , PipelineBinaryHandlesInfoKHR(..)
                                                 , PipelineBinaryDataKHR(..)
                                                 , PipelineBinaryKeysAndDataKHR(..)
                                                 , PipelineBinaryKeyKHR(..)
                                                 , PipelineBinaryInfoKHR(..)
                                                 , ReleaseCapturedPipelineDataInfoKHR(..)
                                                 , PipelineBinaryDataInfoKHR(..)
                                                 , PipelineCreateInfoKHR(..)
                                                 , PhysicalDevicePipelineBinaryFeaturesKHR(..)
                                                 , DevicePipelineBinaryInternalCacheControlKHR(..)
                                                 , PhysicalDevicePipelineBinaryPropertiesKHR(..)
                                                 , KHR_PIPELINE_BINARY_SPEC_VERSION
                                                 , pattern KHR_PIPELINE_BINARY_SPEC_VERSION
                                                 , KHR_PIPELINE_BINARY_EXTENSION_NAME
                                                 , pattern KHR_PIPELINE_BINARY_EXTENSION_NAME
                                                 , PipelineBinaryKHR(..)
                                                 , PipelineCreateFlagBits2KHR(..)
                                                 , PipelineCreateFlags2KHR
                                                 , MAX_PIPELINE_BINARY_KEY_SIZE_KHR
                                                 , pattern MAX_PIPELINE_BINARY_KEY_SIZE_KHR
                                                 ) where

import Vulkan.CStruct.Utils (FixedArray)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
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
import Data.ByteString (packCStringLen)
import Data.Coerce (coerce)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Foreign.C.Types (CSize(..))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
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
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
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
import Vulkan.CStruct.Utils (peekByteStringFromSizedVectorPtr)
import Vulkan.CStruct.Utils (pokeFixedLengthByteString)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkCreatePipelineBinariesKHR))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyPipelineBinaryKHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetPipelineBinaryDataKHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetPipelineKeyKHR))
import Vulkan.Dynamic (DeviceCmds(pVkReleaseCapturedPipelineDataKHR))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.APIConstants (MAX_PIPELINE_BINARY_KEY_SIZE_KHR)
import Vulkan.Core10.Handles (Pipeline)
import Vulkan.Extensions.Handles (PipelineBinaryKHR)
import Vulkan.Extensions.Handles (PipelineBinaryKHR(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_PIPELINE_BINARY_INTERNAL_CACHE_CONTROL_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_BINARY_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_BINARY_PROPERTIES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_BINARY_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_BINARY_DATA_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_BINARY_HANDLES_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_BINARY_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_BINARY_KEY_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RELEASE_CAPTURED_PIPELINE_DATA_INFO_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.APIConstants (MAX_PIPELINE_BINARY_KEY_SIZE_KHR)
import Vulkan.Extensions.Handles (PipelineBinaryKHR(..))
import Vulkan.Extensions.VK_KHR_maintenance5 (PipelineCreateFlagBits2KHR(..))
import Vulkan.Extensions.VK_KHR_maintenance5 (PipelineCreateFlags2KHR)
import Vulkan.Core10.APIConstants (pattern MAX_PIPELINE_BINARY_KEY_SIZE_KHR)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreatePipelineBinariesKHR
  :: FunPtr (Ptr Device_T -> Ptr PipelineBinaryCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr PipelineBinaryHandlesInfoKHR -> IO Result) -> Ptr Device_T -> Ptr PipelineBinaryCreateInfoKHR -> Ptr AllocationCallbacks -> Ptr PipelineBinaryHandlesInfoKHR -> IO Result

-- | vkCreatePipelineBinariesKHR - Create pipeline binaries from a pipeline
-- or previously retrieved data
--
-- = Description
--
-- The implementation will attempt to create all pipeline binaries. If
-- creation fails for any pipeline binary, then:
--
-- -   The corresponding entry in the @pPipelineBinaries@ output array will
--     be filled with 'Vulkan.Core10.APIConstants.NULL_HANDLE'.
--
-- -   The 'Vulkan.Core10.Enums.Result.Result' returned by
--     'createPipelineBinariesKHR' will contain the error value for the
--     first entry in the output array in @pBinaries@ containing
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreatePipelineBinariesKHR-device-parameter# @device@ /must/
--     be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreatePipelineBinariesKHR-pCreateInfo-parameter#
--     @pCreateInfo@ /must/ be a valid pointer to a valid
--     'PipelineBinaryCreateInfoKHR' structure
--
-- -   #VUID-vkCreatePipelineBinariesKHR-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkCreatePipelineBinariesKHR-pBinaries-parameter# @pBinaries@
--     /must/ be a valid pointer to a 'PipelineBinaryHandlesInfoKHR'
--     structure
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.INCOMPLETE'
--
--     -   'Vulkan.Core10.Enums.Result.PIPELINE_BINARY_MISSING_KHR'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_pipeline_binary VK_KHR_pipeline_binary>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'PipelineBinaryCreateInfoKHR',
-- 'PipelineBinaryHandlesInfoKHR'
createPipelineBinariesKHR :: forall io
                           . (MonadIO io)
                          => -- | @device@ is the logical device that creates the pipeline binary objects.
                             Device
                          -> -- | @pCreateInfo@ is a pointer to a 'PipelineBinaryCreateInfoKHR' structure
                             -- that contains the data to create the pipeline binaries from.
                             PipelineBinaryCreateInfoKHR
                          -> -- | @pAllocator@ controls host memory allocation as described in the
                             -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                             -- chapter.
                             ("allocator" ::: Maybe AllocationCallbacks)
                          -> io (Result, ("binaries" ::: PipelineBinaryHandlesInfoKHR))
createPipelineBinariesKHR device createInfo allocator = liftIO . evalContT $ do
  let vkCreatePipelineBinariesKHRPtr = pVkCreatePipelineBinariesKHR (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCreatePipelineBinariesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreatePipelineBinariesKHR is null" Nothing Nothing
  let vkCreatePipelineBinariesKHR' = mkVkCreatePipelineBinariesKHR vkCreatePipelineBinariesKHRPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPBinaries <- ContT (withZeroCStruct @PipelineBinaryHandlesInfoKHR)
  r <- lift $ traceAroundEvent "vkCreatePipelineBinariesKHR" (vkCreatePipelineBinariesKHR'
                                                                (deviceHandle (device))
                                                                pCreateInfo
                                                                pAllocator
                                                                (pPBinaries))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pBinaries <- lift $ peekCStruct @PipelineBinaryHandlesInfoKHR pPBinaries
  pure $ (r, pBinaries)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyPipelineBinaryKHR
  :: FunPtr (Ptr Device_T -> PipelineBinaryKHR -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> PipelineBinaryKHR -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyPipelineBinaryKHR - Destroy a pipeline binary
--
-- == Valid Usage
--
-- -   #VUID-vkDestroyPipelineBinaryKHR-pipelineBinary-09614# If
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @pipelineBinary@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   #VUID-vkDestroyPipelineBinaryKHR-pipelineBinary-09615# If no
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @pipelineBinary@ was created, @pAllocator@ /must/ be
--     @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroyPipelineBinaryKHR-device-parameter# @device@ /must/
--     be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDestroyPipelineBinaryKHR-pipelineBinary-parameter# If
--     @pipelineBinary@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @pipelineBinary@ /must/ be a valid
--     'Vulkan.Extensions.Handles.PipelineBinaryKHR' handle
--
-- -   #VUID-vkDestroyPipelineBinaryKHR-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkDestroyPipelineBinaryKHR-pipelineBinary-parent# If
--     @pipelineBinary@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @pipelineBinary@ /must/ be externally synchronized
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_pipeline_binary VK_KHR_pipeline_binary>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Extensions.Handles.PipelineBinaryKHR'
destroyPipelineBinaryKHR :: forall io
                          . (MonadIO io)
                         => -- | @device@ is the logical device that created the pipeline binary object.
                            Device
                         -> -- | @pipelineBinary@ is the handle of the pipeline binary object to destroy.
                            PipelineBinaryKHR
                         -> -- | @pAllocator@ controls host memory allocation as described in the
                            -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                            -- chapter.
                            ("allocator" ::: Maybe AllocationCallbacks)
                         -> io ()
destroyPipelineBinaryKHR device
                           pipelineBinary
                           allocator = liftIO . evalContT $ do
  let vkDestroyPipelineBinaryKHRPtr = pVkDestroyPipelineBinaryKHR (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkDestroyPipelineBinaryKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyPipelineBinaryKHR is null" Nothing Nothing
  let vkDestroyPipelineBinaryKHR' = mkVkDestroyPipelineBinaryKHR vkDestroyPipelineBinaryKHRPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyPipelineBinaryKHR" (vkDestroyPipelineBinaryKHR'
                                                          (deviceHandle (device))
                                                          (pipelineBinary)
                                                          pAllocator)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPipelineKeyKHR
  :: FunPtr (Ptr Device_T -> Ptr PipelineCreateInfoKHR -> Ptr PipelineBinaryKeyKHR -> IO Result) -> Ptr Device_T -> Ptr PipelineCreateInfoKHR -> Ptr PipelineBinaryKeyKHR -> IO Result

-- | vkGetPipelineKeyKHR - Generate the pipeline key from pipeline creation
-- info
--
-- = Description
--
-- If @pPipelineCreateInfo@ is @NULL@, then the implementation /must/
-- return the global key that applies to all pipelines. If the key obtained
-- in this way changes between saving and restoring data obtained from
-- 'getPipelineBinaryDataKHR' in a different
-- 'Vulkan.Core10.Handles.Device', then the application /must/ assume that
-- the restored data is invalid and cannot be passed to
-- 'createPipelineBinariesKHR'. Otherwise the application /can/ assume the
-- data is still valid.
--
-- If @pPipelineCreateInfo@ is not @NULL@, the key obtained functions as a
-- method to compare two pipeline creation info structures. Implementations
-- /may/ not compare parts of a pipeline creation info which would not
-- contribute to the final binary output. If a shader module identifier is
-- used instead of a shader module, the @pPipelineKey@ generated /must/ be
-- equal to the key generated when using the shader module from which the
-- identifier was queried. If the content of two @pPipelineKey@ are equal,
-- pipelines created with the two @pPipelineCreateInfo->pname@:pNext create
-- infos /must/ produce the same
-- 'Vulkan.Extensions.Handles.PipelineBinaryKHR' contents.
--
-- The pipeline key is distinct from pipeline binary key. Pipeline binary
-- keys /can/ only be obtained after compilation. The pipeline key is
-- intended to optionally allow associating pipeline create info with
-- multiple pipeline binary keys.
--
-- == Valid Usage
--
-- -   #VUID-vkGetPipelineKeyKHR-pNext-09605# The @pNext@ chain of
--     @pPipelineCreateInfo@ /must/ not set
--     'PipelineBinaryInfoKHR'::@binaryCount@ to a value greater than @0@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPipelineKeyKHR-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetPipelineKeyKHR-pPipelineCreateInfo-parameter# If
--     @pPipelineCreateInfo@ is not @NULL@, @pPipelineCreateInfo@ /must/ be
--     a valid pointer to a valid 'PipelineCreateInfoKHR' structure
--
-- -   #VUID-vkGetPipelineKeyKHR-pPipelineKey-parameter# @pPipelineKey@
--     /must/ be a valid pointer to a 'PipelineBinaryKeyKHR' structure
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_pipeline_binary VK_KHR_pipeline_binary>,
-- 'Vulkan.Core10.Handles.Device', 'PipelineBinaryKeyKHR',
-- 'PipelineCreateInfoKHR'
getPipelineKeyKHR :: forall io
                   . (MonadIO io)
                  => -- | @device@ is the logical device that creates the pipeline object.
                     Device
                  -> -- | @pPipelineCreateInfo@ is @NULL@ or a pointer to a
                     -- 'PipelineCreateInfoKHR' structure.
                     ("pipelineCreateInfo" ::: Maybe PipelineCreateInfoKHR)
                  -> io (("pipelineKey" ::: PipelineBinaryKeyKHR))
getPipelineKeyKHR device pipelineCreateInfo = liftIO . evalContT $ do
  let vkGetPipelineKeyKHRPtr = pVkGetPipelineKeyKHR (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetPipelineKeyKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPipelineKeyKHR is null" Nothing Nothing
  let vkGetPipelineKeyKHR' = mkVkGetPipelineKeyKHR vkGetPipelineKeyKHRPtr
  pPipelineCreateInfo <- case (pipelineCreateInfo) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPPipelineKey <- ContT (withZeroCStruct @PipelineBinaryKeyKHR)
  r <- lift $ traceAroundEvent "vkGetPipelineKeyKHR" (vkGetPipelineKeyKHR'
                                                        (deviceHandle (device))
                                                        pPipelineCreateInfo
                                                        (pPPipelineKey))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPipelineKey <- lift $ peekCStruct @PipelineBinaryKeyKHR pPPipelineKey
  pure $ (pPipelineKey)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPipelineBinaryDataKHR
  :: FunPtr (Ptr Device_T -> Ptr PipelineBinaryDataInfoKHR -> Ptr PipelineBinaryKeyKHR -> Ptr CSize -> Ptr () -> IO Result) -> Ptr Device_T -> Ptr PipelineBinaryDataInfoKHR -> Ptr PipelineBinaryKeyKHR -> Ptr CSize -> Ptr () -> IO Result

-- | vkGetPipelineBinaryDataKHR - Get the data store from a pipeline binary
--
-- = Description
--
-- If @pPipelineBinaryData@ is @NULL@, then the size of the data, in bytes,
-- that is required to store the binary is returned in
-- @pPipelineBinaryDataSize@. Otherwise, @pPipelineBinaryDataSize@ /must/
-- contain the size of the buffer, in bytes, pointed to by
-- @pPipelineBinaryData@, and on return @pPipelineBinaryDataSize@ is
-- overwritten with the size of the data, in bytes, that is required to
-- store the binary. If @pPipelineBinaryDataSize@ is less than the size
-- that is required to store the binary, nothing is written to
-- @pPipelineBinaryData@ and
-- 'Vulkan.Core10.Enums.Result.ERROR_NOT_ENOUGH_SPACE_KHR' will be
-- returned, instead of 'Vulkan.Core10.Enums.Result.SUCCESS'.
--
-- If
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-pipelineBinaryCompressedData pipelineBinaryCompressedData>
-- is 'Vulkan.Core10.FundamentalTypes.FALSE', implementations /should/ not
-- return compressed pipeline binary data to the application.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPipelineBinaryDataKHR-device-parameter# @device@ /must/
--     be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetPipelineBinaryDataKHR-pInfo-parameter# @pInfo@ /must/ be
--     a valid pointer to a valid 'PipelineBinaryDataInfoKHR' structure
--
-- -   #VUID-vkGetPipelineBinaryDataKHR-pPipelineBinaryKey-parameter#
--     @pPipelineBinaryKey@ /must/ be a valid pointer to a
--     'PipelineBinaryKeyKHR' structure
--
-- -   #VUID-vkGetPipelineBinaryDataKHR-pPipelineBinaryDataSize-parameter#
--     @pPipelineBinaryDataSize@ /must/ be a valid pointer to a @size_t@
--     value
--
-- -   #VUID-vkGetPipelineBinaryDataKHR-pPipelineBinaryData-parameter# If
--     the value referenced by @pPipelineBinaryDataSize@ is not @0@, and
--     @pPipelineBinaryData@ is not @NULL@, @pPipelineBinaryData@ /must/ be
--     a valid pointer to an array of @pPipelineBinaryDataSize@ bytes
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_NOT_ENOUGH_SPACE_KHR'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_pipeline_binary VK_KHR_pipeline_binary>,
-- 'Vulkan.Core10.Handles.Device', 'PipelineBinaryDataInfoKHR',
-- 'PipelineBinaryKeyKHR'
getPipelineBinaryDataKHR :: forall io
                          . (MonadIO io)
                         => -- | @device@ is the logical device that created the pipeline binary.
                            Device
                         -> -- | @pInfo@ is a pointer to a 'PipelineBinaryDataInfoKHR' structure which
                            -- describes the pipeline binary to get data from.
                            PipelineBinaryDataInfoKHR
                         -> io (PipelineBinaryKeyKHR, ("pipelineBinaryData" ::: ByteString))
getPipelineBinaryDataKHR device info = liftIO . evalContT $ do
  let vkGetPipelineBinaryDataKHRPtr = pVkGetPipelineBinaryDataKHR (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetPipelineBinaryDataKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPipelineBinaryDataKHR is null" Nothing Nothing
  let vkGetPipelineBinaryDataKHR' = mkVkGetPipelineBinaryDataKHR vkGetPipelineBinaryDataKHRPtr
  let device' = deviceHandle (device)
  pInfo <- ContT $ withCStruct (info)
  pPPipelineBinaryKey <- ContT (withZeroCStruct @PipelineBinaryKeyKHR)
  pPPipelineBinaryDataSize <- ContT $ bracket (callocBytes @CSize 8) free
  r <- lift $ traceAroundEvent "vkGetPipelineBinaryDataKHR" (vkGetPipelineBinaryDataKHR'
                                                               device'
                                                               pInfo
                                                               (pPPipelineBinaryKey)
                                                               (pPPipelineBinaryDataSize)
                                                               (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPipelineBinaryDataSize <- lift $ peek @CSize pPPipelineBinaryDataSize
  pPPipelineBinaryData <- ContT $ bracket (callocBytes @(()) (fromIntegral ((coerce @CSize @Word64 pPipelineBinaryDataSize)))) free
  r' <- lift $ traceAroundEvent "vkGetPipelineBinaryDataKHR" (vkGetPipelineBinaryDataKHR'
                                                                device'
                                                                pInfo
                                                                (pPPipelineBinaryKey)
                                                                (pPPipelineBinaryDataSize)
                                                                (pPPipelineBinaryData))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pPipelineBinaryKey <- lift $ peekCStruct @PipelineBinaryKeyKHR pPPipelineBinaryKey
  pPipelineBinaryDataSize'' <- lift $ peek @CSize pPPipelineBinaryDataSize
  pPipelineBinaryData' <- lift $ packCStringLen  ( castPtr @() @CChar pPPipelineBinaryData
                                                 , (fromIntegral ((coerce @CSize @Word64 pPipelineBinaryDataSize''))) )
  pure $ (pPipelineBinaryKey, pPipelineBinaryData')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkReleaseCapturedPipelineDataKHR
  :: FunPtr (Ptr Device_T -> Ptr ReleaseCapturedPipelineDataInfoKHR -> Ptr AllocationCallbacks -> IO Result) -> Ptr Device_T -> Ptr ReleaseCapturedPipelineDataInfoKHR -> Ptr AllocationCallbacks -> IO Result

-- | vkReleaseCapturedPipelineDataKHR - Release captured pipeline binary data
--
-- = Description
--
-- The implementation /may/ free any resources captured as a result of
-- creating the pipeline with
-- 'Vulkan.Extensions.VK_KHR_maintenance5.PIPELINE_CREATE_2_CAPTURE_DATA_BIT_KHR'
-- and put the pipeline into a state as if
-- 'Vulkan.Extensions.VK_KHR_maintenance5.PIPELINE_CREATE_2_CAPTURE_DATA_BIT_KHR'
-- had not been provided at pipeline creation time.
--
-- Any resources captured as a result of creating the pipeline with
-- 'Vulkan.Extensions.VK_KHR_maintenance5.PIPELINE_CREATE_2_CAPTURE_DATA_BIT_KHR'
-- are implicitly freed by 'Vulkan.Core10.Pipeline.destroyPipeline'.
--
-- == Valid Usage
--
-- -   #VUID-vkReleaseCapturedPipelineDataKHR-pipeline-09611# If
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @pipeline@ was created, a compatible set of callbacks
--     /must/ be provided in @pAllocator@
--
-- -   #VUID-vkReleaseCapturedPipelineDataKHR-pipeline-09612# If no
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @pipeline@ was created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkReleaseCapturedPipelineDataKHR-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkReleaseCapturedPipelineDataKHR-pInfo-parameter# @pInfo@
--     /must/ be a valid pointer to a valid
--     'ReleaseCapturedPipelineDataInfoKHR' structure
--
-- -   #VUID-vkReleaseCapturedPipelineDataKHR-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- == Host Synchronization
--
-- -   Host access to @pInfo->pipeline@ /must/ be externally synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     None
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_pipeline_binary VK_KHR_pipeline_binary>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'ReleaseCapturedPipelineDataInfoKHR'
releaseCapturedPipelineDataKHR :: forall io
                                . (MonadIO io)
                               => -- | @device@ is the logical device that created the pipeline object.
                                  Device
                               -> -- | @pInfo@ is a pointer to a 'ReleaseCapturedPipelineDataInfoKHR' structure
                                  -- which describes the pipeline to release the data from.
                                  ReleaseCapturedPipelineDataInfoKHR
                               -> -- | @pAllocator@ controls host memory allocation as described in the
                                  -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                                  -- chapter.
                                  ("allocator" ::: Maybe AllocationCallbacks)
                               -> io ()
releaseCapturedPipelineDataKHR device info allocator = liftIO . evalContT $ do
  let vkReleaseCapturedPipelineDataKHRPtr = pVkReleaseCapturedPipelineDataKHR (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkReleaseCapturedPipelineDataKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkReleaseCapturedPipelineDataKHR is null" Nothing Nothing
  let vkReleaseCapturedPipelineDataKHR' = mkVkReleaseCapturedPipelineDataKHR vkReleaseCapturedPipelineDataKHRPtr
  pInfo <- ContT $ withCStruct (info)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  _ <- lift $ traceAroundEvent "vkReleaseCapturedPipelineDataKHR" (vkReleaseCapturedPipelineDataKHR'
                                                                     (deviceHandle (device))
                                                                     pInfo
                                                                     pAllocator)
  pure $ ()


-- | VkPipelineBinaryCreateInfoKHR - Structure specifying where to retrieve
-- data for pipeline binary creation
--
-- = Description
--
-- When @pPipelineCreateInfo@ is not @NULL@, an implementation will attempt
-- to retrieve pipeline binary data from an internal cache external to the
-- application if
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-pipelineBinaryInternalCache pipelineBinaryInternalCache>
-- is 'Vulkan.Core10.FundamentalTypes.TRUE'. Applications /can/ use this to
-- determine if a pipeline /can/ be created without compilation. If the
-- implementation fails to create a pipeline binary due to missing an
-- internal cache entry,
-- 'Vulkan.Core10.Enums.Result.PIPELINE_BINARY_MISSING_KHR' is returned. If
-- creation succeeds, the resulting binary /can/ be used to create a
-- pipeline. 'Vulkan.Core10.Enums.Result.PIPELINE_BINARY_MISSING_KHR' /may/
-- be returned for any reason in this situation, even if creating a
-- pipeline binary with the same parameters that succeeded earlier.
--
-- If
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-pipelineBinaryPrecompiledInternalCache pipelineBinaryPrecompiledInternalCache>
-- is 'Vulkan.Core10.FundamentalTypes.TRUE', the implementation /may/ be
-- able to create pipeline binaries even when @pPipelineCreateInfo@ has not
-- been used to create binaries before by the application.
--
-- On some platforms, internal pipeline caches may be pre-populated before
-- running the application.
--
-- == Valid Usage
--
-- -   #VUID-VkPipelineBinaryCreateInfoKHR-pipeline-09607# If @pipeline@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @pipeline@ /must/ have
--     been created with
--     'Vulkan.Extensions.VK_KHR_maintenance5.PIPELINE_CREATE_2_CAPTURE_DATA_BIT_KHR'
--
-- -   #VUID-VkPipelineBinaryCreateInfoKHR-pipeline-09608# If @pipeline@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     'releaseCapturedPipelineDataKHR' /must/ not have been called on
--     @pipeline@ prior to this command
--
-- -   #VUID-VkPipelineBinaryCreateInfoKHR-pipelineBinaryInternalCache-09609#
--     If
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-pipelineBinaryInternalCache pipelineBinaryInternalCache>
--     is 'Vulkan.Core10.FundamentalTypes.FALSE' pPipelineCreateInfo /must/
--     be @NULL@
--
-- -   #VUID-VkPipelineBinaryCreateInfoKHR-device-09610# If @device@ was
--     created with
--     'DevicePipelineBinaryInternalCacheControlKHR'::@disableInternalCache@
--     set to 'Vulkan.Core10.FundamentalTypes.TRUE', @pPipelineCreateInfo@
--     /must/ be @NULL@
--
-- -   #VUID-VkPipelineBinaryCreateInfoKHR-pKeysAndDataInfo-09619# One and
--     only one of @pKeysAndDataInfo@, @pipeline@, or @pPipelineCreateInfo@
--     /must/ be non-@NULL@
--
-- -   #VUID-VkPipelineBinaryCreateInfoKHR-pPipelineCreateInfo-09606# If
--     @pPipelineCreateInfo@ is not @NULL@, the @pNext@ chain of
--     @pPipelineCreateInfo@ /must/ not set
--     'PipelineBinaryInfoKHR'::@binaryCount@ to a value greater than @0@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineBinaryCreateInfoKHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_BINARY_CREATE_INFO_KHR'
--
-- -   #VUID-VkPipelineBinaryCreateInfoKHR-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkPipelineBinaryCreateInfoKHR-pKeysAndDataInfo-parameter# If
--     @pKeysAndDataInfo@ is not @NULL@, @pKeysAndDataInfo@ /must/ be a
--     valid pointer to a valid 'PipelineBinaryKeysAndDataKHR' structure
--
-- -   #VUID-VkPipelineBinaryCreateInfoKHR-pipeline-parameter# If
--     @pipeline@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @pipeline@ /must/ be a valid 'Vulkan.Core10.Handles.Pipeline' handle
--
-- -   #VUID-VkPipelineBinaryCreateInfoKHR-pPipelineCreateInfo-parameter#
--     If @pPipelineCreateInfo@ is not @NULL@, @pPipelineCreateInfo@ /must/
--     be a valid pointer to a valid 'PipelineCreateInfoKHR' structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_pipeline_binary VK_KHR_pipeline_binary>,
-- 'Vulkan.Core10.Handles.Pipeline', 'PipelineBinaryKeysAndDataKHR',
-- 'PipelineCreateInfoKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createPipelineBinariesKHR'
data PipelineBinaryCreateInfoKHR = PipelineBinaryCreateInfoKHR
  { -- | @pKeysAndDataInfo@ is @NULL@ or a pointer to a
    -- 'PipelineBinaryKeysAndDataKHR' structure that contains keys and data to
    -- create the pipeline binaries from.
    keysAndDataInfo :: Maybe PipelineBinaryKeysAndDataKHR
  , -- | @pipeline@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE' or a
    -- 'Vulkan.Core10.Handles.Pipeline' that contains data to create the
    -- pipeline binaries from.
    pipeline :: Pipeline
  , -- | @pPipelineCreateInfo@ is @NULL@ or a pointer to a
    -- 'PipelineCreateInfoKHR' structure with the pipeline creation info. This
    -- is used to probe the implementation’s internal cache for pipeline
    -- binaries.
    pipelineCreateInfo :: Maybe PipelineCreateInfoKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineBinaryCreateInfoKHR)
#endif
deriving instance Show PipelineBinaryCreateInfoKHR

instance ToCStruct PipelineBinaryCreateInfoKHR where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineBinaryCreateInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_BINARY_CREATE_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pKeysAndDataInfo'' <- case (keysAndDataInfo) of
      Nothing -> pure nullPtr
      Just j -> ContT $ withCStruct (j)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr PipelineBinaryKeysAndDataKHR))) pKeysAndDataInfo''
    lift $ poke ((p `plusPtr` 24 :: Ptr Pipeline)) (pipeline)
    pPipelineCreateInfo'' <- case (pipelineCreateInfo) of
      Nothing -> pure nullPtr
      Just j -> ContT $ withCStruct (j)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr PipelineCreateInfoKHR))) pPipelineCreateInfo''
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_BINARY_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct PipelineBinaryCreateInfoKHR where
  peekCStruct p = do
    pKeysAndDataInfo <- peek @(Ptr PipelineBinaryKeysAndDataKHR) ((p `plusPtr` 16 :: Ptr (Ptr PipelineBinaryKeysAndDataKHR)))
    pKeysAndDataInfo' <- maybePeek (\j -> peekCStruct @PipelineBinaryKeysAndDataKHR (j)) pKeysAndDataInfo
    pipeline <- peek @Pipeline ((p `plusPtr` 24 :: Ptr Pipeline))
    pPipelineCreateInfo <- peek @(Ptr PipelineCreateInfoKHR) ((p `plusPtr` 32 :: Ptr (Ptr PipelineCreateInfoKHR)))
    pPipelineCreateInfo' <- maybePeek (\j -> peekCStruct @PipelineCreateInfoKHR (j)) pPipelineCreateInfo
    pure $ PipelineBinaryCreateInfoKHR
             pKeysAndDataInfo' pipeline pPipelineCreateInfo'

instance Zero PipelineBinaryCreateInfoKHR where
  zero = PipelineBinaryCreateInfoKHR
           Nothing
           zero
           Nothing


-- | VkPipelineBinaryHandlesInfoKHR - Structure containing newly created
-- pipeline binaries
--
-- = Description
--
-- If @pPipelineBinaries@ is @NULL@, the number of binaries that would be
-- created is returned in @pipelineBinaryCount@. Otherwise,
-- @pipelineBinaryCount@ /must/ be the number of entries in the
-- @pPipelineBinaries@ array, and on return from
-- 'createPipelineBinariesKHR' @pipelineBinaryCount@ is overwritten with
-- the number of handles actually written to @pPipelineBinaries@. If the
-- value of @pipelineBinaryCount@ is less than the number of binaries that
-- would have been created, at most @pipelineBinaryCount@ handles will be
-- written to @pPipelineBinaries@ and
-- 'Vulkan.Core10.Enums.Result.INCOMPLETE' will be returned instead of
-- 'Vulkan.Core10.Enums.Result.SUCCESS', to indicate that
-- @pPipelineBinaries@ was not large enough to create all the binaries.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineBinaryHandlesInfoKHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_BINARY_HANDLES_INFO_KHR'
--
-- -   #VUID-VkPipelineBinaryHandlesInfoKHR-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkPipelineBinaryHandlesInfoKHR-pPipelineBinaries-parameter# If
--     @pipelineBinaryCount@ is not @0@, and @pPipelineBinaries@ is not
--     @NULL@, @pPipelineBinaries@ /must/ be a valid pointer to an array of
--     @pipelineBinaryCount@ 'Vulkan.Extensions.Handles.PipelineBinaryKHR'
--     handles
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_pipeline_binary VK_KHR_pipeline_binary>,
-- 'Vulkan.Extensions.Handles.PipelineBinaryKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createPipelineBinariesKHR'
data PipelineBinaryHandlesInfoKHR = PipelineBinaryHandlesInfoKHR
  { -- | @pipelineBinaryCount@ is the number of binaries associated with this
    -- pipeline or the number of entries in the @pPipelineBinaries@ array.
    pipelineBinaryCount :: Word32
  , -- | @pPipelineBinaries@ is @NULL@ or a pointer to an array of
    -- 'Vulkan.Extensions.Handles.PipelineBinaryKHR' handles in which the
    -- resulting pipeline binaries are returned.
    pipelineBinaries :: Ptr PipelineBinaryKHR
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineBinaryHandlesInfoKHR)
#endif
deriving instance Show PipelineBinaryHandlesInfoKHR

instance ToCStruct PipelineBinaryHandlesInfoKHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineBinaryHandlesInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_BINARY_HANDLES_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (pipelineBinaryCount)
    poke ((p `plusPtr` 24 :: Ptr (Ptr PipelineBinaryKHR))) (pipelineBinaries)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_BINARY_HANDLES_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct PipelineBinaryHandlesInfoKHR where
  peekCStruct p = do
    pipelineBinaryCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pPipelineBinaries <- peek @(Ptr PipelineBinaryKHR) ((p `plusPtr` 24 :: Ptr (Ptr PipelineBinaryKHR)))
    pure $ PipelineBinaryHandlesInfoKHR
             pipelineBinaryCount pPipelineBinaries

instance Storable PipelineBinaryHandlesInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineBinaryHandlesInfoKHR where
  zero = PipelineBinaryHandlesInfoKHR
           zero
           zero


-- | VkPipelineBinaryDataKHR - Structure specifying data and length of a
-- pipeline binary
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_pipeline_binary VK_KHR_pipeline_binary>,
-- 'PipelineBinaryKeysAndDataKHR'
data PipelineBinaryDataKHR = PipelineBinaryDataKHR
  { -- | @dataSize@ is the size of the @pData@ buffer in bytes.
    --
    -- #VUID-VkPipelineBinaryDataKHR-dataSize-arraylength# @dataSize@ /must/ be
    -- greater than @0@
    dataSize :: Word64
  , -- | @pData@ is a pointer to a buffer of @size@ bytes that contains pipeline
    -- binary data obtained from 'getPipelineBinaryDataKHR'.
    --
    -- #VUID-VkPipelineBinaryDataKHR-pData-parameter# @pData@ /must/ be a valid
    -- pointer to an array of @dataSize@ bytes
    data' :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineBinaryDataKHR)
#endif
deriving instance Show PipelineBinaryDataKHR

instance ToCStruct PipelineBinaryDataKHR where
  withCStruct x f = allocaBytes 16 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineBinaryDataKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr CSize)) (CSize (dataSize))
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (data')
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (zero)
    f

instance FromCStruct PipelineBinaryDataKHR where
  peekCStruct p = do
    dataSize <- peek @CSize ((p `plusPtr` 0 :: Ptr CSize))
    pData <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    pure $ PipelineBinaryDataKHR
             (coerce @CSize @Word64 dataSize) pData

instance Storable PipelineBinaryDataKHR where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineBinaryDataKHR where
  zero = PipelineBinaryDataKHR
           zero
           zero


-- | VkPipelineBinaryKeysAndDataKHR - Structure specifying arrays of key and
-- data pairs
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_pipeline_binary VK_KHR_pipeline_binary>,
-- 'PipelineBinaryCreateInfoKHR', 'PipelineBinaryDataKHR',
-- 'PipelineBinaryKeyKHR'
data PipelineBinaryKeysAndDataKHR = PipelineBinaryKeysAndDataKHR
  { -- | @pPipelineBinaryKeys@ is a pointer to an array of 'PipelineBinaryKeyKHR'
    -- structures containing the pipeline binary keys
    --
    -- #VUID-VkPipelineBinaryKeysAndDataKHR-pPipelineBinaryKeys-parameter#
    -- @pPipelineBinaryKeys@ /must/ be a valid pointer to an array of
    -- @binaryCount@ valid 'PipelineBinaryKeyKHR' structures
    pipelineBinaryKeys :: Vector PipelineBinaryKeyKHR
  , -- | @pPipelineBinaryData@ is a pointer to an array of
    -- 'PipelineBinaryDataKHR' structures containing the pipeline binary data
    --
    -- #VUID-VkPipelineBinaryKeysAndDataKHR-pPipelineBinaryData-parameter#
    -- @pPipelineBinaryData@ /must/ be a valid pointer to an array of
    -- @binaryCount@ valid 'PipelineBinaryDataKHR' structures
    pipelineBinaryData :: Vector PipelineBinaryDataKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineBinaryKeysAndDataKHR)
#endif
deriving instance Show PipelineBinaryKeysAndDataKHR

instance ToCStruct PipelineBinaryKeysAndDataKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineBinaryKeysAndDataKHR{..} f = evalContT $ do
    let pPipelineBinaryKeysLength = Data.Vector.length $ (pipelineBinaryKeys)
    lift $ unless ((Data.Vector.length $ (pipelineBinaryData)) == pPipelineBinaryKeysLength) $
      throwIO $ IOError Nothing InvalidArgument "" "pPipelineBinaryData and pPipelineBinaryKeys must have the same length" Nothing Nothing
    lift $ poke ((p `plusPtr` 0 :: Ptr Word32)) ((fromIntegral pPipelineBinaryKeysLength :: Word32))
    pPPipelineBinaryKeys' <- ContT $ allocaBytes @PipelineBinaryKeyKHR ((Data.Vector.length (pipelineBinaryKeys)) * 56)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPPipelineBinaryKeys' `plusPtr` (56 * (i)) :: Ptr PipelineBinaryKeyKHR) (e)) (pipelineBinaryKeys)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr PipelineBinaryKeyKHR))) (pPPipelineBinaryKeys')
    pPPipelineBinaryData' <- ContT $ allocaBytes @PipelineBinaryDataKHR ((Data.Vector.length (pipelineBinaryData)) * 16)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPPipelineBinaryData' `plusPtr` (16 * (i)) :: Ptr PipelineBinaryDataKHR) (e)) (pipelineBinaryData)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr PipelineBinaryDataKHR))) (pPPipelineBinaryData')
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct _ f = f

instance FromCStruct PipelineBinaryKeysAndDataKHR where
  peekCStruct p = do
    binaryCount <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    pPipelineBinaryKeys <- peek @(Ptr PipelineBinaryKeyKHR) ((p `plusPtr` 8 :: Ptr (Ptr PipelineBinaryKeyKHR)))
    pPipelineBinaryKeys' <- generateM (fromIntegral binaryCount) (\i -> peekCStruct @PipelineBinaryKeyKHR ((pPipelineBinaryKeys `advancePtrBytes` (56 * (i)) :: Ptr PipelineBinaryKeyKHR)))
    pPipelineBinaryData <- peek @(Ptr PipelineBinaryDataKHR) ((p `plusPtr` 16 :: Ptr (Ptr PipelineBinaryDataKHR)))
    pPipelineBinaryData' <- generateM (fromIntegral binaryCount) (\i -> peekCStruct @PipelineBinaryDataKHR ((pPipelineBinaryData `advancePtrBytes` (16 * (i)) :: Ptr PipelineBinaryDataKHR)))
    pure $ PipelineBinaryKeysAndDataKHR
             pPipelineBinaryKeys' pPipelineBinaryData'

instance Zero PipelineBinaryKeysAndDataKHR where
  zero = PipelineBinaryKeysAndDataKHR
           mempty
           mempty


-- | VkPipelineBinaryKeyKHR - Structure specifying a key to a pipeline binary
--
-- = Description
--
-- Any returned values beyond the first @keySize@ bytes are undefined.
-- Implementations /must/ return a @keySize@ greater than 0, and
-- less-or-equal to
-- 'Vulkan.Core10.APIConstants.MAX_PIPELINE_BINARY_KEY_SIZE_KHR'.
--
-- Two keys are considered equal if @keySize@ is equal and the first
-- @keySize@ bytes of @key@ compare equal.
--
-- Implementations /may/ return a different @keySize@ for different
-- binaries.
--
-- Implementations /should/ ensure that @keySize@ is large enough to
-- uniquely identify a pipeline binary.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_pipeline_binary VK_KHR_pipeline_binary>,
-- 'PipelineBinaryKeysAndDataKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPipelineBinaryDataKHR', 'getPipelineKeyKHR'
data PipelineBinaryKeyKHR = PipelineBinaryKeyKHR
  { -- | @keySize@ is the size, in bytes, of valid data returned in @key@.
    keySize :: Word32
  , -- | @key@ is a buffer of opaque data specifying a pipeline binary key.
    key :: ByteString
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineBinaryKeyKHR)
#endif
deriving instance Show PipelineBinaryKeyKHR

instance ToCStruct PipelineBinaryKeyKHR where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineBinaryKeyKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_BINARY_KEY_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (keySize)
    pokeFixedLengthByteString ((p `plusPtr` 20 :: Ptr (FixedArray MAX_PIPELINE_BINARY_KEY_SIZE_KHR Word8))) (key)
    f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_BINARY_KEY_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    pokeFixedLengthByteString ((p `plusPtr` 20 :: Ptr (FixedArray MAX_PIPELINE_BINARY_KEY_SIZE_KHR Word8))) (mempty)
    f

instance FromCStruct PipelineBinaryKeyKHR where
  peekCStruct p = do
    keySize <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    key <- peekByteStringFromSizedVectorPtr ((p `plusPtr` 20 :: Ptr (FixedArray MAX_PIPELINE_BINARY_KEY_SIZE_KHR Word8)))
    pure $ PipelineBinaryKeyKHR
             keySize key

instance Storable PipelineBinaryKeyKHR where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineBinaryKeyKHR where
  zero = PipelineBinaryKeyKHR
           zero
           mempty


-- | VkPipelineBinaryInfoKHR - Structure specifying pipeline binaries to use
-- during pipeline creation
--
-- = Description
--
-- If a 'PipelineBinaryInfoKHR' structure with a @binaryCount@ greater than
-- 0 is included in the @pNext@ chain of any @Vk*PipelineCreateInfo@
-- structure when creating a pipeline, implementations /must/ use the data
-- in @pPipelineBinaries@ instead of recalculating it. Any shader module
-- identifiers or shader modules declared in
-- 'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo' instances are
-- ignored.
--
-- If this structure is not included in the @pNext@ chain, it is equivalent
-- to specifying this structure with a @binaryCount@ of @0@.
--
-- == Valid Usage
--
-- -   #VUID-VkPipelineBinaryInfoKHR-binaryCount-09603# @binaryCount@ and
--     the order of the elements in @pPipelineBinaries@ /must/ exactly
--     match that returned by 'createPipelineBinariesKHR' for the matching
--     @Vk*PipelineCreateInfo@ structure and its @pNext@ chain, ignoring
--     the presence of the 'PipelineBinaryInfoKHR' structure, the presence
--     of the
--     'Vulkan.Extensions.VK_KHR_maintenance5.PIPELINE_CREATE_2_CAPTURE_DATA_BIT_KHR'
--     flag, and absence of any shader module identifiers or shader
--     modules, for the same
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#global-pipeline-key global pipeline key>,
--     from either:
--
--     -   'PipelineBinaryCreateInfoKHR'::@pPipelineCreateInfo@, or
--
--     -   'PipelineBinaryCreateInfoKHR'::@pipeline@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineBinaryInfoKHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_BINARY_INFO_KHR'
--
-- -   #VUID-VkPipelineBinaryInfoKHR-pPipelineBinaries-parameter# If
--     @binaryCount@ is not @0@, @pPipelineBinaries@ /must/ be a valid
--     pointer to an array of @binaryCount@ valid
--     'Vulkan.Extensions.Handles.PipelineBinaryKHR' handles
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_pipeline_binary VK_KHR_pipeline_binary>,
-- 'Vulkan.Extensions.Handles.PipelineBinaryKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineBinaryInfoKHR = PipelineBinaryInfoKHR
  { -- | @pPipelineBinaries@ is a pointer to an array of
    -- 'Vulkan.Extensions.Handles.PipelineBinaryKHR' handles.
    pipelineBinaries :: Vector PipelineBinaryKHR }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineBinaryInfoKHR)
#endif
deriving instance Show PipelineBinaryInfoKHR

instance ToCStruct PipelineBinaryInfoKHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineBinaryInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_BINARY_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (pipelineBinaries)) :: Word32))
    pPPipelineBinaries' <- ContT $ allocaBytes @PipelineBinaryKHR ((Data.Vector.length (pipelineBinaries)) * 8)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPPipelineBinaries' `plusPtr` (8 * (i)) :: Ptr PipelineBinaryKHR) (e)) (pipelineBinaries)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr PipelineBinaryKHR))) (pPPipelineBinaries')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_BINARY_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct PipelineBinaryInfoKHR where
  peekCStruct p = do
    binaryCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pPipelineBinaries <- peek @(Ptr PipelineBinaryKHR) ((p `plusPtr` 24 :: Ptr (Ptr PipelineBinaryKHR)))
    pPipelineBinaries' <- generateM (fromIntegral binaryCount) (\i -> peek @PipelineBinaryKHR ((pPipelineBinaries `advancePtrBytes` (8 * (i)) :: Ptr PipelineBinaryKHR)))
    pure $ PipelineBinaryInfoKHR
             pPipelineBinaries'

instance Zero PipelineBinaryInfoKHR where
  zero = PipelineBinaryInfoKHR
           mempty


-- | VkReleaseCapturedPipelineDataInfoKHR - Structure specifying a pipeline
-- whose captured data is to be released
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_pipeline_binary VK_KHR_pipeline_binary>,
-- 'Vulkan.Core10.Handles.Pipeline',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'releaseCapturedPipelineDataKHR'
data ReleaseCapturedPipelineDataInfoKHR = ReleaseCapturedPipelineDataInfoKHR
  { -- | @pipeline@ the handle of the pipeline object to release the data from.
    --
    -- #VUID-VkReleaseCapturedPipelineDataInfoKHR-pipeline-09613# @pipeline@
    -- /must/ have been created with
    -- 'Vulkan.Extensions.VK_KHR_maintenance5.PIPELINE_CREATE_2_CAPTURE_DATA_BIT_KHR'
    --
    -- #VUID-VkReleaseCapturedPipelineDataInfoKHR-pipeline-09618# @pipeline@
    -- /must/ not have been used in a previous call to
    -- 'releaseCapturedPipelineDataKHR'
    --
    -- #VUID-VkReleaseCapturedPipelineDataInfoKHR-pipeline-parameter#
    -- @pipeline@ /must/ be a valid 'Vulkan.Core10.Handles.Pipeline' handle
    pipeline :: Pipeline }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ReleaseCapturedPipelineDataInfoKHR)
#endif
deriving instance Show ReleaseCapturedPipelineDataInfoKHR

instance ToCStruct ReleaseCapturedPipelineDataInfoKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ReleaseCapturedPipelineDataInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RELEASE_CAPTURED_PIPELINE_DATA_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Pipeline)) (pipeline)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RELEASE_CAPTURED_PIPELINE_DATA_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Pipeline)) (zero)
    f

instance FromCStruct ReleaseCapturedPipelineDataInfoKHR where
  peekCStruct p = do
    pipeline <- peek @Pipeline ((p `plusPtr` 16 :: Ptr Pipeline))
    pure $ ReleaseCapturedPipelineDataInfoKHR
             pipeline

instance Storable ReleaseCapturedPipelineDataInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ReleaseCapturedPipelineDataInfoKHR where
  zero = ReleaseCapturedPipelineDataInfoKHR
           zero


-- | VkPipelineBinaryDataInfoKHR - Structure specifying a pipeline binary to
-- retrieve binary data from
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_pipeline_binary VK_KHR_pipeline_binary>,
-- 'Vulkan.Extensions.Handles.PipelineBinaryKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPipelineBinaryDataKHR'
data PipelineBinaryDataInfoKHR = PipelineBinaryDataInfoKHR
  { -- | @pipelineBinary@ is the pipeline binary to get data from.
    --
    -- #VUID-VkPipelineBinaryDataInfoKHR-pipelineBinary-parameter#
    -- @pipelineBinary@ /must/ be a valid
    -- 'Vulkan.Extensions.Handles.PipelineBinaryKHR' handle
    pipelineBinary :: PipelineBinaryKHR }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineBinaryDataInfoKHR)
#endif
deriving instance Show PipelineBinaryDataInfoKHR

instance ToCStruct PipelineBinaryDataInfoKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineBinaryDataInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_BINARY_DATA_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineBinaryKHR)) (pipelineBinary)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_BINARY_DATA_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineBinaryKHR)) (zero)
    f

instance FromCStruct PipelineBinaryDataInfoKHR where
  peekCStruct p = do
    pipelineBinary <- peek @PipelineBinaryKHR ((p `plusPtr` 16 :: Ptr PipelineBinaryKHR))
    pure $ PipelineBinaryDataInfoKHR
             pipelineBinary

instance Storable PipelineBinaryDataInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineBinaryDataInfoKHR where
  zero = PipelineBinaryDataInfoKHR
           zero


-- | VkPipelineCreateInfoKHR - Structure specifying a pipeline createinfo
-- chain
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_pipeline_binary VK_KHR_pipeline_binary>,
-- 'PipelineBinaryCreateInfoKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'getPipelineKeyKHR'
data PipelineCreateInfoKHR = PipelineCreateInfoKHR
  {}
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineCreateInfoKHR)
#endif
deriving instance Show PipelineCreateInfoKHR

instance ToCStruct PipelineCreateInfoKHR where
  withCStruct x f = allocaBytes 16 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineCreateInfoKHR f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct PipelineCreateInfoKHR where
  peekCStruct _ = pure $ PipelineCreateInfoKHR
                           

instance Storable PipelineCreateInfoKHR where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineCreateInfoKHR where
  zero = PipelineCreateInfoKHR
           


-- | VkPhysicalDevicePipelineBinaryFeaturesKHR - Structure describing support
-- for pipeline binaries
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDevicePipelineBinaryFeaturesKHR' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDevicePipelineBinaryFeaturesKHR' /can/ also be used
-- in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_pipeline_binary VK_KHR_pipeline_binary>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePipelineBinaryFeaturesKHR = PhysicalDevicePipelineBinaryFeaturesKHR
  { -- | #features-pipelineBinaries# @pipelineBinaries@ indicates that the
    -- implementation supports
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#pipelines-binaries>.
    pipelineBinaries :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePipelineBinaryFeaturesKHR)
#endif
deriving instance Show PhysicalDevicePipelineBinaryFeaturesKHR

instance ToCStruct PhysicalDevicePipelineBinaryFeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePipelineBinaryFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_BINARY_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (pipelineBinaries))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_BINARY_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDevicePipelineBinaryFeaturesKHR where
  peekCStruct p = do
    pipelineBinaries <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDevicePipelineBinaryFeaturesKHR
             (bool32ToBool pipelineBinaries)

instance Storable PhysicalDevicePipelineBinaryFeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePipelineBinaryFeaturesKHR where
  zero = PhysicalDevicePipelineBinaryFeaturesKHR
           zero


-- | VkDevicePipelineBinaryInternalCacheControlKHR - Structure specifying
-- parameter to disable the internal pipeline cache
--
-- = Description
--
-- If the 'Vulkan.Core10.Device.DeviceCreateInfo'::@pNext@ chain does not
-- include this structure, then @disableInternalCache@ defaults to
-- 'Vulkan.Core10.FundamentalTypes.FALSE'.
--
-- == Valid Usage
--
-- -   #VUID-VkDevicePipelineBinaryInternalCacheControlKHR-disableInternalCache-09602#
--     If
--     'PhysicalDevicePipelineBinaryPropertiesKHR'::@pipelineBinaryInternalCacheControl@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', @disableInternalCache@
--     /must/ be 'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDevicePipelineBinaryInternalCacheControlKHR-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_PIPELINE_BINARY_INTERNAL_CACHE_CONTROL_KHR'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_pipeline_binary VK_KHR_pipeline_binary>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DevicePipelineBinaryInternalCacheControlKHR = DevicePipelineBinaryInternalCacheControlKHR
  { -- | @disableInternalCache@ specifies whether or not to disable the
    -- implementation’s internal pipeline cache.
    disableInternalCache :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DevicePipelineBinaryInternalCacheControlKHR)
#endif
deriving instance Show DevicePipelineBinaryInternalCacheControlKHR

instance ToCStruct DevicePipelineBinaryInternalCacheControlKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DevicePipelineBinaryInternalCacheControlKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_PIPELINE_BINARY_INTERNAL_CACHE_CONTROL_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (disableInternalCache))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_PIPELINE_BINARY_INTERNAL_CACHE_CONTROL_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct DevicePipelineBinaryInternalCacheControlKHR where
  peekCStruct p = do
    disableInternalCache <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ DevicePipelineBinaryInternalCacheControlKHR
             (bool32ToBool disableInternalCache)

instance Storable DevicePipelineBinaryInternalCacheControlKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DevicePipelineBinaryInternalCacheControlKHR where
  zero = DevicePipelineBinaryInternalCacheControlKHR
           zero


-- | VkPhysicalDevicePipelineBinaryPropertiesKHR - Structure describing
-- properties about the pipeline binary implementation
--
-- = Description
--
-- These properties tend to be platform specific and may change depending
-- on external configuration which is outside the scope of this
-- specification. These properties are intended to guide applications when
-- implementations have dedicated caching solutions available. In
-- particular, if the @pipelineBinaryPrefersInternalCache@ limit is
-- exposed, relying on the internal cache may provide some advantage
-- compared to an application-specific solution. An application with its
-- own dedicated solution may still use its own caching system even with
-- this limit exposed.
--
-- If the 'PhysicalDevicePipelineBinaryPropertiesKHR' structure is included
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_pipeline_binary VK_KHR_pipeline_binary>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePipelineBinaryPropertiesKHR = PhysicalDevicePipelineBinaryPropertiesKHR
  { -- | #limits-pipelineBinaryInternalCache# @pipelineBinaryInternalCache@
    -- specifies that the implementation maintains a pipeline cache internal to
    -- the implementation. If this is 'Vulkan.Core10.FundamentalTypes.TRUE',
    -- applications /can/ create pipeline binaries with only a pipeline create
    -- info, and in this case, an implementation /may/ be able to create a
    -- pipeline binary directly without application needing to capture the
    -- binary itself.
    pipelineBinaryInternalCache :: Bool
  , -- | #limits-pipelineBinaryInternalCacheControl#
    -- @pipelineBinaryInternalCacheControl@ specifies whether the driver’s
    -- internal cache /can/ be disabled. If this property is
    -- 'Vulkan.Core10.FundamentalTypes.TRUE'
    -- 'DevicePipelineBinaryInternalCacheControlKHR'::@disableInternalCache@
    -- /can/ be used to disable the driver’s internal cache, allowing an
    -- application to take full control of both memory and disk usage.
    pipelineBinaryInternalCacheControl :: Bool
  , -- | #limits-pipelineBinaryPrefersInternalCache#
    -- @pipelineBinaryPrefersInternalCache@ specifies that the implementation
    -- prefers to maintain an internal cache, and applications /should/ not
    -- store pipeline binaries in their own on-disk caches to avoid increased
    -- on-disk storage requirements. Applications are encouraged to only store
    -- pipeline keys instead, and aim to create pipeline binaries from key
    -- alone on subsequent runs of the application.
    pipelineBinaryPrefersInternalCache :: Bool
  , -- | #limits-pipelineBinaryPrecompiledInternalCache#
    -- @pipelineBinaryPrecompiledInternalCache@ specifies that the
    -- implementation /may/ have pipeline binaries in its internal cache, which
    -- is populated without the application ever having generated that pipeline
    -- itself. Applications /can/ attempt to create binaries without extracting
    -- pipeline binary data from the pipeline prior for a set of pipeline keys,
    -- including from previous runs of the application.
    pipelineBinaryPrecompiledInternalCache :: Bool
  , -- | #limits-pipelineBinaryCompressedData# @pipelineBinaryCompressedData@
    -- specifies that the binary data is already compressed and so applications
    -- /should/ not attempt to compress it.
    pipelineBinaryCompressedData :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePipelineBinaryPropertiesKHR)
#endif
deriving instance Show PhysicalDevicePipelineBinaryPropertiesKHR

instance ToCStruct PhysicalDevicePipelineBinaryPropertiesKHR where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePipelineBinaryPropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_BINARY_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (pipelineBinaryInternalCache))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (pipelineBinaryInternalCacheControl))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (pipelineBinaryPrefersInternalCache))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (pipelineBinaryPrecompiledInternalCache))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (pipelineBinaryCompressedData))
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_BINARY_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDevicePipelineBinaryPropertiesKHR where
  peekCStruct p = do
    pipelineBinaryInternalCache <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pipelineBinaryInternalCacheControl <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pipelineBinaryPrefersInternalCache <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    pipelineBinaryPrecompiledInternalCache <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    pipelineBinaryCompressedData <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    pure $ PhysicalDevicePipelineBinaryPropertiesKHR
             (bool32ToBool pipelineBinaryInternalCache)
             (bool32ToBool pipelineBinaryInternalCacheControl)
             (bool32ToBool pipelineBinaryPrefersInternalCache)
             (bool32ToBool pipelineBinaryPrecompiledInternalCache)
             (bool32ToBool pipelineBinaryCompressedData)

instance Storable PhysicalDevicePipelineBinaryPropertiesKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePipelineBinaryPropertiesKHR where
  zero = PhysicalDevicePipelineBinaryPropertiesKHR
           zero
           zero
           zero
           zero
           zero


type KHR_PIPELINE_BINARY_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_PIPELINE_BINARY_SPEC_VERSION"
pattern KHR_PIPELINE_BINARY_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_PIPELINE_BINARY_SPEC_VERSION = 1


type KHR_PIPELINE_BINARY_EXTENSION_NAME = "VK_KHR_pipeline_binary"

-- No documentation found for TopLevel "VK_KHR_PIPELINE_BINARY_EXTENSION_NAME"
pattern KHR_PIPELINE_BINARY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_PIPELINE_BINARY_EXTENSION_NAME = "VK_KHR_pipeline_binary"

