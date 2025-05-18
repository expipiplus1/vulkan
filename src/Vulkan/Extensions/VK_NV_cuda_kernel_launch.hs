{-# language CPP #-}
-- | = Name
--
-- VK_NV_cuda_kernel_launch - device extension
--
-- == VK_NV_cuda_kernel_launch
--
-- [__Name String__]
--     @VK_NV_cuda_kernel_launch@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     308
--
-- [__Revision__]
--     2
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     None
--
--     -   __This is a /provisional/ extension and /must/ be used with
--         caution. See the
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#boilerplate-provisional-header description>
--         of provisional header files for enablement and stability
--         details.__
--
-- [__API Interactions__]
--
--     -   Interacts with VK_EXT_debug_report
--
-- [__Contact__]
--
--     -   Tristan Lorach
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_cuda_kernel_launch] @tlorach%0A*Here describe the issue or question you have about the VK_NV_cuda_kernel_launch extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-09-30
--
-- [__Contributors__]
--
--     -   Eric Werness, NVIDIA
--
-- == Description
--
-- Interoperability between APIs can sometimes create additional overhead
-- depending on the platform used. This extension targets deployment of
-- existing CUDA kernels via Vulkan, with a way to directly upload PTX
-- kernels and dispatch the kernels from Vulkan’s command buffer without
-- the need to use interoperability between the Vulkan and CUDA contexts.
-- However, we do encourage actual development using the native CUDA
-- runtime for the purpose of debugging and profiling.
--
-- The application will first have to create a CUDA module using
-- 'createCudaModuleNV' then create the CUDA function entry point with
-- 'createCudaFunctionNV'.
--
-- Then in order to dispatch this function, the application will create a
-- command buffer where it will launch the kernel with
-- 'cmdCudaLaunchKernelNV'.
--
-- When done, the application will then destroy the function handle, as
-- well as the CUDA module handle with 'destroyCudaFunctionNV' and
-- 'destroyCudaModuleNV'.
--
-- To reduce the impact of compilation time, this extension offers the
-- capability to return a binary cache from the PTX that was provided. For
-- this, a first query for the required cache size is made with
-- 'getCudaModuleCacheNV' with a @NULL@ pointer to a buffer and with a
-- valid pointer receiving the size; then another call of the same function
-- with a valid pointer to a buffer to retrieve the data. The resulting
-- cache could then be user later for further runs of this application by
-- sending this cache instead of the PTX code (using the same
-- 'createCudaModuleNV'), thus significantly speeding up the initialization
-- of the CUDA module.
--
-- As with 'Vulkan.Core10.Handles.PipelineCache', the binary cache depends
-- on the hardware architecture. The application must assume the cache
-- might fail, and need to handle falling back to the original PTX code as
-- necessary. Most often, the cache will succeed if the same GPU driver and
-- architecture is used between the cache generation from PTX and the use
-- of this cache. In the event of a new driver version, or if using a
-- different GPU architecture, the cache is likely to become invalid.
--
-- == New Object Types
--
-- -   'Vulkan.Extensions.Handles.CudaFunctionNV'
--
-- -   'Vulkan.Extensions.Handles.CudaModuleNV'
--
-- == New Commands
--
-- -   'cmdCudaLaunchKernelNV'
--
-- -   'createCudaFunctionNV'
--
-- -   'createCudaModuleNV'
--
-- -   'destroyCudaFunctionNV'
--
-- -   'destroyCudaModuleNV'
--
-- -   'getCudaModuleCacheNV'
--
-- == New Structures
--
-- -   'CudaFunctionCreateInfoNV'
--
-- -   'CudaLaunchInfoNV'
--
-- -   'CudaModuleCreateInfoNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceCudaKernelLaunchFeaturesNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceCudaKernelLaunchPropertiesNV'
--
-- == New Enum Constants
--
-- -   'NV_CUDA_KERNEL_LAUNCH_EXTENSION_NAME'
--
-- -   'NV_CUDA_KERNEL_LAUNCH_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.ObjectType.ObjectType':
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_CUDA_FUNCTION_NV'
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_CUDA_MODULE_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_CUDA_FUNCTION_CREATE_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_CUDA_LAUNCH_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_CUDA_MODULE_CREATE_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_CUDA_KERNEL_LAUNCH_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_CUDA_KERNEL_LAUNCH_PROPERTIES_NV'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_debug_report VK_EXT_debug_report>
-- is supported:
--
-- -   Extending
--     'Vulkan.Extensions.VK_EXT_debug_report.DebugReportObjectTypeEXT':
--
--     -   'Vulkan.Extensions.VK_EXT_debug_report.DEBUG_REPORT_OBJECT_TYPE_CUDA_FUNCTION_NV_EXT'
--
--     -   'Vulkan.Extensions.VK_EXT_debug_report.DEBUG_REPORT_OBJECT_TYPE_CUDA_MODULE_NV_EXT'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2020-03-01 (Tristan Lorach)
--
-- -   Revision 2, 2020-09-30 (Tristan Lorach)
--
-- == See Also
--
-- 'CudaFunctionCreateInfoNV', 'Vulkan.Extensions.Handles.CudaFunctionNV',
-- 'CudaLaunchInfoNV', 'CudaModuleCreateInfoNV',
-- 'Vulkan.Extensions.Handles.CudaModuleNV',
-- 'PhysicalDeviceCudaKernelLaunchFeaturesNV',
-- 'PhysicalDeviceCudaKernelLaunchPropertiesNV', 'cmdCudaLaunchKernelNV',
-- 'createCudaFunctionNV', 'createCudaModuleNV', 'destroyCudaFunctionNV',
-- 'destroyCudaModuleNV', 'getCudaModuleCacheNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_cuda_kernel_launch Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_cuda_kernel_launch  ( createCudaModuleNV
                                                   , withCudaModuleNV
                                                   , getCudaModuleCacheNV
                                                   , createCudaFunctionNV
                                                   , withCudaFunctionNV
                                                   , destroyCudaModuleNV
                                                   , destroyCudaFunctionNV
                                                   , cmdCudaLaunchKernelNV
                                                   , CudaModuleCreateInfoNV(..)
                                                   , CudaFunctionCreateInfoNV(..)
                                                   , CudaLaunchInfoNV(..)
                                                   , PhysicalDeviceCudaKernelLaunchFeaturesNV(..)
                                                   , PhysicalDeviceCudaKernelLaunchPropertiesNV(..)
                                                   , NV_CUDA_KERNEL_LAUNCH_SPEC_VERSION
                                                   , pattern NV_CUDA_KERNEL_LAUNCH_SPEC_VERSION
                                                   , NV_CUDA_KERNEL_LAUNCH_EXTENSION_NAME
                                                   , pattern NV_CUDA_KERNEL_LAUNCH_EXTENSION_NAME
                                                   , CudaModuleNV(..)
                                                   , CudaFunctionNV(..)
                                                   , DebugReportObjectTypeEXT(..)
                                                   ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.ByteString (packCString)
import Data.ByteString (packCStringLen)
import Data.ByteString (useAsCString)
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
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Extensions.Handles (CudaFunctionNV)
import Vulkan.Extensions.Handles (CudaFunctionNV(..))
import Vulkan.Extensions.Handles (CudaModuleNV)
import Vulkan.Extensions.Handles (CudaModuleNV(..))
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCudaLaunchKernelNV))
import Vulkan.Dynamic (DeviceCmds(pVkCreateCudaFunctionNV))
import Vulkan.Dynamic (DeviceCmds(pVkCreateCudaModuleNV))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyCudaFunctionNV))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyCudaModuleNV))
import Vulkan.Dynamic (DeviceCmds(pVkGetCudaModuleCacheNV))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_CUDA_FUNCTION_CREATE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_CUDA_LAUNCH_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_CUDA_MODULE_CREATE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_CUDA_KERNEL_LAUNCH_FEATURES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_CUDA_KERNEL_LAUNCH_PROPERTIES_NV))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (CudaFunctionNV(..))
import Vulkan.Extensions.Handles (CudaModuleNV(..))
import Vulkan.Extensions.VK_EXT_debug_report (DebugReportObjectTypeEXT(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateCudaModuleNV
  :: FunPtr (Ptr Device_T -> Ptr CudaModuleCreateInfoNV -> Ptr AllocationCallbacks -> Ptr CudaModuleNV -> IO Result) -> Ptr Device_T -> Ptr CudaModuleCreateInfoNV -> Ptr AllocationCallbacks -> Ptr CudaModuleNV -> IO Result

-- | vkCreateCudaModuleNV - Creates a new CUDA module object
--
-- = Description
--
-- Once a CUDA module has been created, the application /may/ create the
-- function entry point, which /must/ refer to one function in the module.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateCudaModuleNV-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateCudaModuleNV-pCreateInfo-parameter# @pCreateInfo@
--     /must/ be a valid pointer to a valid 'CudaModuleCreateInfoNV'
--     structure
--
-- -   #VUID-vkCreateCudaModuleNV-pAllocator-parameter# If @pAllocator@ is
--     not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkCreateCudaModuleNV-pModule-parameter# @pModule@ /must/ be a
--     valid pointer to a 'Vulkan.Extensions.Handles.CudaModuleNV' handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_cuda_kernel_launch VK_NV_cuda_kernel_launch>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'CudaModuleCreateInfoNV', 'Vulkan.Extensions.Handles.CudaModuleNV',
-- 'Vulkan.Core10.Handles.Device'
createCudaModuleNV :: forall io
                    . (MonadIO io)
                   => -- | @device@ is the logical device that creates the shader module.
                      Device
                   -> -- | @pCreateInfo@ is a pointer to a 'CudaModuleCreateInfoNV' structure.
                      CudaModuleCreateInfoNV
                   -> -- | @pAllocator@ controls host memory allocation as described in the
                      -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                      -- chapter.
                      ("allocator" ::: Maybe AllocationCallbacks)
                   -> io (CudaModuleNV)
createCudaModuleNV device createInfo allocator = liftIO . evalContT $ do
  let vkCreateCudaModuleNVPtr = pVkCreateCudaModuleNV (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCreateCudaModuleNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateCudaModuleNV is null" Nothing Nothing
  let vkCreateCudaModuleNV' = mkVkCreateCudaModuleNV vkCreateCudaModuleNVPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPModule <- ContT $ bracket (callocBytes @CudaModuleNV 8) free
  r <- lift $ traceAroundEvent "vkCreateCudaModuleNV" (vkCreateCudaModuleNV'
                                                         (deviceHandle (device))
                                                         pCreateInfo
                                                         pAllocator
                                                         (pPModule))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pModule <- lift $ peek @CudaModuleNV pPModule
  pure $ (pModule)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createCudaModuleNV' and 'destroyCudaModuleNV'
--
-- To ensure that 'destroyCudaModuleNV' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withCudaModuleNV :: forall io r . MonadIO io => Device -> CudaModuleCreateInfoNV -> Maybe AllocationCallbacks -> (io CudaModuleNV -> (CudaModuleNV -> io ()) -> r) -> r
withCudaModuleNV device pCreateInfo pAllocator b =
  b (createCudaModuleNV device pCreateInfo pAllocator)
    (\(o0) -> destroyCudaModuleNV device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetCudaModuleCacheNV
  :: FunPtr (Ptr Device_T -> CudaModuleNV -> Ptr CSize -> Ptr () -> IO Result) -> Ptr Device_T -> CudaModuleNV -> Ptr CSize -> Ptr () -> IO Result

-- | vkGetCudaModuleCacheNV - Get CUDA module cache
--
-- = Description
--
-- If @pCacheData@ is @NULL@, then the size of the binary cache, in bytes,
-- is returned in @pCacheSize@. Otherwise, @pCacheSize@ /must/ point to a
-- variable set by the user to the size of the buffer, in bytes, pointed to
-- by @pCacheData@, and on return the variable is overwritten with the
-- amount of data actually written to @pCacheData@. If @pCacheSize@ is less
-- than the size of the binary shader code, nothing is written to
-- @pCacheData@, and 'Vulkan.Core10.Enums.Result.INCOMPLETE' will be
-- returned instead of 'Vulkan.Core10.Enums.Result.SUCCESS'.
--
-- The returned cache /may/ then be used later for further initialization
-- of the CUDA module, by sending this cache /instead/ of the PTX code when
-- using 'createCudaModuleNV'.
--
-- Note
--
-- Using the binary cache instead of the original PTX code /should/
-- significantly speed up initialization of the CUDA module, given that the
-- whole compilation and validation will not be necessary.
--
-- As with 'Vulkan.Core10.Handles.PipelineCache', the binary cache depends
-- on the specific implementation. The application /must/ assume the cache
-- upload might fail in many circumstances and thus /may/ have to get ready
-- for falling back to the original PTX code if necessary. Most often, the
-- cache /may/ succeed if the same device driver and architecture is used
-- between the cache generation from PTX and the use of this cache. In the
-- event of a new driver version or if using a different device
-- architecture, this cache /may/ become invalid.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetCudaModuleCacheNV-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetCudaModuleCacheNV-module-parameter# @module@ /must/ be a
--     valid 'Vulkan.Extensions.Handles.CudaModuleNV' handle
--
-- -   #VUID-vkGetCudaModuleCacheNV-pCacheSize-parameter# @pCacheSize@
--     /must/ be a valid pointer to a @size_t@ value
--
-- -   #VUID-vkGetCudaModuleCacheNV-pCacheData-parameter# If the value
--     referenced by @pCacheSize@ is not @0@, and @pCacheData@ is not
--     @NULL@, @pCacheData@ /must/ be a valid pointer to an array of
--     @pCacheSize@ bytes
--
-- -   #VUID-vkGetCudaModuleCacheNV-module-parent# @module@ /must/ have
--     been created, allocated, or retrieved from @device@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_cuda_kernel_launch VK_NV_cuda_kernel_launch>,
-- 'Vulkan.Extensions.Handles.CudaModuleNV', 'Vulkan.Core10.Handles.Device'
getCudaModuleCacheNV :: forall io
                      . (MonadIO io)
                     => -- | @device@ is the logical device that destroys the Function.
                        Device
                     -> -- | @module@ is the CUDA module.
                        CudaModuleNV
                     -> io (Result, ("cacheData" ::: ByteString))
getCudaModuleCacheNV device module' = liftIO . evalContT $ do
  let vkGetCudaModuleCacheNVPtr = pVkGetCudaModuleCacheNV (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetCudaModuleCacheNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetCudaModuleCacheNV is null" Nothing Nothing
  let vkGetCudaModuleCacheNV' = mkVkGetCudaModuleCacheNV vkGetCudaModuleCacheNVPtr
  let device' = deviceHandle (device)
  pPCacheSize <- ContT $ bracket (callocBytes @CSize 8) free
  r <- lift $ traceAroundEvent "vkGetCudaModuleCacheNV" (vkGetCudaModuleCacheNV'
                                                           device'
                                                           (module')
                                                           (pPCacheSize)
                                                           (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pCacheSize <- lift $ peek @CSize pPCacheSize
  pPCacheData <- ContT $ bracket (callocBytes @(()) (fromIntegral ((coerce @CSize @Word64 pCacheSize)))) free
  r' <- lift $ traceAroundEvent "vkGetCudaModuleCacheNV" (vkGetCudaModuleCacheNV'
                                                            device'
                                                            (module')
                                                            (pPCacheSize)
                                                            (pPCacheData))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pCacheSize'' <- lift $ peek @CSize pPCacheSize
  pCacheData' <- lift $ packCStringLen  ( castPtr @() @CChar pPCacheData
                                        , (fromIntegral ((coerce @CSize @Word64 pCacheSize''))) )
  pure $ ((r'), pCacheData')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateCudaFunctionNV
  :: FunPtr (Ptr Device_T -> Ptr CudaFunctionCreateInfoNV -> Ptr AllocationCallbacks -> Ptr CudaFunctionNV -> IO Result) -> Ptr Device_T -> Ptr CudaFunctionCreateInfoNV -> Ptr AllocationCallbacks -> Ptr CudaFunctionNV -> IO Result

-- | vkCreateCudaFunctionNV - Creates a new CUDA function object
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateCudaFunctionNV-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateCudaFunctionNV-pCreateInfo-parameter# @pCreateInfo@
--     /must/ be a valid pointer to a valid 'CudaFunctionCreateInfoNV'
--     structure
--
-- -   #VUID-vkCreateCudaFunctionNV-pAllocator-parameter# If @pAllocator@
--     is not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkCreateCudaFunctionNV-pFunction-parameter# @pFunction@ /must/
--     be a valid pointer to a 'Vulkan.Extensions.Handles.CudaFunctionNV'
--     handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_cuda_kernel_launch VK_NV_cuda_kernel_launch>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'CudaFunctionCreateInfoNV', 'Vulkan.Extensions.Handles.CudaFunctionNV',
-- 'Vulkan.Core10.Handles.Device'
createCudaFunctionNV :: forall io
                      . (MonadIO io)
                     => -- | @device@ is the logical device that creates the shader module.
                        Device
                     -> -- | @pCreateInfo@ is a pointer to a 'CudaFunctionCreateInfoNV' structure.
                        CudaFunctionCreateInfoNV
                     -> -- | @pAllocator@ controls host memory allocation as described in the
                        -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                        -- chapter.
                        ("allocator" ::: Maybe AllocationCallbacks)
                     -> io (CudaFunctionNV)
createCudaFunctionNV device createInfo allocator = liftIO . evalContT $ do
  let vkCreateCudaFunctionNVPtr = pVkCreateCudaFunctionNV (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCreateCudaFunctionNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateCudaFunctionNV is null" Nothing Nothing
  let vkCreateCudaFunctionNV' = mkVkCreateCudaFunctionNV vkCreateCudaFunctionNVPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPFunction <- ContT $ bracket (callocBytes @CudaFunctionNV 8) free
  r <- lift $ traceAroundEvent "vkCreateCudaFunctionNV" (vkCreateCudaFunctionNV'
                                                           (deviceHandle (device))
                                                           pCreateInfo
                                                           pAllocator
                                                           (pPFunction))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pFunction <- lift $ peek @CudaFunctionNV pPFunction
  pure $ (pFunction)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createCudaFunctionNV' and 'destroyCudaFunctionNV'
--
-- To ensure that 'destroyCudaFunctionNV' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withCudaFunctionNV :: forall io r . MonadIO io => Device -> CudaFunctionCreateInfoNV -> Maybe AllocationCallbacks -> (io CudaFunctionNV -> (CudaFunctionNV -> io ()) -> r) -> r
withCudaFunctionNV device pCreateInfo pAllocator b =
  b (createCudaFunctionNV device pCreateInfo pAllocator)
    (\(o0) -> destroyCudaFunctionNV device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyCudaModuleNV
  :: FunPtr (Ptr Device_T -> CudaModuleNV -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> CudaModuleNV -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyCudaModuleNV - Destroy a CUDA module
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroyCudaModuleNV-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDestroyCudaModuleNV-module-parameter# @module@ /must/ be a
--     valid 'Vulkan.Extensions.Handles.CudaModuleNV' handle
--
-- -   #VUID-vkDestroyCudaModuleNV-pAllocator-parameter# If @pAllocator@ is
--     not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkDestroyCudaModuleNV-module-parent# @module@ /must/ have been
--     created, allocated, or retrieved from @device@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_cuda_kernel_launch VK_NV_cuda_kernel_launch>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Extensions.Handles.CudaModuleNV', 'Vulkan.Core10.Handles.Device'
destroyCudaModuleNV :: forall io
                     . (MonadIO io)
                    => -- | @device@ is the logical device that destroys the shader module.
                       Device
                    -> -- | @module@ is the handle of the CUDA module to destroy.
                       CudaModuleNV
                    -> -- | @pAllocator@ controls host memory allocation as described in the
                       -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                       -- chapter.
                       ("allocator" ::: Maybe AllocationCallbacks)
                    -> io ()
destroyCudaModuleNV device module' allocator = liftIO . evalContT $ do
  let vkDestroyCudaModuleNVPtr = pVkDestroyCudaModuleNV (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkDestroyCudaModuleNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyCudaModuleNV is null" Nothing Nothing
  let vkDestroyCudaModuleNV' = mkVkDestroyCudaModuleNV vkDestroyCudaModuleNVPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyCudaModuleNV" (vkDestroyCudaModuleNV'
                                                     (deviceHandle (device))
                                                     (module')
                                                     pAllocator)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyCudaFunctionNV
  :: FunPtr (Ptr Device_T -> CudaFunctionNV -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> CudaFunctionNV -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyCudaFunctionNV - Destroy a CUDA function
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroyCudaFunctionNV-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDestroyCudaFunctionNV-function-parameter# @function@ /must/
--     be a valid 'Vulkan.Extensions.Handles.CudaFunctionNV' handle
--
-- -   #VUID-vkDestroyCudaFunctionNV-pAllocator-parameter# If @pAllocator@
--     is not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkDestroyCudaFunctionNV-function-parent# @function@ /must/
--     have been created, allocated, or retrieved from @device@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_cuda_kernel_launch VK_NV_cuda_kernel_launch>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Extensions.Handles.CudaFunctionNV',
-- 'Vulkan.Core10.Handles.Device'
destroyCudaFunctionNV :: forall io
                       . (MonadIO io)
                      => -- | @device@ is the logical device that destroys the Function.
                         Device
                      -> -- | @function@ is the handle of the CUDA function to destroy.
                         CudaFunctionNV
                      -> -- | @pAllocator@ controls host memory allocation as described in the
                         -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                         -- chapter.
                         ("allocator" ::: Maybe AllocationCallbacks)
                      -> io ()
destroyCudaFunctionNV device function allocator = liftIO . evalContT $ do
  let vkDestroyCudaFunctionNVPtr = pVkDestroyCudaFunctionNV (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkDestroyCudaFunctionNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyCudaFunctionNV is null" Nothing Nothing
  let vkDestroyCudaFunctionNV' = mkVkDestroyCudaFunctionNV vkDestroyCudaFunctionNVPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyCudaFunctionNV" (vkDestroyCudaFunctionNV'
                                                       (deviceHandle (device))
                                                       (function)
                                                       pAllocator)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCudaLaunchKernelNV
  :: FunPtr (Ptr CommandBuffer_T -> Ptr CudaLaunchInfoNV -> IO ()) -> Ptr CommandBuffer_T -> Ptr CudaLaunchInfoNV -> IO ()

-- | vkCmdCudaLaunchKernelNV - Dispatch compute work items
--
-- = Description
--
-- When the command is executed, a global workgroup consisting of
-- @gridDimX@ × @gridDimY@ × @gridDimZ@ local workgroups is assembled.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdCudaLaunchKernelNV-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdCudaLaunchKernelNV-pLaunchInfo-parameter# @pLaunchInfo@
--     /must/ be a valid pointer to a valid 'CudaLaunchInfoNV' structure
--
-- -   #VUID-vkCmdCudaLaunchKernelNV-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdCudaLaunchKernelNV-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   #VUID-vkCmdCudaLaunchKernelNV-videocoding# This command /must/ only
--     be called outside of a video coding scope
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_cuda_kernel_launch VK_NV_cuda_kernel_launch>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'CudaLaunchInfoNV'
cmdCudaLaunchKernelNV :: forall io
                       . (MonadIO io)
                      => -- | @commandBuffer@ is the command buffer into which the command will be
                         -- recorded.
                         CommandBuffer
                      -> -- | @pLaunchInfo@ is a pointer to a 'CudaLaunchInfoNV' structure in which
                         -- the grid (similar to workgroup) dimension, function handle and related
                         -- arguments are defined.
                         CudaLaunchInfoNV
                      -> io ()
cmdCudaLaunchKernelNV commandBuffer launchInfo = liftIO . evalContT $ do
  let vkCmdCudaLaunchKernelNVPtr = pVkCmdCudaLaunchKernelNV (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdCudaLaunchKernelNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCudaLaunchKernelNV is null" Nothing Nothing
  let vkCmdCudaLaunchKernelNV' = mkVkCmdCudaLaunchKernelNV vkCmdCudaLaunchKernelNVPtr
  pLaunchInfo <- ContT $ withCStruct (launchInfo)
  lift $ traceAroundEvent "vkCmdCudaLaunchKernelNV" (vkCmdCudaLaunchKernelNV'
                                                       (commandBufferHandle (commandBuffer))
                                                       pLaunchInfo)
  pure $ ()


-- | VkCudaModuleCreateInfoNV - Structure specifying the parameters to create
-- a CUDA Module
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_cuda_kernel_launch VK_NV_cuda_kernel_launch>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'createCudaModuleNV'
data CudaModuleCreateInfoNV = CudaModuleCreateInfoNV
  { -- | @dataSize@ is the length of the @pData@ array.
    --
    -- #VUID-VkCudaModuleCreateInfoNV-dataSize-09413# @dataSize@ /must/ be the
    -- total size in bytes of the PTX files or binary cache passed to @pData@.
    --
    -- #VUID-VkCudaModuleCreateInfoNV-dataSize-arraylength# @dataSize@ /must/
    -- be greater than @0@
    dataSize :: Word64
  , -- | @pData@ is a pointer to CUDA code
    --
    -- #VUID-VkCudaModuleCreateInfoNV-pData-parameter# @pData@ /must/ be a
    -- valid pointer to an array of @dataSize@ bytes
    data' :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CudaModuleCreateInfoNV)
#endif
deriving instance Show CudaModuleCreateInfoNV

instance ToCStruct CudaModuleCreateInfoNV where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CudaModuleCreateInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_CUDA_MODULE_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CSize)) (CSize (dataSize))
    poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (data')
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_CUDA_MODULE_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (zero)
    f

instance FromCStruct CudaModuleCreateInfoNV where
  peekCStruct p = do
    dataSize <- peek @CSize ((p `plusPtr` 16 :: Ptr CSize))
    pData <- peek @(Ptr ()) ((p `plusPtr` 24 :: Ptr (Ptr ())))
    pure $ CudaModuleCreateInfoNV
             (coerce @CSize @Word64 dataSize) pData

instance Storable CudaModuleCreateInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CudaModuleCreateInfoNV where
  zero = CudaModuleCreateInfoNV
           zero
           zero


-- | VkCudaFunctionCreateInfoNV - Structure specifying the parameters to
-- create a CUDA Function
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_cuda_kernel_launch VK_NV_cuda_kernel_launch>,
-- 'Vulkan.Extensions.Handles.CudaModuleNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createCudaFunctionNV'
data CudaFunctionCreateInfoNV = CudaFunctionCreateInfoNV
  { -- | @module@ is the CUDA 'Vulkan.Extensions.Handles.CudaModuleNV' module in
    -- which the function resides.
    --
    -- #VUID-VkCudaFunctionCreateInfoNV-module-parameter# @module@ /must/ be a
    -- valid 'Vulkan.Extensions.Handles.CudaModuleNV' handle
    module' :: CudaModuleNV
  , -- | @pName@ is a null-terminated UTF-8 string containing the name of the
    -- shader entry point for this stage.
    --
    -- #VUID-VkCudaFunctionCreateInfoNV-pName-parameter# @pName@ /must/ be a
    -- null-terminated UTF-8 string
    name :: ByteString
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CudaFunctionCreateInfoNV)
#endif
deriving instance Show CudaFunctionCreateInfoNV

instance ToCStruct CudaFunctionCreateInfoNV where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CudaFunctionCreateInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_CUDA_FUNCTION_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr CudaModuleNV)) (module')
    pName'' <- ContT $ useAsCString (name)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr CChar))) pName''
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_CUDA_FUNCTION_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr CudaModuleNV)) (zero)
    pName'' <- ContT $ useAsCString (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr CChar))) pName''
    lift $ f

instance FromCStruct CudaFunctionCreateInfoNV where
  peekCStruct p = do
    module' <- peek @CudaModuleNV ((p `plusPtr` 16 :: Ptr CudaModuleNV))
    pName <- packCString =<< peek ((p `plusPtr` 24 :: Ptr (Ptr CChar)))
    pure $ CudaFunctionCreateInfoNV
             module' pName

instance Zero CudaFunctionCreateInfoNV where
  zero = CudaFunctionCreateInfoNV
           zero
           mempty


-- | VkCudaLaunchInfoNV - Structure specifying the parameters to launch a
-- CUDA kernel
--
-- = Description
--
-- Kernel parameters of @function@ are specified via @pParams@, very much
-- the same way as described in
-- <https://docs.nvidia.com/cuda/cuda-driver-api/group__CUDA__EXEC.html#group__CUDA__EXEC_1gb8f3dc3031b40da29d5f9a7139e52e15 cuLaunchKernel>
--
-- If @function@ has N parameters, then @pParams@ /must/ be an array of N
-- pointers and @paramCount@ /must/ be set to N. Each of @kernelParams@[0]
-- through @kernelParams@[N-1] /must/ point to a region of memory from
-- which the actual kernel parameter will be copied. The number of kernel
-- parameters and their offsets and sizes are not specified here as that
-- information is stored in the 'Vulkan.Extensions.Handles.CudaFunctionNV'
-- object.
--
-- The application-owned memory pointed to by @pParams@ and
-- @kernelParams@[0] through @kernelParams@[N-1] are consumed immediately,
-- and /may/ be altered or freed after 'cmdCudaLaunchKernelNV' has
-- returned.
--
-- == Valid Usage
--
-- -   #VUID-VkCudaLaunchInfoNV-gridDimX-09406# @gridDimX@ /must/ be less
--     than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[0]
--
-- -   #VUID-VkCudaLaunchInfoNV-gridDimY-09407# @gridDimY@ /must/ be less
--     than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[1]
--
-- -   #VUID-VkCudaLaunchInfoNV-gridDimZ-09408# @gridDimZ@ /must/ be less
--     than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[2]
--
-- -   #VUID-VkCudaLaunchInfoNV-paramCount-09409# @paramCount@ /must/ be
--     the total amount of parameters listed in the @pParams@ table.
--
-- -   #VUID-VkCudaLaunchInfoNV-pParams-09410# @pParams@ /must/ be a
--     pointer to a table of @paramCount@ parameters, corresponding to the
--     arguments of @function@.
--
-- -   #VUID-VkCudaLaunchInfoNV-extraCount-09411# @extraCount@ must be 0
--
-- -   #VUID-VkCudaLaunchInfoNV-pExtras-09412# @pExtras@ must be NULL
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCudaLaunchInfoNV-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_CUDA_LAUNCH_INFO_NV'
--
-- -   #VUID-VkCudaLaunchInfoNV-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkCudaLaunchInfoNV-function-parameter# @function@ /must/ be a
--     valid 'Vulkan.Extensions.Handles.CudaFunctionNV' handle
--
-- -   #VUID-VkCudaLaunchInfoNV-pParams-parameter# If @paramCount@ is not
--     @0@, and @pParams@ is not @NULL@, @pParams@ /must/ be a valid
--     pointer to an array of @paramCount@ bytes
--
-- -   #VUID-VkCudaLaunchInfoNV-pExtras-parameter# If @extraCount@ is not
--     @0@, and @pExtras@ is not @NULL@, @pExtras@ /must/ be a valid
--     pointer to an array of @extraCount@ bytes
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_cuda_kernel_launch VK_NV_cuda_kernel_launch>,
-- 'Vulkan.Extensions.Handles.CudaFunctionNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdCudaLaunchKernelNV'
data CudaLaunchInfoNV = CudaLaunchInfoNV
  { -- | @function@ is the CUDA-Driver handle to the function being launched.
    function :: CudaFunctionNV
  , -- | @gridDimX@ is the number of local workgroups to dispatch in the X
    -- dimension. It must be less than or equal to
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[0]
    gridDimX :: Word32
  , -- | @gridDimY@ is the number of local workgroups to dispatch in the Y
    -- dimension. It must be less than or equal to
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[1]
    gridDimY :: Word32
  , -- | @gridDimZ@ is the number of local workgroups to dispatch in the Z
    -- dimension. It must be less than or equal to
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[2]
    gridDimZ :: Word32
  , -- | @blockDimX@ is block size in the X dimension.
    blockDimX :: Word32
  , -- | @blockDimY@ is block size in the Y dimension.
    blockDimY :: Word32
  , -- | @blockDimZ@ is block size in the Z dimension.
    blockDimZ :: Word32
  , -- | @sharedMemBytes@ is the dynamic shared-memory size per thread block in
    -- bytes.
    sharedMemBytes :: Word32
  , -- | @paramCount@ is the length of the @pParams@ table.
    paramCount :: Word64
  , -- | @pParams@ is a pointer to an array of @paramCount@ pointers,
    -- corresponding to the arguments of @function@.
    params :: Vector (Ptr ())
  , -- | @extraCount@ is reserved for future use.
    extraCount :: Word64
  , -- | @pExtras@ is reserved for future use.
    extras :: Vector (Ptr ())
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CudaLaunchInfoNV)
#endif
deriving instance Show CudaLaunchInfoNV

instance ToCStruct CudaLaunchInfoNV where
  withCStruct x f = allocaBytes 88 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CudaLaunchInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_CUDA_LAUNCH_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr CudaFunctionNV)) (function)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (gridDimX)
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) (gridDimY)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) (gridDimZ)
    lift $ poke ((p `plusPtr` 36 :: Ptr Word32)) (blockDimX)
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) (blockDimY)
    lift $ poke ((p `plusPtr` 44 :: Ptr Word32)) (blockDimZ)
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) (sharedMemBytes)
    let pParamsLength = Data.Vector.length $ (params)
    paramCount'' <- lift $ if (paramCount) == 0
      then pure $ fromIntegral pParamsLength
      else do
        unless (fromIntegral pParamsLength == (paramCount) || pParamsLength == 0) $
          throwIO $ IOError Nothing InvalidArgument "" "pParams must be empty or have 'paramCount' elements" Nothing Nothing
        pure (paramCount)
    lift $ poke ((p `plusPtr` 56 :: Ptr CSize)) (fromIntegral (paramCount''))
    pPParams' <- ContT $ allocaBytes @(Ptr ()) ((Data.Vector.length (params)) * 8)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPParams' `plusPtr` (8 * (i)) :: Ptr (Ptr ())) (e)) (params)
    lift $ poke ((p `plusPtr` 64 :: Ptr (Ptr (Ptr ())))) (pPParams')
    let pExtrasLength = Data.Vector.length $ (extras)
    extraCount'' <- lift $ if (extraCount) == 0
      then pure $ fromIntegral pExtrasLength
      else do
        unless (fromIntegral pExtrasLength == (extraCount) || pExtrasLength == 0) $
          throwIO $ IOError Nothing InvalidArgument "" "pExtras must be empty or have 'extraCount' elements" Nothing Nothing
        pure (extraCount)
    lift $ poke ((p `plusPtr` 72 :: Ptr CSize)) (fromIntegral (extraCount''))
    pPExtras' <- ContT $ allocaBytes @(Ptr ()) ((Data.Vector.length (extras)) * 8)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPExtras' `plusPtr` (8 * (i)) :: Ptr (Ptr ())) (e)) (extras)
    lift $ poke ((p `plusPtr` 80 :: Ptr (Ptr (Ptr ())))) (pPExtras')
    lift $ f
  cStructSize = 88
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_CUDA_LAUNCH_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CudaFunctionNV)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    f

instance FromCStruct CudaLaunchInfoNV where
  peekCStruct p = do
    function <- peek @CudaFunctionNV ((p `plusPtr` 16 :: Ptr CudaFunctionNV))
    gridDimX <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    gridDimY <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    gridDimZ <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    blockDimX <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    blockDimY <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    blockDimZ <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    sharedMemBytes <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    paramCount <- peek @CSize ((p `plusPtr` 56 :: Ptr CSize))
    let paramCount' = coerce @CSize @Word64 paramCount
    pParams <- peek @(Ptr (Ptr ())) ((p `plusPtr` 64 :: Ptr (Ptr (Ptr ()))))
    pParams' <- generateM (fromIntegral paramCount') (\i -> peek @(Ptr ()) ((pParams `advancePtrBytes` (8 * (i)) :: Ptr (Ptr ()))))
    extraCount <- peek @CSize ((p `plusPtr` 72 :: Ptr CSize))
    let extraCount' = coerce @CSize @Word64 extraCount
    pExtras <- peek @(Ptr (Ptr ())) ((p `plusPtr` 80 :: Ptr (Ptr (Ptr ()))))
    pExtras' <- generateM (fromIntegral extraCount') (\i -> peek @(Ptr ()) ((pExtras `advancePtrBytes` (8 * (i)) :: Ptr (Ptr ()))))
    pure $ CudaLaunchInfoNV
             function
             gridDimX
             gridDimY
             gridDimZ
             blockDimX
             blockDimY
             blockDimZ
             sharedMemBytes
             paramCount'
             pParams'
             extraCount'
             pExtras'

instance Zero CudaLaunchInfoNV where
  zero = CudaLaunchInfoNV
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           mempty
           zero
           mempty


-- | VkPhysicalDeviceCudaKernelLaunchFeaturesNV - Structure describing
-- whether cuda kernel launch is supported by the implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceCudaKernelLaunchFeaturesNV' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceCudaKernelLaunchFeaturesNV' /can/ also be used
-- in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_cuda_kernel_launch VK_NV_cuda_kernel_launch>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceCudaKernelLaunchFeaturesNV = PhysicalDeviceCudaKernelLaunchFeaturesNV
  { -- | #features-cudaKernelLaunchFeatures# @cudaKernelLaunchFeatures@ is
    -- non-zero if cuda kernel launch is supported.
    cudaKernelLaunchFeatures :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceCudaKernelLaunchFeaturesNV)
#endif
deriving instance Show PhysicalDeviceCudaKernelLaunchFeaturesNV

instance ToCStruct PhysicalDeviceCudaKernelLaunchFeaturesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceCudaKernelLaunchFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_CUDA_KERNEL_LAUNCH_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (cudaKernelLaunchFeatures))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_CUDA_KERNEL_LAUNCH_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceCudaKernelLaunchFeaturesNV where
  peekCStruct p = do
    cudaKernelLaunchFeatures <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceCudaKernelLaunchFeaturesNV
             (bool32ToBool cudaKernelLaunchFeatures)

instance Storable PhysicalDeviceCudaKernelLaunchFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceCudaKernelLaunchFeaturesNV where
  zero = PhysicalDeviceCudaKernelLaunchFeaturesNV
           zero


-- | VkPhysicalDeviceCudaKernelLaunchPropertiesNV - Structure describing the
-- compute capability version available
--
-- = Members
--
-- The members of the 'PhysicalDeviceCudaKernelLaunchPropertiesNV'
-- structure describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceCudaKernelLaunchPropertiesNV' structure is
-- included in the @pNext@ chain of the
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_cuda_kernel_launch VK_NV_cuda_kernel_launch>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceCudaKernelLaunchPropertiesNV = PhysicalDeviceCudaKernelLaunchPropertiesNV
  { -- | #limits-computeCapabilityMinor# @computeCapabilityMinor@ indicates the
    -- minor version number of the compute code.
    computeCapabilityMinor :: Word32
  , -- | #limits-computeCapabilityMajor# @computeCapabilityMajor@ indicates the
    -- major version number of the compute code.
    computeCapabilityMajor :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceCudaKernelLaunchPropertiesNV)
#endif
deriving instance Show PhysicalDeviceCudaKernelLaunchPropertiesNV

instance ToCStruct PhysicalDeviceCudaKernelLaunchPropertiesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceCudaKernelLaunchPropertiesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_CUDA_KERNEL_LAUNCH_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (computeCapabilityMinor)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (computeCapabilityMajor)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_CUDA_KERNEL_LAUNCH_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceCudaKernelLaunchPropertiesNV where
  peekCStruct p = do
    computeCapabilityMinor <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    computeCapabilityMajor <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pure $ PhysicalDeviceCudaKernelLaunchPropertiesNV
             computeCapabilityMinor computeCapabilityMajor

instance Storable PhysicalDeviceCudaKernelLaunchPropertiesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceCudaKernelLaunchPropertiesNV where
  zero = PhysicalDeviceCudaKernelLaunchPropertiesNV
           zero
           zero


type NV_CUDA_KERNEL_LAUNCH_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_NV_CUDA_KERNEL_LAUNCH_SPEC_VERSION"
pattern NV_CUDA_KERNEL_LAUNCH_SPEC_VERSION :: forall a . Integral a => a
pattern NV_CUDA_KERNEL_LAUNCH_SPEC_VERSION = 2


type NV_CUDA_KERNEL_LAUNCH_EXTENSION_NAME = "VK_NV_cuda_kernel_launch"

-- No documentation found for TopLevel "VK_NV_CUDA_KERNEL_LAUNCH_EXTENSION_NAME"
pattern NV_CUDA_KERNEL_LAUNCH_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_CUDA_KERNEL_LAUNCH_EXTENSION_NAME = "VK_NV_cuda_kernel_launch"

