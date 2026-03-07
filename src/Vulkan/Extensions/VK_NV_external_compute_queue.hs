{-# language CPP #-}
-- | = Name
--
-- VK_NV_external_compute_queue - device extension
--
-- = VK_NV_external_compute_queue
--
-- [__Name String__]
--     @VK_NV_external_compute_queue@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     557
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     None
--
-- [__Contact__]
--
--     -   Chris Lentini
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_external_compute_queue] @clentini%0A*Here describe the issue or question you have about the VK_NV_external_compute_queue extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_NV_external_compute_queue.adoc VK_NV_external_compute_queue>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-03-24
--
-- [__Contributors__]
--
--     -   Chris Lentini, NVIDIA
--
--     -   Eric Werness, NVIDIA
--
--     -   James Jones, NVIDIA
--
--     -   Jeff Juliano, NVIDIA
--
--     -   Liam Middlebrook, NVIDIA
--
--     -   Lionel Duc, NVIDIA
--
-- == Description
--
-- This extension gives applications the ability to join compatible
-- external compute APIs to a 'Vulkan.Core10.Handles.Device'. In this way,
-- the extension allows an application to achieve simultaneous execution
-- between work submitted from these compatible external APIs and work that
-- has been submitted through the Vulkan API.
--
-- At device creation time, an application /must/ supply a
-- 'ExternalComputeQueueDeviceCreateInfoNV'. This communicates to the
-- implementation the maximum number of external queues that the
-- application /can/ create at once. This information /may/ be used by the
-- implementation to aid in decisions made during device creation.
--
-- After device creation, the function 'createExternalComputeQueueNV' is
-- used by an application to create a new
-- 'Vulkan.Extensions.Handles.ExternalComputeQueueNV' object. The
-- 'Vulkan.Extensions.Handles.ExternalComputeQueueNV' object holds
-- information and reserves resources necessary for a compatible external
-- API to be able to join a 'Vulkan.Core10.Handles.Device'. This
-- information can be queried through the 'getExternalComputeQueueDataNV'
-- function, returning an opaque blob of data which can be passed to
-- compatible external APIs. The application /must/ finally call
-- 'destroyExternalComputeQueueNV' when it is done in order to release the
-- reserved resources.
--
-- This extension introduces a new properties structure,
-- 'PhysicalDeviceExternalComputeQueuePropertiesNV', which can be queried
-- through
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2'.
-- The structure provides information on functional limits to the extension
-- as well as a way of querying the size of the application allocated
-- memory which /must/ be passed to the 'getExternalComputeQueueDataNV'
-- function.
--
-- When creating a 'Vulkan.Extensions.Handles.ExternalComputeQueueNV'
-- through 'createExternalComputeQueueNV', the
-- 'ExternalComputeQueueCreateInfoNV' structure requires an application to
-- supply a 'Vulkan.Core10.Handles.Queue' to aid in external compute queue
-- creation. The supplied 'Vulkan.Core10.Handles.Queue' is a strong
-- scheduling hint about which queue it expects to submit graphics
-- workloads to and with which it expects simultaneous execution of compute
-- workloads submitted through the external API.
--
-- == New Object Types
--
-- -   'Vulkan.Extensions.Handles.ExternalComputeQueueNV'
--
-- == New Commands
--
-- -   'createExternalComputeQueueNV'
--
-- -   'destroyExternalComputeQueueNV'
--
-- -   'getExternalComputeQueueDataNV'
--
-- == New Structures
--
-- -   'ExternalComputeQueueCreateInfoNV'
--
-- -   'ExternalComputeQueueDataParamsNV'
--
-- -   Extending 'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'ExternalComputeQueueDeviceCreateInfoNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceExternalComputeQueuePropertiesNV'
--
-- == New Enum Constants
--
-- -   'NV_EXTERNAL_COMPUTE_QUEUE_EXTENSION_NAME'
--
-- -   'NV_EXTERNAL_COMPUTE_QUEUE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.ObjectType.ObjectType':
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_EXTERNAL_COMPUTE_QUEUE_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXTERNAL_COMPUTE_QUEUE_CREATE_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXTERNAL_COMPUTE_QUEUE_DATA_PARAMS_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXTERNAL_COMPUTE_QUEUE_DEVICE_CREATE_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_COMPUTE_QUEUE_PROPERTIES_NV'
--
-- While the external queue is now a part of a
-- 'Vulkan.Core10.Handles.Device', idling the device through
-- 'Vulkan.Core10.Queue.deviceWaitIdle' does not wait for the external
-- queue. Draining the work on an external queue /must/ be done through its
-- own external API. External queues /must/ be idled before destroying the
-- associated 'Vulkan.Core10.Handles.Device'.
--
-- In general, synchronization and resource sharing between the external
-- API and Vulkan must still be accomplished via existing cross-API interop
-- mechanisms.
--
-- == Version History
--
-- -   Revision 1, 2024-05-20 (Chris Lentini)
--
--     -   Internal revisions
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_NV_external_compute_queue Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_external_compute_queue  ( createExternalComputeQueueNV
                                                       , withExternalComputeQueueNV
                                                       , destroyExternalComputeQueueNV
                                                       , getExternalComputeQueueDataNV
                                                       , ExternalComputeQueueDeviceCreateInfoNV(..)
                                                       , ExternalComputeQueueCreateInfoNV(..)
                                                       , ExternalComputeQueueDataParamsNV(..)
                                                       , PhysicalDeviceExternalComputeQueuePropertiesNV(..)
                                                       , NV_EXTERNAL_COMPUTE_QUEUE_SPEC_VERSION
                                                       , pattern NV_EXTERNAL_COMPUTE_QUEUE_SPEC_VERSION
                                                       , NV_EXTERNAL_COMPUTE_QUEUE_EXTENSION_NAME
                                                       , pattern NV_EXTERNAL_COMPUTE_QUEUE_EXTENSION_NAME
                                                       , ExternalComputeQueueNV(..)
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
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
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
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkCreateExternalComputeQueueNV))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyExternalComputeQueueNV))
import Vulkan.Dynamic (DeviceCmds(pVkGetExternalComputeQueueDataNV))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Extensions.Handles (ExternalComputeQueueNV)
import Vulkan.Extensions.Handles (ExternalComputeQueueNV(..))
import Vulkan.Extensions.Handles (ExternalComputeQueueNV(ExternalComputeQueueNV))
import Vulkan.Extensions.Handles (ExternalComputeQueueNV_T)
import Vulkan.Core10.Handles (Queue_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXTERNAL_COMPUTE_QUEUE_CREATE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXTERNAL_COMPUTE_QUEUE_DATA_PARAMS_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXTERNAL_COMPUTE_QUEUE_DEVICE_CREATE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_COMPUTE_QUEUE_PROPERTIES_NV))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (ExternalComputeQueueNV(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateExternalComputeQueueNV
  :: FunPtr (Ptr Device_T -> Ptr ExternalComputeQueueCreateInfoNV -> Ptr AllocationCallbacks -> Ptr (Ptr ExternalComputeQueueNV_T) -> IO Result) -> Ptr Device_T -> Ptr ExternalComputeQueueCreateInfoNV -> Ptr AllocationCallbacks -> Ptr (Ptr ExternalComputeQueueNV_T) -> IO Result

-- | vkCreateExternalComputeQueueNV - Create an external compute queue for
-- use by a compatible external API.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateExternalComputeQueueNV-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateExternalComputeQueueNV-pCreateInfo-parameter#
--     @pCreateInfo@ /must/ be a valid pointer to a valid
--     'ExternalComputeQueueCreateInfoNV' structure
--
-- -   #VUID-vkCreateExternalComputeQueueNV-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkCreateExternalComputeQueueNV-pExternalQueue-parameter#
--     @pExternalQueue@ /must/ be a valid pointer to a
--     'Vulkan.Extensions.Handles.ExternalComputeQueueNV' handle
--
-- -   #VUID-vkCreateExternalComputeQueueNV-device-queuecount# The device
--     /must/ have been created with at least @1@ queue
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_TOO_MANY_OBJECTS'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_external_compute_queue VK_NV_external_compute_queue>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'ExternalComputeQueueCreateInfoNV',
-- 'Vulkan.Extensions.Handles.ExternalComputeQueueNV'
createExternalComputeQueueNV :: forall io
                              . (MonadIO io)
                             => -- | @device@ is the VkDevice that the external queue will be a part of.
                                Device
                             -> -- | @pCreateInfo@ is a pointer to a 'ExternalComputeQueueCreateInfoNV'
                                -- structure specifying configuration info for creating the external queue.
                                ExternalComputeQueueCreateInfoNV
                             -> -- | @pAllocator@ controls host memory allocation as described in the
                                -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#memory-allocation Memory Allocation>
                                -- chapter.
                                ("allocator" ::: Maybe AllocationCallbacks)
                             -> io (("externalQueue" ::: ExternalComputeQueueNV))
createExternalComputeQueueNV device
                               createInfo
                               allocator = liftIO . evalContT $ do
  let cmds = case device of Device{deviceCmds} -> deviceCmds
  let vkCreateExternalComputeQueueNVPtr = pVkCreateExternalComputeQueueNV cmds
  lift $ unless (vkCreateExternalComputeQueueNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateExternalComputeQueueNV is null" Nothing Nothing
  let vkCreateExternalComputeQueueNV' = mkVkCreateExternalComputeQueueNV vkCreateExternalComputeQueueNVPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPExternalQueue <- ContT $ bracket (callocBytes @(Ptr ExternalComputeQueueNV_T) 8) free
  r <- lift $ traceAroundEvent "vkCreateExternalComputeQueueNV" (vkCreateExternalComputeQueueNV'
                                                                   (deviceHandle (device))
                                                                   pCreateInfo
                                                                   pAllocator
                                                                   (pPExternalQueue))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pExternalQueue <- lift $ peek @(Ptr ExternalComputeQueueNV_T) pPExternalQueue
  pure $ (((\h -> ExternalComputeQueueNV h cmds ) pExternalQueue))

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createExternalComputeQueueNV' and 'destroyExternalComputeQueueNV'
--
-- To ensure that 'destroyExternalComputeQueueNV' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withExternalComputeQueueNV :: forall io r . MonadIO io => Device -> ExternalComputeQueueCreateInfoNV -> Maybe AllocationCallbacks -> (io ExternalComputeQueueNV -> (ExternalComputeQueueNV -> io ()) -> r) -> r
withExternalComputeQueueNV device pCreateInfo pAllocator b =
  b (createExternalComputeQueueNV device pCreateInfo pAllocator)
    (\(o0) -> destroyExternalComputeQueueNV device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyExternalComputeQueueNV
  :: FunPtr (Ptr Device_T -> Ptr ExternalComputeQueueNV_T -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> Ptr ExternalComputeQueueNV_T -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyExternalComputeQueueNV - Destroys an external queue.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroyExternalComputeQueueNV-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDestroyExternalComputeQueueNV-externalQueue-parameter#
--     @externalQueue@ /must/ be a valid
--     'Vulkan.Extensions.Handles.ExternalComputeQueueNV' handle
--
-- -   #VUID-vkDestroyExternalComputeQueueNV-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkDestroyExternalComputeQueueNV-externalQueue-parent#
--     @externalQueue@ /must/ have been created, allocated, or retrieved
--     from @device@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_external_compute_queue VK_NV_external_compute_queue>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Extensions.Handles.ExternalComputeQueueNV'
destroyExternalComputeQueueNV :: forall io
                               . (MonadIO io)
                              => -- | @device@ is the logical device that destroys the external queue.
                                 Device
                              -> -- | @externalQueue@ is the
                                 -- 'Vulkan.Extensions.Handles.ExternalComputeQueueNV' to destroy.
                                 ("externalQueue" ::: ExternalComputeQueueNV)
                              -> -- | @pAllocator@ controls host memory allocation as described in the
                                 -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#memory-allocation Memory Allocation>
                                 -- chapter.
                                 ("allocator" ::: Maybe AllocationCallbacks)
                              -> io ()
destroyExternalComputeQueueNV device
                                externalQueue
                                allocator = liftIO . evalContT $ do
  let vkDestroyExternalComputeQueueNVPtr = pVkDestroyExternalComputeQueueNV (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkDestroyExternalComputeQueueNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyExternalComputeQueueNV is null" Nothing Nothing
  let vkDestroyExternalComputeQueueNV' = mkVkDestroyExternalComputeQueueNV vkDestroyExternalComputeQueueNVPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyExternalComputeQueueNV" (vkDestroyExternalComputeQueueNV'
                                                               (deviceHandle (device))
                                                               (externalComputeQueueNVHandle (externalQueue))
                                                               pAllocator)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetExternalComputeQueueDataNV
  :: FunPtr (Ptr ExternalComputeQueueNV_T -> Ptr ExternalComputeQueueDataParamsNV -> Ptr () -> IO ()) -> Ptr ExternalComputeQueueNV_T -> Ptr ExternalComputeQueueDataParamsNV -> Ptr () -> IO ()

-- | vkGetExternalComputeQueueDataNV - Retrieves data necessary for
-- compatible external API initialization
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_external_compute_queue VK_NV_external_compute_queue>,
-- 'ExternalComputeQueueDataParamsNV',
-- 'Vulkan.Extensions.Handles.ExternalComputeQueueNV'
getExternalComputeQueueDataNV :: forall io
                               . (MonadIO io)
                              => -- | @externalQueue@ is the
                                 -- 'Vulkan.Extensions.Handles.ExternalComputeQueueNV' to query the data
                                 -- for.
                                 --
                                 -- #VUID-vkGetExternalComputeQueueDataNV-externalQueue-parameter#
                                 -- @externalQueue@ /must/ be a valid
                                 -- 'Vulkan.Extensions.Handles.ExternalComputeQueueNV' handle
                                 ("externalQueue" ::: ExternalComputeQueueNV)
                              -> -- | @pData@ is a pointer to application-allocated memory in which the
                                 -- requested data will be returned.
                                 --
                                 -- #VUID-vkGetExternalComputeQueueDataNV-pData-08134# @pData@ /must/ be at
                                 -- least the size specified by the externalDataSize field in the
                                 -- 'PhysicalDeviceExternalComputeQueuePropertiesNV' structure
                                 --
                                 -- #VUID-vkGetExternalComputeQueueDataNV-pData-parameter# @pData@ /must/ be
                                 -- a pointer value
                                 ("data" ::: Ptr ())
                              -> io (ExternalComputeQueueDataParamsNV)
getExternalComputeQueueDataNV externalQueue data' = liftIO . evalContT $ do
  let vkGetExternalComputeQueueDataNVPtr = pVkGetExternalComputeQueueDataNV (case externalQueue of ExternalComputeQueueNV{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetExternalComputeQueueDataNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetExternalComputeQueueDataNV is null" Nothing Nothing
  let vkGetExternalComputeQueueDataNV' = mkVkGetExternalComputeQueueDataNV vkGetExternalComputeQueueDataNVPtr
  pParams <- ContT (withZeroCStruct @ExternalComputeQueueDataParamsNV)
  lift $ traceAroundEvent "vkGetExternalComputeQueueDataNV" (vkGetExternalComputeQueueDataNV'
                                                               (externalComputeQueueNVHandle (externalQueue))
                                                               (pParams)
                                                               (data'))
  params <- lift $ peekCStruct @ExternalComputeQueueDataParamsNV pParams
  pure $ (params)


-- | VkExternalComputeQueueDeviceCreateInfoNV - Structure specifying
-- information about external compute queues relevant to device creation
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_external_compute_queue VK_NV_external_compute_queue>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ExternalComputeQueueDeviceCreateInfoNV = ExternalComputeQueueDeviceCreateInfoNV
  { -- | @reservedExternalQueues@ is the maximum number of external queues an
    -- application /can/ create at once. This /must/ be less than or equal to
    -- the @maxExternalQueues@ value reported by
    -- 'PhysicalDeviceExternalComputeQueuePropertiesNV'
    reservedExternalQueues :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExternalComputeQueueDeviceCreateInfoNV)
#endif
deriving instance Show ExternalComputeQueueDeviceCreateInfoNV

instance ToCStruct ExternalComputeQueueDeviceCreateInfoNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExternalComputeQueueDeviceCreateInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXTERNAL_COMPUTE_QUEUE_DEVICE_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (reservedExternalQueues)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXTERNAL_COMPUTE_QUEUE_DEVICE_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct ExternalComputeQueueDeviceCreateInfoNV where
  peekCStruct p = do
    reservedExternalQueues <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ ExternalComputeQueueDeviceCreateInfoNV
             reservedExternalQueues

instance Storable ExternalComputeQueueDeviceCreateInfoNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExternalComputeQueueDeviceCreateInfoNV where
  zero = ExternalComputeQueueDeviceCreateInfoNV
           zero


-- | VkExternalComputeQueueCreateInfoNV - Structure specifying configuration
-- parameters for external compute queue creation
--
-- = Description
--
-- When creating a 'Vulkan.Extensions.Handles.ExternalComputeQueueNV', the
-- @preferredQueue@ field is a strong scheduling hint as to which
-- 'Vulkan.Core10.Handles.Queue' Vulkan graphics workloads will be
-- submitted to with the expectation that execution will overlap with
-- execution of work submitted by the external API.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_external_compute_queue VK_NV_external_compute_queue>,
-- 'Vulkan.Core10.Handles.Queue',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createExternalComputeQueueNV'
data ExternalComputeQueueCreateInfoNV = ExternalComputeQueueCreateInfoNV
  { -- | @preferredQueue@ is a 'Vulkan.Core10.Handles.Queue' supporting graphics
    -- commands.
    --
    -- #VUID-VkExternalComputeQueueCreateInfoNV-preferredQueue-parameter#
    -- @preferredQueue@ /must/ be a valid 'Vulkan.Core10.Handles.Queue' handle
    preferredQueue :: Ptr Queue_T }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExternalComputeQueueCreateInfoNV)
#endif
deriving instance Show ExternalComputeQueueCreateInfoNV

instance ToCStruct ExternalComputeQueueCreateInfoNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExternalComputeQueueCreateInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXTERNAL_COMPUTE_QUEUE_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Queue_T))) (preferredQueue)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXTERNAL_COMPUTE_QUEUE_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Queue_T))) (zero)
    f

instance FromCStruct ExternalComputeQueueCreateInfoNV where
  peekCStruct p = do
    preferredQueue <- peek @(Ptr Queue_T) ((p `plusPtr` 16 :: Ptr (Ptr Queue_T)))
    pure $ ExternalComputeQueueCreateInfoNV
             preferredQueue

instance Storable ExternalComputeQueueCreateInfoNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExternalComputeQueueCreateInfoNV where
  zero = ExternalComputeQueueCreateInfoNV
           zero


-- | VkExternalComputeQueueDataParamsNV - Structure specifying parameters for
-- implementation-specific data retrieval from the external compute queue
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_external_compute_queue VK_NV_external_compute_queue>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getExternalComputeQueueDataNV'
data ExternalComputeQueueDataParamsNV = ExternalComputeQueueDataParamsNV
  { -- | @deviceIndex@ is the index of the device within a device group that the
    -- data is being queried for. This is ignored if device groups are not
    -- utilized.
    deviceIndex :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExternalComputeQueueDataParamsNV)
#endif
deriving instance Show ExternalComputeQueueDataParamsNV

instance ToCStruct ExternalComputeQueueDataParamsNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExternalComputeQueueDataParamsNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXTERNAL_COMPUTE_QUEUE_DATA_PARAMS_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (deviceIndex)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXTERNAL_COMPUTE_QUEUE_DATA_PARAMS_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct ExternalComputeQueueDataParamsNV where
  peekCStruct p = do
    deviceIndex <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ ExternalComputeQueueDataParamsNV
             deviceIndex

instance Storable ExternalComputeQueueDataParamsNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExternalComputeQueueDataParamsNV where
  zero = ExternalComputeQueueDataParamsNV
           zero


-- | VkPhysicalDeviceExternalComputeQueuePropertiesNV - Structure specifying
-- hardware specific information and limits for
-- VK_NV_external_compute_queue functionality
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_external_compute_queue VK_NV_external_compute_queue>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceExternalComputeQueuePropertiesNV = PhysicalDeviceExternalComputeQueuePropertiesNV
  { -- | @externalDataSize@ is the minimum size of the memory allocation that
    -- applications /can/ pass to 'getExternalComputeQueueDataNV'.
    externalDataSize :: Word32
  , -- | @maxExternalQueues@ is the maximum number of external queues that an
    -- application can create.
    maxExternalQueues :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceExternalComputeQueuePropertiesNV)
#endif
deriving instance Show PhysicalDeviceExternalComputeQueuePropertiesNV

instance ToCStruct PhysicalDeviceExternalComputeQueuePropertiesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceExternalComputeQueuePropertiesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_COMPUTE_QUEUE_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (externalDataSize)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxExternalQueues)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_COMPUTE_QUEUE_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceExternalComputeQueuePropertiesNV where
  peekCStruct p = do
    externalDataSize <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxExternalQueues <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pure $ PhysicalDeviceExternalComputeQueuePropertiesNV
             externalDataSize maxExternalQueues

instance Storable PhysicalDeviceExternalComputeQueuePropertiesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceExternalComputeQueuePropertiesNV where
  zero = PhysicalDeviceExternalComputeQueuePropertiesNV
           zero
           zero


type NV_EXTERNAL_COMPUTE_QUEUE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_EXTERNAL_COMPUTE_QUEUE_SPEC_VERSION"
pattern NV_EXTERNAL_COMPUTE_QUEUE_SPEC_VERSION :: forall a . Integral a => a
pattern NV_EXTERNAL_COMPUTE_QUEUE_SPEC_VERSION = 1


type NV_EXTERNAL_COMPUTE_QUEUE_EXTENSION_NAME = "VK_NV_external_compute_queue"

-- No documentation found for TopLevel "VK_NV_EXTERNAL_COMPUTE_QUEUE_EXTENSION_NAME"
pattern NV_EXTERNAL_COMPUTE_QUEUE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_EXTERNAL_COMPUTE_QUEUE_EXTENSION_NAME = "VK_NV_external_compute_queue"

