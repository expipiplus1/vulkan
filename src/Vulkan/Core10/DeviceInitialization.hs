{-# language CPP #-}
-- No documentation found for Chapter "DeviceInitialization"
module Vulkan.Core10.DeviceInitialization  ( createInstance
                                           , withInstance
                                           , destroyInstance
                                           , enumeratePhysicalDevices
                                           , getDeviceProcAddr
                                           , getInstanceProcAddr
                                           , getPhysicalDeviceProperties
                                           , getPhysicalDeviceQueueFamilyProperties
                                           , getPhysicalDeviceMemoryProperties
                                           , getPhysicalDeviceFeatures
                                           , getPhysicalDeviceFormatProperties
                                           , getPhysicalDeviceImageFormatProperties
                                           , PhysicalDeviceProperties(..)
                                           , ApplicationInfo(..)
                                           , InstanceCreateInfo(..)
                                           , QueueFamilyProperties(..)
                                           , PhysicalDeviceMemoryProperties(..)
                                           , MemoryType(..)
                                           , MemoryHeap(..)
                                           , FormatProperties(..)
                                           , ImageFormatProperties(..)
                                           , PhysicalDeviceFeatures(..)
                                           , PhysicalDeviceSparseProperties(..)
                                           , PhysicalDeviceLimits(..)
                                           , Instance(..)
                                           , PhysicalDevice(..)
                                           , AllocationCallbacks(..)
                                           , InstanceCreateFlags(..)
                                           , ImageType(..)
                                           , ImageTiling(..)
                                           , InternalAllocationType(..)
                                           , SystemAllocationScope(..)
                                           , PhysicalDeviceType(..)
                                           , Format(..)
                                           , StructureType(..)
                                           , QueueFlagBits(..)
                                           , QueueFlags
                                           , MemoryPropertyFlagBits(..)
                                           , MemoryPropertyFlags
                                           , MemoryHeapFlagBits(..)
                                           , MemoryHeapFlags
                                           , ImageUsageFlagBits(..)
                                           , ImageUsageFlags
                                           , ImageCreateFlagBits(..)
                                           , ImageCreateFlags
                                           , FormatFeatureFlagBits(..)
                                           , FormatFeatureFlags
                                           , SampleCountFlagBits(..)
                                           , SampleCountFlags
                                           , FN_vkInternalAllocationNotification
                                           , PFN_vkInternalAllocationNotification
                                           , FN_vkInternalFreeNotification
                                           , PFN_vkInternalFreeNotification
                                           , FN_vkReallocationFunction
                                           , PFN_vkReallocationFunction
                                           , FN_vkAllocationFunction
                                           , PFN_vkAllocationFunction
                                           , FN_vkFreeFunction
                                           , PFN_vkFreeFunction
                                           , FN_vkVoidFunction
                                           , PFN_vkVoidFunction
                                           ) where

import Vulkan.CStruct.Utils (FixedArray)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Utils (maybePeek)
import GHC.Base (when)
import GHC.IO (throwIO)
import Foreign.Ptr (castFunPtr)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.ByteString (packCString)
import Data.ByteString (useAsCString)
import Data.Coerce (coerce)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Foreign.C.Types (CChar(..))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(..))
import Foreign.C.Types (CFloat(CFloat))
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
import GHC.Ptr (Ptr(Ptr))
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
import Vulkan.Dynamic (getInstanceProcAddr')
import Vulkan.Dynamic (initInstanceCmds)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.CStruct.Utils (peekByteStringFromSizedVectorPtr)
import Vulkan.CStruct.Utils (pokeFixedLengthByteString)
import Vulkan.CStruct.Utils (pokeFixedLengthNullTerminatedByteString)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_debug_report (DebugReportCallbackCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_debug_utils (DebugUtilsMessengerCreateInfoEXT)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkGetDeviceProcAddr))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.FundamentalTypes (Extent3D)
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.Core10.Enums.Format (Format(..))
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlags)
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlagBits(..))
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlags)
import Vulkan.Core10.Enums.ImageTiling (ImageTiling)
import Vulkan.Core10.Enums.ImageTiling (ImageTiling(..))
import Vulkan.Core10.Enums.ImageType (ImageType)
import Vulkan.Core10.Enums.ImageType (ImageType(..))
import Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlagBits(..))
import Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlags)
import Vulkan.Core10.Handles (Instance)
import Vulkan.Core10.Handles (Instance(..))
import Vulkan.Core10.Handles (Instance(Instance))
import Vulkan.Dynamic (InstanceCmds(pVkDestroyInstance))
import Vulkan.Dynamic (InstanceCmds(pVkEnumeratePhysicalDevices))
import Vulkan.Dynamic (InstanceCmds(pVkGetInstanceProcAddr))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceFeatures))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceFormatProperties))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceImageFormatProperties))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceMemoryProperties))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceProperties))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceQueueFamilyProperties))
import Vulkan.Core10.Enums.InstanceCreateFlags (InstanceCreateFlags)
import Vulkan.Core10.Handles (Instance_T)
import Vulkan.Core10.APIConstants (MAX_MEMORY_HEAPS)
import Vulkan.Core10.APIConstants (MAX_MEMORY_TYPES)
import Vulkan.Core10.APIConstants (MAX_PHYSICAL_DEVICE_NAME_SIZE)
import Vulkan.Core10.Enums.MemoryHeapFlagBits (MemoryHeapFlags)
import Vulkan.Core10.Enums.MemoryPropertyFlagBits (MemoryPropertyFlags)
import Vulkan.Core10.FuncPointers (PFN_vkVoidFunction)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice(PhysicalDevice))
import Vulkan.Core10.Enums.PhysicalDeviceType (PhysicalDeviceType)
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.QueueFlagBits (QueueFlags)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlags)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.APIConstants (UUID_SIZE)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_validation_features (ValidationFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_validation_flags (ValidationFlagsEXT)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.APIConstants (pattern MAX_MEMORY_HEAPS)
import Vulkan.Core10.APIConstants (pattern MAX_MEMORY_TYPES)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_APPLICATION_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_INSTANCE_CREATE_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks(..))
import Vulkan.Core10.FuncPointers (FN_vkAllocationFunction)
import Vulkan.Core10.FuncPointers (FN_vkFreeFunction)
import Vulkan.Core10.FuncPointers (FN_vkInternalAllocationNotification)
import Vulkan.Core10.FuncPointers (FN_vkInternalFreeNotification)
import Vulkan.Core10.FuncPointers (FN_vkReallocationFunction)
import Vulkan.Core10.FuncPointers (FN_vkVoidFunction)
import Vulkan.Core10.Enums.Format (Format(..))
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlagBits(..))
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlags)
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlagBits(..))
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlags)
import Vulkan.Core10.Enums.ImageTiling (ImageTiling(..))
import Vulkan.Core10.Enums.ImageType (ImageType(..))
import Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlagBits(..))
import Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlags)
import Vulkan.Core10.Handles (Instance(..))
import Vulkan.Core10.Enums.InstanceCreateFlags (InstanceCreateFlags(..))
import Vulkan.Core10.Enums.InternalAllocationType (InternalAllocationType(..))
import Vulkan.Core10.Enums.MemoryHeapFlagBits (MemoryHeapFlagBits(..))
import Vulkan.Core10.Enums.MemoryHeapFlagBits (MemoryHeapFlags)
import Vulkan.Core10.Enums.MemoryPropertyFlagBits (MemoryPropertyFlagBits(..))
import Vulkan.Core10.Enums.MemoryPropertyFlagBits (MemoryPropertyFlags)
import Vulkan.Core10.FuncPointers (PFN_vkAllocationFunction)
import Vulkan.Core10.FuncPointers (PFN_vkFreeFunction)
import Vulkan.Core10.FuncPointers (PFN_vkInternalAllocationNotification)
import Vulkan.Core10.FuncPointers (PFN_vkInternalFreeNotification)
import Vulkan.Core10.FuncPointers (PFN_vkReallocationFunction)
import Vulkan.Core10.FuncPointers (PFN_vkVoidFunction)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Enums.PhysicalDeviceType (PhysicalDeviceType(..))
import Vulkan.Core10.Enums.QueueFlagBits (QueueFlagBits(..))
import Vulkan.Core10.Enums.QueueFlagBits (QueueFlags)
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlagBits(..))
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlags)
import Vulkan.Core10.Enums.StructureType (StructureType(..))
import Vulkan.Core10.Enums.SystemAllocationScope (SystemAllocationScope(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateInstance
  :: FunPtr (Ptr (SomeStruct InstanceCreateInfo) -> Ptr AllocationCallbacks -> Ptr (Ptr Instance_T) -> IO Result) -> Ptr (SomeStruct InstanceCreateInfo) -> Ptr AllocationCallbacks -> Ptr (Ptr Instance_T) -> IO Result

-- | vkCreateInstance - Create a new Vulkan instance
--
-- = Description
--
-- 'createInstance' verifies that the requested layers exist. If not,
-- 'createInstance' will return
-- 'Vulkan.Core10.Enums.Result.ERROR_LAYER_NOT_PRESENT'. Next
-- 'createInstance' verifies that the requested extensions are supported
-- (e.g. in the implementation or in any enabled instance layer) and if any
-- requested extension is not supported, 'createInstance' /must/ return
-- 'Vulkan.Core10.Enums.Result.ERROR_EXTENSION_NOT_PRESENT'. After
-- verifying and enabling the instance layers and extensions the
-- 'Vulkan.Core10.Handles.Instance' object is created and returned to the
-- application. If a requested extension is only supported by a layer, both
-- the layer and the extension need to be specified at 'createInstance'
-- time for the creation to succeed.
--
-- == Valid Usage
--
-- -   #VUID-vkCreateInstance-ppEnabledExtensionNames-01388# All
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-extensions-extensiondependencies required extensions>
--     for each extension in the
--     'InstanceCreateInfo'::@ppEnabledExtensionNames@ list /must/ also be
--     present in that list
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateInstance-pCreateInfo-parameter# @pCreateInfo@ /must/
--     be a valid pointer to a valid 'InstanceCreateInfo' structure
--
-- -   #VUID-vkCreateInstance-pAllocator-parameter# If @pAllocator@ is not
--     @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkCreateInstance-pInstance-parameter# @pInstance@ /must/ be a
--     valid pointer to a 'Vulkan.Core10.Handles.Instance' handle
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_LAYER_NOT_PRESENT'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_EXTENSION_NOT_PRESENT'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INCOMPATIBLE_DRIVER'
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Instance', 'InstanceCreateInfo'
createInstance :: forall a io
                . (Extendss InstanceCreateInfo a, PokeChain a, MonadIO io)
               => -- | @pCreateInfo@ is a pointer to a 'InstanceCreateInfo' structure
                  -- controlling creation of the instance.
                  (InstanceCreateInfo a)
               -> -- | @pAllocator@ controls host memory allocation as described in the
                  -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                  -- chapter.
                  ("allocator" ::: Maybe AllocationCallbacks)
               -> io (Instance)
createInstance createInfo allocator = liftIO . evalContT $ do
  vkCreateInstancePtr <- lift $ castFunPtr @_ @(("pCreateInfo" ::: Ptr (SomeStruct InstanceCreateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pInstance" ::: Ptr (Ptr Instance_T)) -> IO Result) <$> getInstanceProcAddr' nullPtr (Ptr "vkCreateInstance"#)
  lift $ unless (vkCreateInstancePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateInstance is null" Nothing Nothing
  let vkCreateInstance' = mkVkCreateInstance vkCreateInstancePtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPInstance <- ContT $ bracket (callocBytes @(Ptr Instance_T) 8) free
  r <- lift $ traceAroundEvent "vkCreateInstance" (vkCreateInstance' (forgetExtensions pCreateInfo) pAllocator (pPInstance))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pInstance <- lift $ peek @(Ptr Instance_T) pPInstance
  pInstance' <- lift $ (\h -> Instance h <$> initInstanceCmds h) pInstance
  pure $ (pInstance')

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createInstance' and 'destroyInstance'
--
-- To ensure that 'destroyInstance' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withInstance :: forall a io r . (Extendss InstanceCreateInfo a, PokeChain a, MonadIO io) => InstanceCreateInfo a -> Maybe AllocationCallbacks -> (io Instance -> (Instance -> io ()) -> r) -> r
withInstance pCreateInfo pAllocator b =
  b (createInstance pCreateInfo pAllocator)
    (\(o0) -> destroyInstance o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyInstance
  :: FunPtr (Ptr Instance_T -> Ptr AllocationCallbacks -> IO ()) -> Ptr Instance_T -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyInstance - Destroy an instance of Vulkan
--
-- == Valid Usage
--
-- -   #VUID-vkDestroyInstance-instance-00629# All child objects created
--     using @instance@ /must/ have been destroyed prior to destroying
--     @instance@
--
-- -   #VUID-vkDestroyInstance-instance-00630# If
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @instance@ was created, a compatible set of callbacks
--     /must/ be provided here
--
-- -   #VUID-vkDestroyInstance-instance-00631# If no
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @instance@ was created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroyInstance-instance-parameter# If @instance@ is not
--     @NULL@, @instance@ /must/ be a valid
--     'Vulkan.Core10.Handles.Instance' handle
--
-- -   #VUID-vkDestroyInstance-pAllocator-parameter# If @pAllocator@ is not
--     @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- == Host Synchronization
--
-- -   Host access to @instance@ /must/ be externally synchronized
--
-- -   Host access to all 'Vulkan.Core10.Handles.PhysicalDevice' objects
--     enumerated from @instance@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Instance'
destroyInstance :: forall io
                 . (MonadIO io)
                => -- | @instance@ is the handle of the instance to destroy.
                   Instance
                -> -- | @pAllocator@ controls host memory allocation as described in the
                   -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                   -- chapter.
                   ("allocator" ::: Maybe AllocationCallbacks)
                -> io ()
destroyInstance instance' allocator = liftIO . evalContT $ do
  let vkDestroyInstancePtr = pVkDestroyInstance (instanceCmds (instance' :: Instance))
  lift $ unless (vkDestroyInstancePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyInstance is null" Nothing Nothing
  let vkDestroyInstance' = mkVkDestroyInstance vkDestroyInstancePtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyInstance" (vkDestroyInstance' (instanceHandle (instance')) pAllocator)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEnumeratePhysicalDevices
  :: FunPtr (Ptr Instance_T -> Ptr Word32 -> Ptr (Ptr PhysicalDevice_T) -> IO Result) -> Ptr Instance_T -> Ptr Word32 -> Ptr (Ptr PhysicalDevice_T) -> IO Result

-- | vkEnumeratePhysicalDevices - Enumerates the physical devices accessible
-- to a Vulkan instance
--
-- = Description
--
-- If @pPhysicalDevices@ is @NULL@, then the number of physical devices
-- available is returned in @pPhysicalDeviceCount@. Otherwise,
-- @pPhysicalDeviceCount@ /must/ point to a variable set by the user to the
-- number of elements in the @pPhysicalDevices@ array, and on return the
-- variable is overwritten with the number of handles actually written to
-- @pPhysicalDevices@. If @pPhysicalDeviceCount@ is less than the number of
-- physical devices available, at most @pPhysicalDeviceCount@ structures
-- will be written. If @pPhysicalDeviceCount@ is smaller than the number of
-- physical devices available, 'Vulkan.Core10.Enums.Result.INCOMPLETE' will
-- be returned instead of 'Vulkan.Core10.Enums.Result.SUCCESS', to indicate
-- that not all the available physical devices were returned.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkEnumeratePhysicalDevices-instance-parameter# @instance@
--     /must/ be a valid 'Vulkan.Core10.Handles.Instance' handle
--
-- -   #VUID-vkEnumeratePhysicalDevices-pPhysicalDeviceCount-parameter#
--     @pPhysicalDeviceCount@ /must/ be a valid pointer to a @uint32_t@
--     value
--
-- -   #VUID-vkEnumeratePhysicalDevices-pPhysicalDevices-parameter# If the
--     value referenced by @pPhysicalDeviceCount@ is not @0@, and
--     @pPhysicalDevices@ is not @NULL@, @pPhysicalDevices@ /must/ be a
--     valid pointer to an array of @pPhysicalDeviceCount@
--     'Vulkan.Core10.Handles.PhysicalDevice' handles
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Instance', 'Vulkan.Core10.Handles.PhysicalDevice'
enumeratePhysicalDevices :: forall io
                          . (MonadIO io)
                         => -- | @instance@ is a handle to a Vulkan instance previously created with
                            -- 'createInstance'.
                            Instance
                         -> io (Result, ("physicalDevices" ::: Vector PhysicalDevice))
enumeratePhysicalDevices instance' = liftIO . evalContT $ do
  let cmds = instanceCmds (instance' :: Instance)
  let vkEnumeratePhysicalDevicesPtr = pVkEnumeratePhysicalDevices cmds
  lift $ unless (vkEnumeratePhysicalDevicesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkEnumeratePhysicalDevices is null" Nothing Nothing
  let vkEnumeratePhysicalDevices' = mkVkEnumeratePhysicalDevices vkEnumeratePhysicalDevicesPtr
  let instance'' = instanceHandle (instance')
  pPPhysicalDeviceCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkEnumeratePhysicalDevices" (vkEnumeratePhysicalDevices' instance'' (pPPhysicalDeviceCount) (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPhysicalDeviceCount <- lift $ peek @Word32 pPPhysicalDeviceCount
  pPPhysicalDevices <- ContT $ bracket (callocBytes @(Ptr PhysicalDevice_T) ((fromIntegral (pPhysicalDeviceCount)) * 8)) free
  r' <- lift $ traceAroundEvent "vkEnumeratePhysicalDevices" (vkEnumeratePhysicalDevices' instance'' (pPPhysicalDeviceCount) (pPPhysicalDevices))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pPhysicalDeviceCount' <- lift $ peek @Word32 pPPhysicalDeviceCount
  pPhysicalDevices' <- lift $ generateM (fromIntegral (pPhysicalDeviceCount')) (\i -> do
    pPhysicalDevicesElem <- peek @(Ptr PhysicalDevice_T) ((pPPhysicalDevices `advancePtrBytes` (8 * (i)) :: Ptr (Ptr PhysicalDevice_T)))
    pure $ (\h -> PhysicalDevice h cmds ) pPhysicalDevicesElem)
  pure $ ((r'), pPhysicalDevices')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceProcAddr
  :: FunPtr (Ptr Device_T -> Ptr CChar -> IO PFN_vkVoidFunction) -> Ptr Device_T -> Ptr CChar -> IO PFN_vkVoidFunction

-- | vkGetDeviceProcAddr - Return a function pointer for a command
--
-- = Parameters
--
-- The table below defines the various use cases for 'getDeviceProcAddr'
-- and expected return value for each case.
--
-- = Description
--
-- The returned function pointer is of type
-- 'Vulkan.Core10.FuncPointers.PFN_vkVoidFunction', and /must/ be cast to
-- the type of the command being queried before use. The function pointer
-- /must/ only be called with a dispatchable object (the first parameter)
-- that is @device@ or a child of @device@.
--
-- +----------------------+----------------------+-----------------------+
-- | @device@             | @pName@              | return value          |
-- +======================+======================+=======================+
-- | @NULL@               | *1                   | undefined             |
-- +----------------------+----------------------+-----------------------+
-- | invalid device       | *1                   | undefined             |
-- +----------------------+----------------------+-----------------------+
-- | device               | @NULL@               | undefined             |
-- +----------------------+----------------------+-----------------------+
-- | device               | core device-level    | fp3                   |
-- |                      | Vulkan command2      |                       |
-- +----------------------+----------------------+-----------------------+
-- | device               | enabled extension    | fp3                   |
-- |                      | device-level         |                       |
-- |                      | commands2            |                       |
-- +----------------------+----------------------+-----------------------+
-- | any other case, not  |                      | @NULL@                |
-- | covered above        |                      |                       |
-- +----------------------+----------------------+-----------------------+
--
-- 'getDeviceProcAddr' behavior
--
-- [1]
--     \"*\" means any representable value for the parameter (including
--     valid values, invalid values, and @NULL@).
--
-- [2]
--     In this function, device-level excludes all physical-device-level
--     commands.
--
-- [3]
--     The returned function pointer /must/ only be called with a
--     dispatchable object (the first parameter) that is @device@ or a
--     child of @device@ e.g. 'Vulkan.Core10.Handles.Device',
--     'Vulkan.Core10.Handles.Queue', or
--     'Vulkan.Core10.Handles.CommandBuffer'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FuncPointers.PFN_vkVoidFunction',
-- 'Vulkan.Core10.Handles.Device'
getDeviceProcAddr :: forall io
                   . (MonadIO io)
                  => -- | #VUID-vkGetDeviceProcAddr-device-parameter# @device@ /must/ be a valid
                     -- 'Vulkan.Core10.Handles.Device' handle
                     Device
                  -> -- | #VUID-vkGetDeviceProcAddr-pName-parameter# @pName@ /must/ be a
                     -- null-terminated UTF-8 string
                     ("name" ::: ByteString)
                  -> io (PFN_vkVoidFunction)
getDeviceProcAddr device name = liftIO . evalContT $ do
  let vkGetDeviceProcAddrPtr = pVkGetDeviceProcAddr (deviceCmds (device :: Device))
  lift $ unless (vkGetDeviceProcAddrPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDeviceProcAddr is null" Nothing Nothing
  let vkGetDeviceProcAddr' = mkVkGetDeviceProcAddr vkGetDeviceProcAddrPtr
  pName <- ContT $ useAsCString (name)
  r <- lift $ traceAroundEvent "vkGetDeviceProcAddr" (vkGetDeviceProcAddr' (deviceHandle (device)) pName)
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetInstanceProcAddr
  :: FunPtr (Ptr Instance_T -> Ptr CChar -> IO PFN_vkVoidFunction) -> Ptr Instance_T -> Ptr CChar -> IO PFN_vkVoidFunction

-- | vkGetInstanceProcAddr - Return a function pointer for a command
--
-- = Description
--
-- 'getInstanceProcAddr' itself is obtained in a platform- and loader-
-- specific manner. Typically, the loader library will export this command
-- as a function symbol, so applications /can/ link against the loader
-- library, or load it dynamically and look up the symbol using
-- platform-specific APIs.
--
-- The table below defines the various use cases for 'getInstanceProcAddr'
-- and expected return value (“fp” is “function pointer”) for each case.
--
-- The returned function pointer is of type
-- 'Vulkan.Core10.FuncPointers.PFN_vkVoidFunction', and /must/ be cast to
-- the type of the command being queried before use.
--
-- +----------------------+-------------------------------------------------------------------------+-----------------------+
-- | @instance@           | @pName@                                                                 | return value          |
-- +======================+=========================================================================+=======================+
-- | *1                   | @NULL@                                                                  | undefined             |
-- +----------------------+-------------------------------------------------------------------------+-----------------------+
-- | invalid non-@NULL@   | *1                                                                      | undefined             |
-- | instance             |                                                                         |                       |
-- +----------------------+-------------------------------------------------------------------------+-----------------------+
-- | @NULL@               | 'getInstanceProcAddr'                                                   | fp4                   |
-- +----------------------+-------------------------------------------------------------------------+-----------------------+
-- | @NULL@               | 'Vulkan.Core11.DeviceInitialization.enumerateInstanceVersion'           | fp                    |
-- +----------------------+-------------------------------------------------------------------------+-----------------------+
-- | @NULL@               | 'Vulkan.Core10.ExtensionDiscovery.enumerateInstanceExtensionProperties' | fp                    |
-- +----------------------+-------------------------------------------------------------------------+-----------------------+
-- | @NULL@               | 'Vulkan.Core10.LayerDiscovery.enumerateInstanceLayerProperties'         | fp                    |
-- +----------------------+-------------------------------------------------------------------------+-----------------------+
-- | @NULL@               | 'createInstance'                                                        | fp                    |
-- +----------------------+-------------------------------------------------------------------------+-----------------------+
-- | instance             | core Vulkan command                                                     | fp2                   |
-- +----------------------+-------------------------------------------------------------------------+-----------------------+
-- | instance             | enabled instance extension commands for @instance@                      | fp2                   |
-- +----------------------+-------------------------------------------------------------------------+-----------------------+
-- | instance             | available device extension3 commands for @instance@                     | fp2                   |
-- +----------------------+-------------------------------------------------------------------------+-----------------------+
-- | any other case, not  |                                                                         | @NULL@                |
-- | covered above        |                                                                         |                       |
-- +----------------------+-------------------------------------------------------------------------+-----------------------+
--
-- 'getInstanceProcAddr' behavior
--
-- [1]
--     \"*\" means any representable value for the parameter (including
--     valid values, invalid values, and @NULL@).
--
-- [2]
--     The returned function pointer /must/ only be called with a
--     dispatchable object (the first parameter) that is @instance@ or a
--     child of @instance@, e.g. 'Vulkan.Core10.Handles.Instance',
--     'Vulkan.Core10.Handles.PhysicalDevice',
--     'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Queue', or
--     'Vulkan.Core10.Handles.CommandBuffer'.
--
-- [3]
--     An “available device extension” is a device extension supported by
--     any physical device enumerated by @instance@.
--
-- [4]
--     Starting with Vulkan 1.2, 'getInstanceProcAddr' can resolve itself
--     with a @NULL@ instance pointer.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetInstanceProcAddr-instance-parameter# If @instance@ is not
--     @NULL@, @instance@ /must/ be a valid
--     'Vulkan.Core10.Handles.Instance' handle
--
-- -   #VUID-vkGetInstanceProcAddr-pName-parameter# @pName@ /must/ be a
--     null-terminated UTF-8 string
--
-- = See Also
--
-- 'Vulkan.Core10.FuncPointers.PFN_vkVoidFunction',
-- 'Vulkan.Core10.Handles.Instance'
getInstanceProcAddr :: forall io
                     . (MonadIO io)
                    => -- | @instance@ is the instance that the function pointer will be compatible
                       -- with, or @NULL@ for commands not dependent on any instance.
                       Instance
                    -> -- | @pName@ is the name of the command to obtain.
                       ("name" ::: ByteString)
                    -> io (PFN_vkVoidFunction)
getInstanceProcAddr instance' name = liftIO . evalContT $ do
  let vkGetInstanceProcAddrPtr = pVkGetInstanceProcAddr (instanceCmds (instance' :: Instance))
  lift $ unless (vkGetInstanceProcAddrPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetInstanceProcAddr is null" Nothing Nothing
  let vkGetInstanceProcAddr' = mkVkGetInstanceProcAddr vkGetInstanceProcAddrPtr
  pName <- ContT $ useAsCString (name)
  r <- lift $ traceAroundEvent "vkGetInstanceProcAddr" (vkGetInstanceProcAddr' (instanceHandle (instance')) pName)
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceProperties
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr PhysicalDeviceProperties -> IO ()) -> Ptr PhysicalDevice_T -> Ptr PhysicalDeviceProperties -> IO ()

-- | vkGetPhysicalDeviceProperties - Returns properties of a physical device
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.PhysicalDevice', 'PhysicalDeviceProperties'
getPhysicalDeviceProperties :: forall io
                             . (MonadIO io)
                            => -- | @physicalDevice@ is the handle to the physical device whose properties
                               -- will be queried.
                               --
                               -- #VUID-vkGetPhysicalDeviceProperties-physicalDevice-parameter#
                               -- @physicalDevice@ /must/ be a valid
                               -- 'Vulkan.Core10.Handles.PhysicalDevice' handle
                               PhysicalDevice
                            -> io (PhysicalDeviceProperties)
getPhysicalDeviceProperties physicalDevice = liftIO . evalContT $ do
  let vkGetPhysicalDevicePropertiesPtr = pVkGetPhysicalDeviceProperties (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDevicePropertiesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceProperties is null" Nothing Nothing
  let vkGetPhysicalDeviceProperties' = mkVkGetPhysicalDeviceProperties vkGetPhysicalDevicePropertiesPtr
  pPProperties <- ContT (withZeroCStruct @PhysicalDeviceProperties)
  lift $ traceAroundEvent "vkGetPhysicalDeviceProperties" (vkGetPhysicalDeviceProperties' (physicalDeviceHandle (physicalDevice)) (pPProperties))
  pProperties <- lift $ peekCStruct @PhysicalDeviceProperties pPProperties
  pure $ (pProperties)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceQueueFamilyProperties
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr QueueFamilyProperties -> IO ()) -> Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr QueueFamilyProperties -> IO ()

-- | vkGetPhysicalDeviceQueueFamilyProperties - Reports properties of the
-- queues of the specified physical device
--
-- = Description
--
-- If @pQueueFamilyProperties@ is @NULL@, then the number of queue families
-- available is returned in @pQueueFamilyPropertyCount@. Implementations
-- /must/ support at least one queue family. Otherwise,
-- @pQueueFamilyPropertyCount@ /must/ point to a variable set by the user
-- to the number of elements in the @pQueueFamilyProperties@ array, and on
-- return the variable is overwritten with the number of structures
-- actually written to @pQueueFamilyProperties@. If
-- @pQueueFamilyPropertyCount@ is less than the number of queue families
-- available, at most @pQueueFamilyPropertyCount@ structures will be
-- written.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPhysicalDeviceQueueFamilyProperties-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkGetPhysicalDeviceQueueFamilyProperties-pQueueFamilyPropertyCount-parameter#
--     @pQueueFamilyPropertyCount@ /must/ be a valid pointer to a
--     @uint32_t@ value
--
-- -   #VUID-vkGetPhysicalDeviceQueueFamilyProperties-pQueueFamilyProperties-parameter#
--     If the value referenced by @pQueueFamilyPropertyCount@ is not @0@,
--     and @pQueueFamilyProperties@ is not @NULL@, @pQueueFamilyProperties@
--     /must/ be a valid pointer to an array of @pQueueFamilyPropertyCount@
--     'QueueFamilyProperties' structures
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.PhysicalDevice', 'QueueFamilyProperties'
getPhysicalDeviceQueueFamilyProperties :: forall io
                                        . (MonadIO io)
                                       => -- | @physicalDevice@ is the handle to the physical device whose properties
                                          -- will be queried.
                                          PhysicalDevice
                                       -> io (("queueFamilyProperties" ::: Vector QueueFamilyProperties))
getPhysicalDeviceQueueFamilyProperties physicalDevice = liftIO . evalContT $ do
  let vkGetPhysicalDeviceQueueFamilyPropertiesPtr = pVkGetPhysicalDeviceQueueFamilyProperties (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceQueueFamilyPropertiesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceQueueFamilyProperties is null" Nothing Nothing
  let vkGetPhysicalDeviceQueueFamilyProperties' = mkVkGetPhysicalDeviceQueueFamilyProperties vkGetPhysicalDeviceQueueFamilyPropertiesPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPQueueFamilyPropertyCount <- ContT $ bracket (callocBytes @Word32 4) free
  lift $ traceAroundEvent "vkGetPhysicalDeviceQueueFamilyProperties" (vkGetPhysicalDeviceQueueFamilyProperties' physicalDevice' (pPQueueFamilyPropertyCount) (nullPtr))
  pQueueFamilyPropertyCount <- lift $ peek @Word32 pPQueueFamilyPropertyCount
  pPQueueFamilyProperties <- ContT $ bracket (callocBytes @QueueFamilyProperties ((fromIntegral (pQueueFamilyPropertyCount)) * 24)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPQueueFamilyProperties `advancePtrBytes` (i * 24) :: Ptr QueueFamilyProperties) . ($ ())) [0..(fromIntegral (pQueueFamilyPropertyCount)) - 1]
  lift $ traceAroundEvent "vkGetPhysicalDeviceQueueFamilyProperties" (vkGetPhysicalDeviceQueueFamilyProperties' physicalDevice' (pPQueueFamilyPropertyCount) ((pPQueueFamilyProperties)))
  pQueueFamilyPropertyCount' <- lift $ peek @Word32 pPQueueFamilyPropertyCount
  pQueueFamilyProperties' <- lift $ generateM (fromIntegral (pQueueFamilyPropertyCount')) (\i -> peekCStruct @QueueFamilyProperties (((pPQueueFamilyProperties) `advancePtrBytes` (24 * (i)) :: Ptr QueueFamilyProperties)))
  pure $ (pQueueFamilyProperties')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceMemoryProperties
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr PhysicalDeviceMemoryProperties -> IO ()) -> Ptr PhysicalDevice_T -> Ptr PhysicalDeviceMemoryProperties -> IO ()

-- | vkGetPhysicalDeviceMemoryProperties - Reports memory information for the
-- specified physical device
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.PhysicalDevice', 'PhysicalDeviceMemoryProperties'
getPhysicalDeviceMemoryProperties :: forall io
                                   . (MonadIO io)
                                  => -- | @physicalDevice@ is the handle to the device to query.
                                     --
                                     -- #VUID-vkGetPhysicalDeviceMemoryProperties-physicalDevice-parameter#
                                     -- @physicalDevice@ /must/ be a valid
                                     -- 'Vulkan.Core10.Handles.PhysicalDevice' handle
                                     PhysicalDevice
                                  -> io (PhysicalDeviceMemoryProperties)
getPhysicalDeviceMemoryProperties physicalDevice = liftIO . evalContT $ do
  let vkGetPhysicalDeviceMemoryPropertiesPtr = pVkGetPhysicalDeviceMemoryProperties (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceMemoryPropertiesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceMemoryProperties is null" Nothing Nothing
  let vkGetPhysicalDeviceMemoryProperties' = mkVkGetPhysicalDeviceMemoryProperties vkGetPhysicalDeviceMemoryPropertiesPtr
  pPMemoryProperties <- ContT (withZeroCStruct @PhysicalDeviceMemoryProperties)
  lift $ traceAroundEvent "vkGetPhysicalDeviceMemoryProperties" (vkGetPhysicalDeviceMemoryProperties' (physicalDeviceHandle (physicalDevice)) (pPMemoryProperties))
  pMemoryProperties <- lift $ peekCStruct @PhysicalDeviceMemoryProperties pPMemoryProperties
  pure $ (pMemoryProperties)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceFeatures
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr PhysicalDeviceFeatures -> IO ()) -> Ptr PhysicalDevice_T -> Ptr PhysicalDeviceFeatures -> IO ()

-- | vkGetPhysicalDeviceFeatures - Reports capabilities of a physical device
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.PhysicalDevice', 'PhysicalDeviceFeatures'
getPhysicalDeviceFeatures :: forall io
                           . (MonadIO io)
                          => -- | @physicalDevice@ is the physical device from which to query the
                             -- supported features.
                             --
                             -- #VUID-vkGetPhysicalDeviceFeatures-physicalDevice-parameter#
                             -- @physicalDevice@ /must/ be a valid
                             -- 'Vulkan.Core10.Handles.PhysicalDevice' handle
                             PhysicalDevice
                          -> io (PhysicalDeviceFeatures)
getPhysicalDeviceFeatures physicalDevice = liftIO . evalContT $ do
  let vkGetPhysicalDeviceFeaturesPtr = pVkGetPhysicalDeviceFeatures (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceFeaturesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceFeatures is null" Nothing Nothing
  let vkGetPhysicalDeviceFeatures' = mkVkGetPhysicalDeviceFeatures vkGetPhysicalDeviceFeaturesPtr
  pPFeatures <- ContT (withZeroCStruct @PhysicalDeviceFeatures)
  lift $ traceAroundEvent "vkGetPhysicalDeviceFeatures" (vkGetPhysicalDeviceFeatures' (physicalDeviceHandle (physicalDevice)) (pPFeatures))
  pFeatures <- lift $ peekCStruct @PhysicalDeviceFeatures pPFeatures
  pure $ (pFeatures)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceFormatProperties
  :: FunPtr (Ptr PhysicalDevice_T -> Format -> Ptr FormatProperties -> IO ()) -> Ptr PhysicalDevice_T -> Format -> Ptr FormatProperties -> IO ()

-- | vkGetPhysicalDeviceFormatProperties - Lists physical device’s format
-- capabilities
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.Format.Format', 'FormatProperties',
-- 'Vulkan.Core10.Handles.PhysicalDevice'
getPhysicalDeviceFormatProperties :: forall io
                                   . (MonadIO io)
                                  => -- | @physicalDevice@ is the physical device from which to query the format
                                     -- properties.
                                     --
                                     -- #VUID-vkGetPhysicalDeviceFormatProperties-physicalDevice-parameter#
                                     -- @physicalDevice@ /must/ be a valid
                                     -- 'Vulkan.Core10.Handles.PhysicalDevice' handle
                                     PhysicalDevice
                                  -> -- | @format@ is the format whose properties are queried.
                                     --
                                     -- #VUID-vkGetPhysicalDeviceFormatProperties-format-parameter# @format@
                                     -- /must/ be a valid 'Vulkan.Core10.Enums.Format.Format' value
                                     Format
                                  -> io (FormatProperties)
getPhysicalDeviceFormatProperties physicalDevice format = liftIO . evalContT $ do
  let vkGetPhysicalDeviceFormatPropertiesPtr = pVkGetPhysicalDeviceFormatProperties (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceFormatPropertiesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceFormatProperties is null" Nothing Nothing
  let vkGetPhysicalDeviceFormatProperties' = mkVkGetPhysicalDeviceFormatProperties vkGetPhysicalDeviceFormatPropertiesPtr
  pPFormatProperties <- ContT (withZeroCStruct @FormatProperties)
  lift $ traceAroundEvent "vkGetPhysicalDeviceFormatProperties" (vkGetPhysicalDeviceFormatProperties' (physicalDeviceHandle (physicalDevice)) (format) (pPFormatProperties))
  pFormatProperties <- lift $ peekCStruct @FormatProperties pPFormatProperties
  pure $ (pFormatProperties)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceImageFormatProperties
  :: FunPtr (Ptr PhysicalDevice_T -> Format -> ImageType -> ImageTiling -> ImageUsageFlags -> ImageCreateFlags -> Ptr ImageFormatProperties -> IO Result) -> Ptr PhysicalDevice_T -> Format -> ImageType -> ImageTiling -> ImageUsageFlags -> ImageCreateFlags -> Ptr ImageFormatProperties -> IO Result

-- | vkGetPhysicalDeviceImageFormatProperties - Lists physical device’s image
-- format capabilities
--
-- = Description
--
-- The @format@, @type@, @tiling@, @usage@, and @flags@ parameters
-- correspond to parameters that would be consumed by
-- 'Vulkan.Core10.Image.createImage' (as members of
-- 'Vulkan.Core10.Image.ImageCreateInfo').
--
-- If @format@ is not a supported image format, or if the combination of
-- @format@, @type@, @tiling@, @usage@, and @flags@ is not supported for
-- images, then 'getPhysicalDeviceImageFormatProperties' returns
-- 'Vulkan.Core10.Enums.Result.ERROR_FORMAT_NOT_SUPPORTED'.
--
-- The limitations on an image format that are reported by
-- 'getPhysicalDeviceImageFormatProperties' have the following property: if
-- @usage1@ and @usage2@ of type
-- 'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlags' are such that
-- the bits set in @usage1@ are a subset of the bits set in @usage2@, and
-- @flags1@ and @flags2@ of type
-- 'Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlags' are such that
-- the bits set in @flags1@ are a subset of the bits set in @flags2@, then
-- the limitations for @usage1@ and @flags1@ /must/ be no more strict than
-- the limitations for @usage2@ and @flags2@, for all values of @format@,
-- @type@, and @tiling@.
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_FORMAT_NOT_SUPPORTED'
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.Format.Format',
-- 'Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlags',
-- 'ImageFormatProperties', 'Vulkan.Core10.Enums.ImageTiling.ImageTiling',
-- 'Vulkan.Core10.Enums.ImageType.ImageType',
-- 'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlags',
-- 'Vulkan.Core10.Handles.PhysicalDevice'
getPhysicalDeviceImageFormatProperties :: forall io
                                        . (MonadIO io)
                                       => -- | @physicalDevice@ is the physical device from which to query the image
                                          -- capabilities.
                                          --
                                          -- #VUID-vkGetPhysicalDeviceImageFormatProperties-physicalDevice-parameter#
                                          -- @physicalDevice@ /must/ be a valid
                                          -- 'Vulkan.Core10.Handles.PhysicalDevice' handle
                                          PhysicalDevice
                                       -> -- | @format@ is a 'Vulkan.Core10.Enums.Format.Format' value specifying the
                                          -- image format, corresponding to
                                          -- 'Vulkan.Core10.Image.ImageCreateInfo'::@format@.
                                          --
                                          -- #VUID-vkGetPhysicalDeviceImageFormatProperties-format-parameter#
                                          -- @format@ /must/ be a valid 'Vulkan.Core10.Enums.Format.Format' value
                                          Format
                                       -> -- | @type@ is a 'Vulkan.Core10.Enums.ImageType.ImageType' value specifying
                                          -- the image type, corresponding to
                                          -- 'Vulkan.Core10.Image.ImageCreateInfo'::@imageType@.
                                          --
                                          -- #VUID-vkGetPhysicalDeviceImageFormatProperties-type-parameter# @type@
                                          -- /must/ be a valid 'Vulkan.Core10.Enums.ImageType.ImageType' value
                                          ImageType
                                       -> -- | @tiling@ is a 'Vulkan.Core10.Enums.ImageTiling.ImageTiling' value
                                          -- specifying the image tiling, corresponding to
                                          -- 'Vulkan.Core10.Image.ImageCreateInfo'::@tiling@.
                                          --
                                          -- #VUID-vkGetPhysicalDeviceImageFormatProperties-tiling-02248# @tiling@
                                          -- /must/ not be
                                          -- 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT'.
                                          -- (Use
                                          -- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
                                          -- instead)
                                          --
                                          -- #VUID-vkGetPhysicalDeviceImageFormatProperties-tiling-parameter#
                                          -- @tiling@ /must/ be a valid 'Vulkan.Core10.Enums.ImageTiling.ImageTiling'
                                          -- value
                                          ImageTiling
                                       -> -- | @usage@ is a bitmask of
                                          -- 'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits' specifying
                                          -- the intended usage of the image, corresponding to
                                          -- 'Vulkan.Core10.Image.ImageCreateInfo'::@usage@.
                                          --
                                          -- #VUID-vkGetPhysicalDeviceImageFormatProperties-usage-parameter# @usage@
                                          -- /must/ be a valid combination of
                                          -- 'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits' values
                                          --
                                          -- #VUID-vkGetPhysicalDeviceImageFormatProperties-usage-requiredbitmask#
                                          -- @usage@ /must/ not be @0@
                                          ImageUsageFlags
                                       -> -- | @flags@ is a bitmask of
                                          -- 'Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlagBits' specifying
                                          -- additional parameters of the image, corresponding to
                                          -- 'Vulkan.Core10.Image.ImageCreateInfo'::@flags@.
                                          --
                                          -- #VUID-vkGetPhysicalDeviceImageFormatProperties-flags-parameter# @flags@
                                          -- /must/ be a valid combination of
                                          -- 'Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlagBits' values
                                          ImageCreateFlags
                                       -> io (ImageFormatProperties)
getPhysicalDeviceImageFormatProperties physicalDevice format type' tiling usage flags = liftIO . evalContT $ do
  let vkGetPhysicalDeviceImageFormatPropertiesPtr = pVkGetPhysicalDeviceImageFormatProperties (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceImageFormatPropertiesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceImageFormatProperties is null" Nothing Nothing
  let vkGetPhysicalDeviceImageFormatProperties' = mkVkGetPhysicalDeviceImageFormatProperties vkGetPhysicalDeviceImageFormatPropertiesPtr
  pPImageFormatProperties <- ContT (withZeroCStruct @ImageFormatProperties)
  r <- lift $ traceAroundEvent "vkGetPhysicalDeviceImageFormatProperties" (vkGetPhysicalDeviceImageFormatProperties' (physicalDeviceHandle (physicalDevice)) (format) (type') (tiling) (usage) (flags) (pPImageFormatProperties))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pImageFormatProperties <- lift $ peekCStruct @ImageFormatProperties pPImageFormatProperties
  pure $ (pImageFormatProperties)


-- | VkPhysicalDeviceProperties - Structure specifying physical device
-- properties
--
-- = Description
--
-- Note
--
-- The value of @apiVersion@ /may/ be different than the version returned
-- by 'Vulkan.Core11.DeviceInitialization.enumerateInstanceVersion'; either
-- higher or lower. In such cases, the application /must/ not use
-- functionality that exceeds the version of Vulkan associated with a given
-- object. The @pApiVersion@ parameter returned by
-- 'Vulkan.Core11.DeviceInitialization.enumerateInstanceVersion' is the
-- version associated with a 'Vulkan.Core10.Handles.Instance' and its
-- children, except for a 'Vulkan.Core10.Handles.PhysicalDevice' and its
-- children. 'PhysicalDeviceProperties'::@apiVersion@ is the version
-- associated with a 'Vulkan.Core10.Handles.PhysicalDevice' and its
-- children.
--
-- The @vendorID@ and @deviceID@ fields are provided to allow applications
-- to adapt to device characteristics that are not adequately exposed by
-- other Vulkan queries.
--
-- Note
--
-- These /may/ include performance profiles, hardware errata, or other
-- characteristics.
--
-- The /vendor/ identified by @vendorID@ is the entity responsible for the
-- most salient characteristics of the underlying implementation of the
-- 'Vulkan.Core10.Handles.PhysicalDevice' being queried.
--
-- Note
--
-- For example, in the case of a discrete GPU implementation, this /should/
-- be the GPU chipset vendor. In the case of a hardware accelerator
-- integrated into a system-on-chip (SoC), this /should/ be the supplier of
-- the silicon IP used to create the accelerator.
--
-- If the vendor has a
-- <https://pcisig.com/membership/member-companies PCI vendor ID>, the low
-- 16 bits of @vendorID@ /must/ contain that PCI vendor ID, and the
-- remaining bits /must/ be set to zero. Otherwise, the value returned
-- /must/ be a valid Khronos vendor ID, obtained as described in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vulkan-styleguide Vulkan Documentation and Extensions: Procedures and Conventions>
-- document in the section “Registering a Vendor ID with Khronos”. Khronos
-- vendor IDs are allocated starting at 0x10000, to distinguish them from
-- the PCI vendor ID namespace. Khronos vendor IDs are symbolically defined
-- in the 'Vulkan.Core10.Enums.VendorId.VendorId' type.
--
-- The vendor is also responsible for the value returned in @deviceID@. If
-- the implementation is driven primarily by a
-- <https://pcisig.com/ PCI device> with a
-- <https://pcisig.com/ PCI device ID>, the low 16 bits of @deviceID@
-- /must/ contain that PCI device ID, and the remaining bits /must/ be set
-- to zero. Otherwise, the choice of what values to return /may/ be
-- dictated by operating system or platform policies - but /should/
-- uniquely identify both the device version and any major configuration
-- options (for example, core count in the case of multicore devices).
--
-- Note
--
-- The same device ID /should/ be used for all physical implementations of
-- that device version and configuration. For example, all uses of a
-- specific silicon IP GPU version and configuration /should/ use the same
-- device ID, even if those uses occur in different SoCs.
--
-- = See Also
--
-- 'PhysicalDeviceLimits',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- 'PhysicalDeviceSparseProperties',
-- 'Vulkan.Core10.Enums.PhysicalDeviceType.PhysicalDeviceType',
-- 'getPhysicalDeviceProperties'
data PhysicalDeviceProperties = PhysicalDeviceProperties
  { -- | @apiVersion@ is the version of Vulkan supported by the device, encoded
    -- as described in
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-coreversions-versionnumbers>.
    apiVersion :: Word32
  , -- | @driverVersion@ is the vendor-specified version of the driver.
    driverVersion :: Word32
  , -- | @vendorID@ is a unique identifier for the /vendor/ (see below) of the
    -- physical device.
    vendorID :: Word32
  , -- | @deviceID@ is a unique identifier for the physical device among devices
    -- available from the vendor.
    deviceID :: Word32
  , -- | @deviceType@ is a
    -- 'Vulkan.Core10.Enums.PhysicalDeviceType.PhysicalDeviceType' specifying
    -- the type of device.
    deviceType :: PhysicalDeviceType
  , -- | @deviceName@ is an array of
    -- 'Vulkan.Core10.APIConstants.MAX_PHYSICAL_DEVICE_NAME_SIZE' @char@
    -- containing a null-terminated UTF-8 string which is the name of the
    -- device.
    deviceName :: ByteString
  , -- | @pipelineCacheUUID@ is an array of
    -- 'Vulkan.Core10.APIConstants.UUID_SIZE' @uint8_t@ values representing a
    -- universally unique identifier for the device.
    pipelineCacheUUID :: ByteString
  , -- | @limits@ is the 'PhysicalDeviceLimits' structure specifying
    -- device-specific limits of the physical device. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits Limits>
    -- for details.
    limits :: PhysicalDeviceLimits
  , -- | @sparseProperties@ is the 'PhysicalDeviceSparseProperties' structure
    -- specifying various sparse related properties of the physical device. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#sparsememory-physicalprops Sparse Properties>
    -- for details.
    sparseProperties :: PhysicalDeviceSparseProperties
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceProperties)
#endif
deriving instance Show PhysicalDeviceProperties

instance ToCStruct PhysicalDeviceProperties where
  withCStruct x f = allocaBytesAligned 824 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (apiVersion)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (driverVersion)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (vendorID)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (deviceID)
    poke ((p `plusPtr` 16 :: Ptr PhysicalDeviceType)) (deviceType)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 20 :: Ptr (FixedArray MAX_PHYSICAL_DEVICE_NAME_SIZE CChar))) (deviceName)
    pokeFixedLengthByteString ((p `plusPtr` 276 :: Ptr (FixedArray UUID_SIZE Word8))) (pipelineCacheUUID)
    poke ((p `plusPtr` 296 :: Ptr PhysicalDeviceLimits)) (limits)
    poke ((p `plusPtr` 800 :: Ptr PhysicalDeviceSparseProperties)) (sparseProperties)
    f
  cStructSize = 824
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 16 :: Ptr PhysicalDeviceType)) (zero)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 20 :: Ptr (FixedArray MAX_PHYSICAL_DEVICE_NAME_SIZE CChar))) (mempty)
    pokeFixedLengthByteString ((p `plusPtr` 276 :: Ptr (FixedArray UUID_SIZE Word8))) (mempty)
    poke ((p `plusPtr` 296 :: Ptr PhysicalDeviceLimits)) (zero)
    poke ((p `plusPtr` 800 :: Ptr PhysicalDeviceSparseProperties)) (zero)
    f

instance FromCStruct PhysicalDeviceProperties where
  peekCStruct p = do
    apiVersion <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    driverVersion <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    vendorID <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    deviceID <- peek @Word32 ((p `plusPtr` 12 :: Ptr Word32))
    deviceType <- peek @PhysicalDeviceType ((p `plusPtr` 16 :: Ptr PhysicalDeviceType))
    deviceName <- packCString (lowerArrayPtr ((p `plusPtr` 20 :: Ptr (FixedArray MAX_PHYSICAL_DEVICE_NAME_SIZE CChar))))
    pipelineCacheUUID <- peekByteStringFromSizedVectorPtr ((p `plusPtr` 276 :: Ptr (FixedArray UUID_SIZE Word8)))
    limits <- peekCStruct @PhysicalDeviceLimits ((p `plusPtr` 296 :: Ptr PhysicalDeviceLimits))
    sparseProperties <- peekCStruct @PhysicalDeviceSparseProperties ((p `plusPtr` 800 :: Ptr PhysicalDeviceSparseProperties))
    pure $ PhysicalDeviceProperties
             apiVersion driverVersion vendorID deviceID deviceType deviceName pipelineCacheUUID limits sparseProperties

instance Storable PhysicalDeviceProperties where
  sizeOf ~_ = 824
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceProperties where
  zero = PhysicalDeviceProperties
           zero
           zero
           zero
           zero
           zero
           mempty
           mempty
           zero
           zero


-- | VkApplicationInfo - Structure specifying application info
--
-- = Description
--
-- Vulkan 1.0 implementations were required to return
-- 'Vulkan.Core10.Enums.Result.ERROR_INCOMPATIBLE_DRIVER' if @apiVersion@
-- was larger than 1.0. Implementations that support Vulkan 1.1 or later
-- /must/ not return 'Vulkan.Core10.Enums.Result.ERROR_INCOMPATIBLE_DRIVER'
-- for any value of @apiVersion@.
--
-- Note
--
-- Because Vulkan 1.0 implementations /may/ fail with
-- 'Vulkan.Core10.Enums.Result.ERROR_INCOMPATIBLE_DRIVER', applications
-- /should/ determine the version of Vulkan available before calling
-- 'createInstance'. If the 'getInstanceProcAddr' returns @NULL@ for
-- 'Vulkan.Core11.DeviceInitialization.enumerateInstanceVersion', it is a
-- Vulkan 1.0 implementation. Otherwise, the application /can/ call
-- 'Vulkan.Core11.DeviceInitialization.enumerateInstanceVersion' to
-- determine the version of Vulkan.
--
-- As long as the instance supports at least Vulkan 1.1, an application
-- /can/ use different versions of Vulkan with an instance than it does
-- with a device or physical device.
--
-- Note
--
-- The Khronos validation layers will treat @apiVersion@ as the highest API
-- version the application targets, and will validate API usage against the
-- minimum of that version and the implementation version (instance or
-- device, depending on context). If an application tries to use
-- functionality from a greater version than this, a validation error will
-- be triggered.
--
-- For example, if the instance supports Vulkan 1.1 and three physical
-- devices support Vulkan 1.0, Vulkan 1.1, and Vulkan 1.2, respectively,
-- and if the application sets @apiVersion@ to 1.2, the application /can/
-- use the following versions of Vulkan:
--
-- -   Vulkan 1.0 /can/ be used with the instance and with all physical
--     devices.
--
-- -   Vulkan 1.1 /can/ be used with the instance and with the physical
--     devices that support Vulkan 1.1 and Vulkan 1.2.
--
-- -   Vulkan 1.2 /can/ be used with the physical device that supports
--     Vulkan 1.2.
--
-- If we modify the above example so that the application sets @apiVersion@
-- to 1.1, then the application /must/ not use Vulkan 1.2 functionality on
-- the physical device that supports Vulkan 1.2.
--
-- Implicit layers /must/ be disabled if they do not support a version at
-- least as high as @apiVersion@. See the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#LoaderAndLayerInterface Vulkan Loader Specification and Architecture Overview>
-- document for additional information.
--
-- Note
--
-- Providing a @NULL@ 'InstanceCreateInfo'::@pApplicationInfo@ or providing
-- an @apiVersion@ of 0 is equivalent to providing an @apiVersion@ of
-- @VK_MAKE_VERSION(1,0,0)@.
--
-- == Valid Usage
--
-- -   #VUID-VkApplicationInfo-apiVersion-04010# If @apiVersion@ is not
--     @0@, then it /must/ be greater or equal to
--     'Vulkan.Core10.API_VERSION_1_0'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkApplicationInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_APPLICATION_INFO'
--
-- -   #VUID-VkApplicationInfo-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkApplicationInfo-pApplicationName-parameter# If
--     @pApplicationName@ is not @NULL@, @pApplicationName@ /must/ be a
--     null-terminated UTF-8 string
--
-- -   #VUID-VkApplicationInfo-pEngineName-parameter# If @pEngineName@ is
--     not @NULL@, @pEngineName@ /must/ be a null-terminated UTF-8 string
--
-- = See Also
--
-- 'InstanceCreateInfo', 'Vulkan.Core10.Enums.StructureType.StructureType'
data ApplicationInfo = ApplicationInfo
  { -- | @pApplicationName@ is @NULL@ or is a pointer to a null-terminated UTF-8
    -- string containing the name of the application.
    applicationName :: Maybe ByteString
  , -- | @applicationVersion@ is an unsigned integer variable containing the
    -- developer-supplied version number of the application.
    applicationVersion :: Word32
  , -- | @pEngineName@ is @NULL@ or is a pointer to a null-terminated UTF-8
    -- string containing the name of the engine (if any) used to create the
    -- application.
    engineName :: Maybe ByteString
  , -- | @engineVersion@ is an unsigned integer variable containing the
    -- developer-supplied version number of the engine used to create the
    -- application.
    engineVersion :: Word32
  , -- | @apiVersion@ /must/ be the highest version of Vulkan that the
    -- application is designed to use, encoded as described in
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-coreversions-versionnumbers>.
    -- The patch version number specified in @apiVersion@ is ignored when
    -- creating an instance object. Only the major and minor versions of the
    -- instance /must/ match those requested in @apiVersion@.
    apiVersion :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ApplicationInfo)
#endif
deriving instance Show ApplicationInfo

instance ToCStruct ApplicationInfo where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ApplicationInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_APPLICATION_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pApplicationName'' <- case (applicationName) of
      Nothing -> pure nullPtr
      Just j -> ContT $ useAsCString (j)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr CChar))) pApplicationName''
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (applicationVersion)
    pEngineName'' <- case (engineName) of
      Nothing -> pure nullPtr
      Just j -> ContT $ useAsCString (j)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr CChar))) pEngineName''
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) (engineVersion)
    lift $ poke ((p `plusPtr` 44 :: Ptr Word32)) (apiVersion)
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_APPLICATION_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (zero)
    f

instance FromCStruct ApplicationInfo where
  peekCStruct p = do
    pApplicationName <- peek @(Ptr CChar) ((p `plusPtr` 16 :: Ptr (Ptr CChar)))
    pApplicationName' <- maybePeek (\j -> packCString (j)) pApplicationName
    applicationVersion <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pEngineName <- peek @(Ptr CChar) ((p `plusPtr` 32 :: Ptr (Ptr CChar)))
    pEngineName' <- maybePeek (\j -> packCString (j)) pEngineName
    engineVersion <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    apiVersion <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    pure $ ApplicationInfo
             pApplicationName' applicationVersion pEngineName' engineVersion apiVersion

instance Zero ApplicationInfo where
  zero = ApplicationInfo
           Nothing
           zero
           Nothing
           zero
           zero


-- | VkInstanceCreateInfo - Structure specifying parameters of a newly
-- created instance
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkInstanceCreateInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_INSTANCE_CREATE_INFO'
--
-- -   #VUID-VkInstanceCreateInfo-pNext-pNext# Each @pNext@ member of any
--     structure (including this one) in the @pNext@ chain /must/ be either
--     @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_EXT_debug_report.DebugReportCallbackCreateInfoEXT',
--     'Vulkan.Extensions.VK_EXT_debug_utils.DebugUtilsMessengerCreateInfoEXT',
--     'Vulkan.Extensions.VK_EXT_validation_features.ValidationFeaturesEXT',
--     or 'Vulkan.Extensions.VK_EXT_validation_flags.ValidationFlagsEXT'
--
-- -   #VUID-VkInstanceCreateInfo-sType-unique# The @sType@ value of each
--     struct in the @pNext@ chain /must/ be unique, with the exception of
--     structures of type
--     'Vulkan.Extensions.VK_EXT_debug_utils.DebugUtilsMessengerCreateInfoEXT'
--
-- -   #VUID-VkInstanceCreateInfo-flags-zerobitmask# @flags@ /must/ be @0@
--
-- -   #VUID-VkInstanceCreateInfo-pApplicationInfo-parameter# If
--     @pApplicationInfo@ is not @NULL@, @pApplicationInfo@ /must/ be a
--     valid pointer to a valid 'ApplicationInfo' structure
--
-- -   #VUID-VkInstanceCreateInfo-ppEnabledLayerNames-parameter# If
--     @enabledLayerCount@ is not @0@, @ppEnabledLayerNames@ /must/ be a
--     valid pointer to an array of @enabledLayerCount@ null-terminated
--     UTF-8 strings
--
-- -   #VUID-VkInstanceCreateInfo-ppEnabledExtensionNames-parameter# If
--     @enabledExtensionCount@ is not @0@, @ppEnabledExtensionNames@ /must/
--     be a valid pointer to an array of @enabledExtensionCount@
--     null-terminated UTF-8 strings
--
-- = See Also
--
-- 'ApplicationInfo',
-- 'Vulkan.Core10.Enums.InstanceCreateFlags.InstanceCreateFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'createInstance'
data InstanceCreateInfo (es :: [Type]) = InstanceCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is reserved for future use.
    flags :: InstanceCreateFlags
  , -- | @pApplicationInfo@ is @NULL@ or a pointer to a 'ApplicationInfo'
    -- structure. If not @NULL@, this information helps implementations
    -- recognize behavior inherent to classes of applications.
    -- 'ApplicationInfo' is defined in detail below.
    applicationInfo :: Maybe ApplicationInfo
  , -- | @ppEnabledLayerNames@ is a pointer to an array of @enabledLayerCount@
    -- null-terminated UTF-8 strings containing the names of layers to enable
    -- for the created instance. The layers are loaded in the order they are
    -- listed in this array, with the first array element being the closest to
    -- the application, and the last array element being the closest to the
    -- driver. See the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-layers>
    -- section for further details.
    enabledLayerNames :: Vector ByteString
  , -- | @ppEnabledExtensionNames@ is a pointer to an array of
    -- @enabledExtensionCount@ null-terminated UTF-8 strings containing the
    -- names of extensions to enable.
    enabledExtensionNames :: Vector ByteString
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (InstanceCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (InstanceCreateInfo es)

instance Extensible InstanceCreateInfo where
  extensibleTypeName = "InstanceCreateInfo"
  setNext x next = x{next = next}
  getNext InstanceCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends InstanceCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @DebugUtilsMessengerCreateInfoEXT = Just f
    | Just Refl <- eqT @e @ValidationFeaturesEXT = Just f
    | Just Refl <- eqT @e @ValidationFlagsEXT = Just f
    | Just Refl <- eqT @e @DebugReportCallbackCreateInfoEXT = Just f
    | otherwise = Nothing

instance (Extendss InstanceCreateInfo es, PokeChain es) => ToCStruct (InstanceCreateInfo es) where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p InstanceCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_INSTANCE_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr InstanceCreateFlags)) (flags)
    pApplicationInfo'' <- case (applicationInfo) of
      Nothing -> pure nullPtr
      Just j -> ContT $ withCStruct (j)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr ApplicationInfo))) pApplicationInfo''
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (enabledLayerNames)) :: Word32))
    pPpEnabledLayerNames' <- ContT $ allocaBytesAligned @(Ptr CChar) ((Data.Vector.length (enabledLayerNames)) * 8) 8
    Data.Vector.imapM_ (\i e -> do
      ppEnabledLayerNames'' <- ContT $ useAsCString (e)
      lift $ poke (pPpEnabledLayerNames' `plusPtr` (8 * (i)) :: Ptr (Ptr CChar)) ppEnabledLayerNames'') (enabledLayerNames)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr (Ptr CChar)))) (pPpEnabledLayerNames')
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (enabledExtensionNames)) :: Word32))
    pPpEnabledExtensionNames' <- ContT $ allocaBytesAligned @(Ptr CChar) ((Data.Vector.length (enabledExtensionNames)) * 8) 8
    Data.Vector.imapM_ (\i e -> do
      ppEnabledExtensionNames'' <- ContT $ useAsCString (e)
      lift $ poke (pPpEnabledExtensionNames' `plusPtr` (8 * (i)) :: Ptr (Ptr CChar)) ppEnabledExtensionNames'') (enabledExtensionNames)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr (Ptr CChar)))) (pPpEnabledExtensionNames')
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_INSTANCE_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ f

instance (Extendss InstanceCreateInfo es, PeekChain es) => FromCStruct (InstanceCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @InstanceCreateFlags ((p `plusPtr` 16 :: Ptr InstanceCreateFlags))
    pApplicationInfo <- peek @(Ptr ApplicationInfo) ((p `plusPtr` 24 :: Ptr (Ptr ApplicationInfo)))
    pApplicationInfo' <- maybePeek (\j -> peekCStruct @ApplicationInfo (j)) pApplicationInfo
    enabledLayerCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    ppEnabledLayerNames <- peek @(Ptr (Ptr CChar)) ((p `plusPtr` 40 :: Ptr (Ptr (Ptr CChar))))
    ppEnabledLayerNames' <- generateM (fromIntegral enabledLayerCount) (\i -> packCString =<< peek ((ppEnabledLayerNames `advancePtrBytes` (8 * (i)) :: Ptr (Ptr CChar))))
    enabledExtensionCount <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    ppEnabledExtensionNames <- peek @(Ptr (Ptr CChar)) ((p `plusPtr` 56 :: Ptr (Ptr (Ptr CChar))))
    ppEnabledExtensionNames' <- generateM (fromIntegral enabledExtensionCount) (\i -> packCString =<< peek ((ppEnabledExtensionNames `advancePtrBytes` (8 * (i)) :: Ptr (Ptr CChar))))
    pure $ InstanceCreateInfo
             next flags pApplicationInfo' ppEnabledLayerNames' ppEnabledExtensionNames'

instance es ~ '[] => Zero (InstanceCreateInfo es) where
  zero = InstanceCreateInfo
           ()
           zero
           Nothing
           mempty
           mempty


-- | VkQueueFamilyProperties - Structure providing information about a queue
-- family
--
-- = Description
--
-- The value returned in @minImageTransferGranularity@ has a unit of
-- compressed texel blocks for images having a block-compressed format, and
-- a unit of texels otherwise.
--
-- Possible values of @minImageTransferGranularity@ are:
--
-- -   (0,0,0) which indicates that only whole mip levels /must/ be
--     transferred using the image transfer operations on the corresponding
--     queues. In this case, the following restrictions apply to all offset
--     and extent parameters of image transfer operations:
--
--     -   The @x@, @y@, and @z@ members of a
--         'Vulkan.Core10.FundamentalTypes.Offset3D' parameter /must/
--         always be zero.
--
--     -   The @width@, @height@, and @depth@ members of a
--         'Vulkan.Core10.FundamentalTypes.Extent3D' parameter /must/
--         always match the width, height, and depth of the image
--         subresource corresponding to the parameter, respectively.
--
-- -   (Ax, Ay, Az) where Ax, Ay, and Az are all integer powers of two. In
--     this case the following restrictions apply to all image transfer
--     operations:
--
--     -   @x@, @y@, and @z@ of a 'Vulkan.Core10.FundamentalTypes.Offset3D'
--         parameter /must/ be integer multiples of Ax, Ay, and Az,
--         respectively.
--
--     -   @width@ of a 'Vulkan.Core10.FundamentalTypes.Extent3D' parameter
--         /must/ be an integer multiple of Ax, or else @x@ + @width@
--         /must/ equal the width of the image subresource corresponding to
--         the parameter.
--
--     -   @height@ of a 'Vulkan.Core10.FundamentalTypes.Extent3D'
--         parameter /must/ be an integer multiple of Ay, or else @y@ +
--         @height@ /must/ equal the height of the image subresource
--         corresponding to the parameter.
--
--     -   @depth@ of a 'Vulkan.Core10.FundamentalTypes.Extent3D' parameter
--         /must/ be an integer multiple of Az, or else @z@ + @depth@
--         /must/ equal the depth of the image subresource corresponding to
--         the parameter.
--
--     -   If the format of the image corresponding to the parameters is
--         one of the block-compressed formats then for the purposes of the
--         above calculations the granularity /must/ be scaled up by the
--         compressed texel block dimensions.
--
-- Queues supporting graphics and\/or compute operations /must/ report
-- (1,1,1) in @minImageTransferGranularity@, meaning that there are no
-- additional restrictions on the granularity of image transfer operations
-- for these queues. Other queues supporting image transfer operations are
-- only /required/ to support whole mip level transfers, thus
-- @minImageTransferGranularity@ for queues belonging to such queue
-- families /may/ be (0,0,0).
--
-- The
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-device Device Memory>
-- section describes memory properties queried from the physical device.
--
-- For physical device feature queries see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features Features>
-- chapter.
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Extent3D',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.QueueFamilyProperties2',
-- 'Vulkan.Core10.Enums.QueueFlagBits.QueueFlags',
-- 'getPhysicalDeviceQueueFamilyProperties'
data QueueFamilyProperties = QueueFamilyProperties
  { -- | @queueFlags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.QueueFlagBits.QueueFlagBits' indicating
    -- capabilities of the queues in this queue family.
    queueFlags :: QueueFlags
  , -- | @queueCount@ is the unsigned integer count of queues in this queue
    -- family. Each queue family /must/ support at least one queue.
    queueCount :: Word32
  , -- | @timestampValidBits@ is the unsigned integer count of meaningful bits in
    -- the timestamps written via
    -- 'Vulkan.Core10.CommandBufferBuilding.cmdWriteTimestamp'. The valid range
    -- for the count is 36..64 bits, or a value of 0, indicating no support for
    -- timestamps. Bits outside the valid range are guaranteed to be zeros.
    timestampValidBits :: Word32
  , -- | @minImageTransferGranularity@ is the minimum granularity supported for
    -- image transfer operations on the queues in this queue family.
    minImageTransferGranularity :: Extent3D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (QueueFamilyProperties)
#endif
deriving instance Show QueueFamilyProperties

instance ToCStruct QueueFamilyProperties where
  withCStruct x f = allocaBytesAligned 24 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p QueueFamilyProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr QueueFlags)) (queueFlags)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (queueCount)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (timestampValidBits)
    poke ((p `plusPtr` 12 :: Ptr Extent3D)) (minImageTransferGranularity)
    f
  cStructSize = 24
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Extent3D)) (zero)
    f

instance FromCStruct QueueFamilyProperties where
  peekCStruct p = do
    queueFlags <- peek @QueueFlags ((p `plusPtr` 0 :: Ptr QueueFlags))
    queueCount <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    timestampValidBits <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    minImageTransferGranularity <- peekCStruct @Extent3D ((p `plusPtr` 12 :: Ptr Extent3D))
    pure $ QueueFamilyProperties
             queueFlags queueCount timestampValidBits minImageTransferGranularity

instance Storable QueueFamilyProperties where
  sizeOf ~_ = 24
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero QueueFamilyProperties where
  zero = QueueFamilyProperties
           zero
           zero
           zero
           zero


-- | VkPhysicalDeviceMemoryProperties - Structure specifying physical device
-- memory properties
--
-- = Description
--
-- The 'PhysicalDeviceMemoryProperties' structure describes a number of
-- /memory heaps/ as well as a number of /memory types/ that /can/ be used
-- to access memory allocated in those heaps. Each heap describes a memory
-- resource of a particular size, and each memory type describes a set of
-- memory properties (e.g. host cached vs uncached) that /can/ be used with
-- a given memory heap. Allocations using a particular memory type will
-- consume resources from the heap indicated by that memory type’s heap
-- index. More than one memory type /may/ share each heap, and the heaps
-- and memory types provide a mechanism to advertise an accurate size of
-- the physical memory resources while allowing the memory to be used with
-- a variety of different properties.
--
-- The number of memory heaps is given by @memoryHeapCount@ and is less
-- than or equal to 'Vulkan.Core10.APIConstants.MAX_MEMORY_HEAPS'. Each
-- heap is described by an element of the @memoryHeaps@ array as a
-- 'MemoryHeap' structure. The number of memory types available across all
-- memory heaps is given by @memoryTypeCount@ and is less than or equal to
-- 'Vulkan.Core10.APIConstants.MAX_MEMORY_TYPES'. Each memory type is
-- described by an element of the @memoryTypes@ array as a 'MemoryType'
-- structure.
--
-- At least one heap /must/ include
-- 'Vulkan.Core10.Enums.MemoryHeapFlagBits.MEMORY_HEAP_DEVICE_LOCAL_BIT' in
-- 'MemoryHeap'::@flags@. If there are multiple heaps that all have similar
-- performance characteristics, they /may/ all include
-- 'Vulkan.Core10.Enums.MemoryHeapFlagBits.MEMORY_HEAP_DEVICE_LOCAL_BIT'.
-- In a unified memory architecture (UMA) system there is often only a
-- single memory heap which is considered to be equally “local” to the host
-- and to the device, and such an implementation /must/ advertise the heap
-- as device-local.
--
-- Each memory type returned by 'getPhysicalDeviceMemoryProperties' /must/
-- have its @propertyFlags@ set to one of the following values:
--
-- -   0
--
-- -   'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_VISIBLE_BIT'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_COHERENT_BIT'
--
-- -   'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_VISIBLE_BIT'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_CACHED_BIT'
--
-- -   'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_VISIBLE_BIT'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_CACHED_BIT'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_COHERENT_BIT'
--
-- -   'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_DEVICE_LOCAL_BIT'
--
-- -   'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_DEVICE_LOCAL_BIT'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_VISIBLE_BIT'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_COHERENT_BIT'
--
-- -   'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_DEVICE_LOCAL_BIT'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_VISIBLE_BIT'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_CACHED_BIT'
--
-- -   'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_DEVICE_LOCAL_BIT'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_VISIBLE_BIT'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_CACHED_BIT'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_COHERENT_BIT'
--
-- -   'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_DEVICE_LOCAL_BIT'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT'
--
-- -   'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_PROTECTED_BIT'
--
-- -   'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_PROTECTED_BIT'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_DEVICE_LOCAL_BIT'
--
-- -   'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_VISIBLE_BIT'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_COHERENT_BIT'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD'
--
-- -   'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_VISIBLE_BIT'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_CACHED_BIT'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_COHERENT_BIT'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD'
--
-- -   'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_DEVICE_LOCAL_BIT'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD'
--
-- -   'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_DEVICE_LOCAL_BIT'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_VISIBLE_BIT'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_COHERENT_BIT'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD'
--
-- -   'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_DEVICE_LOCAL_BIT'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_VISIBLE_BIT'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_CACHED_BIT'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_COHERENT_BIT'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD'
--
-- -   'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_VISIBLE_BIT'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_COHERENT_BIT'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_DEVICE_UNCACHED_BIT_AMD'
--
-- -   'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_VISIBLE_BIT'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_CACHED_BIT'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_COHERENT_BIT'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_DEVICE_UNCACHED_BIT_AMD'
--
-- -   'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_DEVICE_LOCAL_BIT'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_DEVICE_UNCACHED_BIT_AMD'
--
-- -   'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_DEVICE_LOCAL_BIT'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_VISIBLE_BIT'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_COHERENT_BIT'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_DEVICE_UNCACHED_BIT_AMD'
--
-- -   'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_DEVICE_LOCAL_BIT'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_VISIBLE_BIT'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_CACHED_BIT'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_COHERENT_BIT'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD'
--     |
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_DEVICE_UNCACHED_BIT_AMD'
--
-- There /must/ be at least one memory type with both the
-- 'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_VISIBLE_BIT'
-- and
-- 'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_COHERENT_BIT'
-- bits set in its @propertyFlags@. There /must/ be at least one memory
-- type with the
-- 'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_DEVICE_LOCAL_BIT'
-- bit set in its @propertyFlags@. If the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-deviceCoherentMemory deviceCoherentMemory>
-- feature is enabled, there /must/ be at least one memory type with the
-- 'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD'
-- bit set in its @propertyFlags@.
--
-- For each pair of elements __X__ and __Y__ returned in @memoryTypes@,
-- __X__ /must/ be placed at a lower index position than __Y__ if:
--
-- -   the set of bit flags returned in the @propertyFlags@ member of __X__
--     is a strict subset of the set of bit flags returned in the
--     @propertyFlags@ member of __Y__; or
--
-- -   the @propertyFlags@ members of __X__ and __Y__ are equal, and __X__
--     belongs to a memory heap with greater performance (as determined in
--     an implementation-specific manner) ; or
--
-- -   the @propertyFlags@ members of __Y__ includes
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD'
--     or
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_DEVICE_UNCACHED_BIT_AMD'
--     and __X__ does not
--
-- Note
--
-- There is no ordering requirement between __X__ and __Y__ elements for
-- the case their @propertyFlags@ members are not in a subset relation.
-- That potentially allows more than one possible way to order the same set
-- of memory types. Notice that the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-device-bitmask-list list of all allowed memory property flag combinations>
-- is written in a valid order. But if instead
-- 'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_DEVICE_LOCAL_BIT'
-- was before
-- 'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_VISIBLE_BIT'
-- |
-- 'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_COHERENT_BIT',
-- the list would still be in a valid order.
--
-- There may be a performance penalty for using device coherent or uncached
-- device memory types, and using these accidentally is undesirable. In
-- order to avoid this, memory types with these properties always appear at
-- the end of the list; but are subject to the same rules otherwise.
--
-- This ordering requirement enables applications to use a simple search
-- loop to select the desired memory type along the lines of:
--
-- > // Find a memory in `memoryTypeBitsRequirement` that includes all of `requiredProperties`
-- > int32_t findProperties(const VkPhysicalDeviceMemoryProperties* pMemoryProperties,
-- >                        uint32_t memoryTypeBitsRequirement,
-- >                        VkMemoryPropertyFlags requiredProperties) {
-- >     const uint32_t memoryCount = pMemoryProperties->memoryTypeCount;
-- >     for (uint32_t memoryIndex = 0; memoryIndex < memoryCount; ++memoryIndex) {
-- >         const uint32_t memoryTypeBits = (1 << memoryIndex);
-- >         const bool isRequiredMemoryType = memoryTypeBitsRequirement & memoryTypeBits;
-- >
-- >         const VkMemoryPropertyFlags properties =
-- >             pMemoryProperties->memoryTypes[memoryIndex].propertyFlags;
-- >         const bool hasRequiredProperties =
-- >             (properties & requiredProperties) == requiredProperties;
-- >
-- >         if (isRequiredMemoryType && hasRequiredProperties)
-- >             return static_cast<int32_t>(memoryIndex);
-- >     }
-- >
-- >     // failed to find memory type
-- >     return -1;
-- > }
-- >
-- > // Try to find an optimal memory type, or if it does not exist try fallback memory type
-- > // `device` is the VkDevice
-- > // `image` is the VkImage that requires memory to be bound
-- > // `memoryProperties` properties as returned by vkGetPhysicalDeviceMemoryProperties
-- > // `requiredProperties` are the property flags that must be present
-- > // `optimalProperties` are the property flags that are preferred by the application
-- > VkMemoryRequirements memoryRequirements;
-- > vkGetImageMemoryRequirements(device, image, &memoryRequirements);
-- > int32_t memoryType =
-- >     findProperties(&memoryProperties, memoryRequirements.memoryTypeBits, optimalProperties);
-- > if (memoryType == -1) // not found; try fallback properties
-- >     memoryType =
-- >         findProperties(&memoryProperties, memoryRequirements.memoryTypeBits, requiredProperties);
--
-- = See Also
--
-- 'MemoryHeap', 'MemoryType',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceMemoryProperties2',
-- 'getPhysicalDeviceMemoryProperties'
data PhysicalDeviceMemoryProperties = PhysicalDeviceMemoryProperties
  { -- | @memoryTypeCount@ is the number of valid elements in the @memoryTypes@
    -- array.
    memoryTypeCount :: Word32
  , -- | @memoryTypes@ is an array of
    -- 'Vulkan.Core10.APIConstants.MAX_MEMORY_TYPES' 'MemoryType' structures
    -- describing the /memory types/ that /can/ be used to access memory
    -- allocated from the heaps specified by @memoryHeaps@.
    memoryTypes :: Vector MemoryType
  , -- | @memoryHeapCount@ is the number of valid elements in the @memoryHeaps@
    -- array.
    memoryHeapCount :: Word32
  , -- | @memoryHeaps@ is an array of
    -- 'Vulkan.Core10.APIConstants.MAX_MEMORY_HEAPS' 'MemoryHeap' structures
    -- describing the /memory heaps/ from which memory /can/ be allocated.
    memoryHeaps :: Vector MemoryHeap
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMemoryProperties)
#endif
deriving instance Show PhysicalDeviceMemoryProperties

instance ToCStruct PhysicalDeviceMemoryProperties where
  withCStruct x f = allocaBytesAligned 520 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMemoryProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (memoryTypeCount)
    unless ((Data.Vector.length $ (memoryTypes)) <= MAX_MEMORY_TYPES) $
      throwIO $ IOError Nothing InvalidArgument "" "memoryTypes is too long, a maximum of MAX_MEMORY_TYPES elements are allowed" Nothing Nothing
    Data.Vector.imapM_ (\i e -> poke ((lowerArrayPtr ((p `plusPtr` 4 :: Ptr (FixedArray MAX_MEMORY_TYPES MemoryType)))) `plusPtr` (8 * (i)) :: Ptr MemoryType) (e)) (memoryTypes)
    poke ((p `plusPtr` 260 :: Ptr Word32)) (memoryHeapCount)
    unless ((Data.Vector.length $ (memoryHeaps)) <= MAX_MEMORY_HEAPS) $
      throwIO $ IOError Nothing InvalidArgument "" "memoryHeaps is too long, a maximum of MAX_MEMORY_HEAPS elements are allowed" Nothing Nothing
    Data.Vector.imapM_ (\i e -> poke ((lowerArrayPtr ((p `plusPtr` 264 :: Ptr (FixedArray MAX_MEMORY_HEAPS MemoryHeap)))) `plusPtr` (16 * (i)) :: Ptr MemoryHeap) (e)) (memoryHeaps)
    f
  cStructSize = 520
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 260 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceMemoryProperties where
  peekCStruct p = do
    memoryTypeCount <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    memoryTypes <- generateM (MAX_MEMORY_TYPES) (\i -> peekCStruct @MemoryType (((lowerArrayPtr @MemoryType ((p `plusPtr` 4 :: Ptr (FixedArray MAX_MEMORY_TYPES MemoryType)))) `advancePtrBytes` (8 * (i)) :: Ptr MemoryType)))
    memoryHeapCount <- peek @Word32 ((p `plusPtr` 260 :: Ptr Word32))
    memoryHeaps <- generateM (MAX_MEMORY_HEAPS) (\i -> peekCStruct @MemoryHeap (((lowerArrayPtr @MemoryHeap ((p `plusPtr` 264 :: Ptr (FixedArray MAX_MEMORY_HEAPS MemoryHeap)))) `advancePtrBytes` (16 * (i)) :: Ptr MemoryHeap)))
    pure $ PhysicalDeviceMemoryProperties
             memoryTypeCount memoryTypes memoryHeapCount memoryHeaps

instance Storable PhysicalDeviceMemoryProperties where
  sizeOf ~_ = 520
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMemoryProperties where
  zero = PhysicalDeviceMemoryProperties
           zero
           mempty
           zero
           mempty


-- | VkMemoryType - Structure specifying memory type
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MemoryPropertyFlags',
-- 'PhysicalDeviceMemoryProperties'
data MemoryType = MemoryType
  { -- | @propertyFlags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MemoryPropertyFlagBits' of
    -- properties for this memory type.
    propertyFlags :: MemoryPropertyFlags
  , -- | @heapIndex@ describes which memory heap this memory type corresponds to,
    -- and /must/ be less than @memoryHeapCount@ from the
    -- 'PhysicalDeviceMemoryProperties' structure.
    heapIndex :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MemoryType)
#endif
deriving instance Show MemoryType

instance ToCStruct MemoryType where
  withCStruct x f = allocaBytesAligned 8 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryType{..} f = do
    poke ((p `plusPtr` 0 :: Ptr MemoryPropertyFlags)) (propertyFlags)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (heapIndex)
    f
  cStructSize = 8
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    f

instance FromCStruct MemoryType where
  peekCStruct p = do
    propertyFlags <- peek @MemoryPropertyFlags ((p `plusPtr` 0 :: Ptr MemoryPropertyFlags))
    heapIndex <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    pure $ MemoryType
             propertyFlags heapIndex

instance Storable MemoryType where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MemoryType where
  zero = MemoryType
           zero
           zero


-- | VkMemoryHeap - Structure specifying a memory heap
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.MemoryHeapFlagBits.MemoryHeapFlags',
-- 'PhysicalDeviceMemoryProperties'
data MemoryHeap = MemoryHeap
  { -- | @size@ is the total memory size in bytes in the heap.
    size :: DeviceSize
  , -- | @flags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.MemoryHeapFlagBits.MemoryHeapFlagBits' specifying
    -- attribute flags for the heap.
    flags :: MemoryHeapFlags
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MemoryHeap)
#endif
deriving instance Show MemoryHeap

instance ToCStruct MemoryHeap where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryHeap{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceSize)) (size)
    poke ((p `plusPtr` 8 :: Ptr MemoryHeapFlags)) (flags)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct MemoryHeap where
  peekCStruct p = do
    size <- peek @DeviceSize ((p `plusPtr` 0 :: Ptr DeviceSize))
    flags <- peek @MemoryHeapFlags ((p `plusPtr` 8 :: Ptr MemoryHeapFlags))
    pure $ MemoryHeap
             size flags

instance Storable MemoryHeap where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MemoryHeap where
  zero = MemoryHeap
           zero
           zero


-- | VkFormatProperties - Structure specifying image format properties
--
-- = Description
--
-- Note
--
-- If no format feature flags are supported, the format itself is not
-- supported, and images of that format cannot be created.
--
-- If @format@ is a block-compressed format, then @bufferFeatures@ /must/
-- not support any features for the format.
--
-- If @format@ is not a multi-plane format then @linearTilingFeatures@ and
-- @optimalTilingFeatures@ /must/ not contain
-- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_DISJOINT_BIT'.
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlags',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.FormatProperties2',
-- 'getPhysicalDeviceFormatProperties'
data FormatProperties = FormatProperties
  { -- | @linearTilingFeatures@ is a bitmask of
    -- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlagBits'
    -- specifying features supported by images created with a @tiling@
    -- parameter of 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR'.
    linearTilingFeatures :: FormatFeatureFlags
  , -- | @optimalTilingFeatures@ is a bitmask of
    -- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlagBits'
    -- specifying features supported by images created with a @tiling@
    -- parameter of 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL'.
    optimalTilingFeatures :: FormatFeatureFlags
  , -- | @bufferFeatures@ is a bitmask of
    -- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlagBits'
    -- specifying features supported by buffers.
    bufferFeatures :: FormatFeatureFlags
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (FormatProperties)
#endif
deriving instance Show FormatProperties

instance ToCStruct FormatProperties where
  withCStruct x f = allocaBytesAligned 12 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p FormatProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr FormatFeatureFlags)) (linearTilingFeatures)
    poke ((p `plusPtr` 4 :: Ptr FormatFeatureFlags)) (optimalTilingFeatures)
    poke ((p `plusPtr` 8 :: Ptr FormatFeatureFlags)) (bufferFeatures)
    f
  cStructSize = 12
  cStructAlignment = 4
  pokeZeroCStruct _ f = f

instance FromCStruct FormatProperties where
  peekCStruct p = do
    linearTilingFeatures <- peek @FormatFeatureFlags ((p `plusPtr` 0 :: Ptr FormatFeatureFlags))
    optimalTilingFeatures <- peek @FormatFeatureFlags ((p `plusPtr` 4 :: Ptr FormatFeatureFlags))
    bufferFeatures <- peek @FormatFeatureFlags ((p `plusPtr` 8 :: Ptr FormatFeatureFlags))
    pure $ FormatProperties
             linearTilingFeatures optimalTilingFeatures bufferFeatures

instance Storable FormatProperties where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero FormatProperties where
  zero = FormatProperties
           zero
           zero
           zero


-- | VkImageFormatProperties - Structure specifying an image format
-- properties
--
-- = Members
--
-- -   @maxExtent@ are the maximum image dimensions. See the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extentperimagetype Allowed Extent Values>
--     section below for how these values are constrained by @type@.
--
-- -   @maxMipLevels@ is the maximum number of mipmap levels.
--     @maxMipLevels@ /must/ be equal to the number of levels in the
--     complete mipmap chain based on the @maxExtent.width@,
--     @maxExtent.height@, and @maxExtent.depth@, except when one of the
--     following conditions is true, in which case it /may/ instead be @1@:
--
--     -   'getPhysicalDeviceImageFormatProperties'::@tiling@ was
--         'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR'
--
--     -   'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2'::@tiling@
--         was
--         'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT'
--
--     -   the
--         'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2'::@pNext@
--         chain included a
--         'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.PhysicalDeviceExternalImageFormatInfo'
--         structure with a handle type included in the @handleTypes@
--         member for which mipmap image support is not required
--
--     -   image @format@ is one of those listed in
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion>
--
--     -   @flags@ contains
--         'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--
-- -   @maxArrayLayers@ is the maximum number of array layers.
--     @maxArrayLayers@ /must/ be no less than
--     'PhysicalDeviceLimits'::@maxImageArrayLayers@, except when one of
--     the following conditions is true, in which case it /may/ instead be
--     @1@:
--
--     -   @tiling@ is
--         'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR'
--
--     -   @tiling@ is
--         'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL' and
--         @type@ is 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D'
--
--     -   @format@ is one of those listed in
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion>
--
-- -   If @tiling@ is
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT',
--     then @maxArrayLayers@ /must/ not be 0.
--
-- -   @sampleCounts@ is a bitmask of
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits'
--     specifying all the supported sample counts for this image as
--     described
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-supported-sample-counts below>.
--
-- -   @maxResourceSize@ is an upper bound on the total image size in
--     bytes, inclusive of all image subresources. Implementations /may/
--     have an address space limit on total size of a resource, which is
--     advertised by this property. @maxResourceSize@ /must/ be at least
--     231.
--
-- = Description
--
-- Note
--
-- There is no mechanism to query the size of an image before creating it,
-- to compare that size against @maxResourceSize@. If an application
-- attempts to create an image that exceeds this limit, the creation will
-- fail and 'Vulkan.Core10.Image.createImage' will return
-- 'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'. While the
-- advertised limit /must/ be at least 231, it /may/ not be possible to
-- create an image that approaches that size, particularly for
-- 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D'.
--
-- If the combination of parameters to
-- 'getPhysicalDeviceImageFormatProperties' is not supported by the
-- implementation for use in 'Vulkan.Core10.Image.createImage', then all
-- members of 'ImageFormatProperties' will be filled with zero.
--
-- Note
--
-- Filling 'ImageFormatProperties' with zero for unsupported formats is an
-- exception to the usual rule that output structures have undefined
-- contents on error. This exception was unintentional, but is preserved
-- for backwards compatibility.
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.FundamentalTypes.Extent3D',
-- 'Vulkan.Extensions.VK_NV_external_memory_capabilities.ExternalImageFormatPropertiesNV',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.ImageFormatProperties2',
-- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlags',
-- 'getPhysicalDeviceImageFormatProperties'
data ImageFormatProperties = ImageFormatProperties
  { -- No documentation found for Nested "VkImageFormatProperties" "maxExtent"
    maxExtent :: Extent3D
  , -- No documentation found for Nested "VkImageFormatProperties" "maxMipLevels"
    maxMipLevels :: Word32
  , -- No documentation found for Nested "VkImageFormatProperties" "maxArrayLayers"
    maxArrayLayers :: Word32
  , -- No documentation found for Nested "VkImageFormatProperties" "sampleCounts"
    sampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "VkImageFormatProperties" "maxResourceSize"
    maxResourceSize :: DeviceSize
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageFormatProperties)
#endif
deriving instance Show ImageFormatProperties

instance ToCStruct ImageFormatProperties where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageFormatProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Extent3D)) (maxExtent)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (maxMipLevels)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxArrayLayers)
    poke ((p `plusPtr` 20 :: Ptr SampleCountFlags)) (sampleCounts)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (maxResourceSize)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Extent3D)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct ImageFormatProperties where
  peekCStruct p = do
    maxExtent <- peekCStruct @Extent3D ((p `plusPtr` 0 :: Ptr Extent3D))
    maxMipLevels <- peek @Word32 ((p `plusPtr` 12 :: Ptr Word32))
    maxArrayLayers <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    sampleCounts <- peek @SampleCountFlags ((p `plusPtr` 20 :: Ptr SampleCountFlags))
    maxResourceSize <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    pure $ ImageFormatProperties
             maxExtent maxMipLevels maxArrayLayers sampleCounts maxResourceSize

instance Storable ImageFormatProperties where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageFormatProperties where
  zero = ImageFormatProperties
           zero
           zero
           zero
           zero
           zero


-- | VkPhysicalDeviceFeatures - Structure describing the fine-grained
-- features that can be supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceFeatures' structure describe the
-- following features:
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Device.DeviceCreateInfo',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- 'getPhysicalDeviceFeatures'
data PhysicalDeviceFeatures = PhysicalDeviceFeatures
  { -- | #features-robustBufferAccess# @robustBufferAccess@ specifies that
    -- accesses to buffers are bounds-checked against the range of the buffer
    -- descriptor (as determined by
    -- 'Vulkan.Core10.DescriptorSet.DescriptorBufferInfo'::@range@,
    -- 'Vulkan.Core10.BufferView.BufferViewCreateInfo'::@range@, or the size of
    -- the buffer). Out of bounds accesses /must/ not cause application
    -- termination, and the effects of shader loads, stores, and atomics /must/
    -- conform to an implementation-dependent behavior as described below.
    --
    -- -   A buffer access is considered to be out of bounds if any of the
    --     following are true:
    --
    --     -   The pointer was formed by @OpImageTexelPointer@ and the
    --         coordinate is less than zero or greater than or equal to the
    --         number of whole elements in the bound range.
    --
    --     -   The pointer was not formed by @OpImageTexelPointer@ and the
    --         object pointed to is not wholly contained within the bound
    --         range. This includes accesses performed via /variable pointers/
    --         where the buffer descriptor being accessed cannot be statically
    --         determined. Uninitialized pointers and pointers equal to
    --         @OpConstantNull@ are treated as pointing to a zero-sized object,
    --         so all accesses through such pointers are considered to be out
    --         of bounds. Buffer accesses through buffer device addresses are
    --         not bounds-checked. If the
    --         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-cooperativeMatrixRobustBufferAccess cooperativeMatrixRobustBufferAccess>
    --         feature is not enabled, then accesses using
    --         @OpCooperativeMatrixLoadNV@ and @OpCooperativeMatrixStoreNV@
    --         /may/ not be bounds-checked.
    --
    --         Note
    --
    --         If a SPIR-V @OpLoad@ instruction loads a structure and the tail
    --         end of the structure is out of bounds, then all members of the
    --         structure are considered out of bounds even if the members at
    --         the end are not statically used.
    --
    --     -   If
    --         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess2 robustBufferAccess2>
    --         is not enabled and any buffer access is determined to be out of
    --         bounds, then any other access of the same type (load, store, or
    --         atomic) to the same buffer that accesses an address less than 16
    --         bytes away from the out of bounds address /may/ also be
    --         considered out of bounds.
    --
    --     -   If the access is a load that reads from the same memory
    --         locations as a prior store in the same shader invocation, with
    --         no other intervening accesses to the same memory locations in
    --         that shader invocation, then the result of the load /may/ be the
    --         value stored by the store instruction, even if the access is out
    --         of bounds. If the load is @Volatile@, then an out of bounds load
    --         /must/ return the appropriate out of bounds value.
    --
    -- -   Accesses to descriptors written with a
    --     'Vulkan.Core10.APIConstants.NULL_HANDLE' resource or view are not
    --     considered to be out of bounds. Instead, each type of descriptor
    --     access defines a specific behavior for accesses to a null
    --     descriptor.
    --
    -- -   Out-of-bounds buffer loads will return any of the following values:
    --
    --     -   If the access is to a uniform buffer and
    --         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess2 robustBufferAccess2>
    --         is enabled, loads of offsets between the end of the descriptor
    --         range and the end of the descriptor range rounded up to a
    --         multiple of
    --         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-robustUniformBufferAccessSizeAlignment robustUniformBufferAccessSizeAlignment>
    --         bytes /must/ return either zero values or the contents of the
    --         memory at the offset being loaded. Loads of offsets past the
    --         descriptor range rounded up to a multiple of
    --         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-robustUniformBufferAccessSizeAlignment robustUniformBufferAccessSizeAlignment>
    --         bytes /must/ return zero values.
    --
    --     -   If the access is to a storage buffer and
    --         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess2 robustBufferAccess2>
    --         is enabled, loads of offsets between the end of the descriptor
    --         range and the end of the descriptor range rounded up to a
    --         multiple of
    --         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-robustStorageBufferAccessSizeAlignment robustStorageBufferAccessSizeAlignment>
    --         bytes /must/ return either zero values or the contents of the
    --         memory at the offset being loaded. Loads of offsets past the
    --         descriptor range rounded up to a multiple of
    --         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-robustStorageBufferAccessSizeAlignment robustStorageBufferAccessSizeAlignment>
    --         bytes /must/ return zero values. Similarly, stores to addresses
    --         between the end of the descriptor range and the end of the
    --         descriptor range rounded up to a multiple of
    --         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-robustStorageBufferAccessSizeAlignment robustStorageBufferAccessSizeAlignment>
    --         bytes /may/ be discarded.
    --
    --     -   Non-atomic accesses to storage buffers that are a multiple of 32
    --         bits /may/ be decomposed into 32-bit accesses that are
    --         individually bounds-checked.
    --
    --     -   If the access is to an index buffer and
    --         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess2 robustBufferAccess2>
    --         is enabled, zero values /must/ be returned.
    --
    --     -   If the access is to a uniform texel buffer or storage texel
    --         buffer and
    --         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess2 robustBufferAccess2>
    --         is enabled, zero values /must/ be returned, and then
    --         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-conversion-to-rgba Conversion to RGBA>
    --         is applied based on the buffer view’s format.
    --
    --     -   Values from anywhere within the memory range(s) bound to the
    --         buffer (possibly including bytes of memory past the end of the
    --         buffer, up to the end of the bound range).
    --
    --     -   Zero values, or (0,0,0,x) vectors for vector reads where x is a
    --         valid value represented in the type of the vector components and
    --         /may/ be any of:
    --
    --         -   0, 1, or the maximum representable positive integer value,
    --             for signed or unsigned integer components
    --
    --         -   0.0 or 1.0, for floating-point components
    --
    -- -   Out-of-bounds writes /may/ modify values within the memory range(s)
    --     bound to the buffer, but /must/ not modify any other memory.
    --
    --     -   If
    --         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess2 robustBufferAccess2>
    --         is enabled, out of bounds writes /must/ not modify any memory.
    --
    -- -   Out-of-bounds atomics /may/ modify values within the memory range(s)
    --     bound to the buffer, but /must/ not modify any other memory, and
    --     return an undefined value.
    --
    --     -   If
    --         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess2 robustBufferAccess2>
    --         is enabled, out of bounds atomics /must/ not modify any memory,
    --         and return an undefined value.
    --
    -- -   If
    --     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess2 robustBufferAccess2>
    --     is disabled, vertex input attributes are considered out of bounds if
    --     the offset of the attribute in the bound vertex buffer range plus
    --     the size of the attribute is greater than either:
    --
    --     -   @vertexBufferRangeSize@, if @bindingStride@ == 0; or
    --
    --     -   (@vertexBufferRangeSize@ - (@vertexBufferRangeSize@ %
    --         @bindingStride@))
    --
    --     where @vertexBufferRangeSize@ is the byte size of the memory range
    --     bound to the vertex buffer binding and @bindingStride@ is the byte
    --     stride of the corresponding vertex input binding. Further, if any
    --     vertex input attribute using a specific vertex input binding is out
    --     of bounds, then all vertex input attributes using that vertex input
    --     binding for that vertex shader invocation are considered out of
    --     bounds.
    --
    --     -   If a vertex input attribute is out of bounds, it will be
    --         assigned one of the following values:
    --
    --         -   Values from anywhere within the memory range(s) bound to the
    --             buffer, converted according to the format of the attribute.
    --
    --         -   Zero values, format converted according to the format of the
    --             attribute.
    --
    --         -   Zero values, or (0,0,0,x) vectors, as described above.
    --
    -- -   If
    --     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess2 robustBufferAccess2>
    --     is enabled, vertex input attributes are considered out of bounds if
    --     the offset of the attribute in the bound vertex buffer range plus
    --     the size of the attribute is greater than the byte size of the
    --     memory range bound to the vertex buffer binding.
    --
    --     -   If a vertex input attribute is out of bounds, the
    --         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fxvertex-input-extraction raw data>
    --         extracted are zero values, and missing G, B, or A components are
    --         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fxvertex-input-extraction filled with (0,0,1)>.
    --
    -- -   If @robustBufferAccess@ is not enabled, applications /must/ not
    --     perform out of bounds accesses.
    robustBufferAccess :: Bool
  , -- | #features-fullDrawIndexUint32# @fullDrawIndexUint32@ specifies the full
    -- 32-bit range of indices is supported for indexed draw calls when using a
    -- 'Vulkan.Core10.Enums.IndexType.IndexType' of
    -- 'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT32'.
    -- @maxDrawIndexedIndexValue@ is the maximum index value that /may/ be used
    -- (aside from the primitive restart index, which is always 232-1 when the
    -- 'Vulkan.Core10.Enums.IndexType.IndexType' is
    -- 'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT32'). If this feature is
    -- supported, @maxDrawIndexedIndexValue@ /must/ be 232-1; otherwise it
    -- /must/ be no smaller than 224-1. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxDrawIndexedIndexValue maxDrawIndexedIndexValue>.
    fullDrawIndexUint32 :: Bool
  , -- | #features-imageCubeArray# @imageCubeArray@ specifies whether image views
    -- with a 'Vulkan.Core10.Enums.ImageViewType.ImageViewType' of
    -- 'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY' /can/ be
    -- created, and that the corresponding @SampledCubeArray@ and
    -- @ImageCubeArray@ SPIR-V capabilities /can/ be used in shader code.
    imageCubeArray :: Bool
  , -- | #features-independentBlend# @independentBlend@ specifies whether the
    -- 'Vulkan.Core10.Pipeline.PipelineColorBlendAttachmentState' settings are
    -- controlled independently per-attachment. If this feature is not enabled,
    -- the 'Vulkan.Core10.Pipeline.PipelineColorBlendAttachmentState' settings
    -- for all color attachments /must/ be identical. Otherwise, a different
    -- 'Vulkan.Core10.Pipeline.PipelineColorBlendAttachmentState' /can/ be
    -- provided for each bound color attachment.
    independentBlend :: Bool
  , -- | #features-geometryShader# @geometryShader@ specifies whether geometry
    -- shaders are supported. If this feature is not enabled, the
    -- 'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT' and
    -- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
    -- enum values /must/ not be used. This also specifies whether shader
    -- modules /can/ declare the @Geometry@ capability.
    geometryShader :: Bool
  , -- | #features-tessellationShader# @tessellationShader@ specifies whether
    -- tessellation control and evaluation shaders are supported. If this
    -- feature is not enabled, the
    -- 'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT',
    -- 'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT',
    -- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT',
    -- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT',
    -- and
    -- 'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO'
    -- enum values /must/ not be used. This also specifies whether shader
    -- modules /can/ declare the @Tessellation@ capability.
    tessellationShader :: Bool
  , -- | #features-sampleRateShading# @sampleRateShading@ specifies whether
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-sampleshading Sample Shading>
    -- and multisample interpolation are supported. If this feature is not
    -- enabled, the @sampleShadingEnable@ member of the
    -- 'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo' structure
    -- /must/ be set to 'Vulkan.Core10.FundamentalTypes.FALSE' and the
    -- @minSampleShading@ member is ignored. This also specifies whether shader
    -- modules /can/ declare the @SampleRateShading@ capability.
    sampleRateShading :: Bool
  , -- | #features-dualSrcBlend# @dualSrcBlend@ specifies whether blend
    -- operations which take two sources are supported. If this feature is not
    -- enabled, the 'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_SRC1_COLOR',
    -- 'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_SRC1_COLOR',
    -- 'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_SRC1_ALPHA', and
    -- 'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA' enum
    -- values /must/ not be used as source or destination blending factors. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#framebuffer-dsb>.
    dualSrcBlend :: Bool
  , -- | #features-logicOp# @logicOp@ specifies whether logic operations are
    -- supported. If this feature is not enabled, the @logicOpEnable@ member of
    -- the 'Vulkan.Core10.Pipeline.PipelineColorBlendStateCreateInfo' structure
    -- /must/ be set to 'Vulkan.Core10.FundamentalTypes.FALSE', and the
    -- @logicOp@ member is ignored.
    logicOp :: Bool
  , -- | #features-multiDrawIndirect# @multiDrawIndirect@ specifies whether
    -- multiple draw indirect is supported. If this feature is not enabled, the
    -- @drawCount@ parameter to the
    -- 'Vulkan.Core10.CommandBufferBuilding.cmdDrawIndirect' and
    -- 'Vulkan.Core10.CommandBufferBuilding.cmdDrawIndexedIndirect' commands
    -- /must/ be 0 or 1. The @maxDrawIndirectCount@ member of the
    -- 'PhysicalDeviceLimits' structure /must/ also be 1 if this feature is not
    -- supported. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxDrawIndirectCount maxDrawIndirectCount>.
    multiDrawIndirect :: Bool
  , -- | #features-drawIndirectFirstInstance# @drawIndirectFirstInstance@
    -- specifies whether indirect draw calls support the @firstInstance@
    -- parameter. If this feature is not enabled, the @firstInstance@ member of
    -- all 'Vulkan.Core10.OtherTypes.DrawIndirectCommand' and
    -- 'Vulkan.Core10.OtherTypes.DrawIndexedIndirectCommand' structures that
    -- are provided to the
    -- 'Vulkan.Core10.CommandBufferBuilding.cmdDrawIndirect' and
    -- 'Vulkan.Core10.CommandBufferBuilding.cmdDrawIndexedIndirect' commands
    -- /must/ be 0.
    drawIndirectFirstInstance :: Bool
  , -- | #features-depthClamp# @depthClamp@ specifies whether depth clamping is
    -- supported. If this feature is not enabled, the @depthClampEnable@ member
    -- of the 'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo'
    -- structure /must/ be set to 'Vulkan.Core10.FundamentalTypes.FALSE'.
    -- Otherwise, setting @depthClampEnable@ to
    -- 'Vulkan.Core10.FundamentalTypes.TRUE' will enable depth clamping.
    depthClamp :: Bool
  , -- | #features-depthBiasClamp# @depthBiasClamp@ specifies whether depth bias
    -- clamping is supported. If this feature is not enabled, the
    -- @depthBiasClamp@ member of the
    -- 'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo' structure
    -- /must/ be set to 0.0 unless the
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BIAS' dynamic
    -- state is enabled, and the @depthBiasClamp@ parameter to
    -- 'Vulkan.Core10.CommandBufferBuilding.cmdSetDepthBias' /must/ be set to
    -- 0.0.
    depthBiasClamp :: Bool
  , -- | #features-fillModeNonSolid# @fillModeNonSolid@ specifies whether point
    -- and wireframe fill modes are supported. If this feature is not enabled,
    -- the 'Vulkan.Core10.Enums.PolygonMode.POLYGON_MODE_POINT' and
    -- 'Vulkan.Core10.Enums.PolygonMode.POLYGON_MODE_LINE' enum values /must/
    -- not be used.
    fillModeNonSolid :: Bool
  , -- | #features-depthBounds# @depthBounds@ specifies whether depth bounds
    -- tests are supported. If this feature is not enabled, the
    -- @depthBoundsTestEnable@ member of the
    -- 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo' structure
    -- /must/ be set to 'Vulkan.Core10.FundamentalTypes.FALSE'. When
    -- @depthBoundsTestEnable@ is set to
    -- 'Vulkan.Core10.FundamentalTypes.FALSE', the @minDepthBounds@ and
    -- @maxDepthBounds@ members of the
    -- 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo' structure
    -- are ignored.
    depthBounds :: Bool
  , -- | #features-wideLines# @wideLines@ specifies whether lines with width
    -- other than 1.0 are supported. If this feature is not enabled, the
    -- @lineWidth@ member of the
    -- 'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo' structure
    -- /must/ be set to 1.0 unless the
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_WIDTH' dynamic
    -- state is enabled, and the @lineWidth@ parameter to
    -- 'Vulkan.Core10.CommandBufferBuilding.cmdSetLineWidth' /must/ be set to
    -- 1.0. When this feature is supported, the range and granularity of
    -- supported line widths are indicated by the @lineWidthRange@ and
    -- @lineWidthGranularity@ members of the 'PhysicalDeviceLimits' structure,
    -- respectively.
    wideLines :: Bool
  , -- | #features-largePoints# @largePoints@ specifies whether points with size
    -- greater than 1.0 are supported. If this feature is not enabled, only a
    -- point size of 1.0 written by a shader is supported. The range and
    -- granularity of supported point sizes are indicated by the
    -- @pointSizeRange@ and @pointSizeGranularity@ members of the
    -- 'PhysicalDeviceLimits' structure, respectively.
    largePoints :: Bool
  , -- | #features-alphaToOne# @alphaToOne@ specifies whether the implementation
    -- is able to replace the alpha value of the fragment shader color output
    -- in the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-covg Multisample Coverage>
    -- fragment operation. If this feature is not enabled, then the
    -- @alphaToOneEnable@ member of the
    -- 'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo' structure
    -- /must/ be set to 'Vulkan.Core10.FundamentalTypes.FALSE'. Otherwise
    -- setting @alphaToOneEnable@ to 'Vulkan.Core10.FundamentalTypes.TRUE' will
    -- enable alpha-to-one behavior.
    alphaToOne :: Bool
  , -- | #features-multiViewport# @multiViewport@ specifies whether more than one
    -- viewport is supported. If this feature is not enabled:
    --
    -- -   The @viewportCount@ and @scissorCount@ members of the
    --     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo' structure
    --     /must/ be set to 1.
    --
    -- -   The @firstViewport@ and @viewportCount@ parameters to the
    --     'Vulkan.Core10.CommandBufferBuilding.cmdSetViewport' command /must/
    --     be set to 0 and 1, respectively.
    --
    -- -   The @firstScissor@ and @scissorCount@ parameters to the
    --     'Vulkan.Core10.CommandBufferBuilding.cmdSetScissor' command /must/
    --     be set to 0 and 1, respectively.
    --
    -- -   The @exclusiveScissorCount@ member of the
    --     'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV'
    --     structure /must/ be set to 0 or 1.
    --
    -- -   The @firstExclusiveScissor@ and @exclusiveScissorCount@ parameters
    --     to the
    --     'Vulkan.Extensions.VK_NV_scissor_exclusive.cmdSetExclusiveScissorNV'
    --     command /must/ be set to 0 and 1, respectively.
    multiViewport :: Bool
  , -- | #features-samplerAnisotropy# @samplerAnisotropy@ specifies whether
    -- anisotropic filtering is supported. If this feature is not enabled, the
    -- @anisotropyEnable@ member of the
    -- 'Vulkan.Core10.Sampler.SamplerCreateInfo' structure /must/ be
    -- 'Vulkan.Core10.FundamentalTypes.FALSE'.
    samplerAnisotropy :: Bool
  , -- | #features-textureCompressionETC2# @textureCompressionETC2@ specifies
    -- whether all of the ETC2 and EAC compressed texture formats are
    -- supported. If this feature is enabled, then the
    -- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_BIT',
    -- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_BLIT_SRC_BIT'
    -- and
    -- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
    -- features /must/ be supported in @optimalTilingFeatures@ for the
    -- following formats:
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ETC2_R8G8B8_UNORM_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ETC2_R8G8B8_SRGB_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_EAC_R11_UNORM_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_EAC_R11_SNORM_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_EAC_R11G11_UNORM_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_EAC_R11G11_SNORM_BLOCK'
    --
    -- To query for additional properties, or if the feature is not enabled,
    -- 'getPhysicalDeviceFormatProperties' and
    -- 'getPhysicalDeviceImageFormatProperties' /can/ be used to check for
    -- supported properties of individual formats as normal.
    textureCompressionETC2 :: Bool
  , -- | #features-textureCompressionASTC_LDR# @textureCompressionASTC_LDR@
    -- specifies whether all of the ASTC LDR compressed texture formats are
    -- supported. If this feature is enabled, then the
    -- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_BIT',
    -- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_BLIT_SRC_BIT'
    -- and
    -- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
    -- features /must/ be supported in @optimalTilingFeatures@ for the
    -- following formats:
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_4x4_UNORM_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_4x4_SRGB_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_5x4_UNORM_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_5x4_SRGB_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_5x5_UNORM_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_5x5_SRGB_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_6x5_UNORM_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_6x5_SRGB_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_6x6_UNORM_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_6x6_SRGB_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_8x5_UNORM_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_8x5_SRGB_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_8x6_UNORM_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_8x6_SRGB_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_8x8_UNORM_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_8x8_SRGB_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_10x5_UNORM_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_10x5_SRGB_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_10x6_UNORM_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_10x6_SRGB_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_10x8_UNORM_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_10x8_SRGB_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_10x10_UNORM_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_10x10_SRGB_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_12x10_UNORM_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_12x10_SRGB_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_12x12_UNORM_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_12x12_SRGB_BLOCK'
    --
    -- To query for additional properties, or if the feature is not enabled,
    -- 'getPhysicalDeviceFormatProperties' and
    -- 'getPhysicalDeviceImageFormatProperties' /can/ be used to check for
    -- supported properties of individual formats as normal.
    textureCompressionASTC_LDR :: Bool
  , -- | #features-textureCompressionBC# @textureCompressionBC@ specifies whether
    -- all of the BC compressed texture formats are supported. If this feature
    -- is enabled, then the
    -- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_BIT',
    -- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_BLIT_SRC_BIT'
    -- and
    -- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
    -- features /must/ be supported in @optimalTilingFeatures@ for the
    -- following formats:
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_BC1_RGB_UNORM_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_BC1_RGB_SRGB_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_BC1_RGBA_UNORM_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_BC1_RGBA_SRGB_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_BC2_UNORM_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_BC2_SRGB_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_BC3_UNORM_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_BC3_SRGB_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_BC4_UNORM_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_BC4_SNORM_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_BC5_UNORM_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_BC5_SNORM_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_BC6H_UFLOAT_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_BC6H_SFLOAT_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_BC7_UNORM_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_BC7_SRGB_BLOCK'
    --
    -- To query for additional properties, or if the feature is not enabled,
    -- 'getPhysicalDeviceFormatProperties' and
    -- 'getPhysicalDeviceImageFormatProperties' /can/ be used to check for
    -- supported properties of individual formats as normal.
    textureCompressionBC :: Bool
  , -- | #features-occlusionQueryPrecise# @occlusionQueryPrecise@ specifies
    -- whether occlusion queries returning actual sample counts are supported.
    -- Occlusion queries are created in a 'Vulkan.Core10.Handles.QueryPool' by
    -- specifying the @queryType@ of
    -- 'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_OCCLUSION' in the
    -- 'Vulkan.Core10.Query.QueryPoolCreateInfo' structure which is passed to
    -- 'Vulkan.Core10.Query.createQueryPool'. If this feature is enabled,
    -- queries of this type /can/ enable
    -- 'Vulkan.Core10.Enums.QueryControlFlagBits.QUERY_CONTROL_PRECISE_BIT' in
    -- the @flags@ parameter to
    -- 'Vulkan.Core10.CommandBufferBuilding.cmdBeginQuery'. If this feature is
    -- not supported, the implementation supports only boolean occlusion
    -- queries. When any samples are passed, boolean queries will return a
    -- non-zero result value, otherwise a result value of zero is returned.
    -- When this feature is enabled and
    -- 'Vulkan.Core10.Enums.QueryControlFlagBits.QUERY_CONTROL_PRECISE_BIT' is
    -- set, occlusion queries will report the actual number of samples passed.
    occlusionQueryPrecise :: Bool
  , -- | #features-pipelineStatisticsQuery# @pipelineStatisticsQuery@ specifies
    -- whether the pipeline statistics queries are supported. If this feature
    -- is not enabled, queries of type
    -- 'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PIPELINE_STATISTICS' /cannot/
    -- be created, and none of the
    -- 'Vulkan.Core10.Enums.QueryPipelineStatisticFlagBits.QueryPipelineStatisticFlagBits'
    -- bits /can/ be set in the @pipelineStatistics@ member of the
    -- 'Vulkan.Core10.Query.QueryPoolCreateInfo' structure.
    pipelineStatisticsQuery :: Bool
  , -- | #features-vertexPipelineStoresAndAtomics#
    -- @vertexPipelineStoresAndAtomics@ specifies whether storage buffers and
    -- images support stores and atomic operations in the vertex, tessellation,
    -- and geometry shader stages. If this feature is not enabled, all storage
    -- image, storage texel buffers, and storage buffer variables used by these
    -- stages in shader modules /must/ be decorated with the @NonWritable@
    -- decoration (or the @readonly@ memory qualifier in GLSL).
    vertexPipelineStoresAndAtomics :: Bool
  , -- | #features-fragmentStoresAndAtomics# @fragmentStoresAndAtomics@ specifies
    -- whether storage buffers and images support stores and atomic operations
    -- in the fragment shader stage. If this feature is not enabled, all
    -- storage image, storage texel buffers, and storage buffer variables used
    -- by the fragment stage in shader modules /must/ be decorated with the
    -- @NonWritable@ decoration (or the @readonly@ memory qualifier in GLSL).
    fragmentStoresAndAtomics :: Bool
  , -- | #features-shaderTessellationAndGeometryPointSize#
    -- @shaderTessellationAndGeometryPointSize@ specifies whether the
    -- @PointSize@ built-in decoration is available in the tessellation
    -- control, tessellation evaluation, and geometry shader stages. If this
    -- feature is not enabled, members decorated with the @PointSize@ built-in
    -- decoration /must/ not be read from or written to and all points written
    -- from a tessellation or geometry shader will have a size of 1.0. This
    -- also specifies whether shader modules /can/ declare the
    -- @TessellationPointSize@ capability for tessellation control and
    -- evaluation shaders, or if the shader modules /can/ declare the
    -- @GeometryPointSize@ capability for geometry shaders. An implementation
    -- supporting this feature /must/ also support one or both of the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellationShader>
    -- or
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometryShader>
    -- features.
    shaderTessellationAndGeometryPointSize :: Bool
  , -- | #features-shaderImageGatherExtended# @shaderImageGatherExtended@
    -- specifies whether the extended set of image gather instructions are
    -- available in shader code. If this feature is not enabled, the
    -- @OpImage@*@Gather@ instructions do not support the @Offset@ and
    -- @ConstOffsets@ operands. This also specifies whether shader modules
    -- /can/ declare the @ImageGatherExtended@ capability.
    shaderImageGatherExtended :: Bool
  , -- | #features-shaderStorageImageExtendedFormats#
    -- @shaderStorageImageExtendedFormats@ specifies whether all the “storage
    -- image extended formats” below are supported; if this feature is
    -- supported, then the
    -- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_BIT'
    -- /must/ be supported in @optimalTilingFeatures@ for the following
    -- formats:
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_R16G16_SFLOAT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_B10G11R11_UFLOAT_PACK32'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_R16_SFLOAT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_R16G16B16A16_UNORM'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_A2B10G10R10_UNORM_PACK32'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_R16G16_UNORM'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_R8G8_UNORM'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_R16_UNORM'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_R8_UNORM'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_R16G16B16A16_SNORM'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_R16G16_SNORM'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_R8G8_SNORM'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_R16_SNORM'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_R8_SNORM'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_R16G16_SINT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_R8G8_SINT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_R16_SINT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_R8_SINT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_A2B10G10R10_UINT_PACK32'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_R16G16_UINT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_R8G8_UINT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_R16_UINT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_R8_UINT'
    --
    -- Note
    --
    -- @shaderStorageImageExtendedFormats@ feature only adds a guarantee of
    -- format support, which is specified for the whole physical device.
    -- Therefore enabling or disabling the feature via
    -- 'Vulkan.Core10.Device.createDevice' has no practical effect.
    --
    -- To query for additional properties, or if the feature is not supported,
    -- 'getPhysicalDeviceFormatProperties' and
    -- 'getPhysicalDeviceImageFormatProperties' /can/ be used to check for
    -- supported properties of individual formats, as usual rules allow.
    --
    -- 'Vulkan.Core10.Enums.Format.FORMAT_R32G32_UINT',
    -- 'Vulkan.Core10.Enums.Format.FORMAT_R32G32_SINT', and
    -- 'Vulkan.Core10.Enums.Format.FORMAT_R32G32_SFLOAT' from
    -- @StorageImageExtendedFormats@ SPIR-V capability, are already covered by
    -- core Vulkan
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-mandatory-features-32bit mandatory format support>.
    shaderStorageImageExtendedFormats :: Bool
  , -- | #features-shaderStorageImageMultisample# @shaderStorageImageMultisample@
    -- specifies whether multisampled storage images are supported. If this
    -- feature is not enabled, images that are created with a @usage@ that
    -- includes
    -- 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_STORAGE_BIT' /must/
    -- be created with @samples@ equal to
    -- 'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'. This also
    -- specifies whether shader modules /can/ declare the
    -- @StorageImageMultisample@ and @ImageMSArray@ capabilities.
    shaderStorageImageMultisample :: Bool
  , -- | #features-shaderStorageImageReadWithoutFormat#
    -- @shaderStorageImageReadWithoutFormat@ specifies whether storage images
    -- require a format qualifier to be specified when reading from storage
    -- images. If this feature is not enabled, the @OpImageRead@ instruction
    -- /must/ not have an @OpTypeImage@ of @Unknown@. This also specifies
    -- whether shader modules /can/ declare the @StorageImageReadWithoutFormat@
    -- capability.
    shaderStorageImageReadWithoutFormat :: Bool
  , -- | #features-shaderStorageImageWriteWithoutFormat#
    -- @shaderStorageImageWriteWithoutFormat@ specifies whether storage images
    -- require a format qualifier to be specified when writing to storage
    -- images. If this feature is not enabled, the @OpImageWrite@ instruction
    -- /must/ not have an @OpTypeImage@ of @Unknown@. This also specifies
    -- whether shader modules /can/ declare the
    -- @StorageImageWriteWithoutFormat@ capability.
    shaderStorageImageWriteWithoutFormat :: Bool
  , -- | #features-shaderUniformBufferArrayDynamicIndexing#
    -- @shaderUniformBufferArrayDynamicIndexing@ specifies whether arrays of
    -- uniform buffers /can/ be indexed by /dynamically uniform/ integer
    -- expressions in shader code. If this feature is not enabled, resources
    -- with a descriptor type of
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER' or
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
    -- /must/ be indexed only by constant integral expressions when aggregated
    -- into arrays in shader code. This also specifies whether shader modules
    -- /can/ declare the @UniformBufferArrayDynamicIndexing@ capability.
    shaderUniformBufferArrayDynamicIndexing :: Bool
  , -- | #features-shaderSampledImageArrayDynamicIndexing#
    -- @shaderSampledImageArrayDynamicIndexing@ specifies whether arrays of
    -- samplers or sampled images /can/ be indexed by dynamically uniform
    -- integer expressions in shader code. If this feature is not enabled,
    -- resources with a descriptor type of
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLER',
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
    -- or 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE'
    -- /must/ be indexed only by constant integral expressions when aggregated
    -- into arrays in shader code. This also specifies whether shader modules
    -- /can/ declare the @SampledImageArrayDynamicIndexing@ capability.
    shaderSampledImageArrayDynamicIndexing :: Bool
  , -- | #features-shaderStorageBufferArrayDynamicIndexing#
    -- @shaderStorageBufferArrayDynamicIndexing@ specifies whether arrays of
    -- storage buffers /can/ be indexed by dynamically uniform integer
    -- expressions in shader code. If this feature is not enabled, resources
    -- with a descriptor type of
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER' or
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
    -- /must/ be indexed only by constant integral expressions when aggregated
    -- into arrays in shader code. This also specifies whether shader modules
    -- /can/ declare the @StorageBufferArrayDynamicIndexing@ capability.
    shaderStorageBufferArrayDynamicIndexing :: Bool
  , -- | #features-shaderStorageImageArrayDynamicIndexing#
    -- @shaderStorageImageArrayDynamicIndexing@ specifies whether arrays of
    -- storage images /can/ be indexed by dynamically uniform integer
    -- expressions in shader code. If this feature is not enabled, resources
    -- with a descriptor type of
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE'
    -- /must/ be indexed only by constant integral expressions when aggregated
    -- into arrays in shader code. This also specifies whether shader modules
    -- /can/ declare the @StorageImageArrayDynamicIndexing@ capability.
    shaderStorageImageArrayDynamicIndexing :: Bool
  , -- | #features-shaderClipDistance# @shaderClipDistance@ specifies whether
    -- clip distances are supported in shader code. If this feature is not
    -- enabled, any members decorated with the @ClipDistance@ built-in
    -- decoration /must/ not be read from or written to in shader modules. This
    -- also specifies whether shader modules /can/ declare the @ClipDistance@
    -- capability.
    shaderClipDistance :: Bool
  , -- | #features-shaderCullDistance# @shaderCullDistance@ specifies whether
    -- cull distances are supported in shader code. If this feature is not
    -- enabled, any members decorated with the @CullDistance@ built-in
    -- decoration /must/ not be read from or written to in shader modules. This
    -- also specifies whether shader modules /can/ declare the @CullDistance@
    -- capability.
    shaderCullDistance :: Bool
  , -- | #features-shaderFloat64# @shaderFloat64@ specifies whether 64-bit floats
    -- (doubles) are supported in shader code. If this feature is not enabled,
    -- 64-bit floating-point types /must/ not be used in shader code. This also
    -- specifies whether shader modules /can/ declare the @Float64@ capability.
    -- Declaring and using 64-bit floats is enabled for all storage classes
    -- that SPIR-V allows with the @Float64@ capability.
    shaderFloat64 :: Bool
  , -- | #features-shaderInt64# @shaderInt64@ specifies whether 64-bit integers
    -- (signed and unsigned) are supported in shader code. If this feature is
    -- not enabled, 64-bit integer types /must/ not be used in shader code.
    -- This also specifies whether shader modules /can/ declare the @Int64@
    -- capability. Declaring and using 64-bit integers is enabled for all
    -- storage classes that SPIR-V allows with the @Int64@ capability.
    shaderInt64 :: Bool
  , -- | #features-shaderInt16# @shaderInt16@ specifies whether 16-bit integers
    -- (signed and unsigned) are supported in shader code. If this feature is
    -- not enabled, 16-bit integer types /must/ not be used in shader code.
    -- This also specifies whether shader modules /can/ declare the @Int16@
    -- capability. However, this only enables a subset of the storage classes
    -- that SPIR-V allows for the @Int16@ SPIR-V capability: Declaring and
    -- using 16-bit integers in the @Private@, @Workgroup@, and @Function@
    -- storage classes is enabled, while declaring them in the interface
    -- storage classes (e.g., @UniformConstant@, @Uniform@, @StorageBuffer@,
    -- @Input@, @Output@, and @PushConstant@) is not enabled.
    shaderInt16 :: Bool
  , -- | #features-shaderResourceResidency# @shaderResourceResidency@ specifies
    -- whether image operations that return resource residency information are
    -- supported in shader code. If this feature is not enabled, the
    -- @OpImageSparse@* instructions /must/ not be used in shader code. This
    -- also specifies whether shader modules /can/ declare the
    -- @SparseResidency@ capability. The feature requires at least one of the
    -- @sparseResidency*@ features to be supported.
    shaderResourceResidency :: Bool
  , -- | #features-shaderResourceMinLod# @shaderResourceMinLod@ specifies whether
    -- image operations specifying the minimum resource LOD are supported in
    -- shader code. If this feature is not enabled, the @MinLod@ image operand
    -- /must/ not be used in shader code. This also specifies whether shader
    -- modules /can/ declare the @MinLod@ capability.
    shaderResourceMinLod :: Bool
  , -- | #features-sparseBinding# @sparseBinding@ specifies whether resource
    -- memory /can/ be managed at opaque sparse block level instead of at the
    -- object level. If this feature is not enabled, resource memory /must/ be
    -- bound only on a per-object basis using the
    -- 'Vulkan.Core10.MemoryManagement.bindBufferMemory' and
    -- 'Vulkan.Core10.MemoryManagement.bindImageMemory' commands. In this case,
    -- buffers and images /must/ not be created with
    -- 'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_BINDING_BIT'
    -- and
    -- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_BINDING_BIT'
    -- set in the @flags@ member of the 'Vulkan.Core10.Buffer.BufferCreateInfo'
    -- and 'Vulkan.Core10.Image.ImageCreateInfo' structures, respectively.
    -- Otherwise resource memory /can/ be managed as described in
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#sparsememory-sparseresourcefeatures Sparse Resource Features>.
    sparseBinding :: Bool
  , -- | #features-sparseResidencyBuffer# @sparseResidencyBuffer@ specifies
    -- whether the device /can/ access partially resident buffers. If this
    -- feature is not enabled, buffers /must/ not be created with
    -- 'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
    -- set in the @flags@ member of the 'Vulkan.Core10.Buffer.BufferCreateInfo'
    -- structure.
    sparseResidencyBuffer :: Bool
  , -- | #features-sparseResidencyImage2D# @sparseResidencyImage2D@ specifies
    -- whether the device /can/ access partially resident 2D images with 1
    -- sample per pixel. If this feature is not enabled, images with an
    -- @imageType@ of 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D' and
    -- @samples@ set to
    -- 'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT' /must/ not
    -- be created with
    -- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
    -- set in the @flags@ member of the 'Vulkan.Core10.Image.ImageCreateInfo'
    -- structure.
    sparseResidencyImage2D :: Bool
  , -- | #features-sparseResidencyImage3D# @sparseResidencyImage3D@ specifies
    -- whether the device /can/ access partially resident 3D images. If this
    -- feature is not enabled, images with an @imageType@ of
    -- 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D' /must/ not be created with
    -- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
    -- set in the @flags@ member of the 'Vulkan.Core10.Image.ImageCreateInfo'
    -- structure.
    sparseResidencyImage3D :: Bool
  , -- | #features-sparseResidency2Samples# @sparseResidency2Samples@ specifies
    -- whether the physical device /can/ access partially resident 2D images
    -- with 2 samples per pixel. If this feature is not enabled, images with an
    -- @imageType@ of 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D' and
    -- @samples@ set to
    -- 'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_2_BIT' /must/ not
    -- be created with
    -- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
    -- set in the @flags@ member of the 'Vulkan.Core10.Image.ImageCreateInfo'
    -- structure.
    sparseResidency2Samples :: Bool
  , -- | #features-sparseResidency4Samples# @sparseResidency4Samples@ specifies
    -- whether the physical device /can/ access partially resident 2D images
    -- with 4 samples per pixel. If this feature is not enabled, images with an
    -- @imageType@ of 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D' and
    -- @samples@ set to
    -- 'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_4_BIT' /must/ not
    -- be created with
    -- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
    -- set in the @flags@ member of the 'Vulkan.Core10.Image.ImageCreateInfo'
    -- structure.
    sparseResidency4Samples :: Bool
  , -- | #features-sparseResidency8Samples# @sparseResidency8Samples@ specifies
    -- whether the physical device /can/ access partially resident 2D images
    -- with 8 samples per pixel. If this feature is not enabled, images with an
    -- @imageType@ of 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D' and
    -- @samples@ set to
    -- 'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_8_BIT' /must/ not
    -- be created with
    -- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
    -- set in the @flags@ member of the 'Vulkan.Core10.Image.ImageCreateInfo'
    -- structure.
    sparseResidency8Samples :: Bool
  , -- | #features-sparseResidency16Samples# @sparseResidency16Samples@ specifies
    -- whether the physical device /can/ access partially resident 2D images
    -- with 16 samples per pixel. If this feature is not enabled, images with
    -- an @imageType@ of 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D' and
    -- @samples@ set to
    -- 'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_16_BIT' /must/ not
    -- be created with
    -- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
    -- set in the @flags@ member of the 'Vulkan.Core10.Image.ImageCreateInfo'
    -- structure.
    sparseResidency16Samples :: Bool
  , -- | #features-sparseResidencyAliased# @sparseResidencyAliased@ specifies
    -- whether the physical device /can/ correctly access data aliased into
    -- multiple locations. If this feature is not enabled, the
    -- 'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_ALIASED_BIT'
    -- and
    -- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_ALIASED_BIT'
    -- enum values /must/ not be used in @flags@ members of the
    -- 'Vulkan.Core10.Buffer.BufferCreateInfo' and
    -- 'Vulkan.Core10.Image.ImageCreateInfo' structures, respectively.
    sparseResidencyAliased :: Bool
  , -- | #features-variableMultisampleRate# @variableMultisampleRate@ specifies
    -- whether all pipelines that will be bound to a command buffer during a
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-noattachments subpass which uses no attachments>
    -- /must/ have the same value for
    -- 'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo'::@rasterizationSamples@.
    -- If set to 'Vulkan.Core10.FundamentalTypes.TRUE', the implementation
    -- supports variable multisample rates in a subpass which uses no
    -- attachments. If set to 'Vulkan.Core10.FundamentalTypes.FALSE', then all
    -- pipelines bound in such a subpass /must/ have the same multisample rate.
    -- This has no effect in situations where a subpass uses any attachments.
    variableMultisampleRate :: Bool
  , -- | #features-inheritedQueries# @inheritedQueries@ specifies whether a
    -- secondary command buffer /may/ be executed while a query is active.
    inheritedQueries :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceFeatures)
#endif
deriving instance Show PhysicalDeviceFeatures

instance ToCStruct PhysicalDeviceFeatures where
  withCStruct x f = allocaBytesAligned 220 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceFeatures{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Bool32)) (boolToBool32 (robustBufferAccess))
    poke ((p `plusPtr` 4 :: Ptr Bool32)) (boolToBool32 (fullDrawIndexUint32))
    poke ((p `plusPtr` 8 :: Ptr Bool32)) (boolToBool32 (imageCubeArray))
    poke ((p `plusPtr` 12 :: Ptr Bool32)) (boolToBool32 (independentBlend))
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (geometryShader))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (tessellationShader))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (sampleRateShading))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (dualSrcBlend))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (logicOp))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (multiDrawIndirect))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (drawIndirectFirstInstance))
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (depthClamp))
    poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (depthBiasClamp))
    poke ((p `plusPtr` 52 :: Ptr Bool32)) (boolToBool32 (fillModeNonSolid))
    poke ((p `plusPtr` 56 :: Ptr Bool32)) (boolToBool32 (depthBounds))
    poke ((p `plusPtr` 60 :: Ptr Bool32)) (boolToBool32 (wideLines))
    poke ((p `plusPtr` 64 :: Ptr Bool32)) (boolToBool32 (largePoints))
    poke ((p `plusPtr` 68 :: Ptr Bool32)) (boolToBool32 (alphaToOne))
    poke ((p `plusPtr` 72 :: Ptr Bool32)) (boolToBool32 (multiViewport))
    poke ((p `plusPtr` 76 :: Ptr Bool32)) (boolToBool32 (samplerAnisotropy))
    poke ((p `plusPtr` 80 :: Ptr Bool32)) (boolToBool32 (textureCompressionETC2))
    poke ((p `plusPtr` 84 :: Ptr Bool32)) (boolToBool32 (textureCompressionASTC_LDR))
    poke ((p `plusPtr` 88 :: Ptr Bool32)) (boolToBool32 (textureCompressionBC))
    poke ((p `plusPtr` 92 :: Ptr Bool32)) (boolToBool32 (occlusionQueryPrecise))
    poke ((p `plusPtr` 96 :: Ptr Bool32)) (boolToBool32 (pipelineStatisticsQuery))
    poke ((p `plusPtr` 100 :: Ptr Bool32)) (boolToBool32 (vertexPipelineStoresAndAtomics))
    poke ((p `plusPtr` 104 :: Ptr Bool32)) (boolToBool32 (fragmentStoresAndAtomics))
    poke ((p `plusPtr` 108 :: Ptr Bool32)) (boolToBool32 (shaderTessellationAndGeometryPointSize))
    poke ((p `plusPtr` 112 :: Ptr Bool32)) (boolToBool32 (shaderImageGatherExtended))
    poke ((p `plusPtr` 116 :: Ptr Bool32)) (boolToBool32 (shaderStorageImageExtendedFormats))
    poke ((p `plusPtr` 120 :: Ptr Bool32)) (boolToBool32 (shaderStorageImageMultisample))
    poke ((p `plusPtr` 124 :: Ptr Bool32)) (boolToBool32 (shaderStorageImageReadWithoutFormat))
    poke ((p `plusPtr` 128 :: Ptr Bool32)) (boolToBool32 (shaderStorageImageWriteWithoutFormat))
    poke ((p `plusPtr` 132 :: Ptr Bool32)) (boolToBool32 (shaderUniformBufferArrayDynamicIndexing))
    poke ((p `plusPtr` 136 :: Ptr Bool32)) (boolToBool32 (shaderSampledImageArrayDynamicIndexing))
    poke ((p `plusPtr` 140 :: Ptr Bool32)) (boolToBool32 (shaderStorageBufferArrayDynamicIndexing))
    poke ((p `plusPtr` 144 :: Ptr Bool32)) (boolToBool32 (shaderStorageImageArrayDynamicIndexing))
    poke ((p `plusPtr` 148 :: Ptr Bool32)) (boolToBool32 (shaderClipDistance))
    poke ((p `plusPtr` 152 :: Ptr Bool32)) (boolToBool32 (shaderCullDistance))
    poke ((p `plusPtr` 156 :: Ptr Bool32)) (boolToBool32 (shaderFloat64))
    poke ((p `plusPtr` 160 :: Ptr Bool32)) (boolToBool32 (shaderInt64))
    poke ((p `plusPtr` 164 :: Ptr Bool32)) (boolToBool32 (shaderInt16))
    poke ((p `plusPtr` 168 :: Ptr Bool32)) (boolToBool32 (shaderResourceResidency))
    poke ((p `plusPtr` 172 :: Ptr Bool32)) (boolToBool32 (shaderResourceMinLod))
    poke ((p `plusPtr` 176 :: Ptr Bool32)) (boolToBool32 (sparseBinding))
    poke ((p `plusPtr` 180 :: Ptr Bool32)) (boolToBool32 (sparseResidencyBuffer))
    poke ((p `plusPtr` 184 :: Ptr Bool32)) (boolToBool32 (sparseResidencyImage2D))
    poke ((p `plusPtr` 188 :: Ptr Bool32)) (boolToBool32 (sparseResidencyImage3D))
    poke ((p `plusPtr` 192 :: Ptr Bool32)) (boolToBool32 (sparseResidency2Samples))
    poke ((p `plusPtr` 196 :: Ptr Bool32)) (boolToBool32 (sparseResidency4Samples))
    poke ((p `plusPtr` 200 :: Ptr Bool32)) (boolToBool32 (sparseResidency8Samples))
    poke ((p `plusPtr` 204 :: Ptr Bool32)) (boolToBool32 (sparseResidency16Samples))
    poke ((p `plusPtr` 208 :: Ptr Bool32)) (boolToBool32 (sparseResidencyAliased))
    poke ((p `plusPtr` 212 :: Ptr Bool32)) (boolToBool32 (variableMultisampleRate))
    poke ((p `plusPtr` 216 :: Ptr Bool32)) (boolToBool32 (inheritedQueries))
    f
  cStructSize = 220
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 4 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 8 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 12 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 52 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 56 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 60 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 64 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 68 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 72 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 76 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 80 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 84 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 88 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 92 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 96 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 100 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 104 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 108 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 112 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 116 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 120 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 124 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 128 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 132 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 136 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 140 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 144 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 148 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 152 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 156 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 160 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 164 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 168 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 172 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 176 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 180 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 184 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 188 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 192 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 196 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 200 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 204 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 208 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 212 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 216 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceFeatures where
  peekCStruct p = do
    robustBufferAccess <- peek @Bool32 ((p `plusPtr` 0 :: Ptr Bool32))
    fullDrawIndexUint32 <- peek @Bool32 ((p `plusPtr` 4 :: Ptr Bool32))
    imageCubeArray <- peek @Bool32 ((p `plusPtr` 8 :: Ptr Bool32))
    independentBlend <- peek @Bool32 ((p `plusPtr` 12 :: Ptr Bool32))
    geometryShader <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    tessellationShader <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    sampleRateShading <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    dualSrcBlend <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    logicOp <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    multiDrawIndirect <- peek @Bool32 ((p `plusPtr` 36 :: Ptr Bool32))
    drawIndirectFirstInstance <- peek @Bool32 ((p `plusPtr` 40 :: Ptr Bool32))
    depthClamp <- peek @Bool32 ((p `plusPtr` 44 :: Ptr Bool32))
    depthBiasClamp <- peek @Bool32 ((p `plusPtr` 48 :: Ptr Bool32))
    fillModeNonSolid <- peek @Bool32 ((p `plusPtr` 52 :: Ptr Bool32))
    depthBounds <- peek @Bool32 ((p `plusPtr` 56 :: Ptr Bool32))
    wideLines <- peek @Bool32 ((p `plusPtr` 60 :: Ptr Bool32))
    largePoints <- peek @Bool32 ((p `plusPtr` 64 :: Ptr Bool32))
    alphaToOne <- peek @Bool32 ((p `plusPtr` 68 :: Ptr Bool32))
    multiViewport <- peek @Bool32 ((p `plusPtr` 72 :: Ptr Bool32))
    samplerAnisotropy <- peek @Bool32 ((p `plusPtr` 76 :: Ptr Bool32))
    textureCompressionETC2 <- peek @Bool32 ((p `plusPtr` 80 :: Ptr Bool32))
    textureCompressionASTC_LDR <- peek @Bool32 ((p `plusPtr` 84 :: Ptr Bool32))
    textureCompressionBC <- peek @Bool32 ((p `plusPtr` 88 :: Ptr Bool32))
    occlusionQueryPrecise <- peek @Bool32 ((p `plusPtr` 92 :: Ptr Bool32))
    pipelineStatisticsQuery <- peek @Bool32 ((p `plusPtr` 96 :: Ptr Bool32))
    vertexPipelineStoresAndAtomics <- peek @Bool32 ((p `plusPtr` 100 :: Ptr Bool32))
    fragmentStoresAndAtomics <- peek @Bool32 ((p `plusPtr` 104 :: Ptr Bool32))
    shaderTessellationAndGeometryPointSize <- peek @Bool32 ((p `plusPtr` 108 :: Ptr Bool32))
    shaderImageGatherExtended <- peek @Bool32 ((p `plusPtr` 112 :: Ptr Bool32))
    shaderStorageImageExtendedFormats <- peek @Bool32 ((p `plusPtr` 116 :: Ptr Bool32))
    shaderStorageImageMultisample <- peek @Bool32 ((p `plusPtr` 120 :: Ptr Bool32))
    shaderStorageImageReadWithoutFormat <- peek @Bool32 ((p `plusPtr` 124 :: Ptr Bool32))
    shaderStorageImageWriteWithoutFormat <- peek @Bool32 ((p `plusPtr` 128 :: Ptr Bool32))
    shaderUniformBufferArrayDynamicIndexing <- peek @Bool32 ((p `plusPtr` 132 :: Ptr Bool32))
    shaderSampledImageArrayDynamicIndexing <- peek @Bool32 ((p `plusPtr` 136 :: Ptr Bool32))
    shaderStorageBufferArrayDynamicIndexing <- peek @Bool32 ((p `plusPtr` 140 :: Ptr Bool32))
    shaderStorageImageArrayDynamicIndexing <- peek @Bool32 ((p `plusPtr` 144 :: Ptr Bool32))
    shaderClipDistance <- peek @Bool32 ((p `plusPtr` 148 :: Ptr Bool32))
    shaderCullDistance <- peek @Bool32 ((p `plusPtr` 152 :: Ptr Bool32))
    shaderFloat64 <- peek @Bool32 ((p `plusPtr` 156 :: Ptr Bool32))
    shaderInt64 <- peek @Bool32 ((p `plusPtr` 160 :: Ptr Bool32))
    shaderInt16 <- peek @Bool32 ((p `plusPtr` 164 :: Ptr Bool32))
    shaderResourceResidency <- peek @Bool32 ((p `plusPtr` 168 :: Ptr Bool32))
    shaderResourceMinLod <- peek @Bool32 ((p `plusPtr` 172 :: Ptr Bool32))
    sparseBinding <- peek @Bool32 ((p `plusPtr` 176 :: Ptr Bool32))
    sparseResidencyBuffer <- peek @Bool32 ((p `plusPtr` 180 :: Ptr Bool32))
    sparseResidencyImage2D <- peek @Bool32 ((p `plusPtr` 184 :: Ptr Bool32))
    sparseResidencyImage3D <- peek @Bool32 ((p `plusPtr` 188 :: Ptr Bool32))
    sparseResidency2Samples <- peek @Bool32 ((p `plusPtr` 192 :: Ptr Bool32))
    sparseResidency4Samples <- peek @Bool32 ((p `plusPtr` 196 :: Ptr Bool32))
    sparseResidency8Samples <- peek @Bool32 ((p `plusPtr` 200 :: Ptr Bool32))
    sparseResidency16Samples <- peek @Bool32 ((p `plusPtr` 204 :: Ptr Bool32))
    sparseResidencyAliased <- peek @Bool32 ((p `plusPtr` 208 :: Ptr Bool32))
    variableMultisampleRate <- peek @Bool32 ((p `plusPtr` 212 :: Ptr Bool32))
    inheritedQueries <- peek @Bool32 ((p `plusPtr` 216 :: Ptr Bool32))
    pure $ PhysicalDeviceFeatures
             (bool32ToBool robustBufferAccess) (bool32ToBool fullDrawIndexUint32) (bool32ToBool imageCubeArray) (bool32ToBool independentBlend) (bool32ToBool geometryShader) (bool32ToBool tessellationShader) (bool32ToBool sampleRateShading) (bool32ToBool dualSrcBlend) (bool32ToBool logicOp) (bool32ToBool multiDrawIndirect) (bool32ToBool drawIndirectFirstInstance) (bool32ToBool depthClamp) (bool32ToBool depthBiasClamp) (bool32ToBool fillModeNonSolid) (bool32ToBool depthBounds) (bool32ToBool wideLines) (bool32ToBool largePoints) (bool32ToBool alphaToOne) (bool32ToBool multiViewport) (bool32ToBool samplerAnisotropy) (bool32ToBool textureCompressionETC2) (bool32ToBool textureCompressionASTC_LDR) (bool32ToBool textureCompressionBC) (bool32ToBool occlusionQueryPrecise) (bool32ToBool pipelineStatisticsQuery) (bool32ToBool vertexPipelineStoresAndAtomics) (bool32ToBool fragmentStoresAndAtomics) (bool32ToBool shaderTessellationAndGeometryPointSize) (bool32ToBool shaderImageGatherExtended) (bool32ToBool shaderStorageImageExtendedFormats) (bool32ToBool shaderStorageImageMultisample) (bool32ToBool shaderStorageImageReadWithoutFormat) (bool32ToBool shaderStorageImageWriteWithoutFormat) (bool32ToBool shaderUniformBufferArrayDynamicIndexing) (bool32ToBool shaderSampledImageArrayDynamicIndexing) (bool32ToBool shaderStorageBufferArrayDynamicIndexing) (bool32ToBool shaderStorageImageArrayDynamicIndexing) (bool32ToBool shaderClipDistance) (bool32ToBool shaderCullDistance) (bool32ToBool shaderFloat64) (bool32ToBool shaderInt64) (bool32ToBool shaderInt16) (bool32ToBool shaderResourceResidency) (bool32ToBool shaderResourceMinLod) (bool32ToBool sparseBinding) (bool32ToBool sparseResidencyBuffer) (bool32ToBool sparseResidencyImage2D) (bool32ToBool sparseResidencyImage3D) (bool32ToBool sparseResidency2Samples) (bool32ToBool sparseResidency4Samples) (bool32ToBool sparseResidency8Samples) (bool32ToBool sparseResidency16Samples) (bool32ToBool sparseResidencyAliased) (bool32ToBool variableMultisampleRate) (bool32ToBool inheritedQueries)

instance Storable PhysicalDeviceFeatures where
  sizeOf ~_ = 220
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceFeatures where
  zero = PhysicalDeviceFeatures
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


-- | VkPhysicalDeviceSparseProperties - Structure specifying physical device
-- sparse memory properties
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32', 'PhysicalDeviceProperties'
data PhysicalDeviceSparseProperties = PhysicalDeviceSparseProperties
  { -- | @residencyStandard2DBlockShape@ is 'Vulkan.Core10.FundamentalTypes.TRUE'
    -- if the physical device will access all single-sample 2D sparse resources
    -- using the standard sparse image block shapes (based on image format), as
    -- described in the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#sparsememory-sparseblockshapessingle Standard Sparse Image Block Shapes (Single Sample)>
    -- table. If this property is not supported the value returned in the
    -- @imageGranularity@ member of the
    -- 'Vulkan.Core10.SparseResourceMemoryManagement.SparseImageFormatProperties'
    -- structure for single-sample 2D images is not /required/ to match the
    -- standard sparse image block dimensions listed in the table.
    residencyStandard2DBlockShape :: Bool
  , -- | @residencyStandard2DMultisampleBlockShape@ is
    -- 'Vulkan.Core10.FundamentalTypes.TRUE' if the physical device will access
    -- all multisample 2D sparse resources using the standard sparse image
    -- block shapes (based on image format), as described in the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#sparsememory-sparseblockshapesmsaa Standard Sparse Image Block Shapes (MSAA)>
    -- table. If this property is not supported, the value returned in the
    -- @imageGranularity@ member of the
    -- 'Vulkan.Core10.SparseResourceMemoryManagement.SparseImageFormatProperties'
    -- structure for multisample 2D images is not /required/ to match the
    -- standard sparse image block dimensions listed in the table.
    residencyStandard2DMultisampleBlockShape :: Bool
  , -- | @residencyStandard3DBlockShape@ is 'Vulkan.Core10.FundamentalTypes.TRUE'
    -- if the physical device will access all 3D sparse resources using the
    -- standard sparse image block shapes (based on image format), as described
    -- in the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#sparsememory-sparseblockshapessingle Standard Sparse Image Block Shapes (Single Sample)>
    -- table. If this property is not supported, the value returned in the
    -- @imageGranularity@ member of the
    -- 'Vulkan.Core10.SparseResourceMemoryManagement.SparseImageFormatProperties'
    -- structure for 3D images is not /required/ to match the standard sparse
    -- image block dimensions listed in the table.
    residencyStandard3DBlockShape :: Bool
  , -- | @residencyAlignedMipSize@ is 'Vulkan.Core10.FundamentalTypes.TRUE' if
    -- images with mip level dimensions that are not integer multiples of the
    -- corresponding dimensions of the sparse image block /may/ be placed in
    -- the mip tail. If this property is not reported, only mip levels with
    -- dimensions smaller than the @imageGranularity@ member of the
    -- 'Vulkan.Core10.SparseResourceMemoryManagement.SparseImageFormatProperties'
    -- structure will be placed in the mip tail. If this property is reported
    -- the implementation is allowed to return
    -- 'Vulkan.Core10.Enums.SparseImageFormatFlagBits.SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT'
    -- in the @flags@ member of
    -- 'Vulkan.Core10.SparseResourceMemoryManagement.SparseImageFormatProperties',
    -- indicating that mip level dimensions that are not integer multiples of
    -- the corresponding dimensions of the sparse image block will be placed in
    -- the mip tail.
    residencyAlignedMipSize :: Bool
  , -- | @residencyNonResidentStrict@ specifies whether the physical device /can/
    -- consistently access non-resident regions of a resource. If this property
    -- is 'Vulkan.Core10.FundamentalTypes.TRUE', access to non-resident regions
    -- of resources will be guaranteed to return values as if the resource were
    -- populated with 0; writes to non-resident regions will be discarded.
    residencyNonResidentStrict :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceSparseProperties)
#endif
deriving instance Show PhysicalDeviceSparseProperties

instance ToCStruct PhysicalDeviceSparseProperties where
  withCStruct x f = allocaBytesAligned 20 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceSparseProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Bool32)) (boolToBool32 (residencyStandard2DBlockShape))
    poke ((p `plusPtr` 4 :: Ptr Bool32)) (boolToBool32 (residencyStandard2DMultisampleBlockShape))
    poke ((p `plusPtr` 8 :: Ptr Bool32)) (boolToBool32 (residencyStandard3DBlockShape))
    poke ((p `plusPtr` 12 :: Ptr Bool32)) (boolToBool32 (residencyAlignedMipSize))
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (residencyNonResidentStrict))
    f
  cStructSize = 20
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 4 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 8 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 12 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceSparseProperties where
  peekCStruct p = do
    residencyStandard2DBlockShape <- peek @Bool32 ((p `plusPtr` 0 :: Ptr Bool32))
    residencyStandard2DMultisampleBlockShape <- peek @Bool32 ((p `plusPtr` 4 :: Ptr Bool32))
    residencyStandard3DBlockShape <- peek @Bool32 ((p `plusPtr` 8 :: Ptr Bool32))
    residencyAlignedMipSize <- peek @Bool32 ((p `plusPtr` 12 :: Ptr Bool32))
    residencyNonResidentStrict <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceSparseProperties
             (bool32ToBool residencyStandard2DBlockShape) (bool32ToBool residencyStandard2DMultisampleBlockShape) (bool32ToBool residencyStandard3DBlockShape) (bool32ToBool residencyAlignedMipSize) (bool32ToBool residencyNonResidentStrict)

instance Storable PhysicalDeviceSparseProperties where
  sizeOf ~_ = 20
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceSparseProperties where
  zero = PhysicalDeviceSparseProperties
           zero
           zero
           zero
           zero
           zero


-- | VkPhysicalDeviceLimits - Structure reporting implementation-dependent
-- physical device limits
--
-- = Members
--
-- The 'PhysicalDeviceLimits' are properties of the physical device. These
-- are available in the @limits@ member of the 'PhysicalDeviceProperties'
-- structure which is returned from 'getPhysicalDeviceProperties'.
--
-- = Description
--
-- [1]
--     For all bitmasks of
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits', the
--     sample count limits defined above represent the minimum supported
--     sample counts for each image type. Individual images /may/ support
--     additional sample counts, which are queried using
--     'getPhysicalDeviceImageFormatProperties' as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-supported-sample-counts Supported Sample Counts>.
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize', 'PhysicalDeviceProperties',
-- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlags'
data PhysicalDeviceLimits = PhysicalDeviceLimits
  { -- | #limits-maxImageDimension1D# @maxImageDimension1D@ is the largest
    -- dimension (@width@) that is guaranteed to be supported for all images
    -- created with an @imageType@ of
    -- 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D'. Some combinations of
    -- image parameters (format, usage, etc.) /may/ allow support for larger
    -- dimensions, which /can/ be queried using
    -- 'getPhysicalDeviceImageFormatProperties'.
    maxImageDimension1D :: Word32
  , -- | #limits-maxImageDimension2D# @maxImageDimension2D@ is the largest
    -- dimension (@width@ or @height@) that is guaranteed to be supported for
    -- all images created with an @imageType@ of
    -- 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D' and without
    -- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CUBE_COMPATIBLE_BIT'
    -- set in @flags@. Some combinations of image parameters (format, usage,
    -- etc.) /may/ allow support for larger dimensions, which /can/ be queried
    -- using 'getPhysicalDeviceImageFormatProperties'.
    maxImageDimension2D :: Word32
  , -- | #limits-maxImageDimension3D# @maxImageDimension3D@ is the largest
    -- dimension (@width@, @height@, or @depth@) that is guaranteed to be
    -- supported for all images created with an @imageType@ of
    -- 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D'. Some combinations of
    -- image parameters (format, usage, etc.) /may/ allow support for larger
    -- dimensions, which /can/ be queried using
    -- 'getPhysicalDeviceImageFormatProperties'.
    maxImageDimension3D :: Word32
  , -- | #limits-maxImageDimensionCube# @maxImageDimensionCube@ is the largest
    -- dimension (@width@ or @height@) that is guaranteed to be supported for
    -- all images created with an @imageType@ of
    -- 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D' and with
    -- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CUBE_COMPATIBLE_BIT'
    -- set in @flags@. Some combinations of image parameters (format, usage,
    -- etc.) /may/ allow support for larger dimensions, which /can/ be queried
    -- using 'getPhysicalDeviceImageFormatProperties'.
    maxImageDimensionCube :: Word32
  , -- | #limits-maxImageArrayLayers# @maxImageArrayLayers@ is the maximum number
    -- of layers (@arrayLayers@) for an image.
    maxImageArrayLayers :: Word32
  , -- | #limits-maxTexelBufferElements# @maxTexelBufferElements@ is the maximum
    -- number of addressable texels for a buffer view created on a buffer which
    -- was created with the
    -- 'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT'
    -- or
    -- 'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT'
    -- set in the @usage@ member of the 'Vulkan.Core10.Buffer.BufferCreateInfo'
    -- structure.
    maxTexelBufferElements :: Word32
  , -- | #limits-maxUniformBufferRange# @maxUniformBufferRange@ is the maximum
    -- value that /can/ be specified in the @range@ member of any
    -- 'Vulkan.Core10.DescriptorSet.DescriptorBufferInfo' structures passed to
    -- a call to 'Vulkan.Core10.DescriptorSet.updateDescriptorSets' for
    -- descriptors of type
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER' or
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'.
    maxUniformBufferRange :: Word32
  , -- | #limits-maxStorageBufferRange# @maxStorageBufferRange@ is the maximum
    -- value that /can/ be specified in the @range@ member of any
    -- 'Vulkan.Core10.DescriptorSet.DescriptorBufferInfo' structures passed to
    -- a call to 'Vulkan.Core10.DescriptorSet.updateDescriptorSets' for
    -- descriptors of type
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER' or
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'.
    maxStorageBufferRange :: Word32
  , -- | #limits-maxPushConstantsSize# @maxPushConstantsSize@ is the maximum
    -- size, in bytes, of the pool of push constant memory. For each of the
    -- push constant ranges indicated by the @pPushConstantRanges@ member of
    -- the 'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo' structure,
    -- (@offset@ + @size@) /must/ be less than or equal to this limit.
    maxPushConstantsSize :: Word32
  , -- | #limits-maxMemoryAllocationCount# @maxMemoryAllocationCount@ is the
    -- maximum number of device memory allocations, as created by
    -- 'Vulkan.Core10.Memory.allocateMemory', which /can/ simultaneously exist.
    maxMemoryAllocationCount :: Word32
  , -- | #limits-maxSamplerAllocationCount# @maxSamplerAllocationCount@ is the
    -- maximum number of sampler objects, as created by
    -- 'Vulkan.Core10.Sampler.createSampler', which /can/ simultaneously exist
    -- on a device.
    maxSamplerAllocationCount :: Word32
  , -- | #limits-bufferImageGranularity# @bufferImageGranularity@ is the
    -- granularity, in bytes, at which buffer or linear image resources, and
    -- optimal image resources /can/ be bound to adjacent offsets in the same
    -- 'Vulkan.Core10.Handles.DeviceMemory' object without aliasing. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-bufferimagegranularity Buffer-Image Granularity>
    -- for more details.
    bufferImageGranularity :: DeviceSize
  , -- | #limits-sparseAddressSpaceSize# @sparseAddressSpaceSize@ is the total
    -- amount of address space available, in bytes, for sparse memory
    -- resources. This is an upper bound on the sum of the size of all sparse
    -- resources, regardless of whether any memory is bound to them.
    sparseAddressSpaceSize :: DeviceSize
  , -- | #limits-maxBoundDescriptorSets# @maxBoundDescriptorSets@ is the maximum
    -- number of descriptor sets that /can/ be simultaneously used by a
    -- pipeline. All 'Vulkan.Core10.Handles.DescriptorSet' decorations in
    -- shader modules /must/ have a value less than @maxBoundDescriptorSets@.
    -- See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-sets>.
    maxBoundDescriptorSets :: Word32
  , -- | #limits-maxPerStageDescriptorSamplers# @maxPerStageDescriptorSamplers@
    -- is the maximum number of samplers that /can/ be accessible to a single
    -- shader stage in a pipeline layout. Descriptors with a type of
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLER' or
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER'
    -- count against this limit. Only descriptors in descriptor set layouts
    -- created without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set count against this limit. A descriptor is accessible to a shader
    -- stage when the @stageFlags@ member of the
    -- 'Vulkan.Core10.DescriptorSet.DescriptorSetLayoutBinding' structure has
    -- the bit for that shader stage set. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-sampler>
    -- and
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-combinedimagesampler>.
    maxPerStageDescriptorSamplers :: Word32
  , -- | #limits-maxPerStageDescriptorUniformBuffers#
    -- @maxPerStageDescriptorUniformBuffers@ is the maximum number of uniform
    -- buffers that /can/ be accessible to a single shader stage in a pipeline
    -- layout. Descriptors with a type of
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER' or
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
    -- count against this limit. Only descriptors in descriptor set layouts
    -- created without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set count against this limit. A descriptor is accessible to a shader
    -- stage when the @stageFlags@ member of the
    -- 'Vulkan.Core10.DescriptorSet.DescriptorSetLayoutBinding' structure has
    -- the bit for that shader stage set. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-uniformbuffer>
    -- and
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-uniformbufferdynamic>.
    maxPerStageDescriptorUniformBuffers :: Word32
  , -- | #limits-maxPerStageDescriptorStorageBuffers#
    -- @maxPerStageDescriptorStorageBuffers@ is the maximum number of storage
    -- buffers that /can/ be accessible to a single shader stage in a pipeline
    -- layout. Descriptors with a type of
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER' or
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
    -- count against this limit. Only descriptors in descriptor set layouts
    -- created without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set count against this limit. A descriptor is accessible to a
    -- pipeline shader stage when the @stageFlags@ member of the
    -- 'Vulkan.Core10.DescriptorSet.DescriptorSetLayoutBinding' structure has
    -- the bit for that shader stage set. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-storagebuffer>
    -- and
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-storagebufferdynamic>.
    maxPerStageDescriptorStorageBuffers :: Word32
  , -- | #limits-maxPerStageDescriptorSampledImages#
    -- @maxPerStageDescriptorSampledImages@ is the maximum number of sampled
    -- images that /can/ be accessible to a single shader stage in a pipeline
    -- layout. Descriptors with a type of
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE', or
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
    -- count against this limit. Only descriptors in descriptor set layouts
    -- created without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set count against this limit. A descriptor is accessible to a
    -- pipeline shader stage when the @stageFlags@ member of the
    -- 'Vulkan.Core10.DescriptorSet.DescriptorSetLayoutBinding' structure has
    -- the bit for that shader stage set. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-combinedimagesampler>,
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-sampledimage>,
    -- and
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-uniformtexelbuffer>.
    maxPerStageDescriptorSampledImages :: Word32
  , -- | #limits-maxPerStageDescriptorStorageImages#
    -- @maxPerStageDescriptorStorageImages@ is the maximum number of storage
    -- images that /can/ be accessible to a single shader stage in a pipeline
    -- layout. Descriptors with a type of
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE', or
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
    -- count against this limit. Only descriptors in descriptor set layouts
    -- created without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set count against this limit. A descriptor is accessible to a
    -- pipeline shader stage when the @stageFlags@ member of the
    -- 'Vulkan.Core10.DescriptorSet.DescriptorSetLayoutBinding' structure has
    -- the bit for that shader stage set. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-storageimage>,
    -- and
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-storagetexelbuffer>.
    maxPerStageDescriptorStorageImages :: Word32
  , -- | #limits-maxPerStageDescriptorInputAttachments#
    -- @maxPerStageDescriptorInputAttachments@ is the maximum number of input
    -- attachments that /can/ be accessible to a single shader stage in a
    -- pipeline layout. Descriptors with a type of
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INPUT_ATTACHMENT'
    -- count against this limit. Only descriptors in descriptor set layouts
    -- created without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set count against this limit. A descriptor is accessible to a
    -- pipeline shader stage when the @stageFlags@ member of the
    -- 'Vulkan.Core10.DescriptorSet.DescriptorSetLayoutBinding' structure has
    -- the bit for that shader stage set. These are only supported for the
    -- fragment stage. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-inputattachment>.
    maxPerStageDescriptorInputAttachments :: Word32
  , -- | #limits-maxPerStageResources# @maxPerStageResources@ is the maximum
    -- number of resources that /can/ be accessible to a single shader stage in
    -- a pipeline layout. Descriptors with a type of
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE',
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE',
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER',
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER',
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER',
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER',
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC',
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC',
    -- or 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INPUT_ATTACHMENT'
    -- count against this limit. Only descriptors in descriptor set layouts
    -- created without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set count against this limit. For the fragment shader stage the
    -- framebuffer color attachments also count against this limit.
    maxPerStageResources :: Word32
  , -- | #limits-maxDescriptorSetSamplers# @maxDescriptorSetSamplers@ is the
    -- maximum number of samplers that /can/ be included in a pipeline layout.
    -- Descriptors with a type of
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLER' or
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER'
    -- count against this limit. Only descriptors in descriptor set layouts
    -- created without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set count against this limit. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-sampler>
    -- and
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-combinedimagesampler>.
    maxDescriptorSetSamplers :: Word32
  , -- | #limits-maxDescriptorSetUniformBuffers# @maxDescriptorSetUniformBuffers@
    -- is the maximum number of uniform buffers that /can/ be included in a
    -- pipeline layout. Descriptors with a type of
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER' or
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
    -- count against this limit. Only descriptors in descriptor set layouts
    -- created without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set count against this limit. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-uniformbuffer>
    -- and
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-uniformbufferdynamic>.
    maxDescriptorSetUniformBuffers :: Word32
  , -- | #limits-maxDescriptorSetUniformBuffersDynamic#
    -- @maxDescriptorSetUniformBuffersDynamic@ is the maximum number of dynamic
    -- uniform buffers that /can/ be included in a pipeline layout. Descriptors
    -- with a type of
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
    -- count against this limit. Only descriptors in descriptor set layouts
    -- created without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set count against this limit. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-uniformbufferdynamic>.
    maxDescriptorSetUniformBuffersDynamic :: Word32
  , -- | #limits-maxDescriptorSetStorageBuffers# @maxDescriptorSetStorageBuffers@
    -- is the maximum number of storage buffers that /can/ be included in a
    -- pipeline layout. Descriptors with a type of
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER' or
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
    -- count against this limit. Only descriptors in descriptor set layouts
    -- created without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set count against this limit. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-storagebuffer>
    -- and
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-storagebufferdynamic>.
    maxDescriptorSetStorageBuffers :: Word32
  , -- | #limits-maxDescriptorSetStorageBuffersDynamic#
    -- @maxDescriptorSetStorageBuffersDynamic@ is the maximum number of dynamic
    -- storage buffers that /can/ be included in a pipeline layout. Descriptors
    -- with a type of
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
    -- count against this limit. Only descriptors in descriptor set layouts
    -- created without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set count against this limit. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-storagebufferdynamic>.
    maxDescriptorSetStorageBuffersDynamic :: Word32
  , -- | #limits-maxDescriptorSetSampledImages# @maxDescriptorSetSampledImages@
    -- is the maximum number of sampled images that /can/ be included in a
    -- pipeline layout. Descriptors with a type of
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE', or
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
    -- count against this limit. Only descriptors in descriptor set layouts
    -- created without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set count against this limit. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-combinedimagesampler>,
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-sampledimage>,
    -- and
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-uniformtexelbuffer>.
    maxDescriptorSetSampledImages :: Word32
  , -- | #limits-maxDescriptorSetStorageImages# @maxDescriptorSetStorageImages@
    -- is the maximum number of storage images that /can/ be included in a
    -- pipeline layout. Descriptors with a type of
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE', or
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
    -- count against this limit. Only descriptors in descriptor set layouts
    -- created without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set count against this limit. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-storageimage>,
    -- and
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-storagetexelbuffer>.
    maxDescriptorSetStorageImages :: Word32
  , -- | #limits-maxDescriptorSetInputAttachments#
    -- @maxDescriptorSetInputAttachments@ is the maximum number of input
    -- attachments that /can/ be included in a pipeline layout. Descriptors
    -- with a type of
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INPUT_ATTACHMENT'
    -- count against this limit. Only descriptors in descriptor set layouts
    -- created without the
    -- 'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT'
    -- bit set count against this limit. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-inputattachment>.
    maxDescriptorSetInputAttachments :: Word32
  , -- | #limits-maxVertexInputAttributes# @maxVertexInputAttributes@ is the
    -- maximum number of vertex input attributes that /can/ be specified for a
    -- graphics pipeline. These are described in the array of
    -- 'Vulkan.Core10.Pipeline.VertexInputAttributeDescription' structures that
    -- are provided at graphics pipeline creation time via the
    -- @pVertexAttributeDescriptions@ member of the
    -- 'Vulkan.Core10.Pipeline.PipelineVertexInputStateCreateInfo' structure.
    -- See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fxvertex-attrib>
    -- and
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fxvertex-input>.
    maxVertexInputAttributes :: Word32
  , -- | #limits-maxVertexInputBindings# @maxVertexInputBindings@ is the maximum
    -- number of vertex buffers that /can/ be specified for providing vertex
    -- attributes to a graphics pipeline. These are described in the array of
    -- 'Vulkan.Core10.Pipeline.VertexInputBindingDescription' structures that
    -- are provided at graphics pipeline creation time via the
    -- @pVertexBindingDescriptions@ member of the
    -- 'Vulkan.Core10.Pipeline.PipelineVertexInputStateCreateInfo' structure.
    -- The @binding@ member of
    -- 'Vulkan.Core10.Pipeline.VertexInputBindingDescription' /must/ be less
    -- than this limit. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fxvertex-input>.
    maxVertexInputBindings :: Word32
  , -- | #limits-maxVertexInputAttributeOffset# @maxVertexInputAttributeOffset@
    -- is the maximum vertex input attribute offset that /can/ be added to the
    -- vertex input binding stride. The @offset@ member of the
    -- 'Vulkan.Core10.Pipeline.VertexInputAttributeDescription' structure
    -- /must/ be less than or equal to this limit. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fxvertex-input>.
    maxVertexInputAttributeOffset :: Word32
  , -- | #limits-maxVertexInputBindingStride# @maxVertexInputBindingStride@ is
    -- the maximum vertex input binding stride that /can/ be specified in a
    -- vertex input binding. The @stride@ member of the
    -- 'Vulkan.Core10.Pipeline.VertexInputBindingDescription' structure /must/
    -- be less than or equal to this limit. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fxvertex-input>.
    maxVertexInputBindingStride :: Word32
  , -- | #limits-maxVertexOutputComponents# @maxVertexOutputComponents@ is the
    -- maximum number of components of output variables which /can/ be output
    -- by a vertex shader. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-vertex>.
    maxVertexOutputComponents :: Word32
  , -- | #limits-maxTessellationGenerationLevel# @maxTessellationGenerationLevel@
    -- is the maximum tessellation generation level supported by the
    -- fixed-function tessellation primitive generator. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#tessellation>.
    maxTessellationGenerationLevel :: Word32
  , -- | #limits-maxTessellationPatchSize# @maxTessellationPatchSize@ is the
    -- maximum patch size, in vertices, of patches that /can/ be processed by
    -- the tessellation control shader and tessellation primitive generator.
    -- The @patchControlPoints@ member of the
    -- 'Vulkan.Core10.Pipeline.PipelineTessellationStateCreateInfo' structure
    -- specified at pipeline creation time and the value provided in the
    -- @OutputVertices@ execution mode of shader modules /must/ be less than or
    -- equal to this limit. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#tessellation>.
    maxTessellationPatchSize :: Word32
  , -- | #limits-maxTessellationControlPerVertexInputComponents#
    -- @maxTessellationControlPerVertexInputComponents@ is the maximum number
    -- of components of input variables which /can/ be provided as per-vertex
    -- inputs to the tessellation control shader stage.
    maxTessellationControlPerVertexInputComponents :: Word32
  , -- | #limits-maxTessellationControlPerVertexOutputComponents#
    -- @maxTessellationControlPerVertexOutputComponents@ is the maximum number
    -- of components of per-vertex output variables which /can/ be output from
    -- the tessellation control shader stage.
    maxTessellationControlPerVertexOutputComponents :: Word32
  , -- | #limits-maxTessellationControlPerPatchOutputComponents#
    -- @maxTessellationControlPerPatchOutputComponents@ is the maximum number
    -- of components of per-patch output variables which /can/ be output from
    -- the tessellation control shader stage.
    maxTessellationControlPerPatchOutputComponents :: Word32
  , -- | #limits-maxTessellationControlTotalOutputComponents#
    -- @maxTessellationControlTotalOutputComponents@ is the maximum total
    -- number of components of per-vertex and per-patch output variables which
    -- /can/ be output from the tessellation control shader stage.
    maxTessellationControlTotalOutputComponents :: Word32
  , -- | #limits-maxTessellationEvaluationInputComponents#
    -- @maxTessellationEvaluationInputComponents@ is the maximum number of
    -- components of input variables which /can/ be provided as per-vertex
    -- inputs to the tessellation evaluation shader stage.
    maxTessellationEvaluationInputComponents :: Word32
  , -- | #limits-maxTessellationEvaluationOutputComponents#
    -- @maxTessellationEvaluationOutputComponents@ is the maximum number of
    -- components of per-vertex output variables which /can/ be output from the
    -- tessellation evaluation shader stage.
    maxTessellationEvaluationOutputComponents :: Word32
  , -- | #limits-maxGeometryShaderInvocations# @maxGeometryShaderInvocations@ is
    -- the maximum invocation count supported for instanced geometry shaders.
    -- The value provided in the @Invocations@ execution mode of shader modules
    -- /must/ be less than or equal to this limit. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#geometry>.
    maxGeometryShaderInvocations :: Word32
  , -- | #limits-maxGeometryInputComponents# @maxGeometryInputComponents@ is the
    -- maximum number of components of input variables which /can/ be provided
    -- as inputs to the geometry shader stage.
    maxGeometryInputComponents :: Word32
  , -- | #limits-maxGeometryOutputComponents# @maxGeometryOutputComponents@ is
    -- the maximum number of components of output variables which /can/ be
    -- output from the geometry shader stage.
    maxGeometryOutputComponents :: Word32
  , -- | #limits-maxGeometryOutputVertices# @maxGeometryOutputVertices@ is the
    -- maximum number of vertices which /can/ be emitted by any geometry
    -- shader.
    maxGeometryOutputVertices :: Word32
  , -- | #limits-maxGeometryTotalOutputComponents#
    -- @maxGeometryTotalOutputComponents@ is the maximum total number of
    -- components of output, across all emitted vertices, which /can/ be output
    -- from the geometry shader stage.
    maxGeometryTotalOutputComponents :: Word32
  , -- | #limits-maxFragmentInputComponents# @maxFragmentInputComponents@ is the
    -- maximum number of components of input variables which /can/ be provided
    -- as inputs to the fragment shader stage.
    maxFragmentInputComponents :: Word32
  , -- | #limits-maxFragmentOutputAttachments# @maxFragmentOutputAttachments@ is
    -- the maximum number of output attachments which /can/ be written to by
    -- the fragment shader stage.
    maxFragmentOutputAttachments :: Word32
  , -- | #limits-maxFragmentDualSrcAttachments# @maxFragmentDualSrcAttachments@
    -- is the maximum number of output attachments which /can/ be written to by
    -- the fragment shader stage when blending is enabled and one of the dual
    -- source blend modes is in use. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#framebuffer-dsb>
    -- and
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dualSrcBlend dualSrcBlend>.
    maxFragmentDualSrcAttachments :: Word32
  , -- | #limits-maxFragmentCombinedOutputResources#
    -- @maxFragmentCombinedOutputResources@ is the total number of storage
    -- buffers, storage images, and output @Location@ decorated color
    -- attachments (described in
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-fragmentoutput Fragment Output Interface>)
    -- which /can/ be used in the fragment shader stage.
    maxFragmentCombinedOutputResources :: Word32
  , -- | #limits-maxComputeSharedMemorySize# @maxComputeSharedMemorySize@ is the
    -- maximum total storage size, in bytes, available for variables declared
    -- with the @Workgroup@ storage class in shader modules (or with the
    -- @shared@ storage qualifier in GLSL) in the compute shader stage. The
    -- amount of storage consumed by the variables declared with the
    -- @Workgroup@ storage class is implementation-dependent. However, the
    -- amount of storage consumed may not exceed the largest block size that
    -- would be obtained if all active variables declared with @Workgroup@
    -- storage class were assigned offsets in an arbitrary order by
    -- successively taking the smallest valid offset according to the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-resources-standard-layout Standard Storage Buffer Layout>
    -- rules. (This is equivalent to using the GLSL std430 layout rules.)
    maxComputeSharedMemorySize :: Word32
  , -- | #limits-maxComputeWorkGroupCount# @maxComputeWorkGroupCount@[3] is the
    -- maximum number of local workgroups that /can/ be dispatched by a single
    -- dispatch command. These three values represent the maximum number of
    -- local workgroups for the X, Y, and Z dimensions, respectively. The
    -- workgroup count parameters to the dispatch commands /must/ be less than
    -- or equal to the corresponding limit. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dispatch>.
    maxComputeWorkGroupCount :: (Word32, Word32, Word32)
  , -- | #limits-maxComputeWorkGroupInvocations# @maxComputeWorkGroupInvocations@
    -- is the maximum total number of compute shader invocations in a single
    -- local workgroup. The product of the X, Y, and Z sizes, as specified by
    -- the @LocalSize@ execution mode in shader modules or by the object
    -- decorated by the @WorkgroupSize@ decoration, /must/ be less than or
    -- equal to this limit.
    maxComputeWorkGroupInvocations :: Word32
  , -- | #limits-maxComputeWorkGroupSize# @maxComputeWorkGroupSize@[3] is the
    -- maximum size of a local compute workgroup, per dimension. These three
    -- values represent the maximum local workgroup size in the X, Y, and Z
    -- dimensions, respectively. The @x@, @y@, and @z@ sizes, as specified by
    -- the @LocalSize@ execution mode or by the object decorated by the
    -- @WorkgroupSize@ decoration in shader modules, /must/ be less than or
    -- equal to the corresponding limit.
    maxComputeWorkGroupSize :: (Word32, Word32, Word32)
  , -- | #limits-subPixelPrecisionBits# @subPixelPrecisionBits@ is the number of
    -- bits of subpixel precision in framebuffer coordinates xf and yf. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast>.
    subPixelPrecisionBits :: Word32
  , -- | #limits-subTexelPrecisionBits# @subTexelPrecisionBits@ is the number of
    -- bits of precision in the division along an axis of an image used for
    -- minification and magnification filters. 2@subTexelPrecisionBits@ is the
    -- actual number of divisions along each axis of the image represented.
    -- Sub-texel values calculated during image sampling will snap to these
    -- locations when generating the filtered results.
    subTexelPrecisionBits :: Word32
  , -- | #limits-mipmapPrecisionBits# @mipmapPrecisionBits@ is the number of bits
    -- of division that the LOD calculation for mipmap fetching get snapped to
    -- when determining the contribution from each mip level to the mip
    -- filtered results. 2@mipmapPrecisionBits@ is the actual number of
    -- divisions.
    mipmapPrecisionBits :: Word32
  , -- | #limits-maxDrawIndexedIndexValue# @maxDrawIndexedIndexValue@ is the
    -- maximum index value that /can/ be used for indexed draw calls when using
    -- 32-bit indices. This excludes the primitive restart index value of
    -- 0xFFFFFFFF. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fullDrawIndexUint32 fullDrawIndexUint32>.
    maxDrawIndexedIndexValue :: Word32
  , -- | #limits-maxDrawIndirectCount# @maxDrawIndirectCount@ is the maximum draw
    -- count that is supported for indirect draw calls. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiDrawIndirect multiDrawIndirect>.
    maxDrawIndirectCount :: Word32
  , -- | #limits-maxSamplerLodBias# @maxSamplerLodBias@ is the maximum absolute
    -- sampler LOD bias. The sum of the @mipLodBias@ member of the
    -- 'Vulkan.Core10.Sampler.SamplerCreateInfo' structure and the @Bias@
    -- operand of image sampling operations in shader modules (or 0 if no
    -- @Bias@ operand is provided to an image sampling operation) are clamped
    -- to the range [-@maxSamplerLodBias@,+@maxSamplerLodBias@]. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-mipLodBias>.
    maxSamplerLodBias :: Float
  , -- | #limits-maxSamplerAnisotropy# @maxSamplerAnisotropy@ is the maximum
    -- degree of sampler anisotropy. The maximum degree of anisotropic
    -- filtering used for an image sampling operation is the minimum of the
    -- @maxAnisotropy@ member of the 'Vulkan.Core10.Sampler.SamplerCreateInfo'
    -- structure and this limit. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-maxAnisotropy>.
    maxSamplerAnisotropy :: Float
  , -- | #limits-maxViewports# @maxViewports@ is the maximum number of active
    -- viewports. The @viewportCount@ member of the
    -- 'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo' structure that
    -- is provided at pipeline creation /must/ be less than or equal to this
    -- limit.
    maxViewports :: Word32
  , -- | #limits-maxViewportDimensions# @maxViewportDimensions@[2] are the
    -- maximum viewport dimensions in the X (width) and Y (height) dimensions,
    -- respectively. The maximum viewport dimensions /must/ be greater than or
    -- equal to the largest image which /can/ be created and used as a
    -- framebuffer attachment. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vertexpostproc-viewport Controlling the Viewport>.
    maxViewportDimensions :: (Word32, Word32)
  , -- | #limits-viewportboundsrange# @viewportBoundsRange@[2] is the [minimum,
    -- maximum] range that the corners of a viewport /must/ be contained in.
    -- This range /must/ be at least [-2 × @size@, 2 × @size@ - 1], where
    -- @size@ = max(@maxViewportDimensions@[0], @maxViewportDimensions@[1]).
    -- See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vertexpostproc-viewport Controlling the Viewport>.
    --
    -- Note
    --
    -- The intent of the @viewportBoundsRange@ limit is to allow a maximum
    -- sized viewport to be arbitrarily shifted relative to the output target
    -- as long as at least some portion intersects. This would give a bounds
    -- limit of [-@size@ + 1, 2 × @size@ - 1] which would allow all possible
    -- non-empty-set intersections of the output target and the viewport. Since
    -- these numbers are typically powers of two, picking the signed number
    -- range using the smallest possible number of bits ends up with the
    -- specified range.
    viewportBoundsRange :: (Float, Float)
  , -- | #limits-viewportSubPixelBits# @viewportSubPixelBits@ is the number of
    -- bits of subpixel precision for viewport bounds. The subpixel precision
    -- that floating-point viewport bounds are interpreted at is given by this
    -- limit.
    viewportSubPixelBits :: Word32
  , -- | #limits-minMemoryMapAlignment# @minMemoryMapAlignment@ is the minimum
    -- /required/ alignment, in bytes, of host visible memory allocations
    -- within the host address space. When mapping a memory allocation with
    -- 'Vulkan.Core10.Memory.mapMemory', subtracting @offset@ bytes from the
    -- returned pointer will always produce an integer multiple of this limit.
    -- See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-device-hostaccess>.
    minMemoryMapAlignment :: Word64
  , -- | #limits-minTexelBufferOffsetAlignment# @minTexelBufferOffsetAlignment@
    -- is the minimum /required/ alignment, in bytes, for the @offset@ member
    -- of the 'Vulkan.Core10.BufferView.BufferViewCreateInfo' structure for
    -- texel buffers. If
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-texelBufferAlignment texelBufferAlignment>
    -- is enabled, this limit is equivalent to the maximum of the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-uniformTexelBufferOffsetAlignmentBytes uniformTexelBufferOffsetAlignmentBytes>
    -- and
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-storageTexelBufferOffsetAlignmentBytes storageTexelBufferOffsetAlignmentBytes>
    -- members of
    -- 'Vulkan.Extensions.VK_EXT_texel_buffer_alignment.PhysicalDeviceTexelBufferAlignmentPropertiesEXT',
    -- but smaller alignment is optionally: allowed by
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-storageTexelBufferOffsetSingleTexelAlignment storageTexelBufferOffsetSingleTexelAlignment>
    -- and
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-uniformTexelBufferOffsetSingleTexelAlignment uniformTexelBufferOffsetSingleTexelAlignment>.
    -- If
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-texelBufferAlignment texelBufferAlignment>
    -- is not enabled,
    -- 'Vulkan.Core10.BufferView.BufferViewCreateInfo'::@offset@ /must/ be a
    -- multiple of this value.
    minTexelBufferOffsetAlignment :: DeviceSize
  , -- | #limits-minUniformBufferOffsetAlignment#
    -- @minUniformBufferOffsetAlignment@ is the minimum /required/ alignment,
    -- in bytes, for the @offset@ member of the
    -- 'Vulkan.Core10.DescriptorSet.DescriptorBufferInfo' structure for uniform
    -- buffers. When a descriptor of type
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER' or
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
    -- is updated, the @offset@ /must/ be an integer multiple of this limit.
    -- Similarly, dynamic offsets for uniform buffers /must/ be multiples of
    -- this limit.
    minUniformBufferOffsetAlignment :: DeviceSize
  , -- | #limits-minStorageBufferOffsetAlignment#
    -- @minStorageBufferOffsetAlignment@ is the minimum /required/ alignment,
    -- in bytes, for the @offset@ member of the
    -- 'Vulkan.Core10.DescriptorSet.DescriptorBufferInfo' structure for storage
    -- buffers. When a descriptor of type
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER' or
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
    -- is updated, the @offset@ /must/ be an integer multiple of this limit.
    -- Similarly, dynamic offsets for storage buffers /must/ be multiples of
    -- this limit.
    minStorageBufferOffsetAlignment :: DeviceSize
  , -- | #limits-minTexelOffset# @minTexelOffset@ is the minimum offset value for
    -- the @ConstOffset@ image operand of any of the @OpImageSample@* or
    -- @OpImageFetch@* image instructions.
    minTexelOffset :: Int32
  , -- | #limits-maxTexelOffset# @maxTexelOffset@ is the maximum offset value for
    -- the @ConstOffset@ image operand of any of the @OpImageSample@* or
    -- @OpImageFetch@* image instructions.
    maxTexelOffset :: Word32
  , -- | #limits-minTexelGatherOffset# @minTexelGatherOffset@ is the minimum
    -- offset value for the @Offset@, @ConstOffset@, or @ConstOffsets@ image
    -- operands of any of the @OpImage@*@Gather@ image instructions.
    minTexelGatherOffset :: Int32
  , -- | #limits-maxTexelGatherOffset# @maxTexelGatherOffset@ is the maximum
    -- offset value for the @Offset@, @ConstOffset@, or @ConstOffsets@ image
    -- operands of any of the @OpImage@*@Gather@ image instructions.
    maxTexelGatherOffset :: Word32
  , -- | #limits-minInterpolationOffset# @minInterpolationOffset@ is the base
    -- minimum (inclusive) negative offset value for the @Offset@ operand of
    -- the @InterpolateAtOffset@ extended instruction.
    minInterpolationOffset :: Float
  , -- | #limits-maxInterpolationOffset# @maxInterpolationOffset@ is the base
    -- maximum (inclusive) positive offset value for the @Offset@ operand of
    -- the @InterpolateAtOffset@ extended instruction.
    maxInterpolationOffset :: Float
  , -- | #limits-subPixelInterpolationOffsetBits#
    -- @subPixelInterpolationOffsetBits@ is the number of fractional bits that
    -- the @x@ and @y@ offsets to the @InterpolateAtOffset@ extended
    -- instruction /may/ be rounded to as fixed-point values.
    subPixelInterpolationOffsetBits :: Word32
  , -- | #limits-maxFramebufferWidth# @maxFramebufferWidth@ is the maximum width
    -- for a framebuffer. The @width@ member of the
    -- 'Vulkan.Core10.Pass.FramebufferCreateInfo' structure /must/ be less than
    -- or equal to this limit.
    maxFramebufferWidth :: Word32
  , -- | #limits-maxFramebufferHeight# @maxFramebufferHeight@ is the maximum
    -- height for a framebuffer. The @height@ member of the
    -- 'Vulkan.Core10.Pass.FramebufferCreateInfo' structure /must/ be less than
    -- or equal to this limit.
    maxFramebufferHeight :: Word32
  , -- | #limits-maxFramebufferLayers# @maxFramebufferLayers@ is the maximum
    -- layer count for a layered framebuffer. The @layers@ member of the
    -- 'Vulkan.Core10.Pass.FramebufferCreateInfo' structure /must/ be less than
    -- or equal to this limit.
    maxFramebufferLayers :: Word32
  , -- | #limits-framebufferColorSampleCounts# @framebufferColorSampleCounts@ is
    -- a bitmask1 of
    -- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' indicating
    -- the color sample counts that are supported for all framebuffer color
    -- attachments with floating- or fixed-point formats. For color attachments
    -- with integer formats, see
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-framebufferIntegerColorSampleCounts framebufferIntegerColorSampleCounts>.
    framebufferColorSampleCounts :: SampleCountFlags
  , -- | #limits-framebufferDepthSampleCounts# @framebufferDepthSampleCounts@ is
    -- a bitmask1 of
    -- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' indicating
    -- the supported depth sample counts for all framebuffer depth\/stencil
    -- attachments, when the format includes a depth component.
    framebufferDepthSampleCounts :: SampleCountFlags
  , -- | #limits-framebufferStencilSampleCounts# @framebufferStencilSampleCounts@
    -- is a bitmask1 of
    -- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' indicating
    -- the supported stencil sample counts for all framebuffer depth\/stencil
    -- attachments, when the format includes a stencil component.
    framebufferStencilSampleCounts :: SampleCountFlags
  , -- | #limits-framebufferNoAttachmentsSampleCounts#
    -- @framebufferNoAttachmentsSampleCounts@ is a bitmask1 of
    -- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' indicating
    -- the supported sample counts for a
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-noattachments subpass which uses no attachments>.
    framebufferNoAttachmentsSampleCounts :: SampleCountFlags
  , -- | #limits-maxColorAttachments# @maxColorAttachments@ is the maximum number
    -- of color attachments that /can/ be used by a subpass in a render pass.
    -- The @colorAttachmentCount@ member of the
    -- 'Vulkan.Core10.Pass.SubpassDescription' or
    -- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.SubpassDescription2'
    -- structure /must/ be less than or equal to this limit.
    maxColorAttachments :: Word32
  , -- | #limits-sampledImageColorSampleCounts# @sampledImageColorSampleCounts@
    -- is a bitmask1 of
    -- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' indicating
    -- the sample counts supported for all 2D images created with
    -- 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL', @usage@
    -- containing
    -- 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT', and a
    -- non-integer color format.
    sampledImageColorSampleCounts :: SampleCountFlags
  , -- | #limits-sampledImageIntegerSampleCounts#
    -- @sampledImageIntegerSampleCounts@ is a bitmask1 of
    -- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' indicating
    -- the sample counts supported for all 2D images created with
    -- 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL', @usage@
    -- containing
    -- 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT', and an
    -- integer color format.
    sampledImageIntegerSampleCounts :: SampleCountFlags
  , -- | #limits-sampledImageDepthSampleCounts# @sampledImageDepthSampleCounts@
    -- is a bitmask1 of
    -- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' indicating
    -- the sample counts supported for all 2D images created with
    -- 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL', @usage@
    -- containing
    -- 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT', and a
    -- depth format.
    sampledImageDepthSampleCounts :: SampleCountFlags
  , -- | #limits-sampledImageStencilSampleCounts#
    -- @sampledImageStencilSampleCounts@ is a bitmask1 of
    -- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' indicating
    -- the sample supported for all 2D images created with
    -- 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL', @usage@
    -- containing
    -- 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT', and a
    -- stencil format.
    sampledImageStencilSampleCounts :: SampleCountFlags
  , -- | #limits-storageImageSampleCounts# @storageImageSampleCounts@ is a
    -- bitmask1 of
    -- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' indicating
    -- the sample counts supported for all 2D images created with
    -- 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL', and @usage@
    -- containing
    -- 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_STORAGE_BIT'.
    storageImageSampleCounts :: SampleCountFlags
  , -- | #limits-maxSampleMaskWords# @maxSampleMaskWords@ is the maximum number
    -- of array elements of a variable decorated with the
    -- 'Vulkan.Core10.FundamentalTypes.SampleMask' built-in decoration.
    maxSampleMaskWords :: Word32
  , -- | #limits-timestampComputeAndGraphics# @timestampComputeAndGraphics@
    -- specifies support for timestamps on all graphics and compute queues. If
    -- this limit is set to 'Vulkan.Core10.FundamentalTypes.TRUE', all queues
    -- that advertise the
    -- 'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' or
    -- 'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT' in the
    -- 'QueueFamilyProperties'::@queueFlags@ support
    -- 'QueueFamilyProperties'::@timestampValidBits@ of at least 36. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-timestamps Timestamp Queries>.
    timestampComputeAndGraphics :: Bool
  , -- | #limits-timestampPeriod# @timestampPeriod@ is the number of nanoseconds
    -- /required/ for a timestamp query to be incremented by 1. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-timestamps Timestamp Queries>.
    timestampPeriod :: Float
  , -- | #limits-maxClipDistances# @maxClipDistances@ is the maximum number of
    -- clip distances that /can/ be used in a single shader stage. The size of
    -- any array declared with the @ClipDistance@ built-in decoration in a
    -- shader module /must/ be less than or equal to this limit.
    maxClipDistances :: Word32
  , -- | #limits-maxCullDistances# @maxCullDistances@ is the maximum number of
    -- cull distances that /can/ be used in a single shader stage. The size of
    -- any array declared with the @CullDistance@ built-in decoration in a
    -- shader module /must/ be less than or equal to this limit.
    maxCullDistances :: Word32
  , -- | #limits-maxCombinedClipAndCullDistances#
    -- @maxCombinedClipAndCullDistances@ is the maximum combined number of clip
    -- and cull distances that /can/ be used in a single shader stage. The sum
    -- of the sizes of any pair of arrays declared with the @ClipDistance@ and
    -- @CullDistance@ built-in decoration used by a single shader stage in a
    -- shader module /must/ be less than or equal to this limit.
    maxCombinedClipAndCullDistances :: Word32
  , -- | #limits-discreteQueuePriorities# @discreteQueuePriorities@ is the number
    -- of discrete priorities that /can/ be assigned to a queue based on the
    -- value of each member of
    -- 'Vulkan.Core10.Device.DeviceQueueCreateInfo'::@pQueuePriorities@. This
    -- /must/ be at least 2, and levels /must/ be spread evenly over the range,
    -- with at least one level at 1.0, and another at 0.0. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-priority>.
    discreteQueuePriorities :: Word32
  , -- | #limits-pointSizeRange# @pointSizeRange@[2] is the range
    -- [@minimum@,@maximum@] of supported sizes for points. Values written to
    -- variables decorated with the @PointSize@ built-in decoration are clamped
    -- to this range.
    pointSizeRange :: (Float, Float)
  , -- | #limits-lineWidthRange# @lineWidthRange@[2] is the range
    -- [@minimum@,@maximum@] of supported widths for lines. Values specified by
    -- the @lineWidth@ member of the
    -- 'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo' or the
    -- @lineWidth@ parameter to
    -- 'Vulkan.Core10.CommandBufferBuilding.cmdSetLineWidth' are clamped to
    -- this range.
    lineWidthRange :: (Float, Float)
  , -- | #limits-pointSizeGranularity# @pointSizeGranularity@ is the granularity
    -- of supported point sizes. Not all point sizes in the range defined by
    -- @pointSizeRange@ are supported. This limit specifies the granularity (or
    -- increment) between successive supported point sizes.
    pointSizeGranularity :: Float
  , -- | #limits-lineWidthGranularity# @lineWidthGranularity@ is the granularity
    -- of supported line widths. Not all line widths in the range defined by
    -- @lineWidthRange@ are supported. This limit specifies the granularity (or
    -- increment) between successive supported line widths.
    lineWidthGranularity :: Float
  , -- | #limits-strictLines# @strictLines@ specifies whether lines are
    -- rasterized according to the preferred method of rasterization. If set to
    -- 'Vulkan.Core10.FundamentalTypes.FALSE', lines /may/ be rasterized under
    -- a relaxed set of rules. If set to 'Vulkan.Core10.FundamentalTypes.TRUE',
    -- lines are rasterized as per the strict definition. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-lines-basic Basic Line Segment Rasterization>.
    strictLines :: Bool
  , -- | #limits-standardSampleLocations# @standardSampleLocations@ specifies
    -- whether rasterization uses the standard sample locations as documented
    -- in
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-multisampling Multisampling>.
    -- If set to 'Vulkan.Core10.FundamentalTypes.TRUE', the implementation uses
    -- the documented sample locations. If set to
    -- 'Vulkan.Core10.FundamentalTypes.FALSE', the implementation /may/ use
    -- different sample locations.
    standardSampleLocations :: Bool
  , -- | #limits-optimalBufferCopyOffsetAlignment#
    -- @optimalBufferCopyOffsetAlignment@ is the optimal buffer offset
    -- alignment in bytes for
    -- 'Vulkan.Extensions.VK_KHR_copy_commands2.cmdCopyBufferToImage2KHR',
    -- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyBufferToImage',
    -- 'Vulkan.Extensions.VK_KHR_copy_commands2.cmdCopyImageToBuffer2KHR', and
    -- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyImageToBuffer'. The per
    -- texel alignment requirements are enforced, but applications /should/ use
    -- the optimal alignment for optimal performance and power use.
    optimalBufferCopyOffsetAlignment :: DeviceSize
  , -- | #limits-optimalBufferCopyRowPitchAlignment#
    -- @optimalBufferCopyRowPitchAlignment@ is the optimal buffer row pitch
    -- alignment in bytes for
    -- 'Vulkan.Extensions.VK_KHR_copy_commands2.cmdCopyBufferToImage2KHR',
    -- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyBufferToImage',
    -- 'Vulkan.Extensions.VK_KHR_copy_commands2.cmdCopyImageToBuffer2KHR', and
    -- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyImageToBuffer'. Row pitch is
    -- the number of bytes between texels with the same X coordinate in
    -- adjacent rows (Y coordinates differ by one). The per texel alignment
    -- requirements are enforced, but applications /should/ use the optimal
    -- alignment for optimal performance and power use.
    optimalBufferCopyRowPitchAlignment :: DeviceSize
  , -- | #limits-nonCoherentAtomSize# @nonCoherentAtomSize@ is the size and
    -- alignment in bytes that bounds concurrent access to
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-device-hostaccess host-mapped device memory>.
    nonCoherentAtomSize :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceLimits)
#endif
deriving instance Show PhysicalDeviceLimits

instance ToCStruct PhysicalDeviceLimits where
  withCStruct x f = allocaBytesAligned 504 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceLimits{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (maxImageDimension1D)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (maxImageDimension2D)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (maxImageDimension3D)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (maxImageDimensionCube)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxImageArrayLayers)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxTexelBufferElements)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (maxUniformBufferRange)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (maxStorageBufferRange)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (maxPushConstantsSize)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (maxMemoryAllocationCount)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (maxSamplerAllocationCount)
    poke ((p `plusPtr` 48 :: Ptr DeviceSize)) (bufferImageGranularity)
    poke ((p `plusPtr` 56 :: Ptr DeviceSize)) (sparseAddressSpaceSize)
    poke ((p `plusPtr` 64 :: Ptr Word32)) (maxBoundDescriptorSets)
    poke ((p `plusPtr` 68 :: Ptr Word32)) (maxPerStageDescriptorSamplers)
    poke ((p `plusPtr` 72 :: Ptr Word32)) (maxPerStageDescriptorUniformBuffers)
    poke ((p `plusPtr` 76 :: Ptr Word32)) (maxPerStageDescriptorStorageBuffers)
    poke ((p `plusPtr` 80 :: Ptr Word32)) (maxPerStageDescriptorSampledImages)
    poke ((p `plusPtr` 84 :: Ptr Word32)) (maxPerStageDescriptorStorageImages)
    poke ((p `plusPtr` 88 :: Ptr Word32)) (maxPerStageDescriptorInputAttachments)
    poke ((p `plusPtr` 92 :: Ptr Word32)) (maxPerStageResources)
    poke ((p `plusPtr` 96 :: Ptr Word32)) (maxDescriptorSetSamplers)
    poke ((p `plusPtr` 100 :: Ptr Word32)) (maxDescriptorSetUniformBuffers)
    poke ((p `plusPtr` 104 :: Ptr Word32)) (maxDescriptorSetUniformBuffersDynamic)
    poke ((p `plusPtr` 108 :: Ptr Word32)) (maxDescriptorSetStorageBuffers)
    poke ((p `plusPtr` 112 :: Ptr Word32)) (maxDescriptorSetStorageBuffersDynamic)
    poke ((p `plusPtr` 116 :: Ptr Word32)) (maxDescriptorSetSampledImages)
    poke ((p `plusPtr` 120 :: Ptr Word32)) (maxDescriptorSetStorageImages)
    poke ((p `plusPtr` 124 :: Ptr Word32)) (maxDescriptorSetInputAttachments)
    poke ((p `plusPtr` 128 :: Ptr Word32)) (maxVertexInputAttributes)
    poke ((p `plusPtr` 132 :: Ptr Word32)) (maxVertexInputBindings)
    poke ((p `plusPtr` 136 :: Ptr Word32)) (maxVertexInputAttributeOffset)
    poke ((p `plusPtr` 140 :: Ptr Word32)) (maxVertexInputBindingStride)
    poke ((p `plusPtr` 144 :: Ptr Word32)) (maxVertexOutputComponents)
    poke ((p `plusPtr` 148 :: Ptr Word32)) (maxTessellationGenerationLevel)
    poke ((p `plusPtr` 152 :: Ptr Word32)) (maxTessellationPatchSize)
    poke ((p `plusPtr` 156 :: Ptr Word32)) (maxTessellationControlPerVertexInputComponents)
    poke ((p `plusPtr` 160 :: Ptr Word32)) (maxTessellationControlPerVertexOutputComponents)
    poke ((p `plusPtr` 164 :: Ptr Word32)) (maxTessellationControlPerPatchOutputComponents)
    poke ((p `plusPtr` 168 :: Ptr Word32)) (maxTessellationControlTotalOutputComponents)
    poke ((p `plusPtr` 172 :: Ptr Word32)) (maxTessellationEvaluationInputComponents)
    poke ((p `plusPtr` 176 :: Ptr Word32)) (maxTessellationEvaluationOutputComponents)
    poke ((p `plusPtr` 180 :: Ptr Word32)) (maxGeometryShaderInvocations)
    poke ((p `plusPtr` 184 :: Ptr Word32)) (maxGeometryInputComponents)
    poke ((p `plusPtr` 188 :: Ptr Word32)) (maxGeometryOutputComponents)
    poke ((p `plusPtr` 192 :: Ptr Word32)) (maxGeometryOutputVertices)
    poke ((p `plusPtr` 196 :: Ptr Word32)) (maxGeometryTotalOutputComponents)
    poke ((p `plusPtr` 200 :: Ptr Word32)) (maxFragmentInputComponents)
    poke ((p `plusPtr` 204 :: Ptr Word32)) (maxFragmentOutputAttachments)
    poke ((p `plusPtr` 208 :: Ptr Word32)) (maxFragmentDualSrcAttachments)
    poke ((p `plusPtr` 212 :: Ptr Word32)) (maxFragmentCombinedOutputResources)
    poke ((p `plusPtr` 216 :: Ptr Word32)) (maxComputeSharedMemorySize)
    let pMaxComputeWorkGroupCount' = lowerArrayPtr ((p `plusPtr` 220 :: Ptr (FixedArray 3 Word32)))
    case (maxComputeWorkGroupCount) of
      (e0, e1, e2) -> do
        poke (pMaxComputeWorkGroupCount' :: Ptr Word32) (e0)
        poke (pMaxComputeWorkGroupCount' `plusPtr` 4 :: Ptr Word32) (e1)
        poke (pMaxComputeWorkGroupCount' `plusPtr` 8 :: Ptr Word32) (e2)
    poke ((p `plusPtr` 232 :: Ptr Word32)) (maxComputeWorkGroupInvocations)
    let pMaxComputeWorkGroupSize' = lowerArrayPtr ((p `plusPtr` 236 :: Ptr (FixedArray 3 Word32)))
    case (maxComputeWorkGroupSize) of
      (e0, e1, e2) -> do
        poke (pMaxComputeWorkGroupSize' :: Ptr Word32) (e0)
        poke (pMaxComputeWorkGroupSize' `plusPtr` 4 :: Ptr Word32) (e1)
        poke (pMaxComputeWorkGroupSize' `plusPtr` 8 :: Ptr Word32) (e2)
    poke ((p `plusPtr` 248 :: Ptr Word32)) (subPixelPrecisionBits)
    poke ((p `plusPtr` 252 :: Ptr Word32)) (subTexelPrecisionBits)
    poke ((p `plusPtr` 256 :: Ptr Word32)) (mipmapPrecisionBits)
    poke ((p `plusPtr` 260 :: Ptr Word32)) (maxDrawIndexedIndexValue)
    poke ((p `plusPtr` 264 :: Ptr Word32)) (maxDrawIndirectCount)
    poke ((p `plusPtr` 268 :: Ptr CFloat)) (CFloat (maxSamplerLodBias))
    poke ((p `plusPtr` 272 :: Ptr CFloat)) (CFloat (maxSamplerAnisotropy))
    poke ((p `plusPtr` 276 :: Ptr Word32)) (maxViewports)
    let pMaxViewportDimensions' = lowerArrayPtr ((p `plusPtr` 280 :: Ptr (FixedArray 2 Word32)))
    case (maxViewportDimensions) of
      (e0, e1) -> do
        poke (pMaxViewportDimensions' :: Ptr Word32) (e0)
        poke (pMaxViewportDimensions' `plusPtr` 4 :: Ptr Word32) (e1)
    let pViewportBoundsRange' = lowerArrayPtr ((p `plusPtr` 288 :: Ptr (FixedArray 2 CFloat)))
    case (viewportBoundsRange) of
      (e0, e1) -> do
        poke (pViewportBoundsRange' :: Ptr CFloat) (CFloat (e0))
        poke (pViewportBoundsRange' `plusPtr` 4 :: Ptr CFloat) (CFloat (e1))
    poke ((p `plusPtr` 296 :: Ptr Word32)) (viewportSubPixelBits)
    poke ((p `plusPtr` 304 :: Ptr CSize)) (CSize (minMemoryMapAlignment))
    poke ((p `plusPtr` 312 :: Ptr DeviceSize)) (minTexelBufferOffsetAlignment)
    poke ((p `plusPtr` 320 :: Ptr DeviceSize)) (minUniformBufferOffsetAlignment)
    poke ((p `plusPtr` 328 :: Ptr DeviceSize)) (minStorageBufferOffsetAlignment)
    poke ((p `plusPtr` 336 :: Ptr Int32)) (minTexelOffset)
    poke ((p `plusPtr` 340 :: Ptr Word32)) (maxTexelOffset)
    poke ((p `plusPtr` 344 :: Ptr Int32)) (minTexelGatherOffset)
    poke ((p `plusPtr` 348 :: Ptr Word32)) (maxTexelGatherOffset)
    poke ((p `plusPtr` 352 :: Ptr CFloat)) (CFloat (minInterpolationOffset))
    poke ((p `plusPtr` 356 :: Ptr CFloat)) (CFloat (maxInterpolationOffset))
    poke ((p `plusPtr` 360 :: Ptr Word32)) (subPixelInterpolationOffsetBits)
    poke ((p `plusPtr` 364 :: Ptr Word32)) (maxFramebufferWidth)
    poke ((p `plusPtr` 368 :: Ptr Word32)) (maxFramebufferHeight)
    poke ((p `plusPtr` 372 :: Ptr Word32)) (maxFramebufferLayers)
    poke ((p `plusPtr` 376 :: Ptr SampleCountFlags)) (framebufferColorSampleCounts)
    poke ((p `plusPtr` 380 :: Ptr SampleCountFlags)) (framebufferDepthSampleCounts)
    poke ((p `plusPtr` 384 :: Ptr SampleCountFlags)) (framebufferStencilSampleCounts)
    poke ((p `plusPtr` 388 :: Ptr SampleCountFlags)) (framebufferNoAttachmentsSampleCounts)
    poke ((p `plusPtr` 392 :: Ptr Word32)) (maxColorAttachments)
    poke ((p `plusPtr` 396 :: Ptr SampleCountFlags)) (sampledImageColorSampleCounts)
    poke ((p `plusPtr` 400 :: Ptr SampleCountFlags)) (sampledImageIntegerSampleCounts)
    poke ((p `plusPtr` 404 :: Ptr SampleCountFlags)) (sampledImageDepthSampleCounts)
    poke ((p `plusPtr` 408 :: Ptr SampleCountFlags)) (sampledImageStencilSampleCounts)
    poke ((p `plusPtr` 412 :: Ptr SampleCountFlags)) (storageImageSampleCounts)
    poke ((p `plusPtr` 416 :: Ptr Word32)) (maxSampleMaskWords)
    poke ((p `plusPtr` 420 :: Ptr Bool32)) (boolToBool32 (timestampComputeAndGraphics))
    poke ((p `plusPtr` 424 :: Ptr CFloat)) (CFloat (timestampPeriod))
    poke ((p `plusPtr` 428 :: Ptr Word32)) (maxClipDistances)
    poke ((p `plusPtr` 432 :: Ptr Word32)) (maxCullDistances)
    poke ((p `plusPtr` 436 :: Ptr Word32)) (maxCombinedClipAndCullDistances)
    poke ((p `plusPtr` 440 :: Ptr Word32)) (discreteQueuePriorities)
    let pPointSizeRange' = lowerArrayPtr ((p `plusPtr` 444 :: Ptr (FixedArray 2 CFloat)))
    case (pointSizeRange) of
      (e0, e1) -> do
        poke (pPointSizeRange' :: Ptr CFloat) (CFloat (e0))
        poke (pPointSizeRange' `plusPtr` 4 :: Ptr CFloat) (CFloat (e1))
    let pLineWidthRange' = lowerArrayPtr ((p `plusPtr` 452 :: Ptr (FixedArray 2 CFloat)))
    case (lineWidthRange) of
      (e0, e1) -> do
        poke (pLineWidthRange' :: Ptr CFloat) (CFloat (e0))
        poke (pLineWidthRange' `plusPtr` 4 :: Ptr CFloat) (CFloat (e1))
    poke ((p `plusPtr` 460 :: Ptr CFloat)) (CFloat (pointSizeGranularity))
    poke ((p `plusPtr` 464 :: Ptr CFloat)) (CFloat (lineWidthGranularity))
    poke ((p `plusPtr` 468 :: Ptr Bool32)) (boolToBool32 (strictLines))
    poke ((p `plusPtr` 472 :: Ptr Bool32)) (boolToBool32 (standardSampleLocations))
    poke ((p `plusPtr` 480 :: Ptr DeviceSize)) (optimalBufferCopyOffsetAlignment)
    poke ((p `plusPtr` 488 :: Ptr DeviceSize)) (optimalBufferCopyRowPitchAlignment)
    poke ((p `plusPtr` 496 :: Ptr DeviceSize)) (nonCoherentAtomSize)
    f
  cStructSize = 504
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 48 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 56 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 64 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 68 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 72 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 76 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 80 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 84 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 88 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 92 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 96 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 100 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 104 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 108 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 112 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 116 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 120 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 124 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 128 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 132 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 136 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 140 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 144 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 148 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 152 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 156 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 160 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 164 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 168 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 172 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 176 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 180 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 184 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 188 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 192 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 196 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 200 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 204 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 208 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 212 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 216 :: Ptr Word32)) (zero)
    let pMaxComputeWorkGroupCount' = lowerArrayPtr ((p `plusPtr` 220 :: Ptr (FixedArray 3 Word32)))
    case ((zero, zero, zero)) of
      (e0, e1, e2) -> do
        poke (pMaxComputeWorkGroupCount' :: Ptr Word32) (e0)
        poke (pMaxComputeWorkGroupCount' `plusPtr` 4 :: Ptr Word32) (e1)
        poke (pMaxComputeWorkGroupCount' `plusPtr` 8 :: Ptr Word32) (e2)
    poke ((p `plusPtr` 232 :: Ptr Word32)) (zero)
    let pMaxComputeWorkGroupSize' = lowerArrayPtr ((p `plusPtr` 236 :: Ptr (FixedArray 3 Word32)))
    case ((zero, zero, zero)) of
      (e0, e1, e2) -> do
        poke (pMaxComputeWorkGroupSize' :: Ptr Word32) (e0)
        poke (pMaxComputeWorkGroupSize' `plusPtr` 4 :: Ptr Word32) (e1)
        poke (pMaxComputeWorkGroupSize' `plusPtr` 8 :: Ptr Word32) (e2)
    poke ((p `plusPtr` 248 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 252 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 256 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 260 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 264 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 268 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 272 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 276 :: Ptr Word32)) (zero)
    let pMaxViewportDimensions' = lowerArrayPtr ((p `plusPtr` 280 :: Ptr (FixedArray 2 Word32)))
    case ((zero, zero)) of
      (e0, e1) -> do
        poke (pMaxViewportDimensions' :: Ptr Word32) (e0)
        poke (pMaxViewportDimensions' `plusPtr` 4 :: Ptr Word32) (e1)
    let pViewportBoundsRange' = lowerArrayPtr ((p `plusPtr` 288 :: Ptr (FixedArray 2 CFloat)))
    case ((zero, zero)) of
      (e0, e1) -> do
        poke (pViewportBoundsRange' :: Ptr CFloat) (CFloat (e0))
        poke (pViewportBoundsRange' `plusPtr` 4 :: Ptr CFloat) (CFloat (e1))
    poke ((p `plusPtr` 296 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 304 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 312 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 320 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 328 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 336 :: Ptr Int32)) (zero)
    poke ((p `plusPtr` 340 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 344 :: Ptr Int32)) (zero)
    poke ((p `plusPtr` 348 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 352 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 356 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 360 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 364 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 368 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 372 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 392 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 416 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 420 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 424 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 428 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 432 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 436 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 440 :: Ptr Word32)) (zero)
    let pPointSizeRange' = lowerArrayPtr ((p `plusPtr` 444 :: Ptr (FixedArray 2 CFloat)))
    case ((zero, zero)) of
      (e0, e1) -> do
        poke (pPointSizeRange' :: Ptr CFloat) (CFloat (e0))
        poke (pPointSizeRange' `plusPtr` 4 :: Ptr CFloat) (CFloat (e1))
    let pLineWidthRange' = lowerArrayPtr ((p `plusPtr` 452 :: Ptr (FixedArray 2 CFloat)))
    case ((zero, zero)) of
      (e0, e1) -> do
        poke (pLineWidthRange' :: Ptr CFloat) (CFloat (e0))
        poke (pLineWidthRange' `plusPtr` 4 :: Ptr CFloat) (CFloat (e1))
    poke ((p `plusPtr` 460 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 464 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 468 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 472 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 480 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 488 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 496 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct PhysicalDeviceLimits where
  peekCStruct p = do
    maxImageDimension1D <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    maxImageDimension2D <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    maxImageDimension3D <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    maxImageDimensionCube <- peek @Word32 ((p `plusPtr` 12 :: Ptr Word32))
    maxImageArrayLayers <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxTexelBufferElements <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    maxUniformBufferRange <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    maxStorageBufferRange <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    maxPushConstantsSize <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    maxMemoryAllocationCount <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    maxSamplerAllocationCount <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    bufferImageGranularity <- peek @DeviceSize ((p `plusPtr` 48 :: Ptr DeviceSize))
    sparseAddressSpaceSize <- peek @DeviceSize ((p `plusPtr` 56 :: Ptr DeviceSize))
    maxBoundDescriptorSets <- peek @Word32 ((p `plusPtr` 64 :: Ptr Word32))
    maxPerStageDescriptorSamplers <- peek @Word32 ((p `plusPtr` 68 :: Ptr Word32))
    maxPerStageDescriptorUniformBuffers <- peek @Word32 ((p `plusPtr` 72 :: Ptr Word32))
    maxPerStageDescriptorStorageBuffers <- peek @Word32 ((p `plusPtr` 76 :: Ptr Word32))
    maxPerStageDescriptorSampledImages <- peek @Word32 ((p `plusPtr` 80 :: Ptr Word32))
    maxPerStageDescriptorStorageImages <- peek @Word32 ((p `plusPtr` 84 :: Ptr Word32))
    maxPerStageDescriptorInputAttachments <- peek @Word32 ((p `plusPtr` 88 :: Ptr Word32))
    maxPerStageResources <- peek @Word32 ((p `plusPtr` 92 :: Ptr Word32))
    maxDescriptorSetSamplers <- peek @Word32 ((p `plusPtr` 96 :: Ptr Word32))
    maxDescriptorSetUniformBuffers <- peek @Word32 ((p `plusPtr` 100 :: Ptr Word32))
    maxDescriptorSetUniformBuffersDynamic <- peek @Word32 ((p `plusPtr` 104 :: Ptr Word32))
    maxDescriptorSetStorageBuffers <- peek @Word32 ((p `plusPtr` 108 :: Ptr Word32))
    maxDescriptorSetStorageBuffersDynamic <- peek @Word32 ((p `plusPtr` 112 :: Ptr Word32))
    maxDescriptorSetSampledImages <- peek @Word32 ((p `plusPtr` 116 :: Ptr Word32))
    maxDescriptorSetStorageImages <- peek @Word32 ((p `plusPtr` 120 :: Ptr Word32))
    maxDescriptorSetInputAttachments <- peek @Word32 ((p `plusPtr` 124 :: Ptr Word32))
    maxVertexInputAttributes <- peek @Word32 ((p `plusPtr` 128 :: Ptr Word32))
    maxVertexInputBindings <- peek @Word32 ((p `plusPtr` 132 :: Ptr Word32))
    maxVertexInputAttributeOffset <- peek @Word32 ((p `plusPtr` 136 :: Ptr Word32))
    maxVertexInputBindingStride <- peek @Word32 ((p `plusPtr` 140 :: Ptr Word32))
    maxVertexOutputComponents <- peek @Word32 ((p `plusPtr` 144 :: Ptr Word32))
    maxTessellationGenerationLevel <- peek @Word32 ((p `plusPtr` 148 :: Ptr Word32))
    maxTessellationPatchSize <- peek @Word32 ((p `plusPtr` 152 :: Ptr Word32))
    maxTessellationControlPerVertexInputComponents <- peek @Word32 ((p `plusPtr` 156 :: Ptr Word32))
    maxTessellationControlPerVertexOutputComponents <- peek @Word32 ((p `plusPtr` 160 :: Ptr Word32))
    maxTessellationControlPerPatchOutputComponents <- peek @Word32 ((p `plusPtr` 164 :: Ptr Word32))
    maxTessellationControlTotalOutputComponents <- peek @Word32 ((p `plusPtr` 168 :: Ptr Word32))
    maxTessellationEvaluationInputComponents <- peek @Word32 ((p `plusPtr` 172 :: Ptr Word32))
    maxTessellationEvaluationOutputComponents <- peek @Word32 ((p `plusPtr` 176 :: Ptr Word32))
    maxGeometryShaderInvocations <- peek @Word32 ((p `plusPtr` 180 :: Ptr Word32))
    maxGeometryInputComponents <- peek @Word32 ((p `plusPtr` 184 :: Ptr Word32))
    maxGeometryOutputComponents <- peek @Word32 ((p `plusPtr` 188 :: Ptr Word32))
    maxGeometryOutputVertices <- peek @Word32 ((p `plusPtr` 192 :: Ptr Word32))
    maxGeometryTotalOutputComponents <- peek @Word32 ((p `plusPtr` 196 :: Ptr Word32))
    maxFragmentInputComponents <- peek @Word32 ((p `plusPtr` 200 :: Ptr Word32))
    maxFragmentOutputAttachments <- peek @Word32 ((p `plusPtr` 204 :: Ptr Word32))
    maxFragmentDualSrcAttachments <- peek @Word32 ((p `plusPtr` 208 :: Ptr Word32))
    maxFragmentCombinedOutputResources <- peek @Word32 ((p `plusPtr` 212 :: Ptr Word32))
    maxComputeSharedMemorySize <- peek @Word32 ((p `plusPtr` 216 :: Ptr Word32))
    let pmaxComputeWorkGroupCount = lowerArrayPtr @Word32 ((p `plusPtr` 220 :: Ptr (FixedArray 3 Word32)))
    maxComputeWorkGroupCount0 <- peek @Word32 ((pmaxComputeWorkGroupCount `advancePtrBytes` 0 :: Ptr Word32))
    maxComputeWorkGroupCount1 <- peek @Word32 ((pmaxComputeWorkGroupCount `advancePtrBytes` 4 :: Ptr Word32))
    maxComputeWorkGroupCount2 <- peek @Word32 ((pmaxComputeWorkGroupCount `advancePtrBytes` 8 :: Ptr Word32))
    maxComputeWorkGroupInvocations <- peek @Word32 ((p `plusPtr` 232 :: Ptr Word32))
    let pmaxComputeWorkGroupSize = lowerArrayPtr @Word32 ((p `plusPtr` 236 :: Ptr (FixedArray 3 Word32)))
    maxComputeWorkGroupSize0 <- peek @Word32 ((pmaxComputeWorkGroupSize `advancePtrBytes` 0 :: Ptr Word32))
    maxComputeWorkGroupSize1 <- peek @Word32 ((pmaxComputeWorkGroupSize `advancePtrBytes` 4 :: Ptr Word32))
    maxComputeWorkGroupSize2 <- peek @Word32 ((pmaxComputeWorkGroupSize `advancePtrBytes` 8 :: Ptr Word32))
    subPixelPrecisionBits <- peek @Word32 ((p `plusPtr` 248 :: Ptr Word32))
    subTexelPrecisionBits <- peek @Word32 ((p `plusPtr` 252 :: Ptr Word32))
    mipmapPrecisionBits <- peek @Word32 ((p `plusPtr` 256 :: Ptr Word32))
    maxDrawIndexedIndexValue <- peek @Word32 ((p `plusPtr` 260 :: Ptr Word32))
    maxDrawIndirectCount <- peek @Word32 ((p `plusPtr` 264 :: Ptr Word32))
    maxSamplerLodBias <- peek @CFloat ((p `plusPtr` 268 :: Ptr CFloat))
    maxSamplerAnisotropy <- peek @CFloat ((p `plusPtr` 272 :: Ptr CFloat))
    maxViewports <- peek @Word32 ((p `plusPtr` 276 :: Ptr Word32))
    let pmaxViewportDimensions = lowerArrayPtr @Word32 ((p `plusPtr` 280 :: Ptr (FixedArray 2 Word32)))
    maxViewportDimensions0 <- peek @Word32 ((pmaxViewportDimensions `advancePtrBytes` 0 :: Ptr Word32))
    maxViewportDimensions1 <- peek @Word32 ((pmaxViewportDimensions `advancePtrBytes` 4 :: Ptr Word32))
    let pviewportBoundsRange = lowerArrayPtr @CFloat ((p `plusPtr` 288 :: Ptr (FixedArray 2 CFloat)))
    viewportBoundsRange0 <- peek @CFloat ((pviewportBoundsRange `advancePtrBytes` 0 :: Ptr CFloat))
    viewportBoundsRange1 <- peek @CFloat ((pviewportBoundsRange `advancePtrBytes` 4 :: Ptr CFloat))
    viewportSubPixelBits <- peek @Word32 ((p `plusPtr` 296 :: Ptr Word32))
    minMemoryMapAlignment <- peek @CSize ((p `plusPtr` 304 :: Ptr CSize))
    minTexelBufferOffsetAlignment <- peek @DeviceSize ((p `plusPtr` 312 :: Ptr DeviceSize))
    minUniformBufferOffsetAlignment <- peek @DeviceSize ((p `plusPtr` 320 :: Ptr DeviceSize))
    minStorageBufferOffsetAlignment <- peek @DeviceSize ((p `plusPtr` 328 :: Ptr DeviceSize))
    minTexelOffset <- peek @Int32 ((p `plusPtr` 336 :: Ptr Int32))
    maxTexelOffset <- peek @Word32 ((p `plusPtr` 340 :: Ptr Word32))
    minTexelGatherOffset <- peek @Int32 ((p `plusPtr` 344 :: Ptr Int32))
    maxTexelGatherOffset <- peek @Word32 ((p `plusPtr` 348 :: Ptr Word32))
    minInterpolationOffset <- peek @CFloat ((p `plusPtr` 352 :: Ptr CFloat))
    maxInterpolationOffset <- peek @CFloat ((p `plusPtr` 356 :: Ptr CFloat))
    subPixelInterpolationOffsetBits <- peek @Word32 ((p `plusPtr` 360 :: Ptr Word32))
    maxFramebufferWidth <- peek @Word32 ((p `plusPtr` 364 :: Ptr Word32))
    maxFramebufferHeight <- peek @Word32 ((p `plusPtr` 368 :: Ptr Word32))
    maxFramebufferLayers <- peek @Word32 ((p `plusPtr` 372 :: Ptr Word32))
    framebufferColorSampleCounts <- peek @SampleCountFlags ((p `plusPtr` 376 :: Ptr SampleCountFlags))
    framebufferDepthSampleCounts <- peek @SampleCountFlags ((p `plusPtr` 380 :: Ptr SampleCountFlags))
    framebufferStencilSampleCounts <- peek @SampleCountFlags ((p `plusPtr` 384 :: Ptr SampleCountFlags))
    framebufferNoAttachmentsSampleCounts <- peek @SampleCountFlags ((p `plusPtr` 388 :: Ptr SampleCountFlags))
    maxColorAttachments <- peek @Word32 ((p `plusPtr` 392 :: Ptr Word32))
    sampledImageColorSampleCounts <- peek @SampleCountFlags ((p `plusPtr` 396 :: Ptr SampleCountFlags))
    sampledImageIntegerSampleCounts <- peek @SampleCountFlags ((p `plusPtr` 400 :: Ptr SampleCountFlags))
    sampledImageDepthSampleCounts <- peek @SampleCountFlags ((p `plusPtr` 404 :: Ptr SampleCountFlags))
    sampledImageStencilSampleCounts <- peek @SampleCountFlags ((p `plusPtr` 408 :: Ptr SampleCountFlags))
    storageImageSampleCounts <- peek @SampleCountFlags ((p `plusPtr` 412 :: Ptr SampleCountFlags))
    maxSampleMaskWords <- peek @Word32 ((p `plusPtr` 416 :: Ptr Word32))
    timestampComputeAndGraphics <- peek @Bool32 ((p `plusPtr` 420 :: Ptr Bool32))
    timestampPeriod <- peek @CFloat ((p `plusPtr` 424 :: Ptr CFloat))
    maxClipDistances <- peek @Word32 ((p `plusPtr` 428 :: Ptr Word32))
    maxCullDistances <- peek @Word32 ((p `plusPtr` 432 :: Ptr Word32))
    maxCombinedClipAndCullDistances <- peek @Word32 ((p `plusPtr` 436 :: Ptr Word32))
    discreteQueuePriorities <- peek @Word32 ((p `plusPtr` 440 :: Ptr Word32))
    let ppointSizeRange = lowerArrayPtr @CFloat ((p `plusPtr` 444 :: Ptr (FixedArray 2 CFloat)))
    pointSizeRange0 <- peek @CFloat ((ppointSizeRange `advancePtrBytes` 0 :: Ptr CFloat))
    pointSizeRange1 <- peek @CFloat ((ppointSizeRange `advancePtrBytes` 4 :: Ptr CFloat))
    let plineWidthRange = lowerArrayPtr @CFloat ((p `plusPtr` 452 :: Ptr (FixedArray 2 CFloat)))
    lineWidthRange0 <- peek @CFloat ((plineWidthRange `advancePtrBytes` 0 :: Ptr CFloat))
    lineWidthRange1 <- peek @CFloat ((plineWidthRange `advancePtrBytes` 4 :: Ptr CFloat))
    pointSizeGranularity <- peek @CFloat ((p `plusPtr` 460 :: Ptr CFloat))
    lineWidthGranularity <- peek @CFloat ((p `plusPtr` 464 :: Ptr CFloat))
    strictLines <- peek @Bool32 ((p `plusPtr` 468 :: Ptr Bool32))
    standardSampleLocations <- peek @Bool32 ((p `plusPtr` 472 :: Ptr Bool32))
    optimalBufferCopyOffsetAlignment <- peek @DeviceSize ((p `plusPtr` 480 :: Ptr DeviceSize))
    optimalBufferCopyRowPitchAlignment <- peek @DeviceSize ((p `plusPtr` 488 :: Ptr DeviceSize))
    nonCoherentAtomSize <- peek @DeviceSize ((p `plusPtr` 496 :: Ptr DeviceSize))
    pure $ PhysicalDeviceLimits
             maxImageDimension1D maxImageDimension2D maxImageDimension3D maxImageDimensionCube maxImageArrayLayers maxTexelBufferElements maxUniformBufferRange maxStorageBufferRange maxPushConstantsSize maxMemoryAllocationCount maxSamplerAllocationCount bufferImageGranularity sparseAddressSpaceSize maxBoundDescriptorSets maxPerStageDescriptorSamplers maxPerStageDescriptorUniformBuffers maxPerStageDescriptorStorageBuffers maxPerStageDescriptorSampledImages maxPerStageDescriptorStorageImages maxPerStageDescriptorInputAttachments maxPerStageResources maxDescriptorSetSamplers maxDescriptorSetUniformBuffers maxDescriptorSetUniformBuffersDynamic maxDescriptorSetStorageBuffers maxDescriptorSetStorageBuffersDynamic maxDescriptorSetSampledImages maxDescriptorSetStorageImages maxDescriptorSetInputAttachments maxVertexInputAttributes maxVertexInputBindings maxVertexInputAttributeOffset maxVertexInputBindingStride maxVertexOutputComponents maxTessellationGenerationLevel maxTessellationPatchSize maxTessellationControlPerVertexInputComponents maxTessellationControlPerVertexOutputComponents maxTessellationControlPerPatchOutputComponents maxTessellationControlTotalOutputComponents maxTessellationEvaluationInputComponents maxTessellationEvaluationOutputComponents maxGeometryShaderInvocations maxGeometryInputComponents maxGeometryOutputComponents maxGeometryOutputVertices maxGeometryTotalOutputComponents maxFragmentInputComponents maxFragmentOutputAttachments maxFragmentDualSrcAttachments maxFragmentCombinedOutputResources maxComputeSharedMemorySize ((maxComputeWorkGroupCount0, maxComputeWorkGroupCount1, maxComputeWorkGroupCount2)) maxComputeWorkGroupInvocations ((maxComputeWorkGroupSize0, maxComputeWorkGroupSize1, maxComputeWorkGroupSize2)) subPixelPrecisionBits subTexelPrecisionBits mipmapPrecisionBits maxDrawIndexedIndexValue maxDrawIndirectCount (coerce @CFloat @Float maxSamplerLodBias) (coerce @CFloat @Float maxSamplerAnisotropy) maxViewports ((maxViewportDimensions0, maxViewportDimensions1)) (((coerce @CFloat @Float viewportBoundsRange0), (coerce @CFloat @Float viewportBoundsRange1))) viewportSubPixelBits (coerce @CSize @Word64 minMemoryMapAlignment) minTexelBufferOffsetAlignment minUniformBufferOffsetAlignment minStorageBufferOffsetAlignment minTexelOffset maxTexelOffset minTexelGatherOffset maxTexelGatherOffset (coerce @CFloat @Float minInterpolationOffset) (coerce @CFloat @Float maxInterpolationOffset) subPixelInterpolationOffsetBits maxFramebufferWidth maxFramebufferHeight maxFramebufferLayers framebufferColorSampleCounts framebufferDepthSampleCounts framebufferStencilSampleCounts framebufferNoAttachmentsSampleCounts maxColorAttachments sampledImageColorSampleCounts sampledImageIntegerSampleCounts sampledImageDepthSampleCounts sampledImageStencilSampleCounts storageImageSampleCounts maxSampleMaskWords (bool32ToBool timestampComputeAndGraphics) (coerce @CFloat @Float timestampPeriod) maxClipDistances maxCullDistances maxCombinedClipAndCullDistances discreteQueuePriorities (((coerce @CFloat @Float pointSizeRange0), (coerce @CFloat @Float pointSizeRange1))) (((coerce @CFloat @Float lineWidthRange0), (coerce @CFloat @Float lineWidthRange1))) (coerce @CFloat @Float pointSizeGranularity) (coerce @CFloat @Float lineWidthGranularity) (bool32ToBool strictLines) (bool32ToBool standardSampleLocations) optimalBufferCopyOffsetAlignment optimalBufferCopyRowPitchAlignment nonCoherentAtomSize

instance Storable PhysicalDeviceLimits where
  sizeOf ~_ = 504
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceLimits where
  zero = PhysicalDeviceLimits
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
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           (zero, zero, zero)
           zero
           (zero, zero, zero)
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           (zero, zero)
           (zero, zero)
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
           (zero, zero)
           (zero, zero)
           zero
           zero
           zero
           zero
           zero
           zero
           zero

