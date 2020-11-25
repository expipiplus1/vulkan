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
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Foreign.C.Types (CChar(..))
import Control.Monad.IO.Class (MonadIO)
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
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
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
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Core10.APIConstants (UUID_SIZE)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_validation_features (ValidationFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_validation_flags (ValidationFlagsEXT)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
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

-- No documentation found for TopLevel "vkCreateInstance"
createInstance :: forall a io
                . (Extendss InstanceCreateInfo a, PokeChain a, MonadIO io)
               => -- No documentation found for Nested "vkCreateInstance" "pCreateInfo"
                  (InstanceCreateInfo a)
               -> -- No documentation found for Nested "vkCreateInstance" "pAllocator"
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
  r <- lift $ vkCreateInstance' (forgetExtensions pCreateInfo) pAllocator (pPInstance)
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

-- No documentation found for TopLevel "vkDestroyInstance"
destroyInstance :: forall io
                 . (MonadIO io)
                => -- No documentation found for Nested "vkDestroyInstance" "instance"
                   Instance
                -> -- No documentation found for Nested "vkDestroyInstance" "pAllocator"
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
  lift $ vkDestroyInstance' (instanceHandle (instance')) pAllocator
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEnumeratePhysicalDevices
  :: FunPtr (Ptr Instance_T -> Ptr Word32 -> Ptr (Ptr PhysicalDevice_T) -> IO Result) -> Ptr Instance_T -> Ptr Word32 -> Ptr (Ptr PhysicalDevice_T) -> IO Result

-- No documentation found for TopLevel "vkEnumeratePhysicalDevices"
enumeratePhysicalDevices :: forall io
                          . (MonadIO io)
                         => -- No documentation found for Nested "vkEnumeratePhysicalDevices" "instance"
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
  r <- lift $ vkEnumeratePhysicalDevices' instance'' (pPPhysicalDeviceCount) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPhysicalDeviceCount <- lift $ peek @Word32 pPPhysicalDeviceCount
  pPPhysicalDevices <- ContT $ bracket (callocBytes @(Ptr PhysicalDevice_T) ((fromIntegral (pPhysicalDeviceCount)) * 8)) free
  r' <- lift $ vkEnumeratePhysicalDevices' instance'' (pPPhysicalDeviceCount) (pPPhysicalDevices)
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

-- No documentation found for TopLevel "vkGetDeviceProcAddr"
getDeviceProcAddr :: forall io
                   . (MonadIO io)
                  => -- No documentation found for Nested "vkGetDeviceProcAddr" "device"
                     Device
                  -> -- No documentation found for Nested "vkGetDeviceProcAddr" "pName"
                     ("name" ::: ByteString)
                  -> io (PFN_vkVoidFunction)
getDeviceProcAddr device name = liftIO . evalContT $ do
  let vkGetDeviceProcAddrPtr = pVkGetDeviceProcAddr (deviceCmds (device :: Device))
  lift $ unless (vkGetDeviceProcAddrPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDeviceProcAddr is null" Nothing Nothing
  let vkGetDeviceProcAddr' = mkVkGetDeviceProcAddr vkGetDeviceProcAddrPtr
  pName <- ContT $ useAsCString (name)
  r <- lift $ vkGetDeviceProcAddr' (deviceHandle (device)) pName
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetInstanceProcAddr
  :: FunPtr (Ptr Instance_T -> Ptr CChar -> IO PFN_vkVoidFunction) -> Ptr Instance_T -> Ptr CChar -> IO PFN_vkVoidFunction

-- No documentation found for TopLevel "vkGetInstanceProcAddr"
getInstanceProcAddr :: forall io
                     . (MonadIO io)
                    => -- No documentation found for Nested "vkGetInstanceProcAddr" "instance"
                       Instance
                    -> -- No documentation found for Nested "vkGetInstanceProcAddr" "pName"
                       ("name" ::: ByteString)
                    -> io (PFN_vkVoidFunction)
getInstanceProcAddr instance' name = liftIO . evalContT $ do
  let vkGetInstanceProcAddrPtr = pVkGetInstanceProcAddr (instanceCmds (instance' :: Instance))
  lift $ unless (vkGetInstanceProcAddrPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetInstanceProcAddr is null" Nothing Nothing
  let vkGetInstanceProcAddr' = mkVkGetInstanceProcAddr vkGetInstanceProcAddrPtr
  pName <- ContT $ useAsCString (name)
  r <- lift $ vkGetInstanceProcAddr' (instanceHandle (instance')) pName
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceProperties
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr PhysicalDeviceProperties -> IO ()) -> Ptr PhysicalDevice_T -> Ptr PhysicalDeviceProperties -> IO ()

-- No documentation found for TopLevel "vkGetPhysicalDeviceProperties"
getPhysicalDeviceProperties :: forall io
                             . (MonadIO io)
                            => -- No documentation found for Nested "vkGetPhysicalDeviceProperties" "physicalDevice"
                               PhysicalDevice
                            -> io (PhysicalDeviceProperties)
getPhysicalDeviceProperties physicalDevice = liftIO . evalContT $ do
  let vkGetPhysicalDevicePropertiesPtr = pVkGetPhysicalDeviceProperties (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDevicePropertiesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceProperties is null" Nothing Nothing
  let vkGetPhysicalDeviceProperties' = mkVkGetPhysicalDeviceProperties vkGetPhysicalDevicePropertiesPtr
  pPProperties <- ContT (withZeroCStruct @PhysicalDeviceProperties)
  lift $ vkGetPhysicalDeviceProperties' (physicalDeviceHandle (physicalDevice)) (pPProperties)
  pProperties <- lift $ peekCStruct @PhysicalDeviceProperties pPProperties
  pure $ (pProperties)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceQueueFamilyProperties
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr QueueFamilyProperties -> IO ()) -> Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr QueueFamilyProperties -> IO ()

-- No documentation found for TopLevel "vkGetPhysicalDeviceQueueFamilyProperties"
getPhysicalDeviceQueueFamilyProperties :: forall io
                                        . (MonadIO io)
                                       => -- No documentation found for Nested "vkGetPhysicalDeviceQueueFamilyProperties" "physicalDevice"
                                          PhysicalDevice
                                       -> io (("queueFamilyProperties" ::: Vector QueueFamilyProperties))
getPhysicalDeviceQueueFamilyProperties physicalDevice = liftIO . evalContT $ do
  let vkGetPhysicalDeviceQueueFamilyPropertiesPtr = pVkGetPhysicalDeviceQueueFamilyProperties (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceQueueFamilyPropertiesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceQueueFamilyProperties is null" Nothing Nothing
  let vkGetPhysicalDeviceQueueFamilyProperties' = mkVkGetPhysicalDeviceQueueFamilyProperties vkGetPhysicalDeviceQueueFamilyPropertiesPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPQueueFamilyPropertyCount <- ContT $ bracket (callocBytes @Word32 4) free
  lift $ vkGetPhysicalDeviceQueueFamilyProperties' physicalDevice' (pPQueueFamilyPropertyCount) (nullPtr)
  pQueueFamilyPropertyCount <- lift $ peek @Word32 pPQueueFamilyPropertyCount
  pPQueueFamilyProperties <- ContT $ bracket (callocBytes @QueueFamilyProperties ((fromIntegral (pQueueFamilyPropertyCount)) * 24)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPQueueFamilyProperties `advancePtrBytes` (i * 24) :: Ptr QueueFamilyProperties) . ($ ())) [0..(fromIntegral (pQueueFamilyPropertyCount)) - 1]
  lift $ vkGetPhysicalDeviceQueueFamilyProperties' physicalDevice' (pPQueueFamilyPropertyCount) ((pPQueueFamilyProperties))
  pQueueFamilyPropertyCount' <- lift $ peek @Word32 pPQueueFamilyPropertyCount
  pQueueFamilyProperties' <- lift $ generateM (fromIntegral (pQueueFamilyPropertyCount')) (\i -> peekCStruct @QueueFamilyProperties (((pPQueueFamilyProperties) `advancePtrBytes` (24 * (i)) :: Ptr QueueFamilyProperties)))
  pure $ (pQueueFamilyProperties')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceMemoryProperties
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr PhysicalDeviceMemoryProperties -> IO ()) -> Ptr PhysicalDevice_T -> Ptr PhysicalDeviceMemoryProperties -> IO ()

-- No documentation found for TopLevel "vkGetPhysicalDeviceMemoryProperties"
getPhysicalDeviceMemoryProperties :: forall io
                                   . (MonadIO io)
                                  => -- No documentation found for Nested "vkGetPhysicalDeviceMemoryProperties" "physicalDevice"
                                     PhysicalDevice
                                  -> io (PhysicalDeviceMemoryProperties)
getPhysicalDeviceMemoryProperties physicalDevice = liftIO . evalContT $ do
  let vkGetPhysicalDeviceMemoryPropertiesPtr = pVkGetPhysicalDeviceMemoryProperties (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceMemoryPropertiesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceMemoryProperties is null" Nothing Nothing
  let vkGetPhysicalDeviceMemoryProperties' = mkVkGetPhysicalDeviceMemoryProperties vkGetPhysicalDeviceMemoryPropertiesPtr
  pPMemoryProperties <- ContT (withZeroCStruct @PhysicalDeviceMemoryProperties)
  lift $ vkGetPhysicalDeviceMemoryProperties' (physicalDeviceHandle (physicalDevice)) (pPMemoryProperties)
  pMemoryProperties <- lift $ peekCStruct @PhysicalDeviceMemoryProperties pPMemoryProperties
  pure $ (pMemoryProperties)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceFeatures
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr PhysicalDeviceFeatures -> IO ()) -> Ptr PhysicalDevice_T -> Ptr PhysicalDeviceFeatures -> IO ()

-- No documentation found for TopLevel "vkGetPhysicalDeviceFeatures"
getPhysicalDeviceFeatures :: forall io
                           . (MonadIO io)
                          => -- No documentation found for Nested "vkGetPhysicalDeviceFeatures" "physicalDevice"
                             PhysicalDevice
                          -> io (PhysicalDeviceFeatures)
getPhysicalDeviceFeatures physicalDevice = liftIO . evalContT $ do
  let vkGetPhysicalDeviceFeaturesPtr = pVkGetPhysicalDeviceFeatures (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceFeaturesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceFeatures is null" Nothing Nothing
  let vkGetPhysicalDeviceFeatures' = mkVkGetPhysicalDeviceFeatures vkGetPhysicalDeviceFeaturesPtr
  pPFeatures <- ContT (withZeroCStruct @PhysicalDeviceFeatures)
  lift $ vkGetPhysicalDeviceFeatures' (physicalDeviceHandle (physicalDevice)) (pPFeatures)
  pFeatures <- lift $ peekCStruct @PhysicalDeviceFeatures pPFeatures
  pure $ (pFeatures)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceFormatProperties
  :: FunPtr (Ptr PhysicalDevice_T -> Format -> Ptr FormatProperties -> IO ()) -> Ptr PhysicalDevice_T -> Format -> Ptr FormatProperties -> IO ()

-- No documentation found for TopLevel "vkGetPhysicalDeviceFormatProperties"
getPhysicalDeviceFormatProperties :: forall io
                                   . (MonadIO io)
                                  => -- No documentation found for Nested "vkGetPhysicalDeviceFormatProperties" "physicalDevice"
                                     PhysicalDevice
                                  -> -- No documentation found for Nested "vkGetPhysicalDeviceFormatProperties" "format"
                                     Format
                                  -> io (FormatProperties)
getPhysicalDeviceFormatProperties physicalDevice format = liftIO . evalContT $ do
  let vkGetPhysicalDeviceFormatPropertiesPtr = pVkGetPhysicalDeviceFormatProperties (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceFormatPropertiesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceFormatProperties is null" Nothing Nothing
  let vkGetPhysicalDeviceFormatProperties' = mkVkGetPhysicalDeviceFormatProperties vkGetPhysicalDeviceFormatPropertiesPtr
  pPFormatProperties <- ContT (withZeroCStruct @FormatProperties)
  lift $ vkGetPhysicalDeviceFormatProperties' (physicalDeviceHandle (physicalDevice)) (format) (pPFormatProperties)
  pFormatProperties <- lift $ peekCStruct @FormatProperties pPFormatProperties
  pure $ (pFormatProperties)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceImageFormatProperties
  :: FunPtr (Ptr PhysicalDevice_T -> Format -> ImageType -> ImageTiling -> ImageUsageFlags -> ImageCreateFlags -> Ptr ImageFormatProperties -> IO Result) -> Ptr PhysicalDevice_T -> Format -> ImageType -> ImageTiling -> ImageUsageFlags -> ImageCreateFlags -> Ptr ImageFormatProperties -> IO Result

-- No documentation found for TopLevel "vkGetPhysicalDeviceImageFormatProperties"
getPhysicalDeviceImageFormatProperties :: forall io
                                        . (MonadIO io)
                                       => -- No documentation found for Nested "vkGetPhysicalDeviceImageFormatProperties" "physicalDevice"
                                          PhysicalDevice
                                       -> -- No documentation found for Nested "vkGetPhysicalDeviceImageFormatProperties" "format"
                                          Format
                                       -> -- No documentation found for Nested "vkGetPhysicalDeviceImageFormatProperties" "type"
                                          ImageType
                                       -> -- No documentation found for Nested "vkGetPhysicalDeviceImageFormatProperties" "tiling"
                                          ImageTiling
                                       -> -- No documentation found for Nested "vkGetPhysicalDeviceImageFormatProperties" "usage"
                                          ImageUsageFlags
                                       -> -- No documentation found for Nested "vkGetPhysicalDeviceImageFormatProperties" "flags"
                                          ImageCreateFlags
                                       -> io (ImageFormatProperties)
getPhysicalDeviceImageFormatProperties physicalDevice format type' tiling usage flags = liftIO . evalContT $ do
  let vkGetPhysicalDeviceImageFormatPropertiesPtr = pVkGetPhysicalDeviceImageFormatProperties (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceImageFormatPropertiesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceImageFormatProperties is null" Nothing Nothing
  let vkGetPhysicalDeviceImageFormatProperties' = mkVkGetPhysicalDeviceImageFormatProperties vkGetPhysicalDeviceImageFormatPropertiesPtr
  pPImageFormatProperties <- ContT (withZeroCStruct @ImageFormatProperties)
  r <- lift $ vkGetPhysicalDeviceImageFormatProperties' (physicalDeviceHandle (physicalDevice)) (format) (type') (tiling) (usage) (flags) (pPImageFormatProperties)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pImageFormatProperties <- lift $ peekCStruct @ImageFormatProperties pPImageFormatProperties
  pure $ (pImageFormatProperties)



-- No documentation found for TopLevel "VkPhysicalDeviceProperties"
data PhysicalDeviceProperties = PhysicalDeviceProperties
  { -- No documentation found for Nested "VkPhysicalDeviceProperties" "apiVersion"
    apiVersion :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceProperties" "driverVersion"
    driverVersion :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceProperties" "vendorID"
    vendorID :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceProperties" "deviceID"
    deviceID :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceProperties" "deviceType"
    deviceType :: PhysicalDeviceType
  , -- No documentation found for Nested "VkPhysicalDeviceProperties" "deviceName"
    deviceName :: ByteString
  , -- No documentation found for Nested "VkPhysicalDeviceProperties" "pipelineCacheUUID"
    pipelineCacheUUID :: ByteString
  , -- No documentation found for Nested "VkPhysicalDeviceProperties" "limits"
    limits :: PhysicalDeviceLimits
  , -- No documentation found for Nested "VkPhysicalDeviceProperties" "sparseProperties"
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



-- No documentation found for TopLevel "VkApplicationInfo"
data ApplicationInfo = ApplicationInfo
  { -- No documentation found for Nested "VkApplicationInfo" "pApplicationName"
    applicationName :: Maybe ByteString
  , -- No documentation found for Nested "VkApplicationInfo" "applicationVersion"
    applicationVersion :: Word32
  , -- No documentation found for Nested "VkApplicationInfo" "pEngineName"
    engineName :: Maybe ByteString
  , -- No documentation found for Nested "VkApplicationInfo" "engineVersion"
    engineVersion :: Word32
  , -- No documentation found for Nested "VkApplicationInfo" "apiVersion"
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
    pApplicationName' <- maybePeek (\j -> packCString  (j)) pApplicationName
    applicationVersion <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pEngineName <- peek @(Ptr CChar) ((p `plusPtr` 32 :: Ptr (Ptr CChar)))
    pEngineName' <- maybePeek (\j -> packCString  (j)) pEngineName
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



-- No documentation found for TopLevel "VkInstanceCreateInfo"
data InstanceCreateInfo (es :: [Type]) = InstanceCreateInfo
  { -- No documentation found for Nested "VkInstanceCreateInfo" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkInstanceCreateInfo" "flags"
    flags :: InstanceCreateFlags
  , -- No documentation found for Nested "VkInstanceCreateInfo" "pApplicationInfo"
    applicationInfo :: Maybe ApplicationInfo
  , -- No documentation found for Nested "VkInstanceCreateInfo" "ppEnabledLayerNames"
    enabledLayerNames :: Vector ByteString
  , -- No documentation found for Nested "VkInstanceCreateInfo" "ppEnabledExtensionNames"
    enabledExtensionNames :: Vector ByteString
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (InstanceCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (InstanceCreateInfo es)

instance Extensible InstanceCreateInfo where
  extensibleType = STRUCTURE_TYPE_INSTANCE_CREATE_INFO
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
    pPpEnabledLayerNames' <- ContT $ allocaBytesAligned @(Ptr CChar) ((Data.Vector.length (mempty)) * 8) 8
    Data.Vector.imapM_ (\i e -> do
      ppEnabledLayerNames'' <- ContT $ useAsCString (e)
      lift $ poke (pPpEnabledLayerNames' `plusPtr` (8 * (i)) :: Ptr (Ptr CChar)) ppEnabledLayerNames'') (mempty)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr (Ptr CChar)))) (pPpEnabledLayerNames')
    pPpEnabledExtensionNames' <- ContT $ allocaBytesAligned @(Ptr CChar) ((Data.Vector.length (mempty)) * 8) 8
    Data.Vector.imapM_ (\i e -> do
      ppEnabledExtensionNames'' <- ContT $ useAsCString (e)
      lift $ poke (pPpEnabledExtensionNames' `plusPtr` (8 * (i)) :: Ptr (Ptr CChar)) ppEnabledExtensionNames'') (mempty)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr (Ptr CChar)))) (pPpEnabledExtensionNames')
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



-- No documentation found for TopLevel "VkQueueFamilyProperties"
data QueueFamilyProperties = QueueFamilyProperties
  { -- No documentation found for Nested "VkQueueFamilyProperties" "queueFlags"
    queueFlags :: QueueFlags
  , -- No documentation found for Nested "VkQueueFamilyProperties" "queueCount"
    queueCount :: Word32
  , -- No documentation found for Nested "VkQueueFamilyProperties" "timestampValidBits"
    timestampValidBits :: Word32
  , -- No documentation found for Nested "VkQueueFamilyProperties" "minImageTransferGranularity"
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



-- No documentation found for TopLevel "VkPhysicalDeviceMemoryProperties"
data PhysicalDeviceMemoryProperties = PhysicalDeviceMemoryProperties
  { -- No documentation found for Nested "VkPhysicalDeviceMemoryProperties" "memoryTypeCount"
    memoryTypeCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceMemoryProperties" "memoryTypes"
    memoryTypes :: Vector MemoryType
  , -- No documentation found for Nested "VkPhysicalDeviceMemoryProperties" "memoryHeapCount"
    memoryHeapCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceMemoryProperties" "memoryHeaps"
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
    unless ((Data.Vector.length $ (mempty)) <= MAX_MEMORY_TYPES) $
      throwIO $ IOError Nothing InvalidArgument "" "memoryTypes is too long, a maximum of MAX_MEMORY_TYPES elements are allowed" Nothing Nothing
    Data.Vector.imapM_ (\i e -> poke ((lowerArrayPtr ((p `plusPtr` 4 :: Ptr (FixedArray MAX_MEMORY_TYPES MemoryType)))) `plusPtr` (8 * (i)) :: Ptr MemoryType) (e)) (mempty)
    poke ((p `plusPtr` 260 :: Ptr Word32)) (zero)
    unless ((Data.Vector.length $ (mempty)) <= MAX_MEMORY_HEAPS) $
      throwIO $ IOError Nothing InvalidArgument "" "memoryHeaps is too long, a maximum of MAX_MEMORY_HEAPS elements are allowed" Nothing Nothing
    Data.Vector.imapM_ (\i e -> poke ((lowerArrayPtr ((p `plusPtr` 264 :: Ptr (FixedArray MAX_MEMORY_HEAPS MemoryHeap)))) `plusPtr` (16 * (i)) :: Ptr MemoryHeap) (e)) (mempty)
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



-- No documentation found for TopLevel "VkMemoryType"
data MemoryType = MemoryType
  { -- No documentation found for Nested "VkMemoryType" "propertyFlags"
    propertyFlags :: MemoryPropertyFlags
  , -- No documentation found for Nested "VkMemoryType" "heapIndex"
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



-- No documentation found for TopLevel "VkMemoryHeap"
data MemoryHeap = MemoryHeap
  { -- No documentation found for Nested "VkMemoryHeap" "size"
    size :: DeviceSize
  , -- No documentation found for Nested "VkMemoryHeap" "flags"
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



-- No documentation found for TopLevel "VkFormatProperties"
data FormatProperties = FormatProperties
  { -- No documentation found for Nested "VkFormatProperties" "linearTilingFeatures"
    linearTilingFeatures :: FormatFeatureFlags
  , -- No documentation found for Nested "VkFormatProperties" "optimalTilingFeatures"
    optimalTilingFeatures :: FormatFeatureFlags
  , -- No documentation found for Nested "VkFormatProperties" "bufferFeatures"
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



-- No documentation found for TopLevel "VkImageFormatProperties"
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



-- No documentation found for TopLevel "VkPhysicalDeviceFeatures"
data PhysicalDeviceFeatures = PhysicalDeviceFeatures
  { -- No documentation found for Nested "VkPhysicalDeviceFeatures" "robustBufferAccess"
    robustBufferAccess :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "fullDrawIndexUint32"
    fullDrawIndexUint32 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "imageCubeArray"
    imageCubeArray :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "independentBlend"
    independentBlend :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "geometryShader"
    geometryShader :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "tessellationShader"
    tessellationShader :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "sampleRateShading"
    sampleRateShading :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "dualSrcBlend"
    dualSrcBlend :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "logicOp"
    logicOp :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "multiDrawIndirect"
    multiDrawIndirect :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "drawIndirectFirstInstance"
    drawIndirectFirstInstance :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "depthClamp"
    depthClamp :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "depthBiasClamp"
    depthBiasClamp :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "fillModeNonSolid"
    fillModeNonSolid :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "depthBounds"
    depthBounds :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "wideLines"
    wideLines :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "largePoints"
    largePoints :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "alphaToOne"
    alphaToOne :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "multiViewport"
    multiViewport :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "samplerAnisotropy"
    samplerAnisotropy :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "textureCompressionETC2"
    textureCompressionETC2 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "textureCompressionASTC_LDR"
    textureCompressionASTC_LDR :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "textureCompressionBC"
    textureCompressionBC :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "occlusionQueryPrecise"
    occlusionQueryPrecise :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "pipelineStatisticsQuery"
    pipelineStatisticsQuery :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "vertexPipelineStoresAndAtomics"
    vertexPipelineStoresAndAtomics :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "fragmentStoresAndAtomics"
    fragmentStoresAndAtomics :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "shaderTessellationAndGeometryPointSize"
    shaderTessellationAndGeometryPointSize :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "shaderImageGatherExtended"
    shaderImageGatherExtended :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "shaderStorageImageExtendedFormats"
    shaderStorageImageExtendedFormats :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "shaderStorageImageMultisample"
    shaderStorageImageMultisample :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "shaderStorageImageReadWithoutFormat"
    shaderStorageImageReadWithoutFormat :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "shaderStorageImageWriteWithoutFormat"
    shaderStorageImageWriteWithoutFormat :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "shaderUniformBufferArrayDynamicIndexing"
    shaderUniformBufferArrayDynamicIndexing :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "shaderSampledImageArrayDynamicIndexing"
    shaderSampledImageArrayDynamicIndexing :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "shaderStorageBufferArrayDynamicIndexing"
    shaderStorageBufferArrayDynamicIndexing :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "shaderStorageImageArrayDynamicIndexing"
    shaderStorageImageArrayDynamicIndexing :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "shaderClipDistance"
    shaderClipDistance :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "shaderCullDistance"
    shaderCullDistance :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "shaderFloat64"
    shaderFloat64 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "shaderInt64"
    shaderInt64 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "shaderInt16"
    shaderInt16 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "shaderResourceResidency"
    shaderResourceResidency :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "shaderResourceMinLod"
    shaderResourceMinLod :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "sparseBinding"
    sparseBinding :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "sparseResidencyBuffer"
    sparseResidencyBuffer :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "sparseResidencyImage2D"
    sparseResidencyImage2D :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "sparseResidencyImage3D"
    sparseResidencyImage3D :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "sparseResidency2Samples"
    sparseResidency2Samples :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "sparseResidency4Samples"
    sparseResidency4Samples :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "sparseResidency8Samples"
    sparseResidency8Samples :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "sparseResidency16Samples"
    sparseResidency16Samples :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "sparseResidencyAliased"
    sparseResidencyAliased :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "variableMultisampleRate"
    variableMultisampleRate :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures" "inheritedQueries"
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



-- No documentation found for TopLevel "VkPhysicalDeviceSparseProperties"
data PhysicalDeviceSparseProperties = PhysicalDeviceSparseProperties
  { -- No documentation found for Nested "VkPhysicalDeviceSparseProperties" "residencyStandard2DBlockShape"
    residencyStandard2DBlockShape :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceSparseProperties" "residencyStandard2DMultisampleBlockShape"
    residencyStandard2DMultisampleBlockShape :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceSparseProperties" "residencyStandard3DBlockShape"
    residencyStandard3DBlockShape :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceSparseProperties" "residencyAlignedMipSize"
    residencyAlignedMipSize :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceSparseProperties" "residencyNonResidentStrict"
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



-- No documentation found for TopLevel "VkPhysicalDeviceLimits"
data PhysicalDeviceLimits = PhysicalDeviceLimits
  { -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxImageDimension1D"
    maxImageDimension1D :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxImageDimension2D"
    maxImageDimension2D :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxImageDimension3D"
    maxImageDimension3D :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxImageDimensionCube"
    maxImageDimensionCube :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxImageArrayLayers"
    maxImageArrayLayers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxTexelBufferElements"
    maxTexelBufferElements :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxUniformBufferRange"
    maxUniformBufferRange :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxStorageBufferRange"
    maxStorageBufferRange :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxPushConstantsSize"
    maxPushConstantsSize :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxMemoryAllocationCount"
    maxMemoryAllocationCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxSamplerAllocationCount"
    maxSamplerAllocationCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "bufferImageGranularity"
    bufferImageGranularity :: DeviceSize
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "sparseAddressSpaceSize"
    sparseAddressSpaceSize :: DeviceSize
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxBoundDescriptorSets"
    maxBoundDescriptorSets :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxPerStageDescriptorSamplers"
    maxPerStageDescriptorSamplers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxPerStageDescriptorUniformBuffers"
    maxPerStageDescriptorUniformBuffers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxPerStageDescriptorStorageBuffers"
    maxPerStageDescriptorStorageBuffers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxPerStageDescriptorSampledImages"
    maxPerStageDescriptorSampledImages :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxPerStageDescriptorStorageImages"
    maxPerStageDescriptorStorageImages :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxPerStageDescriptorInputAttachments"
    maxPerStageDescriptorInputAttachments :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxPerStageResources"
    maxPerStageResources :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxDescriptorSetSamplers"
    maxDescriptorSetSamplers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxDescriptorSetUniformBuffers"
    maxDescriptorSetUniformBuffers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxDescriptorSetUniformBuffersDynamic"
    maxDescriptorSetUniformBuffersDynamic :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxDescriptorSetStorageBuffers"
    maxDescriptorSetStorageBuffers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxDescriptorSetStorageBuffersDynamic"
    maxDescriptorSetStorageBuffersDynamic :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxDescriptorSetSampledImages"
    maxDescriptorSetSampledImages :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxDescriptorSetStorageImages"
    maxDescriptorSetStorageImages :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxDescriptorSetInputAttachments"
    maxDescriptorSetInputAttachments :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxVertexInputAttributes"
    maxVertexInputAttributes :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxVertexInputBindings"
    maxVertexInputBindings :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxVertexInputAttributeOffset"
    maxVertexInputAttributeOffset :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxVertexInputBindingStride"
    maxVertexInputBindingStride :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxVertexOutputComponents"
    maxVertexOutputComponents :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxTessellationGenerationLevel"
    maxTessellationGenerationLevel :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxTessellationPatchSize"
    maxTessellationPatchSize :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxTessellationControlPerVertexInputComponents"
    maxTessellationControlPerVertexInputComponents :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxTessellationControlPerVertexOutputComponents"
    maxTessellationControlPerVertexOutputComponents :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxTessellationControlPerPatchOutputComponents"
    maxTessellationControlPerPatchOutputComponents :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxTessellationControlTotalOutputComponents"
    maxTessellationControlTotalOutputComponents :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxTessellationEvaluationInputComponents"
    maxTessellationEvaluationInputComponents :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxTessellationEvaluationOutputComponents"
    maxTessellationEvaluationOutputComponents :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxGeometryShaderInvocations"
    maxGeometryShaderInvocations :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxGeometryInputComponents"
    maxGeometryInputComponents :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxGeometryOutputComponents"
    maxGeometryOutputComponents :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxGeometryOutputVertices"
    maxGeometryOutputVertices :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxGeometryTotalOutputComponents"
    maxGeometryTotalOutputComponents :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxFragmentInputComponents"
    maxFragmentInputComponents :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxFragmentOutputAttachments"
    maxFragmentOutputAttachments :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxFragmentDualSrcAttachments"
    maxFragmentDualSrcAttachments :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxFragmentCombinedOutputResources"
    maxFragmentCombinedOutputResources :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxComputeSharedMemorySize"
    maxComputeSharedMemorySize :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxComputeWorkGroupCount"
    maxComputeWorkGroupCount :: (Word32, Word32, Word32)
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxComputeWorkGroupInvocations"
    maxComputeWorkGroupInvocations :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxComputeWorkGroupSize"
    maxComputeWorkGroupSize :: (Word32, Word32, Word32)
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "subPixelPrecisionBits"
    subPixelPrecisionBits :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "subTexelPrecisionBits"
    subTexelPrecisionBits :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "mipmapPrecisionBits"
    mipmapPrecisionBits :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxDrawIndexedIndexValue"
    maxDrawIndexedIndexValue :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxDrawIndirectCount"
    maxDrawIndirectCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxSamplerLodBias"
    maxSamplerLodBias :: Float
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxSamplerAnisotropy"
    maxSamplerAnisotropy :: Float
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxViewports"
    maxViewports :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxViewportDimensions"
    maxViewportDimensions :: (Word32, Word32)
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "viewportBoundsRange"
    viewportBoundsRange :: (Float, Float)
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "viewportSubPixelBits"
    viewportSubPixelBits :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "minMemoryMapAlignment"
    minMemoryMapAlignment :: Word64
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "minTexelBufferOffsetAlignment"
    minTexelBufferOffsetAlignment :: DeviceSize
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "minUniformBufferOffsetAlignment"
    minUniformBufferOffsetAlignment :: DeviceSize
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "minStorageBufferOffsetAlignment"
    minStorageBufferOffsetAlignment :: DeviceSize
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "minTexelOffset"
    minTexelOffset :: Int32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxTexelOffset"
    maxTexelOffset :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "minTexelGatherOffset"
    minTexelGatherOffset :: Int32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxTexelGatherOffset"
    maxTexelGatherOffset :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "minInterpolationOffset"
    minInterpolationOffset :: Float
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxInterpolationOffset"
    maxInterpolationOffset :: Float
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "subPixelInterpolationOffsetBits"
    subPixelInterpolationOffsetBits :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxFramebufferWidth"
    maxFramebufferWidth :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxFramebufferHeight"
    maxFramebufferHeight :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxFramebufferLayers"
    maxFramebufferLayers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "framebufferColorSampleCounts"
    framebufferColorSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "framebufferDepthSampleCounts"
    framebufferDepthSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "framebufferStencilSampleCounts"
    framebufferStencilSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "framebufferNoAttachmentsSampleCounts"
    framebufferNoAttachmentsSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxColorAttachments"
    maxColorAttachments :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "sampledImageColorSampleCounts"
    sampledImageColorSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "sampledImageIntegerSampleCounts"
    sampledImageIntegerSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "sampledImageDepthSampleCounts"
    sampledImageDepthSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "sampledImageStencilSampleCounts"
    sampledImageStencilSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "storageImageSampleCounts"
    storageImageSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxSampleMaskWords"
    maxSampleMaskWords :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "timestampComputeAndGraphics"
    timestampComputeAndGraphics :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "timestampPeriod"
    timestampPeriod :: Float
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxClipDistances"
    maxClipDistances :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxCullDistances"
    maxCullDistances :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "maxCombinedClipAndCullDistances"
    maxCombinedClipAndCullDistances :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "discreteQueuePriorities"
    discreteQueuePriorities :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "pointSizeRange"
    pointSizeRange :: (Float, Float)
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "lineWidthRange"
    lineWidthRange :: (Float, Float)
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "pointSizeGranularity"
    pointSizeGranularity :: Float
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "lineWidthGranularity"
    lineWidthGranularity :: Float
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "strictLines"
    strictLines :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "standardSampleLocations"
    standardSampleLocations :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "optimalBufferCopyOffsetAlignment"
    optimalBufferCopyOffsetAlignment :: DeviceSize
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "optimalBufferCopyRowPitchAlignment"
    optimalBufferCopyRowPitchAlignment :: DeviceSize
  , -- No documentation found for Nested "VkPhysicalDeviceLimits" "nonCoherentAtomSize"
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
             maxImageDimension1D maxImageDimension2D maxImageDimension3D maxImageDimensionCube maxImageArrayLayers maxTexelBufferElements maxUniformBufferRange maxStorageBufferRange maxPushConstantsSize maxMemoryAllocationCount maxSamplerAllocationCount bufferImageGranularity sparseAddressSpaceSize maxBoundDescriptorSets maxPerStageDescriptorSamplers maxPerStageDescriptorUniformBuffers maxPerStageDescriptorStorageBuffers maxPerStageDescriptorSampledImages maxPerStageDescriptorStorageImages maxPerStageDescriptorInputAttachments maxPerStageResources maxDescriptorSetSamplers maxDescriptorSetUniformBuffers maxDescriptorSetUniformBuffersDynamic maxDescriptorSetStorageBuffers maxDescriptorSetStorageBuffersDynamic maxDescriptorSetSampledImages maxDescriptorSetStorageImages maxDescriptorSetInputAttachments maxVertexInputAttributes maxVertexInputBindings maxVertexInputAttributeOffset maxVertexInputBindingStride maxVertexOutputComponents maxTessellationGenerationLevel maxTessellationPatchSize maxTessellationControlPerVertexInputComponents maxTessellationControlPerVertexOutputComponents maxTessellationControlPerPatchOutputComponents maxTessellationControlTotalOutputComponents maxTessellationEvaluationInputComponents maxTessellationEvaluationOutputComponents maxGeometryShaderInvocations maxGeometryInputComponents maxGeometryOutputComponents maxGeometryOutputVertices maxGeometryTotalOutputComponents maxFragmentInputComponents maxFragmentOutputAttachments maxFragmentDualSrcAttachments maxFragmentCombinedOutputResources maxComputeSharedMemorySize ((maxComputeWorkGroupCount0, maxComputeWorkGroupCount1, maxComputeWorkGroupCount2)) maxComputeWorkGroupInvocations ((maxComputeWorkGroupSize0, maxComputeWorkGroupSize1, maxComputeWorkGroupSize2)) subPixelPrecisionBits subTexelPrecisionBits mipmapPrecisionBits maxDrawIndexedIndexValue maxDrawIndirectCount ((\(CFloat a) -> a) maxSamplerLodBias) ((\(CFloat a) -> a) maxSamplerAnisotropy) maxViewports ((maxViewportDimensions0, maxViewportDimensions1)) ((((\(CFloat a) -> a) viewportBoundsRange0), ((\(CFloat a) -> a) viewportBoundsRange1))) viewportSubPixelBits ((\(CSize a) -> a) minMemoryMapAlignment) minTexelBufferOffsetAlignment minUniformBufferOffsetAlignment minStorageBufferOffsetAlignment minTexelOffset maxTexelOffset minTexelGatherOffset maxTexelGatherOffset ((\(CFloat a) -> a) minInterpolationOffset) ((\(CFloat a) -> a) maxInterpolationOffset) subPixelInterpolationOffsetBits maxFramebufferWidth maxFramebufferHeight maxFramebufferLayers framebufferColorSampleCounts framebufferDepthSampleCounts framebufferStencilSampleCounts framebufferNoAttachmentsSampleCounts maxColorAttachments sampledImageColorSampleCounts sampledImageIntegerSampleCounts sampledImageDepthSampleCounts sampledImageStencilSampleCounts storageImageSampleCounts maxSampleMaskWords (bool32ToBool timestampComputeAndGraphics) ((\(CFloat a) -> a) timestampPeriod) maxClipDistances maxCullDistances maxCombinedClipAndCullDistances discreteQueuePriorities ((((\(CFloat a) -> a) pointSizeRange0), ((\(CFloat a) -> a) pointSizeRange1))) ((((\(CFloat a) -> a) lineWidthRange0), ((\(CFloat a) -> a) lineWidthRange1))) ((\(CFloat a) -> a) pointSizeGranularity) ((\(CFloat a) -> a) lineWidthGranularity) (bool32ToBool strictLines) (bool32ToBool standardSampleLocations) optimalBufferCopyOffsetAlignment optimalBufferCopyRowPitchAlignment nonCoherentAtomSize


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

