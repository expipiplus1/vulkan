{-# language CPP #-}
-- No documentation found for Chapter "Memory"
module Vulkan.Core10.Memory  ( allocateMemory
                             , withMemory
                             , freeMemory
                             , mapMemory
                             , withMappedMemory
                             , unmapMemory
                             , flushMappedMemoryRanges
                             , invalidateMappedMemoryRanges
                             , getDeviceMemoryCommitment
                             , MemoryAllocateInfo(..)
                             , MappedMemoryRange(..)
                             , MemoryMapFlags(..)
                             ) where

import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Control.Monad.IO.Class (MonadIO)
import Data.Type.Equality ((:~:)(Refl))
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
import Data.Vector (Vector)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_dedicated_allocation (DedicatedAllocationMemoryAllocateInfoNV)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkAllocateMemory))
import Vulkan.Dynamic (DeviceCmds(pVkFlushMappedMemoryRanges))
import Vulkan.Dynamic (DeviceCmds(pVkFreeMemory))
import Vulkan.Dynamic (DeviceCmds(pVkGetDeviceMemoryCommitment))
import Vulkan.Dynamic (DeviceCmds(pVkInvalidateMappedMemoryRanges))
import Vulkan.Dynamic (DeviceCmds(pVkMapMemory))
import Vulkan.Dynamic (DeviceCmds(pVkUnmapMemory))
import Vulkan.Core10.Handles (DeviceMemory)
import Vulkan.Core10.Handles (DeviceMemory(..))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_memory (ExportMemoryAllocateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_external_memory (ExportMemoryAllocateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_memory_win32 (ExportMemoryWin32HandleInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_external_memory_win32 (ExportMemoryWin32HandleInfoNV)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer (ImportAndroidHardwareBufferInfoANDROID)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_memory_fd (ImportMemoryFdInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_external_memory_host (ImportMemoryHostPointerInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_memory_win32 (ImportMemoryWin32HandleInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_external_memory_win32 (ImportMemoryWin32HandleInfoNV)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_device_group (MemoryAllocateFlagsInfo)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation (MemoryDedicatedAllocateInfo)
import Vulkan.Core10.Enums.MemoryMapFlags (MemoryMapFlags)
import Vulkan.Core10.Enums.MemoryMapFlags (MemoryMapFlags(..))
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address (MemoryOpaqueCaptureAddressAllocateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_memory_priority (MemoryPriorityAllocateInfoEXT)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MAPPED_MEMORY_RANGE))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Enums.MemoryMapFlags (MemoryMapFlags(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkAllocateMemory
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct MemoryAllocateInfo) -> Ptr AllocationCallbacks -> Ptr DeviceMemory -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct MemoryAllocateInfo) -> Ptr AllocationCallbacks -> Ptr DeviceMemory -> IO Result

-- No documentation found for TopLevel "vkAllocateMemory"
allocateMemory :: forall a io
                . (Extendss MemoryAllocateInfo a, PokeChain a, MonadIO io)
               => -- No documentation found for Nested "vkAllocateMemory" "device"
                  Device
               -> -- No documentation found for Nested "vkAllocateMemory" "pAllocateInfo"
                  (MemoryAllocateInfo a)
               -> -- No documentation found for Nested "vkAllocateMemory" "pAllocator"
                  ("allocator" ::: Maybe AllocationCallbacks)
               -> io (DeviceMemory)
allocateMemory device allocateInfo allocator = liftIO . evalContT $ do
  let vkAllocateMemoryPtr = pVkAllocateMemory (deviceCmds (device :: Device))
  lift $ unless (vkAllocateMemoryPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkAllocateMemory is null" Nothing Nothing
  let vkAllocateMemory' = mkVkAllocateMemory vkAllocateMemoryPtr
  pAllocateInfo <- ContT $ withCStruct (allocateInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPMemory <- ContT $ bracket (callocBytes @DeviceMemory 8) free
  r <- lift $ vkAllocateMemory' (deviceHandle (device)) (forgetExtensions pAllocateInfo) pAllocator (pPMemory)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pMemory <- lift $ peek @DeviceMemory pPMemory
  pure $ (pMemory)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'allocateMemory' and 'freeMemory'
--
-- To ensure that 'freeMemory' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withMemory :: forall a io r . (Extendss MemoryAllocateInfo a, PokeChain a, MonadIO io) => Device -> MemoryAllocateInfo a -> Maybe AllocationCallbacks -> (io DeviceMemory -> (DeviceMemory -> io ()) -> r) -> r
withMemory device pAllocateInfo pAllocator b =
  b (allocateMemory device pAllocateInfo pAllocator)
    (\(o0) -> freeMemory device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkFreeMemory
  :: FunPtr (Ptr Device_T -> DeviceMemory -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> DeviceMemory -> Ptr AllocationCallbacks -> IO ()

-- No documentation found for TopLevel "vkFreeMemory"
freeMemory :: forall io
            . (MonadIO io)
           => -- No documentation found for Nested "vkFreeMemory" "device"
              Device
           -> -- No documentation found for Nested "vkFreeMemory" "memory"
              DeviceMemory
           -> -- No documentation found for Nested "vkFreeMemory" "pAllocator"
              ("allocator" ::: Maybe AllocationCallbacks)
           -> io ()
freeMemory device memory allocator = liftIO . evalContT $ do
  let vkFreeMemoryPtr = pVkFreeMemory (deviceCmds (device :: Device))
  lift $ unless (vkFreeMemoryPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkFreeMemory is null" Nothing Nothing
  let vkFreeMemory' = mkVkFreeMemory vkFreeMemoryPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkFreeMemory' (deviceHandle (device)) (memory) pAllocator
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkMapMemory
  :: FunPtr (Ptr Device_T -> DeviceMemory -> DeviceSize -> DeviceSize -> MemoryMapFlags -> Ptr (Ptr ()) -> IO Result) -> Ptr Device_T -> DeviceMemory -> DeviceSize -> DeviceSize -> MemoryMapFlags -> Ptr (Ptr ()) -> IO Result

-- No documentation found for TopLevel "vkMapMemory"
mapMemory :: forall io
           . (MonadIO io)
          => -- No documentation found for Nested "vkMapMemory" "device"
             Device
          -> -- No documentation found for Nested "vkMapMemory" "memory"
             DeviceMemory
          -> -- No documentation found for Nested "vkMapMemory" "offset"
             ("offset" ::: DeviceSize)
          -> -- No documentation found for Nested "vkMapMemory" "size"
             DeviceSize
          -> -- No documentation found for Nested "vkMapMemory" "flags"
             MemoryMapFlags
          -> io (("data" ::: Ptr ()))
mapMemory device memory offset size flags = liftIO . evalContT $ do
  let vkMapMemoryPtr = pVkMapMemory (deviceCmds (device :: Device))
  lift $ unless (vkMapMemoryPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkMapMemory is null" Nothing Nothing
  let vkMapMemory' = mkVkMapMemory vkMapMemoryPtr
  pPpData <- ContT $ bracket (callocBytes @(Ptr ()) 8) free
  r <- lift $ vkMapMemory' (deviceHandle (device)) (memory) (offset) (size) (flags) (pPpData)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  ppData <- lift $ peek @(Ptr ()) pPpData
  pure $ (ppData)

-- | A convenience wrapper to make a compatible pair of calls to 'mapMemory'
-- and 'unmapMemory'
--
-- To ensure that 'unmapMemory' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withMappedMemory :: forall io r . MonadIO io => Device -> DeviceMemory -> DeviceSize -> DeviceSize -> MemoryMapFlags -> (io (Ptr ()) -> (Ptr () -> io ()) -> r) -> r
withMappedMemory device memory offset size flags b =
  b (mapMemory device memory offset size flags)
    (\(_) -> unmapMemory device memory)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkUnmapMemory
  :: FunPtr (Ptr Device_T -> DeviceMemory -> IO ()) -> Ptr Device_T -> DeviceMemory -> IO ()

-- No documentation found for TopLevel "vkUnmapMemory"
unmapMemory :: forall io
             . (MonadIO io)
            => -- No documentation found for Nested "vkUnmapMemory" "device"
               Device
            -> -- No documentation found for Nested "vkUnmapMemory" "memory"
               DeviceMemory
            -> io ()
unmapMemory device memory = liftIO $ do
  let vkUnmapMemoryPtr = pVkUnmapMemory (deviceCmds (device :: Device))
  unless (vkUnmapMemoryPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkUnmapMemory is null" Nothing Nothing
  let vkUnmapMemory' = mkVkUnmapMemory vkUnmapMemoryPtr
  vkUnmapMemory' (deviceHandle (device)) (memory)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkFlushMappedMemoryRanges
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr MappedMemoryRange -> IO Result) -> Ptr Device_T -> Word32 -> Ptr MappedMemoryRange -> IO Result

-- No documentation found for TopLevel "vkFlushMappedMemoryRanges"
flushMappedMemoryRanges :: forall io
                         . (MonadIO io)
                        => -- No documentation found for Nested "vkFlushMappedMemoryRanges" "device"
                           Device
                        -> -- No documentation found for Nested "vkFlushMappedMemoryRanges" "pMemoryRanges"
                           ("memoryRanges" ::: Vector MappedMemoryRange)
                        -> io ()
flushMappedMemoryRanges device memoryRanges = liftIO . evalContT $ do
  let vkFlushMappedMemoryRangesPtr = pVkFlushMappedMemoryRanges (deviceCmds (device :: Device))
  lift $ unless (vkFlushMappedMemoryRangesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkFlushMappedMemoryRanges is null" Nothing Nothing
  let vkFlushMappedMemoryRanges' = mkVkFlushMappedMemoryRanges vkFlushMappedMemoryRangesPtr
  pPMemoryRanges <- ContT $ allocaBytesAligned @MappedMemoryRange ((Data.Vector.length (memoryRanges)) * 40) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPMemoryRanges `plusPtr` (40 * (i)) :: Ptr MappedMemoryRange) (e)) (memoryRanges)
  r <- lift $ vkFlushMappedMemoryRanges' (deviceHandle (device)) ((fromIntegral (Data.Vector.length $ (memoryRanges)) :: Word32)) (pPMemoryRanges)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkInvalidateMappedMemoryRanges
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr MappedMemoryRange -> IO Result) -> Ptr Device_T -> Word32 -> Ptr MappedMemoryRange -> IO Result

-- No documentation found for TopLevel "vkInvalidateMappedMemoryRanges"
invalidateMappedMemoryRanges :: forall io
                              . (MonadIO io)
                             => -- No documentation found for Nested "vkInvalidateMappedMemoryRanges" "device"
                                Device
                             -> -- No documentation found for Nested "vkInvalidateMappedMemoryRanges" "pMemoryRanges"
                                ("memoryRanges" ::: Vector MappedMemoryRange)
                             -> io ()
invalidateMappedMemoryRanges device memoryRanges = liftIO . evalContT $ do
  let vkInvalidateMappedMemoryRangesPtr = pVkInvalidateMappedMemoryRanges (deviceCmds (device :: Device))
  lift $ unless (vkInvalidateMappedMemoryRangesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkInvalidateMappedMemoryRanges is null" Nothing Nothing
  let vkInvalidateMappedMemoryRanges' = mkVkInvalidateMappedMemoryRanges vkInvalidateMappedMemoryRangesPtr
  pPMemoryRanges <- ContT $ allocaBytesAligned @MappedMemoryRange ((Data.Vector.length (memoryRanges)) * 40) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPMemoryRanges `plusPtr` (40 * (i)) :: Ptr MappedMemoryRange) (e)) (memoryRanges)
  r <- lift $ vkInvalidateMappedMemoryRanges' (deviceHandle (device)) ((fromIntegral (Data.Vector.length $ (memoryRanges)) :: Word32)) (pPMemoryRanges)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceMemoryCommitment
  :: FunPtr (Ptr Device_T -> DeviceMemory -> Ptr DeviceSize -> IO ()) -> Ptr Device_T -> DeviceMemory -> Ptr DeviceSize -> IO ()

-- No documentation found for TopLevel "vkGetDeviceMemoryCommitment"
getDeviceMemoryCommitment :: forall io
                           . (MonadIO io)
                          => -- No documentation found for Nested "vkGetDeviceMemoryCommitment" "device"
                             Device
                          -> -- No documentation found for Nested "vkGetDeviceMemoryCommitment" "memory"
                             DeviceMemory
                          -> io (("committedMemoryInBytes" ::: DeviceSize))
getDeviceMemoryCommitment device memory = liftIO . evalContT $ do
  let vkGetDeviceMemoryCommitmentPtr = pVkGetDeviceMemoryCommitment (deviceCmds (device :: Device))
  lift $ unless (vkGetDeviceMemoryCommitmentPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDeviceMemoryCommitment is null" Nothing Nothing
  let vkGetDeviceMemoryCommitment' = mkVkGetDeviceMemoryCommitment vkGetDeviceMemoryCommitmentPtr
  pPCommittedMemoryInBytes <- ContT $ bracket (callocBytes @DeviceSize 8) free
  lift $ vkGetDeviceMemoryCommitment' (deviceHandle (device)) (memory) (pPCommittedMemoryInBytes)
  pCommittedMemoryInBytes <- lift $ peek @DeviceSize pPCommittedMemoryInBytes
  pure $ (pCommittedMemoryInBytes)



-- No documentation found for TopLevel "VkMemoryAllocateInfo"
data MemoryAllocateInfo (es :: [Type]) = MemoryAllocateInfo
  { -- No documentation found for Nested "VkMemoryAllocateInfo" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkMemoryAllocateInfo" "allocationSize"
    allocationSize :: DeviceSize
  , -- No documentation found for Nested "VkMemoryAllocateInfo" "memoryTypeIndex"
    memoryTypeIndex :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MemoryAllocateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (MemoryAllocateInfo es)

instance Extensible MemoryAllocateInfo where
  extensibleType = STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
  setNext x next = x{next = next}
  getNext MemoryAllocateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends MemoryAllocateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @MemoryOpaqueCaptureAddressAllocateInfo = Just f
    | Just Refl <- eqT @e @MemoryPriorityAllocateInfoEXT = Just f
    | Just Refl <- eqT @e @ImportAndroidHardwareBufferInfoANDROID = Just f
    | Just Refl <- eqT @e @ImportMemoryHostPointerInfoEXT = Just f
    | Just Refl <- eqT @e @MemoryDedicatedAllocateInfo = Just f
    | Just Refl <- eqT @e @MemoryAllocateFlagsInfo = Just f
    | Just Refl <- eqT @e @ImportMemoryFdInfoKHR = Just f
    | Just Refl <- eqT @e @ExportMemoryWin32HandleInfoKHR = Just f
    | Just Refl <- eqT @e @ImportMemoryWin32HandleInfoKHR = Just f
    | Just Refl <- eqT @e @ExportMemoryAllocateInfo = Just f
    | Just Refl <- eqT @e @ExportMemoryWin32HandleInfoNV = Just f
    | Just Refl <- eqT @e @ImportMemoryWin32HandleInfoNV = Just f
    | Just Refl <- eqT @e @ExportMemoryAllocateInfoNV = Just f
    | Just Refl <- eqT @e @DedicatedAllocationMemoryAllocateInfoNV = Just f
    | otherwise = Nothing

instance (Extendss MemoryAllocateInfo es, PokeChain es) => ToCStruct (MemoryAllocateInfo es) where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryAllocateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (allocationSize)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (memoryTypeIndex)
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    lift $ f

instance (Extendss MemoryAllocateInfo es, PeekChain es) => FromCStruct (MemoryAllocateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    allocationSize <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    memoryTypeIndex <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ MemoryAllocateInfo
             next allocationSize memoryTypeIndex

instance es ~ '[] => Zero (MemoryAllocateInfo es) where
  zero = MemoryAllocateInfo
           ()
           zero
           zero



-- No documentation found for TopLevel "VkMappedMemoryRange"
data MappedMemoryRange = MappedMemoryRange
  { -- No documentation found for Nested "VkMappedMemoryRange" "memory"
    memory :: DeviceMemory
  , -- No documentation found for Nested "VkMappedMemoryRange" "offset"
    offset :: DeviceSize
  , -- No documentation found for Nested "VkMappedMemoryRange" "size"
    size :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MappedMemoryRange)
#endif
deriving instance Show MappedMemoryRange

instance ToCStruct MappedMemoryRange where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MappedMemoryRange{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MAPPED_MEMORY_RANGE)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceMemory)) (memory)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (offset)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (size)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MAPPED_MEMORY_RANGE)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceMemory)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct MappedMemoryRange where
  peekCStruct p = do
    memory <- peek @DeviceMemory ((p `plusPtr` 16 :: Ptr DeviceMemory))
    offset <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    size <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    pure $ MappedMemoryRange
             memory offset size


instance Storable MappedMemoryRange where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MappedMemoryRange where
  zero = MappedMemoryRange
           zero
           zero
           zero

