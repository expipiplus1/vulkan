{-# language CPP #-}
-- No documentation found for Chapter "Buffer"
module Vulkan.Core10.Buffer  ( createBuffer
                             , withBuffer
                             , destroyBuffer
                             , BufferCreateInfo(..)
                             , Buffer(..)
                             , SharingMode(..)
                             , BufferUsageFlagBits(..)
                             , BufferUsageFlags
                             , BufferCreateFlagBits(..)
                             , BufferCreateFlags
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
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Control.Monad.IO.Class (MonadIO)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.Handles (Buffer)
import Vulkan.Core10.Handles (Buffer(..))
import Vulkan.Core10.Enums.BufferCreateFlagBits (BufferCreateFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_buffer_device_address (BufferDeviceAddressCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address (BufferOpaqueCaptureAddressCreateInfo)
import Vulkan.Core10.Enums.BufferUsageFlagBits (BufferUsageFlags)
import Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_dedicated_allocation (DedicatedAllocationBufferCreateInfoNV)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkCreateBuffer))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyBuffer))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_memory (ExternalMemoryBufferCreateInfo)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.SharingMode (SharingMode)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_CREATE_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Handles (Buffer(..))
import Vulkan.Core10.Enums.BufferCreateFlagBits (BufferCreateFlagBits(..))
import Vulkan.Core10.Enums.BufferCreateFlagBits (BufferCreateFlags)
import Vulkan.Core10.Enums.BufferUsageFlagBits (BufferUsageFlagBits(..))
import Vulkan.Core10.Enums.BufferUsageFlagBits (BufferUsageFlags)
import Vulkan.Core10.Enums.SharingMode (SharingMode(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateBuffer
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct BufferCreateInfo) -> Ptr AllocationCallbacks -> Ptr Buffer -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct BufferCreateInfo) -> Ptr AllocationCallbacks -> Ptr Buffer -> IO Result

-- No documentation found for TopLevel "vkCreateBuffer"
createBuffer :: forall a io
              . (Extendss BufferCreateInfo a, PokeChain a, MonadIO io)
             => -- No documentation found for Nested "vkCreateBuffer" "device"
                Device
             -> -- No documentation found for Nested "vkCreateBuffer" "pCreateInfo"
                (BufferCreateInfo a)
             -> -- No documentation found for Nested "vkCreateBuffer" "pAllocator"
                ("allocator" ::: Maybe AllocationCallbacks)
             -> io (Buffer)
createBuffer device createInfo allocator = liftIO . evalContT $ do
  let vkCreateBufferPtr = pVkCreateBuffer (deviceCmds (device :: Device))
  lift $ unless (vkCreateBufferPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateBuffer is null" Nothing Nothing
  let vkCreateBuffer' = mkVkCreateBuffer vkCreateBufferPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPBuffer <- ContT $ bracket (callocBytes @Buffer 8) free
  r <- lift $ vkCreateBuffer' (deviceHandle (device)) (forgetExtensions pCreateInfo) pAllocator (pPBuffer)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pBuffer <- lift $ peek @Buffer pPBuffer
  pure $ (pBuffer)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createBuffer' and 'destroyBuffer'
--
-- To ensure that 'destroyBuffer' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withBuffer :: forall a io r . (Extendss BufferCreateInfo a, PokeChain a, MonadIO io) => Device -> BufferCreateInfo a -> Maybe AllocationCallbacks -> (io Buffer -> (Buffer -> io ()) -> r) -> r
withBuffer device pCreateInfo pAllocator b =
  b (createBuffer device pCreateInfo pAllocator)
    (\(o0) -> destroyBuffer device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyBuffer
  :: FunPtr (Ptr Device_T -> Buffer -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> Buffer -> Ptr AllocationCallbacks -> IO ()

-- No documentation found for TopLevel "vkDestroyBuffer"
destroyBuffer :: forall io
               . (MonadIO io)
              => -- No documentation found for Nested "vkDestroyBuffer" "device"
                 Device
              -> -- No documentation found for Nested "vkDestroyBuffer" "buffer"
                 Buffer
              -> -- No documentation found for Nested "vkDestroyBuffer" "pAllocator"
                 ("allocator" ::: Maybe AllocationCallbacks)
              -> io ()
destroyBuffer device buffer allocator = liftIO . evalContT $ do
  let vkDestroyBufferPtr = pVkDestroyBuffer (deviceCmds (device :: Device))
  lift $ unless (vkDestroyBufferPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyBuffer is null" Nothing Nothing
  let vkDestroyBuffer' = mkVkDestroyBuffer vkDestroyBufferPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroyBuffer' (deviceHandle (device)) (buffer) pAllocator
  pure $ ()



-- No documentation found for TopLevel "VkBufferCreateInfo"
data BufferCreateInfo (es :: [Type]) = BufferCreateInfo
  { -- No documentation found for Nested "VkBufferCreateInfo" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkBufferCreateInfo" "flags"
    flags :: BufferCreateFlags
  , -- No documentation found for Nested "VkBufferCreateInfo" "size"
    size :: DeviceSize
  , -- No documentation found for Nested "VkBufferCreateInfo" "usage"
    usage :: BufferUsageFlags
  , -- No documentation found for Nested "VkBufferCreateInfo" "sharingMode"
    sharingMode :: SharingMode
  , -- No documentation found for Nested "VkBufferCreateInfo" "pQueueFamilyIndices"
    queueFamilyIndices :: Vector Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BufferCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (BufferCreateInfo es)

instance Extensible BufferCreateInfo where
  extensibleType = STRUCTURE_TYPE_BUFFER_CREATE_INFO
  setNext x next = x{next = next}
  getNext BufferCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends BufferCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @BufferDeviceAddressCreateInfoEXT = Just f
    | Just Refl <- eqT @e @BufferOpaqueCaptureAddressCreateInfo = Just f
    | Just Refl <- eqT @e @ExternalMemoryBufferCreateInfo = Just f
    | Just Refl <- eqT @e @DedicatedAllocationBufferCreateInfoNV = Just f
    | otherwise = Nothing

instance (Extendss BufferCreateInfo es, PokeChain es) => ToCStruct (BufferCreateInfo es) where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BufferCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr BufferCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (size)
    lift $ poke ((p `plusPtr` 32 :: Ptr BufferUsageFlags)) (usage)
    lift $ poke ((p `plusPtr` 36 :: Ptr SharingMode)) (sharingMode)
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (queueFamilyIndices)) :: Word32))
    pPQueueFamilyIndices' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (queueFamilyIndices)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPQueueFamilyIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (queueFamilyIndices)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr Word32))) (pPQueueFamilyIndices')
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr BufferUsageFlags)) (zero)
    lift $ poke ((p `plusPtr` 36 :: Ptr SharingMode)) (zero)
    pPQueueFamilyIndices' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPQueueFamilyIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (mempty)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr Word32))) (pPQueueFamilyIndices')
    lift $ f

instance (Extendss BufferCreateInfo es, PeekChain es) => FromCStruct (BufferCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @BufferCreateFlags ((p `plusPtr` 16 :: Ptr BufferCreateFlags))
    size <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    usage <- peek @BufferUsageFlags ((p `plusPtr` 32 :: Ptr BufferUsageFlags))
    sharingMode <- peek @SharingMode ((p `plusPtr` 36 :: Ptr SharingMode))
    queueFamilyIndexCount <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    pQueueFamilyIndices <- peek @(Ptr Word32) ((p `plusPtr` 48 :: Ptr (Ptr Word32)))
    pQueueFamilyIndices' <- generateM (fromIntegral queueFamilyIndexCount) (\i -> peek @Word32 ((pQueueFamilyIndices `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pure $ BufferCreateInfo
             next flags size usage sharingMode pQueueFamilyIndices'

instance es ~ '[] => Zero (BufferCreateInfo es) where
  zero = BufferCreateInfo
           ()
           zero
           zero
           zero
           zero
           mempty

