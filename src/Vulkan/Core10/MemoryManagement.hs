{-# language CPP #-}
-- No documentation found for Chapter "MemoryManagement"
module Vulkan.Core10.MemoryManagement  ( getBufferMemoryRequirements
                                       , bindBufferMemory
                                       , getImageMemoryRequirements
                                       , bindImageMemory
                                       , MemoryRequirements(..)
                                       , DeviceMemory(..)
                                       ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.IO.Class (MonadIO)
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
import Vulkan.Core10.Handles (Buffer)
import Vulkan.Core10.Handles (Buffer(..))
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkBindBufferMemory))
import Vulkan.Dynamic (DeviceCmds(pVkBindImageMemory))
import Vulkan.Dynamic (DeviceCmds(pVkGetBufferMemoryRequirements))
import Vulkan.Dynamic (DeviceCmds(pVkGetImageMemoryRequirements))
import Vulkan.Core10.Handles (DeviceMemory)
import Vulkan.Core10.Handles (DeviceMemory(..))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Handles (Image)
import Vulkan.Core10.Handles (Image(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Handles (DeviceMemory(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetBufferMemoryRequirements
  :: FunPtr (Ptr Device_T -> Buffer -> Ptr MemoryRequirements -> IO ()) -> Ptr Device_T -> Buffer -> Ptr MemoryRequirements -> IO ()

-- No documentation found for TopLevel "vkGetBufferMemoryRequirements"
getBufferMemoryRequirements :: forall io
                             . (MonadIO io)
                            => -- No documentation found for Nested "vkGetBufferMemoryRequirements" "device"
                               Device
                            -> -- No documentation found for Nested "vkGetBufferMemoryRequirements" "buffer"
                               Buffer
                            -> io (MemoryRequirements)
getBufferMemoryRequirements device buffer = liftIO . evalContT $ do
  let vkGetBufferMemoryRequirementsPtr = pVkGetBufferMemoryRequirements (deviceCmds (device :: Device))
  lift $ unless (vkGetBufferMemoryRequirementsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetBufferMemoryRequirements is null" Nothing Nothing
  let vkGetBufferMemoryRequirements' = mkVkGetBufferMemoryRequirements vkGetBufferMemoryRequirementsPtr
  pPMemoryRequirements <- ContT (withZeroCStruct @MemoryRequirements)
  lift $ vkGetBufferMemoryRequirements' (deviceHandle (device)) (buffer) (pPMemoryRequirements)
  pMemoryRequirements <- lift $ peekCStruct @MemoryRequirements pPMemoryRequirements
  pure $ (pMemoryRequirements)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkBindBufferMemory
  :: FunPtr (Ptr Device_T -> Buffer -> DeviceMemory -> DeviceSize -> IO Result) -> Ptr Device_T -> Buffer -> DeviceMemory -> DeviceSize -> IO Result

-- No documentation found for TopLevel "vkBindBufferMemory"
bindBufferMemory :: forall io
                  . (MonadIO io)
                 => -- No documentation found for Nested "vkBindBufferMemory" "device"
                    Device
                 -> -- No documentation found for Nested "vkBindBufferMemory" "buffer"
                    Buffer
                 -> -- No documentation found for Nested "vkBindBufferMemory" "memory"
                    DeviceMemory
                 -> -- No documentation found for Nested "vkBindBufferMemory" "memoryOffset"
                    ("memoryOffset" ::: DeviceSize)
                 -> io ()
bindBufferMemory device buffer memory memoryOffset = liftIO $ do
  let vkBindBufferMemoryPtr = pVkBindBufferMemory (deviceCmds (device :: Device))
  unless (vkBindBufferMemoryPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkBindBufferMemory is null" Nothing Nothing
  let vkBindBufferMemory' = mkVkBindBufferMemory vkBindBufferMemoryPtr
  r <- vkBindBufferMemory' (deviceHandle (device)) (buffer) (memory) (memoryOffset)
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetImageMemoryRequirements
  :: FunPtr (Ptr Device_T -> Image -> Ptr MemoryRequirements -> IO ()) -> Ptr Device_T -> Image -> Ptr MemoryRequirements -> IO ()

-- No documentation found for TopLevel "vkGetImageMemoryRequirements"
getImageMemoryRequirements :: forall io
                            . (MonadIO io)
                           => -- No documentation found for Nested "vkGetImageMemoryRequirements" "device"
                              Device
                           -> -- No documentation found for Nested "vkGetImageMemoryRequirements" "image"
                              Image
                           -> io (MemoryRequirements)
getImageMemoryRequirements device image = liftIO . evalContT $ do
  let vkGetImageMemoryRequirementsPtr = pVkGetImageMemoryRequirements (deviceCmds (device :: Device))
  lift $ unless (vkGetImageMemoryRequirementsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetImageMemoryRequirements is null" Nothing Nothing
  let vkGetImageMemoryRequirements' = mkVkGetImageMemoryRequirements vkGetImageMemoryRequirementsPtr
  pPMemoryRequirements <- ContT (withZeroCStruct @MemoryRequirements)
  lift $ vkGetImageMemoryRequirements' (deviceHandle (device)) (image) (pPMemoryRequirements)
  pMemoryRequirements <- lift $ peekCStruct @MemoryRequirements pPMemoryRequirements
  pure $ (pMemoryRequirements)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkBindImageMemory
  :: FunPtr (Ptr Device_T -> Image -> DeviceMemory -> DeviceSize -> IO Result) -> Ptr Device_T -> Image -> DeviceMemory -> DeviceSize -> IO Result

-- No documentation found for TopLevel "vkBindImageMemory"
bindImageMemory :: forall io
                 . (MonadIO io)
                => -- No documentation found for Nested "vkBindImageMemory" "device"
                   Device
                -> -- No documentation found for Nested "vkBindImageMemory" "image"
                   Image
                -> -- No documentation found for Nested "vkBindImageMemory" "memory"
                   DeviceMemory
                -> -- No documentation found for Nested "vkBindImageMemory" "memoryOffset"
                   ("memoryOffset" ::: DeviceSize)
                -> io ()
bindImageMemory device image memory memoryOffset = liftIO $ do
  let vkBindImageMemoryPtr = pVkBindImageMemory (deviceCmds (device :: Device))
  unless (vkBindImageMemoryPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkBindImageMemory is null" Nothing Nothing
  let vkBindImageMemory' = mkVkBindImageMemory vkBindImageMemoryPtr
  r <- vkBindImageMemory' (deviceHandle (device)) (image) (memory) (memoryOffset)
  when (r < SUCCESS) (throwIO (VulkanException r))



-- No documentation found for TopLevel "VkMemoryRequirements"
data MemoryRequirements = MemoryRequirements
  { -- No documentation found for Nested "VkMemoryRequirements" "size"
    size :: DeviceSize
  , -- No documentation found for Nested "VkMemoryRequirements" "alignment"
    alignment :: DeviceSize
  , -- No documentation found for Nested "VkMemoryRequirements" "memoryTypeBits"
    memoryTypeBits :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MemoryRequirements)
#endif
deriving instance Show MemoryRequirements

instance ToCStruct MemoryRequirements where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryRequirements{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceSize)) (size)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (alignment)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (memoryTypeBits)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct MemoryRequirements where
  peekCStruct p = do
    size <- peek @DeviceSize ((p `plusPtr` 0 :: Ptr DeviceSize))
    alignment <- peek @DeviceSize ((p `plusPtr` 8 :: Ptr DeviceSize))
    memoryTypeBits <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ MemoryRequirements
             size alignment memoryTypeBits


instance Storable MemoryRequirements where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MemoryRequirements where
  zero = MemoryRequirements
           zero
           zero
           zero

