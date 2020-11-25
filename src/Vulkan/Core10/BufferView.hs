{-# language CPP #-}
-- No documentation found for Chapter "BufferView"
module Vulkan.Core10.BufferView  ( createBufferView
                                 , withBufferView
                                 , destroyBufferView
                                 , BufferViewCreateInfo(..)
                                 , BufferView(..)
                                 , BufferViewCreateFlags(..)
                                 ) where

import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
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
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.Handles (Buffer)
import Vulkan.Core10.Handles (BufferView)
import Vulkan.Core10.Handles (BufferView(..))
import Vulkan.Core10.Enums.BufferViewCreateFlags (BufferViewCreateFlags)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkCreateBufferView))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyBufferView))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Handles (BufferView(..))
import Vulkan.Core10.Enums.BufferViewCreateFlags (BufferViewCreateFlags(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateBufferView
  :: FunPtr (Ptr Device_T -> Ptr BufferViewCreateInfo -> Ptr AllocationCallbacks -> Ptr BufferView -> IO Result) -> Ptr Device_T -> Ptr BufferViewCreateInfo -> Ptr AllocationCallbacks -> Ptr BufferView -> IO Result

-- No documentation found for TopLevel "vkCreateBufferView"
createBufferView :: forall io
                  . (MonadIO io)
                 => -- No documentation found for Nested "vkCreateBufferView" "device"
                    Device
                 -> -- No documentation found for Nested "vkCreateBufferView" "pCreateInfo"
                    BufferViewCreateInfo
                 -> -- No documentation found for Nested "vkCreateBufferView" "pAllocator"
                    ("allocator" ::: Maybe AllocationCallbacks)
                 -> io (BufferView)
createBufferView device createInfo allocator = liftIO . evalContT $ do
  let vkCreateBufferViewPtr = pVkCreateBufferView (deviceCmds (device :: Device))
  lift $ unless (vkCreateBufferViewPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateBufferView is null" Nothing Nothing
  let vkCreateBufferView' = mkVkCreateBufferView vkCreateBufferViewPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPView <- ContT $ bracket (callocBytes @BufferView 8) free
  r <- lift $ vkCreateBufferView' (deviceHandle (device)) pCreateInfo pAllocator (pPView)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pView <- lift $ peek @BufferView pPView
  pure $ (pView)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createBufferView' and 'destroyBufferView'
--
-- To ensure that 'destroyBufferView' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withBufferView :: forall io r . MonadIO io => Device -> BufferViewCreateInfo -> Maybe AllocationCallbacks -> (io BufferView -> (BufferView -> io ()) -> r) -> r
withBufferView device pCreateInfo pAllocator b =
  b (createBufferView device pCreateInfo pAllocator)
    (\(o0) -> destroyBufferView device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyBufferView
  :: FunPtr (Ptr Device_T -> BufferView -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> BufferView -> Ptr AllocationCallbacks -> IO ()

-- No documentation found for TopLevel "vkDestroyBufferView"
destroyBufferView :: forall io
                   . (MonadIO io)
                  => -- No documentation found for Nested "vkDestroyBufferView" "device"
                     Device
                  -> -- No documentation found for Nested "vkDestroyBufferView" "bufferView"
                     BufferView
                  -> -- No documentation found for Nested "vkDestroyBufferView" "pAllocator"
                     ("allocator" ::: Maybe AllocationCallbacks)
                  -> io ()
destroyBufferView device bufferView allocator = liftIO . evalContT $ do
  let vkDestroyBufferViewPtr = pVkDestroyBufferView (deviceCmds (device :: Device))
  lift $ unless (vkDestroyBufferViewPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyBufferView is null" Nothing Nothing
  let vkDestroyBufferView' = mkVkDestroyBufferView vkDestroyBufferViewPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroyBufferView' (deviceHandle (device)) (bufferView) pAllocator
  pure $ ()



-- No documentation found for TopLevel "VkBufferViewCreateInfo"
data BufferViewCreateInfo = BufferViewCreateInfo
  { -- No documentation found for Nested "VkBufferViewCreateInfo" "flags"
    flags :: BufferViewCreateFlags
  , -- No documentation found for Nested "VkBufferViewCreateInfo" "buffer"
    buffer :: Buffer
  , -- No documentation found for Nested "VkBufferViewCreateInfo" "format"
    format :: Format
  , -- No documentation found for Nested "VkBufferViewCreateInfo" "offset"
    offset :: DeviceSize
  , -- No documentation found for Nested "VkBufferViewCreateInfo" "range"
    range :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BufferViewCreateInfo)
#endif
deriving instance Show BufferViewCreateInfo

instance ToCStruct BufferViewCreateInfo where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BufferViewCreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr BufferViewCreateFlags)) (flags)
    poke ((p `plusPtr` 24 :: Ptr Buffer)) (buffer)
    poke ((p `plusPtr` 32 :: Ptr Format)) (format)
    poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (offset)
    poke ((p `plusPtr` 48 :: Ptr DeviceSize)) (range)
    f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr Buffer)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Format)) (zero)
    poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 48 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct BufferViewCreateInfo where
  peekCStruct p = do
    flags <- peek @BufferViewCreateFlags ((p `plusPtr` 16 :: Ptr BufferViewCreateFlags))
    buffer <- peek @Buffer ((p `plusPtr` 24 :: Ptr Buffer))
    format <- peek @Format ((p `plusPtr` 32 :: Ptr Format))
    offset <- peek @DeviceSize ((p `plusPtr` 40 :: Ptr DeviceSize))
    range <- peek @DeviceSize ((p `plusPtr` 48 :: Ptr DeviceSize))
    pure $ BufferViewCreateInfo
             flags buffer format offset range


instance Storable BufferViewCreateInfo where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BufferViewCreateInfo where
  zero = BufferViewCreateInfo
           zero
           zero
           zero
           zero
           zero

