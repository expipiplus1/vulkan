{-# language CPP #-}
-- No documentation found for Chapter "CommandPool"
module Vulkan.Core10.CommandPool  ( createCommandPool
                                  , withCommandPool
                                  , destroyCommandPool
                                  , resetCommandPool
                                  , CommandPoolCreateInfo(..)
                                  , CommandPool(..)
                                  , CommandPoolCreateFlagBits(..)
                                  , CommandPoolCreateFlags
                                  , CommandPoolResetFlagBits(..)
                                  , CommandPoolResetFlags
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
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.Handles (CommandPool)
import Vulkan.Core10.Handles (CommandPool(..))
import Vulkan.Core10.Enums.CommandPoolCreateFlagBits (CommandPoolCreateFlags)
import Vulkan.Core10.Enums.CommandPoolResetFlagBits (CommandPoolResetFlagBits(..))
import Vulkan.Core10.Enums.CommandPoolResetFlagBits (CommandPoolResetFlags)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkCreateCommandPool))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyCommandPool))
import Vulkan.Dynamic (DeviceCmds(pVkResetCommandPool))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Handles (CommandPool(..))
import Vulkan.Core10.Enums.CommandPoolCreateFlagBits (CommandPoolCreateFlagBits(..))
import Vulkan.Core10.Enums.CommandPoolCreateFlagBits (CommandPoolCreateFlags)
import Vulkan.Core10.Enums.CommandPoolResetFlagBits (CommandPoolResetFlagBits(..))
import Vulkan.Core10.Enums.CommandPoolResetFlagBits (CommandPoolResetFlags)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateCommandPool
  :: FunPtr (Ptr Device_T -> Ptr CommandPoolCreateInfo -> Ptr AllocationCallbacks -> Ptr CommandPool -> IO Result) -> Ptr Device_T -> Ptr CommandPoolCreateInfo -> Ptr AllocationCallbacks -> Ptr CommandPool -> IO Result

-- No documentation found for TopLevel "vkCreateCommandPool"
createCommandPool :: forall io
                   . (MonadIO io)
                  => -- No documentation found for Nested "vkCreateCommandPool" "device"
                     Device
                  -> -- No documentation found for Nested "vkCreateCommandPool" "pCreateInfo"
                     CommandPoolCreateInfo
                  -> -- No documentation found for Nested "vkCreateCommandPool" "pAllocator"
                     ("allocator" ::: Maybe AllocationCallbacks)
                  -> io (CommandPool)
createCommandPool device createInfo allocator = liftIO . evalContT $ do
  let vkCreateCommandPoolPtr = pVkCreateCommandPool (deviceCmds (device :: Device))
  lift $ unless (vkCreateCommandPoolPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateCommandPool is null" Nothing Nothing
  let vkCreateCommandPool' = mkVkCreateCommandPool vkCreateCommandPoolPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPCommandPool <- ContT $ bracket (callocBytes @CommandPool 8) free
  r <- lift $ vkCreateCommandPool' (deviceHandle (device)) pCreateInfo pAllocator (pPCommandPool)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pCommandPool <- lift $ peek @CommandPool pPCommandPool
  pure $ (pCommandPool)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createCommandPool' and 'destroyCommandPool'
--
-- To ensure that 'destroyCommandPool' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withCommandPool :: forall io r . MonadIO io => Device -> CommandPoolCreateInfo -> Maybe AllocationCallbacks -> (io CommandPool -> (CommandPool -> io ()) -> r) -> r
withCommandPool device pCreateInfo pAllocator b =
  b (createCommandPool device pCreateInfo pAllocator)
    (\(o0) -> destroyCommandPool device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyCommandPool
  :: FunPtr (Ptr Device_T -> CommandPool -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> CommandPool -> Ptr AllocationCallbacks -> IO ()

-- No documentation found for TopLevel "vkDestroyCommandPool"
destroyCommandPool :: forall io
                    . (MonadIO io)
                   => -- No documentation found for Nested "vkDestroyCommandPool" "device"
                      Device
                   -> -- No documentation found for Nested "vkDestroyCommandPool" "commandPool"
                      CommandPool
                   -> -- No documentation found for Nested "vkDestroyCommandPool" "pAllocator"
                      ("allocator" ::: Maybe AllocationCallbacks)
                   -> io ()
destroyCommandPool device commandPool allocator = liftIO . evalContT $ do
  let vkDestroyCommandPoolPtr = pVkDestroyCommandPool (deviceCmds (device :: Device))
  lift $ unless (vkDestroyCommandPoolPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyCommandPool is null" Nothing Nothing
  let vkDestroyCommandPool' = mkVkDestroyCommandPool vkDestroyCommandPoolPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroyCommandPool' (deviceHandle (device)) (commandPool) pAllocator
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkResetCommandPool
  :: FunPtr (Ptr Device_T -> CommandPool -> CommandPoolResetFlags -> IO Result) -> Ptr Device_T -> CommandPool -> CommandPoolResetFlags -> IO Result

-- No documentation found for TopLevel "vkResetCommandPool"
resetCommandPool :: forall io
                  . (MonadIO io)
                 => -- No documentation found for Nested "vkResetCommandPool" "device"
                    Device
                 -> -- No documentation found for Nested "vkResetCommandPool" "commandPool"
                    CommandPool
                 -> -- No documentation found for Nested "vkResetCommandPool" "flags"
                    CommandPoolResetFlags
                 -> io ()
resetCommandPool device commandPool flags = liftIO $ do
  let vkResetCommandPoolPtr = pVkResetCommandPool (deviceCmds (device :: Device))
  unless (vkResetCommandPoolPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkResetCommandPool is null" Nothing Nothing
  let vkResetCommandPool' = mkVkResetCommandPool vkResetCommandPoolPtr
  r <- vkResetCommandPool' (deviceHandle (device)) (commandPool) (flags)
  when (r < SUCCESS) (throwIO (VulkanException r))



-- No documentation found for TopLevel "VkCommandPoolCreateInfo"
data CommandPoolCreateInfo = CommandPoolCreateInfo
  { -- No documentation found for Nested "VkCommandPoolCreateInfo" "flags"
    flags :: CommandPoolCreateFlags
  , -- No documentation found for Nested "VkCommandPoolCreateInfo" "queueFamilyIndex"
    queueFamilyIndex :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CommandPoolCreateInfo)
#endif
deriving instance Show CommandPoolCreateInfo

instance ToCStruct CommandPoolCreateInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CommandPoolCreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CommandPoolCreateFlags)) (flags)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (queueFamilyIndex)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    f

instance FromCStruct CommandPoolCreateInfo where
  peekCStruct p = do
    flags <- peek @CommandPoolCreateFlags ((p `plusPtr` 16 :: Ptr CommandPoolCreateFlags))
    queueFamilyIndex <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pure $ CommandPoolCreateInfo
             flags queueFamilyIndex


instance Storable CommandPoolCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CommandPoolCreateInfo where
  zero = CommandPoolCreateInfo
           zero
           zero

