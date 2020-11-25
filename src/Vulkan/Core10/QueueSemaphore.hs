{-# language CPP #-}
-- No documentation found for Chapter "QueueSemaphore"
module Vulkan.Core10.QueueSemaphore  ( createSemaphore
                                     , withSemaphore
                                     , destroySemaphore
                                     , SemaphoreCreateInfo(..)
                                     , Semaphore(..)
                                     , SemaphoreCreateFlags(..)
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
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkCreateSemaphore))
import Vulkan.Dynamic (DeviceCmds(pVkDestroySemaphore))
import Vulkan.Core10.Handles (Device_T)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore (ExportSemaphoreCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_semaphore_win32 (ExportSemaphoreWin32HandleInfoKHR)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Handles (Semaphore)
import Vulkan.Core10.Handles (Semaphore(..))
import Vulkan.Core10.Enums.SemaphoreCreateFlags (SemaphoreCreateFlags)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (SemaphoreTypeCreateInfo)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Handles (Semaphore(..))
import Vulkan.Core10.Enums.SemaphoreCreateFlags (SemaphoreCreateFlags(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateSemaphore
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct SemaphoreCreateInfo) -> Ptr AllocationCallbacks -> Ptr Semaphore -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct SemaphoreCreateInfo) -> Ptr AllocationCallbacks -> Ptr Semaphore -> IO Result

-- No documentation found for TopLevel "vkCreateSemaphore"
createSemaphore :: forall a io
                 . (Extendss SemaphoreCreateInfo a, PokeChain a, MonadIO io)
                => -- No documentation found for Nested "vkCreateSemaphore" "device"
                   Device
                -> -- No documentation found for Nested "vkCreateSemaphore" "pCreateInfo"
                   (SemaphoreCreateInfo a)
                -> -- No documentation found for Nested "vkCreateSemaphore" "pAllocator"
                   ("allocator" ::: Maybe AllocationCallbacks)
                -> io (Semaphore)
createSemaphore device createInfo allocator = liftIO . evalContT $ do
  let vkCreateSemaphorePtr = pVkCreateSemaphore (deviceCmds (device :: Device))
  lift $ unless (vkCreateSemaphorePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateSemaphore is null" Nothing Nothing
  let vkCreateSemaphore' = mkVkCreateSemaphore vkCreateSemaphorePtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSemaphore <- ContT $ bracket (callocBytes @Semaphore 8) free
  r <- lift $ vkCreateSemaphore' (deviceHandle (device)) (forgetExtensions pCreateInfo) pAllocator (pPSemaphore)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSemaphore <- lift $ peek @Semaphore pPSemaphore
  pure $ (pSemaphore)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createSemaphore' and 'destroySemaphore'
--
-- To ensure that 'destroySemaphore' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withSemaphore :: forall a io r . (Extendss SemaphoreCreateInfo a, PokeChain a, MonadIO io) => Device -> SemaphoreCreateInfo a -> Maybe AllocationCallbacks -> (io Semaphore -> (Semaphore -> io ()) -> r) -> r
withSemaphore device pCreateInfo pAllocator b =
  b (createSemaphore device pCreateInfo pAllocator)
    (\(o0) -> destroySemaphore device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroySemaphore
  :: FunPtr (Ptr Device_T -> Semaphore -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> Semaphore -> Ptr AllocationCallbacks -> IO ()

-- No documentation found for TopLevel "vkDestroySemaphore"
destroySemaphore :: forall io
                  . (MonadIO io)
                 => -- No documentation found for Nested "vkDestroySemaphore" "device"
                    Device
                 -> -- No documentation found for Nested "vkDestroySemaphore" "semaphore"
                    Semaphore
                 -> -- No documentation found for Nested "vkDestroySemaphore" "pAllocator"
                    ("allocator" ::: Maybe AllocationCallbacks)
                 -> io ()
destroySemaphore device semaphore allocator = liftIO . evalContT $ do
  let vkDestroySemaphorePtr = pVkDestroySemaphore (deviceCmds (device :: Device))
  lift $ unless (vkDestroySemaphorePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroySemaphore is null" Nothing Nothing
  let vkDestroySemaphore' = mkVkDestroySemaphore vkDestroySemaphorePtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroySemaphore' (deviceHandle (device)) (semaphore) pAllocator
  pure $ ()



-- No documentation found for TopLevel "VkSemaphoreCreateInfo"
data SemaphoreCreateInfo (es :: [Type]) = SemaphoreCreateInfo
  { -- No documentation found for Nested "VkSemaphoreCreateInfo" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkSemaphoreCreateInfo" "flags"
    flags :: SemaphoreCreateFlags
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SemaphoreCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (SemaphoreCreateInfo es)

instance Extensible SemaphoreCreateInfo where
  extensibleType = STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO
  setNext x next = x{next = next}
  getNext SemaphoreCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends SemaphoreCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @SemaphoreTypeCreateInfo = Just f
    | Just Refl <- eqT @e @ExportSemaphoreWin32HandleInfoKHR = Just f
    | Just Refl <- eqT @e @ExportSemaphoreCreateInfo = Just f
    | otherwise = Nothing

instance (Extendss SemaphoreCreateInfo es, PokeChain es) => ToCStruct (SemaphoreCreateInfo es) where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SemaphoreCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr SemaphoreCreateFlags)) (flags)
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ f

instance (Extendss SemaphoreCreateInfo es, PeekChain es) => FromCStruct (SemaphoreCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @SemaphoreCreateFlags ((p `plusPtr` 16 :: Ptr SemaphoreCreateFlags))
    pure $ SemaphoreCreateInfo
             next flags

instance es ~ '[] => Zero (SemaphoreCreateInfo es) where
  zero = SemaphoreCreateInfo
           ()
           zero

