{-# language CPP #-}
-- No documentation found for Chapter "Fence"
module Vulkan.Core10.Fence  ( createFence
                            , withFence
                            , destroyFence
                            , resetFences
                            , getFenceStatus
                            , waitForFences
                            , waitForFencesSafe
                            , FenceCreateInfo(..)
                            , Fence(..)
                            , FenceCreateFlagBits(..)
                            , FenceCreateFlags
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
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (Bool32(..))
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkCreateFence))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyFence))
import Vulkan.Dynamic (DeviceCmds(pVkGetFenceStatus))
import Vulkan.Dynamic (DeviceCmds(pVkResetFences))
import Vulkan.Dynamic (DeviceCmds(pVkWaitForFences))
import Vulkan.Core10.Handles (Device_T)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_fence (ExportFenceCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_fence_win32 (ExportFenceWin32HandleInfoKHR)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.Handles (Fence)
import Vulkan.Core10.Handles (Fence(..))
import Vulkan.Core10.Enums.FenceCreateFlagBits (FenceCreateFlags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_FENCE_CREATE_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Handles (Fence(..))
import Vulkan.Core10.Enums.FenceCreateFlagBits (FenceCreateFlagBits(..))
import Vulkan.Core10.Enums.FenceCreateFlagBits (FenceCreateFlags)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateFence
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct FenceCreateInfo) -> Ptr AllocationCallbacks -> Ptr Fence -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct FenceCreateInfo) -> Ptr AllocationCallbacks -> Ptr Fence -> IO Result

-- No documentation found for TopLevel "vkCreateFence"
createFence :: forall a io
             . (Extendss FenceCreateInfo a, PokeChain a, MonadIO io)
            => -- No documentation found for Nested "vkCreateFence" "device"
               Device
            -> -- No documentation found for Nested "vkCreateFence" "pCreateInfo"
               (FenceCreateInfo a)
            -> -- No documentation found for Nested "vkCreateFence" "pAllocator"
               ("allocator" ::: Maybe AllocationCallbacks)
            -> io (Fence)
createFence device createInfo allocator = liftIO . evalContT $ do
  let vkCreateFencePtr = pVkCreateFence (deviceCmds (device :: Device))
  lift $ unless (vkCreateFencePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateFence is null" Nothing Nothing
  let vkCreateFence' = mkVkCreateFence vkCreateFencePtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPFence <- ContT $ bracket (callocBytes @Fence 8) free
  r <- lift $ vkCreateFence' (deviceHandle (device)) (forgetExtensions pCreateInfo) pAllocator (pPFence)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pFence <- lift $ peek @Fence pPFence
  pure $ (pFence)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createFence' and 'destroyFence'
--
-- To ensure that 'destroyFence' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withFence :: forall a io r . (Extendss FenceCreateInfo a, PokeChain a, MonadIO io) => Device -> FenceCreateInfo a -> Maybe AllocationCallbacks -> (io Fence -> (Fence -> io ()) -> r) -> r
withFence device pCreateInfo pAllocator b =
  b (createFence device pCreateInfo pAllocator)
    (\(o0) -> destroyFence device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyFence
  :: FunPtr (Ptr Device_T -> Fence -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> Fence -> Ptr AllocationCallbacks -> IO ()

-- No documentation found for TopLevel "vkDestroyFence"
destroyFence :: forall io
              . (MonadIO io)
             => -- No documentation found for Nested "vkDestroyFence" "device"
                Device
             -> -- No documentation found for Nested "vkDestroyFence" "fence"
                Fence
             -> -- No documentation found for Nested "vkDestroyFence" "pAllocator"
                ("allocator" ::: Maybe AllocationCallbacks)
             -> io ()
destroyFence device fence allocator = liftIO . evalContT $ do
  let vkDestroyFencePtr = pVkDestroyFence (deviceCmds (device :: Device))
  lift $ unless (vkDestroyFencePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyFence is null" Nothing Nothing
  let vkDestroyFence' = mkVkDestroyFence vkDestroyFencePtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroyFence' (deviceHandle (device)) (fence) pAllocator
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkResetFences
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr Fence -> IO Result) -> Ptr Device_T -> Word32 -> Ptr Fence -> IO Result

-- No documentation found for TopLevel "vkResetFences"
resetFences :: forall io
             . (MonadIO io)
            => -- No documentation found for Nested "vkResetFences" "device"
               Device
            -> -- No documentation found for Nested "vkResetFences" "pFences"
               ("fences" ::: Vector Fence)
            -> io ()
resetFences device fences = liftIO . evalContT $ do
  let vkResetFencesPtr = pVkResetFences (deviceCmds (device :: Device))
  lift $ unless (vkResetFencesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkResetFences is null" Nothing Nothing
  let vkResetFences' = mkVkResetFences vkResetFencesPtr
  pPFences <- ContT $ allocaBytesAligned @Fence ((Data.Vector.length (fences)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPFences `plusPtr` (8 * (i)) :: Ptr Fence) (e)) (fences)
  r <- lift $ vkResetFences' (deviceHandle (device)) ((fromIntegral (Data.Vector.length $ (fences)) :: Word32)) (pPFences)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetFenceStatus
  :: FunPtr (Ptr Device_T -> Fence -> IO Result) -> Ptr Device_T -> Fence -> IO Result

-- No documentation found for TopLevel "vkGetFenceStatus"
getFenceStatus :: forall io
                . (MonadIO io)
               => -- No documentation found for Nested "vkGetFenceStatus" "device"
                  Device
               -> -- No documentation found for Nested "vkGetFenceStatus" "fence"
                  Fence
               -> io (Result)
getFenceStatus device fence = liftIO $ do
  let vkGetFenceStatusPtr = pVkGetFenceStatus (deviceCmds (device :: Device))
  unless (vkGetFenceStatusPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetFenceStatus is null" Nothing Nothing
  let vkGetFenceStatus' = mkVkGetFenceStatus vkGetFenceStatusPtr
  r <- vkGetFenceStatus' (deviceHandle (device)) (fence)
  when (r < SUCCESS) (throwIO (VulkanException r))
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkWaitForFencesUnsafe
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr Fence -> Bool32 -> Word64 -> IO Result) -> Ptr Device_T -> Word32 -> Ptr Fence -> Bool32 -> Word64 -> IO Result

foreign import ccall
  "dynamic" mkVkWaitForFencesSafe
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr Fence -> Bool32 -> Word64 -> IO Result) -> Ptr Device_T -> Word32 -> Ptr Fence -> Bool32 -> Word64 -> IO Result

-- | waitForFences with selectable safeness
waitForFencesSafeOrUnsafe :: forall io
                           . (MonadIO io)
                          => -- No documentation found for TopLevel ""
                             (FunPtr (Ptr Device_T -> Word32 -> Ptr Fence -> Bool32 -> Word64 -> IO Result) -> Ptr Device_T -> Word32 -> Ptr Fence -> Bool32 -> Word64 -> IO Result)
                          -> -- No documentation found for Nested "vkWaitForFences" "device"
                             Device
                          -> -- No documentation found for Nested "vkWaitForFences" "pFences"
                             ("fences" ::: Vector Fence)
                          -> -- No documentation found for Nested "vkWaitForFences" "waitAll"
                             ("waitAll" ::: Bool)
                          -> -- No documentation found for Nested "vkWaitForFences" "timeout"
                             ("timeout" ::: Word64)
                          -> io (Result)
waitForFencesSafeOrUnsafe mkVkWaitForFences device fences waitAll timeout = liftIO . evalContT $ do
  let vkWaitForFencesPtr = pVkWaitForFences (deviceCmds (device :: Device))
  lift $ unless (vkWaitForFencesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkWaitForFences is null" Nothing Nothing
  let vkWaitForFences' = mkVkWaitForFences vkWaitForFencesPtr
  pPFences <- ContT $ allocaBytesAligned @Fence ((Data.Vector.length (fences)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPFences `plusPtr` (8 * (i)) :: Ptr Fence) (e)) (fences)
  r <- lift $ vkWaitForFences' (deviceHandle (device)) ((fromIntegral (Data.Vector.length $ (fences)) :: Word32)) (pPFences) (boolToBool32 (waitAll)) (timeout)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pure $ (r)

-- No documentation found for TopLevel "vkWaitForFences"
waitForFences :: forall io
               . (MonadIO io)
              => -- No documentation found for Nested "vkWaitForFences" "device"
                 Device
              -> -- No documentation found for Nested "vkWaitForFences" "pFences"
                 ("fences" ::: Vector Fence)
              -> -- No documentation found for Nested "vkWaitForFences" "waitAll"
                 ("waitAll" ::: Bool)
              -> -- No documentation found for Nested "vkWaitForFences" "timeout"
                 ("timeout" ::: Word64)
              -> io (Result)
waitForFences = waitForFencesSafeOrUnsafe mkVkWaitForFencesUnsafe

-- | A variant of 'waitForFences' which makes a *safe* FFI call
waitForFencesSafe :: forall io
                   . (MonadIO io)
                  => -- No documentation found for Nested "vkWaitForFences" "device"
                     Device
                  -> -- No documentation found for Nested "vkWaitForFences" "pFences"
                     ("fences" ::: Vector Fence)
                  -> -- No documentation found for Nested "vkWaitForFences" "waitAll"
                     ("waitAll" ::: Bool)
                  -> -- No documentation found for Nested "vkWaitForFences" "timeout"
                     ("timeout" ::: Word64)
                  -> io (Result)
waitForFencesSafe = waitForFencesSafeOrUnsafe mkVkWaitForFencesSafe



-- No documentation found for TopLevel "VkFenceCreateInfo"
data FenceCreateInfo (es :: [Type]) = FenceCreateInfo
  { -- No documentation found for Nested "VkFenceCreateInfo" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkFenceCreateInfo" "flags"
    flags :: FenceCreateFlags
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (FenceCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (FenceCreateInfo es)

instance Extensible FenceCreateInfo where
  extensibleType = STRUCTURE_TYPE_FENCE_CREATE_INFO
  setNext x next = x{next = next}
  getNext FenceCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends FenceCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @ExportFenceWin32HandleInfoKHR = Just f
    | Just Refl <- eqT @e @ExportFenceCreateInfo = Just f
    | otherwise = Nothing

instance (Extendss FenceCreateInfo es, PokeChain es) => ToCStruct (FenceCreateInfo es) where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p FenceCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FENCE_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr FenceCreateFlags)) (flags)
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FENCE_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ f

instance (Extendss FenceCreateInfo es, PeekChain es) => FromCStruct (FenceCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @FenceCreateFlags ((p `plusPtr` 16 :: Ptr FenceCreateFlags))
    pure $ FenceCreateInfo
             next flags

instance es ~ '[] => Zero (FenceCreateInfo es) where
  zero = FenceCreateInfo
           ()
           zero

