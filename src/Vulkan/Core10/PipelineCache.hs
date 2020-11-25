{-# language CPP #-}
-- No documentation found for Chapter "PipelineCache"
module Vulkan.Core10.PipelineCache  ( createPipelineCache
                                    , withPipelineCache
                                    , destroyPipelineCache
                                    , getPipelineCacheData
                                    , mergePipelineCaches
                                    , PipelineCacheCreateInfo(..)
                                    , PipelineCache(..)
                                    , PipelineCacheCreateFlagBits(..)
                                    , PipelineCacheCreateFlags
                                    ) where

import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.ByteString (packCStringLen)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Foreign.C.Types (CSize(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.C.Types (CSize)
import Foreign.C.Types (CSize(CSize))
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
import Data.Word (Word64)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkCreatePipelineCache))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyPipelineCache))
import Vulkan.Dynamic (DeviceCmds(pVkGetPipelineCacheData))
import Vulkan.Dynamic (DeviceCmds(pVkMergePipelineCaches))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Handles (PipelineCache)
import Vulkan.Core10.Handles (PipelineCache(..))
import Vulkan.Core10.Enums.PipelineCacheCreateFlagBits (PipelineCacheCreateFlags)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Handles (PipelineCache(..))
import Vulkan.Core10.Enums.PipelineCacheCreateFlagBits (PipelineCacheCreateFlagBits(..))
import Vulkan.Core10.Enums.PipelineCacheCreateFlagBits (PipelineCacheCreateFlags)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreatePipelineCache
  :: FunPtr (Ptr Device_T -> Ptr PipelineCacheCreateInfo -> Ptr AllocationCallbacks -> Ptr PipelineCache -> IO Result) -> Ptr Device_T -> Ptr PipelineCacheCreateInfo -> Ptr AllocationCallbacks -> Ptr PipelineCache -> IO Result

-- No documentation found for TopLevel "vkCreatePipelineCache"
createPipelineCache :: forall io
                     . (MonadIO io)
                    => -- No documentation found for Nested "vkCreatePipelineCache" "device"
                       Device
                    -> -- No documentation found for Nested "vkCreatePipelineCache" "pCreateInfo"
                       PipelineCacheCreateInfo
                    -> -- No documentation found for Nested "vkCreatePipelineCache" "pAllocator"
                       ("allocator" ::: Maybe AllocationCallbacks)
                    -> io (PipelineCache)
createPipelineCache device createInfo allocator = liftIO . evalContT $ do
  let vkCreatePipelineCachePtr = pVkCreatePipelineCache (deviceCmds (device :: Device))
  lift $ unless (vkCreatePipelineCachePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreatePipelineCache is null" Nothing Nothing
  let vkCreatePipelineCache' = mkVkCreatePipelineCache vkCreatePipelineCachePtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPPipelineCache <- ContT $ bracket (callocBytes @PipelineCache 8) free
  r <- lift $ vkCreatePipelineCache' (deviceHandle (device)) pCreateInfo pAllocator (pPPipelineCache)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPipelineCache <- lift $ peek @PipelineCache pPPipelineCache
  pure $ (pPipelineCache)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createPipelineCache' and 'destroyPipelineCache'
--
-- To ensure that 'destroyPipelineCache' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withPipelineCache :: forall io r . MonadIO io => Device -> PipelineCacheCreateInfo -> Maybe AllocationCallbacks -> (io PipelineCache -> (PipelineCache -> io ()) -> r) -> r
withPipelineCache device pCreateInfo pAllocator b =
  b (createPipelineCache device pCreateInfo pAllocator)
    (\(o0) -> destroyPipelineCache device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyPipelineCache
  :: FunPtr (Ptr Device_T -> PipelineCache -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> PipelineCache -> Ptr AllocationCallbacks -> IO ()

-- No documentation found for TopLevel "vkDestroyPipelineCache"
destroyPipelineCache :: forall io
                      . (MonadIO io)
                     => -- No documentation found for Nested "vkDestroyPipelineCache" "device"
                        Device
                     -> -- No documentation found for Nested "vkDestroyPipelineCache" "pipelineCache"
                        PipelineCache
                     -> -- No documentation found for Nested "vkDestroyPipelineCache" "pAllocator"
                        ("allocator" ::: Maybe AllocationCallbacks)
                     -> io ()
destroyPipelineCache device pipelineCache allocator = liftIO . evalContT $ do
  let vkDestroyPipelineCachePtr = pVkDestroyPipelineCache (deviceCmds (device :: Device))
  lift $ unless (vkDestroyPipelineCachePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyPipelineCache is null" Nothing Nothing
  let vkDestroyPipelineCache' = mkVkDestroyPipelineCache vkDestroyPipelineCachePtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroyPipelineCache' (deviceHandle (device)) (pipelineCache) pAllocator
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPipelineCacheData
  :: FunPtr (Ptr Device_T -> PipelineCache -> Ptr CSize -> Ptr () -> IO Result) -> Ptr Device_T -> PipelineCache -> Ptr CSize -> Ptr () -> IO Result

-- No documentation found for TopLevel "vkGetPipelineCacheData"
getPipelineCacheData :: forall io
                      . (MonadIO io)
                     => -- No documentation found for Nested "vkGetPipelineCacheData" "device"
                        Device
                     -> -- No documentation found for Nested "vkGetPipelineCacheData" "pipelineCache"
                        PipelineCache
                     -> io (Result, ("data" ::: ByteString))
getPipelineCacheData device pipelineCache = liftIO . evalContT $ do
  let vkGetPipelineCacheDataPtr = pVkGetPipelineCacheData (deviceCmds (device :: Device))
  lift $ unless (vkGetPipelineCacheDataPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPipelineCacheData is null" Nothing Nothing
  let vkGetPipelineCacheData' = mkVkGetPipelineCacheData vkGetPipelineCacheDataPtr
  let device' = deviceHandle (device)
  pPDataSize <- ContT $ bracket (callocBytes @CSize 8) free
  r <- lift $ vkGetPipelineCacheData' device' (pipelineCache) (pPDataSize) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pDataSize <- lift $ peek @CSize pPDataSize
  pPData <- ContT $ bracket (callocBytes @(()) (fromIntegral (((\(CSize a) -> a) pDataSize)))) free
  r' <- lift $ vkGetPipelineCacheData' device' (pipelineCache) (pPDataSize) (pPData)
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pDataSize'' <- lift $ peek @CSize pPDataSize
  pData' <- lift $ packCStringLen  (castPtr @() @CChar pPData, (fromIntegral (((\(CSize a) -> a) pDataSize''))))
  pure $ ((r'), pData')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkMergePipelineCaches
  :: FunPtr (Ptr Device_T -> PipelineCache -> Word32 -> Ptr PipelineCache -> IO Result) -> Ptr Device_T -> PipelineCache -> Word32 -> Ptr PipelineCache -> IO Result

-- No documentation found for TopLevel "vkMergePipelineCaches"
mergePipelineCaches :: forall io
                     . (MonadIO io)
                    => -- No documentation found for Nested "vkMergePipelineCaches" "device"
                       Device
                    -> -- No documentation found for Nested "vkMergePipelineCaches" "dstCache"
                       ("dstCache" ::: PipelineCache)
                    -> -- No documentation found for Nested "vkMergePipelineCaches" "pSrcCaches"
                       ("srcCaches" ::: Vector PipelineCache)
                    -> io ()
mergePipelineCaches device dstCache srcCaches = liftIO . evalContT $ do
  let vkMergePipelineCachesPtr = pVkMergePipelineCaches (deviceCmds (device :: Device))
  lift $ unless (vkMergePipelineCachesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkMergePipelineCaches is null" Nothing Nothing
  let vkMergePipelineCaches' = mkVkMergePipelineCaches vkMergePipelineCachesPtr
  pPSrcCaches <- ContT $ allocaBytesAligned @PipelineCache ((Data.Vector.length (srcCaches)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPSrcCaches `plusPtr` (8 * (i)) :: Ptr PipelineCache) (e)) (srcCaches)
  r <- lift $ vkMergePipelineCaches' (deviceHandle (device)) (dstCache) ((fromIntegral (Data.Vector.length $ (srcCaches)) :: Word32)) (pPSrcCaches)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))



-- No documentation found for TopLevel "VkPipelineCacheCreateInfo"
data PipelineCacheCreateInfo = PipelineCacheCreateInfo
  { -- No documentation found for Nested "VkPipelineCacheCreateInfo" "flags"
    flags :: PipelineCacheCreateFlags
  , -- No documentation found for Nested "VkPipelineCacheCreateInfo" "initialDataSize"
    initialDataSize :: Word64
  , -- No documentation found for Nested "VkPipelineCacheCreateInfo" "pInitialData"
    initialData :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineCacheCreateInfo)
#endif
deriving instance Show PipelineCacheCreateInfo

instance ToCStruct PipelineCacheCreateInfo where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineCacheCreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineCacheCreateFlags)) (flags)
    poke ((p `plusPtr` 24 :: Ptr CSize)) (CSize (initialDataSize))
    poke ((p `plusPtr` 32 :: Ptr (Ptr ()))) (initialData)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 32 :: Ptr (Ptr ()))) (zero)
    f

instance FromCStruct PipelineCacheCreateInfo where
  peekCStruct p = do
    flags <- peek @PipelineCacheCreateFlags ((p `plusPtr` 16 :: Ptr PipelineCacheCreateFlags))
    initialDataSize <- peek @CSize ((p `plusPtr` 24 :: Ptr CSize))
    pInitialData <- peek @(Ptr ()) ((p `plusPtr` 32 :: Ptr (Ptr ())))
    pure $ PipelineCacheCreateInfo
             flags ((\(CSize a) -> a) initialDataSize) pInitialData


instance Storable PipelineCacheCreateInfo where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineCacheCreateInfo where
  zero = PipelineCacheCreateInfo
           zero
           zero
           zero

