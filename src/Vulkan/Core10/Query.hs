{-# language CPP #-}
-- No documentation found for Chapter "Query"
module Vulkan.Core10.Query  ( createQueryPool
                            , withQueryPool
                            , destroyQueryPool
                            , getQueryPoolResults
                            , QueryPoolCreateInfo(..)
                            , QueryPool(..)
                            , QueryPoolCreateFlags(..)
                            , QueryType(..)
                            , QueryResultFlagBits(..)
                            , QueryResultFlags
                            , QueryPipelineStatisticFlagBits(..)
                            , QueryPipelineStatisticFlags
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
import Foreign.C.Types (CSize(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.C.Types (CSize)
import Foreign.C.Types (CSize(CSize))
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
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkCreateQueryPool))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyQueryPool))
import Vulkan.Dynamic (DeviceCmds(pVkGetQueryPoolResults))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.QueryPipelineStatisticFlagBits (QueryPipelineStatisticFlags)
import Vulkan.Core10.Handles (QueryPool)
import Vulkan.Core10.Handles (QueryPool(..))
import Vulkan.Core10.Enums.QueryPoolCreateFlags (QueryPoolCreateFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_performance_query (QueryPoolPerformanceCreateInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_INTEL_performance_query (QueryPoolPerformanceQueryCreateInfoINTEL)
import Vulkan.Core10.Enums.QueryResultFlagBits (QueryResultFlagBits(..))
import Vulkan.Core10.Enums.QueryResultFlagBits (QueryResultFlags)
import Vulkan.Core10.Enums.QueryType (QueryType)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Enums.QueryPipelineStatisticFlagBits (QueryPipelineStatisticFlagBits(..))
import Vulkan.Core10.Enums.QueryPipelineStatisticFlagBits (QueryPipelineStatisticFlags)
import Vulkan.Core10.Handles (QueryPool(..))
import Vulkan.Core10.Enums.QueryPoolCreateFlags (QueryPoolCreateFlags(..))
import Vulkan.Core10.Enums.QueryResultFlagBits (QueryResultFlagBits(..))
import Vulkan.Core10.Enums.QueryResultFlagBits (QueryResultFlags)
import Vulkan.Core10.Enums.QueryType (QueryType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateQueryPool
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct QueryPoolCreateInfo) -> Ptr AllocationCallbacks -> Ptr QueryPool -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct QueryPoolCreateInfo) -> Ptr AllocationCallbacks -> Ptr QueryPool -> IO Result

-- No documentation found for TopLevel "vkCreateQueryPool"
createQueryPool :: forall a io
                 . (Extendss QueryPoolCreateInfo a, PokeChain a, MonadIO io)
                => -- No documentation found for Nested "vkCreateQueryPool" "device"
                   Device
                -> -- No documentation found for Nested "vkCreateQueryPool" "pCreateInfo"
                   (QueryPoolCreateInfo a)
                -> -- No documentation found for Nested "vkCreateQueryPool" "pAllocator"
                   ("allocator" ::: Maybe AllocationCallbacks)
                -> io (QueryPool)
createQueryPool device createInfo allocator = liftIO . evalContT $ do
  let vkCreateQueryPoolPtr = pVkCreateQueryPool (deviceCmds (device :: Device))
  lift $ unless (vkCreateQueryPoolPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateQueryPool is null" Nothing Nothing
  let vkCreateQueryPool' = mkVkCreateQueryPool vkCreateQueryPoolPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPQueryPool <- ContT $ bracket (callocBytes @QueryPool 8) free
  r <- lift $ vkCreateQueryPool' (deviceHandle (device)) (forgetExtensions pCreateInfo) pAllocator (pPQueryPool)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pQueryPool <- lift $ peek @QueryPool pPQueryPool
  pure $ (pQueryPool)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createQueryPool' and 'destroyQueryPool'
--
-- To ensure that 'destroyQueryPool' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withQueryPool :: forall a io r . (Extendss QueryPoolCreateInfo a, PokeChain a, MonadIO io) => Device -> QueryPoolCreateInfo a -> Maybe AllocationCallbacks -> (io QueryPool -> (QueryPool -> io ()) -> r) -> r
withQueryPool device pCreateInfo pAllocator b =
  b (createQueryPool device pCreateInfo pAllocator)
    (\(o0) -> destroyQueryPool device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyQueryPool
  :: FunPtr (Ptr Device_T -> QueryPool -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> QueryPool -> Ptr AllocationCallbacks -> IO ()

-- No documentation found for TopLevel "vkDestroyQueryPool"
destroyQueryPool :: forall io
                  . (MonadIO io)
                 => -- No documentation found for Nested "vkDestroyQueryPool" "device"
                    Device
                 -> -- No documentation found for Nested "vkDestroyQueryPool" "queryPool"
                    QueryPool
                 -> -- No documentation found for Nested "vkDestroyQueryPool" "pAllocator"
                    ("allocator" ::: Maybe AllocationCallbacks)
                 -> io ()
destroyQueryPool device queryPool allocator = liftIO . evalContT $ do
  let vkDestroyQueryPoolPtr = pVkDestroyQueryPool (deviceCmds (device :: Device))
  lift $ unless (vkDestroyQueryPoolPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyQueryPool is null" Nothing Nothing
  let vkDestroyQueryPool' = mkVkDestroyQueryPool vkDestroyQueryPoolPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroyQueryPool' (deviceHandle (device)) (queryPool) pAllocator
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetQueryPoolResults
  :: FunPtr (Ptr Device_T -> QueryPool -> Word32 -> Word32 -> CSize -> Ptr () -> DeviceSize -> QueryResultFlags -> IO Result) -> Ptr Device_T -> QueryPool -> Word32 -> Word32 -> CSize -> Ptr () -> DeviceSize -> QueryResultFlags -> IO Result

-- No documentation found for TopLevel "vkGetQueryPoolResults"
getQueryPoolResults :: forall io
                     . (MonadIO io)
                    => -- No documentation found for Nested "vkGetQueryPoolResults" "device"
                       Device
                    -> -- No documentation found for Nested "vkGetQueryPoolResults" "queryPool"
                       QueryPool
                    -> -- No documentation found for Nested "vkGetQueryPoolResults" "firstQuery"
                       ("firstQuery" ::: Word32)
                    -> -- No documentation found for Nested "vkGetQueryPoolResults" "queryCount"
                       ("queryCount" ::: Word32)
                    -> -- No documentation found for Nested "vkGetQueryPoolResults" "dataSize"
                       ("dataSize" ::: Word64)
                    -> -- No documentation found for Nested "vkGetQueryPoolResults" "pData"
                       ("data" ::: Ptr ())
                    -> -- No documentation found for Nested "vkGetQueryPoolResults" "stride"
                       ("stride" ::: DeviceSize)
                    -> -- No documentation found for Nested "vkGetQueryPoolResults" "flags"
                       QueryResultFlags
                    -> io (Result)
getQueryPoolResults device queryPool firstQuery queryCount dataSize data' stride flags = liftIO $ do
  let vkGetQueryPoolResultsPtr = pVkGetQueryPoolResults (deviceCmds (device :: Device))
  unless (vkGetQueryPoolResultsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetQueryPoolResults is null" Nothing Nothing
  let vkGetQueryPoolResults' = mkVkGetQueryPoolResults vkGetQueryPoolResultsPtr
  r <- vkGetQueryPoolResults' (deviceHandle (device)) (queryPool) (firstQuery) (queryCount) (CSize (dataSize)) (data') (stride) (flags)
  when (r < SUCCESS) (throwIO (VulkanException r))
  pure $ (r)



-- No documentation found for TopLevel "VkQueryPoolCreateInfo"
data QueryPoolCreateInfo (es :: [Type]) = QueryPoolCreateInfo
  { -- No documentation found for Nested "VkQueryPoolCreateInfo" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkQueryPoolCreateInfo" "flags"
    flags :: QueryPoolCreateFlags
  , -- No documentation found for Nested "VkQueryPoolCreateInfo" "queryType"
    queryType :: QueryType
  , -- No documentation found for Nested "VkQueryPoolCreateInfo" "queryCount"
    queryCount :: Word32
  , -- No documentation found for Nested "VkQueryPoolCreateInfo" "pipelineStatistics"
    pipelineStatistics :: QueryPipelineStatisticFlags
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (QueryPoolCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (QueryPoolCreateInfo es)

instance Extensible QueryPoolCreateInfo where
  extensibleType = STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO
  setNext x next = x{next = next}
  getNext QueryPoolCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends QueryPoolCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @QueryPoolPerformanceQueryCreateInfoINTEL = Just f
    | Just Refl <- eqT @e @QueryPoolPerformanceCreateInfoKHR = Just f
    | otherwise = Nothing

instance (Extendss QueryPoolCreateInfo es, PokeChain es) => ToCStruct (QueryPoolCreateInfo es) where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p QueryPoolCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr QueryPoolCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr QueryType)) (queryType)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (queryCount)
    lift $ poke ((p `plusPtr` 28 :: Ptr QueryPipelineStatisticFlags)) (pipelineStatistics)
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 20 :: Ptr QueryType)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    lift $ f

instance (Extendss QueryPoolCreateInfo es, PeekChain es) => FromCStruct (QueryPoolCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @QueryPoolCreateFlags ((p `plusPtr` 16 :: Ptr QueryPoolCreateFlags))
    queryType <- peek @QueryType ((p `plusPtr` 20 :: Ptr QueryType))
    queryCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pipelineStatistics <- peek @QueryPipelineStatisticFlags ((p `plusPtr` 28 :: Ptr QueryPipelineStatisticFlags))
    pure $ QueryPoolCreateInfo
             next flags queryType queryCount pipelineStatistics

instance es ~ '[] => Zero (QueryPoolCreateInfo es) where
  zero = QueryPoolCreateInfo
           ()
           zero
           zero
           zero
           zero

