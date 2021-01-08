{-# language CPP #-}
-- No documentation found for Chapter "VulkanMemoryAllocator"
module VulkanMemoryAllocator  ( createAllocator
                              , withAllocator
                              , destroyAllocator
                              , getAllocatorInfo
                              , getPhysicalDeviceProperties
                              , getMemoryProperties
                              , getMemoryTypeProperties
                              , setCurrentFrameIndex
                              , calculateStats
                              , getBudget
                              , buildStatsString
                              , freeStatsString
                              , findMemoryTypeIndex
                              , findMemoryTypeIndexForBufferInfo
                              , findMemoryTypeIndexForImageInfo
                              , createPool
                              , withPool
                              , destroyPool
                              , getPoolStats
                              , makePoolAllocationsLost
                              , checkPoolCorruption
                              , getPoolName
                              , setPoolName
                              , allocateMemory
                              , withMemory
                              , allocateMemoryPages
                              , withMemoryPages
                              , allocateMemoryForBuffer
                              , withMemoryForBuffer
                              , allocateMemoryForImage
                              , withMemoryForImage
                              , freeMemory
                              , freeMemoryPages
                              , resizeAllocation
                              , getAllocationInfo
                              , touchAllocation
                              , setAllocationUserData
                              , createLostAllocation
                              , withLostAllocation
                              , mapMemory
                              , withMappedMemory
                              , unmapMemory
                              , flushAllocation
                              , invalidateAllocation
                              , flushAllocations
                              , invalidateAllocations
                              , checkCorruption
                              , defragmentationBegin
                              , withDefragmentation
                              , defragmentationEnd
                              , beginDefragmentationPass
                              , useDefragmentationPass
                              , endDefragmentationPass
                              , defragment
                              , bindBufferMemory
                              , bindBufferMemory2
                              , bindImageMemory
                              , bindImageMemory2
                              , createBuffer
                              , withBuffer
                              , destroyBuffer
                              , createImage
                              , withImage
                              , destroyImage
                              , Allocator(..)
                              , PFN_vmaAllocateDeviceMemoryFunction
                              , FN_vmaAllocateDeviceMemoryFunction
                              , PFN_vmaFreeDeviceMemoryFunction
                              , FN_vmaFreeDeviceMemoryFunction
                              , DeviceMemoryCallbacks(..)
                              , AllocatorCreateFlags
                              , AllocatorCreateFlagBits( ALLOCATOR_CREATE_EXTERNALLY_SYNCHRONIZED_BIT
                                                       , ALLOCATOR_CREATE_KHR_DEDICATED_ALLOCATION_BIT
                                                       , ALLOCATOR_CREATE_KHR_BIND_MEMORY2_BIT
                                                       , ALLOCATOR_CREATE_EXT_MEMORY_BUDGET_BIT
                                                       , ALLOCATOR_CREATE_AMD_DEVICE_COHERENT_MEMORY_BIT
                                                       , ALLOCATOR_CREATE_BUFFER_DEVICE_ADDRESS_BIT
                                                       , ..
                                                       )
                              , VulkanFunctions(..)
                              , RecordFlags
                              , RecordFlagBits( RECORD_FLUSH_AFTER_CALL_BIT
                                              , ..
                                              )
                              , RecordSettings(..)
                              , AllocatorCreateInfo(..)
                              , AllocatorInfo(..)
                              , StatInfo(..)
                              , Stats(..)
                              , Budget(..)
                              , Pool(..)
                              , MemoryUsage( MEMORY_USAGE_UNKNOWN
                                           , MEMORY_USAGE_GPU_ONLY
                                           , MEMORY_USAGE_CPU_ONLY
                                           , MEMORY_USAGE_CPU_TO_GPU
                                           , MEMORY_USAGE_GPU_TO_CPU
                                           , MEMORY_USAGE_CPU_COPY
                                           , MEMORY_USAGE_GPU_LAZILY_ALLOCATED
                                           , ..
                                           )
                              , AllocationCreateFlags
                              , AllocationCreateFlagBits( ALLOCATION_CREATE_DEDICATED_MEMORY_BIT
                                                        , ALLOCATION_CREATE_NEVER_ALLOCATE_BIT
                                                        , ALLOCATION_CREATE_MAPPED_BIT
                                                        , ALLOCATION_CREATE_CAN_BECOME_LOST_BIT
                                                        , ALLOCATION_CREATE_CAN_MAKE_OTHER_LOST_BIT
                                                        , ALLOCATION_CREATE_USER_DATA_COPY_STRING_BIT
                                                        , ALLOCATION_CREATE_UPPER_ADDRESS_BIT
                                                        , ALLOCATION_CREATE_DONT_BIND_BIT
                                                        , ALLOCATION_CREATE_WITHIN_BUDGET_BIT
                                                        , ALLOCATION_CREATE_STRATEGY_BEST_FIT_BIT
                                                        , ALLOCATION_CREATE_STRATEGY_WORST_FIT_BIT
                                                        , ALLOCATION_CREATE_STRATEGY_FIRST_FIT_BIT
                                                        , ALLOCATION_CREATE_STRATEGY_MIN_MEMORY_BIT
                                                        , ALLOCATION_CREATE_STRATEGY_MIN_TIME_BIT
                                                        , ALLOCATION_CREATE_STRATEGY_MIN_FRAGMENTATION_BIT
                                                        , ALLOCATION_CREATE_STRATEGY_MASK
                                                        , ..
                                                        )
                              , AllocationCreateInfo(..)
                              , PoolCreateFlags
                              , PoolCreateFlagBits( POOL_CREATE_IGNORE_BUFFER_IMAGE_GRANULARITY_BIT
                                                  , POOL_CREATE_LINEAR_ALGORITHM_BIT
                                                  , POOL_CREATE_BUDDY_ALGORITHM_BIT
                                                  , POOL_CREATE_ALGORITHM_MASK
                                                  , ..
                                                  )
                              , PoolCreateInfo(..)
                              , PoolStats(..)
                              , Allocation(..)
                              , AllocationInfo(..)
                              , DefragmentationContext(..)
                              , DefragmentationFlags
                              , DefragmentationFlagBits( DEFRAGMENTATION_FLAG_INCREMENTAL
                                                       , ..
                                                       )
                              , DefragmentationInfo2(..)
                              , DefragmentationPassMoveInfo(..)
                              , DefragmentationPassInfo(..)
                              , DefragmentationInfo(..)
                              , DefragmentationStats(..)
                              ) where

import Vulkan (AllocationCallbacks)
import Vulkan (BindBufferMemoryInfo)
import Vulkan (BindImageMemoryInfo)
import Vulkan (Bool32)
import Vulkan (Buffer)
import Vulkan (BufferCopy)
import Vulkan (BufferCreateInfo)
import Vulkan (BufferMemoryRequirementsInfo2)
import Vulkan (CommandBuffer_T)
import Vulkan (DeviceMemory)
import Vulkan (DeviceSize)
import Vulkan (Device_T)
import Vulkan (Flags)
import Vulkan (Image)
import Vulkan (ImageCreateInfo)
import Vulkan (ImageMemoryRequirementsInfo2)
import Vulkan (Instance_T)
import Vulkan (MappedMemoryRange)
import Vulkan (MemoryAllocateInfo)
import Vulkan (MemoryMapFlags)
import Vulkan (MemoryPropertyFlags)
import Vulkan (MemoryRequirements)
import Vulkan (MemoryRequirements2)
import Vulkan (PhysicalDeviceMemoryProperties)
import Vulkan (PhysicalDeviceMemoryProperties2)
import Vulkan (PhysicalDeviceProperties)
import Vulkan (PhysicalDevice_T)
import Vulkan (Result)
import Vulkan.CStruct.Utils (FixedArray)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Utils (maybePeek)
import GHC.Base (when)
import GHC.IO (throwIO)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Numeric (showHex)
import Data.ByteString (packCString)
import Data.ByteString (useAsCString)
import Data.Coerce (coerce)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import qualified Data.Vector (null)
import Vulkan.Core10.APIConstants (pattern MAX_MEMORY_HEAPS)
import Vulkan.Core10.APIConstants (pattern MAX_MEMORY_TYPES)
import Vulkan.Core10.Enums.Result (pattern SUCCESS)
import Foreign.C.Types (CChar(..))
import Foreign.C.Types (CSize(..))
import Vulkan (Bool32(..))
import Vulkan (Buffer(..))
import Vulkan (Image(..))
import Vulkan (MemoryPropertyFlagBits(..))
import Vulkan (Result(..))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.APIConstants (IsHandle)
import Vulkan.Core10.APIConstants (MAX_MEMORY_HEAPS)
import Vulkan.Core10.APIConstants (MAX_MEMORY_TYPES)
import Vulkan.Exception (VulkanException(..))
import Vulkan.NamedType ((:::))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.C.Types (CSize)
import Foreign.C.Types (CSize(..))
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
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Word (Word64)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)

foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaCreateAllocator" ffiVmaCreateAllocator
  :: Ptr AllocatorCreateInfo -> Ptr Allocator -> IO Result

-- | Creates Allocator object.
createAllocator :: forall io
                 . (MonadIO io)
                => -- No documentation found for Nested "vmaCreateAllocator" "pCreateInfo"
                   AllocatorCreateInfo
                -> io (Allocator)
createAllocator createInfo = liftIO . evalContT $ do
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pPAllocator <- ContT $ bracket (callocBytes @Allocator 8) free
  r <- lift $ traceAroundEvent "vmaCreateAllocator" ((ffiVmaCreateAllocator) pCreateInfo (pPAllocator))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pAllocator <- lift $ peek @Allocator pPAllocator
  pure $ (pAllocator)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createAllocator' and 'destroyAllocator'
--
-- To ensure that 'destroyAllocator' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withAllocator :: forall io r . MonadIO io => AllocatorCreateInfo -> (io Allocator -> (Allocator -> io ()) -> r) -> r
withAllocator pCreateInfo b =
  b (createAllocator pCreateInfo)
    (\(o0) -> destroyAllocator o0)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaDestroyAllocator" ffiVmaDestroyAllocator
  :: Allocator -> IO ()

-- | Destroys allocator object.
destroyAllocator :: forall io
                  . (MonadIO io)
                 => -- No documentation found for Nested "vmaDestroyAllocator" "allocator"
                    Allocator
                 -> io ()
destroyAllocator allocator = liftIO $ do
  traceAroundEvent "vmaDestroyAllocator" ((ffiVmaDestroyAllocator) (allocator))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaGetAllocatorInfo" ffiVmaGetAllocatorInfo
  :: Allocator -> Ptr AllocatorInfo -> IO ()

-- | Returns information about existing 'Allocator' object - handle to Vulkan
-- device etc.
--
-- It might be useful if you want to keep just the 'Allocator' handle and
-- fetch other required handles to @VkPhysicalDevice@, @VkDevice@ etc.
-- every time using this function.
getAllocatorInfo :: forall io
                  . (MonadIO io)
                 => -- No documentation found for Nested "vmaGetAllocatorInfo" "allocator"
                    Allocator
                 -> io (AllocatorInfo)
getAllocatorInfo allocator = liftIO . evalContT $ do
  pPAllocatorInfo <- ContT (withZeroCStruct @AllocatorInfo)
  lift $ traceAroundEvent "vmaGetAllocatorInfo" ((ffiVmaGetAllocatorInfo) (allocator) (pPAllocatorInfo))
  pAllocatorInfo <- lift $ peekCStruct @AllocatorInfo pPAllocatorInfo
  pure $ (pAllocatorInfo)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaGetPhysicalDeviceProperties" ffiVmaGetPhysicalDeviceProperties
  :: Allocator -> Ptr (Ptr PhysicalDeviceProperties) -> IO ()

-- | PhysicalDeviceProperties are fetched from physicalDevice by the
-- allocator. You can access it here, without fetching it again on your
-- own.
getPhysicalDeviceProperties :: forall io
                             . (MonadIO io)
                            => -- No documentation found for Nested "vmaGetPhysicalDeviceProperties" "allocator"
                               Allocator
                            -> io (Ptr PhysicalDeviceProperties)
getPhysicalDeviceProperties allocator = liftIO . evalContT $ do
  pPpPhysicalDeviceProperties <- ContT $ bracket (callocBytes @(Ptr PhysicalDeviceProperties) 8) free
  lift $ traceAroundEvent "vmaGetPhysicalDeviceProperties" ((ffiVmaGetPhysicalDeviceProperties) (allocator) (pPpPhysicalDeviceProperties))
  ppPhysicalDeviceProperties <- lift $ peek @(Ptr PhysicalDeviceProperties) pPpPhysicalDeviceProperties
  pure $ (ppPhysicalDeviceProperties)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaGetMemoryProperties" ffiVmaGetMemoryProperties
  :: Allocator -> Ptr (Ptr PhysicalDeviceMemoryProperties) -> IO ()

-- | PhysicalDeviceMemoryProperties are fetched from physicalDevice by the
-- allocator. You can access it here, without fetching it again on your
-- own.
getMemoryProperties :: forall io
                     . (MonadIO io)
                    => -- No documentation found for Nested "vmaGetMemoryProperties" "allocator"
                       Allocator
                    -> io (Ptr PhysicalDeviceMemoryProperties)
getMemoryProperties allocator = liftIO . evalContT $ do
  pPpPhysicalDeviceMemoryProperties <- ContT $ bracket (callocBytes @(Ptr PhysicalDeviceMemoryProperties) 8) free
  lift $ traceAroundEvent "vmaGetMemoryProperties" ((ffiVmaGetMemoryProperties) (allocator) (pPpPhysicalDeviceMemoryProperties))
  ppPhysicalDeviceMemoryProperties <- lift $ peek @(Ptr PhysicalDeviceMemoryProperties) pPpPhysicalDeviceMemoryProperties
  pure $ (ppPhysicalDeviceMemoryProperties)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaGetMemoryTypeProperties" ffiVmaGetMemoryTypeProperties
  :: Allocator -> Word32 -> Ptr MemoryPropertyFlags -> IO ()

-- | Given Memory Type Index, returns Property Flags of this memory type.
--
-- This is just a convenience function. Same information can be obtained
-- using 'getMemoryProperties'.
getMemoryTypeProperties :: forall io
                         . (MonadIO io)
                        => -- No documentation found for Nested "vmaGetMemoryTypeProperties" "allocator"
                           Allocator
                        -> -- No documentation found for Nested "vmaGetMemoryTypeProperties" "memoryTypeIndex"
                           ("memoryTypeIndex" ::: Word32)
                        -> io (MemoryPropertyFlags)
getMemoryTypeProperties allocator memoryTypeIndex = liftIO . evalContT $ do
  pPFlags <- ContT $ bracket (callocBytes @MemoryPropertyFlags 4) free
  lift $ traceAroundEvent "vmaGetMemoryTypeProperties" ((ffiVmaGetMemoryTypeProperties) (allocator) (memoryTypeIndex) (pPFlags))
  pFlags <- lift $ peek @MemoryPropertyFlags pPFlags
  pure $ (pFlags)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaSetCurrentFrameIndex" ffiVmaSetCurrentFrameIndex
  :: Allocator -> Word32 -> IO ()

-- | Sets index of the current frame.
--
-- This function must be used if you make allocations with
-- 'ALLOCATION_CREATE_CAN_BECOME_LOST_BIT' and
-- 'ALLOCATION_CREATE_CAN_MAKE_OTHER_LOST_BIT' flags to inform the
-- allocator when a new frame begins. Allocations queried using
-- 'getAllocationInfo' cannot become lost in the current frame.
setCurrentFrameIndex :: forall io
                      . (MonadIO io)
                     => -- No documentation found for Nested "vmaSetCurrentFrameIndex" "allocator"
                        Allocator
                     -> -- No documentation found for Nested "vmaSetCurrentFrameIndex" "frameIndex"
                        ("frameIndex" ::: Word32)
                     -> io ()
setCurrentFrameIndex allocator frameIndex = liftIO $ do
  traceAroundEvent "vmaSetCurrentFrameIndex" ((ffiVmaSetCurrentFrameIndex) (allocator) (frameIndex))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaCalculateStats" ffiVmaCalculateStats
  :: Allocator -> Ptr Stats -> IO ()

-- | Retrieves statistics from current state of the Allocator.
--
-- This function is called \"calculate\" not \"get\" because it has to
-- traverse all internal data structures, so it may be quite slow. For
-- faster but more brief statistics suitable to be called every frame or
-- every allocation, use 'getBudget'.
--
-- Note that when using allocator from multiple threads, returned
-- information may immediately become outdated.
calculateStats :: forall io
                . (MonadIO io)
               => -- No documentation found for Nested "vmaCalculateStats" "allocator"
                  Allocator
               -> io (Stats)
calculateStats allocator = liftIO . evalContT $ do
  pPStats <- ContT (withZeroCStruct @Stats)
  lift $ traceAroundEvent "vmaCalculateStats" ((ffiVmaCalculateStats) (allocator) (pPStats))
  pStats <- lift $ peekCStruct @Stats pPStats
  pure $ (pStats)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaGetBudget" ffiVmaGetBudget
  :: Allocator -> Ptr Budget -> IO ()

-- | Retrieves information about current memory budget for all memory heaps.
--
-- __Parameters__
--
-- +-----------+-----------+-----------------------------------------------+
-- | out       | pBudget   | Must point to array with number of elements   |
-- |           |           | at least equal to number of memory heaps in   |
-- |           |           | physical device used.                         |
-- +-----------+-----------+-----------------------------------------------+
--
-- This function is called \"get\" not \"calculate\" because it is very
-- fast, suitable to be called every frame or every allocation. For more
-- detailed statistics use 'calculateStats'.
--
-- Note that when using allocator from multiple threads, returned
-- information may immediately become outdated.
getBudget :: forall io
           . (MonadIO io)
          => -- No documentation found for Nested "vmaGetBudget" "allocator"
             Allocator
          -> io (("budget" ::: Vector Budget))
getBudget allocator = liftIO . evalContT $ do
  pPBudget <- ContT $ bracket (callocBytes @Budget ((MAX_MEMORY_HEAPS) * 32)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPBudget `advancePtrBytes` (i * 32) :: Ptr Budget) . ($ ())) [0..(MAX_MEMORY_HEAPS) - 1]
  lift $ traceAroundEvent "vmaGetBudget" ((ffiVmaGetBudget) (allocator) ((pPBudget)))
  pBudget <- lift $ generateM (MAX_MEMORY_HEAPS) (\i -> peekCStruct @Budget (((pPBudget) `advancePtrBytes` (32 * (i)) :: Ptr Budget)))
  pure $ (pBudget)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaBuildStatsString" ffiVmaBuildStatsString
  :: Allocator -> Ptr (Ptr CChar) -> Bool32 -> IO ()

-- | Builds and returns statistics as string in JSON format.
--
-- __Parameters__
--
-- +-----------+---------------+-----------------------------------------------+
-- | out       | ppStatsString | Must be freed using 'freeStatsString'         |
-- |           |               | function.                                     |
-- +-----------+---------------+-----------------------------------------------+
buildStatsString :: forall io
                  . (MonadIO io)
                 => -- No documentation found for Nested "vmaBuildStatsString" "allocator"
                    Allocator
                 -> -- No documentation found for Nested "vmaBuildStatsString" "detailedMap"
                    ("detailedMap" ::: Bool)
                 -> io (("statsString" ::: Ptr CChar))
buildStatsString allocator detailedMap = liftIO . evalContT $ do
  pPpStatsString <- ContT $ bracket (callocBytes @(Ptr CChar) 8) free
  lift $ traceAroundEvent "vmaBuildStatsString" ((ffiVmaBuildStatsString) (allocator) (pPpStatsString) (boolToBool32 (detailedMap)))
  ppStatsString <- lift $ peek @(Ptr CChar) pPpStatsString
  pure $ (ppStatsString)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaFreeStatsString" ffiVmaFreeStatsString
  :: Allocator -> Ptr CChar -> IO ()


freeStatsString :: forall io
                 . (MonadIO io)
                => -- No documentation found for Nested "vmaFreeStatsString" "allocator"
                   Allocator
                -> -- No documentation found for Nested "vmaFreeStatsString" "pStatsString"
                   ("statsString" ::: Ptr CChar)
                -> io ()
freeStatsString allocator statsString = liftIO $ do
  traceAroundEvent "vmaFreeStatsString" ((ffiVmaFreeStatsString) (allocator) (statsString))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaFindMemoryTypeIndex" ffiVmaFindMemoryTypeIndex
  :: Allocator -> Word32 -> Ptr AllocationCreateInfo -> Ptr Word32 -> IO Result

-- | Helps to find memoryTypeIndex, given memoryTypeBits and
-- 'AllocationCreateInfo'.
--
-- This algorithm tries to find a memory type that:
--
-- -   Is allowed by memoryTypeBits.
--
-- -   Contains all the flags from pAllocationCreateInfo->requiredFlags.
--
-- -   Matches intended usage.
--
-- -   Has as many flags from pAllocationCreateInfo->preferredFlags as
--     possible.
--
-- __Returns__
--
-- Returns VK_ERROR_FEATURE_NOT_PRESENT if not found. Receiving such result
-- from this function or any other allocating function probably means that
-- your device doesn\'t support any memory type with requested features for
-- the specific type of resource you want to use it for. Please check
-- parameters of your resource, like image layout (OPTIMAL versus LINEAR)
-- or mip level count.
findMemoryTypeIndex :: forall io
                     . (MonadIO io)
                    => -- No documentation found for Nested "vmaFindMemoryTypeIndex" "allocator"
                       Allocator
                    -> -- No documentation found for Nested "vmaFindMemoryTypeIndex" "memoryTypeBits"
                       ("memoryTypeBits" ::: Word32)
                    -> -- No documentation found for Nested "vmaFindMemoryTypeIndex" "pAllocationCreateInfo"
                       AllocationCreateInfo
                    -> io (("memoryTypeIndex" ::: Word32))
findMemoryTypeIndex allocator memoryTypeBits allocationCreateInfo = liftIO . evalContT $ do
  pAllocationCreateInfo <- ContT $ withCStruct (allocationCreateInfo)
  pPMemoryTypeIndex <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vmaFindMemoryTypeIndex" ((ffiVmaFindMemoryTypeIndex) (allocator) (memoryTypeBits) pAllocationCreateInfo (pPMemoryTypeIndex))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pMemoryTypeIndex <- lift $ peek @Word32 pPMemoryTypeIndex
  pure $ (pMemoryTypeIndex)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaFindMemoryTypeIndexForBufferInfo" ffiVmaFindMemoryTypeIndexForBufferInfo
  :: Allocator -> Ptr (SomeStruct BufferCreateInfo) -> Ptr AllocationCreateInfo -> Ptr Word32 -> IO Result

-- | Helps to find memoryTypeIndex, given VkBufferCreateInfo and
-- 'AllocationCreateInfo'.
--
-- It can be useful e.g. to determine value to be used as
-- /VmaPoolCreateInfo::memoryTypeIndex/. It internally creates a temporary,
-- dummy buffer that never has memory bound. It is just a convenience
-- function, equivalent to calling:
--
-- -   @vkCreateBuffer@
--
-- -   @vkGetBufferMemoryRequirements@
--
-- -   'findMemoryTypeIndex'
--
-- -   @vkDestroyBuffer@
findMemoryTypeIndexForBufferInfo :: forall a io
                                  . (Extendss BufferCreateInfo a, PokeChain a, MonadIO io)
                                 => -- No documentation found for Nested "vmaFindMemoryTypeIndexForBufferInfo" "allocator"
                                    Allocator
                                 -> -- No documentation found for Nested "vmaFindMemoryTypeIndexForBufferInfo" "pBufferCreateInfo"
                                    (BufferCreateInfo a)
                                 -> -- No documentation found for Nested "vmaFindMemoryTypeIndexForBufferInfo" "pAllocationCreateInfo"
                                    AllocationCreateInfo
                                 -> io (("memoryTypeIndex" ::: Word32))
findMemoryTypeIndexForBufferInfo allocator bufferCreateInfo allocationCreateInfo = liftIO . evalContT $ do
  pBufferCreateInfo <- ContT $ withCStruct (bufferCreateInfo)
  pAllocationCreateInfo <- ContT $ withCStruct (allocationCreateInfo)
  pPMemoryTypeIndex <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vmaFindMemoryTypeIndexForBufferInfo" ((ffiVmaFindMemoryTypeIndexForBufferInfo) (allocator) (forgetExtensions pBufferCreateInfo) pAllocationCreateInfo (pPMemoryTypeIndex))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pMemoryTypeIndex <- lift $ peek @Word32 pPMemoryTypeIndex
  pure $ (pMemoryTypeIndex)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaFindMemoryTypeIndexForImageInfo" ffiVmaFindMemoryTypeIndexForImageInfo
  :: Allocator -> Ptr (SomeStruct ImageCreateInfo) -> Ptr AllocationCreateInfo -> Ptr Word32 -> IO Result

-- | Helps to find memoryTypeIndex, given VkImageCreateInfo and
-- 'AllocationCreateInfo'.
--
-- It can be useful e.g. to determine value to be used as
-- /VmaPoolCreateInfo::memoryTypeIndex/. It internally creates a temporary,
-- dummy image that never has memory bound. It is just a convenience
-- function, equivalent to calling:
--
-- -   @vkCreateImage@
--
-- -   @vkGetImageMemoryRequirements@
--
-- -   'findMemoryTypeIndex'
--
-- -   @vkDestroyImage@
findMemoryTypeIndexForImageInfo :: forall a io
                                 . (Extendss ImageCreateInfo a, PokeChain a, MonadIO io)
                                => -- No documentation found for Nested "vmaFindMemoryTypeIndexForImageInfo" "allocator"
                                   Allocator
                                -> -- No documentation found for Nested "vmaFindMemoryTypeIndexForImageInfo" "pImageCreateInfo"
                                   (ImageCreateInfo a)
                                -> -- No documentation found for Nested "vmaFindMemoryTypeIndexForImageInfo" "pAllocationCreateInfo"
                                   AllocationCreateInfo
                                -> io (("memoryTypeIndex" ::: Word32))
findMemoryTypeIndexForImageInfo allocator imageCreateInfo allocationCreateInfo = liftIO . evalContT $ do
  pImageCreateInfo <- ContT $ withCStruct (imageCreateInfo)
  pAllocationCreateInfo <- ContT $ withCStruct (allocationCreateInfo)
  pPMemoryTypeIndex <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vmaFindMemoryTypeIndexForImageInfo" ((ffiVmaFindMemoryTypeIndexForImageInfo) (allocator) (forgetExtensions pImageCreateInfo) pAllocationCreateInfo (pPMemoryTypeIndex))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pMemoryTypeIndex <- lift $ peek @Word32 pPMemoryTypeIndex
  pure $ (pMemoryTypeIndex)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaCreatePool" ffiVmaCreatePool
  :: Allocator -> Ptr PoolCreateInfo -> Ptr Pool -> IO Result

-- | Allocates Vulkan device memory and creates 'Pool' object.
--
-- __Parameters__
--
-- +-----------+-------------+-----------------------------------------------+
-- |           | allocator   | Allocator object.                             |
-- +-----------+-------------+-----------------------------------------------+
-- |           | pCreateInfo | Parameters of pool to create.                 |
-- +-----------+-------------+-----------------------------------------------+
-- | out       | pPool       | Handle to created pool.                       |
-- +-----------+-------------+-----------------------------------------------+
createPool :: forall io
            . (MonadIO io)
           => -- No documentation found for Nested "vmaCreatePool" "allocator"
              Allocator
           -> -- No documentation found for Nested "vmaCreatePool" "pCreateInfo"
              PoolCreateInfo
           -> io (Pool)
createPool allocator createInfo = liftIO . evalContT $ do
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pPPool <- ContT $ bracket (callocBytes @Pool 8) free
  r <- lift $ traceAroundEvent "vmaCreatePool" ((ffiVmaCreatePool) (allocator) pCreateInfo (pPPool))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPool <- lift $ peek @Pool pPPool
  pure $ (pPool)

-- | A convenience wrapper to make a compatible pair of calls to 'createPool'
-- and 'destroyPool'
--
-- To ensure that 'destroyPool' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withPool :: forall io r . MonadIO io => Allocator -> PoolCreateInfo -> (io Pool -> (Pool -> io ()) -> r) -> r
withPool allocator pCreateInfo b =
  b (createPool allocator pCreateInfo)
    (\(o0) -> destroyPool allocator o0)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaDestroyPool" ffiVmaDestroyPool
  :: Allocator -> Pool -> IO ()

-- | Destroys 'Pool' object and frees Vulkan device memory.
destroyPool :: forall io
             . (MonadIO io)
            => -- No documentation found for Nested "vmaDestroyPool" "allocator"
               Allocator
            -> -- No documentation found for Nested "vmaDestroyPool" "pool"
               Pool
            -> io ()
destroyPool allocator pool = liftIO $ do
  traceAroundEvent "vmaDestroyPool" ((ffiVmaDestroyPool) (allocator) (pool))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaGetPoolStats" ffiVmaGetPoolStats
  :: Allocator -> Pool -> Ptr PoolStats -> IO ()

-- | Retrieves statistics of existing 'Pool' object.
--
-- __Parameters__
--
-- +-----------+------------+-----------------------------------------------+
-- |           | allocator  | Allocator object.                             |
-- +-----------+------------+-----------------------------------------------+
-- |           | pool       | Pool object.                                  |
-- +-----------+------------+-----------------------------------------------+
-- | out       | pPoolStats | Statistics of specified pool.                 |
-- +-----------+------------+-----------------------------------------------+
getPoolStats :: forall io
              . (MonadIO io)
             => -- No documentation found for Nested "vmaGetPoolStats" "allocator"
                Allocator
             -> -- No documentation found for Nested "vmaGetPoolStats" "pool"
                Pool
             -> io (PoolStats)
getPoolStats allocator pool = liftIO . evalContT $ do
  pPPoolStats <- ContT (withZeroCStruct @PoolStats)
  lift $ traceAroundEvent "vmaGetPoolStats" ((ffiVmaGetPoolStats) (allocator) (pool) (pPPoolStats))
  pPoolStats <- lift $ peekCStruct @PoolStats pPPoolStats
  pure $ (pPoolStats)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaMakePoolAllocationsLost" ffiVmaMakePoolAllocationsLost
  :: Allocator -> Pool -> Ptr CSize -> IO ()

-- | Marks all allocations in given pool as lost if they are not used in
-- current frame or /VmaPoolCreateInfo::frameInUseCount/ back from now.
--
-- __Parameters__
--
-- +-----------+----------------------+-----------------------------------------------+
-- |           | allocator            | Allocator object.                             |
-- +-----------+----------------------+-----------------------------------------------+
-- |           | pool                 | Pool.                                         |
-- +-----------+----------------------+-----------------------------------------------+
-- | out       | pLostAllocationCount | Number of allocations marked as lost.         |
-- |           |                      | Optional - pass null if you don\'t need this  |
-- |           |                      | information.                                  |
-- +-----------+----------------------+-----------------------------------------------+
makePoolAllocationsLost :: forall io
                         . (MonadIO io)
                        => -- No documentation found for Nested "vmaMakePoolAllocationsLost" "allocator"
                           Allocator
                        -> -- No documentation found for Nested "vmaMakePoolAllocationsLost" "pool"
                           Pool
                        -> io (("lostAllocationCount" ::: Word64))
makePoolAllocationsLost allocator pool = liftIO . evalContT $ do
  pPLostAllocationCount <- ContT $ bracket (callocBytes @CSize 8) free
  lift $ traceAroundEvent "vmaMakePoolAllocationsLost" ((ffiVmaMakePoolAllocationsLost) (allocator) (pool) (pPLostAllocationCount))
  pLostAllocationCount <- lift $ peek @CSize pPLostAllocationCount
  pure $ ((coerce @CSize @Word64 pLostAllocationCount))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaCheckPoolCorruption" ffiVmaCheckPoolCorruption
  :: Allocator -> Pool -> IO Result

-- | Checks magic number in margins around all allocations in given memory
-- pool in search for corruptions.
--
-- Corruption detection is enabled only when @VMA_DEBUG_DETECT_CORRUPTION@
-- macro is defined to nonzero, @VMA_DEBUG_MARGIN@ is defined to nonzero
-- and the pool is created in memory type that is @HOST_VISIBLE@ and
-- @HOST_COHERENT@. For more information, see /Corruption detection/.
--
-- Possible return values:
--
-- -   @VK_ERROR_FEATURE_NOT_PRESENT@ - corruption detection is not enabled
--     for specified pool.
--
-- -   @VK_SUCCESS@ - corruption detection has been performed and
--     succeeded.
--
-- -   @VK_ERROR_VALIDATION_FAILED_EXT@ - corruption detection has been
--     performed and found memory corruptions around one of the
--     allocations. @VMA_ASSERT@ is also fired in that case.
--
-- -   Other value: Error returned by Vulkan, e.g. memory mapping failure.
checkPoolCorruption :: forall io
                     . (MonadIO io)
                    => -- No documentation found for Nested "vmaCheckPoolCorruption" "allocator"
                       Allocator
                    -> -- No documentation found for Nested "vmaCheckPoolCorruption" "pool"
                       Pool
                    -> io ()
checkPoolCorruption allocator pool = liftIO $ do
  r <- traceAroundEvent "vmaCheckPoolCorruption" ((ffiVmaCheckPoolCorruption) (allocator) (pool))
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaGetPoolName" ffiVmaGetPoolName
  :: Allocator -> Pool -> Ptr (Ptr CChar) -> IO ()

-- | Retrieves name of a custom pool.
--
-- After the call @ppName@ is either null or points to an internally-owned
-- null-terminated string containing name of the pool that was previously
-- set. The pointer becomes invalid when the pool is destroyed or its name
-- is changed using 'setPoolName'.
getPoolName :: forall io
             . (MonadIO io)
            => -- No documentation found for Nested "vmaGetPoolName" "allocator"
               Allocator
            -> -- No documentation found for Nested "vmaGetPoolName" "pool"
               Pool
            -> io (("name" ::: Ptr CChar))
getPoolName allocator pool = liftIO . evalContT $ do
  pPpName <- ContT $ bracket (callocBytes @(Ptr CChar) 8) free
  lift $ traceAroundEvent "vmaGetPoolName" ((ffiVmaGetPoolName) (allocator) (pool) (pPpName))
  ppName <- lift $ peek @(Ptr CChar) pPpName
  pure $ (ppName)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaSetPoolName" ffiVmaSetPoolName
  :: Allocator -> Pool -> Ptr CChar -> IO ()

-- | Sets name of a custom pool.
--
-- @pName@ can be either null or pointer to a null-terminated string with
-- new name for the pool. Function makes internal copy of the string, so it
-- can be changed or freed immediately after this call.
setPoolName :: forall io
             . (MonadIO io)
            => -- No documentation found for Nested "vmaSetPoolName" "allocator"
               Allocator
            -> -- No documentation found for Nested "vmaSetPoolName" "pool"
               Pool
            -> -- No documentation found for Nested "vmaSetPoolName" "pName"
               ("name" ::: Maybe ByteString)
            -> io ()
setPoolName allocator pool name = liftIO . evalContT $ do
  pName <- case (name) of
    Nothing -> pure nullPtr
    Just j -> ContT $ useAsCString (j)
  lift $ traceAroundEvent "vmaSetPoolName" ((ffiVmaSetPoolName) (allocator) (pool) pName)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaAllocateMemory" ffiVmaAllocateMemory
  :: Allocator -> Ptr MemoryRequirements -> Ptr AllocationCreateInfo -> Ptr Allocation -> Ptr AllocationInfo -> IO Result

-- | General purpose memory allocation.
--
-- __Parameters__
--
-- +-----------+-----------------+-----------------------------------------------+
-- | out       | pAllocation     | Handle to allocated memory.                   |
-- +-----------+-----------------+-----------------------------------------------+
-- | out       | pAllocationInfo | Optional. Information about allocated memory. |
-- |           |                 | It can be later fetched using function        |
-- |           |                 | 'getAllocationInfo'.                          |
-- +-----------+-----------------+-----------------------------------------------+
--
-- You should free the memory using 'freeMemory' or 'freeMemoryPages'.
--
-- It is recommended to use 'allocateMemoryForBuffer',
-- 'allocateMemoryForImage', 'createBuffer', 'createImage' instead whenever
-- possible.
allocateMemory :: forall io
                . (MonadIO io)
               => -- No documentation found for Nested "vmaAllocateMemory" "allocator"
                  Allocator
               -> -- No documentation found for Nested "vmaAllocateMemory" "pVkMemoryRequirements"
                  ("vkMemoryRequirements" ::: MemoryRequirements)
               -> -- No documentation found for Nested "vmaAllocateMemory" "pCreateInfo"
                  AllocationCreateInfo
               -> io (Allocation, AllocationInfo)
allocateMemory allocator vkMemoryRequirements createInfo = liftIO . evalContT $ do
  pVkMemoryRequirements <- ContT $ withCStruct (vkMemoryRequirements)
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pPAllocation <- ContT $ bracket (callocBytes @Allocation 8) free
  pPAllocationInfo <- ContT (withZeroCStruct @AllocationInfo)
  r <- lift $ traceAroundEvent "vmaAllocateMemory" ((ffiVmaAllocateMemory) (allocator) pVkMemoryRequirements pCreateInfo (pPAllocation) (pPAllocationInfo))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pAllocation <- lift $ peek @Allocation pPAllocation
  pAllocationInfo <- lift $ peekCStruct @AllocationInfo pPAllocationInfo
  pure $ (pAllocation, pAllocationInfo)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'allocateMemory' and 'freeMemory'
--
-- To ensure that 'freeMemory' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withMemory :: forall io r . MonadIO io => Allocator -> MemoryRequirements -> AllocationCreateInfo -> (io (Allocation, AllocationInfo) -> ((Allocation, AllocationInfo) -> io ()) -> r) -> r
withMemory allocator pVkMemoryRequirements pCreateInfo b =
  b (allocateMemory allocator pVkMemoryRequirements pCreateInfo)
    (\(o0, _) -> freeMemory allocator o0)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaAllocateMemoryPages" ffiVmaAllocateMemoryPages
  :: Allocator -> Ptr MemoryRequirements -> Ptr AllocationCreateInfo -> CSize -> Ptr Allocation -> Ptr AllocationInfo -> IO Result

-- | General purpose memory allocation for multiple allocation objects at
-- once.
--
-- __Parameters__
--
-- +-----------+-----------------------+-----------------------------------------------+
-- |           | allocator             | Allocator object.                             |
-- +-----------+-----------------------+-----------------------------------------------+
-- |           | pVkMemoryRequirements | Memory requirements for each allocation.      |
-- +-----------+-----------------------+-----------------------------------------------+
-- |           | pCreateInfo           | Creation parameters for each alloction.       |
-- +-----------+-----------------------+-----------------------------------------------+
-- |           | allocationCount       | Number of allocations to make.                |
-- +-----------+-----------------------+-----------------------------------------------+
-- | out       | pAllocations          | Pointer to array that will be filled with     |
-- |           |                       | handles to created allocations.               |
-- +-----------+-----------------------+-----------------------------------------------+
-- | out       | pAllocationInfo       | Optional. Pointer to array that will be       |
-- |           |                       | filled with parameters of created             |
-- |           |                       | allocations.                                  |
-- +-----------+-----------------------+-----------------------------------------------+
--
-- You should free the memory using 'freeMemory' or 'freeMemoryPages'.
--
-- Word \"pages\" is just a suggestion to use this function to allocate
-- pieces of memory needed for sparse binding. It is just a general purpose
-- allocation function able to make multiple allocations at once. It may be
-- internally optimized to be more efficient than calling 'allocateMemory'
-- @allocationCount@ times.
--
-- All allocations are made using same parameters. All of them are created
-- out of the same memory pool and type. If any allocation fails, all
-- allocations already made within this function call are also freed, so
-- that when returned result is not @VK_SUCCESS@, @pAllocation@ array is
-- always entirely filled with @VK_NULL_HANDLE@.
allocateMemoryPages :: forall io
                     . (MonadIO io)
                    => -- No documentation found for Nested "vmaAllocateMemoryPages" "allocator"
                       Allocator
                    -> -- No documentation found for Nested "vmaAllocateMemoryPages" "pVkMemoryRequirements"
                       ("vkMemoryRequirements" ::: Vector MemoryRequirements)
                    -> -- No documentation found for Nested "vmaAllocateMemoryPages" "pCreateInfo"
                       ("createInfo" ::: Vector AllocationCreateInfo)
                    -> io (("allocations" ::: Vector Allocation), ("allocationInfo" ::: Vector AllocationInfo))
allocateMemoryPages allocator vkMemoryRequirements createInfo = liftIO . evalContT $ do
  pPVkMemoryRequirements <- ContT $ allocaBytesAligned @MemoryRequirements ((Data.Vector.length (vkMemoryRequirements)) * 24) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPVkMemoryRequirements `plusPtr` (24 * (i)) :: Ptr MemoryRequirements) (e) . ($ ())) (vkMemoryRequirements)
  pPCreateInfo <- ContT $ allocaBytesAligned @AllocationCreateInfo ((Data.Vector.length (createInfo)) * 40) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPCreateInfo `plusPtr` (40 * (i)) :: Ptr AllocationCreateInfo) (e)) (createInfo)
  let pVkMemoryRequirementsLength = Data.Vector.length $ (vkMemoryRequirements)
  lift $ unless ((Data.Vector.length $ (createInfo)) == pVkMemoryRequirementsLength) $
    throwIO $ IOError Nothing InvalidArgument "" "pCreateInfo and pVkMemoryRequirements must have the same length" Nothing Nothing
  pPAllocations <- ContT $ bracket (callocBytes @Allocation ((fromIntegral ((fromIntegral pVkMemoryRequirementsLength :: CSize))) * 8)) free
  pPAllocationInfo <- ContT $ bracket (callocBytes @AllocationInfo ((fromIntegral ((fromIntegral pVkMemoryRequirementsLength :: CSize))) * 48)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPAllocationInfo `advancePtrBytes` (i * 48) :: Ptr AllocationInfo) . ($ ())) [0..(fromIntegral ((fromIntegral pVkMemoryRequirementsLength :: CSize))) - 1]
  r <- lift $ traceAroundEvent "vmaAllocateMemoryPages" ((ffiVmaAllocateMemoryPages) (allocator) (pPVkMemoryRequirements) (pPCreateInfo) ((fromIntegral pVkMemoryRequirementsLength :: CSize)) (pPAllocations) ((pPAllocationInfo)))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pAllocations <- lift $ generateM (fromIntegral ((fromIntegral pVkMemoryRequirementsLength :: CSize))) (\i -> peek @Allocation ((pPAllocations `advancePtrBytes` (8 * (i)) :: Ptr Allocation)))
  pAllocationInfo <- lift $ generateM (fromIntegral ((fromIntegral pVkMemoryRequirementsLength :: CSize))) (\i -> peekCStruct @AllocationInfo (((pPAllocationInfo) `advancePtrBytes` (48 * (i)) :: Ptr AllocationInfo)))
  pure $ (pAllocations, pAllocationInfo)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'allocateMemoryPages' and 'freeMemoryPages'
--
-- To ensure that 'freeMemoryPages' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withMemoryPages :: forall io r . MonadIO io => Allocator -> Vector MemoryRequirements -> Vector AllocationCreateInfo -> (io (Vector Allocation, Vector AllocationInfo) -> ((Vector Allocation, Vector AllocationInfo) -> io ()) -> r) -> r
withMemoryPages allocator pVkMemoryRequirements pCreateInfo b =
  b (allocateMemoryPages allocator pVkMemoryRequirements pCreateInfo)
    (\(o0, _) -> freeMemoryPages allocator o0)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaAllocateMemoryForBuffer" ffiVmaAllocateMemoryForBuffer
  :: Allocator -> Buffer -> Ptr AllocationCreateInfo -> Ptr Allocation -> Ptr AllocationInfo -> IO Result

-- | __Parameters__
--
-- +-----------+-----------------+-----------------------------------------------+
-- | out       | pAllocation     | Handle to allocated memory.                   |
-- +-----------+-----------------+-----------------------------------------------+
-- | out       | pAllocationInfo | Optional. Information about allocated memory. |
-- |           |                 | It can be later fetched using function        |
-- |           |                 | 'getAllocationInfo'.                          |
-- +-----------+-----------------+-----------------------------------------------+
--
-- You should free the memory using 'freeMemory'.
allocateMemoryForBuffer :: forall io
                         . (MonadIO io)
                        => -- No documentation found for Nested "vmaAllocateMemoryForBuffer" "allocator"
                           Allocator
                        -> -- No documentation found for Nested "vmaAllocateMemoryForBuffer" "buffer"
                           Buffer
                        -> -- No documentation found for Nested "vmaAllocateMemoryForBuffer" "pCreateInfo"
                           AllocationCreateInfo
                        -> io (Allocation, AllocationInfo)
allocateMemoryForBuffer allocator buffer createInfo = liftIO . evalContT $ do
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pPAllocation <- ContT $ bracket (callocBytes @Allocation 8) free
  pPAllocationInfo <- ContT (withZeroCStruct @AllocationInfo)
  r <- lift $ traceAroundEvent "vmaAllocateMemoryForBuffer" ((ffiVmaAllocateMemoryForBuffer) (allocator) (buffer) pCreateInfo (pPAllocation) (pPAllocationInfo))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pAllocation <- lift $ peek @Allocation pPAllocation
  pAllocationInfo <- lift $ peekCStruct @AllocationInfo pPAllocationInfo
  pure $ (pAllocation, pAllocationInfo)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'allocateMemoryForBuffer' and 'freeMemory'
--
-- To ensure that 'freeMemory' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withMemoryForBuffer :: forall io r . MonadIO io => Allocator -> Buffer -> AllocationCreateInfo -> (io (Allocation, AllocationInfo) -> ((Allocation, AllocationInfo) -> io ()) -> r) -> r
withMemoryForBuffer allocator buffer pCreateInfo b =
  b (allocateMemoryForBuffer allocator buffer pCreateInfo)
    (\(o0, _) -> freeMemory allocator o0)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaAllocateMemoryForImage" ffiVmaAllocateMemoryForImage
  :: Allocator -> Image -> Ptr AllocationCreateInfo -> Ptr Allocation -> Ptr AllocationInfo -> IO Result

-- | Function similar to 'allocateMemoryForBuffer'.
allocateMemoryForImage :: forall io
                        . (MonadIO io)
                       => -- No documentation found for Nested "vmaAllocateMemoryForImage" "allocator"
                          Allocator
                       -> -- No documentation found for Nested "vmaAllocateMemoryForImage" "image"
                          Image
                       -> -- No documentation found for Nested "vmaAllocateMemoryForImage" "pCreateInfo"
                          AllocationCreateInfo
                       -> io (Allocation, AllocationInfo)
allocateMemoryForImage allocator image createInfo = liftIO . evalContT $ do
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pPAllocation <- ContT $ bracket (callocBytes @Allocation 8) free
  pPAllocationInfo <- ContT (withZeroCStruct @AllocationInfo)
  r <- lift $ traceAroundEvent "vmaAllocateMemoryForImage" ((ffiVmaAllocateMemoryForImage) (allocator) (image) pCreateInfo (pPAllocation) (pPAllocationInfo))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pAllocation <- lift $ peek @Allocation pPAllocation
  pAllocationInfo <- lift $ peekCStruct @AllocationInfo pPAllocationInfo
  pure $ (pAllocation, pAllocationInfo)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'allocateMemoryForImage' and 'freeMemory'
--
-- To ensure that 'freeMemory' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withMemoryForImage :: forall io r . MonadIO io => Allocator -> Image -> AllocationCreateInfo -> (io (Allocation, AllocationInfo) -> ((Allocation, AllocationInfo) -> io ()) -> r) -> r
withMemoryForImage allocator image pCreateInfo b =
  b (allocateMemoryForImage allocator image pCreateInfo)
    (\(o0, _) -> freeMemory allocator o0)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaFreeMemory" ffiVmaFreeMemory
  :: Allocator -> Allocation -> IO ()

-- | Frees memory previously allocated using 'allocateMemory',
-- 'allocateMemoryForBuffer', or 'allocateMemoryForImage'.
--
-- Passing @VK_NULL_HANDLE@ as @allocation@ is valid. Such function call is
-- just skipped.
freeMemory :: forall io
            . (MonadIO io)
           => -- No documentation found for Nested "vmaFreeMemory" "allocator"
              Allocator
           -> -- No documentation found for Nested "vmaFreeMemory" "allocation"
              Allocation
           -> io ()
freeMemory allocator allocation = liftIO $ do
  traceAroundEvent "vmaFreeMemory" ((ffiVmaFreeMemory) (allocator) (allocation))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaFreeMemoryPages" ffiVmaFreeMemoryPages
  :: Allocator -> CSize -> Ptr Allocation -> IO ()

-- | Frees memory and destroys multiple allocations.
--
-- Word \"pages\" is just a suggestion to use this function to free pieces
-- of memory used for sparse binding. It is just a general purpose function
-- to free memory and destroy allocations made using e.g. 'allocateMemory',
-- 'allocateMemoryPages' and other functions. It may be internally
-- optimized to be more efficient than calling 'freeMemory'
-- @allocationCount@ times.
--
-- Allocations in @pAllocations@ array can come from any memory pools and
-- types. Passing @VK_NULL_HANDLE@ as elements of @pAllocations@ array is
-- valid. Such entries are just skipped.
freeMemoryPages :: forall io
                 . (MonadIO io)
                => -- No documentation found for Nested "vmaFreeMemoryPages" "allocator"
                   Allocator
                -> -- No documentation found for Nested "vmaFreeMemoryPages" "pAllocations"
                   ("allocations" ::: Vector Allocation)
                -> io ()
freeMemoryPages allocator allocations = liftIO . evalContT $ do
  pPAllocations <- ContT $ allocaBytesAligned @Allocation ((Data.Vector.length (allocations)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPAllocations `plusPtr` (8 * (i)) :: Ptr Allocation) (e)) (allocations)
  lift $ traceAroundEvent "vmaFreeMemoryPages" ((ffiVmaFreeMemoryPages) (allocator) ((fromIntegral (Data.Vector.length $ (allocations)) :: CSize)) (pPAllocations))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaResizeAllocation" ffiVmaResizeAllocation
  :: Allocator -> Allocation -> DeviceSize -> IO Result

-- | Deprecated.
--
-- /Deprecated/
--
-- In version 2.2.0 it used to try to change allocation\'s size without
-- moving or reallocating it. In current version it returns @VK_SUCCESS@
-- only if @newSize@ equals current allocation\'s size. Otherwise returns
-- @VK_ERROR_OUT_OF_POOL_MEMORY@, indicating that allocation\'s size could
-- not be changed.
resizeAllocation :: forall io
                  . (MonadIO io)
                 => -- No documentation found for Nested "vmaResizeAllocation" "allocator"
                    Allocator
                 -> -- No documentation found for Nested "vmaResizeAllocation" "allocation"
                    Allocation
                 -> -- No documentation found for Nested "vmaResizeAllocation" "newSize"
                    ("newSize" ::: DeviceSize)
                 -> io ()
resizeAllocation allocator allocation newSize = liftIO $ do
  r <- traceAroundEvent "vmaResizeAllocation" ((ffiVmaResizeAllocation) (allocator) (allocation) (newSize))
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaGetAllocationInfo" ffiVmaGetAllocationInfo
  :: Allocator -> Allocation -> Ptr AllocationInfo -> IO ()

-- | Returns current information about specified allocation and atomically
-- marks it as used in current frame.
--
-- Current paramteres of given allocation are returned in
-- @pAllocationInfo@.
--
-- This function also atomically \"touches\" allocation - marks it as used
-- in current frame, just like 'touchAllocation'. If the allocation is in
-- lost state, @pAllocationInfo->deviceMemory == VK_NULL_HANDLE@.
--
-- Although this function uses atomics and doesn\'t lock any mutex, so it
-- should be quite efficient, you can avoid calling it too often.
--
-- -   You can retrieve same 'AllocationInfo' structure while creating your
--     resource, from function 'createBuffer', 'createImage'. You can
--     remember it if you are sure parameters don\'t change (e.g. due to
--     defragmentation or allocation becoming lost).
--
-- -   If you just want to check if allocation is not lost,
--     'touchAllocation' will work faster.
getAllocationInfo :: forall io
                   . (MonadIO io)
                  => -- No documentation found for Nested "vmaGetAllocationInfo" "allocator"
                     Allocator
                  -> -- No documentation found for Nested "vmaGetAllocationInfo" "allocation"
                     Allocation
                  -> io (AllocationInfo)
getAllocationInfo allocator allocation = liftIO . evalContT $ do
  pPAllocationInfo <- ContT (withZeroCStruct @AllocationInfo)
  lift $ traceAroundEvent "vmaGetAllocationInfo" ((ffiVmaGetAllocationInfo) (allocator) (allocation) (pPAllocationInfo))
  pAllocationInfo <- lift $ peekCStruct @AllocationInfo pPAllocationInfo
  pure $ (pAllocationInfo)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaTouchAllocation" ffiVmaTouchAllocation
  :: Allocator -> Allocation -> IO Bool32

-- | Returns @VK_TRUE@ if allocation is not lost and atomically marks it as
-- used in current frame.
--
-- If the allocation has been created with
-- 'ALLOCATION_CREATE_CAN_BECOME_LOST_BIT' flag, this function returns
-- @VK_TRUE@ if it\'s not in lost state, so it can still be used. It then
-- also atomically \"touches\" the allocation - marks it as used in current
-- frame, so that you can be sure it won\'t become lost in current frame or
-- next @frameInUseCount@ frames.
--
-- If the allocation is in lost state, the function returns @VK_FALSE@.
-- Memory of such allocation, as well as buffer or image bound to it,
-- should not be used. Lost allocation and the buffer\/image still need to
-- be destroyed.
--
-- If the allocation has been created without
-- 'ALLOCATION_CREATE_CAN_BECOME_LOST_BIT' flag, this function always
-- returns @VK_TRUE@.
touchAllocation :: forall io
                 . (MonadIO io)
                => -- No documentation found for Nested "vmaTouchAllocation" "allocator"
                   Allocator
                -> -- No documentation found for Nested "vmaTouchAllocation" "allocation"
                   Allocation
                -> io (Bool)
touchAllocation allocator allocation = liftIO $ do
  r <- traceAroundEvent "vmaTouchAllocation" ((ffiVmaTouchAllocation) (allocator) (allocation))
  pure $ ((bool32ToBool r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaSetAllocationUserData" ffiVmaSetAllocationUserData
  :: Allocator -> Allocation -> Ptr () -> IO ()

-- | Sets pUserData in given allocation to new value.
--
-- If the allocation was created with
-- VMA_ALLOCATION_CREATE_USER_DATA_COPY_STRING_BIT, pUserData must be
-- either null, or pointer to a null-terminated string. The function makes
-- local copy of the string and sets it as allocation\'s @pUserData@.
-- String passed as pUserData doesn\'t need to be valid for whole lifetime
-- of the allocation - you can free it after this call. String previously
-- pointed by allocation\'s pUserData is freed from memory.
--
-- If the flag was not used, the value of pointer @pUserData@ is just
-- copied to allocation\'s @pUserData@. It is opaque, so you can use it
-- however you want - e.g. as a pointer, ordinal number or some handle to
-- you own data.
setAllocationUserData :: forall io
                       . (MonadIO io)
                      => -- No documentation found for Nested "vmaSetAllocationUserData" "allocator"
                         Allocator
                      -> -- No documentation found for Nested "vmaSetAllocationUserData" "allocation"
                         Allocation
                      -> -- No documentation found for Nested "vmaSetAllocationUserData" "pUserData"
                         ("userData" ::: Ptr ())
                      -> io ()
setAllocationUserData allocator allocation userData = liftIO $ do
  traceAroundEvent "vmaSetAllocationUserData" ((ffiVmaSetAllocationUserData) (allocator) (allocation) (userData))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaCreateLostAllocation" ffiVmaCreateLostAllocation
  :: Allocator -> Ptr Allocation -> IO ()

-- | Creates new allocation that is in lost state from the beginning.
--
-- It can be useful if you need a dummy, non-null allocation.
--
-- You still need to destroy created object using 'freeMemory'.
--
-- Returned allocation is not tied to any specific memory pool or memory
-- type and not bound to any image or buffer. It has size = 0. It cannot be
-- turned into a real, non-empty allocation.
createLostAllocation :: forall io
                      . (MonadIO io)
                     => -- No documentation found for Nested "vmaCreateLostAllocation" "allocator"
                        Allocator
                     -> io (Allocation)
createLostAllocation allocator = liftIO . evalContT $ do
  pPAllocation <- ContT $ bracket (callocBytes @Allocation 8) free
  lift $ traceAroundEvent "vmaCreateLostAllocation" ((ffiVmaCreateLostAllocation) (allocator) (pPAllocation))
  pAllocation <- lift $ peek @Allocation pPAllocation
  pure $ (pAllocation)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createLostAllocation' and 'freeMemory'
--
-- To ensure that 'freeMemory' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withLostAllocation :: forall io r . MonadIO io => Allocator -> (io Allocation -> (Allocation -> io ()) -> r) -> r
withLostAllocation allocator b =
  b (createLostAllocation allocator)
    (\(o0) -> freeMemory allocator o0)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaMapMemory" ffiVmaMapMemory
  :: Allocator -> Allocation -> Ptr (Ptr ()) -> IO Result

-- | Maps memory represented by given allocation and returns pointer to it.
--
-- Maps memory represented by given allocation to make it accessible to CPU
-- code. When succeeded, @*ppData@ contains pointer to first byte of this
-- memory. If the allocation is part of bigger @VkDeviceMemory@ block, the
-- pointer is correctly offseted to the beginning of region assigned to
-- this particular allocation.
--
-- Mapping is internally reference-counted and synchronized, so despite raw
-- Vulkan function @vkMapMemory()@ cannot be used to map same block of
-- @VkDeviceMemory@ multiple times simultaneously, it is safe to call this
-- function on allocations assigned to the same memory block. Actual Vulkan
-- memory will be mapped on first mapping and unmapped on last unmapping.
--
-- If the function succeeded, you must call 'unmapMemory' to unmap the
-- allocation when mapping is no longer needed or before freeing the
-- allocation, at the latest.
--
-- It also safe to call this function multiple times on the same
-- allocation. You must call 'unmapMemory' same number of times as you
-- called 'mapMemory'.
--
-- It is also safe to call this function on allocation created with
-- 'ALLOCATION_CREATE_MAPPED_BIT' flag. Its memory stays mapped all the
-- time. You must still call 'unmapMemory' same number of times as you
-- called 'mapMemory'. You must not call 'unmapMemory' additional time to
-- free the \"0-th\" mapping made automatically due to
-- 'ALLOCATION_CREATE_MAPPED_BIT' flag.
--
-- This function fails when used on allocation made in memory type that is
-- not @HOST_VISIBLE@.
--
-- This function always fails when called for allocation that was created
-- with 'ALLOCATION_CREATE_CAN_BECOME_LOST_BIT' flag. Such allocations
-- cannot be mapped.
--
-- This function doesn\'t automatically flush or invalidate caches. If the
-- allocation is made from a memory types that is not @HOST_COHERENT@, you
-- also need to use 'invalidateAllocation' \/ 'flushAllocation', as
-- required by Vulkan specification.
mapMemory :: forall io
           . (MonadIO io)
          => -- No documentation found for Nested "vmaMapMemory" "allocator"
             Allocator
          -> -- No documentation found for Nested "vmaMapMemory" "allocation"
             Allocation
          -> io (("data" ::: Ptr ()))
mapMemory allocator allocation = liftIO . evalContT $ do
  pPpData <- ContT $ bracket (callocBytes @(Ptr ()) 8) free
  r <- lift $ traceAroundEvent "vmaMapMemory" ((ffiVmaMapMemory) (allocator) (allocation) (pPpData))
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
withMappedMemory :: forall io r . MonadIO io => Allocator -> Allocation -> (io (Ptr ()) -> (Ptr () -> io ()) -> r) -> r
withMappedMemory allocator allocation b =
  b (mapMemory allocator allocation)
    (\(_) -> unmapMemory allocator allocation)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaUnmapMemory" ffiVmaUnmapMemory
  :: Allocator -> Allocation -> IO ()

-- | Unmaps memory represented by given allocation, mapped previously using
-- 'mapMemory'.
--
-- For details, see description of 'mapMemory'.
--
-- This function doesn\'t automatically flush or invalidate caches. If the
-- allocation is made from a memory types that is not @HOST_COHERENT@, you
-- also need to use 'invalidateAllocation' \/ 'flushAllocation', as
-- required by Vulkan specification.
unmapMemory :: forall io
             . (MonadIO io)
            => -- No documentation found for Nested "vmaUnmapMemory" "allocator"
               Allocator
            -> -- No documentation found for Nested "vmaUnmapMemory" "allocation"
               Allocation
            -> io ()
unmapMemory allocator allocation = liftIO $ do
  traceAroundEvent "vmaUnmapMemory" ((ffiVmaUnmapMemory) (allocator) (allocation))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaFlushAllocation" ffiVmaFlushAllocation
  :: Allocator -> Allocation -> DeviceSize -> DeviceSize -> IO Result

-- | Flushes memory of given allocation.
--
-- Calls @vkFlushMappedMemoryRanges()@ for memory associated with given
-- range of given allocation. It needs to be called after writing to a
-- mapped memory for memory types that are not @HOST_COHERENT@. Unmap
-- operation doesn\'t do that automatically.
--
-- -   @offset@ must be relative to the beginning of allocation.
--
-- -   @size@ can be @VK_WHOLE_SIZE@. It means all memory from @offset@ the
--     the end of given allocation.
--
-- -   @offset@ and @size@ don\'t have to be aligned. They are internally
--     rounded down\/up to multiply of @nonCoherentAtomSize@.
--
-- -   If @size@ is 0, this call is ignored.
--
-- -   If memory type that the @allocation@ belongs to is not
--     @HOST_VISIBLE@ or it is @HOST_COHERENT@, this call is ignored.
--
-- Warning! @offset@ and @size@ are relative to the contents of given
-- @allocation@. If you mean whole allocation, you can pass 0 and
-- @VK_WHOLE_SIZE@, respectively. Do not pass allocation\'s offset as
-- @offset@!!!
--
-- This function returns the @VkResult@ from @vkFlushMappedMemoryRanges@ if
-- it is called, otherwise @VK_SUCCESS@.
flushAllocation :: forall io
                 . (MonadIO io)
                => -- No documentation found for Nested "vmaFlushAllocation" "allocator"
                   Allocator
                -> -- No documentation found for Nested "vmaFlushAllocation" "allocation"
                   Allocation
                -> -- No documentation found for Nested "vmaFlushAllocation" "offset"
                   ("offset" ::: DeviceSize)
                -> -- No documentation found for Nested "vmaFlushAllocation" "size"
                   DeviceSize
                -> io ()
flushAllocation allocator allocation offset size = liftIO $ do
  r <- traceAroundEvent "vmaFlushAllocation" ((ffiVmaFlushAllocation) (allocator) (allocation) (offset) (size))
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaInvalidateAllocation" ffiVmaInvalidateAllocation
  :: Allocator -> Allocation -> DeviceSize -> DeviceSize -> IO Result

-- | Invalidates memory of given allocation.
--
-- Calls @vkInvalidateMappedMemoryRanges()@ for memory associated with
-- given range of given allocation. It needs to be called before reading
-- from a mapped memory for memory types that are not @HOST_COHERENT@. Map
-- operation doesn\'t do that automatically.
--
-- -   @offset@ must be relative to the beginning of allocation.
--
-- -   @size@ can be @VK_WHOLE_SIZE@. It means all memory from @offset@ the
--     the end of given allocation.
--
-- -   @offset@ and @size@ don\'t have to be aligned. They are internally
--     rounded down\/up to multiply of @nonCoherentAtomSize@.
--
-- -   If @size@ is 0, this call is ignored.
--
-- -   If memory type that the @allocation@ belongs to is not
--     @HOST_VISIBLE@ or it is @HOST_COHERENT@, this call is ignored.
--
-- Warning! @offset@ and @size@ are relative to the contents of given
-- @allocation@. If you mean whole allocation, you can pass 0 and
-- @VK_WHOLE_SIZE@, respectively. Do not pass allocation\'s offset as
-- @offset@!!!
--
-- This function returns the @VkResult@ from
-- @vkInvalidateMappedMemoryRanges@ if it is called, otherwise
-- @VK_SUCCESS@.
invalidateAllocation :: forall io
                      . (MonadIO io)
                     => -- No documentation found for Nested "vmaInvalidateAllocation" "allocator"
                        Allocator
                     -> -- No documentation found for Nested "vmaInvalidateAllocation" "allocation"
                        Allocation
                     -> -- No documentation found for Nested "vmaInvalidateAllocation" "offset"
                        ("offset" ::: DeviceSize)
                     -> -- No documentation found for Nested "vmaInvalidateAllocation" "size"
                        DeviceSize
                     -> io ()
invalidateAllocation allocator allocation offset size = liftIO $ do
  r <- traceAroundEvent "vmaInvalidateAllocation" ((ffiVmaInvalidateAllocation) (allocator) (allocation) (offset) (size))
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaFlushAllocations" ffiVmaFlushAllocations
  :: Allocator -> Word32 -> Ptr Allocation -> Ptr DeviceSize -> Ptr DeviceSize -> IO Result

-- | Flushes memory of given set of allocations.
--
-- Calls @vkFlushMappedMemoryRanges()@ for memory associated with given
-- ranges of given allocations. For more information, see documentation of
-- 'flushAllocation'.
--
-- __Parameters__
--
-- +-----------------+--------------------------------------------------------+
-- | allocator       |                                                        |
-- +-----------------+--------------------------------------------------------+
-- | allocationCount |                                                        |
-- +-----------------+--------------------------------------------------------+
-- | allocations     |                                                        |
-- +-----------------+--------------------------------------------------------+
-- | offsets         | If not null, it must point to an array of offsets of   |
-- |                 | regions to flush, relative to the beginning of         |
-- |                 | respective allocations. Null means all ofsets are      |
-- |                 | zero.                                                  |
-- +-----------------+--------------------------------------------------------+
-- | sizes           | If not null, it must point to an array of sizes of     |
-- |                 | regions to flush in respective allocations. Null means |
-- |                 | @VK_WHOLE_SIZE@ for all allocations.                   |
-- +-----------------+--------------------------------------------------------+
--
-- This function returns the @VkResult@ from @vkFlushMappedMemoryRanges@ if
-- it is called, otherwise @VK_SUCCESS@.
flushAllocations :: forall io
                  . (MonadIO io)
                 => -- No documentation found for Nested "vmaFlushAllocations" "allocator"
                    Allocator
                 -> -- No documentation found for Nested "vmaFlushAllocations" "allocations"
                    ("allocations" ::: Vector Allocation)
                 -> -- No documentation found for Nested "vmaFlushAllocations" "offsets"
                    ("offsets" ::: Vector DeviceSize)
                 -> -- No documentation found for Nested "vmaFlushAllocations" "sizes"
                    ("sizes" ::: Vector DeviceSize)
                 -> io ()
flushAllocations allocator allocations offsets sizes = liftIO . evalContT $ do
  let allocationsLength = Data.Vector.length $ (allocations)
  let offsetsLength = Data.Vector.length $ (offsets)
  lift $ unless (fromIntegral offsetsLength == allocationsLength || offsetsLength == 0) $
    throwIO $ IOError Nothing InvalidArgument "" "offsets and allocations must have the same length" Nothing Nothing
  let sizesLength = Data.Vector.length $ (sizes)
  lift $ unless (fromIntegral sizesLength == allocationsLength || sizesLength == 0) $
    throwIO $ IOError Nothing InvalidArgument "" "sizes and allocations must have the same length" Nothing Nothing
  pAllocations <- ContT $ allocaBytesAligned @Allocation ((Data.Vector.length (allocations)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pAllocations `plusPtr` (8 * (i)) :: Ptr Allocation) (e)) (allocations)
  offsets' <- if Data.Vector.null (offsets)
    then pure nullPtr
    else do
      pOffsets <- ContT $ allocaBytesAligned @DeviceSize (((Data.Vector.length (offsets))) * 8) 8
      lift $ Data.Vector.imapM_ (\i e -> poke (pOffsets `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) ((offsets))
      pure $ pOffsets
  sizes' <- if Data.Vector.null (sizes)
    then pure nullPtr
    else do
      pSizes <- ContT $ allocaBytesAligned @DeviceSize (((Data.Vector.length (sizes))) * 8) 8
      lift $ Data.Vector.imapM_ (\i e -> poke (pSizes `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) ((sizes))
      pure $ pSizes
  r <- lift $ traceAroundEvent "vmaFlushAllocations" ((ffiVmaFlushAllocations) (allocator) ((fromIntegral allocationsLength :: Word32)) (pAllocations) offsets' sizes')
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaInvalidateAllocations" ffiVmaInvalidateAllocations
  :: Allocator -> Word32 -> Ptr Allocation -> Ptr DeviceSize -> Ptr DeviceSize -> IO Result

-- | Invalidates memory of given set of allocations.
--
-- Calls @vkInvalidateMappedMemoryRanges()@ for memory associated with
-- given ranges of given allocations. For more information, see
-- documentation of 'invalidateAllocation'.
--
-- __Parameters__
--
-- +-----------------+--------------------------------------------------------+
-- | allocator       |                                                        |
-- +-----------------+--------------------------------------------------------+
-- | allocationCount |                                                        |
-- +-----------------+--------------------------------------------------------+
-- | allocations     |                                                        |
-- +-----------------+--------------------------------------------------------+
-- | offsets         | If not null, it must point to an array of offsets of   |
-- |                 | regions to flush, relative to the beginning of         |
-- |                 | respective allocations. Null means all ofsets are      |
-- |                 | zero.                                                  |
-- +-----------------+--------------------------------------------------------+
-- | sizes           | If not null, it must point to an array of sizes of     |
-- |                 | regions to flush in respective allocations. Null means |
-- |                 | @VK_WHOLE_SIZE@ for all allocations.                   |
-- +-----------------+--------------------------------------------------------+
--
-- This function returns the @VkResult@ from
-- @vkInvalidateMappedMemoryRanges@ if it is called, otherwise
-- @VK_SUCCESS@.
invalidateAllocations :: forall io
                       . (MonadIO io)
                      => -- No documentation found for Nested "vmaInvalidateAllocations" "allocator"
                         Allocator
                      -> -- No documentation found for Nested "vmaInvalidateAllocations" "allocations"
                         ("allocations" ::: Vector Allocation)
                      -> -- No documentation found for Nested "vmaInvalidateAllocations" "offsets"
                         ("offsets" ::: Vector DeviceSize)
                      -> -- No documentation found for Nested "vmaInvalidateAllocations" "sizes"
                         ("sizes" ::: Vector DeviceSize)
                      -> io ()
invalidateAllocations allocator allocations offsets sizes = liftIO . evalContT $ do
  let allocationsLength = Data.Vector.length $ (allocations)
  let offsetsLength = Data.Vector.length $ (offsets)
  lift $ unless (fromIntegral offsetsLength == allocationsLength || offsetsLength == 0) $
    throwIO $ IOError Nothing InvalidArgument "" "offsets and allocations must have the same length" Nothing Nothing
  let sizesLength = Data.Vector.length $ (sizes)
  lift $ unless (fromIntegral sizesLength == allocationsLength || sizesLength == 0) $
    throwIO $ IOError Nothing InvalidArgument "" "sizes and allocations must have the same length" Nothing Nothing
  pAllocations <- ContT $ allocaBytesAligned @Allocation ((Data.Vector.length (allocations)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pAllocations `plusPtr` (8 * (i)) :: Ptr Allocation) (e)) (allocations)
  offsets' <- if Data.Vector.null (offsets)
    then pure nullPtr
    else do
      pOffsets <- ContT $ allocaBytesAligned @DeviceSize (((Data.Vector.length (offsets))) * 8) 8
      lift $ Data.Vector.imapM_ (\i e -> poke (pOffsets `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) ((offsets))
      pure $ pOffsets
  sizes' <- if Data.Vector.null (sizes)
    then pure nullPtr
    else do
      pSizes <- ContT $ allocaBytesAligned @DeviceSize (((Data.Vector.length (sizes))) * 8) 8
      lift $ Data.Vector.imapM_ (\i e -> poke (pSizes `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) ((sizes))
      pure $ pSizes
  r <- lift $ traceAroundEvent "vmaInvalidateAllocations" ((ffiVmaInvalidateAllocations) (allocator) ((fromIntegral allocationsLength :: Word32)) (pAllocations) offsets' sizes')
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaCheckCorruption" ffiVmaCheckCorruption
  :: Allocator -> Word32 -> IO Result

-- | Checks magic number in margins around all allocations in given memory
-- types (in both default and custom pools) in search for corruptions.
--
-- __Parameters__
--
-- +----------------+--------------------------------------------------------+
-- | memoryTypeBits | Bit mask, where each bit set means that a memory type  |
-- |                | with that index should be checked.                     |
-- +----------------+--------------------------------------------------------+
--
-- Corruption detection is enabled only when @VMA_DEBUG_DETECT_CORRUPTION@
-- macro is defined to nonzero, @VMA_DEBUG_MARGIN@ is defined to nonzero
-- and only for memory types that are @HOST_VISIBLE@ and @HOST_COHERENT@.
-- For more information, see /Corruption detection/.
--
-- Possible return values:
--
-- -   @VK_ERROR_FEATURE_NOT_PRESENT@ - corruption detection is not enabled
--     for any of specified memory types.
--
-- -   @VK_SUCCESS@ - corruption detection has been performed and
--     succeeded.
--
-- -   @VK_ERROR_VALIDATION_FAILED_EXT@ - corruption detection has been
--     performed and found memory corruptions around one of the
--     allocations. @VMA_ASSERT@ is also fired in that case.
--
-- -   Other value: Error returned by Vulkan, e.g. memory mapping failure.
checkCorruption :: forall io
                 . (MonadIO io)
                => -- No documentation found for Nested "vmaCheckCorruption" "allocator"
                   Allocator
                -> -- No documentation found for Nested "vmaCheckCorruption" "memoryTypeBits"
                   ("memoryTypeBits" ::: Word32)
                -> io ()
checkCorruption allocator memoryTypeBits = liftIO $ do
  r <- traceAroundEvent "vmaCheckCorruption" ((ffiVmaCheckCorruption) (allocator) (memoryTypeBits))
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaDefragmentationBegin" ffiVmaDefragmentationBegin
  :: Allocator -> Ptr DefragmentationInfo2 -> Ptr DefragmentationStats -> Ptr DefragmentationContext -> IO Result

-- | Begins defragmentation process.
--
-- __Parameters__
--
-- +-----------+-----------+-----------------------------------------------+
-- |           | allocator | Allocator object.                             |
-- +-----------+-----------+-----------------------------------------------+
-- |           | pInfo     | Structure filled with parameters of           |
-- |           |           | defragmentation.                              |
-- +-----------+-----------+-----------------------------------------------+
-- | out       | pStats    | Optional. Statistics of defragmentation. You  |
-- |           |           | can pass null if you are not interested in    |
-- |           |           | this information.                             |
-- +-----------+-----------+-----------------------------------------------+
-- | out       | pContext  | Context object that must be passed to         |
-- |           |           | 'defragmentationEnd' to finish                |
-- |           |           | defragmentation.                              |
-- +-----------+-----------+-----------------------------------------------+
--
-- __Returns__
--
-- @VK_SUCCESS@ and @*pContext == null@ if defragmentation finished within
-- this function call. @VK_NOT_READY@ and @*pContext != null@ if
-- defragmentation has been started and you need to call
-- 'defragmentationEnd' to finish it. Negative value in case of error.
--
-- Use this function instead of old, deprecated 'defragment'.
--
-- Warning! Between the call to 'defragmentationBegin' and
-- 'defragmentationEnd':
--
-- -   You should not use any of allocations passed as
--     @pInfo->pAllocations@ or any allocations that belong to pools passed
--     as @pInfo->pPools@, including calling 'getAllocationInfo',
--     'touchAllocation', or access their data.
--
-- -   Some mutexes protecting internal data structures may be locked, so
--     trying to make or free any allocations, bind buffers or images, map
--     memory, or launch another simultaneous defragmentation in between
--     may cause stall (when done on another thread) or deadlock (when done
--     on the same thread), unless you are 100% sure that defragmented
--     allocations are in different pools.
--
-- -   Information returned via @pStats@ and @pInfo->pAllocationsChanged@
--     are undefined. They become valid after call to 'defragmentationEnd'.
--
-- -   If @pInfo->commandBuffer@ is not null, you must submit that command
--     buffer and make sure it finished execution before calling
--     'defragmentationEnd'.
--
-- For more information and important limitations regarding
-- defragmentation, see documentation chapter: /Defragmentation/.
defragmentationBegin :: forall io
                      . (MonadIO io)
                     => -- No documentation found for Nested "vmaDefragmentationBegin" "allocator"
                        Allocator
                     -> -- No documentation found for Nested "vmaDefragmentationBegin" "pInfo"
                        DefragmentationInfo2
                     -> io (Result, DefragmentationStats, DefragmentationContext)
defragmentationBegin allocator info = liftIO . evalContT $ do
  pInfo <- ContT $ withCStruct (info)
  pPStats <- ContT (withZeroCStruct @DefragmentationStats)
  pPContext <- ContT $ bracket (callocBytes @DefragmentationContext 8) free
  r <- lift $ traceAroundEvent "vmaDefragmentationBegin" ((ffiVmaDefragmentationBegin) (allocator) pInfo (pPStats) (pPContext))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pStats <- lift $ peekCStruct @DefragmentationStats pPStats
  pContext <- lift $ peek @DefragmentationContext pPContext
  pure $ (r, pStats, pContext)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'defragmentationBegin' and 'defragmentationEnd'
--
-- To ensure that 'defragmentationEnd' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withDefragmentation :: forall io r . MonadIO io => Allocator -> DefragmentationInfo2 -> (io (Result, DefragmentationStats, DefragmentationContext) -> ((Result, DefragmentationStats, DefragmentationContext) -> io ()) -> r) -> r
withDefragmentation allocator pInfo b =
  b (defragmentationBegin allocator pInfo)
    (\(_, _, o2) -> defragmentationEnd allocator o2)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaDefragmentationEnd" ffiVmaDefragmentationEnd
  :: Allocator -> DefragmentationContext -> IO Result

-- | Ends defragmentation process.
--
-- Use this function to finish defragmentation started by
-- 'defragmentationBegin'. It is safe to pass @context == null@. The
-- function then does nothing.
defragmentationEnd :: forall io
                    . (MonadIO io)
                   => -- No documentation found for Nested "vmaDefragmentationEnd" "allocator"
                      Allocator
                   -> -- No documentation found for Nested "vmaDefragmentationEnd" "context"
                      DefragmentationContext
                   -> io ()
defragmentationEnd allocator context = liftIO $ do
  r <- traceAroundEvent "vmaDefragmentationEnd" ((ffiVmaDefragmentationEnd) (allocator) (context))
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaBeginDefragmentationPass" ffiVmaBeginDefragmentationPass
  :: Allocator -> DefragmentationContext -> Ptr DefragmentationPassInfo -> IO Result


beginDefragmentationPass :: forall io
                          . (MonadIO io)
                         => -- No documentation found for Nested "vmaBeginDefragmentationPass" "allocator"
                            Allocator
                         -> -- No documentation found for Nested "vmaBeginDefragmentationPass" "context"
                            DefragmentationContext
                         -> io (DefragmentationPassInfo)
beginDefragmentationPass allocator context = liftIO . evalContT $ do
  pPInfo <- ContT (withZeroCStruct @DefragmentationPassInfo)
  r <- lift $ traceAroundEvent "vmaBeginDefragmentationPass" ((ffiVmaBeginDefragmentationPass) (allocator) (context) (pPInfo))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pInfo <- lift $ peekCStruct @DefragmentationPassInfo pPInfo
  pure $ (pInfo)

-- | This function will call the supplied action between calls to
-- 'beginDefragmentationPass' and 'endDefragmentationPass'
--
-- Note that 'endDefragmentationPass' is *not* called if an exception is
-- thrown by the inner action.
useDefragmentationPass :: forall io r . MonadIO io => Allocator -> DefragmentationContext -> (DefragmentationPassInfo -> io r) -> io r
useDefragmentationPass allocator context a =
  do
    x <- beginDefragmentationPass allocator context
    r <- a x
    (\(_) -> endDefragmentationPass allocator context) x
    pure r


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaEndDefragmentationPass" ffiVmaEndDefragmentationPass
  :: Allocator -> DefragmentationContext -> IO Result


endDefragmentationPass :: forall io
                        . (MonadIO io)
                       => -- No documentation found for Nested "vmaEndDefragmentationPass" "allocator"
                          Allocator
                       -> -- No documentation found for Nested "vmaEndDefragmentationPass" "context"
                          DefragmentationContext
                       -> io ()
endDefragmentationPass allocator context = liftIO $ do
  r <- traceAroundEvent "vmaEndDefragmentationPass" ((ffiVmaEndDefragmentationPass) (allocator) (context))
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaDefragment" ffiVmaDefragment
  :: Allocator -> Ptr Allocation -> CSize -> Ptr Bool32 -> Ptr DefragmentationInfo -> Ptr DefragmentationStats -> IO Result

-- | Deprecated. Compacts memory by moving allocations.
--
-- __Parameters__
--
-- +-----------+-----------------------+-----------------------------------------------+
-- |           | pAllocations          | Array of allocations that can be moved during |
-- |           |                       | this compation.                               |
-- +-----------+-----------------------+-----------------------------------------------+
-- |           | allocationCount       | Number of elements in pAllocations and        |
-- |           |                       | pAllocationsChanged arrays.                   |
-- +-----------+-----------------------+-----------------------------------------------+
-- | out       | pAllocationsChanged   | Array of boolean values that will indicate    |
-- |           |                       | whether matching allocation in pAllocations   |
-- |           |                       | array has been moved. This parameter is       |
-- |           |                       | optional. Pass null if you don\'t need this   |
-- |           |                       | information.                                  |
-- +-----------+-----------------------+-----------------------------------------------+
-- |           | pDefragmentationInfo  | Configuration parameters. Optional - pass     |
-- |           |                       | null to use default values.                   |
-- +-----------+-----------------------+-----------------------------------------------+
-- | out       | pDefragmentationStats | Statistics returned by the function. Optional |
-- |           |                       | - pass null if you don\'t need this           |
-- |           |                       | information.                                  |
-- +-----------+-----------------------+-----------------------------------------------+
--
-- __Returns__
--
-- @VK_SUCCESS@ if completed, negative error code in case of error.
--
-- /Deprecated/
--
-- This is a part of the old interface. It is recommended to use structure
-- 'DefragmentationInfo2' and function 'defragmentationBegin' instead.
--
-- This function works by moving allocations to different places (different
-- @VkDeviceMemory@ objects and\/or different offsets) in order to optimize
-- memory usage. Only allocations that are in @pAllocations@ array can be
-- moved. All other allocations are considered nonmovable in this call.
-- Basic rules:
--
-- -   Only allocations made in memory types that have
--     @VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT@ and
--     @VK_MEMORY_PROPERTY_HOST_COHERENT_BIT@ flags can be compacted. You
--     may pass other allocations but it makes no sense - these will never
--     be moved.
--
-- -   Custom pools created with 'POOL_CREATE_LINEAR_ALGORITHM_BIT' or
--     'POOL_CREATE_BUDDY_ALGORITHM_BIT' flag are not defragmented.
--     Allocations passed to this function that come from such pools are
--     ignored.
--
-- -   Allocations created with 'ALLOCATION_CREATE_DEDICATED_MEMORY_BIT' or
--     created as dedicated allocations for any other reason are also
--     ignored.
--
-- -   Both allocations made with or without 'ALLOCATION_CREATE_MAPPED_BIT'
--     flag can be compacted. If not persistently mapped, memory will be
--     mapped temporarily inside this function if needed.
--
-- -   You must not pass same 'Allocation' object multiple times in
--     @pAllocations@ array.
--
-- The function also frees empty @VkDeviceMemory@ blocks.
--
-- Warning: This function may be time-consuming, so you shouldn\'t call it
-- too often (like after every resource creation\/destruction). You can
-- call it on special occasions (like when reloading a game level or when
-- you just destroyed a lot of objects). Calling it every frame may be OK,
-- but you should measure that on your platform.
--
-- For more information, see /Defragmentation/ chapter.
defragment :: forall io
            . (MonadIO io)
           => -- No documentation found for Nested "vmaDefragment" "allocator"
              Allocator
           -> -- No documentation found for Nested "vmaDefragment" "pAllocations"
              ("allocations" ::: Vector Allocation)
           -> -- No documentation found for Nested "vmaDefragment" "pDefragmentationInfo"
              ("defragmentationInfo" ::: Maybe DefragmentationInfo)
           -> io (("allocationsChanged" ::: Vector Bool), DefragmentationStats)
defragment allocator allocations defragmentationInfo = liftIO . evalContT $ do
  pPAllocations <- ContT $ allocaBytesAligned @Allocation ((Data.Vector.length (allocations)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPAllocations `plusPtr` (8 * (i)) :: Ptr Allocation) (e)) (allocations)
  pPAllocationsChanged <- ContT $ bracket (callocBytes @Bool32 ((fromIntegral ((fromIntegral (Data.Vector.length $ (allocations)) :: CSize))) * 4)) free
  pDefragmentationInfo <- case (defragmentationInfo) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPDefragmentationStats <- ContT (withZeroCStruct @DefragmentationStats)
  r <- lift $ traceAroundEvent "vmaDefragment" ((ffiVmaDefragment) (allocator) (pPAllocations) ((fromIntegral (Data.Vector.length $ (allocations)) :: CSize)) (pPAllocationsChanged) pDefragmentationInfo (pPDefragmentationStats))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pAllocationsChanged <- lift $ generateM (fromIntegral ((fromIntegral (Data.Vector.length $ (allocations)) :: CSize))) (\i -> do
    pAllocationsChangedElem <- peek @Bool32 ((pPAllocationsChanged `advancePtrBytes` (4 * (i)) :: Ptr Bool32))
    pure $ bool32ToBool pAllocationsChangedElem)
  pDefragmentationStats <- lift $ peekCStruct @DefragmentationStats pPDefragmentationStats
  pure $ (pAllocationsChanged, pDefragmentationStats)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaBindBufferMemory" ffiVmaBindBufferMemory
  :: Allocator -> Allocation -> Buffer -> IO Result

-- | Binds buffer to allocation.
--
-- Binds specified buffer to region of memory represented by specified
-- allocation. Gets @VkDeviceMemory@ handle and offset from the allocation.
-- If you want to create a buffer, allocate memory for it and bind them
-- together separately, you should use this function for binding instead of
-- standard @vkBindBufferMemory()@, because it ensures proper
-- synchronization so that when a @VkDeviceMemory@ object is used by
-- multiple allocations, calls to @vkBind*Memory()@ or @vkMapMemory()@
-- won\'t happen from multiple threads simultaneously (which is illegal in
-- Vulkan).
--
-- It is recommended to use function 'createBuffer' instead of this one.
bindBufferMemory :: forall io
                  . (MonadIO io)
                 => -- No documentation found for Nested "vmaBindBufferMemory" "allocator"
                    Allocator
                 -> -- No documentation found for Nested "vmaBindBufferMemory" "allocation"
                    Allocation
                 -> -- No documentation found for Nested "vmaBindBufferMemory" "buffer"
                    Buffer
                 -> io ()
bindBufferMemory allocator allocation buffer = liftIO $ do
  r <- traceAroundEvent "vmaBindBufferMemory" ((ffiVmaBindBufferMemory) (allocator) (allocation) (buffer))
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaBindBufferMemory2" ffiVmaBindBufferMemory2
  :: Allocator -> Allocation -> DeviceSize -> Buffer -> Ptr () -> IO Result

-- | Binds buffer to allocation with additional parameters.
--
-- __Parameters__
--
-- +-----------------------+--------------------------------------------------------+
-- | allocationLocalOffset | Additional offset to be added while binding, relative  |
-- |                       | to the beginnig of the @allocation@. Normally it       |
-- |                       | should be 0.                                           |
-- +-----------------------+--------------------------------------------------------+
-- | pNext                 | A chain of structures to be attached to                |
-- |                       | @VkBindBufferMemoryInfoKHR@ structure used internally. |
-- |                       | Normally it should be null.                            |
-- +-----------------------+--------------------------------------------------------+
--
-- This function is similar to 'bindBufferMemory', but it provides
-- additional parameters.
--
-- If @pNext@ is not null, 'Allocator' object must have been created with
-- 'ALLOCATOR_CREATE_KHR_BIND_MEMORY2_BIT' flag or with
-- /VmaAllocatorCreateInfo::vulkanApiVersion/ @>= VK_API_VERSION_1_1@.
-- Otherwise the call fails.
bindBufferMemory2 :: forall io
                   . (MonadIO io)
                  => -- No documentation found for Nested "vmaBindBufferMemory2" "allocator"
                     Allocator
                  -> -- No documentation found for Nested "vmaBindBufferMemory2" "allocation"
                     Allocation
                  -> -- No documentation found for Nested "vmaBindBufferMemory2" "allocationLocalOffset"
                     ("allocationLocalOffset" ::: DeviceSize)
                  -> -- No documentation found for Nested "vmaBindBufferMemory2" "buffer"
                     Buffer
                  -> -- No documentation found for Nested "vmaBindBufferMemory2" "pNext"
                     ("next" ::: Ptr ())
                  -> io ()
bindBufferMemory2 allocator allocation allocationLocalOffset buffer next = liftIO $ do
  r <- traceAroundEvent "vmaBindBufferMemory2" ((ffiVmaBindBufferMemory2) (allocator) (allocation) (allocationLocalOffset) (buffer) (next))
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaBindImageMemory" ffiVmaBindImageMemory
  :: Allocator -> Allocation -> Image -> IO Result

-- | Binds image to allocation.
--
-- Binds specified image to region of memory represented by specified
-- allocation. Gets @VkDeviceMemory@ handle and offset from the allocation.
-- If you want to create an image, allocate memory for it and bind them
-- together separately, you should use this function for binding instead of
-- standard @vkBindImageMemory()@, because it ensures proper
-- synchronization so that when a @VkDeviceMemory@ object is used by
-- multiple allocations, calls to @vkBind*Memory()@ or @vkMapMemory()@
-- won\'t happen from multiple threads simultaneously (which is illegal in
-- Vulkan).
--
-- It is recommended to use function 'createImage' instead of this one.
bindImageMemory :: forall io
                 . (MonadIO io)
                => -- No documentation found for Nested "vmaBindImageMemory" "allocator"
                   Allocator
                -> -- No documentation found for Nested "vmaBindImageMemory" "allocation"
                   Allocation
                -> -- No documentation found for Nested "vmaBindImageMemory" "image"
                   Image
                -> io ()
bindImageMemory allocator allocation image = liftIO $ do
  r <- traceAroundEvent "vmaBindImageMemory" ((ffiVmaBindImageMemory) (allocator) (allocation) (image))
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaBindImageMemory2" ffiVmaBindImageMemory2
  :: Allocator -> Allocation -> DeviceSize -> Image -> Ptr () -> IO Result

-- | Binds image to allocation with additional parameters.
--
-- __Parameters__
--
-- +-----------------------+--------------------------------------------------------+
-- | allocationLocalOffset | Additional offset to be added while binding, relative  |
-- |                       | to the beginnig of the @allocation@. Normally it       |
-- |                       | should be 0.                                           |
-- +-----------------------+--------------------------------------------------------+
-- | pNext                 | A chain of structures to be attached to                |
-- |                       | @VkBindImageMemoryInfoKHR@ structure used internally.  |
-- |                       | Normally it should be null.                            |
-- +-----------------------+--------------------------------------------------------+
--
-- This function is similar to 'bindImageMemory', but it provides
-- additional parameters.
--
-- If @pNext@ is not null, 'Allocator' object must have been created with
-- 'ALLOCATOR_CREATE_KHR_BIND_MEMORY2_BIT' flag or with
-- /VmaAllocatorCreateInfo::vulkanApiVersion/ @>= VK_API_VERSION_1_1@.
-- Otherwise the call fails.
bindImageMemory2 :: forall io
                  . (MonadIO io)
                 => -- No documentation found for Nested "vmaBindImageMemory2" "allocator"
                    Allocator
                 -> -- No documentation found for Nested "vmaBindImageMemory2" "allocation"
                    Allocation
                 -> -- No documentation found for Nested "vmaBindImageMemory2" "allocationLocalOffset"
                    ("allocationLocalOffset" ::: DeviceSize)
                 -> -- No documentation found for Nested "vmaBindImageMemory2" "image"
                    Image
                 -> -- No documentation found for Nested "vmaBindImageMemory2" "pNext"
                    ("next" ::: Ptr ())
                 -> io ()
bindImageMemory2 allocator allocation allocationLocalOffset image next = liftIO $ do
  r <- traceAroundEvent "vmaBindImageMemory2" ((ffiVmaBindImageMemory2) (allocator) (allocation) (allocationLocalOffset) (image) (next))
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaCreateBuffer" ffiVmaCreateBuffer
  :: Allocator -> Ptr (SomeStruct BufferCreateInfo) -> Ptr AllocationCreateInfo -> Ptr Buffer -> Ptr Allocation -> Ptr AllocationInfo -> IO Result

-- | __Parameters__
--
-- +-----------+-----------------+-----------------------------------------------+
-- | out       | pBuffer         | Buffer that was created.                      |
-- +-----------+-----------------+-----------------------------------------------+
-- | out       | pAllocation     | Allocation that was created.                  |
-- +-----------+-----------------+-----------------------------------------------+
-- | out       | pAllocationInfo | Optional. Information about allocated memory. |
-- |           |                 | It can be later fetched using function        |
-- |           |                 | 'getAllocationInfo'.                          |
-- +-----------+-----------------+-----------------------------------------------+
--
-- This function automatically:
--
-- 1.  Creates buffer.
--
-- 2.  Allocates appropriate memory for it.
--
-- 3.  Binds the buffer with the memory.
--
-- If any of these operations fail, buffer and allocation are not created,
-- returned value is negative error code, *pBuffer and *pAllocation are
-- null.
--
-- If the function succeeded, you must destroy both buffer and allocation
-- when you no longer need them using either convenience function
-- 'destroyBuffer' or separately, using @vkDestroyBuffer()@ and
-- 'freeMemory'.
--
-- If 'ALLOCATOR_CREATE_KHR_DEDICATED_ALLOCATION_BIT' flag was used,
-- VK_KHR_dedicated_allocation extension is used internally to query driver
-- whether it requires or prefers the new buffer to have dedicated
-- allocation. If yes, and if dedicated allocation is possible
-- (/VmaAllocationCreateInfo::pool/ is null and
-- 'ALLOCATION_CREATE_NEVER_ALLOCATE_BIT' is not used), it creates
-- dedicated allocation for this buffer, just like when using
-- 'ALLOCATION_CREATE_DEDICATED_MEMORY_BIT'.
--
-- Note
--
-- This function creates a new @VkBuffer@. Sub-allocation of parts of one
-- large buffer, although recommended as a good practice, is out of scope
-- of this library and could be implemented by the user as a higher-level
-- logic on top of VMA.
createBuffer :: forall a io
              . (Extendss BufferCreateInfo a, PokeChain a, MonadIO io)
             => -- No documentation found for Nested "vmaCreateBuffer" "allocator"
                Allocator
             -> -- No documentation found for Nested "vmaCreateBuffer" "pBufferCreateInfo"
                (BufferCreateInfo a)
             -> -- No documentation found for Nested "vmaCreateBuffer" "pAllocationCreateInfo"
                AllocationCreateInfo
             -> io (Buffer, Allocation, AllocationInfo)
createBuffer allocator bufferCreateInfo allocationCreateInfo = liftIO . evalContT $ do
  pBufferCreateInfo <- ContT $ withCStruct (bufferCreateInfo)
  pAllocationCreateInfo <- ContT $ withCStruct (allocationCreateInfo)
  pPBuffer <- ContT $ bracket (callocBytes @Buffer 8) free
  pPAllocation <- ContT $ bracket (callocBytes @Allocation 8) free
  pPAllocationInfo <- ContT (withZeroCStruct @AllocationInfo)
  r <- lift $ traceAroundEvent "vmaCreateBuffer" ((ffiVmaCreateBuffer) (allocator) (forgetExtensions pBufferCreateInfo) pAllocationCreateInfo (pPBuffer) (pPAllocation) (pPAllocationInfo))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pBuffer <- lift $ peek @Buffer pPBuffer
  pAllocation <- lift $ peek @Allocation pPAllocation
  pAllocationInfo <- lift $ peekCStruct @AllocationInfo pPAllocationInfo
  pure $ (pBuffer, pAllocation, pAllocationInfo)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createBuffer' and 'destroyBuffer'
--
-- To ensure that 'destroyBuffer' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withBuffer :: forall a io r . (Extendss BufferCreateInfo a, PokeChain a, MonadIO io) => Allocator -> BufferCreateInfo a -> AllocationCreateInfo -> (io (Buffer, Allocation, AllocationInfo) -> ((Buffer, Allocation, AllocationInfo) -> io ()) -> r) -> r
withBuffer allocator pBufferCreateInfo pAllocationCreateInfo b =
  b (createBuffer allocator pBufferCreateInfo pAllocationCreateInfo)
    (\(o0, o1, _) -> destroyBuffer allocator o0 o1)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaDestroyBuffer" ffiVmaDestroyBuffer
  :: Allocator -> Buffer -> Allocation -> IO ()

-- | Destroys Vulkan buffer and frees allocated memory.
--
-- This is just a convenience function equivalent to:
--
-- > vkDestroyBuffer(device, buffer, allocationCallbacks);
-- > vmaFreeMemory(allocator, allocation);
--
-- It it safe to pass null as buffer and\/or allocation.
destroyBuffer :: forall io
               . (MonadIO io)
              => -- No documentation found for Nested "vmaDestroyBuffer" "allocator"
                 Allocator
              -> -- No documentation found for Nested "vmaDestroyBuffer" "buffer"
                 Buffer
              -> -- No documentation found for Nested "vmaDestroyBuffer" "allocation"
                 Allocation
              -> io ()
destroyBuffer allocator buffer allocation = liftIO $ do
  traceAroundEvent "vmaDestroyBuffer" ((ffiVmaDestroyBuffer) (allocator) (buffer) (allocation))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaCreateImage" ffiVmaCreateImage
  :: Allocator -> Ptr (SomeStruct ImageCreateInfo) -> Ptr AllocationCreateInfo -> Ptr Image -> Ptr Allocation -> Ptr AllocationInfo -> IO Result

-- | Function similar to 'createBuffer'.
createImage :: forall a io
             . (Extendss ImageCreateInfo a, PokeChain a, MonadIO io)
            => -- No documentation found for Nested "vmaCreateImage" "allocator"
               Allocator
            -> -- No documentation found for Nested "vmaCreateImage" "pImageCreateInfo"
               (ImageCreateInfo a)
            -> -- No documentation found for Nested "vmaCreateImage" "pAllocationCreateInfo"
               AllocationCreateInfo
            -> io (Image, Allocation, AllocationInfo)
createImage allocator imageCreateInfo allocationCreateInfo = liftIO . evalContT $ do
  pImageCreateInfo <- ContT $ withCStruct (imageCreateInfo)
  pAllocationCreateInfo <- ContT $ withCStruct (allocationCreateInfo)
  pPImage <- ContT $ bracket (callocBytes @Image 8) free
  pPAllocation <- ContT $ bracket (callocBytes @Allocation 8) free
  pPAllocationInfo <- ContT (withZeroCStruct @AllocationInfo)
  r <- lift $ traceAroundEvent "vmaCreateImage" ((ffiVmaCreateImage) (allocator) (forgetExtensions pImageCreateInfo) pAllocationCreateInfo (pPImage) (pPAllocation) (pPAllocationInfo))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pImage <- lift $ peek @Image pPImage
  pAllocation <- lift $ peek @Allocation pPAllocation
  pAllocationInfo <- lift $ peekCStruct @AllocationInfo pPAllocationInfo
  pure $ (pImage, pAllocation, pAllocationInfo)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createImage' and 'destroyImage'
--
-- To ensure that 'destroyImage' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withImage :: forall a io r . (Extendss ImageCreateInfo a, PokeChain a, MonadIO io) => Allocator -> ImageCreateInfo a -> AllocationCreateInfo -> (io (Image, Allocation, AllocationInfo) -> ((Image, Allocation, AllocationInfo) -> io ()) -> r) -> r
withImage allocator pImageCreateInfo pAllocationCreateInfo b =
  b (createImage allocator pImageCreateInfo pAllocationCreateInfo)
    (\(o0, o1, _) -> destroyImage allocator o0 o1)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaDestroyImage" ffiVmaDestroyImage
  :: Allocator -> Image -> Allocation -> IO ()

-- | Destroys Vulkan image and frees allocated memory.
--
-- This is just a convenience function equivalent to:
--
-- > vkDestroyImage(device, image, allocationCallbacks);
-- > vmaFreeMemory(allocator, allocation);
--
-- It it safe to pass null as image and\/or allocation.
destroyImage :: forall io
              . (MonadIO io)
             => -- No documentation found for Nested "vmaDestroyImage" "allocator"
                Allocator
             -> -- No documentation found for Nested "vmaDestroyImage" "image"
                Image
             -> -- No documentation found for Nested "vmaDestroyImage" "allocation"
                Allocation
             -> io ()
destroyImage allocator image allocation = liftIO $ do
  traceAroundEvent "vmaDestroyImage" ((ffiVmaDestroyImage) (allocator) (image) (allocation))
  pure $ ()


type FN_vkAllocateMemory = Ptr Device_T -> ("pAllocateInfo" ::: Ptr (SomeStruct MemoryAllocateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pMemory" ::: Ptr DeviceMemory) -> IO Result
-- No documentation found for TopLevel "PFN_vkAllocateMemory"
type PFN_vkAllocateMemory = FunPtr FN_vkAllocateMemory


type FN_vkBindBufferMemory = Ptr Device_T -> Buffer -> DeviceMemory -> ("memoryOffset" ::: DeviceSize) -> IO Result
-- No documentation found for TopLevel "PFN_vkBindBufferMemory"
type PFN_vkBindBufferMemory = FunPtr FN_vkBindBufferMemory


type FN_vkBindBufferMemory2KHR = Ptr Device_T -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr (SomeStruct BindBufferMemoryInfo)) -> IO Result
-- No documentation found for TopLevel "PFN_vkBindBufferMemory2KHR"
type PFN_vkBindBufferMemory2KHR = FunPtr FN_vkBindBufferMemory2KHR


type FN_vkBindImageMemory = Ptr Device_T -> Image -> DeviceMemory -> ("memoryOffset" ::: DeviceSize) -> IO Result
-- No documentation found for TopLevel "PFN_vkBindImageMemory"
type PFN_vkBindImageMemory = FunPtr FN_vkBindImageMemory


type FN_vkBindImageMemory2KHR = Ptr Device_T -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr (SomeStruct BindImageMemoryInfo)) -> IO Result
-- No documentation found for TopLevel "PFN_vkBindImageMemory2KHR"
type PFN_vkBindImageMemory2KHR = FunPtr FN_vkBindImageMemory2KHR


type FN_vkCmdCopyBuffer = Ptr CommandBuffer_T -> ("srcBuffer" ::: Buffer) -> ("dstBuffer" ::: Buffer) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr BufferCopy) -> IO ()
-- No documentation found for TopLevel "PFN_vkCmdCopyBuffer"
type PFN_vkCmdCopyBuffer = FunPtr FN_vkCmdCopyBuffer


type FN_vkCreateBuffer = Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct BufferCreateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pBuffer" ::: Ptr Buffer) -> IO Result
-- No documentation found for TopLevel "PFN_vkCreateBuffer"
type PFN_vkCreateBuffer = FunPtr FN_vkCreateBuffer


type FN_vkCreateImage = Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct ImageCreateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pImage" ::: Ptr Image) -> IO Result
-- No documentation found for TopLevel "PFN_vkCreateImage"
type PFN_vkCreateImage = FunPtr FN_vkCreateImage


type FN_vkDestroyBuffer = Ptr Device_T -> Buffer -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()
-- No documentation found for TopLevel "PFN_vkDestroyBuffer"
type PFN_vkDestroyBuffer = FunPtr FN_vkDestroyBuffer


type FN_vkDestroyImage = Ptr Device_T -> Image -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()
-- No documentation found for TopLevel "PFN_vkDestroyImage"
type PFN_vkDestroyImage = FunPtr FN_vkDestroyImage


type FN_vkFlushMappedMemoryRanges = Ptr Device_T -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr MappedMemoryRange) -> IO Result
-- No documentation found for TopLevel "PFN_vkFlushMappedMemoryRanges"
type PFN_vkFlushMappedMemoryRanges = FunPtr FN_vkFlushMappedMemoryRanges


type FN_vkFreeMemory = Ptr Device_T -> DeviceMemory -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()
-- No documentation found for TopLevel "PFN_vkFreeMemory"
type PFN_vkFreeMemory = FunPtr FN_vkFreeMemory


type FN_vkGetBufferMemoryRequirements = Ptr Device_T -> Buffer -> ("pMemoryRequirements" ::: Ptr MemoryRequirements) -> IO ()
-- No documentation found for TopLevel "PFN_vkGetBufferMemoryRequirements"
type PFN_vkGetBufferMemoryRequirements = FunPtr FN_vkGetBufferMemoryRequirements


type FN_vkGetBufferMemoryRequirements2KHR = Ptr Device_T -> ("pInfo" ::: Ptr BufferMemoryRequirementsInfo2) -> ("pMemoryRequirements" ::: Ptr (SomeStruct MemoryRequirements2)) -> IO ()
-- No documentation found for TopLevel "PFN_vkGetBufferMemoryRequirements2KHR"
type PFN_vkGetBufferMemoryRequirements2KHR = FunPtr FN_vkGetBufferMemoryRequirements2KHR


type FN_vkGetImageMemoryRequirements = Ptr Device_T -> Image -> ("pMemoryRequirements" ::: Ptr MemoryRequirements) -> IO ()
-- No documentation found for TopLevel "PFN_vkGetImageMemoryRequirements"
type PFN_vkGetImageMemoryRequirements = FunPtr FN_vkGetImageMemoryRequirements


type FN_vkGetImageMemoryRequirements2KHR = Ptr Device_T -> ("pInfo" ::: Ptr (SomeStruct ImageMemoryRequirementsInfo2)) -> ("pMemoryRequirements" ::: Ptr (SomeStruct MemoryRequirements2)) -> IO ()
-- No documentation found for TopLevel "PFN_vkGetImageMemoryRequirements2KHR"
type PFN_vkGetImageMemoryRequirements2KHR = FunPtr FN_vkGetImageMemoryRequirements2KHR


type FN_vkGetPhysicalDeviceMemoryProperties = Ptr PhysicalDevice_T -> ("pMemoryProperties" ::: Ptr PhysicalDeviceMemoryProperties) -> IO ()
-- No documentation found for TopLevel "PFN_vkGetPhysicalDeviceMemoryProperties"
type PFN_vkGetPhysicalDeviceMemoryProperties = FunPtr FN_vkGetPhysicalDeviceMemoryProperties


type FN_vkGetPhysicalDeviceMemoryProperties2KHR = Ptr PhysicalDevice_T -> ("pMemoryProperties" ::: Ptr (SomeStruct PhysicalDeviceMemoryProperties2)) -> IO ()
-- No documentation found for TopLevel "PFN_vkGetPhysicalDeviceMemoryProperties2KHR"
type PFN_vkGetPhysicalDeviceMemoryProperties2KHR = FunPtr FN_vkGetPhysicalDeviceMemoryProperties2KHR


type FN_vkGetPhysicalDeviceProperties = Ptr PhysicalDevice_T -> ("pProperties" ::: Ptr PhysicalDeviceProperties) -> IO ()
-- No documentation found for TopLevel "PFN_vkGetPhysicalDeviceProperties"
type PFN_vkGetPhysicalDeviceProperties = FunPtr FN_vkGetPhysicalDeviceProperties


type FN_vkInvalidateMappedMemoryRanges = Ptr Device_T -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr MappedMemoryRange) -> IO Result
-- No documentation found for TopLevel "PFN_vkInvalidateMappedMemoryRanges"
type PFN_vkInvalidateMappedMemoryRanges = FunPtr FN_vkInvalidateMappedMemoryRanges


type FN_vkMapMemory = Ptr Device_T -> DeviceMemory -> ("offset" ::: DeviceSize) -> DeviceSize -> MemoryMapFlags -> ("ppData" ::: Ptr (Ptr ())) -> IO Result
-- No documentation found for TopLevel "PFN_vkMapMemory"
type PFN_vkMapMemory = FunPtr FN_vkMapMemory


type FN_vkUnmapMemory = Ptr Device_T -> DeviceMemory -> IO ()
-- No documentation found for TopLevel "PFN_vkUnmapMemory"
type PFN_vkUnmapMemory = FunPtr FN_vkUnmapMemory


-- | VmaAllocator
--
-- Represents main object of this library initialized.
--
-- Fill structure 'AllocatorCreateInfo' and call function 'createAllocator'
-- to create it. Call function 'destroyAllocator' to destroy it.
--
-- It is recommended to create just one object of this type per @VkDevice@
-- object, right after Vulkan is initialized and keep it alive until before
-- Vulkan device is destroyed.
newtype Allocator = Allocator Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show Allocator where
  showsPrec p (Allocator x) = showParen (p >= 11) (showString "Allocator 0x" . showHex x)


type FN_vmaAllocateDeviceMemoryFunction = Allocator -> ("memoryType" ::: Word32) -> DeviceMemory -> DeviceSize -> ("pUserData" ::: Ptr ()) -> IO ()
-- No documentation found for TopLevel "PFN_vmaAllocateDeviceMemoryFunction"
type PFN_vmaAllocateDeviceMemoryFunction = FunPtr FN_vmaAllocateDeviceMemoryFunction


type FN_vmaFreeDeviceMemoryFunction = Allocator -> ("memoryType" ::: Word32) -> DeviceMemory -> DeviceSize -> ("pUserData" ::: Ptr ()) -> IO ()
-- No documentation found for TopLevel "PFN_vmaFreeDeviceMemoryFunction"
type PFN_vmaFreeDeviceMemoryFunction = FunPtr FN_vmaFreeDeviceMemoryFunction


-- | VmaDeviceMemoryCallbacks
--
-- Set of callbacks that the library will call for @vkAllocateMemory@ and
-- @vkFreeMemory@.
--
-- Provided for informative purpose, e.g. to gather statistics about number
-- of allocations or total amount of memory allocated in Vulkan.
--
-- Used in /VmaAllocatorCreateInfo::pDeviceMemoryCallbacks/.
data DeviceMemoryCallbacks = DeviceMemoryCallbacks
  { -- | Optional, can be null.
    pfnAllocate :: PFN_vmaAllocateDeviceMemoryFunction
  , -- | Optional, can be null.
    pfnFree :: PFN_vmaFreeDeviceMemoryFunction
  , -- | Optional, can be null.
    userData :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceMemoryCallbacks)
#endif
deriving instance Show DeviceMemoryCallbacks

instance ToCStruct DeviceMemoryCallbacks where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceMemoryCallbacks{..} f = do
    poke ((p `plusPtr` 0 :: Ptr PFN_vmaAllocateDeviceMemoryFunction)) (pfnAllocate)
    poke ((p `plusPtr` 8 :: Ptr PFN_vmaFreeDeviceMemoryFunction)) (pfnFree)
    poke ((p `plusPtr` 16 :: Ptr (Ptr ()))) (userData)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct _ f = f

instance FromCStruct DeviceMemoryCallbacks where
  peekCStruct p = do
    pfnAllocate <- peek @PFN_vmaAllocateDeviceMemoryFunction ((p `plusPtr` 0 :: Ptr PFN_vmaAllocateDeviceMemoryFunction))
    pfnFree <- peek @PFN_vmaFreeDeviceMemoryFunction ((p `plusPtr` 8 :: Ptr PFN_vmaFreeDeviceMemoryFunction))
    pUserData <- peek @(Ptr ()) ((p `plusPtr` 16 :: Ptr (Ptr ())))
    pure $ DeviceMemoryCallbacks
             pfnAllocate pfnFree pUserData

instance Storable DeviceMemoryCallbacks where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeviceMemoryCallbacks where
  zero = DeviceMemoryCallbacks
           zero
           zero
           zero


type AllocatorCreateFlags = AllocatorCreateFlagBits

-- | Flags for created 'Allocator'.
newtype AllocatorCreateFlagBits = AllocatorCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | Allocator and all objects created from it will not be synchronized
-- internally, so you must guarantee they are used from only one thread at
-- a time or synchronized externally by you.
--
-- Using this flag may increase performance because internal mutexes are
-- not used.
pattern ALLOCATOR_CREATE_EXTERNALLY_SYNCHRONIZED_BIT    = AllocatorCreateFlagBits 0x00000001
-- | Enables usage of VK_KHR_dedicated_allocation extension.
--
-- The flag works only if /VmaAllocatorCreateInfo::vulkanApiVersion/
-- @== VK_API_VERSION_1_0@. When it\'s @VK_API_VERSION_1_1@, the flag is
-- ignored because the extension has been promoted to Vulkan 1.1.
--
-- Using this extenion will automatically allocate dedicated blocks of
-- memory for some buffers and images instead of suballocating place for
-- them out of bigger memory blocks (as if you explicitly used
-- 'ALLOCATION_CREATE_DEDICATED_MEMORY_BIT' flag) when it is recommended by
-- the driver. It may improve performance on some GPUs.
--
-- You may set this flag only if you found out that following device
-- extensions are supported, you enabled them while creating Vulkan device
-- passed as /VmaAllocatorCreateInfo::device/, and you want them to be used
-- internally by this library:
--
-- -   VK_KHR_get_memory_requirements2 (device extension)
--
-- -   VK_KHR_dedicated_allocation (device extension)
--
-- When this flag is set, you can experience following warnings reported by
-- Vulkan validation layer. You can ignore them.
--
-- vkBindBufferMemory(): Binding memory to buffer 0x2d but
-- vkGetBufferMemoryRequirements() has not been called on that buffer.
pattern ALLOCATOR_CREATE_KHR_DEDICATED_ALLOCATION_BIT   = AllocatorCreateFlagBits 0x00000002
-- | Enables usage of VK_KHR_bind_memory2 extension.
--
-- The flag works only if /VmaAllocatorCreateInfo::vulkanApiVersion/
-- @== VK_API_VERSION_1_0@. When it\'s @VK_API_VERSION_1_1@, the flag is
-- ignored because the extension has been promoted to Vulkan 1.1.
--
-- You may set this flag only if you found out that this device extension
-- is supported, you enabled it while creating Vulkan device passed as
-- /VmaAllocatorCreateInfo::device/, and you want it to be used internally
-- by this library.
--
-- The extension provides functions @vkBindBufferMemory2KHR@ and
-- @vkBindImageMemory2KHR@, which allow to pass a chain of @pNext@
-- structures while binding. This flag is required if you use @pNext@
-- parameter in 'bindBufferMemory2' or 'bindImageMemory2'.
pattern ALLOCATOR_CREATE_KHR_BIND_MEMORY2_BIT           = AllocatorCreateFlagBits 0x00000004
-- | Enables usage of VK_EXT_memory_budget extension.
--
-- You may set this flag only if you found out that this device extension
-- is supported, you enabled it while creating Vulkan device passed as
-- /VmaAllocatorCreateInfo::device/, and you want it to be used internally
-- by this library, along with another instance extension
-- VK_KHR_get_physical_device_properties2, which is required by it (or
-- Vulkan 1.1, where this extension is promoted).
--
-- The extension provides query for current memory usage and budget, which
-- will probably be more accurate than an estimation used by the library
-- otherwise.
pattern ALLOCATOR_CREATE_EXT_MEMORY_BUDGET_BIT          = AllocatorCreateFlagBits 0x00000008
-- | Enables usage of VK_AMD_device_coherent_memory extension.
--
-- You may set this flag only if you:
--
-- -   found out that this device extension is supported and enabled it
--     while creating Vulkan device passed as
--     /VmaAllocatorCreateInfo::device/,
--
-- -   checked that
--     @VkPhysicalDeviceCoherentMemoryFeaturesAMD::deviceCoherentMemory@ is
--     true and set it while creating the Vulkan device,
--
-- -   want it to be used internally by this library.
--
-- The extension and accompanying device feature provide access to memory
-- types with @VK_MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD@ and
-- @VK_MEMORY_PROPERTY_DEVICE_UNCACHED_BIT_AMD@ flags. They are useful
-- mostly for writing breadcrumb markers - a common method for debugging
-- GPU crash\/hang\/TDR.
--
-- When the extension is not enabled, such memory types are still
-- enumerated, but their usage is illegal. To protect from this error, if
-- you don\'t create the allocator with this flag, it will refuse to
-- allocate any memory or create a custom pool in such memory type,
-- returning @VK_ERROR_FEATURE_NOT_PRESENT@.
pattern ALLOCATOR_CREATE_AMD_DEVICE_COHERENT_MEMORY_BIT = AllocatorCreateFlagBits 0x00000010
-- | Enables usage of \"buffer device address\" feature, which allows you to
-- use function @vkGetBufferDeviceAddress*@ to get raw GPU pointer to a
-- buffer and pass it for usage inside a shader.
--
-- You may set this flag only if you:
--
-- 1.  (For Vulkan version \< 1.2) Found as available and enabled device
--     extension VK_KHR_buffer_device_address. This extension is promoted
--     to core Vulkan 1.2.
--
-- 2.  Found as available and enabled device feature
--     @VkPhysicalDeviceBufferDeviceAddressFeatures::bufferDeviceAddress@.
--
-- When this flag is set, you can create buffers with
-- @VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT@ using VMA. The library
-- automatically adds @VK_MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT@ to allocated
-- memory blocks wherever it might be needed.
--
-- For more information, see documentation chapter /Enabling buffer device
-- address/.
pattern ALLOCATOR_CREATE_BUFFER_DEVICE_ADDRESS_BIT      = AllocatorCreateFlagBits 0x00000020

conNameAllocatorCreateFlagBits :: String
conNameAllocatorCreateFlagBits = "AllocatorCreateFlagBits"

enumPrefixAllocatorCreateFlagBits :: String
enumPrefixAllocatorCreateFlagBits = "ALLOCATOR_CREATE_"

showTableAllocatorCreateFlagBits :: [(AllocatorCreateFlagBits, String)]
showTableAllocatorCreateFlagBits =
  [ (ALLOCATOR_CREATE_EXTERNALLY_SYNCHRONIZED_BIT   , "EXTERNALLY_SYNCHRONIZED_BIT")
  , (ALLOCATOR_CREATE_KHR_DEDICATED_ALLOCATION_BIT  , "KHR_DEDICATED_ALLOCATION_BIT")
  , (ALLOCATOR_CREATE_KHR_BIND_MEMORY2_BIT          , "KHR_BIND_MEMORY2_BIT")
  , (ALLOCATOR_CREATE_EXT_MEMORY_BUDGET_BIT         , "EXT_MEMORY_BUDGET_BIT")
  , (ALLOCATOR_CREATE_AMD_DEVICE_COHERENT_MEMORY_BIT, "AMD_DEVICE_COHERENT_MEMORY_BIT")
  , (ALLOCATOR_CREATE_BUFFER_DEVICE_ADDRESS_BIT     , "BUFFER_DEVICE_ADDRESS_BIT")
  ]

instance Show AllocatorCreateFlagBits where
  showsPrec = enumShowsPrec enumPrefixAllocatorCreateFlagBits
                            showTableAllocatorCreateFlagBits
                            conNameAllocatorCreateFlagBits
                            (\(AllocatorCreateFlagBits x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read AllocatorCreateFlagBits where
  readPrec = enumReadPrec enumPrefixAllocatorCreateFlagBits
                          showTableAllocatorCreateFlagBits
                          conNameAllocatorCreateFlagBits
                          AllocatorCreateFlagBits


-- | VmaVulkanFunctions
--
-- Pointers to some Vulkan functions - a subset used by the library.
--
-- Used in /VmaAllocatorCreateInfo::pVulkanFunctions/.
data VulkanFunctions = VulkanFunctions
  { 
    vkGetPhysicalDeviceProperties :: PFN_vkGetPhysicalDeviceProperties
  , 
    vkGetPhysicalDeviceMemoryProperties :: PFN_vkGetPhysicalDeviceMemoryProperties
  , 
    vkAllocateMemory :: PFN_vkAllocateMemory
  , 
    vkFreeMemory :: PFN_vkFreeMemory
  , 
    vkMapMemory :: PFN_vkMapMemory
  , 
    vkUnmapMemory :: PFN_vkUnmapMemory
  , 
    vkFlushMappedMemoryRanges :: PFN_vkFlushMappedMemoryRanges
  , 
    vkInvalidateMappedMemoryRanges :: PFN_vkInvalidateMappedMemoryRanges
  , 
    vkBindBufferMemory :: PFN_vkBindBufferMemory
  , 
    vkBindImageMemory :: PFN_vkBindImageMemory
  , 
    vkGetBufferMemoryRequirements :: PFN_vkGetBufferMemoryRequirements
  , 
    vkGetImageMemoryRequirements :: PFN_vkGetImageMemoryRequirements
  , 
    vkCreateBuffer :: PFN_vkCreateBuffer
  , 
    vkDestroyBuffer :: PFN_vkDestroyBuffer
  , 
    vkCreateImage :: PFN_vkCreateImage
  , 
    vkDestroyImage :: PFN_vkDestroyImage
  , 
    vkCmdCopyBuffer :: PFN_vkCmdCopyBuffer
  , -- No documentation found for Nested "VmaVulkanFunctions" "vkGetBufferMemoryRequirements2KHR"
    vkGetBufferMemoryRequirements2KHR :: PFN_vkGetBufferMemoryRequirements2KHR
  , -- No documentation found for Nested "VmaVulkanFunctions" "vkGetImageMemoryRequirements2KHR"
    vkGetImageMemoryRequirements2KHR :: PFN_vkGetImageMemoryRequirements2KHR
  , -- No documentation found for Nested "VmaVulkanFunctions" "vkBindBufferMemory2KHR"
    vkBindBufferMemory2KHR :: PFN_vkBindBufferMemory2KHR
  , -- No documentation found for Nested "VmaVulkanFunctions" "vkBindImageMemory2KHR"
    vkBindImageMemory2KHR :: PFN_vkBindImageMemory2KHR
  , -- No documentation found for Nested "VmaVulkanFunctions" "vkGetPhysicalDeviceMemoryProperties2KHR"
    vkGetPhysicalDeviceMemoryProperties2KHR :: PFN_vkGetPhysicalDeviceMemoryProperties2KHR
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (VulkanFunctions)
#endif
deriving instance Show VulkanFunctions

instance ToCStruct VulkanFunctions where
  withCStruct x f = allocaBytesAligned 176 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p VulkanFunctions{..} f = do
    poke ((p `plusPtr` 0 :: Ptr PFN_vkGetPhysicalDeviceProperties)) (vkGetPhysicalDeviceProperties)
    poke ((p `plusPtr` 8 :: Ptr PFN_vkGetPhysicalDeviceMemoryProperties)) (vkGetPhysicalDeviceMemoryProperties)
    poke ((p `plusPtr` 16 :: Ptr PFN_vkAllocateMemory)) (vkAllocateMemory)
    poke ((p `plusPtr` 24 :: Ptr PFN_vkFreeMemory)) (vkFreeMemory)
    poke ((p `plusPtr` 32 :: Ptr PFN_vkMapMemory)) (vkMapMemory)
    poke ((p `plusPtr` 40 :: Ptr PFN_vkUnmapMemory)) (vkUnmapMemory)
    poke ((p `plusPtr` 48 :: Ptr PFN_vkFlushMappedMemoryRanges)) (vkFlushMappedMemoryRanges)
    poke ((p `plusPtr` 56 :: Ptr PFN_vkInvalidateMappedMemoryRanges)) (vkInvalidateMappedMemoryRanges)
    poke ((p `plusPtr` 64 :: Ptr PFN_vkBindBufferMemory)) (vkBindBufferMemory)
    poke ((p `plusPtr` 72 :: Ptr PFN_vkBindImageMemory)) (vkBindImageMemory)
    poke ((p `plusPtr` 80 :: Ptr PFN_vkGetBufferMemoryRequirements)) (vkGetBufferMemoryRequirements)
    poke ((p `plusPtr` 88 :: Ptr PFN_vkGetImageMemoryRequirements)) (vkGetImageMemoryRequirements)
    poke ((p `plusPtr` 96 :: Ptr PFN_vkCreateBuffer)) (vkCreateBuffer)
    poke ((p `plusPtr` 104 :: Ptr PFN_vkDestroyBuffer)) (vkDestroyBuffer)
    poke ((p `plusPtr` 112 :: Ptr PFN_vkCreateImage)) (vkCreateImage)
    poke ((p `plusPtr` 120 :: Ptr PFN_vkDestroyImage)) (vkDestroyImage)
    poke ((p `plusPtr` 128 :: Ptr PFN_vkCmdCopyBuffer)) (vkCmdCopyBuffer)
    poke ((p `plusPtr` 136 :: Ptr PFN_vkGetBufferMemoryRequirements2KHR)) (vkGetBufferMemoryRequirements2KHR)
    poke ((p `plusPtr` 144 :: Ptr PFN_vkGetImageMemoryRequirements2KHR)) (vkGetImageMemoryRequirements2KHR)
    poke ((p `plusPtr` 152 :: Ptr PFN_vkBindBufferMemory2KHR)) (vkBindBufferMemory2KHR)
    poke ((p `plusPtr` 160 :: Ptr PFN_vkBindImageMemory2KHR)) (vkBindImageMemory2KHR)
    poke ((p `plusPtr` 168 :: Ptr PFN_vkGetPhysicalDeviceMemoryProperties2KHR)) (vkGetPhysicalDeviceMemoryProperties2KHR)
    f
  cStructSize = 176
  cStructAlignment = 8
  pokeZeroCStruct _ f = f

instance FromCStruct VulkanFunctions where
  peekCStruct p = do
    vkGetPhysicalDeviceProperties <- peek @PFN_vkGetPhysicalDeviceProperties ((p `plusPtr` 0 :: Ptr PFN_vkGetPhysicalDeviceProperties))
    vkGetPhysicalDeviceMemoryProperties <- peek @PFN_vkGetPhysicalDeviceMemoryProperties ((p `plusPtr` 8 :: Ptr PFN_vkGetPhysicalDeviceMemoryProperties))
    vkAllocateMemory <- peek @PFN_vkAllocateMemory ((p `plusPtr` 16 :: Ptr PFN_vkAllocateMemory))
    vkFreeMemory <- peek @PFN_vkFreeMemory ((p `plusPtr` 24 :: Ptr PFN_vkFreeMemory))
    vkMapMemory <- peek @PFN_vkMapMemory ((p `plusPtr` 32 :: Ptr PFN_vkMapMemory))
    vkUnmapMemory <- peek @PFN_vkUnmapMemory ((p `plusPtr` 40 :: Ptr PFN_vkUnmapMemory))
    vkFlushMappedMemoryRanges <- peek @PFN_vkFlushMappedMemoryRanges ((p `plusPtr` 48 :: Ptr PFN_vkFlushMappedMemoryRanges))
    vkInvalidateMappedMemoryRanges <- peek @PFN_vkInvalidateMappedMemoryRanges ((p `plusPtr` 56 :: Ptr PFN_vkInvalidateMappedMemoryRanges))
    vkBindBufferMemory <- peek @PFN_vkBindBufferMemory ((p `plusPtr` 64 :: Ptr PFN_vkBindBufferMemory))
    vkBindImageMemory <- peek @PFN_vkBindImageMemory ((p `plusPtr` 72 :: Ptr PFN_vkBindImageMemory))
    vkGetBufferMemoryRequirements <- peek @PFN_vkGetBufferMemoryRequirements ((p `plusPtr` 80 :: Ptr PFN_vkGetBufferMemoryRequirements))
    vkGetImageMemoryRequirements <- peek @PFN_vkGetImageMemoryRequirements ((p `plusPtr` 88 :: Ptr PFN_vkGetImageMemoryRequirements))
    vkCreateBuffer <- peek @PFN_vkCreateBuffer ((p `plusPtr` 96 :: Ptr PFN_vkCreateBuffer))
    vkDestroyBuffer <- peek @PFN_vkDestroyBuffer ((p `plusPtr` 104 :: Ptr PFN_vkDestroyBuffer))
    vkCreateImage <- peek @PFN_vkCreateImage ((p `plusPtr` 112 :: Ptr PFN_vkCreateImage))
    vkDestroyImage <- peek @PFN_vkDestroyImage ((p `plusPtr` 120 :: Ptr PFN_vkDestroyImage))
    vkCmdCopyBuffer <- peek @PFN_vkCmdCopyBuffer ((p `plusPtr` 128 :: Ptr PFN_vkCmdCopyBuffer))
    vkGetBufferMemoryRequirements2KHR <- peek @PFN_vkGetBufferMemoryRequirements2KHR ((p `plusPtr` 136 :: Ptr PFN_vkGetBufferMemoryRequirements2KHR))
    vkGetImageMemoryRequirements2KHR <- peek @PFN_vkGetImageMemoryRequirements2KHR ((p `plusPtr` 144 :: Ptr PFN_vkGetImageMemoryRequirements2KHR))
    vkBindBufferMemory2KHR <- peek @PFN_vkBindBufferMemory2KHR ((p `plusPtr` 152 :: Ptr PFN_vkBindBufferMemory2KHR))
    vkBindImageMemory2KHR <- peek @PFN_vkBindImageMemory2KHR ((p `plusPtr` 160 :: Ptr PFN_vkBindImageMemory2KHR))
    vkGetPhysicalDeviceMemoryProperties2KHR <- peek @PFN_vkGetPhysicalDeviceMemoryProperties2KHR ((p `plusPtr` 168 :: Ptr PFN_vkGetPhysicalDeviceMemoryProperties2KHR))
    pure $ VulkanFunctions
             vkGetPhysicalDeviceProperties vkGetPhysicalDeviceMemoryProperties vkAllocateMemory vkFreeMemory vkMapMemory vkUnmapMemory vkFlushMappedMemoryRanges vkInvalidateMappedMemoryRanges vkBindBufferMemory vkBindImageMemory vkGetBufferMemoryRequirements vkGetImageMemoryRequirements vkCreateBuffer vkDestroyBuffer vkCreateImage vkDestroyImage vkCmdCopyBuffer vkGetBufferMemoryRequirements2KHR vkGetImageMemoryRequirements2KHR vkBindBufferMemory2KHR vkBindImageMemory2KHR vkGetPhysicalDeviceMemoryProperties2KHR

instance Storable VulkanFunctions where
  sizeOf ~_ = 176
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero VulkanFunctions where
  zero = VulkanFunctions
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


type RecordFlags = RecordFlagBits

-- | Flags to be used in /VmaRecordSettings::flags/.
newtype RecordFlagBits = RecordFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | Enables flush after recording every function call.
--
-- Enable it if you expect your application to crash, which may leave
-- recording file truncated. It may degrade performance though.
pattern RECORD_FLUSH_AFTER_CALL_BIT = RecordFlagBits 0x00000001

conNameRecordFlagBits :: String
conNameRecordFlagBits = "RecordFlagBits"

enumPrefixRecordFlagBits :: String
enumPrefixRecordFlagBits = "RECORD_FLUSH_AFTER_CALL_BIT"

showTableRecordFlagBits :: [(RecordFlagBits, String)]
showTableRecordFlagBits = [(RECORD_FLUSH_AFTER_CALL_BIT, "")]

instance Show RecordFlagBits where
  showsPrec = enumShowsPrec enumPrefixRecordFlagBits
                            showTableRecordFlagBits
                            conNameRecordFlagBits
                            (\(RecordFlagBits x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read RecordFlagBits where
  readPrec = enumReadPrec enumPrefixRecordFlagBits showTableRecordFlagBits conNameRecordFlagBits RecordFlagBits


-- | VmaRecordSettings
--
-- Parameters for recording calls to VMA functions. To be used in
-- /VmaAllocatorCreateInfo::pRecordSettings/.
data RecordSettings = RecordSettings
  { -- | Flags for recording. Use 'RecordFlagBits' enum.
    flags :: RecordFlags
  , -- | Path to the file that should be written by the recording.
    --
    -- Suggested extension: \"csv\". If the file already exists, it will be
    -- overwritten. It will be opened for the whole time 'Allocator' object is
    -- alive. If opening this file fails, creation of the whole allocator
    -- object fails.
    filePath :: ByteString
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RecordSettings)
#endif
deriving instance Show RecordSettings

instance ToCStruct RecordSettings where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RecordSettings{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr RecordFlags)) (flags)
    pFilePath'' <- ContT $ useAsCString (filePath)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr CChar))) pFilePath''
    lift $ f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr RecordFlags)) (zero)
    pFilePath'' <- ContT $ useAsCString (mempty)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr CChar))) pFilePath''
    lift $ f

instance FromCStruct RecordSettings where
  peekCStruct p = do
    flags <- peek @RecordFlags ((p `plusPtr` 0 :: Ptr RecordFlags))
    pFilePath <- packCString =<< peek ((p `plusPtr` 8 :: Ptr (Ptr CChar)))
    pure $ RecordSettings
             flags pFilePath

instance Zero RecordSettings where
  zero = RecordSettings
           zero
           mempty


-- | VmaAllocatorCreateInfo
--
-- Description of a Allocator to be created.
data AllocatorCreateInfo = AllocatorCreateInfo
  { -- | Flags for created allocator. Use 'AllocatorCreateFlagBits' enum.
    flags :: AllocatorCreateFlags
  , -- | Vulkan physical device.
    --
    -- It must be valid throughout whole lifetime of created allocator.
    physicalDevice :: Ptr PhysicalDevice_T
  , -- | Vulkan device.
    --
    -- It must be valid throughout whole lifetime of created allocator.
    device :: Ptr Device_T
  , -- | Preferred size of a single @VkDeviceMemory@ block to be allocated from
    -- large heaps > 1 GiB. Optional.
    --
    -- Set to 0 to use default, which is currently 256 MiB.
    preferredLargeHeapBlockSize :: DeviceSize
  , -- | Custom CPU memory allocation callbacks. Optional.
    --
    -- Optional, can be null. When specified, will also be used for all
    -- CPU-side memory allocations.
    allocationCallbacks :: Maybe AllocationCallbacks
  , -- | Informative callbacks for @vkAllocateMemory@, @vkFreeMemory@. Optional.
    --
    -- Optional, can be null.
    deviceMemoryCallbacks :: Maybe DeviceMemoryCallbacks
  , -- | Maximum number of additional frames that are in use at the same time as
    -- current frame.
    --
    -- This value is used only when you make allocations with
    -- VMA_ALLOCATION_CREATE_CAN_BECOME_LOST_BIT flag. Such allocation cannot
    -- become lost if allocation.lastUseFrameIndex >=
    -- allocator.currentFrameIndex - frameInUseCount.
    --
    -- For example, if you double-buffer your command buffers, so resources
    -- used for rendering in previous frame may still be in use by the GPU at
    -- the moment you allocate resources needed for the current frame, set this
    -- value to 1.
    --
    -- If you want to allow any allocations other than used in the current
    -- frame to become lost, set this value to 0.
    frameInUseCount :: Word32
  , -- | Either null or a pointer to an array of limits on maximum number of
    -- bytes that can be allocated out of particular Vulkan memory heap.
    --
    -- If not NULL, it must be a pointer to an array of
    -- @VkPhysicalDeviceMemoryProperties::memoryHeapCount@ elements, defining
    -- limit on maximum number of bytes that can be allocated out of particular
    -- Vulkan memory heap.
    --
    -- Any of the elements may be equal to @VK_WHOLE_SIZE@, which means no
    -- limit on that heap. This is also the default in case of @pHeapSizeLimit@
    -- = NULL.
    --
    -- If there is a limit defined for a heap:
    --
    -- -   If user tries to allocate more memory from that heap using this
    --     allocator, the allocation fails with
    --     @VK_ERROR_OUT_OF_DEVICE_MEMORY@.
    --
    -- -   If the limit is smaller than heap size reported in
    --     @VkMemoryHeap::size@, the value of this limit will be reported
    --     instead when using 'getMemoryProperties'.
    --
    -- Warning! Using this feature may not be equivalent to installing a GPU
    -- with smaller amount of memory, because graphics driver doesn\'t
    -- necessary fail new allocations with @VK_ERROR_OUT_OF_DEVICE_MEMORY@
    -- result when memory capacity is exceeded. It may return success and just
    -- silently migrate some device memory blocks to system RAM. This driver
    -- behavior can also be controlled using
    -- VK_AMD_memory_overallocation_behavior extension.
    heapSizeLimit :: Ptr DeviceSize
  , -- | Pointers to Vulkan functions. Can be null.
    --
    -- For details see /Pointers to Vulkan functions/.
    vulkanFunctions :: Maybe VulkanFunctions
  , -- | Parameters for recording of VMA calls. Can be null.
    --
    -- If not null, it enables recording of calls to VMA functions to a file.
    -- If support for recording is not enabled using @VMA_RECORDING_ENABLED@
    -- macro, creation of the allocator object fails with
    -- @VK_ERROR_FEATURE_NOT_PRESENT@.
    recordSettings :: Maybe RecordSettings
  , -- | Handle to Vulkan instance object.
    --
    -- Starting from version 3.0.0 this member is no longer optional, it must
    -- be set!
    instance' :: Ptr Instance_T
  , -- | Optional. The highest version of Vulkan that the application is designed
    -- to use.
    --
    -- It must be a value in the format as created by macro @VK_MAKE_VERSION@
    -- or a constant like: @VK_API_VERSION_1_1@, @VK_API_VERSION_1_0@. The
    -- patch version number specified is ignored. Only the major and minor
    -- versions are considered. It must be less or equal (preferably equal) to
    -- value as passed to @vkCreateInstance@ as
    -- @VkApplicationInfo::apiVersion@. Only versions 1.0, 1.1, 1.2 are
    -- supported by the current implementation. Leaving it initialized to zero
    -- is equivalent to @VK_API_VERSION_1_0@.
    vulkanApiVersion :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AllocatorCreateInfo)
#endif
deriving instance Show AllocatorCreateInfo

instance ToCStruct AllocatorCreateInfo where
  withCStruct x f = allocaBytesAligned 96 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AllocatorCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr AllocatorCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr PhysicalDevice_T))) (physicalDevice)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr Device_T))) (device)
    lift $ poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (preferredLargeHeapBlockSize)
    pAllocationCallbacks'' <- case (allocationCallbacks) of
      Nothing -> pure nullPtr
      Just j -> ContT $ withCStruct (j)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr AllocationCallbacks))) pAllocationCallbacks''
    pDeviceMemoryCallbacks'' <- case (deviceMemoryCallbacks) of
      Nothing -> pure nullPtr
      Just j -> ContT $ withCStruct (j)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr DeviceMemoryCallbacks))) pDeviceMemoryCallbacks''
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) (frameInUseCount)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr DeviceSize))) (heapSizeLimit)
    pVulkanFunctions'' <- case (vulkanFunctions) of
      Nothing -> pure nullPtr
      Just j -> ContT $ withCStruct (j)
    lift $ poke ((p `plusPtr` 64 :: Ptr (Ptr VulkanFunctions))) pVulkanFunctions''
    pRecordSettings'' <- case (recordSettings) of
      Nothing -> pure nullPtr
      Just j -> ContT $ withCStruct (j)
    lift $ poke ((p `plusPtr` 72 :: Ptr (Ptr RecordSettings))) pRecordSettings''
    lift $ poke ((p `plusPtr` 80 :: Ptr (Ptr Instance_T))) (instance')
    lift $ poke ((p `plusPtr` 88 :: Ptr Word32)) (vulkanApiVersion)
    lift $ f
  cStructSize = 96
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr AllocatorCreateFlags)) (zero)
    poke ((p `plusPtr` 8 :: Ptr (Ptr PhysicalDevice_T))) (zero)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Device_T))) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 80 :: Ptr (Ptr Instance_T))) (zero)
    poke ((p `plusPtr` 88 :: Ptr Word32)) (zero)
    f

instance FromCStruct AllocatorCreateInfo where
  peekCStruct p = do
    flags <- peek @AllocatorCreateFlags ((p `plusPtr` 0 :: Ptr AllocatorCreateFlags))
    physicalDevice <- peek @(Ptr PhysicalDevice_T) ((p `plusPtr` 8 :: Ptr (Ptr PhysicalDevice_T)))
    device <- peek @(Ptr Device_T) ((p `plusPtr` 16 :: Ptr (Ptr Device_T)))
    preferredLargeHeapBlockSize <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    pAllocationCallbacks <- peek @(Ptr AllocationCallbacks) ((p `plusPtr` 32 :: Ptr (Ptr AllocationCallbacks)))
    pAllocationCallbacks' <- maybePeek (\j -> peekCStruct @AllocationCallbacks (j)) pAllocationCallbacks
    pDeviceMemoryCallbacks <- peek @(Ptr DeviceMemoryCallbacks) ((p `plusPtr` 40 :: Ptr (Ptr DeviceMemoryCallbacks)))
    pDeviceMemoryCallbacks' <- maybePeek (\j -> peekCStruct @DeviceMemoryCallbacks (j)) pDeviceMemoryCallbacks
    frameInUseCount <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    pHeapSizeLimit <- peek @(Ptr DeviceSize) ((p `plusPtr` 56 :: Ptr (Ptr DeviceSize)))
    pVulkanFunctions <- peek @(Ptr VulkanFunctions) ((p `plusPtr` 64 :: Ptr (Ptr VulkanFunctions)))
    pVulkanFunctions' <- maybePeek (\j -> peekCStruct @VulkanFunctions (j)) pVulkanFunctions
    pRecordSettings <- peek @(Ptr RecordSettings) ((p `plusPtr` 72 :: Ptr (Ptr RecordSettings)))
    pRecordSettings' <- maybePeek (\j -> peekCStruct @RecordSettings (j)) pRecordSettings
    instance' <- peek @(Ptr Instance_T) ((p `plusPtr` 80 :: Ptr (Ptr Instance_T)))
    vulkanApiVersion <- peek @Word32 ((p `plusPtr` 88 :: Ptr Word32))
    pure $ AllocatorCreateInfo
             flags physicalDevice device preferredLargeHeapBlockSize pAllocationCallbacks' pDeviceMemoryCallbacks' frameInUseCount pHeapSizeLimit pVulkanFunctions' pRecordSettings' instance' vulkanApiVersion

instance Zero AllocatorCreateInfo where
  zero = AllocatorCreateInfo
           zero
           zero
           zero
           zero
           Nothing
           Nothing
           zero
           zero
           Nothing
           Nothing
           zero
           zero


-- | VmaAllocatorInfo
--
-- Information about existing 'Allocator' object.
data AllocatorInfo = AllocatorInfo
  { -- | Handle to Vulkan instance object.
    --
    -- This is the same value as has been passed through
    -- /VmaAllocatorCreateInfo::instance/.
    instance' :: Ptr Instance_T
  , -- | Handle to Vulkan physical device object.
    --
    -- This is the same value as has been passed through
    -- /VmaAllocatorCreateInfo::physicalDevice/.
    physicalDevice :: Ptr PhysicalDevice_T
  , -- | Handle to Vulkan device object.
    --
    -- This is the same value as has been passed through
    -- /VmaAllocatorCreateInfo::device/.
    device :: Ptr Device_T
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AllocatorInfo)
#endif
deriving instance Show AllocatorInfo

instance ToCStruct AllocatorInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AllocatorInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr (Ptr Instance_T))) (instance')
    poke ((p `plusPtr` 8 :: Ptr (Ptr PhysicalDevice_T))) (physicalDevice)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Device_T))) (device)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr (Ptr Instance_T))) (zero)
    poke ((p `plusPtr` 8 :: Ptr (Ptr PhysicalDevice_T))) (zero)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Device_T))) (zero)
    f

instance FromCStruct AllocatorInfo where
  peekCStruct p = do
    instance' <- peek @(Ptr Instance_T) ((p `plusPtr` 0 :: Ptr (Ptr Instance_T)))
    physicalDevice <- peek @(Ptr PhysicalDevice_T) ((p `plusPtr` 8 :: Ptr (Ptr PhysicalDevice_T)))
    device <- peek @(Ptr Device_T) ((p `plusPtr` 16 :: Ptr (Ptr Device_T)))
    pure $ AllocatorInfo
             instance' physicalDevice device

instance Storable AllocatorInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AllocatorInfo where
  zero = AllocatorInfo
           zero
           zero
           zero


-- | VmaStatInfo
--
-- Calculated statistics of memory usage in entire allocator.
data StatInfo = StatInfo
  { -- | Number of @VkDeviceMemory@ Vulkan memory blocks allocated.
    blockCount :: Word32
  , -- | Number of 'Allocation' allocation objects allocated.
    allocationCount :: Word32
  , -- | Number of free ranges of memory between allocations.
    unusedRangeCount :: Word32
  , -- | Total number of bytes occupied by all allocations.
    usedBytes :: DeviceSize
  , -- | Total number of bytes occupied by unused ranges.
    unusedBytes :: DeviceSize
  , 
    allocationSizeMin :: DeviceSize
  , 
    allocationSizeAvg :: DeviceSize
  , 
    allocationSizeMax :: DeviceSize
  , 
    unusedRangeSizeMin :: DeviceSize
  , 
    unusedRangeSizeAvg :: DeviceSize
  , 
    unusedRangeSizeMax :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (StatInfo)
#endif
deriving instance Show StatInfo

instance ToCStruct StatInfo where
  withCStruct x f = allocaBytesAligned 80 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p StatInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (blockCount)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (allocationCount)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (unusedRangeCount)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (usedBytes)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (unusedBytes)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (allocationSizeMin)
    poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (allocationSizeAvg)
    poke ((p `plusPtr` 48 :: Ptr DeviceSize)) (allocationSizeMax)
    poke ((p `plusPtr` 56 :: Ptr DeviceSize)) (unusedRangeSizeMin)
    poke ((p `plusPtr` 64 :: Ptr DeviceSize)) (unusedRangeSizeAvg)
    poke ((p `plusPtr` 72 :: Ptr DeviceSize)) (unusedRangeSizeMax)
    f
  cStructSize = 80
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 48 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 56 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 64 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 72 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct StatInfo where
  peekCStruct p = do
    blockCount <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    allocationCount <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    unusedRangeCount <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    usedBytes <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    unusedBytes <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    allocationSizeMin <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    allocationSizeAvg <- peek @DeviceSize ((p `plusPtr` 40 :: Ptr DeviceSize))
    allocationSizeMax <- peek @DeviceSize ((p `plusPtr` 48 :: Ptr DeviceSize))
    unusedRangeSizeMin <- peek @DeviceSize ((p `plusPtr` 56 :: Ptr DeviceSize))
    unusedRangeSizeAvg <- peek @DeviceSize ((p `plusPtr` 64 :: Ptr DeviceSize))
    unusedRangeSizeMax <- peek @DeviceSize ((p `plusPtr` 72 :: Ptr DeviceSize))
    pure $ StatInfo
             blockCount allocationCount unusedRangeCount usedBytes unusedBytes allocationSizeMin allocationSizeAvg allocationSizeMax unusedRangeSizeMin unusedRangeSizeAvg unusedRangeSizeMax

instance Storable StatInfo where
  sizeOf ~_ = 80
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero StatInfo where
  zero = StatInfo
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


-- | VmaStats
--
-- -   'StatInfo' /memoryType/ [VK_MAX_MEMORY_TYPES]
--
-- -   'StatInfo' /memoryHeap/ [VK_MAX_MEMORY_HEAPS]
--
-- -   'StatInfo' /total/
--
-- General statistics from current state of Allocator.
--
-- === memoryHeap
--
-- memoryHeap
-- VmaStats
-- VmaStats
-- memoryHeap
-- @VmaStatInfo VmaStats::memoryHeap[VK_MAX_MEMORY_HEAPS]@
--
-- === memoryType
--
-- memoryType
-- VmaStats
-- VmaStats
-- memoryType
-- @VmaStatInfo VmaStats::memoryType[VK_MAX_MEMORY_TYPES]@
data Stats = Stats
  { -- No documentation found for Nested "VmaStats" "memoryType"
    memoryType :: Vector StatInfo
  , -- No documentation found for Nested "VmaStats" "memoryHeap"
    memoryHeap :: Vector StatInfo
  , 
    total :: StatInfo
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (Stats)
#endif
deriving instance Show Stats

instance ToCStruct Stats where
  withCStruct x f = allocaBytesAligned 3920 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p Stats{..} f = do
    unless ((Data.Vector.length $ (memoryType)) <= MAX_MEMORY_TYPES) $
      throwIO $ IOError Nothing InvalidArgument "" "memoryType is too long, a maximum of MAX_MEMORY_TYPES elements are allowed" Nothing Nothing
    Data.Vector.imapM_ (\i e -> poke ((lowerArrayPtr ((p `plusPtr` 0 :: Ptr (FixedArray MAX_MEMORY_TYPES StatInfo)))) `plusPtr` (80 * (i)) :: Ptr StatInfo) (e)) (memoryType)
    unless ((Data.Vector.length $ (memoryHeap)) <= MAX_MEMORY_HEAPS) $
      throwIO $ IOError Nothing InvalidArgument "" "memoryHeap is too long, a maximum of MAX_MEMORY_HEAPS elements are allowed" Nothing Nothing
    Data.Vector.imapM_ (\i e -> poke ((lowerArrayPtr ((p `plusPtr` 2560 :: Ptr (FixedArray MAX_MEMORY_HEAPS StatInfo)))) `plusPtr` (80 * (i)) :: Ptr StatInfo) (e)) (memoryHeap)
    poke ((p `plusPtr` 3840 :: Ptr StatInfo)) (total)
    f
  cStructSize = 3920
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    unless ((Data.Vector.length $ (mempty)) <= MAX_MEMORY_TYPES) $
      throwIO $ IOError Nothing InvalidArgument "" "memoryType is too long, a maximum of MAX_MEMORY_TYPES elements are allowed" Nothing Nothing
    Data.Vector.imapM_ (\i e -> poke ((lowerArrayPtr ((p `plusPtr` 0 :: Ptr (FixedArray MAX_MEMORY_TYPES StatInfo)))) `plusPtr` (80 * (i)) :: Ptr StatInfo) (e)) (mempty)
    unless ((Data.Vector.length $ (mempty)) <= MAX_MEMORY_HEAPS) $
      throwIO $ IOError Nothing InvalidArgument "" "memoryHeap is too long, a maximum of MAX_MEMORY_HEAPS elements are allowed" Nothing Nothing
    Data.Vector.imapM_ (\i e -> poke ((lowerArrayPtr ((p `plusPtr` 2560 :: Ptr (FixedArray MAX_MEMORY_HEAPS StatInfo)))) `plusPtr` (80 * (i)) :: Ptr StatInfo) (e)) (mempty)
    poke ((p `plusPtr` 3840 :: Ptr StatInfo)) (zero)
    f

instance FromCStruct Stats where
  peekCStruct p = do
    memoryType <- generateM (MAX_MEMORY_TYPES) (\i -> peekCStruct @StatInfo (((lowerArrayPtr @StatInfo ((p `plusPtr` 0 :: Ptr (FixedArray MAX_MEMORY_TYPES StatInfo)))) `advancePtrBytes` (80 * (i)) :: Ptr StatInfo)))
    memoryHeap <- generateM (MAX_MEMORY_HEAPS) (\i -> peekCStruct @StatInfo (((lowerArrayPtr @StatInfo ((p `plusPtr` 2560 :: Ptr (FixedArray MAX_MEMORY_HEAPS StatInfo)))) `advancePtrBytes` (80 * (i)) :: Ptr StatInfo)))
    total <- peekCStruct @StatInfo ((p `plusPtr` 3840 :: Ptr StatInfo))
    pure $ Stats
             memoryType memoryHeap total

instance Storable Stats where
  sizeOf ~_ = 3920
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero Stats where
  zero = Stats
           mempty
           mempty
           zero


-- | VmaBudget
--
-- Statistics of current memory usage and available budget, in bytes, for
-- specific memory heap.
data Budget = Budget
  { -- | Sum size of all @VkDeviceMemory@ blocks allocated from particular heap,
    -- in bytes.
    blockBytes :: DeviceSize
  , -- | Sum size of all allocations created in particular heap, in bytes.
    --
    -- Usually less or equal than @blockBytes@. Difference
    -- @blockBytes - allocationBytes@ is the amount of memory allocated but
    -- unused - available for new allocations or wasted due to fragmentation.
    --
    -- It might be greater than @blockBytes@ if there are some allocations in
    -- lost state, as they account to this value as well.
    allocationBytes :: DeviceSize
  , -- | Estimated current memory usage of the program, in bytes.
    --
    -- Fetched from system using @VK_EXT_memory_budget@ extension if enabled.
    --
    -- It might be different than @blockBytes@ (usually higher) due to
    -- additional implicit objects also occupying the memory, like swapchain,
    -- pipelines, descriptor heaps, command buffers, or @VkDeviceMemory@ blocks
    -- allocated outside of this library, if any.
    usage :: DeviceSize
  , -- | Estimated amount of memory available to the program, in bytes.
    --
    -- Fetched from system using @VK_EXT_memory_budget@ extension if enabled.
    --
    -- It might be different (most probably smaller) than
    -- @VkMemoryHeap::size[heapIndex]@ due to factors external to the program,
    -- like other programs also consuming system resources. Difference
    -- @budget - usage@ is the amount of additional memory that can probably be
    -- allocated without problems. Exceeding the budget may result in various
    -- problems.
    budget :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (Budget)
#endif
deriving instance Show Budget

instance ToCStruct Budget where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p Budget{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceSize)) (blockBytes)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (allocationBytes)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (usage)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (budget)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct Budget where
  peekCStruct p = do
    blockBytes <- peek @DeviceSize ((p `plusPtr` 0 :: Ptr DeviceSize))
    allocationBytes <- peek @DeviceSize ((p `plusPtr` 8 :: Ptr DeviceSize))
    usage <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    budget <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    pure $ Budget
             blockBytes allocationBytes usage budget

instance Storable Budget where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero Budget where
  zero = Budget
           zero
           zero
           zero
           zero


-- | VmaPool
--
-- Represents custom memory pool.
--
-- Fill structure 'PoolCreateInfo' and call function 'createPool' to create
-- it. Call function 'destroyPool' to destroy it.
--
-- For more information see /Custom memory pools/.
newtype Pool = Pool Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show Pool where
  showsPrec p (Pool x) = showParen (p >= 11) (showString "Pool 0x" . showHex x)



newtype MemoryUsage = MemoryUsage Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | No intended memory usage specified. Use other members of
-- 'AllocationCreateInfo' to specify your requirements.
pattern MEMORY_USAGE_UNKNOWN              = MemoryUsage 0
-- | Memory will be used on device only, so fast access from the device is
-- preferred. It usually means device-local GPU (video) memory. No need to
-- be mappable on host. It is roughly equivalent of
-- @D3D12_HEAP_TYPE_DEFAULT@.
--
-- Usage:
--
-- -   Resources written and read by device, e.g. images used as
--     attachments.
--
-- -   Resources transferred from host once (immutable) or infrequently and
--     read by device multiple times, e.g. textures to be sampled, vertex
--     buffers, uniform (constant) buffers, and majority of other types of
--     resources used on GPU.
--
-- Allocation may still end up in @HOST_VISIBLE@ memory on some
-- implementations. In such case, you are free to map it. You can use
-- 'ALLOCATION_CREATE_MAPPED_BIT' with this usage type.
pattern MEMORY_USAGE_GPU_ONLY             = MemoryUsage 1
-- | Memory will be mappable on host. It usually means CPU (system) memory.
-- Guarantees to be @HOST_VISIBLE@ and @HOST_COHERENT@. CPU access is
-- typically uncached. Writes may be write-combined. Resources created in
-- this pool may still be accessible to the device, but access to them can
-- be slow. It is roughly equivalent of @D3D12_HEAP_TYPE_UPLOAD@.
--
-- Usage: Staging copy of resources used as transfer source.
pattern MEMORY_USAGE_CPU_ONLY             = MemoryUsage 2
-- | Memory that is both mappable on host (guarantees to be @HOST_VISIBLE@)
-- and preferably fast to access by GPU. CPU access is typically uncached.
-- Writes may be write-combined.
--
-- Usage: Resources written frequently by host (dynamic), read by device.
-- E.g. textures (with LINEAR layout), vertex buffers, uniform buffers
-- updated every frame or every draw call.
pattern MEMORY_USAGE_CPU_TO_GPU           = MemoryUsage 3
-- | Memory mappable on host (guarantees to be @HOST_VISIBLE@) and cached. It
-- is roughly equivalent of @D3D12_HEAP_TYPE_READBACK@.
--
-- Usage:
--
-- -   Resources written by device, read by host - results of some
--     computations, e.g. screen capture, average scene luminance for HDR
--     tone mapping.
--
-- -   Any resources read or accessed randomly on host, e.g. CPU-side copy
--     of vertex buffer used as source of transfer, but also used for
--     collision detection.
pattern MEMORY_USAGE_GPU_TO_CPU           = MemoryUsage 4
-- | CPU memory - memory that is preferably not @DEVICE_LOCAL@, but also not
-- guaranteed to be @HOST_VISIBLE@.
--
-- Usage: Staging copy of resources moved from GPU memory to CPU memory as
-- part of custom paging\/residency mechanism, to be moved back to GPU
-- memory when needed.
pattern MEMORY_USAGE_CPU_COPY             = MemoryUsage 5
-- | Lazily allocated GPU memory having
-- @VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT@. Exists mostly on mobile
-- platforms. Using it on desktop PC or other GPUs with no such memory type
-- present will fail the allocation.
--
-- Usage: Memory for transient attachment images (color attachments, depth
-- attachments etc.), created with
-- @VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT@.
--
-- Allocations with this usage are always created as dedicated - it implies
-- 'ALLOCATION_CREATE_DEDICATED_MEMORY_BIT'.
pattern MEMORY_USAGE_GPU_LAZILY_ALLOCATED = MemoryUsage 6
{-# complete MEMORY_USAGE_UNKNOWN,
             MEMORY_USAGE_GPU_ONLY,
             MEMORY_USAGE_CPU_ONLY,
             MEMORY_USAGE_CPU_TO_GPU,
             MEMORY_USAGE_GPU_TO_CPU,
             MEMORY_USAGE_CPU_COPY,
             MEMORY_USAGE_GPU_LAZILY_ALLOCATED :: MemoryUsage #-}

conNameMemoryUsage :: String
conNameMemoryUsage = "MemoryUsage"

enumPrefixMemoryUsage :: String
enumPrefixMemoryUsage = "MEMORY_USAGE_"

showTableMemoryUsage :: [(MemoryUsage, String)]
showTableMemoryUsage =
  [ (MEMORY_USAGE_UNKNOWN             , "UNKNOWN")
  , (MEMORY_USAGE_GPU_ONLY            , "GPU_ONLY")
  , (MEMORY_USAGE_CPU_ONLY            , "CPU_ONLY")
  , (MEMORY_USAGE_CPU_TO_GPU          , "CPU_TO_GPU")
  , (MEMORY_USAGE_GPU_TO_CPU          , "GPU_TO_CPU")
  , (MEMORY_USAGE_CPU_COPY            , "CPU_COPY")
  , (MEMORY_USAGE_GPU_LAZILY_ALLOCATED, "GPU_LAZILY_ALLOCATED")
  ]

instance Show MemoryUsage where
  showsPrec =
    enumShowsPrec enumPrefixMemoryUsage showTableMemoryUsage conNameMemoryUsage (\(MemoryUsage x) -> x) (showsPrec 11)

instance Read MemoryUsage where
  readPrec = enumReadPrec enumPrefixMemoryUsage showTableMemoryUsage conNameMemoryUsage MemoryUsage


type AllocationCreateFlags = AllocationCreateFlagBits

-- | Flags to be passed as /VmaAllocationCreateInfo::flags/.
newtype AllocationCreateFlagBits = AllocationCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | Set this flag if the allocation should have its own memory block.
--
-- Use it for special, big resources, like fullscreen images used as
-- attachments.
--
-- You should not use this flag if /VmaAllocationCreateInfo::pool/ is not
-- null.
pattern ALLOCATION_CREATE_DEDICATED_MEMORY_BIT           = AllocationCreateFlagBits 0x00000001
-- | Set this flag to only try to allocate from existing @VkDeviceMemory@
-- blocks and never create new such block.
--
-- If new allocation cannot be placed in any of the existing blocks,
-- allocation fails with @VK_ERROR_OUT_OF_DEVICE_MEMORY@ error.
--
-- You should not use 'ALLOCATION_CREATE_DEDICATED_MEMORY_BIT' and
-- 'ALLOCATION_CREATE_NEVER_ALLOCATE_BIT' at the same time. It makes no
-- sense.
--
-- If /VmaAllocationCreateInfo::pool/ is not null, this flag is implied and
-- ignored.
pattern ALLOCATION_CREATE_NEVER_ALLOCATE_BIT             = AllocationCreateFlagBits 0x00000002
-- | Set this flag to use a memory that will be persistently mapped and
-- retrieve pointer to it.
--
-- Pointer to mapped memory will be returned through
-- /VmaAllocationInfo::pMappedData/.
--
-- It is valid to use this flag for allocation made from memory type that
-- is not @HOST_VISIBLE@. This flag is then ignored and memory is not
-- mapped. This is useful if you need an allocation that is efficient to
-- use on GPU (@DEVICE_LOCAL@) and still want to map it directly if
-- possible on platforms that support it (e.g. Intel GPU).
--
-- You should not use this flag together with
-- 'ALLOCATION_CREATE_CAN_BECOME_LOST_BIT'.
pattern ALLOCATION_CREATE_MAPPED_BIT                     = AllocationCreateFlagBits 0x00000004
-- | Allocation created with this flag can become lost as a result of another
-- allocation with 'ALLOCATION_CREATE_CAN_MAKE_OTHER_LOST_BIT' flag, so you
-- must check it before use.
--
-- To check if allocation is not lost, call 'getAllocationInfo' and check
-- if /VmaAllocationInfo::deviceMemory/ is not @VK_NULL_HANDLE@.
--
-- For details about supporting lost allocations, see Lost Allocations
-- chapter of User Guide on Main Page.
--
-- You should not use this flag together with
-- 'ALLOCATION_CREATE_MAPPED_BIT'.
pattern ALLOCATION_CREATE_CAN_BECOME_LOST_BIT            = AllocationCreateFlagBits 0x00000008
-- | While creating allocation using this flag, other allocations that were
-- created with flag 'ALLOCATION_CREATE_CAN_BECOME_LOST_BIT' can become
-- lost.
--
-- For details about supporting lost allocations, see Lost Allocations
-- chapter of User Guide on Main Page.
pattern ALLOCATION_CREATE_CAN_MAKE_OTHER_LOST_BIT        = AllocationCreateFlagBits 0x00000010
-- | Set this flag to treat /VmaAllocationCreateInfo::pUserData/ as pointer
-- to a null-terminated string. Instead of copying pointer value, a local
-- copy of the string is made and stored in allocation\'s @pUserData@. The
-- string is automatically freed together with the allocation. It is also
-- used in 'buildStatsString'.
pattern ALLOCATION_CREATE_USER_DATA_COPY_STRING_BIT      = AllocationCreateFlagBits 0x00000020
-- | Allocation will be created from upper stack in a double stack pool.
--
-- This flag is only allowed for custom pools created with
-- 'POOL_CREATE_LINEAR_ALGORITHM_BIT' flag.
pattern ALLOCATION_CREATE_UPPER_ADDRESS_BIT              = AllocationCreateFlagBits 0x00000040
-- | Create both buffer\/image and allocation, but don\'t bind them together.
-- It is useful when you want to bind yourself to do some more advanced
-- binding, e.g. using some extensions. The flag is meaningful only with
-- functions that bind by default: 'createBuffer', 'createImage'. Otherwise
-- it is ignored.
pattern ALLOCATION_CREATE_DONT_BIND_BIT                  = AllocationCreateFlagBits 0x00000080
-- | Create allocation only if additional device memory required for it, if
-- any, won\'t exceed memory budget. Otherwise return
-- @VK_ERROR_OUT_OF_DEVICE_MEMORY@.
pattern ALLOCATION_CREATE_WITHIN_BUDGET_BIT              = AllocationCreateFlagBits 0x00000100
-- | Allocation strategy that chooses smallest possible free range for the
-- allocation.
pattern ALLOCATION_CREATE_STRATEGY_BEST_FIT_BIT          = AllocationCreateFlagBits 0x00010000
-- | Allocation strategy that chooses biggest possible free range for the
-- allocation.
pattern ALLOCATION_CREATE_STRATEGY_WORST_FIT_BIT         = AllocationCreateFlagBits 0x00020000
-- | Allocation strategy that chooses first suitable free range for the
-- allocation.
--
-- \"First\" doesn\'t necessarily means the one with smallest offset in
-- memory, but rather the one that is easiest and fastest to find.
pattern ALLOCATION_CREATE_STRATEGY_FIRST_FIT_BIT         = AllocationCreateFlagBits 0x00040000
-- | Allocation strategy that tries to minimize memory usage.
pattern ALLOCATION_CREATE_STRATEGY_MIN_MEMORY_BIT        = AllocationCreateFlagBits 0x00010000
-- | Allocation strategy that tries to minimize allocation time.
pattern ALLOCATION_CREATE_STRATEGY_MIN_TIME_BIT          = AllocationCreateFlagBits 0x00040000
-- | Allocation strategy that tries to minimize memory fragmentation.
pattern ALLOCATION_CREATE_STRATEGY_MIN_FRAGMENTATION_BIT = AllocationCreateFlagBits 0x00020000
-- | A bit mask to extract only @STRATEGY@ bits from entire set of flags.
pattern ALLOCATION_CREATE_STRATEGY_MASK                  = AllocationCreateFlagBits 0x00070000

conNameAllocationCreateFlagBits :: String
conNameAllocationCreateFlagBits = "AllocationCreateFlagBits"

enumPrefixAllocationCreateFlagBits :: String
enumPrefixAllocationCreateFlagBits = "ALLOCATION_CREATE_"

showTableAllocationCreateFlagBits :: [(AllocationCreateFlagBits, String)]
showTableAllocationCreateFlagBits =
  [ (ALLOCATION_CREATE_DEDICATED_MEMORY_BIT          , "DEDICATED_MEMORY_BIT")
  , (ALLOCATION_CREATE_NEVER_ALLOCATE_BIT            , "NEVER_ALLOCATE_BIT")
  , (ALLOCATION_CREATE_MAPPED_BIT                    , "MAPPED_BIT")
  , (ALLOCATION_CREATE_CAN_BECOME_LOST_BIT           , "CAN_BECOME_LOST_BIT")
  , (ALLOCATION_CREATE_CAN_MAKE_OTHER_LOST_BIT       , "CAN_MAKE_OTHER_LOST_BIT")
  , (ALLOCATION_CREATE_USER_DATA_COPY_STRING_BIT     , "USER_DATA_COPY_STRING_BIT")
  , (ALLOCATION_CREATE_UPPER_ADDRESS_BIT             , "UPPER_ADDRESS_BIT")
  , (ALLOCATION_CREATE_DONT_BIND_BIT                 , "DONT_BIND_BIT")
  , (ALLOCATION_CREATE_WITHIN_BUDGET_BIT             , "WITHIN_BUDGET_BIT")
  , (ALLOCATION_CREATE_STRATEGY_BEST_FIT_BIT         , "STRATEGY_BEST_FIT_BIT")
  , (ALLOCATION_CREATE_STRATEGY_WORST_FIT_BIT        , "STRATEGY_WORST_FIT_BIT")
  , (ALLOCATION_CREATE_STRATEGY_FIRST_FIT_BIT        , "STRATEGY_FIRST_FIT_BIT")
  , (ALLOCATION_CREATE_STRATEGY_MIN_MEMORY_BIT       , "STRATEGY_MIN_MEMORY_BIT")
  , (ALLOCATION_CREATE_STRATEGY_MIN_TIME_BIT         , "STRATEGY_MIN_TIME_BIT")
  , (ALLOCATION_CREATE_STRATEGY_MIN_FRAGMENTATION_BIT, "STRATEGY_MIN_FRAGMENTATION_BIT")
  , (ALLOCATION_CREATE_STRATEGY_MASK                 , "STRATEGY_MASK")
  ]

instance Show AllocationCreateFlagBits where
  showsPrec = enumShowsPrec enumPrefixAllocationCreateFlagBits
                            showTableAllocationCreateFlagBits
                            conNameAllocationCreateFlagBits
                            (\(AllocationCreateFlagBits x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read AllocationCreateFlagBits where
  readPrec = enumReadPrec enumPrefixAllocationCreateFlagBits
                          showTableAllocationCreateFlagBits
                          conNameAllocationCreateFlagBits
                          AllocationCreateFlagBits


-- | VmaAllocationCreateInfo
data AllocationCreateInfo = AllocationCreateInfo
  { -- | Use 'AllocationCreateFlagBits' enum.
    flags :: AllocationCreateFlags
  , -- | Intended usage of memory.
    --
    -- You can leave 'MEMORY_USAGE_UNKNOWN' if you specify memory requirements
    -- in other way.
    --
    -- >  
    --
    -- If @pool@ is not null, this member is ignored.
    usage :: MemoryUsage
  , -- | Flags that must be set in a Memory Type chosen for an allocation.
    --
    -- Leave 0 if you specify memory requirements in other way.
    --
    -- >  
    --
    -- If @pool@ is not null, this member is ignored.
    requiredFlags :: MemoryPropertyFlags
  , -- | Flags that preferably should be set in a memory type chosen for an
    -- allocation.
    --
    -- Set to 0 if no additional flags are preferred.
    --
    -- >  
    --
    -- If @pool@ is not null, this member is ignored.
    preferredFlags :: MemoryPropertyFlags
  , -- | Bitmask containing one bit set for every memory type acceptable for this
    -- allocation.
    --
    -- Value 0 is equivalent to @UINT32_MAX@ - it means any memory type is
    -- accepted if it meets other requirements specified by this structure,
    -- with no further restrictions on memory type index.
    --
    -- >  
    --
    -- If @pool@ is not null, this member is ignored.
    memoryTypeBits :: Word32
  , -- | Pool that this allocation should be created in.
    --
    -- Leave @VK_NULL_HANDLE@ to allocate from default pool. If not null,
    -- members: @usage@, @requiredFlags@, @preferredFlags@, @memoryTypeBits@
    -- are ignored.
    pool :: Pool
  , -- | Custom general-purpose pointer that will be stored in 'Allocation', can
    -- be read as /VmaAllocationInfo::pUserData/ and changed using
    -- 'setAllocationUserData'.
    --
    -- If 'ALLOCATION_CREATE_USER_DATA_COPY_STRING_BIT' is used, it must be
    -- either null or pointer to a null-terminated string. The string will be
    -- then copied to internal buffer, so it doesn\'t need to be valid after
    -- allocation call.
    userData :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AllocationCreateInfo)
#endif
deriving instance Show AllocationCreateInfo

instance ToCStruct AllocationCreateInfo where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AllocationCreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr AllocationCreateFlags)) (flags)
    poke ((p `plusPtr` 4 :: Ptr MemoryUsage)) (usage)
    poke ((p `plusPtr` 8 :: Ptr MemoryPropertyFlags)) (requiredFlags)
    poke ((p `plusPtr` 12 :: Ptr MemoryPropertyFlags)) (preferredFlags)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (memoryTypeBits)
    poke ((p `plusPtr` 24 :: Ptr Pool)) (pool)
    poke ((p `plusPtr` 32 :: Ptr (Ptr ()))) (userData)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr AllocationCreateFlags)) (zero)
    poke ((p `plusPtr` 4 :: Ptr MemoryUsage)) (zero)
    poke ((p `plusPtr` 8 :: Ptr MemoryPropertyFlags)) (zero)
    poke ((p `plusPtr` 12 :: Ptr MemoryPropertyFlags)) (zero)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct AllocationCreateInfo where
  peekCStruct p = do
    flags <- peek @AllocationCreateFlags ((p `plusPtr` 0 :: Ptr AllocationCreateFlags))
    usage <- peek @MemoryUsage ((p `plusPtr` 4 :: Ptr MemoryUsage))
    requiredFlags <- peek @MemoryPropertyFlags ((p `plusPtr` 8 :: Ptr MemoryPropertyFlags))
    preferredFlags <- peek @MemoryPropertyFlags ((p `plusPtr` 12 :: Ptr MemoryPropertyFlags))
    memoryTypeBits <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pool <- peek @Pool ((p `plusPtr` 24 :: Ptr Pool))
    pUserData <- peek @(Ptr ()) ((p `plusPtr` 32 :: Ptr (Ptr ())))
    pure $ AllocationCreateInfo
             flags usage requiredFlags preferredFlags memoryTypeBits pool pUserData

instance Storable AllocationCreateInfo where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AllocationCreateInfo where
  zero = AllocationCreateInfo
           zero
           zero
           zero
           zero
           zero
           zero
           zero


type PoolCreateFlags = PoolCreateFlagBits

-- | Flags to be passed as /VmaPoolCreateInfo::flags/.
newtype PoolCreateFlagBits = PoolCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | Use this flag if you always allocate only buffers and linear images or
-- only optimal images out of this pool and so Buffer-Image Granularity can
-- be ignored.
--
-- This is an optional optimization flag.
--
-- If you always allocate using 'createBuffer', 'createImage',
-- 'allocateMemoryForBuffer', then you don\'t need to use it because
-- allocator knows exact type of your allocations so it can handle
-- Buffer-Image Granularity in the optimal way.
--
-- If you also allocate using 'allocateMemoryForImage' or 'allocateMemory',
-- exact type of such allocations is not known, so allocator must be
-- conservative in handling Buffer-Image Granularity, which can lead to
-- suboptimal allocation (wasted memory). In that case, if you can make
-- sure you always allocate only buffers and linear images or only optimal
-- images out of this pool, use this flag to make allocator disregard
-- Buffer-Image Granularity and so make allocations faster and more
-- optimal.
pattern POOL_CREATE_IGNORE_BUFFER_IMAGE_GRANULARITY_BIT = PoolCreateFlagBits 0x00000002
-- | Enables alternative, linear allocation algorithm in this pool.
--
-- Specify this flag to enable linear allocation algorithm, which always
-- creates new allocations after last one and doesn\'t reuse space from
-- allocations freed in between. It trades memory consumption for
-- simplified algorithm and data structure, which has better performance
-- and uses less memory for metadata.
--
-- By using this flag, you can achieve behavior of free-at-once, stack,
-- ring buffer, and double stack. For details, see documentation chapter
-- /Linear allocation algorithm/.
--
-- When using this flag, you must specify
-- /VmaPoolCreateInfo::maxBlockCount/ == 1 (or 0 for default).
--
-- For more details, see /Linear allocation algorithm/.
pattern POOL_CREATE_LINEAR_ALGORITHM_BIT                = PoolCreateFlagBits 0x00000004
-- | Enables alternative, buddy allocation algorithm in this pool.
--
-- It operates on a tree of blocks, each having size that is a power of two
-- and a half of its parent\'s size. Comparing to default algorithm, this
-- one provides faster allocation and deallocation and decreased external
-- fragmentation, at the expense of more memory wasted (internal
-- fragmentation).
--
-- For more details, see /Buddy allocation algorithm/.
pattern POOL_CREATE_BUDDY_ALGORITHM_BIT                 = PoolCreateFlagBits 0x00000008
-- | Bit mask to extract only @ALGORITHM@ bits from entire set of flags.
pattern POOL_CREATE_ALGORITHM_MASK                      = PoolCreateFlagBits 0x0000000c

conNamePoolCreateFlagBits :: String
conNamePoolCreateFlagBits = "PoolCreateFlagBits"

enumPrefixPoolCreateFlagBits :: String
enumPrefixPoolCreateFlagBits = "POOL_CREATE_"

showTablePoolCreateFlagBits :: [(PoolCreateFlagBits, String)]
showTablePoolCreateFlagBits =
  [ (POOL_CREATE_IGNORE_BUFFER_IMAGE_GRANULARITY_BIT, "IGNORE_BUFFER_IMAGE_GRANULARITY_BIT")
  , (POOL_CREATE_LINEAR_ALGORITHM_BIT               , "LINEAR_ALGORITHM_BIT")
  , (POOL_CREATE_BUDDY_ALGORITHM_BIT                , "BUDDY_ALGORITHM_BIT")
  , (POOL_CREATE_ALGORITHM_MASK                     , "ALGORITHM_MASK")
  ]

instance Show PoolCreateFlagBits where
  showsPrec = enumShowsPrec enumPrefixPoolCreateFlagBits
                            showTablePoolCreateFlagBits
                            conNamePoolCreateFlagBits
                            (\(PoolCreateFlagBits x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read PoolCreateFlagBits where
  readPrec =
    enumReadPrec enumPrefixPoolCreateFlagBits showTablePoolCreateFlagBits conNamePoolCreateFlagBits PoolCreateFlagBits


-- | VmaPoolCreateInfo
--
-- Describes parameter of created 'Pool'.
data PoolCreateInfo = PoolCreateInfo
  { -- | Vulkan memory type index to allocate this pool from.
    memoryTypeIndex :: Word32
  , -- | Use combination of 'PoolCreateFlagBits'.
    flags :: PoolCreateFlags
  , -- | Size of a single @VkDeviceMemory@ block to be allocated as part of this
    -- pool, in bytes. Optional.
    --
    -- Specify nonzero to set explicit, constant size of memory blocks used by
    -- this pool.
    --
    -- Leave 0 to use default and let the library manage block sizes
    -- automatically. Sizes of particular blocks may vary.
    blockSize :: DeviceSize
  , -- | Minimum number of blocks to be always allocated in this pool, even if
    -- they stay empty.
    --
    -- Set to 0 to have no preallocated blocks and allow the pool be completely
    -- empty.
    minBlockCount :: Word64
  , -- | Maximum number of blocks that can be allocated in this pool. Optional.
    --
    -- Set to 0 to use default, which is @SIZE_MAX@, which means no limit.
    --
    -- Set to same value as /VmaPoolCreateInfo::minBlockCount/ to have fixed
    -- amount of memory allocated throughout whole lifetime of this pool.
    maxBlockCount :: Word64
  , -- | Maximum number of additional frames that are in use at the same time as
    -- current frame.
    --
    -- This value is used only when you make allocations with
    -- 'ALLOCATION_CREATE_CAN_BECOME_LOST_BIT' flag. Such allocation cannot
    -- become lost if allocation.lastUseFrameIndex >=
    -- allocator.currentFrameIndex - frameInUseCount.
    --
    -- For example, if you double-buffer your command buffers, so resources
    -- used for rendering in previous frame may still be in use by the GPU at
    -- the moment you allocate resources needed for the current frame, set this
    -- value to 1.
    --
    -- If you want to allow any allocations other than used in the current
    -- frame to become lost, set this value to 0.
    frameInUseCount :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PoolCreateInfo)
#endif
deriving instance Show PoolCreateInfo

instance ToCStruct PoolCreateInfo where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PoolCreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (memoryTypeIndex)
    poke ((p `plusPtr` 4 :: Ptr PoolCreateFlags)) (flags)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (blockSize)
    poke ((p `plusPtr` 16 :: Ptr CSize)) (CSize (minBlockCount))
    poke ((p `plusPtr` 24 :: Ptr CSize)) (CSize (maxBlockCount))
    poke ((p `plusPtr` 32 :: Ptr Word32)) (frameInUseCount)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr PoolCreateFlags)) (zero)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 16 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 24 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    f

instance FromCStruct PoolCreateInfo where
  peekCStruct p = do
    memoryTypeIndex <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    flags <- peek @PoolCreateFlags ((p `plusPtr` 4 :: Ptr PoolCreateFlags))
    blockSize <- peek @DeviceSize ((p `plusPtr` 8 :: Ptr DeviceSize))
    minBlockCount <- peek @CSize ((p `plusPtr` 16 :: Ptr CSize))
    maxBlockCount <- peek @CSize ((p `plusPtr` 24 :: Ptr CSize))
    frameInUseCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pure $ PoolCreateInfo
             memoryTypeIndex flags blockSize (coerce @CSize @Word64 minBlockCount) (coerce @CSize @Word64 maxBlockCount) frameInUseCount

instance Storable PoolCreateInfo where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PoolCreateInfo where
  zero = PoolCreateInfo
           zero
           zero
           zero
           zero
           zero
           zero


-- | VmaPoolStats
--
-- Describes parameter of existing 'Pool'.
data PoolStats = PoolStats
  { -- | Total amount of @VkDeviceMemory@ allocated from Vulkan for this pool, in
    -- bytes.
    size :: DeviceSize
  , -- | Total number of bytes in the pool not used by any 'Allocation'.
    unusedSize :: DeviceSize
  , -- | Number of 'Allocation' objects created from this pool that were not
    -- destroyed or lost.
    allocationCount :: Word64
  , -- | Number of continuous memory ranges in the pool not used by any
    -- 'Allocation'.
    unusedRangeCount :: Word64
  , -- | Size of the largest continuous free memory region available for new
    -- allocation.
    --
    -- Making a new allocation of that size is not guaranteed to succeed
    -- because of possible additional margin required to respect alignment and
    -- buffer\/image granularity.
    unusedRangeSizeMax :: DeviceSize
  , -- | Number of @VkDeviceMemory@ blocks allocated for this pool.
    blockCount :: Word64
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PoolStats)
#endif
deriving instance Show PoolStats

instance ToCStruct PoolStats where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PoolStats{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceSize)) (size)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (unusedSize)
    poke ((p `plusPtr` 16 :: Ptr CSize)) (CSize (allocationCount))
    poke ((p `plusPtr` 24 :: Ptr CSize)) (CSize (unusedRangeCount))
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (unusedRangeSizeMax)
    poke ((p `plusPtr` 40 :: Ptr CSize)) (CSize (blockCount))
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 16 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 24 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 40 :: Ptr CSize)) (CSize (zero))
    f

instance FromCStruct PoolStats where
  peekCStruct p = do
    size <- peek @DeviceSize ((p `plusPtr` 0 :: Ptr DeviceSize))
    unusedSize <- peek @DeviceSize ((p `plusPtr` 8 :: Ptr DeviceSize))
    allocationCount <- peek @CSize ((p `plusPtr` 16 :: Ptr CSize))
    unusedRangeCount <- peek @CSize ((p `plusPtr` 24 :: Ptr CSize))
    unusedRangeSizeMax <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    blockCount <- peek @CSize ((p `plusPtr` 40 :: Ptr CSize))
    pure $ PoolStats
             size unusedSize (coerce @CSize @Word64 allocationCount) (coerce @CSize @Word64 unusedRangeCount) unusedRangeSizeMax (coerce @CSize @Word64 blockCount)

instance Storable PoolStats where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PoolStats where
  zero = PoolStats
           zero
           zero
           zero
           zero
           zero
           zero


-- | VmaAllocation
--
-- Represents single memory allocation.
--
-- It may be either dedicated block of @VkDeviceMemory@ or a specific
-- region of a bigger block of this type plus unique offset.
--
-- There are multiple ways to create such object. You need to fill
-- structure 'AllocationCreateInfo'. For more information see /Choosing
-- memory type/.
--
-- Although the library provides convenience functions that create Vulkan
-- buffer or image, allocate memory for it and bind them together, binding
-- of the allocation to a buffer or an image is out of scope of the
-- allocation itself. Allocation object can exist without buffer\/image
-- bound, binding can be done manually by the user, and destruction of it
-- can be done independently of destruction of the allocation.
--
-- The object also remembers its size and some other information. To
-- retrieve this information, use function 'getAllocationInfo' and inspect
-- returned structure 'AllocationInfo'.
--
-- Some kinds allocations can be in lost state. For more information, see
-- /Lost allocations/.
newtype Allocation = Allocation Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show Allocation where
  showsPrec p (Allocation x) = showParen (p >= 11) (showString "Allocation 0x" . showHex x)


-- | VmaAllocationInfo
--
-- Parameters of 'Allocation' objects, that can be retrieved using function
-- 'getAllocationInfo'.
data AllocationInfo = AllocationInfo
  { -- | Memory type index that this allocation was allocated from.
    --
    -- It never changes.
    memoryType :: Word32
  , -- | Handle to Vulkan memory object.
    --
    -- Same memory object can be shared by multiple allocations.
    --
    -- It can change after call to 'defragment' if this allocation is passed to
    -- the function, or if allocation is lost.
    --
    -- If the allocation is lost, it is equal to @VK_NULL_HANDLE@.
    deviceMemory :: DeviceMemory
  , -- | Offset in @VkDeviceMemory@ object to the beginning of this allocation,
    -- in bytes. @(deviceMemory, offset)@ pair is unique to this allocation.
    --
    -- You usually don\'t need to use this offset. If you create a buffer or an
    -- image together with the allocation using e.g. function 'createBuffer',
    -- 'createImage', functions that operate on these resources refer to the
    -- beginning of the buffer or image, not entire device memory block.
    -- Functions like 'mapMemory', 'bindBufferMemory' also refer to the
    -- beginning of the allocation and apply this offset automatically.
    --
    -- It can change after call to 'defragment' if this allocation is passed to
    -- the function, or if allocation is lost.
    offset :: DeviceSize
  , -- | Size of this allocation, in bytes.
    --
    -- It never changes, unless allocation is lost.
    --
    -- Note
    --
    -- Allocation size returned in this variable may be greater than the size
    -- requested for the resource e.g. as @VkBufferCreateInfo::size@. Whole
    -- size of the allocation is accessible for operations on memory e.g. using
    -- a pointer after mapping with 'mapMemory', but operations on the resource
    -- e.g. using @vkCmdCopyBuffer@ must be limited to the size of the
    -- resource.
    size :: DeviceSize
  , -- | Pointer to the beginning of this allocation as mapped data.
    --
    -- If the allocation hasn\'t been mapped using 'mapMemory' and hasn\'t been
    -- created with 'ALLOCATION_CREATE_MAPPED_BIT' flag, this value is null.
    --
    -- It can change after call to 'mapMemory', 'unmapMemory'. It can also
    -- change after call to 'defragment' if this allocation is passed to the
    -- function.
    mappedData :: Ptr ()
  , -- | Custom general-purpose pointer that was passed as
    -- /VmaAllocationCreateInfo::pUserData/ or set using
    -- 'setAllocationUserData'.
    --
    -- It can change after call to 'setAllocationUserData' for this allocation.
    userData :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AllocationInfo)
#endif
deriving instance Show AllocationInfo

instance ToCStruct AllocationInfo where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AllocationInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (memoryType)
    poke ((p `plusPtr` 8 :: Ptr DeviceMemory)) (deviceMemory)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (offset)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (size)
    poke ((p `plusPtr` 32 :: Ptr (Ptr ()))) (mappedData)
    poke ((p `plusPtr` 40 :: Ptr (Ptr ()))) (userData)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct AllocationInfo where
  peekCStruct p = do
    memoryType <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    deviceMemory <- peek @DeviceMemory ((p `plusPtr` 8 :: Ptr DeviceMemory))
    offset <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    size <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    pMappedData <- peek @(Ptr ()) ((p `plusPtr` 32 :: Ptr (Ptr ())))
    pUserData <- peek @(Ptr ()) ((p `plusPtr` 40 :: Ptr (Ptr ())))
    pure $ AllocationInfo
             memoryType deviceMemory offset size pMappedData pUserData

instance Storable AllocationInfo where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AllocationInfo where
  zero = AllocationInfo
           zero
           zero
           zero
           zero
           zero
           zero


-- | VmaDefragmentationContext
--
-- Represents Opaque object that represents started defragmentation
-- process.
--
-- Fill structure 'DefragmentationInfo2' and call function
-- 'defragmentationBegin' to create it. Call function 'defragmentationEnd'
-- to destroy it.
newtype DefragmentationContext = DefragmentationContext Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show DefragmentationContext where
  showsPrec p (DefragmentationContext x) = showParen (p >= 11) (showString "DefragmentationContext 0x" . showHex x)


type DefragmentationFlags = DefragmentationFlagBits

-- | Flags to be used in 'defragmentationBegin'. None at the moment. Reserved
-- for future use.
newtype DefragmentationFlagBits = DefragmentationFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)


pattern DEFRAGMENTATION_FLAG_INCREMENTAL = DefragmentationFlagBits 0x00000001

conNameDefragmentationFlagBits :: String
conNameDefragmentationFlagBits = "DefragmentationFlagBits"

enumPrefixDefragmentationFlagBits :: String
enumPrefixDefragmentationFlagBits = "DEFRAGMENTATION_FLAG_INCREMENTAL"

showTableDefragmentationFlagBits :: [(DefragmentationFlagBits, String)]
showTableDefragmentationFlagBits = [(DEFRAGMENTATION_FLAG_INCREMENTAL, "")]

instance Show DefragmentationFlagBits where
  showsPrec = enumShowsPrec enumPrefixDefragmentationFlagBits
                            showTableDefragmentationFlagBits
                            conNameDefragmentationFlagBits
                            (\(DefragmentationFlagBits x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read DefragmentationFlagBits where
  readPrec = enumReadPrec enumPrefixDefragmentationFlagBits
                          showTableDefragmentationFlagBits
                          conNameDefragmentationFlagBits
                          DefragmentationFlagBits


-- | VmaDefragmentationInfo2
--
-- Parameters for defragmentation.
--
-- To be used with function 'defragmentationBegin'.
data DefragmentationInfo2 = DefragmentationInfo2
  { -- | Reserved for future use. Should be 0.
    flags :: DefragmentationFlags
  , -- | Pointer to array of allocations that can be defragmented.
    --
    -- The array should have @allocationCount@ elements. The array should not
    -- contain nulls. Elements in the array should be unique - same allocation
    -- cannot occur twice. It is safe to pass allocations that are in the lost
    -- state - they are ignored. All allocations not present in this array are
    -- considered non-moveable during this defragmentation.
    allocations :: Vector Allocation
  , -- | Optional, output. Pointer to array that will be filled with information
    -- whether the allocation at certain index has been changed during
    -- defragmentation.
    --
    -- The array should have @allocationCount@ elements. You can pass null if
    -- you are not interested in this information.
    allocationsChanged :: Ptr Bool32
  , -- | Either null or pointer to array of pools to be defragmented.
    --
    -- All the allocations in the specified pools can be moved during
    -- defragmentation and there is no way to check if they were really moved
    -- as in @pAllocationsChanged@, so you must query all the allocations in
    -- all these pools for new @VkDeviceMemory@ and offset using
    -- 'getAllocationInfo' if you might need to recreate buffers and images
    -- bound to them.
    --
    -- The array should have @poolCount@ elements. The array should not contain
    -- nulls. Elements in the array should be unique - same pool cannot occur
    -- twice.
    --
    -- Using this array is equivalent to specifying all allocations from the
    -- pools in @pAllocations@. It might be more efficient.
    pools :: Vector Pool
  , -- | Maximum total numbers of bytes that can be copied while moving
    -- allocations to different places using transfers on CPU side, like
    -- @memcpy()@, @memmove()@.
    --
    -- @VK_WHOLE_SIZE@ means no limit.
    maxCpuBytesToMove :: DeviceSize
  , -- | Maximum number of allocations that can be moved to a different place
    -- using transfers on CPU side, like @memcpy()@, @memmove()@.
    --
    -- @UINT32_MAX@ means no limit.
    maxCpuAllocationsToMove :: Word32
  , -- | Maximum total numbers of bytes that can be copied while moving
    -- allocations to different places using transfers on GPU side, posted to
    -- @commandBuffer@.
    --
    -- @VK_WHOLE_SIZE@ means no limit.
    maxGpuBytesToMove :: DeviceSize
  , -- | Maximum number of allocations that can be moved to a different place
    -- using transfers on GPU side, posted to @commandBuffer@.
    --
    -- @UINT32_MAX@ means no limit.
    maxGpuAllocationsToMove :: Word32
  , -- | Optional. Command buffer where GPU copy commands will be posted.
    --
    -- If not null, it must be a valid command buffer handle that supports
    -- Transfer queue type. It must be in the recording state and outside of a
    -- render pass instance. You need to submit it and make sure it finished
    -- execution before calling 'defragmentationEnd'.
    --
    -- Passing null means that only CPU defragmentation will be performed.
    commandBuffer :: Ptr CommandBuffer_T
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DefragmentationInfo2)
#endif
deriving instance Show DefragmentationInfo2

instance ToCStruct DefragmentationInfo2 where
  withCStruct x f = allocaBytesAligned 80 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DefragmentationInfo2{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr DefragmentationFlags)) (flags)
    lift $ poke ((p `plusPtr` 4 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (allocations)) :: Word32))
    pPAllocations' <- ContT $ allocaBytesAligned @Allocation ((Data.Vector.length (allocations)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPAllocations' `plusPtr` (8 * (i)) :: Ptr Allocation) (e)) (allocations)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr Allocation))) (pPAllocations')
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr Bool32))) (allocationsChanged)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (pools)) :: Word32))
    pPPools' <- ContT $ allocaBytesAligned @Pool ((Data.Vector.length (pools)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPPools' `plusPtr` (8 * (i)) :: Ptr Pool) (e)) (pools)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr Pool))) (pPPools')
    lift $ poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (maxCpuBytesToMove)
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) (maxCpuAllocationsToMove)
    lift $ poke ((p `plusPtr` 56 :: Ptr DeviceSize)) (maxGpuBytesToMove)
    lift $ poke ((p `plusPtr` 64 :: Ptr Word32)) (maxGpuAllocationsToMove)
    lift $ poke ((p `plusPtr` 72 :: Ptr (Ptr CommandBuffer_T))) (commandBuffer)
    lift $ f
  cStructSize = 80
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr DefragmentationFlags)) (zero)
    pPAllocations' <- ContT $ allocaBytesAligned @Allocation ((Data.Vector.length (mempty)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPAllocations' `plusPtr` (8 * (i)) :: Ptr Allocation) (e)) (mempty)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr Allocation))) (pPAllocations')
    pPPools' <- ContT $ allocaBytesAligned @Pool ((Data.Vector.length (mempty)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPPools' `plusPtr` (8 * (i)) :: Ptr Pool) (e)) (mempty)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr Pool))) (pPPools')
    lift $ poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (zero)
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 56 :: Ptr DeviceSize)) (zero)
    lift $ poke ((p `plusPtr` 64 :: Ptr Word32)) (zero)
    lift $ f

instance FromCStruct DefragmentationInfo2 where
  peekCStruct p = do
    flags <- peek @DefragmentationFlags ((p `plusPtr` 0 :: Ptr DefragmentationFlags))
    allocationCount <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    pAllocations <- peek @(Ptr Allocation) ((p `plusPtr` 8 :: Ptr (Ptr Allocation)))
    pAllocations' <- generateM (fromIntegral allocationCount) (\i -> peek @Allocation ((pAllocations `advancePtrBytes` (8 * (i)) :: Ptr Allocation)))
    pAllocationsChanged <- peek @(Ptr Bool32) ((p `plusPtr` 16 :: Ptr (Ptr Bool32)))
    poolCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pPools <- peek @(Ptr Pool) ((p `plusPtr` 32 :: Ptr (Ptr Pool)))
    pPools' <- generateM (fromIntegral poolCount) (\i -> peek @Pool ((pPools `advancePtrBytes` (8 * (i)) :: Ptr Pool)))
    maxCpuBytesToMove <- peek @DeviceSize ((p `plusPtr` 40 :: Ptr DeviceSize))
    maxCpuAllocationsToMove <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    maxGpuBytesToMove <- peek @DeviceSize ((p `plusPtr` 56 :: Ptr DeviceSize))
    maxGpuAllocationsToMove <- peek @Word32 ((p `plusPtr` 64 :: Ptr Word32))
    commandBuffer <- peek @(Ptr CommandBuffer_T) ((p `plusPtr` 72 :: Ptr (Ptr CommandBuffer_T)))
    pure $ DefragmentationInfo2
             flags pAllocations' pAllocationsChanged pPools' maxCpuBytesToMove maxCpuAllocationsToMove maxGpuBytesToMove maxGpuAllocationsToMove commandBuffer

instance Zero DefragmentationInfo2 where
  zero = DefragmentationInfo2
           zero
           mempty
           zero
           mempty
           zero
           zero
           zero
           zero
           zero


-- | VmaDefragmentationPassMoveInfo
data DefragmentationPassMoveInfo = DefragmentationPassMoveInfo
  { 
    allocation :: Allocation
  , 
    memory :: DeviceMemory
  , 
    offset :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DefragmentationPassMoveInfo)
#endif
deriving instance Show DefragmentationPassMoveInfo

instance ToCStruct DefragmentationPassMoveInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DefragmentationPassMoveInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Allocation)) (allocation)
    poke ((p `plusPtr` 8 :: Ptr DeviceMemory)) (memory)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (offset)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Allocation)) (zero)
    poke ((p `plusPtr` 8 :: Ptr DeviceMemory)) (zero)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct DefragmentationPassMoveInfo where
  peekCStruct p = do
    allocation <- peek @Allocation ((p `plusPtr` 0 :: Ptr Allocation))
    memory <- peek @DeviceMemory ((p `plusPtr` 8 :: Ptr DeviceMemory))
    offset <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    pure $ DefragmentationPassMoveInfo
             allocation memory offset

instance Storable DefragmentationPassMoveInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DefragmentationPassMoveInfo where
  zero = DefragmentationPassMoveInfo
           zero
           zero
           zero


-- | VmaDefragmentationPassInfo
--
-- Parameters for incremental defragmentation steps.
--
-- To be used with function 'beginDefragmentationPass'.
data DefragmentationPassInfo = DefragmentationPassInfo
  { 
    moveCount :: Word32
  , 
    moves :: Ptr DefragmentationPassMoveInfo
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DefragmentationPassInfo)
#endif
deriving instance Show DefragmentationPassInfo

instance ToCStruct DefragmentationPassInfo where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DefragmentationPassInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (moveCount)
    poke ((p `plusPtr` 8 :: Ptr (Ptr DefragmentationPassMoveInfo))) (moves)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr (Ptr DefragmentationPassMoveInfo))) (zero)
    f

instance FromCStruct DefragmentationPassInfo where
  peekCStruct p = do
    moveCount <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    pMoves <- peek @(Ptr DefragmentationPassMoveInfo) ((p `plusPtr` 8 :: Ptr (Ptr DefragmentationPassMoveInfo)))
    pure $ DefragmentationPassInfo
             moveCount pMoves

instance Storable DefragmentationPassInfo where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DefragmentationPassInfo where
  zero = DefragmentationPassInfo
           zero
           zero


-- | VmaDefragmentationInfo
--
-- Deprecated. Optional configuration parameters to be passed to function
-- 'defragment'.
--
-- /Deprecated/
--
-- This is a part of the old interface. It is recommended to use structure
-- 'DefragmentationInfo2' and function 'defragmentationBegin' instead.
data DefragmentationInfo = DefragmentationInfo
  { -- | Maximum total numbers of bytes that can be copied while moving
    -- allocations to different places.
    --
    -- Default is @VK_WHOLE_SIZE@, which means no limit.
    maxBytesToMove :: DeviceSize
  , -- | Maximum number of allocations that can be moved to different place.
    --
    -- Default is @UINT32_MAX@, which means no limit.
    maxAllocationsToMove :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DefragmentationInfo)
#endif
deriving instance Show DefragmentationInfo

instance ToCStruct DefragmentationInfo where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DefragmentationInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceSize)) (maxBytesToMove)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (maxAllocationsToMove)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    f

instance FromCStruct DefragmentationInfo where
  peekCStruct p = do
    maxBytesToMove <- peek @DeviceSize ((p `plusPtr` 0 :: Ptr DeviceSize))
    maxAllocationsToMove <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    pure $ DefragmentationInfo
             maxBytesToMove maxAllocationsToMove

instance Storable DefragmentationInfo where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DefragmentationInfo where
  zero = DefragmentationInfo
           zero
           zero


-- | VmaDefragmentationStats
--
-- Statistics returned by function 'defragment'.
data DefragmentationStats = DefragmentationStats
  { -- | Total number of bytes that have been copied while moving allocations to
    -- different places.
    bytesMoved :: DeviceSize
  , -- | Total number of bytes that have been released to the system by freeing
    -- empty @VkDeviceMemory@ objects.
    bytesFreed :: DeviceSize
  , -- | Number of allocations that have been moved to different places.
    allocationsMoved :: Word32
  , -- | Number of empty @VkDeviceMemory@ objects that have been released to the
    -- system.
    deviceMemoryBlocksFreed :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DefragmentationStats)
#endif
deriving instance Show DefragmentationStats

instance ToCStruct DefragmentationStats where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DefragmentationStats{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceSize)) (bytesMoved)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (bytesFreed)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (allocationsMoved)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (deviceMemoryBlocksFreed)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    f

instance FromCStruct DefragmentationStats where
  peekCStruct p = do
    bytesMoved <- peek @DeviceSize ((p `plusPtr` 0 :: Ptr DeviceSize))
    bytesFreed <- peek @DeviceSize ((p `plusPtr` 8 :: Ptr DeviceSize))
    allocationsMoved <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    deviceMemoryBlocksFreed <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pure $ DefragmentationStats
             bytesMoved bytesFreed allocationsMoved deviceMemoryBlocksFreed

instance Storable DefragmentationStats where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DefragmentationStats where
  zero = DefragmentationStats
           zero
           zero
           zero
           zero

