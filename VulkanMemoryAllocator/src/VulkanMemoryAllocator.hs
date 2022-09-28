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
                              , calculateStatistics
                              , getHeapBudgets
                              , findMemoryTypeIndex
                              , findMemoryTypeIndexForBufferInfo
                              , findMemoryTypeIndexForImageInfo
                              , createPool
                              , withPool
                              , destroyPool
                              , getPoolStatistics
                              , calculatePoolStatistics
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
                              , getAllocationInfo
                              , setAllocationUserData
                              , setAllocationName
                              , getAllocationMemoryProperties
                              , mapMemory
                              , withMappedMemory
                              , unmapMemory
                              , flushAllocation
                              , invalidateAllocation
                              , flushAllocations
                              , invalidateAllocations
                              , checkCorruption
                              , beginDefragmentation
                              , withDefragmentation
                              , endDefragmentation
                              , beginDefragmentationPass
                              , useDefragmentationPass
                              , endDefragmentationPass
                              , bindBufferMemory
                              , bindBufferMemory2
                              , bindImageMemory
                              , bindImageMemory2
                              , createBuffer
                              , withBuffer
                              , createBufferWithAlignment
                              , createAliasingBuffer
                              , createAliasingBuffer2
                              , destroyBuffer
                              , createImage
                              , withImage
                              , createAliasingImage
                              , createAliasingImage2
                              , destroyImage
                              , createVirtualBlock
                              , withVirtualBlock
                              , destroyVirtualBlock
                              , isVirtualBlockEmpty
                              , getVirtualAllocationInfo
                              , virtualAllocate
                              , withVirtualAllocation
                              , virtualFree
                              , clearVirtualBlock
                              , setVirtualAllocationUserData
                              , getVirtualBlockStatistics
                              , calculateVirtualBlockStatistics
                              , buildVirtualBlockStatsString
                              , freeVirtualBlockStatsString
                              , buildStatsString
                              , freeStatsString
                              , AllocatorCreateFlags
                              , AllocatorCreateFlagBits( ALLOCATOR_CREATE_EXTERNALLY_SYNCHRONIZED_BIT
                                                       , ALLOCATOR_CREATE_KHR_DEDICATED_ALLOCATION_BIT
                                                       , ALLOCATOR_CREATE_KHR_BIND_MEMORY2_BIT
                                                       , ALLOCATOR_CREATE_EXT_MEMORY_BUDGET_BIT
                                                       , ALLOCATOR_CREATE_AMD_DEVICE_COHERENT_MEMORY_BIT
                                                       , ALLOCATOR_CREATE_BUFFER_DEVICE_ADDRESS_BIT
                                                       , ALLOCATOR_CREATE_EXT_MEMORY_PRIORITY_BIT
                                                       , ..
                                                       )
                              , MemoryUsage( MEMORY_USAGE_UNKNOWN
                                           , MEMORY_USAGE_GPU_ONLY
                                           , MEMORY_USAGE_CPU_ONLY
                                           , MEMORY_USAGE_CPU_TO_GPU
                                           , MEMORY_USAGE_GPU_TO_CPU
                                           , MEMORY_USAGE_CPU_COPY
                                           , MEMORY_USAGE_GPU_LAZILY_ALLOCATED
                                           , MEMORY_USAGE_AUTO
                                           , MEMORY_USAGE_AUTO_PREFER_DEVICE
                                           , MEMORY_USAGE_AUTO_PREFER_HOST
                                           , ..
                                           )
                              , AllocationCreateFlags
                              , AllocationCreateFlagBits( ALLOCATION_CREATE_DEDICATED_MEMORY_BIT
                                                        , ALLOCATION_CREATE_NEVER_ALLOCATE_BIT
                                                        , ALLOCATION_CREATE_MAPPED_BIT
                                                        , ALLOCATION_CREATE_USER_DATA_COPY_STRING_BIT
                                                        , ALLOCATION_CREATE_UPPER_ADDRESS_BIT
                                                        , ALLOCATION_CREATE_DONT_BIND_BIT
                                                        , ALLOCATION_CREATE_WITHIN_BUDGET_BIT
                                                        , ALLOCATION_CREATE_CAN_ALIAS_BIT
                                                        , ALLOCATION_CREATE_HOST_ACCESS_SEQUENTIAL_WRITE_BIT
                                                        , ALLOCATION_CREATE_HOST_ACCESS_RANDOM_BIT
                                                        , ALLOCATION_CREATE_HOST_ACCESS_ALLOW_TRANSFER_INSTEAD_BIT
                                                        , ALLOCATION_CREATE_STRATEGY_MIN_MEMORY_BIT
                                                        , ALLOCATION_CREATE_STRATEGY_MIN_TIME_BIT
                                                        , ALLOCATION_CREATE_STRATEGY_MIN_OFFSET_BIT
                                                        , ALLOCATION_CREATE_STRATEGY_BEST_FIT_BIT
                                                        , ALLOCATION_CREATE_STRATEGY_FIRST_FIT_BIT
                                                        , ALLOCATION_CREATE_STRATEGY_MASK
                                                        , ..
                                                        )
                              , PoolCreateFlags
                              , PoolCreateFlagBits( POOL_CREATE_IGNORE_BUFFER_IMAGE_GRANULARITY_BIT
                                                  , POOL_CREATE_LINEAR_ALGORITHM_BIT
                                                  , POOL_CREATE_ALGORITHM_MASK
                                                  , ..
                                                  )
                              , DefragmentationFlags
                              , DefragmentationFlagBits( DEFRAGMENTATION_FLAG_ALGORITHM_FAST_BIT
                                                       , DEFRAGMENTATION_FLAG_ALGORITHM_BALANCED_BIT
                                                       , DEFRAGMENTATION_FLAG_ALGORITHM_FULL_BIT
                                                       , DEFRAGMENTATION_FLAG_ALGORITHM_EXTENSIVE_BIT
                                                       , DEFRAGMENTATION_FLAG_ALGORITHM_MASK
                                                       , ..
                                                       )
                              , DefragmentationMoveOperation( DEFRAGMENTATION_MOVE_OPERATION_COPY
                                                            , DEFRAGMENTATION_MOVE_OPERATION_IGNORE
                                                            , DEFRAGMENTATION_MOVE_OPERATION_DESTROY
                                                            , ..
                                                            )
                              , VirtualBlockCreateFlags
                              , VirtualBlockCreateFlagBits( VIRTUAL_BLOCK_CREATE_LINEAR_ALGORITHM_BIT
                                                          , VIRTUAL_BLOCK_CREATE_ALGORITHM_MASK
                                                          , ..
                                                          )
                              , VirtualAllocationCreateFlags
                              , VirtualAllocationCreateFlagBits( VIRTUAL_ALLOCATION_CREATE_UPPER_ADDRESS_BIT
                                                               , VIRTUAL_ALLOCATION_CREATE_STRATEGY_MIN_MEMORY_BIT
                                                               , VIRTUAL_ALLOCATION_CREATE_STRATEGY_MIN_TIME_BIT
                                                               , VIRTUAL_ALLOCATION_CREATE_STRATEGY_MIN_OFFSET_BIT
                                                               , VIRTUAL_ALLOCATION_CREATE_STRATEGY_MASK
                                                               , ..
                                                               )
                              , Allocator(..)
                              , Pool(..)
                              , Allocation(..)
                              , DefragmentationContext(..)
                              , VirtualAllocation(..)
                              , VirtualBlock(..)
                              , PFN_vmaAllocateDeviceMemoryFunction
                              , FN_vmaAllocateDeviceMemoryFunction
                              , PFN_vmaFreeDeviceMemoryFunction
                              , FN_vmaFreeDeviceMemoryFunction
                              , DeviceMemoryCallbacks(..)
                              , VulkanFunctions(..)
                              , AllocatorCreateInfo(..)
                              , AllocatorInfo(..)
                              , Statistics(..)
                              , DetailedStatistics(..)
                              , TotalStatistics(..)
                              , Budget(..)
                              , AllocationCreateInfo(..)
                              , PoolCreateInfo(..)
                              , AllocationInfo(..)
                              , DefragmentationInfo(..)
                              , DefragmentationMove(..)
                              , DefragmentationPassMoveInfo(..)
                              , DefragmentationStats(..)
                              , VirtualBlockCreateInfo(..)
                              , VirtualAllocationCreateInfo(..)
                              , VirtualAllocationInfo(..)
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
import Vulkan (DeviceBufferMemoryRequirements)
import Vulkan (DeviceImageMemoryRequirements)
import Vulkan (DeviceMemory)
import Vulkan (DeviceSize)
import Vulkan (Device_T)
import Vulkan (ExternalMemoryHandleTypeFlagsKHR)
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
import Foreign.Marshal.Alloc (allocaBytes)
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
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(..))
import Foreign.C.Types (CFloat(CFloat))
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

-- | Creates 'Allocator' object.
createAllocator :: forall io
                 . (MonadIO io)
                => -- No documentation found for Nested "vmaCreateAllocator" "pCreateInfo"
                   AllocatorCreateInfo
                -> io (Allocator)
createAllocator createInfo = liftIO . evalContT $ do
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pPAllocator <- ContT $ bracket (callocBytes @Allocator 8) free
  r <- lift $ traceAroundEvent "vmaCreateAllocator" ((ffiVmaCreateAllocator)
                                                       pCreateInfo
                                                       (pPAllocator))
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
  lift $ traceAroundEvent "vmaGetAllocatorInfo" ((ffiVmaGetAllocatorInfo)
                                                   (allocator)
                                                   (pPAllocatorInfo))
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
  lift $ traceAroundEvent "vmaGetPhysicalDeviceProperties" ((ffiVmaGetPhysicalDeviceProperties)
                                                              (allocator)
                                                              (pPpPhysicalDeviceProperties))
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
  lift $ traceAroundEvent "vmaGetMemoryProperties" ((ffiVmaGetMemoryProperties)
                                                      (allocator)
                                                      (pPpPhysicalDeviceMemoryProperties))
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
  lift $ traceAroundEvent "vmaGetMemoryTypeProperties" ((ffiVmaGetMemoryTypeProperties)
                                                          (allocator)
                                                          (memoryTypeIndex)
                                                          (pPFlags))
  pFlags <- lift $ peek @MemoryPropertyFlags pPFlags
  pure $ (pFlags)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaSetCurrentFrameIndex" ffiVmaSetCurrentFrameIndex
  :: Allocator -> Word32 -> IO ()

-- | Sets index of the current frame.
setCurrentFrameIndex :: forall io
                      . (MonadIO io)
                     => -- No documentation found for Nested "vmaSetCurrentFrameIndex" "allocator"
                        Allocator
                     -> -- No documentation found for Nested "vmaSetCurrentFrameIndex" "frameIndex"
                        ("frameIndex" ::: Word32)
                     -> io ()
setCurrentFrameIndex allocator frameIndex = liftIO $ do
  traceAroundEvent "vmaSetCurrentFrameIndex" ((ffiVmaSetCurrentFrameIndex)
                                                (allocator)
                                                (frameIndex))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaCalculateStatistics" ffiVmaCalculateStatistics
  :: Allocator -> Ptr TotalStatistics -> IO ()

-- | Retrieves statistics from current state of the Allocator.
--
-- This function is called \"calculate\" not \"get\" because it has to
-- traverse all internal data structures, so it may be quite slow. Use it
-- for debugging purposes. For faster but more brief statistics suitable to
-- be called every frame or every allocation, use 'getHeapBudgets'.
--
-- Note that when using allocator from multiple threads, returned
-- information may immediately become outdated.
calculateStatistics :: forall io
                     . (MonadIO io)
                    => -- No documentation found for Nested "vmaCalculateStatistics" "allocator"
                       Allocator
                    -> io (("stats" ::: TotalStatistics))
calculateStatistics allocator = liftIO . evalContT $ do
  pPStats <- ContT (withZeroCStruct @TotalStatistics)
  lift $ traceAroundEvent "vmaCalculateStatistics" ((ffiVmaCalculateStatistics)
                                                      (allocator)
                                                      (pPStats))
  pStats <- lift $ peekCStruct @TotalStatistics pPStats
  pure $ (pStats)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaGetHeapBudgets" ffiVmaGetHeapBudgets
  :: Allocator -> Ptr Budget -> IO ()

-- | Retrieves information about current memory usage and budget for all
-- memory heaps.
--
-- __Parameters__
--
-- +-----------+-----------+-----------------------------------------------+
-- |           | allocator |                                               |
-- +-----------+-----------+-----------------------------------------------+
-- | out       | pBudgets  | Must point to array with number of elements   |
-- |           |           | at least equal to number of memory heaps in   |
-- |           |           | physical device used.                         |
-- +-----------+-----------+-----------------------------------------------+
--
-- This function is called \"get\" not \"calculate\" because it is very
-- fast, suitable to be called every frame or every allocation. For more
-- detailed statistics use 'calculateStatistics'.
--
-- Note that when using allocator from multiple threads, returned
-- information may immediately become outdated.
getHeapBudgets :: forall io
                . (MonadIO io)
               => -- No documentation found for Nested "vmaGetHeapBudgets" "allocator"
                  Allocator
               -> -- No documentation found for Nested "vmaGetHeapBudgets" "pBudgets"
                  ("budgets" ::: Ptr Budget)
               -> io ()
getHeapBudgets allocator budgets = liftIO $ do
  traceAroundEvent "vmaGetHeapBudgets" ((ffiVmaGetHeapBudgets)
                                          (allocator)
                                          (budgets))
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
findMemoryTypeIndex allocator
                      memoryTypeBits
                      allocationCreateInfo = liftIO . evalContT $ do
  pAllocationCreateInfo <- ContT $ withCStruct (allocationCreateInfo)
  pPMemoryTypeIndex <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vmaFindMemoryTypeIndex" ((ffiVmaFindMemoryTypeIndex)
                                                           (allocator)
                                                           (memoryTypeBits)
                                                           pAllocationCreateInfo
                                                           (pPMemoryTypeIndex))
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
-- dummy buffer that never has memory bound.
findMemoryTypeIndexForBufferInfo :: forall a io
                                  . ( Extendss BufferCreateInfo a
                                    , PokeChain a
                                    , MonadIO io )
                                 => -- No documentation found for Nested "vmaFindMemoryTypeIndexForBufferInfo" "allocator"
                                    Allocator
                                 -> -- No documentation found for Nested "vmaFindMemoryTypeIndexForBufferInfo" "pBufferCreateInfo"
                                    (BufferCreateInfo a)
                                 -> -- No documentation found for Nested "vmaFindMemoryTypeIndexForBufferInfo" "pAllocationCreateInfo"
                                    AllocationCreateInfo
                                 -> io (("memoryTypeIndex" ::: Word32))
findMemoryTypeIndexForBufferInfo allocator
                                   bufferCreateInfo
                                   allocationCreateInfo = liftIO . evalContT $ do
  pBufferCreateInfo <- ContT $ withCStruct (bufferCreateInfo)
  pAllocationCreateInfo <- ContT $ withCStruct (allocationCreateInfo)
  pPMemoryTypeIndex <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vmaFindMemoryTypeIndexForBufferInfo" ((ffiVmaFindMemoryTypeIndexForBufferInfo)
                                                                        (allocator)
                                                                        (forgetExtensions pBufferCreateInfo)
                                                                        pAllocationCreateInfo
                                                                        (pPMemoryTypeIndex))
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
-- dummy image that never has memory bound.
findMemoryTypeIndexForImageInfo :: forall a io
                                 . ( Extendss ImageCreateInfo a
                                   , PokeChain a
                                   , MonadIO io )
                                => -- No documentation found for Nested "vmaFindMemoryTypeIndexForImageInfo" "allocator"
                                   Allocator
                                -> -- No documentation found for Nested "vmaFindMemoryTypeIndexForImageInfo" "pImageCreateInfo"
                                   (ImageCreateInfo a)
                                -> -- No documentation found for Nested "vmaFindMemoryTypeIndexForImageInfo" "pAllocationCreateInfo"
                                   AllocationCreateInfo
                                -> io (("memoryTypeIndex" ::: Word32))
findMemoryTypeIndexForImageInfo allocator
                                  imageCreateInfo
                                  allocationCreateInfo = liftIO . evalContT $ do
  pImageCreateInfo <- ContT $ withCStruct (imageCreateInfo)
  pAllocationCreateInfo <- ContT $ withCStruct (allocationCreateInfo)
  pPMemoryTypeIndex <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vmaFindMemoryTypeIndexForImageInfo" ((ffiVmaFindMemoryTypeIndexForImageInfo)
                                                                       (allocator)
                                                                       (forgetExtensions pImageCreateInfo)
                                                                       pAllocationCreateInfo
                                                                       (pPMemoryTypeIndex))
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
  r <- lift $ traceAroundEvent "vmaCreatePool" ((ffiVmaCreatePool)
                                                  (allocator)
                                                  pCreateInfo
                                                  (pPPool))
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
  "vmaGetPoolStatistics" ffiVmaGetPoolStatistics
  :: Allocator -> Pool -> Ptr Statistics -> IO ()

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
getPoolStatistics :: forall io
                   . (MonadIO io)
                  => -- No documentation found for Nested "vmaGetPoolStatistics" "allocator"
                     Allocator
                  -> -- No documentation found for Nested "vmaGetPoolStatistics" "pool"
                     Pool
                  -> io (("poolStats" ::: Statistics))
getPoolStatistics allocator pool = liftIO . evalContT $ do
  pPPoolStats <- ContT (withZeroCStruct @Statistics)
  lift $ traceAroundEvent "vmaGetPoolStatistics" ((ffiVmaGetPoolStatistics)
                                                    (allocator)
                                                    (pool)
                                                    (pPPoolStats))
  pPoolStats <- lift $ peekCStruct @Statistics pPPoolStats
  pure $ (pPoolStats)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaCalculatePoolStatistics" ffiVmaCalculatePoolStatistics
  :: Allocator -> Pool -> Ptr DetailedStatistics -> IO ()

-- | Retrieves detailed statistics of existing 'Pool' object.
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
calculatePoolStatistics :: forall io
                         . (MonadIO io)
                        => -- No documentation found for Nested "vmaCalculatePoolStatistics" "allocator"
                           Allocator
                        -> -- No documentation found for Nested "vmaCalculatePoolStatistics" "pool"
                           Pool
                        -> io (("poolStats" ::: DetailedStatistics))
calculatePoolStatistics allocator pool = liftIO . evalContT $ do
  pPPoolStats <- ContT (withZeroCStruct @DetailedStatistics)
  lift $ traceAroundEvent "vmaCalculatePoolStatistics" ((ffiVmaCalculatePoolStatistics)
                                                          (allocator)
                                                          (pool)
                                                          (pPPoolStats))
  pPoolStats <- lift $ peekCStruct @DetailedStatistics pPPoolStats
  pure $ (pPoolStats)


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
-- -   @VK_ERROR_UNKNOWN@ - corruption detection has been performed and
--     found memory corruptions around one of the allocations. @VMA_ASSERT@
--     is also fired in that case.
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
  r <- traceAroundEvent "vmaCheckPoolCorruption" ((ffiVmaCheckPoolCorruption)
                                                    (allocator)
                                                    (pool))
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
  lift $ traceAroundEvent "vmaGetPoolName" ((ffiVmaGetPoolName)
                                              (allocator)
                                              (pool)
                                              (pPpName))
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
  lift $ traceAroundEvent "vmaSetPoolName" ((ffiVmaSetPoolName)
                                              (allocator)
                                              (pool)
                                              pName)
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
-- +-----------+-----------------------+-----------------------------------------------+
-- |           | allocator             |                                               |
-- +-----------+-----------------------+-----------------------------------------------+
-- |           | pVkMemoryRequirements |                                               |
-- +-----------+-----------------------+-----------------------------------------------+
-- |           | pCreateInfo           |                                               |
-- +-----------+-----------------------+-----------------------------------------------+
-- | out       | pAllocation           | Handle to allocated memory.                   |
-- +-----------+-----------------------+-----------------------------------------------+
-- | out       | pAllocationInfo       | Optional. Information about allocated memory. |
-- |           |                       | It can be later fetched using function        |
-- |           |                       | 'getAllocationInfo'.                          |
-- +-----------+-----------------------+-----------------------------------------------+
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
allocateMemory allocator
                 vkMemoryRequirements
                 createInfo = liftIO . evalContT $ do
  pVkMemoryRequirements <- ContT $ withCStruct (vkMemoryRequirements)
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pPAllocation <- ContT $ bracket (callocBytes @Allocation 8) free
  pPAllocationInfo <- ContT (withZeroCStruct @AllocationInfo)
  r <- lift $ traceAroundEvent "vmaAllocateMemory" ((ffiVmaAllocateMemory)
                                                      (allocator)
                                                      pVkMemoryRequirements
                                                      pCreateInfo
                                                      (pPAllocation)
                                                      (pPAllocationInfo))
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
-- |           | pCreateInfo           | Creation parameters for each allocation.      |
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
allocateMemoryPages allocator
                      vkMemoryRequirements
                      createInfo = liftIO . evalContT $ do
  pPVkMemoryRequirements <- ContT $ allocaBytes @MemoryRequirements ((Data.Vector.length (vkMemoryRequirements)) * 24)
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPVkMemoryRequirements `plusPtr` (24 * (i)) :: Ptr MemoryRequirements) (e) . ($ ())) (vkMemoryRequirements)
  pPCreateInfo <- ContT $ allocaBytes @AllocationCreateInfo ((Data.Vector.length (createInfo)) * 48)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPCreateInfo `plusPtr` (48 * (i)) :: Ptr AllocationCreateInfo) (e)) (createInfo)
  let pVkMemoryRequirementsLength = Data.Vector.length $ (vkMemoryRequirements)
  lift $ unless ((Data.Vector.length $ (createInfo)) == pVkMemoryRequirementsLength) $
    throwIO $ IOError Nothing InvalidArgument "" "pCreateInfo and pVkMemoryRequirements must have the same length" Nothing Nothing
  pPAllocations <- ContT $ bracket (callocBytes @Allocation ((fromIntegral ((fromIntegral pVkMemoryRequirementsLength :: CSize))) * 8)) free
  pPAllocationInfo <- ContT $ bracket (callocBytes @AllocationInfo ((fromIntegral ((fromIntegral pVkMemoryRequirementsLength :: CSize))) * 56)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPAllocationInfo `advancePtrBytes` (i * 56) :: Ptr AllocationInfo) . ($ ())) [0..(fromIntegral ((fromIntegral pVkMemoryRequirementsLength :: CSize))) - 1]
  r <- lift $ traceAroundEvent "vmaAllocateMemoryPages" ((ffiVmaAllocateMemoryPages)
                                                           (allocator)
                                                           (pPVkMemoryRequirements)
                                                           (pPCreateInfo)
                                                           ((fromIntegral pVkMemoryRequirementsLength :: CSize))
                                                           (pPAllocations)
                                                           ((pPAllocationInfo)))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pAllocations <- lift $ generateM (fromIntegral ((fromIntegral pVkMemoryRequirementsLength :: CSize))) (\i -> peek @Allocation ((pPAllocations `advancePtrBytes` (8 * (i)) :: Ptr Allocation)))
  pAllocationInfo <- lift $ generateM (fromIntegral ((fromIntegral pVkMemoryRequirementsLength :: CSize))) (\i -> peekCStruct @AllocationInfo (((pPAllocationInfo) `advancePtrBytes` (56 * (i)) :: Ptr AllocationInfo)))
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

-- | Allocates memory suitable for given @VkBuffer@.
--
-- __Parameters__
--
-- +-----------+-----------------+-----------------------------------------------+
-- |           | allocator       |                                               |
-- +-----------+-----------------+-----------------------------------------------+
-- |           | buffer          |                                               |
-- +-----------+-----------------+-----------------------------------------------+
-- |           | pCreateInfo     |                                               |
-- +-----------+-----------------+-----------------------------------------------+
-- | out       | pAllocation     | Handle to allocated memory.                   |
-- +-----------+-----------------+-----------------------------------------------+
-- | out       | pAllocationInfo | Optional. Information about allocated memory. |
-- |           |                 | It can be later fetched using function        |
-- |           |                 | 'getAllocationInfo'.                          |
-- +-----------+-----------------+-----------------------------------------------+
--
-- It only creates 'Allocation'. To bind the memory to the buffer, use
-- 'bindBufferMemory'.
--
-- This is a special-purpose function. In most cases you should use
-- 'createBuffer'.
--
-- You must free the allocation using 'freeMemory' when no longer needed.
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
  r <- lift $ traceAroundEvent "vmaAllocateMemoryForBuffer" ((ffiVmaAllocateMemoryForBuffer)
                                                               (allocator)
                                                               (buffer)
                                                               pCreateInfo
                                                               (pPAllocation)
                                                               (pPAllocationInfo))
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

-- | Allocates memory suitable for given @VkImage@.
--
-- __Parameters__
--
-- +-----------+-----------------+-----------------------------------------------+
-- |           | allocator       |                                               |
-- +-----------+-----------------+-----------------------------------------------+
-- |           | image           |                                               |
-- +-----------+-----------------+-----------------------------------------------+
-- |           | pCreateInfo     |                                               |
-- +-----------+-----------------+-----------------------------------------------+
-- | out       | pAllocation     | Handle to allocated memory.                   |
-- +-----------+-----------------+-----------------------------------------------+
-- | out       | pAllocationInfo | Optional. Information about allocated memory. |
-- |           |                 | It can be later fetched using function        |
-- |           |                 | 'getAllocationInfo'.                          |
-- +-----------+-----------------+-----------------------------------------------+
--
-- It only creates 'Allocation'. To bind the memory to the buffer, use
-- 'bindImageMemory'.
--
-- This is a special-purpose function. In most cases you should use
-- 'createImage'.
--
-- You must free the allocation using 'freeMemory' when no longer needed.
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
  r <- lift $ traceAroundEvent "vmaAllocateMemoryForImage" ((ffiVmaAllocateMemoryForImage)
                                                              (allocator)
                                                              (image)
                                                              pCreateInfo
                                                              (pPAllocation)
                                                              (pPAllocationInfo))
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
  pPAllocations <- ContT $ allocaBytes @Allocation ((Data.Vector.length (allocations)) * 8)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPAllocations `plusPtr` (8 * (i)) :: Ptr Allocation) (e)) (allocations)
  lift $ traceAroundEvent "vmaFreeMemoryPages" ((ffiVmaFreeMemoryPages)
                                                  (allocator)
                                                  ((fromIntegral (Data.Vector.length $ (allocations)) :: CSize))
                                                  (pPAllocations))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaGetAllocationInfo" ffiVmaGetAllocationInfo
  :: Allocator -> Allocation -> Ptr AllocationInfo -> IO ()

-- | Returns current information about specified allocation.
--
-- Current parameters of given allocation are returned in
-- @pAllocationInfo@.
--
-- Although this function doesn\'t lock any mutex, so it should be quite
-- efficient, you should avoid calling it too often. You can retrieve same
-- 'AllocationInfo' structure while creating your resource, from function
-- 'createBuffer', 'createImage'. You can remember it if you are sure
-- parameters don\'t change (e.g. due to defragmentation).
getAllocationInfo :: forall io
                   . (MonadIO io)
                  => -- No documentation found for Nested "vmaGetAllocationInfo" "allocator"
                     Allocator
                  -> -- No documentation found for Nested "vmaGetAllocationInfo" "allocation"
                     Allocation
                  -> io (AllocationInfo)
getAllocationInfo allocator allocation = liftIO . evalContT $ do
  pPAllocationInfo <- ContT (withZeroCStruct @AllocationInfo)
  lift $ traceAroundEvent "vmaGetAllocationInfo" ((ffiVmaGetAllocationInfo)
                                                    (allocator)
                                                    (allocation)
                                                    (pPAllocationInfo))
  pAllocationInfo <- lift $ peekCStruct @AllocationInfo pPAllocationInfo
  pure $ (pAllocationInfo)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaSetAllocationUserData" ffiVmaSetAllocationUserData
  :: Allocator -> Allocation -> Ptr () -> IO ()

-- | Sets pUserData in given allocation to new value.
--
-- The value of pointer @pUserData@ is copied to allocation\'s @pUserData@.
-- It is opaque, so you can use it however you want - e.g. as a pointer,
-- ordinal number or some handle to you own data.
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
  traceAroundEvent "vmaSetAllocationUserData" ((ffiVmaSetAllocationUserData)
                                                 (allocator)
                                                 (allocation)
                                                 (userData))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaSetAllocationName" ffiVmaSetAllocationName
  :: Allocator -> Allocation -> Ptr CChar -> IO ()

-- | Sets pName in given allocation to new value.
--
-- @pName@ must be either null, or pointer to a null-terminated string. The
-- function makes local copy of the string and sets it as allocation\'s
-- @pName@. String passed as pName doesn\'t need to be valid for whole
-- lifetime of the allocation - you can free it after this call. String
-- previously pointed by allocation\'s @pName@ is freed from memory.
setAllocationName :: forall io
                   . (MonadIO io)
                  => -- No documentation found for Nested "vmaSetAllocationName" "allocator"
                     Allocator
                  -> -- No documentation found for Nested "vmaSetAllocationName" "allocation"
                     Allocation
                  -> -- No documentation found for Nested "vmaSetAllocationName" "pName"
                     ("name" ::: Maybe ByteString)
                  -> io ()
setAllocationName allocator allocation name = liftIO . evalContT $ do
  pName <- case (name) of
    Nothing -> pure nullPtr
    Just j -> ContT $ useAsCString (j)
  lift $ traceAroundEvent "vmaSetAllocationName" ((ffiVmaSetAllocationName)
                                                    (allocator)
                                                    (allocation)
                                                    pName)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaGetAllocationMemoryProperties" ffiVmaGetAllocationMemoryProperties
  :: Allocator -> Allocation -> Ptr MemoryPropertyFlags -> IO ()

-- | Given an allocation, returns Property Flags of its memory type.
--
-- This is just a convenience function. Same information can be obtained
-- using 'getAllocationInfo' + 'getMemoryProperties'.
getAllocationMemoryProperties :: forall io
                               . (MonadIO io)
                              => -- No documentation found for Nested "vmaGetAllocationMemoryProperties" "allocator"
                                 Allocator
                              -> -- No documentation found for Nested "vmaGetAllocationMemoryProperties" "allocation"
                                 Allocation
                              -> io (MemoryPropertyFlags)
getAllocationMemoryProperties allocator allocation = liftIO . evalContT $ do
  pPFlags <- ContT $ bracket (callocBytes @MemoryPropertyFlags 4) free
  lift $ traceAroundEvent "vmaGetAllocationMemoryProperties" ((ffiVmaGetAllocationMemoryProperties)
                                                                (allocator)
                                                                (allocation)
                                                                (pPFlags))
  pFlags <- lift $ peek @MemoryPropertyFlags pPFlags
  pure $ (pFlags)


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
-- memory.
--
-- Warning
--
-- If the allocation is part of a bigger @VkDeviceMemory@ block, returned
-- pointer is correctly offsetted to the beginning of region assigned to
-- this particular allocation. Unlike the result of @vkMapMemory@, it
-- points to the allocation, not to the beginning of the whole block. You
-- should not add /VmaAllocationInfo::offset/ to it!
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
  r <- lift $ traceAroundEvent "vmaMapMemory" ((ffiVmaMapMemory)
                                                 (allocator)
                                                 (allocation)
                                                 (pPpData))
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
  traceAroundEvent "vmaUnmapMemory" ((ffiVmaUnmapMemory)
                                       (allocator)
                                       (allocation))
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
  r <- traceAroundEvent "vmaFlushAllocation" ((ffiVmaFlushAllocation)
                                                (allocator)
                                                (allocation)
                                                (offset)
                                                (size))
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
  r <- traceAroundEvent "vmaInvalidateAllocation" ((ffiVmaInvalidateAllocation)
                                                     (allocator)
                                                     (allocation)
                                                     (offset)
                                                     (size))
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
  pAllocations <- ContT $ allocaBytes @Allocation ((Data.Vector.length (allocations)) * 8)
  lift $ Data.Vector.imapM_ (\i e -> poke (pAllocations `plusPtr` (8 * (i)) :: Ptr Allocation) (e)) (allocations)
  offsets' <- if Data.Vector.null (offsets)
    then pure nullPtr
    else do
      pOffsets <- ContT $ allocaBytes @DeviceSize (((Data.Vector.length (offsets))) * 8)
      lift $ Data.Vector.imapM_ (\i e -> poke (pOffsets `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) ((offsets))
      pure $ pOffsets
  sizes' <- if Data.Vector.null (sizes)
    then pure nullPtr
    else do
      pSizes <- ContT $ allocaBytes @DeviceSize (((Data.Vector.length (sizes))) * 8)
      lift $ Data.Vector.imapM_ (\i e -> poke (pSizes `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) ((sizes))
      pure $ pSizes
  r <- lift $ traceAroundEvent "vmaFlushAllocations" ((ffiVmaFlushAllocations)
                                                        (allocator)
                                                        ((fromIntegral allocationsLength :: Word32))
                                                        (pAllocations)
                                                        offsets'
                                                        sizes')
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
invalidateAllocations allocator
                        allocations
                        offsets
                        sizes = liftIO . evalContT $ do
  let allocationsLength = Data.Vector.length $ (allocations)
  let offsetsLength = Data.Vector.length $ (offsets)
  lift $ unless (fromIntegral offsetsLength == allocationsLength || offsetsLength == 0) $
    throwIO $ IOError Nothing InvalidArgument "" "offsets and allocations must have the same length" Nothing Nothing
  let sizesLength = Data.Vector.length $ (sizes)
  lift $ unless (fromIntegral sizesLength == allocationsLength || sizesLength == 0) $
    throwIO $ IOError Nothing InvalidArgument "" "sizes and allocations must have the same length" Nothing Nothing
  pAllocations <- ContT $ allocaBytes @Allocation ((Data.Vector.length (allocations)) * 8)
  lift $ Data.Vector.imapM_ (\i e -> poke (pAllocations `plusPtr` (8 * (i)) :: Ptr Allocation) (e)) (allocations)
  offsets' <- if Data.Vector.null (offsets)
    then pure nullPtr
    else do
      pOffsets <- ContT $ allocaBytes @DeviceSize (((Data.Vector.length (offsets))) * 8)
      lift $ Data.Vector.imapM_ (\i e -> poke (pOffsets `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) ((offsets))
      pure $ pOffsets
  sizes' <- if Data.Vector.null (sizes)
    then pure nullPtr
    else do
      pSizes <- ContT $ allocaBytes @DeviceSize (((Data.Vector.length (sizes))) * 8)
      lift $ Data.Vector.imapM_ (\i e -> poke (pSizes `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) ((sizes))
      pure $ pSizes
  r <- lift $ traceAroundEvent "vmaInvalidateAllocations" ((ffiVmaInvalidateAllocations)
                                                             (allocator)
                                                             ((fromIntegral allocationsLength :: Word32))
                                                             (pAllocations)
                                                             offsets'
                                                             sizes')
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
-- | allocator      |                                                        |
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
-- -   @VK_ERROR_UNKNOWN@ - corruption detection has been performed and
--     found memory corruptions around one of the allocations. @VMA_ASSERT@
--     is also fired in that case.
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
  r <- traceAroundEvent "vmaCheckCorruption" ((ffiVmaCheckCorruption)
                                                (allocator)
                                                (memoryTypeBits))
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaBeginDefragmentation" ffiVmaBeginDefragmentation
  :: Allocator -> Ptr DefragmentationInfo -> Ptr DefragmentationContext -> IO Result

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
-- | out       | pContext  | Context object that must be passed to         |
-- |           |           | 'endDefragmentation' to finish                |
-- |           |           | defragmentation.                              |
-- +-----------+-----------+-----------------------------------------------+
--
-- __Returns__
--
-- -   @VK_SUCCESS@ if defragmentation can begin.
--
-- -   @VK_ERROR_FEATURE_NOT_PRESENT@ if defragmentation is not supported.
--
-- For more information about defragmentation, see documentation chapter:
-- /Defragmentation/.
beginDefragmentation :: forall io
                      . (MonadIO io)
                     => -- No documentation found for Nested "vmaBeginDefragmentation" "allocator"
                        Allocator
                     -> -- No documentation found for Nested "vmaBeginDefragmentation" "pInfo"
                        DefragmentationInfo
                     -> io (DefragmentationContext)
beginDefragmentation allocator info = liftIO . evalContT $ do
  pInfo <- ContT $ withCStruct (info)
  pPContext <- ContT $ bracket (callocBytes @DefragmentationContext 8) free
  r <- lift $ traceAroundEvent "vmaBeginDefragmentation" ((ffiVmaBeginDefragmentation)
                                                            (allocator)
                                                            pInfo
                                                            (pPContext))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pContext <- lift $ peek @DefragmentationContext pPContext
  pure $ (pContext)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'beginDefragmentation' and 'endDefragmentation'
--
-- To ensure that 'endDefragmentation' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withDefragmentation :: forall io r . MonadIO io => Allocator -> DefragmentationInfo -> (io DefragmentationContext -> (DefragmentationContext -> io (DefragmentationStats)) -> r) -> r
withDefragmentation allocator pInfo b =
  b (beginDefragmentation allocator pInfo)
    (\(o0) -> endDefragmentation allocator o0)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaEndDefragmentation" ffiVmaEndDefragmentation
  :: Allocator -> DefragmentationContext -> Ptr DefragmentationStats -> IO ()

-- | Ends defragmentation process.
--
-- __Parameters__
--
-- +-----------+-----------+-----------------------------------------------+
-- |           | allocator | Allocator object.                             |
-- +-----------+-----------+-----------------------------------------------+
-- |           | context   | Context object that has been created by       |
-- |           |           | 'beginDefragmentation'.                       |
-- +-----------+-----------+-----------------------------------------------+
-- | out       | pStats    | Optional stats for the defragmentation. Can   |
-- |           |           | be null.                                      |
-- +-----------+-----------+-----------------------------------------------+
--
-- Use this function to finish defragmentation started by
-- 'beginDefragmentation'.
endDefragmentation :: forall io
                    . (MonadIO io)
                   => -- No documentation found for Nested "vmaEndDefragmentation" "allocator"
                      Allocator
                   -> -- No documentation found for Nested "vmaEndDefragmentation" "context"
                      DefragmentationContext
                   -> io (DefragmentationStats)
endDefragmentation allocator context = liftIO . evalContT $ do
  pPStats <- ContT (withZeroCStruct @DefragmentationStats)
  lift $ traceAroundEvent "vmaEndDefragmentation" ((ffiVmaEndDefragmentation)
                                                     (allocator)
                                                     (context)
                                                     (pPStats))
  pStats <- lift $ peekCStruct @DefragmentationStats pPStats
  pure $ (pStats)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaBeginDefragmentationPass" ffiVmaBeginDefragmentationPass
  :: Allocator -> DefragmentationContext -> Ptr DefragmentationPassMoveInfo -> IO Result

-- | Starts single defragmentation pass.
--
-- __Parameters__
--
-- +-----------+-----------+-----------------------------------------------+
-- |           | allocator | Allocator object.                             |
-- +-----------+-----------+-----------------------------------------------+
-- |           | context   | Context object that has been created by       |
-- |           |           | 'beginDefragmentation'.                       |
-- +-----------+-----------+-----------------------------------------------+
-- | out       | pPassInfo | Computed information for current pass.        |
-- +-----------+-----------+-----------------------------------------------+
--
-- __Returns__
--
-- -   @VK_SUCCESS@ if no more moves are possible. Then you can omit call
--     to 'endDefragmentationPass' and simply end whole defragmentation.
--
-- -   @VK_INCOMPLETE@ if there are pending moves returned in @pPassInfo@.
--     You need to perform them, call 'endDefragmentationPass', and then
--     preferably try another pass with 'beginDefragmentationPass'.
beginDefragmentationPass :: forall io
                          . (MonadIO io)
                         => -- No documentation found for Nested "vmaBeginDefragmentationPass" "allocator"
                            Allocator
                         -> -- No documentation found for Nested "vmaBeginDefragmentationPass" "context"
                            DefragmentationContext
                         -> io (("passInfo" ::: DefragmentationPassMoveInfo))
beginDefragmentationPass allocator context = liftIO . evalContT $ do
  pPPassInfo <- ContT (withZeroCStruct @DefragmentationPassMoveInfo)
  r <- lift $ traceAroundEvent "vmaBeginDefragmentationPass" ((ffiVmaBeginDefragmentationPass)
                                                                (allocator)
                                                                (context)
                                                                (pPPassInfo))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPassInfo <- lift $ peekCStruct @DefragmentationPassMoveInfo pPPassInfo
  pure $ (pPassInfo)

-- | This function will call the supplied action between calls to
-- 'beginDefragmentationPass' and 'endDefragmentationPass'
--
-- Note that 'endDefragmentationPass' is *not* called if an exception is
-- thrown by the inner action.
useDefragmentationPass :: forall io r . MonadIO io => Allocator -> DefragmentationContext -> (DefragmentationPassMoveInfo -> io r) -> io (("passInfo" ::: DefragmentationPassMoveInfo), r)
useDefragmentationPass allocator context a =
  do
    x <- beginDefragmentationPass allocator context
    r <- a x
    d <- (\(_) -> endDefragmentationPass allocator context) x
    pure (d, r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaEndDefragmentationPass" ffiVmaEndDefragmentationPass
  :: Allocator -> DefragmentationContext -> Ptr DefragmentationPassMoveInfo -> IO Result

-- | Ends single defragmentation pass.
--
-- __Parameters__
--
-- +-------------+--------------------------------------------------------+
-- | allocator   | Allocator object.                                      |
-- +-------------+--------------------------------------------------------+
-- | context     | Context object that has been created by                |
-- |             | 'beginDefragmentation'.                                |
-- +-------------+--------------------------------------------------------+
-- | pPassInfo   | Computed information for current pass filled by        |
-- |             | 'beginDefragmentationPass' and possibly modified by    |
-- |             | you.                                                   |
-- +-------------+--------------------------------------------------------+
--
-- Returns @VK_SUCCESS@ if no more moves are possible or @VK_INCOMPLETE@ if
-- more defragmentations are possible.
--
-- Ends incremental defragmentation pass and commits all defragmentation
-- moves from @pPassInfo@. After this call:
--
-- -   Allocations at @pPassInfo[i].srcAllocation@ that had
--     @pPassInfo[i].operation ==@ 'DEFRAGMENTATION_MOVE_OPERATION_COPY'
--     (which is the default) will be pointing to the new destination
--     place.
--
-- -   Allocation at @pPassInfo[i].srcAllocation@ that had
--     @pPassInfo[i].operation ==@ 'DEFRAGMENTATION_MOVE_OPERATION_DESTROY'
--     will be freed.
--
-- If no more moves are possible you can end whole defragmentation.
endDefragmentationPass :: forall io
                        . (MonadIO io)
                       => -- No documentation found for Nested "vmaEndDefragmentationPass" "allocator"
                          Allocator
                       -> -- No documentation found for Nested "vmaEndDefragmentationPass" "context"
                          DefragmentationContext
                       -> io (("passInfo" ::: DefragmentationPassMoveInfo))
endDefragmentationPass allocator context = liftIO . evalContT $ do
  pPPassInfo <- ContT (withZeroCStruct @DefragmentationPassMoveInfo)
  r <- lift $ traceAroundEvent "vmaEndDefragmentationPass" ((ffiVmaEndDefragmentationPass)
                                                              (allocator)
                                                              (context)
                                                              (pPPassInfo))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPassInfo <- lift $ peekCStruct @DefragmentationPassMoveInfo pPPassInfo
  pure $ (pPassInfo)


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
  r <- traceAroundEvent "vmaBindBufferMemory" ((ffiVmaBindBufferMemory)
                                                 (allocator)
                                                 (allocation)
                                                 (buffer))
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
-- | allocator             |                                                        |
-- +-----------------------+--------------------------------------------------------+
-- | allocation            |                                                        |
-- +-----------------------+--------------------------------------------------------+
-- | allocationLocalOffset | Additional offset to be added while binding, relative  |
-- |                       | to the beginning of the @allocation@. Normally it      |
-- |                       | should be 0.                                           |
-- +-----------------------+--------------------------------------------------------+
-- | buffer                |                                                        |
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
bindBufferMemory2 allocator
                    allocation
                    allocationLocalOffset
                    buffer
                    next = liftIO $ do
  r <- traceAroundEvent "vmaBindBufferMemory2" ((ffiVmaBindBufferMemory2)
                                                  (allocator)
                                                  (allocation)
                                                  (allocationLocalOffset)
                                                  (buffer)
                                                  (next))
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
  r <- traceAroundEvent "vmaBindImageMemory" ((ffiVmaBindImageMemory)
                                                (allocator)
                                                (allocation)
                                                (image))
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
-- | allocator             |                                                        |
-- +-----------------------+--------------------------------------------------------+
-- | allocation            |                                                        |
-- +-----------------------+--------------------------------------------------------+
-- | allocationLocalOffset | Additional offset to be added while binding, relative  |
-- |                       | to the beginning of the @allocation@. Normally it      |
-- |                       | should be 0.                                           |
-- +-----------------------+--------------------------------------------------------+
-- | image                 |                                                        |
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
bindImageMemory2 allocator
                   allocation
                   allocationLocalOffset
                   image
                   next = liftIO $ do
  r <- traceAroundEvent "vmaBindImageMemory2" ((ffiVmaBindImageMemory2)
                                                 (allocator)
                                                 (allocation)
                                                 (allocationLocalOffset)
                                                 (image)
                                                 (next))
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaCreateBuffer" ffiVmaCreateBuffer
  :: Allocator -> Ptr (SomeStruct BufferCreateInfo) -> Ptr AllocationCreateInfo -> Ptr Buffer -> Ptr Allocation -> Ptr AllocationInfo -> IO Result

-- | Creates a new @VkBuffer@, allocates and binds memory for it.
--
-- __Parameters__
--
-- +-----------+-----------------------+-----------------------------------------------+
-- |           | allocator             |                                               |
-- +-----------+-----------------------+-----------------------------------------------+
-- |           | pBufferCreateInfo     |                                               |
-- +-----------+-----------------------+-----------------------------------------------+
-- |           | pAllocationCreateInfo |                                               |
-- +-----------+-----------------------+-----------------------------------------------+
-- | out       | pBuffer               | Buffer that was created.                      |
-- +-----------+-----------------------+-----------------------------------------------+
-- | out       | pAllocation           | Allocation that was created.                  |
-- +-----------+-----------------------+-----------------------------------------------+
-- | out       | pAllocationInfo       | Optional. Information about allocated memory. |
-- |           |                       | It can be later fetched using function        |
-- |           |                       | 'getAllocationInfo'.                          |
-- +-----------+-----------------------+-----------------------------------------------+
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
-- returned value is negative error code, @*pBuffer@ and @*pAllocation@ are
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
-- ('ALLOCATION_CREATE_NEVER_ALLOCATE_BIT' is not used), it creates
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
createBuffer allocator
               bufferCreateInfo
               allocationCreateInfo = liftIO . evalContT $ do
  pBufferCreateInfo <- ContT $ withCStruct (bufferCreateInfo)
  pAllocationCreateInfo <- ContT $ withCStruct (allocationCreateInfo)
  pPBuffer <- ContT $ bracket (callocBytes @Buffer 8) free
  pPAllocation <- ContT $ bracket (callocBytes @Allocation 8) free
  pPAllocationInfo <- ContT (withZeroCStruct @AllocationInfo)
  r <- lift $ traceAroundEvent "vmaCreateBuffer" ((ffiVmaCreateBuffer)
                                                    (allocator)
                                                    (forgetExtensions pBufferCreateInfo)
                                                    pAllocationCreateInfo
                                                    (pPBuffer)
                                                    (pPAllocation)
                                                    (pPAllocationInfo))
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
  "vmaCreateBufferWithAlignment" ffiVmaCreateBufferWithAlignment
  :: Allocator -> Ptr (SomeStruct BufferCreateInfo) -> Ptr AllocationCreateInfo -> DeviceSize -> Ptr Buffer -> Ptr Allocation -> Ptr AllocationInfo -> IO Result

-- | Creates a buffer with additional minimum alignment.
--
-- Similar to 'createBuffer' but provides additional parameter
-- @minAlignment@ which allows to specify custom, minimum alignment to be
-- used when placing the buffer inside a larger memory block, which may be
-- needed e.g. for interop with OpenGL.
createBufferWithAlignment :: forall a io
                           . ( Extendss BufferCreateInfo a
                             , PokeChain a
                             , MonadIO io )
                          => -- No documentation found for Nested "vmaCreateBufferWithAlignment" "allocator"
                             Allocator
                          -> -- No documentation found for Nested "vmaCreateBufferWithAlignment" "pBufferCreateInfo"
                             (BufferCreateInfo a)
                          -> -- No documentation found for Nested "vmaCreateBufferWithAlignment" "pAllocationCreateInfo"
                             AllocationCreateInfo
                          -> -- No documentation found for Nested "vmaCreateBufferWithAlignment" "minAlignment"
                             ("minAlignment" ::: DeviceSize)
                          -> io (Buffer, Allocation, AllocationInfo)
createBufferWithAlignment allocator
                            bufferCreateInfo
                            allocationCreateInfo
                            minAlignment = liftIO . evalContT $ do
  pBufferCreateInfo <- ContT $ withCStruct (bufferCreateInfo)
  pAllocationCreateInfo <- ContT $ withCStruct (allocationCreateInfo)
  pPBuffer <- ContT $ bracket (callocBytes @Buffer 8) free
  pPAllocation <- ContT $ bracket (callocBytes @Allocation 8) free
  pPAllocationInfo <- ContT (withZeroCStruct @AllocationInfo)
  r <- lift $ traceAroundEvent "vmaCreateBufferWithAlignment" ((ffiVmaCreateBufferWithAlignment)
                                                                 (allocator)
                                                                 (forgetExtensions pBufferCreateInfo)
                                                                 pAllocationCreateInfo
                                                                 (minAlignment)
                                                                 (pPBuffer)
                                                                 (pPAllocation)
                                                                 (pPAllocationInfo))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pBuffer <- lift $ peek @Buffer pPBuffer
  pAllocation <- lift $ peek @Allocation pPAllocation
  pAllocationInfo <- lift $ peekCStruct @AllocationInfo pPAllocationInfo
  pure $ (pBuffer, pAllocation, pAllocationInfo)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaCreateAliasingBuffer" ffiVmaCreateAliasingBuffer
  :: Allocator -> Allocation -> Ptr (SomeStruct BufferCreateInfo) -> Ptr Buffer -> IO Result

-- | Creates a new @VkBuffer@, binds already created memory for it.
--
-- __Parameters__
--
-- +-----------+-------------------+-----------------------------------------------+
-- |           | allocator         |                                               |
-- +-----------+-------------------+-----------------------------------------------+
-- |           | allocation        | Allocation that provides memory to be used    |
-- |           |                   | for binding new buffer to it.                 |
-- +-----------+-------------------+-----------------------------------------------+
-- |           | pBufferCreateInfo |                                               |
-- +-----------+-------------------+-----------------------------------------------+
-- | out       | pBuffer           | Buffer that was created.                      |
-- +-----------+-------------------+-----------------------------------------------+
--
-- This function automatically:
--
-- 1.  Creates buffer.
--
-- 2.  Binds the buffer with the supplied memory.
--
-- If any of these operations fail, buffer is not created, returned value
-- is negative error code and @*pBuffer@ is null.
--
-- If the function succeeded, you must destroy the buffer when you no
-- longer need it using @vkDestroyBuffer()@. If you want to also destroy
-- the corresponding allocation you can use convenience function
-- 'destroyBuffer'.
--
-- Note
--
-- There is a new version of this function augmented with parameter
-- @allocationLocalOffset@ - see 'createAliasingBuffer2'.
createAliasingBuffer :: forall a io
                      . (Extendss BufferCreateInfo a, PokeChain a, MonadIO io)
                     => -- No documentation found for Nested "vmaCreateAliasingBuffer" "allocator"
                        Allocator
                     -> -- No documentation found for Nested "vmaCreateAliasingBuffer" "allocation"
                        Allocation
                     -> -- No documentation found for Nested "vmaCreateAliasingBuffer" "pBufferCreateInfo"
                        (BufferCreateInfo a)
                     -> io (Buffer)
createAliasingBuffer allocator
                       allocation
                       bufferCreateInfo = liftIO . evalContT $ do
  pBufferCreateInfo <- ContT $ withCStruct (bufferCreateInfo)
  pPBuffer <- ContT $ bracket (callocBytes @Buffer 8) free
  r <- lift $ traceAroundEvent "vmaCreateAliasingBuffer" ((ffiVmaCreateAliasingBuffer)
                                                            (allocator)
                                                            (allocation)
                                                            (forgetExtensions pBufferCreateInfo)
                                                            (pPBuffer))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pBuffer <- lift $ peek @Buffer pPBuffer
  pure $ (pBuffer)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaCreateAliasingBuffer2" ffiVmaCreateAliasingBuffer2
  :: Allocator -> Allocation -> DeviceSize -> Ptr (SomeStruct BufferCreateInfo) -> Ptr Buffer -> IO Result

-- | Creates a new @VkBuffer@, binds already created memory for it.
--
-- __Parameters__
--
-- +-----------+-----------------------+-----------------------------------------------+
-- |           | allocator             |                                               |
-- +-----------+-----------------------+-----------------------------------------------+
-- |           | allocation            | Allocation that provides memory to be used    |
-- |           |                       | for binding new buffer to it.                 |
-- +-----------+-----------------------+-----------------------------------------------+
-- |           | allocationLocalOffset | Additional offset to be added while binding,  |
-- |           |                       | relative to the beginning of the allocation.  |
-- |           |                       | Normally it should be 0.                      |
-- +-----------+-----------------------+-----------------------------------------------+
-- |           | pBufferCreateInfo     |                                               |
-- +-----------+-----------------------+-----------------------------------------------+
-- | out       | pBuffer               | Buffer that was created.                      |
-- +-----------+-----------------------+-----------------------------------------------+
--
-- This function automatically:
--
-- 1.  Creates buffer.
--
-- 2.  Binds the buffer with the supplied memory.
--
-- If any of these operations fail, buffer is not created, returned value
-- is negative error code and @*pBuffer@ is null.
--
-- If the function succeeded, you must destroy the buffer when you no
-- longer need it using @vkDestroyBuffer()@. If you want to also destroy
-- the corresponding allocation you can use convenience function
-- 'destroyBuffer'.
--
-- Note
--
-- This is a new version of the function augmented with parameter
-- @allocationLocalOffset@.
createAliasingBuffer2 :: forall a io
                       . (Extendss BufferCreateInfo a, PokeChain a, MonadIO io)
                      => -- No documentation found for Nested "vmaCreateAliasingBuffer2" "allocator"
                         Allocator
                      -> -- No documentation found for Nested "vmaCreateAliasingBuffer2" "allocation"
                         Allocation
                      -> -- No documentation found for Nested "vmaCreateAliasingBuffer2" "allocationLocalOffset"
                         ("allocationLocalOffset" ::: DeviceSize)
                      -> -- No documentation found for Nested "vmaCreateAliasingBuffer2" "pBufferCreateInfo"
                         (BufferCreateInfo a)
                      -> io (Buffer)
createAliasingBuffer2 allocator
                        allocation
                        allocationLocalOffset
                        bufferCreateInfo = liftIO . evalContT $ do
  pBufferCreateInfo <- ContT $ withCStruct (bufferCreateInfo)
  pPBuffer <- ContT $ bracket (callocBytes @Buffer 8) free
  r <- lift $ traceAroundEvent "vmaCreateAliasingBuffer2" ((ffiVmaCreateAliasingBuffer2)
                                                             (allocator)
                                                             (allocation)
                                                             (allocationLocalOffset)
                                                             (forgetExtensions pBufferCreateInfo)
                                                             (pPBuffer))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pBuffer <- lift $ peek @Buffer pPBuffer
  pure $ (pBuffer)


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
-- It is safe to pass null as buffer and\/or allocation.
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
  traceAroundEvent "vmaDestroyBuffer" ((ffiVmaDestroyBuffer)
                                         (allocator)
                                         (buffer)
                                         (allocation))
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
createImage allocator
              imageCreateInfo
              allocationCreateInfo = liftIO . evalContT $ do
  pImageCreateInfo <- ContT $ withCStruct (imageCreateInfo)
  pAllocationCreateInfo <- ContT $ withCStruct (allocationCreateInfo)
  pPImage <- ContT $ bracket (callocBytes @Image 8) free
  pPAllocation <- ContT $ bracket (callocBytes @Allocation 8) free
  pPAllocationInfo <- ContT (withZeroCStruct @AllocationInfo)
  r <- lift $ traceAroundEvent "vmaCreateImage" ((ffiVmaCreateImage)
                                                   (allocator)
                                                   (forgetExtensions pImageCreateInfo)
                                                   pAllocationCreateInfo
                                                   (pPImage)
                                                   (pPAllocation)
                                                   (pPAllocationInfo))
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
  "vmaCreateAliasingImage" ffiVmaCreateAliasingImage
  :: Allocator -> Allocation -> Ptr (SomeStruct ImageCreateInfo) -> Ptr Image -> IO Result

-- | Function similar to 'createAliasingBuffer' but for images.
createAliasingImage :: forall a io
                     . (Extendss ImageCreateInfo a, PokeChain a, MonadIO io)
                    => -- No documentation found for Nested "vmaCreateAliasingImage" "allocator"
                       Allocator
                    -> -- No documentation found for Nested "vmaCreateAliasingImage" "allocation"
                       Allocation
                    -> -- No documentation found for Nested "vmaCreateAliasingImage" "pImageCreateInfo"
                       (ImageCreateInfo a)
                    -> io (Image)
createAliasingImage allocator
                      allocation
                      imageCreateInfo = liftIO . evalContT $ do
  pImageCreateInfo <- ContT $ withCStruct (imageCreateInfo)
  pPImage <- ContT $ bracket (callocBytes @Image 8) free
  r <- lift $ traceAroundEvent "vmaCreateAliasingImage" ((ffiVmaCreateAliasingImage)
                                                           (allocator)
                                                           (allocation)
                                                           (forgetExtensions pImageCreateInfo)
                                                           (pPImage))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pImage <- lift $ peek @Image pPImage
  pure $ (pImage)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaCreateAliasingImage2" ffiVmaCreateAliasingImage2
  :: Allocator -> Allocation -> DeviceSize -> Ptr (SomeStruct ImageCreateInfo) -> Ptr Image -> IO Result

-- | Function similar to 'createAliasingBuffer2' but for images.
createAliasingImage2 :: forall a io
                      . (Extendss ImageCreateInfo a, PokeChain a, MonadIO io)
                     => -- No documentation found for Nested "vmaCreateAliasingImage2" "allocator"
                        Allocator
                     -> -- No documentation found for Nested "vmaCreateAliasingImage2" "allocation"
                        Allocation
                     -> -- No documentation found for Nested "vmaCreateAliasingImage2" "allocationLocalOffset"
                        ("allocationLocalOffset" ::: DeviceSize)
                     -> -- No documentation found for Nested "vmaCreateAliasingImage2" "pImageCreateInfo"
                        (ImageCreateInfo a)
                     -> io (Image)
createAliasingImage2 allocator
                       allocation
                       allocationLocalOffset
                       imageCreateInfo = liftIO . evalContT $ do
  pImageCreateInfo <- ContT $ withCStruct (imageCreateInfo)
  pPImage <- ContT $ bracket (callocBytes @Image 8) free
  r <- lift $ traceAroundEvent "vmaCreateAliasingImage2" ((ffiVmaCreateAliasingImage2)
                                                            (allocator)
                                                            (allocation)
                                                            (allocationLocalOffset)
                                                            (forgetExtensions pImageCreateInfo)
                                                            (pPImage))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pImage <- lift $ peek @Image pPImage
  pure $ (pImage)


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
-- It is safe to pass null as image and\/or allocation.
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
  traceAroundEvent "vmaDestroyImage" ((ffiVmaDestroyImage)
                                        (allocator)
                                        (image)
                                        (allocation))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaCreateVirtualBlock" ffiVmaCreateVirtualBlock
  :: Ptr VirtualBlockCreateInfo -> Ptr VirtualBlock -> IO Result

-- | Creates new 'VirtualBlock' object.
--
-- __Parameters__
--
-- +-----------+---------------+-----------------------------------------------+
-- |           | pCreateInfo   | Parameters for creation.                      |
-- +-----------+---------------+-----------------------------------------------+
-- | out       | pVirtualBlock | Returned virtual block object or @VMA_NULL@   |
-- |           |               | if creation failed.                           |
-- +-----------+---------------+-----------------------------------------------+
createVirtualBlock :: forall io
                    . (MonadIO io)
                   => -- No documentation found for Nested "vmaCreateVirtualBlock" "pCreateInfo"
                      VirtualBlockCreateInfo
                   -> io (VirtualBlock)
createVirtualBlock createInfo = liftIO . evalContT $ do
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pPVirtualBlock <- ContT $ bracket (callocBytes @VirtualBlock 8) free
  r <- lift $ traceAroundEvent "vmaCreateVirtualBlock" ((ffiVmaCreateVirtualBlock)
                                                          pCreateInfo
                                                          (pPVirtualBlock))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pVirtualBlock <- lift $ peek @VirtualBlock pPVirtualBlock
  pure $ (pVirtualBlock)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createVirtualBlock' and 'destroyVirtualBlock'
--
-- To ensure that 'destroyVirtualBlock' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withVirtualBlock :: forall io r . MonadIO io => VirtualBlockCreateInfo -> (io VirtualBlock -> (VirtualBlock -> io ()) -> r) -> r
withVirtualBlock pCreateInfo b =
  b (createVirtualBlock pCreateInfo)
    (\(o0) -> destroyVirtualBlock o0)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaDestroyVirtualBlock" ffiVmaDestroyVirtualBlock
  :: VirtualBlock -> IO ()

-- | Destroys 'VirtualBlock' object.
--
-- Please note that you should consciously handle virtual allocations that
-- could remain unfreed in the block. You should either free them
-- individually using 'virtualFree' or call 'clearVirtualBlock' if you are
-- sure this is what you want. If you do neither, an assert is called.
--
-- If you keep pointers to some additional metadata associated with your
-- virtual allocations in their @pUserData@, don\'t forget to free them.
destroyVirtualBlock :: forall io
                     . (MonadIO io)
                    => -- No documentation found for Nested "vmaDestroyVirtualBlock" "virtualBlock"
                       VirtualBlock
                    -> io ()
destroyVirtualBlock virtualBlock = liftIO $ do
  traceAroundEvent "vmaDestroyVirtualBlock" ((ffiVmaDestroyVirtualBlock)
                                               (virtualBlock))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaIsVirtualBlockEmpty" ffiVmaIsVirtualBlockEmpty
  :: VirtualBlock -> IO Bool32

-- | Returns true of the 'VirtualBlock' is empty - contains 0 virtual
-- allocations and has all its space available for new allocations.
isVirtualBlockEmpty :: forall io
                     . (MonadIO io)
                    => -- No documentation found for Nested "vmaIsVirtualBlockEmpty" "virtualBlock"
                       VirtualBlock
                    -> io (Bool)
isVirtualBlockEmpty virtualBlock = liftIO $ do
  r <- traceAroundEvent "vmaIsVirtualBlockEmpty" ((ffiVmaIsVirtualBlockEmpty)
                                                    (virtualBlock))
  pure $ ((bool32ToBool r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaGetVirtualAllocationInfo" ffiVmaGetVirtualAllocationInfo
  :: VirtualBlock -> VirtualAllocation -> Ptr VirtualAllocationInfo -> IO ()

-- | Returns information about a specific virtual allocation within a virtual
-- block, like its size and @pUserData@ pointer.
getVirtualAllocationInfo :: forall io
                          . (MonadIO io)
                         => -- No documentation found for Nested "vmaGetVirtualAllocationInfo" "virtualBlock"
                            VirtualBlock
                         -> -- No documentation found for Nested "vmaGetVirtualAllocationInfo" "allocation"
                            VirtualAllocation
                         -> io (("virtualAllocInfo" ::: VirtualAllocationInfo))
getVirtualAllocationInfo virtualBlock allocation = liftIO . evalContT $ do
  pPVirtualAllocInfo <- ContT (withZeroCStruct @VirtualAllocationInfo)
  lift $ traceAroundEvent "vmaGetVirtualAllocationInfo" ((ffiVmaGetVirtualAllocationInfo)
                                                           (virtualBlock)
                                                           (allocation)
                                                           (pPVirtualAllocInfo))
  pVirtualAllocInfo <- lift $ peekCStruct @VirtualAllocationInfo pPVirtualAllocInfo
  pure $ (pVirtualAllocInfo)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaVirtualAllocate" ffiVmaVirtualAllocate
  :: VirtualBlock -> Ptr VirtualAllocationCreateInfo -> Ptr VirtualAllocation -> Ptr DeviceSize -> IO Result

-- | Allocates new virtual allocation inside given 'VirtualBlock'.
--
-- If the allocation fails due to not enough free space available,
-- @VK_ERROR_OUT_OF_DEVICE_MEMORY@ is returned (despite the function
-- doesn\'t ever allocate actual GPU memory). @pAllocation@ is then set to
-- @VK_NULL_HANDLE@ and @pOffset@, if not null, it set to @UINT64_MAX@.
--
-- __Parameters__
--
-- +-----------+--------------+-----------------------------------------------+
-- |           | virtualBlock | Virtual block                                 |
-- +-----------+--------------+-----------------------------------------------+
-- |           | pCreateInfo  | Parameters for the allocation                 |
-- +-----------+--------------+-----------------------------------------------+
-- | out       | pAllocation  | Returned handle of the new allocation         |
-- +-----------+--------------+-----------------------------------------------+
-- | out       | pOffset      | Returned offset of the new allocation.        |
-- |           |              | Optional, can be null.                        |
-- +-----------+--------------+-----------------------------------------------+
virtualAllocate :: forall io
                 . (MonadIO io)
                => -- No documentation found for Nested "vmaVirtualAllocate" "virtualBlock"
                   VirtualBlock
                -> -- No documentation found for Nested "vmaVirtualAllocate" "pCreateInfo"
                   VirtualAllocationCreateInfo
                -> io (VirtualAllocation, ("offset" ::: DeviceSize))
virtualAllocate virtualBlock createInfo = liftIO . evalContT $ do
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pPAllocation <- ContT $ bracket (callocBytes @VirtualAllocation 8) free
  pPOffset <- ContT $ bracket (callocBytes @DeviceSize 8) free
  r <- lift $ traceAroundEvent "vmaVirtualAllocate" ((ffiVmaVirtualAllocate)
                                                       (virtualBlock)
                                                       pCreateInfo
                                                       (pPAllocation)
                                                       (pPOffset))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pAllocation <- lift $ peek @VirtualAllocation pPAllocation
  pOffset <- lift $ peek @DeviceSize pPOffset
  pure $ (pAllocation, pOffset)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'virtualAllocate' and 'virtualFree'
--
-- To ensure that 'virtualFree' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withVirtualAllocation :: forall io r . MonadIO io => VirtualBlock -> VirtualAllocationCreateInfo -> (io (VirtualAllocation, DeviceSize) -> ((VirtualAllocation, DeviceSize) -> io ()) -> r) -> r
withVirtualAllocation virtualBlock pCreateInfo b =
  b (virtualAllocate virtualBlock pCreateInfo)
    (\(o0, _) -> virtualFree virtualBlock o0)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaVirtualFree" ffiVmaVirtualFree
  :: VirtualBlock -> VirtualAllocation -> IO ()

-- | Frees virtual allocation inside given 'VirtualBlock'.
--
-- It is correct to call this function with @allocation == VK_NULL_HANDLE@
-- - it does nothing.
virtualFree :: forall io
             . (MonadIO io)
            => -- No documentation found for Nested "vmaVirtualFree" "virtualBlock"
               VirtualBlock
            -> -- No documentation found for Nested "vmaVirtualFree" "allocation"
               VirtualAllocation
            -> io ()
virtualFree virtualBlock allocation = liftIO $ do
  traceAroundEvent "vmaVirtualFree" ((ffiVmaVirtualFree)
                                       (virtualBlock)
                                       (allocation))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaClearVirtualBlock" ffiVmaClearVirtualBlock
  :: VirtualBlock -> IO ()

-- | Frees all virtual allocations inside given 'VirtualBlock'.
--
-- You must either call this function or free each virtual allocation
-- individually with 'virtualFree' before destroying a virtual block.
-- Otherwise, an assert is called.
--
-- If you keep pointer to some additional metadata associated with your
-- virtual allocation in its @pUserData@, don\'t forget to free it as well.
clearVirtualBlock :: forall io
                   . (MonadIO io)
                  => -- No documentation found for Nested "vmaClearVirtualBlock" "virtualBlock"
                     VirtualBlock
                  -> io ()
clearVirtualBlock virtualBlock = liftIO $ do
  traceAroundEvent "vmaClearVirtualBlock" ((ffiVmaClearVirtualBlock)
                                             (virtualBlock))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaSetVirtualAllocationUserData" ffiVmaSetVirtualAllocationUserData
  :: VirtualBlock -> VirtualAllocation -> Ptr () -> IO ()

-- | Changes custom pointer associated with given virtual allocation.
setVirtualAllocationUserData :: forall io
                              . (MonadIO io)
                             => -- No documentation found for Nested "vmaSetVirtualAllocationUserData" "virtualBlock"
                                VirtualBlock
                             -> -- No documentation found for Nested "vmaSetVirtualAllocationUserData" "allocation"
                                VirtualAllocation
                             -> -- No documentation found for Nested "vmaSetVirtualAllocationUserData" "pUserData"
                                ("userData" ::: Ptr ())
                             -> io ()
setVirtualAllocationUserData virtualBlock allocation userData = liftIO $ do
  traceAroundEvent "vmaSetVirtualAllocationUserData" ((ffiVmaSetVirtualAllocationUserData)
                                                        (virtualBlock)
                                                        (allocation)
                                                        (userData))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaGetVirtualBlockStatistics" ffiVmaGetVirtualBlockStatistics
  :: VirtualBlock -> Ptr Statistics -> IO ()

-- | Calculates and returns statistics about virtual allocations and memory
-- usage in given 'VirtualBlock'.
--
-- This function is fast to call. For more detailed statistics, see
-- 'calculateVirtualBlockStatistics'.
getVirtualBlockStatistics :: forall io
                           . (MonadIO io)
                          => -- No documentation found for Nested "vmaGetVirtualBlockStatistics" "virtualBlock"
                             VirtualBlock
                          -> io (("stats" ::: Statistics))
getVirtualBlockStatistics virtualBlock = liftIO . evalContT $ do
  pPStats <- ContT (withZeroCStruct @Statistics)
  lift $ traceAroundEvent "vmaGetVirtualBlockStatistics" ((ffiVmaGetVirtualBlockStatistics)
                                                            (virtualBlock)
                                                            (pPStats))
  pStats <- lift $ peekCStruct @Statistics pPStats
  pure $ (pStats)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaCalculateVirtualBlockStatistics" ffiVmaCalculateVirtualBlockStatistics
  :: VirtualBlock -> Ptr DetailedStatistics -> IO ()

-- | Calculates and returns detailed statistics about virtual allocations and
-- memory usage in given 'VirtualBlock'.
--
-- This function is slow to call. Use for debugging purposes. For less
-- detailed statistics, see 'getVirtualBlockStatistics'.
calculateVirtualBlockStatistics :: forall io
                                 . (MonadIO io)
                                => -- No documentation found for Nested "vmaCalculateVirtualBlockStatistics" "virtualBlock"
                                   VirtualBlock
                                -> io (("stats" ::: DetailedStatistics))
calculateVirtualBlockStatistics virtualBlock = liftIO . evalContT $ do
  pPStats <- ContT (withZeroCStruct @DetailedStatistics)
  lift $ traceAroundEvent "vmaCalculateVirtualBlockStatistics" ((ffiVmaCalculateVirtualBlockStatistics)
                                                                  (virtualBlock)
                                                                  (pPStats))
  pStats <- lift $ peekCStruct @DetailedStatistics pPStats
  pure $ (pStats)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaBuildVirtualBlockStatsString" ffiVmaBuildVirtualBlockStatsString
  :: VirtualBlock -> Ptr (Ptr CChar) -> Bool32 -> IO ()

-- | Builds and returns a null-terminated string in JSON format with
-- information about given 'VirtualBlock'.
--
-- __Parameters__
--
-- +-----------+---------------+-----------------------------------------------+
-- |           | virtualBlock  | Virtual block.                                |
-- +-----------+---------------+-----------------------------------------------+
-- | out       | ppStatsString | Returned string.                              |
-- +-----------+---------------+-----------------------------------------------+
-- |           | detailedMap   | Pass @VK_FALSE@ to only obtain statistics as  |
-- |           |               | returned by                                   |
-- |           |               | 'calculateVirtualBlockStatistics'. Pass       |
-- |           |               | @VK_TRUE@ to also obtain full list of         |
-- |           |               | allocations and free spaces.                  |
-- +-----------+---------------+-----------------------------------------------+
--
-- Returned string must be freed using 'freeVirtualBlockStatsString'.
buildVirtualBlockStatsString :: forall io
                              . (MonadIO io)
                             => -- No documentation found for Nested "vmaBuildVirtualBlockStatsString" "virtualBlock"
                                VirtualBlock
                             -> -- No documentation found for Nested "vmaBuildVirtualBlockStatsString" "detailedMap"
                                ("detailedMap" ::: Bool)
                             -> io (("statsString" ::: Ptr CChar))
buildVirtualBlockStatsString virtualBlock detailedMap = liftIO . evalContT $ do
  pPpStatsString <- ContT $ bracket (callocBytes @(Ptr CChar) 8) free
  lift $ traceAroundEvent "vmaBuildVirtualBlockStatsString" ((ffiVmaBuildVirtualBlockStatsString)
                                                               (virtualBlock)
                                                               (pPpStatsString)
                                                               (boolToBool32 (detailedMap)))
  ppStatsString <- lift $ peek @(Ptr CChar) pPpStatsString
  pure $ (ppStatsString)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaFreeVirtualBlockStatsString" ffiVmaFreeVirtualBlockStatsString
  :: VirtualBlock -> Ptr CChar -> IO ()

-- | Frees a string returned by 'buildVirtualBlockStatsString'.
freeVirtualBlockStatsString :: forall io
                             . (MonadIO io)
                            => -- No documentation found for Nested "vmaFreeVirtualBlockStatsString" "virtualBlock"
                               VirtualBlock
                            -> -- No documentation found for Nested "vmaFreeVirtualBlockStatsString" "pStatsString"
                               ("statsString" ::: Ptr CChar)
                            -> io ()
freeVirtualBlockStatsString virtualBlock statsString = liftIO $ do
  traceAroundEvent "vmaFreeVirtualBlockStatsString" ((ffiVmaFreeVirtualBlockStatsString)
                                                       (virtualBlock)
                                                       (statsString))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaBuildStatsString" ffiVmaBuildStatsString
  :: Allocator -> Ptr (Ptr CChar) -> Bool32 -> IO ()

-- | Builds and returns statistics as a null-terminated string in JSON
-- format.
--
-- __Parameters__
--
-- +-----------+---------------+-----------------------------------------------+
-- |           | allocator     |                                               |
-- +-----------+---------------+-----------------------------------------------+
-- | out       | ppStatsString | Must be freed using 'freeStatsString'         |
-- |           |               | function.                                     |
-- +-----------+---------------+-----------------------------------------------+
-- |           | detailedMap   |                                               |
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
  lift $ traceAroundEvent "vmaBuildStatsString" ((ffiVmaBuildStatsString)
                                                   (allocator)
                                                   (pPpStatsString)
                                                   (boolToBool32 (detailedMap)))
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
  traceAroundEvent "vmaFreeStatsString" ((ffiVmaFreeStatsString)
                                           (allocator)
                                           (statsString))
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


type FN_vkGetDeviceProcAddr = Ptr Device_T -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction
-- No documentation found for TopLevel "PFN_vkGetDeviceProcAddr"
type PFN_vkGetDeviceProcAddr = FunPtr FN_vkGetDeviceProcAddr


type FN_vkGetInstanceProcAddr = Ptr Instance_T -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction
-- No documentation found for TopLevel "PFN_vkGetInstanceProcAddr"
type PFN_vkGetInstanceProcAddr = FunPtr FN_vkGetInstanceProcAddr


type FN_vkInvalidateMappedMemoryRanges = Ptr Device_T -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr MappedMemoryRange) -> IO Result
-- No documentation found for TopLevel "PFN_vkInvalidateMappedMemoryRanges"
type PFN_vkInvalidateMappedMemoryRanges = FunPtr FN_vkInvalidateMappedMemoryRanges


type FN_vkVoidFunction = IO ()
-- No documentation found for TopLevel "PFN_vkVoidFunction"
type PFN_vkVoidFunction = FunPtr FN_vkVoidFunction


type FN_vkMapMemory = Ptr Device_T -> DeviceMemory -> ("offset" ::: DeviceSize) -> DeviceSize -> MemoryMapFlags -> ("ppData" ::: Ptr (Ptr ())) -> IO Result
-- No documentation found for TopLevel "PFN_vkMapMemory"
type PFN_vkMapMemory = FunPtr FN_vkMapMemory


type FN_vkUnmapMemory = Ptr Device_T -> DeviceMemory -> IO ()
-- No documentation found for TopLevel "PFN_vkUnmapMemory"
type PFN_vkUnmapMemory = FunPtr FN_vkUnmapMemory


type FN_vkGetDeviceBufferMemoryRequirements = Ptr Device_T -> ("pInfo" ::: Ptr DeviceBufferMemoryRequirements) -> ("pMemoryRequirements" ::: Ptr (SomeStruct MemoryRequirements2)) -> IO ()
-- No documentation found for TopLevel "PFN_vkGetDeviceBufferMemoryRequirements"
type PFN_vkGetDeviceBufferMemoryRequirements = FunPtr FN_vkGetDeviceBufferMemoryRequirements


type FN_vkGetDeviceImageMemoryRequirements = Ptr Device_T -> ("pInfo" ::: Ptr DeviceImageMemoryRequirements) -> ("pMemoryRequirements" ::: Ptr (SomeStruct MemoryRequirements2)) -> IO ()
-- No documentation found for TopLevel "PFN_vkGetDeviceImageMemoryRequirements"
type PFN_vkGetDeviceImageMemoryRequirements = FunPtr FN_vkGetDeviceImageMemoryRequirements


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
pattern ALLOCATOR_CREATE_EXTERNALLY_SYNCHRONIZED_BIT = AllocatorCreateFlagBits 0x00000001

-- | Enables usage of VK_KHR_dedicated_allocation extension.
--
-- The flag works only if /VmaAllocatorCreateInfo::vulkanApiVersion/
-- @== VK_API_VERSION_1_0@. When it is @VK_API_VERSION_1_1@, the flag is
-- ignored because the extension has been promoted to Vulkan 1.1.
--
-- Using this extension will automatically allocate dedicated blocks of
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
-- ‍vkBindBufferMemory(): Binding memory to buffer 0x2d but
-- vkGetBufferMemoryRequirements() has not been called on that buffer.
pattern ALLOCATOR_CREATE_KHR_DEDICATED_ALLOCATION_BIT = AllocatorCreateFlagBits 0x00000002

-- | Enables usage of VK_KHR_bind_memory2 extension.
--
-- The flag works only if /VmaAllocatorCreateInfo::vulkanApiVersion/
-- @== VK_API_VERSION_1_0@. When it is @VK_API_VERSION_1_1@, the flag is
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
pattern ALLOCATOR_CREATE_KHR_BIND_MEMORY2_BIT = AllocatorCreateFlagBits 0x00000004

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
pattern ALLOCATOR_CREATE_EXT_MEMORY_BUDGET_BIT = AllocatorCreateFlagBits 0x00000008

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
pattern ALLOCATOR_CREATE_BUFFER_DEVICE_ADDRESS_BIT = AllocatorCreateFlagBits 0x00000020

-- | Enables usage of VK_EXT_memory_priority extension in the library.
--
-- You may set this flag only if you found available and enabled this
-- device extension, along with
-- @VkPhysicalDeviceMemoryPriorityFeaturesEXT::memoryPriority == VK_TRUE@,
-- while creating Vulkan device passed as /VmaAllocatorCreateInfo::device/.
--
-- When this flag is used, /VmaAllocationCreateInfo::priority/ and
-- /VmaPoolCreateInfo::priority/ are used to set priorities of allocated
-- Vulkan memory. Without it, these variables are ignored.
--
-- A priority must be a floating-point value between 0 and 1, indicating
-- the priority of the allocation relative to other memory allocations.
-- Larger values are higher priority. The granularity of the priorities is
-- implementation-dependent. It is automatically passed to every call to
-- @vkAllocateMemory@ done by the library using structure
-- @VkMemoryPriorityAllocateInfoEXT@. The value to be used for default
-- priority is 0.5. For more details, see the documentation of the
-- VK_EXT_memory_priority extension.
pattern ALLOCATOR_CREATE_EXT_MEMORY_PRIORITY_BIT = AllocatorCreateFlagBits 0x00000040

conNameAllocatorCreateFlagBits :: String
conNameAllocatorCreateFlagBits = "AllocatorCreateFlagBits"

enumPrefixAllocatorCreateFlagBits :: String
enumPrefixAllocatorCreateFlagBits = "ALLOCATOR_CREATE_"

showTableAllocatorCreateFlagBits :: [(AllocatorCreateFlagBits, String)]
showTableAllocatorCreateFlagBits =
  [
    ( ALLOCATOR_CREATE_EXTERNALLY_SYNCHRONIZED_BIT
    , "EXTERNALLY_SYNCHRONIZED_BIT"
    )
  ,
    ( ALLOCATOR_CREATE_KHR_DEDICATED_ALLOCATION_BIT
    , "KHR_DEDICATED_ALLOCATION_BIT"
    )
  ,
    ( ALLOCATOR_CREATE_KHR_BIND_MEMORY2_BIT
    , "KHR_BIND_MEMORY2_BIT"
    )
  ,
    ( ALLOCATOR_CREATE_EXT_MEMORY_BUDGET_BIT
    , "EXT_MEMORY_BUDGET_BIT"
    )
  ,
    ( ALLOCATOR_CREATE_AMD_DEVICE_COHERENT_MEMORY_BIT
    , "AMD_DEVICE_COHERENT_MEMORY_BIT"
    )
  ,
    ( ALLOCATOR_CREATE_BUFFER_DEVICE_ADDRESS_BIT
    , "BUFFER_DEVICE_ADDRESS_BIT"
    )
  ,
    ( ALLOCATOR_CREATE_EXT_MEMORY_PRIORITY_BIT
    , "EXT_MEMORY_PRIORITY_BIT"
    )
  ]

instance Show AllocatorCreateFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixAllocatorCreateFlagBits
      showTableAllocatorCreateFlagBits
      conNameAllocatorCreateFlagBits
      (\(AllocatorCreateFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read AllocatorCreateFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixAllocatorCreateFlagBits
      showTableAllocatorCreateFlagBits
      conNameAllocatorCreateFlagBits
      AllocatorCreateFlagBits

-- | Intended usage of the allocated memory.
newtype MemoryUsage = MemoryUsage Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | No intended memory usage specified. Use other members of
-- 'AllocationCreateInfo' to specify your requirements.
pattern MEMORY_USAGE_UNKNOWN = MemoryUsage 0

-- | /Deprecated/
--
-- Obsolete, preserved for backward compatibility. Prefers
-- @VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT@.
pattern MEMORY_USAGE_GPU_ONLY = MemoryUsage 1

-- | /Deprecated/
--
-- Obsolete, preserved for backward compatibility. Guarantees
-- @VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT@ and
-- @VK_MEMORY_PROPERTY_HOST_COHERENT_BIT@.
pattern MEMORY_USAGE_CPU_ONLY = MemoryUsage 2

-- | /Deprecated/
--
-- Obsolete, preserved for backward compatibility. Guarantees
-- @VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT@, prefers
-- @VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT@.
pattern MEMORY_USAGE_CPU_TO_GPU = MemoryUsage 3

-- | /Deprecated/
--
-- Obsolete, preserved for backward compatibility. Guarantees
-- @VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT@, prefers
-- @VK_MEMORY_PROPERTY_HOST_CACHED_BIT@.
pattern MEMORY_USAGE_GPU_TO_CPU = MemoryUsage 4

-- | /Deprecated/
--
-- Obsolete, preserved for backward compatibility. Prefers not
-- @VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT@.
pattern MEMORY_USAGE_CPU_COPY = MemoryUsage 5

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

-- | Selects best memory type automatically. This flag is recommended for
-- most common use cases.
--
-- When using this flag, if you want to map the allocation (using
-- 'mapMemory' or 'ALLOCATION_CREATE_MAPPED_BIT'), you must pass one of the
-- flags: 'ALLOCATION_CREATE_HOST_ACCESS_SEQUENTIAL_WRITE_BIT' or
-- 'ALLOCATION_CREATE_HOST_ACCESS_RANDOM_BIT' in
-- /VmaAllocationCreateInfo::flags/.
--
-- It can be used only with functions that let the library know
-- @VkBufferCreateInfo@ or @VkImageCreateInfo@, e.g. 'createBuffer',
-- 'createImage', 'findMemoryTypeIndexForBufferInfo',
-- 'findMemoryTypeIndexForImageInfo' and not with generic memory allocation
-- functions.
pattern MEMORY_USAGE_AUTO = MemoryUsage 7

-- | Selects best memory type automatically with preference for GPU (device)
-- memory.
--
-- When using this flag, if you want to map the allocation (using
-- 'mapMemory' or 'ALLOCATION_CREATE_MAPPED_BIT'), you must pass one of the
-- flags: 'ALLOCATION_CREATE_HOST_ACCESS_SEQUENTIAL_WRITE_BIT' or
-- 'ALLOCATION_CREATE_HOST_ACCESS_RANDOM_BIT' in
-- /VmaAllocationCreateInfo::flags/.
--
-- It can be used only with functions that let the library know
-- @VkBufferCreateInfo@ or @VkImageCreateInfo@, e.g. 'createBuffer',
-- 'createImage', 'findMemoryTypeIndexForBufferInfo',
-- 'findMemoryTypeIndexForImageInfo' and not with generic memory allocation
-- functions.
pattern MEMORY_USAGE_AUTO_PREFER_DEVICE = MemoryUsage 8

-- | Selects best memory type automatically with preference for CPU (host)
-- memory.
--
-- When using this flag, if you want to map the allocation (using
-- 'mapMemory' or 'ALLOCATION_CREATE_MAPPED_BIT'), you must pass one of the
-- flags: 'ALLOCATION_CREATE_HOST_ACCESS_SEQUENTIAL_WRITE_BIT' or
-- 'ALLOCATION_CREATE_HOST_ACCESS_RANDOM_BIT' in
-- /VmaAllocationCreateInfo::flags/.
--
-- It can be used only with functions that let the library know
-- @VkBufferCreateInfo@ or @VkImageCreateInfo@, e.g. 'createBuffer',
-- 'createImage', 'findMemoryTypeIndexForBufferInfo',
-- 'findMemoryTypeIndexForImageInfo' and not with generic memory allocation
-- functions.
pattern MEMORY_USAGE_AUTO_PREFER_HOST = MemoryUsage 9

{-# COMPLETE
  MEMORY_USAGE_UNKNOWN
  , MEMORY_USAGE_GPU_ONLY
  , MEMORY_USAGE_CPU_ONLY
  , MEMORY_USAGE_CPU_TO_GPU
  , MEMORY_USAGE_GPU_TO_CPU
  , MEMORY_USAGE_CPU_COPY
  , MEMORY_USAGE_GPU_LAZILY_ALLOCATED
  , MEMORY_USAGE_AUTO
  , MEMORY_USAGE_AUTO_PREFER_DEVICE
  , MEMORY_USAGE_AUTO_PREFER_HOST ::
    MemoryUsage
  #-}

conNameMemoryUsage :: String
conNameMemoryUsage = "MemoryUsage"

enumPrefixMemoryUsage :: String
enumPrefixMemoryUsage = "MEMORY_USAGE_"

showTableMemoryUsage :: [(MemoryUsage, String)]
showTableMemoryUsage =
  [ (MEMORY_USAGE_UNKNOWN, "UNKNOWN")
  , (MEMORY_USAGE_GPU_ONLY, "GPU_ONLY")
  , (MEMORY_USAGE_CPU_ONLY, "CPU_ONLY")
  , (MEMORY_USAGE_CPU_TO_GPU, "CPU_TO_GPU")
  , (MEMORY_USAGE_GPU_TO_CPU, "GPU_TO_CPU")
  , (MEMORY_USAGE_CPU_COPY, "CPU_COPY")
  ,
    ( MEMORY_USAGE_GPU_LAZILY_ALLOCATED
    , "GPU_LAZILY_ALLOCATED"
    )
  , (MEMORY_USAGE_AUTO, "AUTO")
  , (MEMORY_USAGE_AUTO_PREFER_DEVICE, "AUTO_PREFER_DEVICE")
  , (MEMORY_USAGE_AUTO_PREFER_HOST, "AUTO_PREFER_HOST")
  ]

instance Show MemoryUsage where
  showsPrec =
    enumShowsPrec
      enumPrefixMemoryUsage
      showTableMemoryUsage
      conNameMemoryUsage
      (\(MemoryUsage x) -> x)
      (showsPrec 11)

instance Read MemoryUsage where
  readPrec =
    enumReadPrec
      enumPrefixMemoryUsage
      showTableMemoryUsage
      conNameMemoryUsage
      MemoryUsage

type AllocationCreateFlags = AllocationCreateFlagBits

-- | Flags to be passed as /VmaAllocationCreateInfo::flags/.
newtype AllocationCreateFlagBits = AllocationCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | Set this flag if the allocation should have its own memory block.
--
-- Use it for special, big resources, like fullscreen images used as
-- attachments.
pattern ALLOCATION_CREATE_DEDICATED_MEMORY_BIT = AllocationCreateFlagBits 0x00000001

-- | Set this flag to only try to allocate from existing @VkDeviceMemory@
-- blocks and never create new such block.
--
-- If new allocation cannot be placed in any of the existing blocks,
-- allocation fails with @VK_ERROR_OUT_OF_DEVICE_MEMORY@ error.
--
-- You should not use 'ALLOCATION_CREATE_DEDICATED_MEMORY_BIT' and
-- 'ALLOCATION_CREATE_NEVER_ALLOCATE_BIT' at the same time. It makes no
-- sense.
pattern ALLOCATION_CREATE_NEVER_ALLOCATE_BIT = AllocationCreateFlagBits 0x00000002

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
pattern ALLOCATION_CREATE_MAPPED_BIT = AllocationCreateFlagBits 0x00000004

-- | /Deprecated/
--
-- Preserved for backward compatibility. Consider using 'setAllocationName'
-- instead.
--
-- Set this flag to treat /VmaAllocationCreateInfo::pUserData/ as pointer
-- to a null-terminated string. Instead of copying pointer value, a local
-- copy of the string is made and stored in allocation\'s @pName@. The
-- string is automatically freed together with the allocation. It is also
-- used in 'buildStatsString'.
pattern ALLOCATION_CREATE_USER_DATA_COPY_STRING_BIT = AllocationCreateFlagBits 0x00000020

-- | Allocation will be created from upper stack in a double stack pool.
--
-- This flag is only allowed for custom pools created with
-- 'POOL_CREATE_LINEAR_ALGORITHM_BIT' flag.
pattern ALLOCATION_CREATE_UPPER_ADDRESS_BIT = AllocationCreateFlagBits 0x00000040

-- | Create both buffer\/image and allocation, but don\'t bind them together.
-- It is useful when you want to bind yourself to do some more advanced
-- binding, e.g. using some extensions. The flag is meaningful only with
-- functions that bind by default: 'createBuffer', 'createImage'. Otherwise
-- it is ignored.
--
-- If you want to make sure the new buffer\/image is not tied to the new
-- memory allocation through @VkMemoryDedicatedAllocateInfoKHR@ structure
-- in case the allocation ends up in its own memory block, use also flag
-- 'ALLOCATION_CREATE_CAN_ALIAS_BIT'.
pattern ALLOCATION_CREATE_DONT_BIND_BIT = AllocationCreateFlagBits 0x00000080

-- | Create allocation only if additional device memory required for it, if
-- any, won\'t exceed memory budget. Otherwise return
-- @VK_ERROR_OUT_OF_DEVICE_MEMORY@.
pattern ALLOCATION_CREATE_WITHIN_BUDGET_BIT = AllocationCreateFlagBits 0x00000100

-- | Set this flag if the allocated memory will have aliasing resources.
--
-- Usage of this flag prevents supplying @VkMemoryDedicatedAllocateInfoKHR@
-- when 'ALLOCATION_CREATE_DEDICATED_MEMORY_BIT' is specified. Otherwise
-- created dedicated memory will not be suitable for aliasing resources,
-- resulting in Vulkan Validation Layer errors.
pattern ALLOCATION_CREATE_CAN_ALIAS_BIT = AllocationCreateFlagBits 0x00000200

-- | Requests possibility to map the allocation (using 'mapMemory' or
-- 'ALLOCATION_CREATE_MAPPED_BIT').
--
-- -   If you use 'MEMORY_USAGE_AUTO' or other @VMA_MEMORY_USAGE_AUTO*@
--     value, you must use this flag to be able to map the allocation.
--     Otherwise, mapping is incorrect.
--
-- -   If you use other value of 'MemoryUsage', this flag is ignored and
--     mapping is always possible in memory types that are @HOST_VISIBLE@.
--     This includes allocations created in /Custom memory pools/.
--
-- Declares that mapped memory will only be written sequentially, e.g.
-- using @memcpy()@ or a loop writing number-by-number, never read or
-- accessed randomly, so a memory type can be selected that is uncached and
-- write-combined.
--
-- Warning
--
-- Violating this declaration may work correctly, but will likely be very
-- slow. Watch out for implicit reads introduced by doing e.g.
-- @pMappedData[i] += x;@ Better prepare your data in a local variable and
-- @memcpy()@ it to the mapped pointer all at once.
pattern ALLOCATION_CREATE_HOST_ACCESS_SEQUENTIAL_WRITE_BIT = AllocationCreateFlagBits 0x00000400

-- | Requests possibility to map the allocation (using 'mapMemory' or
-- 'ALLOCATION_CREATE_MAPPED_BIT').
--
-- -   If you use 'MEMORY_USAGE_AUTO' or other @VMA_MEMORY_USAGE_AUTO*@
--     value, you must use this flag to be able to map the allocation.
--     Otherwise, mapping is incorrect.
--
-- -   If you use other value of 'MemoryUsage', this flag is ignored and
--     mapping is always possible in memory types that are @HOST_VISIBLE@.
--     This includes allocations created in /Custom memory pools/.
--
-- Declares that mapped memory can be read, written, and accessed in random
-- order, so a @HOST_CACHED@ memory type is required.
pattern ALLOCATION_CREATE_HOST_ACCESS_RANDOM_BIT = AllocationCreateFlagBits 0x00000800

-- | Together with 'ALLOCATION_CREATE_HOST_ACCESS_SEQUENTIAL_WRITE_BIT' or
-- 'ALLOCATION_CREATE_HOST_ACCESS_RANDOM_BIT', it says that despite request
-- for host access, a not-@HOST_VISIBLE@ memory type can be selected if it
-- may improve performance.
--
-- By using this flag, you declare that you will check if the allocation
-- ended up in a @HOST_VISIBLE@ memory type (e.g. using
-- 'getAllocationMemoryProperties') and if not, you will create some
-- \"staging\" buffer and issue an explicit transfer to write\/read your
-- data. To prepare for this possibility, don\'t forget to add appropriate
-- flags like @VK_BUFFER_USAGE_TRANSFER_DST_BIT@,
-- @VK_BUFFER_USAGE_TRANSFER_SRC_BIT@ to the parameters of created buffer
-- or image.
pattern ALLOCATION_CREATE_HOST_ACCESS_ALLOW_TRANSFER_INSTEAD_BIT = AllocationCreateFlagBits 0x00001000

-- | Allocation strategy that chooses smallest possible free range for the
-- allocation to minimize memory usage and fragmentation, possibly at the
-- expense of allocation time.
pattern ALLOCATION_CREATE_STRATEGY_MIN_MEMORY_BIT = AllocationCreateFlagBits 0x00010000

-- | Allocation strategy that chooses first suitable free range for the
-- allocation - not necessarily in terms of the smallest offset but the one
-- that is easiest and fastest to find to minimize allocation time,
-- possibly at the expense of allocation quality.
pattern ALLOCATION_CREATE_STRATEGY_MIN_TIME_BIT = AllocationCreateFlagBits 0x00020000

-- | Allocation strategy that chooses always the lowest offset in available
-- space. This is not the most efficient strategy but achieves highly
-- packed data. Used internally by defragmentation, not recommended in
-- typical usage.
pattern ALLOCATION_CREATE_STRATEGY_MIN_OFFSET_BIT = AllocationCreateFlagBits 0x00040000

-- | Alias to 'ALLOCATION_CREATE_STRATEGY_MIN_MEMORY_BIT'.
pattern ALLOCATION_CREATE_STRATEGY_BEST_FIT_BIT = AllocationCreateFlagBits 0x00010000

-- | Alias to 'ALLOCATION_CREATE_STRATEGY_MIN_TIME_BIT'.
pattern ALLOCATION_CREATE_STRATEGY_FIRST_FIT_BIT = AllocationCreateFlagBits 0x00020000

-- | A bit mask to extract only @STRATEGY@ bits from entire set of flags.
pattern ALLOCATION_CREATE_STRATEGY_MASK = AllocationCreateFlagBits 0x00070000

conNameAllocationCreateFlagBits :: String
conNameAllocationCreateFlagBits = "AllocationCreateFlagBits"

enumPrefixAllocationCreateFlagBits :: String
enumPrefixAllocationCreateFlagBits = "ALLOCATION_CREATE_"

showTableAllocationCreateFlagBits :: [(AllocationCreateFlagBits, String)]
showTableAllocationCreateFlagBits =
  [
    ( ALLOCATION_CREATE_DEDICATED_MEMORY_BIT
    , "DEDICATED_MEMORY_BIT"
    )
  ,
    ( ALLOCATION_CREATE_NEVER_ALLOCATE_BIT
    , "NEVER_ALLOCATE_BIT"
    )
  ,
    ( ALLOCATION_CREATE_MAPPED_BIT
    , "MAPPED_BIT"
    )
  ,
    ( ALLOCATION_CREATE_USER_DATA_COPY_STRING_BIT
    , "USER_DATA_COPY_STRING_BIT"
    )
  ,
    ( ALLOCATION_CREATE_UPPER_ADDRESS_BIT
    , "UPPER_ADDRESS_BIT"
    )
  ,
    ( ALLOCATION_CREATE_DONT_BIND_BIT
    , "DONT_BIND_BIT"
    )
  ,
    ( ALLOCATION_CREATE_WITHIN_BUDGET_BIT
    , "WITHIN_BUDGET_BIT"
    )
  ,
    ( ALLOCATION_CREATE_CAN_ALIAS_BIT
    , "CAN_ALIAS_BIT"
    )
  ,
    ( ALLOCATION_CREATE_HOST_ACCESS_SEQUENTIAL_WRITE_BIT
    , "HOST_ACCESS_SEQUENTIAL_WRITE_BIT"
    )
  ,
    ( ALLOCATION_CREATE_HOST_ACCESS_RANDOM_BIT
    , "HOST_ACCESS_RANDOM_BIT"
    )
  ,
    ( ALLOCATION_CREATE_HOST_ACCESS_ALLOW_TRANSFER_INSTEAD_BIT
    , "HOST_ACCESS_ALLOW_TRANSFER_INSTEAD_BIT"
    )
  ,
    ( ALLOCATION_CREATE_STRATEGY_MIN_MEMORY_BIT
    , "STRATEGY_MIN_MEMORY_BIT"
    )
  ,
    ( ALLOCATION_CREATE_STRATEGY_MIN_TIME_BIT
    , "STRATEGY_MIN_TIME_BIT"
    )
  ,
    ( ALLOCATION_CREATE_STRATEGY_MIN_OFFSET_BIT
    , "STRATEGY_MIN_OFFSET_BIT"
    )
  ,
    ( ALLOCATION_CREATE_STRATEGY_BEST_FIT_BIT
    , "STRATEGY_BEST_FIT_BIT"
    )
  ,
    ( ALLOCATION_CREATE_STRATEGY_FIRST_FIT_BIT
    , "STRATEGY_FIRST_FIT_BIT"
    )
  ,
    ( ALLOCATION_CREATE_STRATEGY_MASK
    , "STRATEGY_MASK"
    )
  ]

instance Show AllocationCreateFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixAllocationCreateFlagBits
      showTableAllocationCreateFlagBits
      conNameAllocationCreateFlagBits
      (\(AllocationCreateFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read AllocationCreateFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixAllocationCreateFlagBits
      showTableAllocationCreateFlagBits
      conNameAllocationCreateFlagBits
      AllocationCreateFlagBits

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
pattern POOL_CREATE_LINEAR_ALGORITHM_BIT = PoolCreateFlagBits 0x00000004

-- | Bit mask to extract only @ALGORITHM@ bits from entire set of flags.
pattern POOL_CREATE_ALGORITHM_MASK = PoolCreateFlagBits 0x00000004

conNamePoolCreateFlagBits :: String
conNamePoolCreateFlagBits = "PoolCreateFlagBits"

enumPrefixPoolCreateFlagBits :: String
enumPrefixPoolCreateFlagBits = "POOL_CREATE_"

showTablePoolCreateFlagBits :: [(PoolCreateFlagBits, String)]
showTablePoolCreateFlagBits =
  [
    ( POOL_CREATE_IGNORE_BUFFER_IMAGE_GRANULARITY_BIT
    , "IGNORE_BUFFER_IMAGE_GRANULARITY_BIT"
    )
  ,
    ( POOL_CREATE_LINEAR_ALGORITHM_BIT
    , "LINEAR_ALGORITHM_BIT"
    )
  , (POOL_CREATE_ALGORITHM_MASK, "ALGORITHM_MASK")
  ]

instance Show PoolCreateFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixPoolCreateFlagBits
      showTablePoolCreateFlagBits
      conNamePoolCreateFlagBits
      (\(PoolCreateFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read PoolCreateFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixPoolCreateFlagBits
      showTablePoolCreateFlagBits
      conNamePoolCreateFlagBits
      PoolCreateFlagBits

type DefragmentationFlags = DefragmentationFlagBits

-- | Flags to be passed as /VmaDefragmentationInfo::flags/.
newtype DefragmentationFlagBits = DefragmentationFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

pattern DEFRAGMENTATION_FLAG_ALGORITHM_FAST_BIT = DefragmentationFlagBits 0x00000001

pattern DEFRAGMENTATION_FLAG_ALGORITHM_BALANCED_BIT = DefragmentationFlagBits 0x00000002

pattern DEFRAGMENTATION_FLAG_ALGORITHM_FULL_BIT = DefragmentationFlagBits 0x00000004

-- | Use the most roboust algorithm at the cost of time to compute and number
-- of copies to make. Only available when bufferImageGranularity is greater
-- than 1, since it aims to reduce alignment issues between different types
-- of resources. Otherwise falls back to same behavior as
-- 'DEFRAGMENTATION_FLAG_ALGORITHM_FULL_BIT'.
pattern DEFRAGMENTATION_FLAG_ALGORITHM_EXTENSIVE_BIT = DefragmentationFlagBits 0x00000008

-- | A bit mask to extract only @ALGORITHM@ bits from entire set of flags.
pattern DEFRAGMENTATION_FLAG_ALGORITHM_MASK = DefragmentationFlagBits 0x0000000f

conNameDefragmentationFlagBits :: String
conNameDefragmentationFlagBits = "DefragmentationFlagBits"

enumPrefixDefragmentationFlagBits :: String
enumPrefixDefragmentationFlagBits = "DEFRAGMENTATION_FLAG_ALGORITHM_"

showTableDefragmentationFlagBits :: [(DefragmentationFlagBits, String)]
showTableDefragmentationFlagBits =
  [
    ( DEFRAGMENTATION_FLAG_ALGORITHM_FAST_BIT
    , "FAST_BIT"
    )
  ,
    ( DEFRAGMENTATION_FLAG_ALGORITHM_BALANCED_BIT
    , "BALANCED_BIT"
    )
  ,
    ( DEFRAGMENTATION_FLAG_ALGORITHM_FULL_BIT
    , "FULL_BIT"
    )
  ,
    ( DEFRAGMENTATION_FLAG_ALGORITHM_EXTENSIVE_BIT
    , "EXTENSIVE_BIT"
    )
  ,
    ( DEFRAGMENTATION_FLAG_ALGORITHM_MASK
    , "MASK"
    )
  ]

instance Show DefragmentationFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixDefragmentationFlagBits
      showTableDefragmentationFlagBits
      conNameDefragmentationFlagBits
      (\(DefragmentationFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read DefragmentationFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixDefragmentationFlagBits
      showTableDefragmentationFlagBits
      conNameDefragmentationFlagBits
      DefragmentationFlagBits

-- | Operation performed on single defragmentation move. See structure
-- 'DefragmentationMove'.
newtype DefragmentationMoveOperation = DefragmentationMoveOperation Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | Buffer\/image has been recreated at @dstTmpAllocation@, data has been
-- copied, old buffer\/image has been destroyed. @srcAllocation@ should be
-- changed to point to the new place. This is the default value set by
-- 'beginDefragmentationPass'.
pattern DEFRAGMENTATION_MOVE_OPERATION_COPY = DefragmentationMoveOperation 0

-- | Set this value if you cannot move the allocation. New place reserved at
-- @dstTmpAllocation@ will be freed. @srcAllocation@ will remain unchanged.
pattern DEFRAGMENTATION_MOVE_OPERATION_IGNORE = DefragmentationMoveOperation 1

-- | Set this value if you decide to abandon the allocation and you destroyed
-- the buffer\/image. New place reserved at @dstTmpAllocation@ will be
-- freed, along with @srcAllocation@, which will be destroyed.
pattern DEFRAGMENTATION_MOVE_OPERATION_DESTROY = DefragmentationMoveOperation 2

{-# COMPLETE
  DEFRAGMENTATION_MOVE_OPERATION_COPY
  , DEFRAGMENTATION_MOVE_OPERATION_IGNORE
  , DEFRAGMENTATION_MOVE_OPERATION_DESTROY ::
    DefragmentationMoveOperation
  #-}

conNameDefragmentationMoveOperation :: String
conNameDefragmentationMoveOperation = "DefragmentationMoveOperation"

enumPrefixDefragmentationMoveOperation :: String
enumPrefixDefragmentationMoveOperation = "DEFRAGMENTATION_MOVE_OPERATION_"

showTableDefragmentationMoveOperation :: [(DefragmentationMoveOperation, String)]
showTableDefragmentationMoveOperation =
  [
    ( DEFRAGMENTATION_MOVE_OPERATION_COPY
    , "COPY"
    )
  ,
    ( DEFRAGMENTATION_MOVE_OPERATION_IGNORE
    , "IGNORE"
    )
  ,
    ( DEFRAGMENTATION_MOVE_OPERATION_DESTROY
    , "DESTROY"
    )
  ]

instance Show DefragmentationMoveOperation where
  showsPrec =
    enumShowsPrec
      enumPrefixDefragmentationMoveOperation
      showTableDefragmentationMoveOperation
      conNameDefragmentationMoveOperation
      (\(DefragmentationMoveOperation x) -> x)
      (showsPrec 11)

instance Read DefragmentationMoveOperation where
  readPrec =
    enumReadPrec
      enumPrefixDefragmentationMoveOperation
      showTableDefragmentationMoveOperation
      conNameDefragmentationMoveOperation
      DefragmentationMoveOperation

type VirtualBlockCreateFlags = VirtualBlockCreateFlagBits

-- | Flags to be passed as /VmaVirtualBlockCreateInfo::flags/.
newtype VirtualBlockCreateFlagBits = VirtualBlockCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | Enables alternative, linear allocation algorithm in this virtual block.
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
pattern VIRTUAL_BLOCK_CREATE_LINEAR_ALGORITHM_BIT = VirtualBlockCreateFlagBits 0x00000001

-- | Bit mask to extract only @ALGORITHM@ bits from entire set of flags.
pattern VIRTUAL_BLOCK_CREATE_ALGORITHM_MASK = VirtualBlockCreateFlagBits 0x00000001

conNameVirtualBlockCreateFlagBits :: String
conNameVirtualBlockCreateFlagBits = "VirtualBlockCreateFlagBits"

enumPrefixVirtualBlockCreateFlagBits :: String
enumPrefixVirtualBlockCreateFlagBits = "VIRTUAL_BLOCK_CREATE_"

showTableVirtualBlockCreateFlagBits :: [(VirtualBlockCreateFlagBits, String)]
showTableVirtualBlockCreateFlagBits =
  [
    ( VIRTUAL_BLOCK_CREATE_LINEAR_ALGORITHM_BIT
    , "LINEAR_ALGORITHM_BIT"
    )
  ,
    ( VIRTUAL_BLOCK_CREATE_ALGORITHM_MASK
    , "ALGORITHM_MASK"
    )
  ]

instance Show VirtualBlockCreateFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixVirtualBlockCreateFlagBits
      showTableVirtualBlockCreateFlagBits
      conNameVirtualBlockCreateFlagBits
      (\(VirtualBlockCreateFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read VirtualBlockCreateFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixVirtualBlockCreateFlagBits
      showTableVirtualBlockCreateFlagBits
      conNameVirtualBlockCreateFlagBits
      VirtualBlockCreateFlagBits

type VirtualAllocationCreateFlags = VirtualAllocationCreateFlagBits

-- | Flags to be passed as /VmaVirtualAllocationCreateInfo::flags/.
newtype VirtualAllocationCreateFlagBits = VirtualAllocationCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | Allocation will be created from upper stack in a double stack pool.
--
-- This flag is only allowed for virtual blocks created with
-- 'VIRTUAL_BLOCK_CREATE_LINEAR_ALGORITHM_BIT' flag.
pattern VIRTUAL_ALLOCATION_CREATE_UPPER_ADDRESS_BIT = VirtualAllocationCreateFlagBits 0x00000040

-- | Allocation strategy that tries to minimize memory usage.
pattern VIRTUAL_ALLOCATION_CREATE_STRATEGY_MIN_MEMORY_BIT = VirtualAllocationCreateFlagBits 0x00010000

-- | Allocation strategy that tries to minimize allocation time.
pattern VIRTUAL_ALLOCATION_CREATE_STRATEGY_MIN_TIME_BIT = VirtualAllocationCreateFlagBits 0x00020000

-- | Allocation strategy that chooses always the lowest offset in available
-- space. This is not the most efficient strategy but achieves highly
-- packed data.
pattern VIRTUAL_ALLOCATION_CREATE_STRATEGY_MIN_OFFSET_BIT = VirtualAllocationCreateFlagBits 0x00040000

-- | A bit mask to extract only @STRATEGY@ bits from entire set of flags.
--
-- These strategy flags are binary compatible with equivalent flags in
-- 'AllocationCreateFlagBits'.
pattern VIRTUAL_ALLOCATION_CREATE_STRATEGY_MASK = VirtualAllocationCreateFlagBits 0x00070000

conNameVirtualAllocationCreateFlagBits :: String
conNameVirtualAllocationCreateFlagBits = "VirtualAllocationCreateFlagBits"

enumPrefixVirtualAllocationCreateFlagBits :: String
enumPrefixVirtualAllocationCreateFlagBits = "VIRTUAL_ALLOCATION_CREATE_"

showTableVirtualAllocationCreateFlagBits :: [(VirtualAllocationCreateFlagBits, String)]
showTableVirtualAllocationCreateFlagBits =
  [
    ( VIRTUAL_ALLOCATION_CREATE_UPPER_ADDRESS_BIT
    , "UPPER_ADDRESS_BIT"
    )
  ,
    ( VIRTUAL_ALLOCATION_CREATE_STRATEGY_MIN_MEMORY_BIT
    , "STRATEGY_MIN_MEMORY_BIT"
    )
  ,
    ( VIRTUAL_ALLOCATION_CREATE_STRATEGY_MIN_TIME_BIT
    , "STRATEGY_MIN_TIME_BIT"
    )
  ,
    ( VIRTUAL_ALLOCATION_CREATE_STRATEGY_MIN_OFFSET_BIT
    , "STRATEGY_MIN_OFFSET_BIT"
    )
  ,
    ( VIRTUAL_ALLOCATION_CREATE_STRATEGY_MASK
    , "STRATEGY_MASK"
    )
  ]

instance Show VirtualAllocationCreateFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixVirtualAllocationCreateFlagBits
      showTableVirtualAllocationCreateFlagBits
      conNameVirtualAllocationCreateFlagBits
      (\(VirtualAllocationCreateFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read VirtualAllocationCreateFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixVirtualAllocationCreateFlagBits
      showTableVirtualAllocationCreateFlagBits
      conNameVirtualAllocationCreateFlagBits
      VirtualAllocationCreateFlagBits

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
newtype Allocation = Allocation Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show Allocation where
  showsPrec p (Allocation x) = showParen (p >= 11) (showString "Allocation 0x" . showHex x)


-- | VmaDefragmentationContext
--
-- An opaque object that represents started defragmentation process.
--
-- Fill structure 'DefragmentationInfo' and call function
-- 'beginDefragmentation' to create it. Call function 'endDefragmentation'
-- to destroy it.
newtype DefragmentationContext = DefragmentationContext Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show DefragmentationContext where
  showsPrec p (DefragmentationContext x) = showParen (p >= 11) (showString "DefragmentationContext 0x" . showHex x)


-- | VmaVirtualAllocation
--
-- Represents single memory allocation done inside 'VirtualBlock'.
--
-- Use it as a unique identifier to virtual allocation within the single
-- block.
--
-- Use value @VK_NULL_HANDLE@ to represent a null\/invalid allocation.
newtype VirtualAllocation = VirtualAllocation Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show VirtualAllocation where
  showsPrec p (VirtualAllocation x) = showParen (p >= 11) (showString "VirtualAllocation 0x" . showHex x)


-- | VmaVirtualBlock
--
-- Handle to a virtual block object that allows to use core allocation
-- algorithm without allocating any real GPU memory.
--
-- Fill in 'VirtualBlockCreateInfo' structure and use 'createVirtualBlock'
-- to create it. Use 'destroyVirtualBlock' to destroy it. For more
-- information, see documentation chapter /Virtual allocator/.
--
-- This object is not thread-safe - should not be used from multiple
-- threads simultaneously, must be synchronized externally.
newtype VirtualBlock = VirtualBlock Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show VirtualBlock where
  showsPrec p (VirtualBlock x) = showParen (p >= 11) (showString "VirtualBlock 0x" . showHex x)


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
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
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


-- | VmaVulkanFunctions
--
-- Pointers to some Vulkan functions - a subset used by the library.
--
-- Used in /VmaAllocatorCreateInfo::pVulkanFunctions/.
data VulkanFunctions = VulkanFunctions
  { -- | Required when using VMA_DYNAMIC_VULKAN_FUNCTIONS.
    vkGetInstanceProcAddr :: PFN_vkGetInstanceProcAddr
  , -- | Required when using VMA_DYNAMIC_VULKAN_FUNCTIONS.
    vkGetDeviceProcAddr :: PFN_vkGetDeviceProcAddr
  , 
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
  , -- | Fetch \"vkGetBufferMemoryRequirements2\" on Vulkan >= 1.1, fetch
    -- \"vkGetBufferMemoryRequirements2KHR\" when using
    -- VK_KHR_dedicated_allocation extension.
    vkGetBufferMemoryRequirements2KHR :: PFN_vkGetBufferMemoryRequirements2KHR
  , -- | Fetch \"vkGetImageMemoryRequirements2\" on Vulkan >= 1.1, fetch
    -- \"vkGetImageMemoryRequirements2KHR\" when using
    -- VK_KHR_dedicated_allocation extension.
    vkGetImageMemoryRequirements2KHR :: PFN_vkGetImageMemoryRequirements2KHR
  , -- | Fetch \"vkBindBufferMemory2\" on Vulkan >= 1.1, fetch
    -- \"vkBindBufferMemory2KHR\" when using VK_KHR_bind_memory2 extension.
    vkBindBufferMemory2KHR :: PFN_vkBindBufferMemory2KHR
  , -- | Fetch \"vkBindImageMemory2\" on Vulkan >= 1.1, fetch
    -- \"vkBindImageMemory2KHR\" when using VK_KHR_bind_memory2 extension.
    vkBindImageMemory2KHR :: PFN_vkBindImageMemory2KHR
  , 
    vkGetPhysicalDeviceMemoryProperties2KHR :: PFN_vkGetPhysicalDeviceMemoryProperties2KHR
  , -- | Fetch from \"vkGetDeviceBufferMemoryRequirements\" on Vulkan >= 1.3, but
    -- you can also fetch it from \"vkGetDeviceBufferMemoryRequirementsKHR\" if
    -- you enabled extension VK_KHR_maintenance4.
    vkGetDeviceBufferMemoryRequirements :: PFN_vkGetDeviceBufferMemoryRequirements
  , -- | Fetch from \"vkGetDeviceImageMemoryRequirements\" on Vulkan >= 1.3, but
    -- you can also fetch it from \"vkGetDeviceImageMemoryRequirementsKHR\" if
    -- you enabled extension VK_KHR_maintenance4.
    vkGetDeviceImageMemoryRequirements :: PFN_vkGetDeviceImageMemoryRequirements
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (VulkanFunctions)
#endif
deriving instance Show VulkanFunctions

instance ToCStruct VulkanFunctions where
  withCStruct x f = allocaBytes 208 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p VulkanFunctions{..} f = do
    poke ((p `plusPtr` 0 :: Ptr PFN_vkGetInstanceProcAddr)) (vkGetInstanceProcAddr)
    poke ((p `plusPtr` 8 :: Ptr PFN_vkGetDeviceProcAddr)) (vkGetDeviceProcAddr)
    poke ((p `plusPtr` 16 :: Ptr PFN_vkGetPhysicalDeviceProperties)) (vkGetPhysicalDeviceProperties)
    poke ((p `plusPtr` 24 :: Ptr PFN_vkGetPhysicalDeviceMemoryProperties)) (vkGetPhysicalDeviceMemoryProperties)
    poke ((p `plusPtr` 32 :: Ptr PFN_vkAllocateMemory)) (vkAllocateMemory)
    poke ((p `plusPtr` 40 :: Ptr PFN_vkFreeMemory)) (vkFreeMemory)
    poke ((p `plusPtr` 48 :: Ptr PFN_vkMapMemory)) (vkMapMemory)
    poke ((p `plusPtr` 56 :: Ptr PFN_vkUnmapMemory)) (vkUnmapMemory)
    poke ((p `plusPtr` 64 :: Ptr PFN_vkFlushMappedMemoryRanges)) (vkFlushMappedMemoryRanges)
    poke ((p `plusPtr` 72 :: Ptr PFN_vkInvalidateMappedMemoryRanges)) (vkInvalidateMappedMemoryRanges)
    poke ((p `plusPtr` 80 :: Ptr PFN_vkBindBufferMemory)) (vkBindBufferMemory)
    poke ((p `plusPtr` 88 :: Ptr PFN_vkBindImageMemory)) (vkBindImageMemory)
    poke ((p `plusPtr` 96 :: Ptr PFN_vkGetBufferMemoryRequirements)) (vkGetBufferMemoryRequirements)
    poke ((p `plusPtr` 104 :: Ptr PFN_vkGetImageMemoryRequirements)) (vkGetImageMemoryRequirements)
    poke ((p `plusPtr` 112 :: Ptr PFN_vkCreateBuffer)) (vkCreateBuffer)
    poke ((p `plusPtr` 120 :: Ptr PFN_vkDestroyBuffer)) (vkDestroyBuffer)
    poke ((p `plusPtr` 128 :: Ptr PFN_vkCreateImage)) (vkCreateImage)
    poke ((p `plusPtr` 136 :: Ptr PFN_vkDestroyImage)) (vkDestroyImage)
    poke ((p `plusPtr` 144 :: Ptr PFN_vkCmdCopyBuffer)) (vkCmdCopyBuffer)
    poke ((p `plusPtr` 152 :: Ptr PFN_vkGetBufferMemoryRequirements2KHR)) (vkGetBufferMemoryRequirements2KHR)
    poke ((p `plusPtr` 160 :: Ptr PFN_vkGetImageMemoryRequirements2KHR)) (vkGetImageMemoryRequirements2KHR)
    poke ((p `plusPtr` 168 :: Ptr PFN_vkBindBufferMemory2KHR)) (vkBindBufferMemory2KHR)
    poke ((p `plusPtr` 176 :: Ptr PFN_vkBindImageMemory2KHR)) (vkBindImageMemory2KHR)
    poke ((p `plusPtr` 184 :: Ptr PFN_vkGetPhysicalDeviceMemoryProperties2KHR)) (vkGetPhysicalDeviceMemoryProperties2KHR)
    poke ((p `plusPtr` 192 :: Ptr PFN_vkGetDeviceBufferMemoryRequirements)) (vkGetDeviceBufferMemoryRequirements)
    poke ((p `plusPtr` 200 :: Ptr PFN_vkGetDeviceImageMemoryRequirements)) (vkGetDeviceImageMemoryRequirements)
    f
  cStructSize = 208
  cStructAlignment = 8
  pokeZeroCStruct _ f = f

instance FromCStruct VulkanFunctions where
  peekCStruct p = do
    vkGetInstanceProcAddr <- peek @PFN_vkGetInstanceProcAddr ((p `plusPtr` 0 :: Ptr PFN_vkGetInstanceProcAddr))
    vkGetDeviceProcAddr <- peek @PFN_vkGetDeviceProcAddr ((p `plusPtr` 8 :: Ptr PFN_vkGetDeviceProcAddr))
    vkGetPhysicalDeviceProperties <- peek @PFN_vkGetPhysicalDeviceProperties ((p `plusPtr` 16 :: Ptr PFN_vkGetPhysicalDeviceProperties))
    vkGetPhysicalDeviceMemoryProperties <- peek @PFN_vkGetPhysicalDeviceMemoryProperties ((p `plusPtr` 24 :: Ptr PFN_vkGetPhysicalDeviceMemoryProperties))
    vkAllocateMemory <- peek @PFN_vkAllocateMemory ((p `plusPtr` 32 :: Ptr PFN_vkAllocateMemory))
    vkFreeMemory <- peek @PFN_vkFreeMemory ((p `plusPtr` 40 :: Ptr PFN_vkFreeMemory))
    vkMapMemory <- peek @PFN_vkMapMemory ((p `plusPtr` 48 :: Ptr PFN_vkMapMemory))
    vkUnmapMemory <- peek @PFN_vkUnmapMemory ((p `plusPtr` 56 :: Ptr PFN_vkUnmapMemory))
    vkFlushMappedMemoryRanges <- peek @PFN_vkFlushMappedMemoryRanges ((p `plusPtr` 64 :: Ptr PFN_vkFlushMappedMemoryRanges))
    vkInvalidateMappedMemoryRanges <- peek @PFN_vkInvalidateMappedMemoryRanges ((p `plusPtr` 72 :: Ptr PFN_vkInvalidateMappedMemoryRanges))
    vkBindBufferMemory <- peek @PFN_vkBindBufferMemory ((p `plusPtr` 80 :: Ptr PFN_vkBindBufferMemory))
    vkBindImageMemory <- peek @PFN_vkBindImageMemory ((p `plusPtr` 88 :: Ptr PFN_vkBindImageMemory))
    vkGetBufferMemoryRequirements <- peek @PFN_vkGetBufferMemoryRequirements ((p `plusPtr` 96 :: Ptr PFN_vkGetBufferMemoryRequirements))
    vkGetImageMemoryRequirements <- peek @PFN_vkGetImageMemoryRequirements ((p `plusPtr` 104 :: Ptr PFN_vkGetImageMemoryRequirements))
    vkCreateBuffer <- peek @PFN_vkCreateBuffer ((p `plusPtr` 112 :: Ptr PFN_vkCreateBuffer))
    vkDestroyBuffer <- peek @PFN_vkDestroyBuffer ((p `plusPtr` 120 :: Ptr PFN_vkDestroyBuffer))
    vkCreateImage <- peek @PFN_vkCreateImage ((p `plusPtr` 128 :: Ptr PFN_vkCreateImage))
    vkDestroyImage <- peek @PFN_vkDestroyImage ((p `plusPtr` 136 :: Ptr PFN_vkDestroyImage))
    vkCmdCopyBuffer <- peek @PFN_vkCmdCopyBuffer ((p `plusPtr` 144 :: Ptr PFN_vkCmdCopyBuffer))
    vkGetBufferMemoryRequirements2KHR <- peek @PFN_vkGetBufferMemoryRequirements2KHR ((p `plusPtr` 152 :: Ptr PFN_vkGetBufferMemoryRequirements2KHR))
    vkGetImageMemoryRequirements2KHR <- peek @PFN_vkGetImageMemoryRequirements2KHR ((p `plusPtr` 160 :: Ptr PFN_vkGetImageMemoryRequirements2KHR))
    vkBindBufferMemory2KHR <- peek @PFN_vkBindBufferMemory2KHR ((p `plusPtr` 168 :: Ptr PFN_vkBindBufferMemory2KHR))
    vkBindImageMemory2KHR <- peek @PFN_vkBindImageMemory2KHR ((p `plusPtr` 176 :: Ptr PFN_vkBindImageMemory2KHR))
    vkGetPhysicalDeviceMemoryProperties2KHR <- peek @PFN_vkGetPhysicalDeviceMemoryProperties2KHR ((p `plusPtr` 184 :: Ptr PFN_vkGetPhysicalDeviceMemoryProperties2KHR))
    vkGetDeviceBufferMemoryRequirements <- peek @PFN_vkGetDeviceBufferMemoryRequirements ((p `plusPtr` 192 :: Ptr PFN_vkGetDeviceBufferMemoryRequirements))
    vkGetDeviceImageMemoryRequirements <- peek @PFN_vkGetDeviceImageMemoryRequirements ((p `plusPtr` 200 :: Ptr PFN_vkGetDeviceImageMemoryRequirements))
    pure $ VulkanFunctions
             vkGetInstanceProcAddr
             vkGetDeviceProcAddr
             vkGetPhysicalDeviceProperties
             vkGetPhysicalDeviceMemoryProperties
             vkAllocateMemory
             vkFreeMemory
             vkMapMemory
             vkUnmapMemory
             vkFlushMappedMemoryRanges
             vkInvalidateMappedMemoryRanges
             vkBindBufferMemory
             vkBindImageMemory
             vkGetBufferMemoryRequirements
             vkGetImageMemoryRequirements
             vkCreateBuffer
             vkDestroyBuffer
             vkCreateImage
             vkDestroyImage
             vkCmdCopyBuffer
             vkGetBufferMemoryRequirements2KHR
             vkGetImageMemoryRequirements2KHR
             vkBindBufferMemory2KHR
             vkBindImageMemory2KHR
             vkGetPhysicalDeviceMemoryProperties2KHR
             vkGetDeviceBufferMemoryRequirements
             vkGetDeviceImageMemoryRequirements

instance Storable VulkanFunctions where
  sizeOf ~_ = 208
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
           zero
           zero
           zero
           zero


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
    -- @VkApplicationInfo::apiVersion@. Only versions 1.0, 1.1, 1.2, 1.3 are
    -- supported by the current implementation. Leaving it initialized to zero
    -- is equivalent to @VK_API_VERSION_1_0@.
    vulkanApiVersion :: Word32
  , -- | Either null or a pointer to an array of external memory handle types for
    -- each Vulkan memory type.
    --
    -- If not NULL, it must be a pointer to an array of
    -- @VkPhysicalDeviceMemoryProperties::memoryTypeCount@ elements, defining
    -- external memory handle types of particular Vulkan memory type, to be
    -- passed using @VkExportMemoryAllocateInfoKHR@.
    --
    -- Any of the elements may be equal to 0, which means not to use
    -- @VkExportMemoryAllocateInfoKHR@ on this memory type. This is also the
    -- default in case of @pTypeExternalMemoryHandleTypes@ = NULL.
    typeExternalMemoryHandleTypes :: Ptr ExternalMemoryHandleTypeFlagsKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AllocatorCreateInfo)
#endif
deriving instance Show AllocatorCreateInfo

instance ToCStruct AllocatorCreateInfo where
  withCStruct x f = allocaBytes 88 $ \p -> pokeCStruct p x (f p)
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
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr DeviceSize))) (heapSizeLimit)
    pVulkanFunctions'' <- case (vulkanFunctions) of
      Nothing -> pure nullPtr
      Just j -> ContT $ withCStruct (j)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr VulkanFunctions))) pVulkanFunctions''
    lift $ poke ((p `plusPtr` 64 :: Ptr (Ptr Instance_T))) (instance')
    lift $ poke ((p `plusPtr` 72 :: Ptr Word32)) (vulkanApiVersion)
    lift $ poke ((p `plusPtr` 80 :: Ptr (Ptr ExternalMemoryHandleTypeFlagsKHR))) (typeExternalMemoryHandleTypes)
    lift $ f
  cStructSize = 88
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr AllocatorCreateFlags)) (zero)
    poke ((p `plusPtr` 8 :: Ptr (Ptr PhysicalDevice_T))) (zero)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Device_T))) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 64 :: Ptr (Ptr Instance_T))) (zero)
    poke ((p `plusPtr` 72 :: Ptr Word32)) (zero)
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
    pHeapSizeLimit <- peek @(Ptr DeviceSize) ((p `plusPtr` 48 :: Ptr (Ptr DeviceSize)))
    pVulkanFunctions <- peek @(Ptr VulkanFunctions) ((p `plusPtr` 56 :: Ptr (Ptr VulkanFunctions)))
    pVulkanFunctions' <- maybePeek (\j -> peekCStruct @VulkanFunctions (j)) pVulkanFunctions
    instance' <- peek @(Ptr Instance_T) ((p `plusPtr` 64 :: Ptr (Ptr Instance_T)))
    vulkanApiVersion <- peek @Word32 ((p `plusPtr` 72 :: Ptr Word32))
    pTypeExternalMemoryHandleTypes <- peek @(Ptr ExternalMemoryHandleTypeFlagsKHR) ((p `plusPtr` 80 :: Ptr (Ptr ExternalMemoryHandleTypeFlagsKHR)))
    pure $ AllocatorCreateInfo
             flags
             physicalDevice
             device
             preferredLargeHeapBlockSize
             pAllocationCallbacks'
             pDeviceMemoryCallbacks'
             pHeapSizeLimit
             pVulkanFunctions'
             instance'
             vulkanApiVersion
             pTypeExternalMemoryHandleTypes

instance Zero AllocatorCreateInfo where
  zero = AllocatorCreateInfo
           zero
           zero
           zero
           zero
           Nothing
           Nothing
           zero
           Nothing
           zero
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
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
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


-- | VmaStatistics
--
-- Calculated statistics of memory usage e.g. in a specific memory type,
-- heap, custom pool, or total.
--
-- These are fast to calculate. See functions: 'getHeapBudgets',
-- 'getPoolStatistics'.
data Statistics = Statistics
  { -- | Number of @VkDeviceMemory@ objects - Vulkan memory blocks allocated.
    blockCount :: Word32
  , -- | Number of 'Allocation' objects allocated.
    --
    -- Dedicated allocations have their own blocks, so each one adds 1 to
    -- @allocationCount@ as well as @blockCount@.
    allocationCount :: Word32
  , -- | Number of bytes allocated in @VkDeviceMemory@ blocks.
    --
    -- Note
    --
    -- To avoid confusion, please be aware that what Vulkan calls an
    -- \"allocation\" - a whole @VkDeviceMemory@ object (e.g. as in
    -- @VkPhysicalDeviceLimits::maxMemoryAllocationCount@) is called a
    -- \"block\" in VMA, while VMA calls \"allocation\" a 'Allocation' object
    -- that represents a memory region sub-allocated from such block, usually
    -- for a single buffer or image.
    blockBytes :: DeviceSize
  , -- | Total number of bytes occupied by all 'Allocation' objects.
    --
    -- Always less or equal than @blockBytes@. Difference
    -- @(blockBytes - allocationBytes)@ is the amount of memory allocated from
    -- Vulkan but unused by any 'Allocation'.
    allocationBytes :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (Statistics)
#endif
deriving instance Show Statistics

instance ToCStruct Statistics where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p Statistics{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (blockCount)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (allocationCount)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (blockBytes)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (allocationBytes)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct Statistics where
  peekCStruct p = do
    blockCount <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    allocationCount <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    blockBytes <- peek @DeviceSize ((p `plusPtr` 8 :: Ptr DeviceSize))
    allocationBytes <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    pure $ Statistics
             blockCount allocationCount blockBytes allocationBytes

instance Storable Statistics where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero Statistics where
  zero = Statistics
           zero
           zero
           zero
           zero


-- | VmaDetailedStatistics
--
-- More detailed statistics than 'Statistics'.
--
-- These are slower to calculate. Use for debugging purposes. See
-- functions: 'calculateStatistics', 'calculatePoolStatistics'.
--
-- Previous version of the statistics API provided averages, but they have
-- been removed because they can be easily calculated as:
--
-- > VkDeviceSize allocationSizeAvg = detailedStats.statistics.allocationBytes / detailedStats.statistics.allocationCount;
-- > VkDeviceSize unusedBytes = detailedStats.statistics.blockBytes - detailedStats.statistics.allocationBytes;
-- > VkDeviceSize unusedRangeSizeAvg = unusedBytes / detailedStats.unusedRangeCount;
data DetailedStatistics = DetailedStatistics
  { -- | Basic statistics.
    statistics :: Statistics
  , -- | Number of free ranges of memory between allocations.
    unusedRangeCount :: Word32
  , -- | Smallest allocation size. @VK_WHOLE_SIZE@ if there are 0 allocations.
    allocationSizeMin :: DeviceSize
  , -- | Largest allocation size. 0 if there are 0 allocations.
    allocationSizeMax :: DeviceSize
  , -- | Smallest empty range size. @VK_WHOLE_SIZE@ if there are 0 empty ranges.
    unusedRangeSizeMin :: DeviceSize
  , -- | Largest empty range size. 0 if there are 0 empty ranges.
    unusedRangeSizeMax :: DeviceSize
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DetailedStatistics)
#endif
deriving instance Show DetailedStatistics

instance ToCStruct DetailedStatistics where
  withCStruct x f = allocaBytes 64 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DetailedStatistics{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Statistics)) (statistics)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (unusedRangeCount)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (allocationSizeMin)
    poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (allocationSizeMax)
    poke ((p `plusPtr` 48 :: Ptr DeviceSize)) (unusedRangeSizeMin)
    poke ((p `plusPtr` 56 :: Ptr DeviceSize)) (unusedRangeSizeMax)
    f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Statistics)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 48 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 56 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct DetailedStatistics where
  peekCStruct p = do
    statistics <- peekCStruct @Statistics ((p `plusPtr` 0 :: Ptr Statistics))
    unusedRangeCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    allocationSizeMin <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    allocationSizeMax <- peek @DeviceSize ((p `plusPtr` 40 :: Ptr DeviceSize))
    unusedRangeSizeMin <- peek @DeviceSize ((p `plusPtr` 48 :: Ptr DeviceSize))
    unusedRangeSizeMax <- peek @DeviceSize ((p `plusPtr` 56 :: Ptr DeviceSize))
    pure $ DetailedStatistics
             statistics
             unusedRangeCount
             allocationSizeMin
             allocationSizeMax
             unusedRangeSizeMin
             unusedRangeSizeMax

instance Storable DetailedStatistics where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DetailedStatistics where
  zero = DetailedStatistics
           zero
           zero
           zero
           zero
           zero
           zero


-- | VmaTotalStatistics
--
-- -   'DetailedStatistics' /memoryType/ [VK_MAX_MEMORY_TYPES]
--
-- -   'DetailedStatistics' /memoryHeap/ [VK_MAX_MEMORY_HEAPS]
--
-- -   'DetailedStatistics' /total/
--
-- General statistics from current state of the Allocator - total memory
-- usage across all memory heaps and types.
--
-- These are slower to calculate. Use for debugging purposes. See function
-- 'calculateStatistics'.
--
-- === memoryHeap
--
-- memoryHeap
-- VmaTotalStatistics
-- VmaTotalStatistics
-- memoryHeap
-- @VmaDetailedStatistics VmaTotalStatistics::memoryHeap[VK_MAX_MEMORY_HEAPS]@
--
-- === memoryType
--
-- memoryType
-- VmaTotalStatistics
-- VmaTotalStatistics
-- memoryType
-- @VmaDetailedStatistics VmaTotalStatistics::memoryType[VK_MAX_MEMORY_TYPES]@
data TotalStatistics = TotalStatistics
  { -- No documentation found for Nested "VmaTotalStatistics" "memoryType"
    memoryType :: Vector DetailedStatistics
  , -- No documentation found for Nested "VmaTotalStatistics" "memoryHeap"
    memoryHeap :: Vector DetailedStatistics
  , 
    total :: DetailedStatistics
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (TotalStatistics)
#endif
deriving instance Show TotalStatistics

instance ToCStruct TotalStatistics where
  withCStruct x f = allocaBytes 3136 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p TotalStatistics{..} f = do
    unless ((Data.Vector.length $ (memoryType)) <= MAX_MEMORY_TYPES) $
      throwIO $ IOError Nothing InvalidArgument "" "memoryType is too long, a maximum of MAX_MEMORY_TYPES elements are allowed" Nothing Nothing
    Data.Vector.imapM_ (\i e -> poke ((lowerArrayPtr ((p `plusPtr` 0 :: Ptr (FixedArray MAX_MEMORY_TYPES DetailedStatistics)))) `plusPtr` (64 * (i)) :: Ptr DetailedStatistics) (e)) (memoryType)
    unless ((Data.Vector.length $ (memoryHeap)) <= MAX_MEMORY_HEAPS) $
      throwIO $ IOError Nothing InvalidArgument "" "memoryHeap is too long, a maximum of MAX_MEMORY_HEAPS elements are allowed" Nothing Nothing
    Data.Vector.imapM_ (\i e -> poke ((lowerArrayPtr ((p `plusPtr` 2048 :: Ptr (FixedArray MAX_MEMORY_HEAPS DetailedStatistics)))) `plusPtr` (64 * (i)) :: Ptr DetailedStatistics) (e)) (memoryHeap)
    poke ((p `plusPtr` 3072 :: Ptr DetailedStatistics)) (total)
    f
  cStructSize = 3136
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 3072 :: Ptr DetailedStatistics)) (zero)
    f

instance FromCStruct TotalStatistics where
  peekCStruct p = do
    memoryType <- generateM (MAX_MEMORY_TYPES) (\i -> peekCStruct @DetailedStatistics (((lowerArrayPtr @DetailedStatistics ((p `plusPtr` 0 :: Ptr (FixedArray MAX_MEMORY_TYPES DetailedStatistics)))) `advancePtrBytes` (64 * (i)) :: Ptr DetailedStatistics)))
    memoryHeap <- generateM (MAX_MEMORY_HEAPS) (\i -> peekCStruct @DetailedStatistics (((lowerArrayPtr @DetailedStatistics ((p `plusPtr` 2048 :: Ptr (FixedArray MAX_MEMORY_HEAPS DetailedStatistics)))) `advancePtrBytes` (64 * (i)) :: Ptr DetailedStatistics)))
    total <- peekCStruct @DetailedStatistics ((p `plusPtr` 3072 :: Ptr DetailedStatistics))
    pure $ TotalStatistics
             memoryType memoryHeap total

instance Storable TotalStatistics where
  sizeOf ~_ = 3136
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero TotalStatistics where
  zero = TotalStatistics
           mempty
           mempty
           zero


-- | VmaBudget
--
-- Statistics of current memory usage and available budget for a specific
-- memory heap.
--
-- These are fast to calculate. See function 'getHeapBudgets'.
data Budget = Budget
  { -- | Statistics fetched from the library.
    statistics :: Statistics
  , -- | Estimated current memory usage of the program, in bytes.
    --
    -- Fetched from system using VK_EXT_memory_budget extension if enabled.
    --
    -- It might be different than @statistics.blockBytes@ (usually higher) due
    -- to additional implicit objects also occupying the memory, like
    -- swapchain, pipelines, descriptor heaps, command buffers, or
    -- @VkDeviceMemory@ blocks allocated outside of this library, if any.
    usage :: DeviceSize
  , -- | Estimated amount of memory available to the program, in bytes.
    --
    -- Fetched from system using VK_EXT_memory_budget extension if enabled.
    --
    -- It might be different (most probably smaller) than
    -- @VkMemoryHeap::size[heapIndex]@ due to factors external to the program,
    -- decided by the operating system. Difference @budget - usage@ is the
    -- amount of additional memory that can probably be allocated without
    -- problems. Exceeding the budget may result in various problems.
    budget :: DeviceSize
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (Budget)
#endif
deriving instance Show Budget

instance ToCStruct Budget where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p Budget{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Statistics)) (statistics)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (usage)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (budget)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Statistics)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct Budget where
  peekCStruct p = do
    statistics <- peekCStruct @Statistics ((p `plusPtr` 0 :: Ptr Statistics))
    usage <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    budget <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    pure $ Budget
             statistics usage budget

instance Storable Budget where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero Budget where
  zero = Budget
           zero
           zero
           zero


-- | VmaAllocationCreateInfo
--
-- Parameters of new 'Allocation'.
--
-- To be used with functions like 'createBuffer', 'createImage', and many
-- others.
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
  , -- | A floating-point value between 0 and 1, indicating the priority of the
    -- allocation relative to other memory allocations.
    --
    -- It is used only when 'ALLOCATOR_CREATE_EXT_MEMORY_PRIORITY_BIT' flag was
    -- used during creation of the 'Allocator' object and this allocation ends
    -- up as dedicated or is explicitly forced as dedicated using
    -- 'ALLOCATION_CREATE_DEDICATED_MEMORY_BIT'. Otherwise, it has the priority
    -- of a memory block where it is placed and this variable is ignored.
    priority :: Float
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AllocationCreateInfo)
#endif
deriving instance Show AllocationCreateInfo

instance ToCStruct AllocationCreateInfo where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AllocationCreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr AllocationCreateFlags)) (flags)
    poke ((p `plusPtr` 4 :: Ptr MemoryUsage)) (usage)
    poke ((p `plusPtr` 8 :: Ptr MemoryPropertyFlags)) (requiredFlags)
    poke ((p `plusPtr` 12 :: Ptr MemoryPropertyFlags)) (preferredFlags)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (memoryTypeBits)
    poke ((p `plusPtr` 24 :: Ptr Pool)) (pool)
    poke ((p `plusPtr` 32 :: Ptr (Ptr ()))) (userData)
    poke ((p `plusPtr` 40 :: Ptr CFloat)) (CFloat (priority))
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr AllocationCreateFlags)) (zero)
    poke ((p `plusPtr` 4 :: Ptr MemoryUsage)) (zero)
    poke ((p `plusPtr` 8 :: Ptr MemoryPropertyFlags)) (zero)
    poke ((p `plusPtr` 12 :: Ptr MemoryPropertyFlags)) (zero)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr CFloat)) (CFloat (zero))
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
    priority <- peek @CFloat ((p `plusPtr` 40 :: Ptr CFloat))
    pure $ AllocationCreateInfo
             flags
             usage
             requiredFlags
             preferredFlags
             memoryTypeBits
             pool
             pUserData
             (coerce @CFloat @Float priority)

instance Storable AllocationCreateInfo where
  sizeOf ~_ = 48
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
           zero


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
    -- automatically. Sizes of particular blocks may vary. In this case, the
    -- pool will also support dedicated allocations.
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
  , -- | A floating-point value between 0 and 1, indicating the priority of the
    -- allocations in this pool relative to other memory allocations.
    --
    -- It is used only when 'ALLOCATOR_CREATE_EXT_MEMORY_PRIORITY_BIT' flag was
    -- used during creation of the 'Allocator' object. Otherwise, this variable
    -- is ignored.
    priority :: Float
  , -- | Additional minimum alignment to be used for all allocations created from
    -- this pool. Can be 0.
    --
    -- Leave 0 (default) not to impose any additional alignment. If not 0, it
    -- must be a power of two. It can be useful in cases where alignment
    -- returned by Vulkan by functions like @vkGetBufferMemoryRequirements@ is
    -- not enough, e.g. when doing interop with OpenGL.
    minAllocationAlignment :: DeviceSize
  , -- | Additional @pNext@ chain to be attached to @VkMemoryAllocateInfo@ used
    -- for every allocation made by this pool. Optional.
    --
    -- Optional, can be null. If not null, it must point to a @pNext@ chain of
    -- structures that can be attached to @VkMemoryAllocateInfo@. It can be
    -- useful for special needs such as adding @VkExportMemoryAllocateInfoKHR@.
    -- Structures pointed by this member must remain alive and unchanged for
    -- the whole lifetime of the custom pool.
    --
    -- Please note that some structures, e.g.
    -- @VkMemoryPriorityAllocateInfoEXT@, @VkMemoryDedicatedAllocateInfoKHR@,
    -- can be attached automatically by this library when using other, more
    -- convenient of its features.
    memoryAllocateNext :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PoolCreateInfo)
#endif
deriving instance Show PoolCreateInfo

instance ToCStruct PoolCreateInfo where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PoolCreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (memoryTypeIndex)
    poke ((p `plusPtr` 4 :: Ptr PoolCreateFlags)) (flags)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (blockSize)
    poke ((p `plusPtr` 16 :: Ptr CSize)) (CSize (minBlockCount))
    poke ((p `plusPtr` 24 :: Ptr CSize)) (CSize (maxBlockCount))
    poke ((p `plusPtr` 32 :: Ptr CFloat)) (CFloat (priority))
    poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (minAllocationAlignment)
    poke ((p `plusPtr` 48 :: Ptr (Ptr ()))) (memoryAllocateNext)
    f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr PoolCreateFlags)) (zero)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 16 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 24 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 32 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct PoolCreateInfo where
  peekCStruct p = do
    memoryTypeIndex <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    flags <- peek @PoolCreateFlags ((p `plusPtr` 4 :: Ptr PoolCreateFlags))
    blockSize <- peek @DeviceSize ((p `plusPtr` 8 :: Ptr DeviceSize))
    minBlockCount <- peek @CSize ((p `plusPtr` 16 :: Ptr CSize))
    maxBlockCount <- peek @CSize ((p `plusPtr` 24 :: Ptr CSize))
    priority <- peek @CFloat ((p `plusPtr` 32 :: Ptr CFloat))
    minAllocationAlignment <- peek @DeviceSize ((p `plusPtr` 40 :: Ptr DeviceSize))
    pMemoryAllocateNext <- peek @(Ptr ()) ((p `plusPtr` 48 :: Ptr (Ptr ())))
    pure $ PoolCreateInfo
             memoryTypeIndex
             flags
             blockSize
             (coerce @CSize @Word64 minBlockCount)
             (coerce @CSize @Word64 maxBlockCount)
             (coerce @CFloat @Float priority)
             minAllocationAlignment
             pMemoryAllocateNext

instance Storable PoolCreateInfo where
  sizeOf ~_ = 56
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
           zero
           zero


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
    -- It can change after the allocation is moved during /Defragmentation/.
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
    -- It can change after the allocation is moved during /Defragmentation/.
    offset :: DeviceSize
  , -- | Size of this allocation, in bytes.
    --
    -- It never changes.
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
    -- change after the allocation is moved during /Defragmentation/.
    mappedData :: Ptr ()
  , -- | Custom general-purpose pointer that was passed as
    -- /VmaAllocationCreateInfo::pUserData/ or set using
    -- 'setAllocationUserData'.
    --
    -- It can change after call to 'setAllocationUserData' for this allocation.
    userData :: Ptr ()
  , -- | Custom allocation name that was set with 'setAllocationName'.
    --
    -- It can change after call to 'setAllocationName' for this allocation.
    --
    -- Another way to set custom name is to pass it in
    -- /VmaAllocationCreateInfo::pUserData/ with additional flag
    -- 'ALLOCATION_CREATE_USER_DATA_COPY_STRING_BIT' set [DEPRECATED].
    name :: Maybe ByteString
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AllocationInfo)
#endif
deriving instance Show AllocationInfo

instance ToCStruct AllocationInfo where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AllocationInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr Word32)) (memoryType)
    lift $ poke ((p `plusPtr` 8 :: Ptr DeviceMemory)) (deviceMemory)
    lift $ poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (offset)
    lift $ poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (size)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr ()))) (mappedData)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr ()))) (userData)
    pName'' <- case (name) of
      Nothing -> pure nullPtr
      Just j -> ContT $ useAsCString (j)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr CChar))) pName''
    lift $ f
  cStructSize = 56
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
    pName <- peek @(Ptr CChar) ((p `plusPtr` 48 :: Ptr (Ptr CChar)))
    pName' <- maybePeek (\j -> packCString (j)) pName
    pure $ AllocationInfo
             memoryType deviceMemory offset size pMappedData pUserData pName'

instance Zero AllocationInfo where
  zero = AllocationInfo
           zero
           zero
           zero
           zero
           zero
           zero
           Nothing


-- | VmaDefragmentationInfo
--
-- Parameters for defragmentation.
--
-- To be used with function 'beginDefragmentation'.
data DefragmentationInfo = DefragmentationInfo
  { -- | Use combination of 'DefragmentationFlagBits'.
    flags :: DefragmentationFlags
  , -- | Custom pool to be defragmented.
    --
    -- If null then default pools will undergo defragmentation process.
    pool :: Pool
  , -- | Maximum numbers of bytes that can be copied during single pass, while
    -- moving allocations to different places.
    --
    -- @0@ means no limit.
    maxBytesPerPass :: DeviceSize
  , -- | Maximum number of allocations that can be moved during single pass to a
    -- different place.
    --
    -- @0@ means no limit.
    maxAllocationsPerPass :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DefragmentationInfo)
#endif
deriving instance Show DefragmentationInfo

instance ToCStruct DefragmentationInfo where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DefragmentationInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DefragmentationFlags)) (flags)
    poke ((p `plusPtr` 8 :: Ptr Pool)) (pool)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (maxBytesPerPass)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (maxAllocationsPerPass)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DefragmentationFlags)) (zero)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    f

instance FromCStruct DefragmentationInfo where
  peekCStruct p = do
    flags <- peek @DefragmentationFlags ((p `plusPtr` 0 :: Ptr DefragmentationFlags))
    pool <- peek @Pool ((p `plusPtr` 8 :: Ptr Pool))
    maxBytesPerPass <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    maxAllocationsPerPass <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ DefragmentationInfo
             flags pool maxBytesPerPass maxAllocationsPerPass

instance Storable DefragmentationInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DefragmentationInfo where
  zero = DefragmentationInfo
           zero
           zero
           zero
           zero


-- | VmaDefragmentationMove
--
-- Single move of an allocation to be done for defragmentation.
data DefragmentationMove = DefragmentationMove
  { -- | Operation to be performed on the allocation by 'endDefragmentationPass'.
    -- Default value is 'DEFRAGMENTATION_MOVE_OPERATION_COPY'. You can modify
    -- it.
    operation :: DefragmentationMoveOperation
  , -- | Allocation that should be moved.
    srcAllocation :: Allocation
  , -- | Temporary allocation pointing to destination memory that will replace
    -- @srcAllocation@.
    --
    -- Warning
    --
    -- Do not store this allocation in your data structures! It exists only
    -- temporarily, for the duration of the defragmentation pass, to be used
    -- for binding new buffer\/image to the destination memory using e.g.
    -- 'bindBufferMemory'. 'endDefragmentationPass' will destroy it and make
    -- @srcAllocation@ point to this memory.
    dstTmpAllocation :: Allocation
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DefragmentationMove)
#endif
deriving instance Show DefragmentationMove

instance ToCStruct DefragmentationMove where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DefragmentationMove{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DefragmentationMoveOperation)) (operation)
    poke ((p `plusPtr` 8 :: Ptr Allocation)) (srcAllocation)
    poke ((p `plusPtr` 16 :: Ptr Allocation)) (dstTmpAllocation)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DefragmentationMoveOperation)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Allocation)) (zero)
    poke ((p `plusPtr` 16 :: Ptr Allocation)) (zero)
    f

instance FromCStruct DefragmentationMove where
  peekCStruct p = do
    operation <- peek @DefragmentationMoveOperation ((p `plusPtr` 0 :: Ptr DefragmentationMoveOperation))
    srcAllocation <- peek @Allocation ((p `plusPtr` 8 :: Ptr Allocation))
    dstTmpAllocation <- peek @Allocation ((p `plusPtr` 16 :: Ptr Allocation))
    pure $ DefragmentationMove
             operation srcAllocation dstTmpAllocation

instance Storable DefragmentationMove where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DefragmentationMove where
  zero = DefragmentationMove
           zero
           zero
           zero


-- | VmaDefragmentationPassMoveInfo
--
-- Parameters for incremental defragmentation steps.
--
-- To be used with function 'beginDefragmentationPass'.
data DefragmentationPassMoveInfo = DefragmentationPassMoveInfo
  { -- | Number of elements in the @pMoves@ array.
    moveCount :: Word32
  , -- | Array of moves to be performed by the user in the current
    -- defragmentation pass.
    --
    -- Pointer to an array of @moveCount@ elements, owned by VMA, created in
    -- 'beginDefragmentationPass', destroyed in 'endDefragmentationPass'.
    --
    -- For each element, you should:
    --
    -- 1.  Create a new buffer\/image in the place pointed by
    --     VmaDefragmentationMove::dstMemory +
    --     VmaDefragmentationMove::dstOffset.
    --
    -- 2.  Copy data from the /VmaDefragmentationMove::srcAllocation/ e.g.
    --     using @vkCmdCopyBuffer@, @vkCmdCopyImage@.
    --
    -- 3.  Make sure these commands finished executing on the GPU.
    --
    -- 4.  Destroy the old buffer\/image.
    --
    -- Only then you can finish defragmentation pass by calling
    -- 'endDefragmentationPass'. After this call, the allocation will point to
    -- the new place in memory.
    --
    -- Alternatively, if you cannot move specific allocation, you can set
    -- /VmaDefragmentationMove::operation/ to
    -- 'DEFRAGMENTATION_MOVE_OPERATION_IGNORE'.
    --
    -- Alternatively, if you decide you want to completely remove the
    -- allocation:
    --
    -- 1.  Destroy its buffer\/image.
    --
    -- 2.  Set /VmaDefragmentationMove::operation/ to
    --     'DEFRAGMENTATION_MOVE_OPERATION_DESTROY'.
    --
    -- Then, after 'endDefragmentationPass' the allocation will be freed.
    moves :: Ptr DefragmentationMove
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DefragmentationPassMoveInfo)
#endif
deriving instance Show DefragmentationPassMoveInfo

instance ToCStruct DefragmentationPassMoveInfo where
  withCStruct x f = allocaBytes 16 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DefragmentationPassMoveInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (moveCount)
    poke ((p `plusPtr` 8 :: Ptr (Ptr DefragmentationMove))) (moves)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    f

instance FromCStruct DefragmentationPassMoveInfo where
  peekCStruct p = do
    moveCount <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    pMoves <- peek @(Ptr DefragmentationMove) ((p `plusPtr` 8 :: Ptr (Ptr DefragmentationMove)))
    pure $ DefragmentationPassMoveInfo
             moveCount pMoves

instance Storable DefragmentationPassMoveInfo where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DefragmentationPassMoveInfo where
  zero = DefragmentationPassMoveInfo
           zero
           zero


-- | VmaDefragmentationStats
--
-- Statistics returned for defragmentation process in function
-- 'endDefragmentation'.
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
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
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


-- | VmaVirtualBlockCreateInfo
--
-- Parameters of created 'VirtualBlock' object to be passed to
-- 'createVirtualBlock'.
data VirtualBlockCreateInfo = VirtualBlockCreateInfo
  { -- | Total size of the virtual block.
    --
    -- Sizes can be expressed in bytes or any units you want as long as you are
    -- consistent in using them. For example, if you allocate from some array
    -- of structures, 1 can mean single instance of entire structure.
    size :: DeviceSize
  , -- | Use combination of 'VirtualBlockCreateFlagBits'.
    flags :: VirtualBlockCreateFlags
  , -- | Custom CPU memory allocation callbacks. Optional.
    --
    -- Optional, can be null. When specified, they will be used for all
    -- CPU-side memory allocations.
    allocationCallbacks :: Maybe AllocationCallbacks
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (VirtualBlockCreateInfo)
#endif
deriving instance Show VirtualBlockCreateInfo

instance ToCStruct VirtualBlockCreateInfo where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p VirtualBlockCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr DeviceSize)) (size)
    lift $ poke ((p `plusPtr` 8 :: Ptr VirtualBlockCreateFlags)) (flags)
    pAllocationCallbacks'' <- case (allocationCallbacks) of
      Nothing -> pure nullPtr
      Just j -> ContT $ withCStruct (j)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr AllocationCallbacks))) pAllocationCallbacks''
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 8 :: Ptr VirtualBlockCreateFlags)) (zero)
    f

instance FromCStruct VirtualBlockCreateInfo where
  peekCStruct p = do
    size <- peek @DeviceSize ((p `plusPtr` 0 :: Ptr DeviceSize))
    flags <- peek @VirtualBlockCreateFlags ((p `plusPtr` 8 :: Ptr VirtualBlockCreateFlags))
    pAllocationCallbacks <- peek @(Ptr AllocationCallbacks) ((p `plusPtr` 16 :: Ptr (Ptr AllocationCallbacks)))
    pAllocationCallbacks' <- maybePeek (\j -> peekCStruct @AllocationCallbacks (j)) pAllocationCallbacks
    pure $ VirtualBlockCreateInfo
             size flags pAllocationCallbacks'

instance Zero VirtualBlockCreateInfo where
  zero = VirtualBlockCreateInfo
           zero
           zero
           Nothing


-- | VmaVirtualAllocationCreateInfo
--
-- Parameters of created virtual allocation to be passed to
-- 'virtualAllocate'.
data VirtualAllocationCreateInfo = VirtualAllocationCreateInfo
  { -- | Size of the allocation.
    --
    -- Cannot be zero.
    size :: DeviceSize
  , -- | Required alignment of the allocation. Optional.
    --
    -- Must be power of two. Special value 0 has the same meaning as 1 - means
    -- no special alignment is required, so allocation can start at any offset.
    alignment :: DeviceSize
  , -- | Use combination of 'VirtualAllocationCreateFlagBits'.
    flags :: VirtualAllocationCreateFlags
  , -- | Custom pointer to be associated with the allocation. Optional.
    --
    -- It can be any value and can be used for user-defined purposes. It can be
    -- fetched or changed later.
    userData :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (VirtualAllocationCreateInfo)
#endif
deriving instance Show VirtualAllocationCreateInfo

instance ToCStruct VirtualAllocationCreateInfo where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p VirtualAllocationCreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceSize)) (size)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (alignment)
    poke ((p `plusPtr` 16 :: Ptr VirtualAllocationCreateFlags)) (flags)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (userData)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 16 :: Ptr VirtualAllocationCreateFlags)) (zero)
    f

instance FromCStruct VirtualAllocationCreateInfo where
  peekCStruct p = do
    size <- peek @DeviceSize ((p `plusPtr` 0 :: Ptr DeviceSize))
    alignment <- peek @DeviceSize ((p `plusPtr` 8 :: Ptr DeviceSize))
    flags <- peek @VirtualAllocationCreateFlags ((p `plusPtr` 16 :: Ptr VirtualAllocationCreateFlags))
    pUserData <- peek @(Ptr ()) ((p `plusPtr` 24 :: Ptr (Ptr ())))
    pure $ VirtualAllocationCreateInfo
             size alignment flags pUserData

instance Storable VirtualAllocationCreateInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero VirtualAllocationCreateInfo where
  zero = VirtualAllocationCreateInfo
           zero
           zero
           zero
           zero


-- | VmaVirtualAllocationInfo
--
-- Parameters of an existing virtual allocation, returned by
-- 'getVirtualAllocationInfo'.
data VirtualAllocationInfo = VirtualAllocationInfo
  { -- | Offset of the allocation.
    --
    -- Offset at which the allocation was made.
    offset :: DeviceSize
  , -- | Size of the allocation.
    --
    -- Same value as passed in /VmaVirtualAllocationCreateInfo::size/.
    size :: DeviceSize
  , -- | Custom pointer associated with the allocation.
    --
    -- Same value as passed in /VmaVirtualAllocationCreateInfo::pUserData/ or
    -- to 'setVirtualAllocationUserData'.
    userData :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (VirtualAllocationInfo)
#endif
deriving instance Show VirtualAllocationInfo

instance ToCStruct VirtualAllocationInfo where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p VirtualAllocationInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceSize)) (offset)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (size)
    poke ((p `plusPtr` 16 :: Ptr (Ptr ()))) (userData)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct VirtualAllocationInfo where
  peekCStruct p = do
    offset <- peek @DeviceSize ((p `plusPtr` 0 :: Ptr DeviceSize))
    size <- peek @DeviceSize ((p `plusPtr` 8 :: Ptr DeviceSize))
    pUserData <- peek @(Ptr ()) ((p `plusPtr` 16 :: Ptr (Ptr ())))
    pure $ VirtualAllocationInfo
             offset size pUserData

instance Storable VirtualAllocationInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero VirtualAllocationInfo where
  zero = VirtualAllocationInfo
           zero
           zero
           zero

