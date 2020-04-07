{-# language CPP #-}
module Graphics.VulkanMemoryAllocator  ( allocateMemoryPages
                                       , beginDefragmentationPass
                                       , bindBufferMemory
                                       , findMemoryTypeIndex
                                       , getAllocatorInfo
                                       , bindImageMemory
                                       , bindImageMemory2
                                       , setAllocationUserData
                                       , defragmentationBegin
                                       , allocateMemory
                                       , getMemoryTypeProperties
                                       , endDefragmentationPass
                                       , defragmentationEnd
                                       , freeMemoryPages
                                       , getAllocationInfo
                                       , freeStatsString
                                       , checkPoolCorruption
                                       , findMemoryTypeIndexForBufferInfo
                                       , getMemoryProperties
                                       , destroyAllocator
                                       , freeMemory
                                       , flushAllocation
                                       , setCurrentFrameIndex
                                       , bindBufferMemory2
                                       , checkCorruption
                                       , invalidateAllocation
                                       , createLostAllocation
                                       , buildStatsString
                                       , calculateStats
                                       , defragment
                                       , touchAllocation
                                       , getPoolName
                                       , createAllocator
                                       , createImage
                                       , findMemoryTypeIndexForImageInfo
                                       , destroyBuffer
                                       , createBuffer
                                       , makePoolAllocationsLost
                                       , setPoolName
                                       , destroyImage
                                       , destroyPool
                                       , createPool
                                       , getPoolStats
                                       , mapMemory
                                       , resizeAllocation
                                       , getBudget
                                       , unmapMemory
                                       , getPhysicalDeviceProperties
                                       , allocateMemoryForBuffer
                                       , allocateMemoryForImage
                                       , AllocatorCreateFlagBits( ALLOCATOR_CREATE_EXTERNALLY_SYNCHRONIZED_BIT
                                                                , ALLOCATOR_CREATE_KHR_DEDICATED_ALLOCATION_BIT
                                                                , ALLOCATOR_CREATE_KHR_BIND_MEMORY2_BIT
                                                                , ALLOCATOR_CREATE_EXT_MEMORY_BUDGET_BIT
                                                                , ALLOCATOR_CREATE_AMD_DEVICE_COHERENT_MEMORY_BIT
                                                                , ALLOCATOR_CREATE_BUFFER_DEVICE_ADDRESS_BIT
                                                                , ..
                                                                )
                                       , AllocatorCreateFlags
                                       , DefragmentationFlagBits( DEFRAGMENTATION_FLAG_INCREMENTAL
                                                                , ..
                                                                )
                                       , DefragmentationFlags
                                       , PoolCreateFlagBits( POOL_CREATE_IGNORE_BUFFER_IMAGE_GRANULARITY_BIT
                                                           , POOL_CREATE_LINEAR_ALGORITHM_BIT
                                                           , POOL_CREATE_BUDDY_ALGORITHM_BIT
                                                           , POOL_CREATE_ALGORITHM_MASK
                                                           , ..
                                                           )
                                       , PoolCreateFlags
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
                                       , AllocationCreateFlags
                                       , RecordFlagBits( RECORD_FLUSH_AFTER_CALL_BIT
                                                       , ..
                                                       )
                                       , RecordFlags
                                       , MemoryUsage( MEMORY_USAGE_UNKNOWN
                                                    , MEMORY_USAGE_GPU_ONLY
                                                    , MEMORY_USAGE_CPU_ONLY
                                                    , MEMORY_USAGE_CPU_TO_GPU
                                                    , MEMORY_USAGE_GPU_TO_CPU
                                                    , MEMORY_USAGE_CPU_COPY
                                                    , MEMORY_USAGE_GPU_LAZILY_ALLOCATED
                                                    , ..
                                                    )
                                       , AllocationCreateInfo(..)
                                       , DefragmentationPassInfo(..)
                                       , AllocatorCreateInfo(..)
                                       , DefragmentationInfo(..)
                                       , DefragmentationInfo2(..)
                                       , PoolCreateInfo(..)
                                       , DefragmentationStats(..)
                                       , AllocationInfo(..)
                                       , AllocatorInfo(..)
                                       , VulkanFunctions(..)
                                       , PoolStats(..)
                                       , StatInfo(..)
                                       , RecordSettings(..)
                                       , Budget(..)
                                       , DeviceMemoryCallbacks(..)
                                       , DefragmentationPassMoveInfo(..)
                                       , Stats(..)
                                       , Pool(..)
                                       , DefragmentationContext(..)
                                       , Allocation(..)
                                       , Allocator(..)
                                       , PFN_vmaFreeDeviceMemoryFunction
                                       , FN_vmaFreeDeviceMemoryFunction
                                       , PFN_vmaAllocateDeviceMemoryFunction
                                       , FN_vmaAllocateDeviceMemoryFunction
                                       ) where

import Graphics.Vulkan (AllocationCallbacks)
import Graphics.Vulkan (BindBufferMemoryInfo)
import Graphics.Vulkan (BindImageMemoryInfo)
import Graphics.Vulkan (Bool32)
import Graphics.Vulkan (Buffer)
import Graphics.Vulkan (BufferCopy)
import Graphics.Vulkan (BufferCreateInfo)
import Graphics.Vulkan (BufferMemoryRequirementsInfo2)
import Graphics.Vulkan (CommandBuffer_T)
import Graphics.Vulkan (DeviceMemory)
import Graphics.Vulkan (DeviceSize)
import Graphics.Vulkan (Device_T)
import Graphics.Vulkan (Flags)
import Graphics.Vulkan (Image)
import Graphics.Vulkan (ImageCreateInfo)
import Graphics.Vulkan (ImageMemoryRequirementsInfo2)
import Graphics.Vulkan (Instance_T)
import Graphics.Vulkan (MappedMemoryRange)
import Graphics.Vulkan (MemoryAllocateInfo)
import Graphics.Vulkan (MemoryMapFlags)
import Graphics.Vulkan (MemoryPropertyFlags)
import Graphics.Vulkan (MemoryRequirements)
import Graphics.Vulkan (MemoryRequirements2)
import Graphics.Vulkan (PhysicalDeviceMemoryProperties)
import Graphics.Vulkan (PhysicalDeviceMemoryProperties2)
import Graphics.Vulkan (PhysicalDeviceProperties)
import Graphics.Vulkan (PhysicalDevice_T)
import Graphics.Vulkan (Result)
import Graphics.Vulkan.CStruct.Utils (advancePtrBytes)
import Graphics.Vulkan.CStruct.Utils (lowerArrayPtr)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Utils (maybePeek)
import GHC.Base (when)
import GHC.IO (throwIO)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Numeric (showHex)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Data.ByteString (packCString)
import Data.ByteString (useAsCString)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Graphics.Vulkan.Core10.APIConstants (pattern MAX_MEMORY_HEAPS)
import Graphics.Vulkan.Core10.APIConstants (pattern MAX_MEMORY_TYPES)
import Graphics.Vulkan.Core10.Enums.Result (pattern SUCCESS)
import Foreign.C.Types (CChar(..))
import Foreign.C.Types (CSize(..))
import Graphics.Vulkan (Bool32(..))
import Graphics.Vulkan (Buffer(..))
import Graphics.Vulkan (Image(..))
import Graphics.Vulkan (MemoryPropertyFlagBits(..))
import Graphics.Vulkan (Result(..))
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct.Extends (SomeStruct)
import Graphics.Vulkan.Core10.APIConstants (IsHandle)
import Graphics.Vulkan.Core10.APIConstants (MAX_MEMORY_HEAPS)
import Graphics.Vulkan.Core10.APIConstants (MAX_MEMORY_TYPES)
import Graphics.Vulkan.Exception (VulkanException(..))
import Graphics.Vulkan.NamedType ((:::))
import Graphics.Vulkan.Zero (Zero)
import Graphics.Vulkan.Zero (Zero(..))
import Data.Bits (Bits)
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.C.Types (CSize)
import Foreign.C.Types (CSize(CSize))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import Data.Word (Word32)
import Data.Word (Word64)
import Text.Read.Lex (Lexeme(Ident))
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import qualified Data.Vector.Storable.Sized (Vector)

foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaAllocateMemoryPages" ffiVmaAllocateMemoryPages
  :: Allocator -> Ptr MemoryRequirements -> Ptr AllocationCreateInfo -> CSize -> Ptr Allocation -> Ptr AllocationInfo -> IO Result

-- No documentation found for TopLevel "vmaAllocateMemoryPages"
allocateMemoryPages :: Allocator -> ("vkMemoryRequirements" ::: MemoryRequirements) -> AllocationCreateInfo -> ("allocationCount" ::: Word64) -> IO (("allocations" ::: Vector Allocation), ("allocationInfo" ::: Vector AllocationInfo))
allocateMemoryPages allocator vkMemoryRequirements createInfo allocationCount = evalContT $ do
  pVkMemoryRequirements <- ContT $ withCStruct (vkMemoryRequirements)
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pPAllocations <- ContT $ bracket (callocBytes @Allocation ((fromIntegral (allocationCount)) * 8)) free
  pPAllocationInfo <- ContT $ bracket (callocBytes @AllocationInfo ((fromIntegral (allocationCount)) * 48)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPAllocationInfo `advancePtrBytes` (i * 48) :: Ptr AllocationInfo) . ($ ())) [0..(fromIntegral (allocationCount)) - 1]
  r <- lift $ (ffiVmaAllocateMemoryPages) (allocator) pVkMemoryRequirements pCreateInfo (CSize (allocationCount)) (pPAllocations) ((pPAllocationInfo))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pAllocations <- lift $ generateM (fromIntegral (allocationCount)) (\i -> peek @Allocation ((pPAllocations `advancePtrBytes` (8 * (i)) :: Ptr Allocation)))
  pAllocationInfo <- lift $ generateM (fromIntegral (allocationCount)) (\i -> peekCStruct @AllocationInfo (((pPAllocationInfo) `advancePtrBytes` (48 * (i)) :: Ptr AllocationInfo)))
  pure $ (pAllocations, pAllocationInfo)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaBeginDefragmentationPass" ffiVmaBeginDefragmentationPass
  :: Allocator -> DefragmentationContext -> Ptr DefragmentationPassInfo -> IO Result

-- No documentation found for TopLevel "vmaBeginDefragmentationPass"
beginDefragmentationPass :: Allocator -> DefragmentationContext -> IO (DefragmentationPassInfo)
beginDefragmentationPass allocator context = evalContT $ do
  pPInfo <- ContT (withZeroCStruct @DefragmentationPassInfo)
  r <- lift $ (ffiVmaBeginDefragmentationPass) (allocator) (context) (pPInfo)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pInfo <- lift $ peekCStruct @DefragmentationPassInfo pPInfo
  pure $ (pInfo)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaBindBufferMemory" ffiVmaBindBufferMemory
  :: Allocator -> Allocation -> Buffer -> IO Result

-- No documentation found for TopLevel "vmaBindBufferMemory"
bindBufferMemory :: Allocator -> Allocation -> Buffer -> IO ()
bindBufferMemory allocator allocation buffer = do
  r <- (ffiVmaBindBufferMemory) (allocator) (allocation) (buffer)
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaFindMemoryTypeIndex" ffiVmaFindMemoryTypeIndex
  :: Allocator -> Word32 -> Ptr AllocationCreateInfo -> Ptr Word32 -> IO Result

-- No documentation found for TopLevel "vmaFindMemoryTypeIndex"
findMemoryTypeIndex :: Allocator -> ("memoryTypeBits" ::: Word32) -> AllocationCreateInfo -> IO (("memoryTypeIndex" ::: Word32))
findMemoryTypeIndex allocator memoryTypeBits allocationCreateInfo = evalContT $ do
  pAllocationCreateInfo <- ContT $ withCStruct (allocationCreateInfo)
  pPMemoryTypeIndex <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ (ffiVmaFindMemoryTypeIndex) (allocator) (memoryTypeBits) pAllocationCreateInfo (pPMemoryTypeIndex)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pMemoryTypeIndex <- lift $ peek @Word32 pPMemoryTypeIndex
  pure $ (pMemoryTypeIndex)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaGetAllocatorInfo" ffiVmaGetAllocatorInfo
  :: Allocator -> Ptr AllocatorInfo -> IO ()

-- No documentation found for TopLevel "vmaGetAllocatorInfo"
getAllocatorInfo :: Allocator -> IO (AllocatorInfo)
getAllocatorInfo allocator = evalContT $ do
  pPAllocatorInfo <- ContT (withZeroCStruct @AllocatorInfo)
  lift $ (ffiVmaGetAllocatorInfo) (allocator) (pPAllocatorInfo)
  pAllocatorInfo <- lift $ peekCStruct @AllocatorInfo pPAllocatorInfo
  pure $ (pAllocatorInfo)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaBindImageMemory" ffiVmaBindImageMemory
  :: Allocator -> Allocation -> Image -> IO Result

-- No documentation found for TopLevel "vmaBindImageMemory"
bindImageMemory :: Allocator -> Allocation -> Image -> IO ()
bindImageMemory allocator allocation image = do
  r <- (ffiVmaBindImageMemory) (allocator) (allocation) (image)
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaBindImageMemory2" ffiVmaBindImageMemory2
  :: Allocator -> Allocation -> DeviceSize -> Image -> Ptr () -> IO Result

-- No documentation found for TopLevel "vmaBindImageMemory2"
bindImageMemory2 :: Allocator -> Allocation -> ("allocationLocalOffset" ::: DeviceSize) -> Image -> ("next" ::: Ptr ()) -> IO ()
bindImageMemory2 allocator allocation allocationLocalOffset image next = do
  r <- (ffiVmaBindImageMemory2) (allocator) (allocation) (allocationLocalOffset) (image) (next)
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaSetAllocationUserData" ffiVmaSetAllocationUserData
  :: Allocator -> Allocation -> Ptr () -> IO ()

-- No documentation found for TopLevel "vmaSetAllocationUserData"
setAllocationUserData :: Allocator -> Allocation -> ("userData" ::: Ptr ()) -> IO ()
setAllocationUserData allocator allocation userData = do
  (ffiVmaSetAllocationUserData) (allocator) (allocation) (userData)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaDefragmentationBegin" ffiVmaDefragmentationBegin
  :: Allocator -> Ptr DefragmentationInfo2 -> Ptr DefragmentationStats -> Ptr DefragmentationContext -> IO Result

-- No documentation found for TopLevel "vmaDefragmentationBegin"
defragmentationBegin :: Allocator -> DefragmentationInfo2 -> IO (Result, DefragmentationStats, DefragmentationContext)
defragmentationBegin allocator info = evalContT $ do
  pInfo <- ContT $ withCStruct (info)
  pPStats <- ContT (withZeroCStruct @DefragmentationStats)
  pPContext <- ContT $ bracket (callocBytes @DefragmentationContext 8) free
  r <- lift $ (ffiVmaDefragmentationBegin) (allocator) pInfo (pPStats) (pPContext)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pStats <- lift $ peekCStruct @DefragmentationStats pPStats
  pContext <- lift $ peek @DefragmentationContext pPContext
  pure $ (r, pStats, pContext)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaAllocateMemory" ffiVmaAllocateMemory
  :: Allocator -> Ptr MemoryRequirements -> Ptr AllocationCreateInfo -> Ptr Allocation -> Ptr AllocationInfo -> IO Result

-- No documentation found for TopLevel "vmaAllocateMemory"
allocateMemory :: Allocator -> ("vkMemoryRequirements" ::: MemoryRequirements) -> AllocationCreateInfo -> IO (Allocation, AllocationInfo)
allocateMemory allocator vkMemoryRequirements createInfo = evalContT $ do
  pVkMemoryRequirements <- ContT $ withCStruct (vkMemoryRequirements)
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pPAllocation <- ContT $ bracket (callocBytes @Allocation 8) free
  pPAllocationInfo <- ContT (withZeroCStruct @AllocationInfo)
  r <- lift $ (ffiVmaAllocateMemory) (allocator) pVkMemoryRequirements pCreateInfo (pPAllocation) (pPAllocationInfo)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pAllocation <- lift $ peek @Allocation pPAllocation
  pAllocationInfo <- lift $ peekCStruct @AllocationInfo pPAllocationInfo
  pure $ (pAllocation, pAllocationInfo)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaGetMemoryTypeProperties" ffiVmaGetMemoryTypeProperties
  :: Allocator -> Word32 -> Ptr MemoryPropertyFlags -> IO ()

-- No documentation found for TopLevel "vmaGetMemoryTypeProperties"
getMemoryTypeProperties :: Allocator -> ("memoryTypeIndex" ::: Word32) -> IO (MemoryPropertyFlags)
getMemoryTypeProperties allocator memoryTypeIndex = evalContT $ do
  pPFlags <- ContT $ bracket (callocBytes @MemoryPropertyFlags 4) free
  lift $ (ffiVmaGetMemoryTypeProperties) (allocator) (memoryTypeIndex) (pPFlags)
  pFlags <- lift $ peek @MemoryPropertyFlags pPFlags
  pure $ (pFlags)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaEndDefragmentationPass" ffiVmaEndDefragmentationPass
  :: Allocator -> DefragmentationContext -> IO Result

-- No documentation found for TopLevel "vmaEndDefragmentationPass"
endDefragmentationPass :: Allocator -> DefragmentationContext -> IO ()
endDefragmentationPass allocator context = do
  r <- (ffiVmaEndDefragmentationPass) (allocator) (context)
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaDefragmentationEnd" ffiVmaDefragmentationEnd
  :: Allocator -> DefragmentationContext -> IO Result

-- No documentation found for TopLevel "vmaDefragmentationEnd"
defragmentationEnd :: Allocator -> DefragmentationContext -> IO ()
defragmentationEnd allocator context = do
  r <- (ffiVmaDefragmentationEnd) (allocator) (context)
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaFreeMemoryPages" ffiVmaFreeMemoryPages
  :: Allocator -> CSize -> Ptr Allocation -> IO ()

-- No documentation found for TopLevel "vmaFreeMemoryPages"
freeMemoryPages :: Allocator -> ("allocations" ::: Vector Allocation) -> IO ()
freeMemoryPages allocator allocations = evalContT $ do
  pPAllocations <- ContT $ allocaBytesAligned @Allocation ((Data.Vector.length (allocations)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPAllocations `plusPtr` (8 * (i)) :: Ptr Allocation) (e)) (allocations)
  lift $ (ffiVmaFreeMemoryPages) (allocator) ((fromIntegral (Data.Vector.length $ (allocations)) :: CSize)) (pPAllocations)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaGetAllocationInfo" ffiVmaGetAllocationInfo
  :: Allocator -> Allocation -> Ptr AllocationInfo -> IO ()

-- No documentation found for TopLevel "vmaGetAllocationInfo"
getAllocationInfo :: Allocator -> Allocation -> IO (AllocationInfo)
getAllocationInfo allocator allocation = evalContT $ do
  pPAllocationInfo <- ContT (withZeroCStruct @AllocationInfo)
  lift $ (ffiVmaGetAllocationInfo) (allocator) (allocation) (pPAllocationInfo)
  pAllocationInfo <- lift $ peekCStruct @AllocationInfo pPAllocationInfo
  pure $ (pAllocationInfo)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaFreeStatsString" ffiVmaFreeStatsString
  :: Allocator -> Ptr CChar -> IO ()

-- No documentation found for TopLevel "vmaFreeStatsString"
freeStatsString :: Allocator -> ("statsString" ::: Ptr CChar) -> IO ()
freeStatsString allocator statsString = do
  (ffiVmaFreeStatsString) (allocator) (statsString)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaCheckPoolCorruption" ffiVmaCheckPoolCorruption
  :: Allocator -> Pool -> IO Result

-- No documentation found for TopLevel "vmaCheckPoolCorruption"
checkPoolCorruption :: Allocator -> Pool -> IO ()
checkPoolCorruption allocator pool = do
  r <- (ffiVmaCheckPoolCorruption) (allocator) (pool)
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaFindMemoryTypeIndexForBufferInfo" ffiVmaFindMemoryTypeIndexForBufferInfo
  :: Allocator -> Ptr (BufferCreateInfo a) -> Ptr AllocationCreateInfo -> Ptr Word32 -> IO Result

-- No documentation found for TopLevel "vmaFindMemoryTypeIndexForBufferInfo"
findMemoryTypeIndexForBufferInfo :: PokeChain a => Allocator -> BufferCreateInfo a -> AllocationCreateInfo -> IO (("memoryTypeIndex" ::: Word32))
findMemoryTypeIndexForBufferInfo allocator bufferCreateInfo allocationCreateInfo = evalContT $ do
  pBufferCreateInfo <- ContT $ withCStruct (bufferCreateInfo)
  pAllocationCreateInfo <- ContT $ withCStruct (allocationCreateInfo)
  pPMemoryTypeIndex <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ (ffiVmaFindMemoryTypeIndexForBufferInfo) (allocator) pBufferCreateInfo pAllocationCreateInfo (pPMemoryTypeIndex)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pMemoryTypeIndex <- lift $ peek @Word32 pPMemoryTypeIndex
  pure $ (pMemoryTypeIndex)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaGetMemoryProperties" ffiVmaGetMemoryProperties
  :: Allocator -> Ptr (Ptr PhysicalDeviceMemoryProperties) -> IO ()

-- No documentation found for TopLevel "vmaGetMemoryProperties"
getMemoryProperties :: Allocator -> IO (Ptr PhysicalDeviceMemoryProperties)
getMemoryProperties allocator = evalContT $ do
  pPpPhysicalDeviceMemoryProperties <- ContT $ bracket (callocBytes @(Ptr PhysicalDeviceMemoryProperties) 8) free
  lift $ (ffiVmaGetMemoryProperties) (allocator) (pPpPhysicalDeviceMemoryProperties)
  ppPhysicalDeviceMemoryProperties <- lift $ peek @(Ptr PhysicalDeviceMemoryProperties) pPpPhysicalDeviceMemoryProperties
  pure $ (ppPhysicalDeviceMemoryProperties)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaDestroyAllocator" ffiVmaDestroyAllocator
  :: Allocator -> IO ()

-- No documentation found for TopLevel "vmaDestroyAllocator"
destroyAllocator :: Allocator -> IO ()
destroyAllocator allocator = do
  (ffiVmaDestroyAllocator) (allocator)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaFreeMemory" ffiVmaFreeMemory
  :: Allocator -> Allocation -> IO ()

-- No documentation found for TopLevel "vmaFreeMemory"
freeMemory :: Allocator -> Allocation -> IO ()
freeMemory allocator allocation = do
  (ffiVmaFreeMemory) (allocator) (allocation)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaFlushAllocation" ffiVmaFlushAllocation
  :: Allocator -> Allocation -> DeviceSize -> DeviceSize -> IO ()

-- No documentation found for TopLevel "vmaFlushAllocation"
flushAllocation :: Allocator -> Allocation -> ("offset" ::: DeviceSize) -> DeviceSize -> IO ()
flushAllocation allocator allocation offset size = do
  (ffiVmaFlushAllocation) (allocator) (allocation) (offset) (size)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaSetCurrentFrameIndex" ffiVmaSetCurrentFrameIndex
  :: Allocator -> Word32 -> IO ()

-- No documentation found for TopLevel "vmaSetCurrentFrameIndex"
setCurrentFrameIndex :: Allocator -> ("frameIndex" ::: Word32) -> IO ()
setCurrentFrameIndex allocator frameIndex = do
  (ffiVmaSetCurrentFrameIndex) (allocator) (frameIndex)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaBindBufferMemory2" ffiVmaBindBufferMemory2
  :: Allocator -> Allocation -> DeviceSize -> Buffer -> Ptr () -> IO Result

-- No documentation found for TopLevel "vmaBindBufferMemory2"
bindBufferMemory2 :: Allocator -> Allocation -> ("allocationLocalOffset" ::: DeviceSize) -> Buffer -> ("next" ::: Ptr ()) -> IO ()
bindBufferMemory2 allocator allocation allocationLocalOffset buffer next = do
  r <- (ffiVmaBindBufferMemory2) (allocator) (allocation) (allocationLocalOffset) (buffer) (next)
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaCheckCorruption" ffiVmaCheckCorruption
  :: Allocator -> Word32 -> IO Result

-- No documentation found for TopLevel "vmaCheckCorruption"
checkCorruption :: Allocator -> ("memoryTypeBits" ::: Word32) -> IO ()
checkCorruption allocator memoryTypeBits = do
  r <- (ffiVmaCheckCorruption) (allocator) (memoryTypeBits)
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaInvalidateAllocation" ffiVmaInvalidateAllocation
  :: Allocator -> Allocation -> DeviceSize -> DeviceSize -> IO ()

-- No documentation found for TopLevel "vmaInvalidateAllocation"
invalidateAllocation :: Allocator -> Allocation -> ("offset" ::: DeviceSize) -> DeviceSize -> IO ()
invalidateAllocation allocator allocation offset size = do
  (ffiVmaInvalidateAllocation) (allocator) (allocation) (offset) (size)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaCreateLostAllocation" ffiVmaCreateLostAllocation
  :: Allocator -> Ptr Allocation -> IO ()

-- No documentation found for TopLevel "vmaCreateLostAllocation"
createLostAllocation :: Allocator -> IO (Allocation)
createLostAllocation allocator = evalContT $ do
  pPAllocation <- ContT $ bracket (callocBytes @Allocation 8) free
  lift $ (ffiVmaCreateLostAllocation) (allocator) (pPAllocation)
  pAllocation <- lift $ peek @Allocation pPAllocation
  pure $ (pAllocation)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaBuildStatsString" ffiVmaBuildStatsString
  :: Allocator -> Ptr (Ptr CChar) -> Bool32 -> IO ()

-- No documentation found for TopLevel "vmaBuildStatsString"
buildStatsString :: Allocator -> ("detailedMap" ::: Bool32) -> IO (("statsString" ::: Ptr CChar))
buildStatsString allocator detailedMap = evalContT $ do
  pPpStatsString <- ContT $ bracket (callocBytes @(Ptr CChar) 8) free
  lift $ (ffiVmaBuildStatsString) (allocator) (pPpStatsString) (detailedMap)
  ppStatsString <- lift $ peek @(Ptr CChar) pPpStatsString
  pure $ (ppStatsString)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaCalculateStats" ffiVmaCalculateStats
  :: Allocator -> Ptr Stats -> IO ()

-- No documentation found for TopLevel "vmaCalculateStats"
calculateStats :: Allocator -> IO (Stats)
calculateStats allocator = evalContT $ do
  pPStats <- ContT (withZeroCStruct @Stats)
  lift $ (ffiVmaCalculateStats) (allocator) (pPStats)
  pStats <- lift $ peekCStruct @Stats pPStats
  pure $ (pStats)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaDefragment" ffiVmaDefragment
  :: Allocator -> Allocation -> CSize -> Ptr Bool32 -> Ptr DefragmentationInfo -> Ptr DefragmentationStats -> IO Result

-- No documentation found for TopLevel "vmaDefragment"
defragment :: Allocator -> ("allocations" ::: Allocation) -> ("allocationCount" ::: Word64) -> ("defragmentationInfo" ::: Maybe DefragmentationInfo) -> IO (("allocationsChanged" ::: Vector Bool32), DefragmentationStats)
defragment allocator allocations allocationCount defragmentationInfo = evalContT $ do
  pPAllocationsChanged <- ContT $ bracket (callocBytes @Bool32 ((fromIntegral (allocationCount)) * 4)) free
  pDefragmentationInfo <- case (defragmentationInfo) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPDefragmentationStats <- ContT (withZeroCStruct @DefragmentationStats)
  r <- lift $ (ffiVmaDefragment) (allocator) (allocations) (CSize (allocationCount)) (pPAllocationsChanged) pDefragmentationInfo (pPDefragmentationStats)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pAllocationsChanged <- lift $ generateM (fromIntegral (allocationCount)) (\i -> peek @Bool32 ((pPAllocationsChanged `advancePtrBytes` (4 * (i)) :: Ptr Bool32)))
  pDefragmentationStats <- lift $ peekCStruct @DefragmentationStats pPDefragmentationStats
  pure $ (pAllocationsChanged, pDefragmentationStats)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaTouchAllocation" ffiVmaTouchAllocation
  :: Allocator -> Allocation -> IO Bool32

-- No documentation found for TopLevel "vmaTouchAllocation"
touchAllocation :: Allocator -> Allocation -> IO (Bool32)
touchAllocation allocator allocation = do
  r <- (ffiVmaTouchAllocation) (allocator) (allocation)
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaGetPoolName" ffiVmaGetPoolName
  :: Allocator -> Pool -> Ptr (Ptr CChar) -> IO ()

-- No documentation found for TopLevel "vmaGetPoolName"
getPoolName :: Allocator -> Pool -> IO (("name" ::: Ptr CChar))
getPoolName allocator pool = evalContT $ do
  pPpName <- ContT $ bracket (callocBytes @(Ptr CChar) 8) free
  lift $ (ffiVmaGetPoolName) (allocator) (pool) (pPpName)
  ppName <- lift $ peek @(Ptr CChar) pPpName
  pure $ (ppName)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaCreateAllocator" ffiVmaCreateAllocator
  :: Ptr AllocatorCreateInfo -> Ptr Allocator -> IO Result

-- No documentation found for TopLevel "vmaCreateAllocator"
createAllocator :: AllocatorCreateInfo -> IO (Allocator)
createAllocator createInfo = evalContT $ do
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pPAllocator <- ContT $ bracket (callocBytes @Allocator 8) free
  r <- lift $ (ffiVmaCreateAllocator) pCreateInfo (pPAllocator)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pAllocator <- lift $ peek @Allocator pPAllocator
  pure $ (pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaCreateImage" ffiVmaCreateImage
  :: Allocator -> Ptr (ImageCreateInfo a) -> Ptr AllocationCreateInfo -> Ptr Image -> Ptr Allocation -> Ptr AllocationInfo -> IO Result

-- No documentation found for TopLevel "vmaCreateImage"
createImage :: PokeChain a => Allocator -> ImageCreateInfo a -> AllocationCreateInfo -> IO (Image, Allocation, AllocationInfo)
createImage allocator imageCreateInfo allocationCreateInfo = evalContT $ do
  pImageCreateInfo <- ContT $ withCStruct (imageCreateInfo)
  pAllocationCreateInfo <- ContT $ withCStruct (allocationCreateInfo)
  pPImage <- ContT $ bracket (callocBytes @Image 8) free
  pPAllocation <- ContT $ bracket (callocBytes @Allocation 8) free
  pPAllocationInfo <- ContT (withZeroCStruct @AllocationInfo)
  r <- lift $ (ffiVmaCreateImage) (allocator) pImageCreateInfo pAllocationCreateInfo (pPImage) (pPAllocation) (pPAllocationInfo)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pImage <- lift $ peek @Image pPImage
  pAllocation <- lift $ peek @Allocation pPAllocation
  pAllocationInfo <- lift $ peekCStruct @AllocationInfo pPAllocationInfo
  pure $ (pImage, pAllocation, pAllocationInfo)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaFindMemoryTypeIndexForImageInfo" ffiVmaFindMemoryTypeIndexForImageInfo
  :: Allocator -> Ptr (ImageCreateInfo a) -> Ptr AllocationCreateInfo -> Ptr Word32 -> IO Result

-- No documentation found for TopLevel "vmaFindMemoryTypeIndexForImageInfo"
findMemoryTypeIndexForImageInfo :: PokeChain a => Allocator -> ImageCreateInfo a -> AllocationCreateInfo -> IO (("memoryTypeIndex" ::: Word32))
findMemoryTypeIndexForImageInfo allocator imageCreateInfo allocationCreateInfo = evalContT $ do
  pImageCreateInfo <- ContT $ withCStruct (imageCreateInfo)
  pAllocationCreateInfo <- ContT $ withCStruct (allocationCreateInfo)
  pPMemoryTypeIndex <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ (ffiVmaFindMemoryTypeIndexForImageInfo) (allocator) pImageCreateInfo pAllocationCreateInfo (pPMemoryTypeIndex)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pMemoryTypeIndex <- lift $ peek @Word32 pPMemoryTypeIndex
  pure $ (pMemoryTypeIndex)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaDestroyBuffer" ffiVmaDestroyBuffer
  :: Allocator -> Buffer -> Allocation -> IO ()

-- No documentation found for TopLevel "vmaDestroyBuffer"
destroyBuffer :: Allocator -> Buffer -> Allocation -> IO ()
destroyBuffer allocator buffer allocation = do
  (ffiVmaDestroyBuffer) (allocator) (buffer) (allocation)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaCreateBuffer" ffiVmaCreateBuffer
  :: Allocator -> Ptr (BufferCreateInfo a) -> Ptr AllocationCreateInfo -> Ptr Buffer -> Ptr Allocation -> Ptr AllocationInfo -> IO Result

-- No documentation found for TopLevel "vmaCreateBuffer"
createBuffer :: PokeChain a => Allocator -> BufferCreateInfo a -> AllocationCreateInfo -> IO (Buffer, Allocation, AllocationInfo)
createBuffer allocator bufferCreateInfo allocationCreateInfo = evalContT $ do
  pBufferCreateInfo <- ContT $ withCStruct (bufferCreateInfo)
  pAllocationCreateInfo <- ContT $ withCStruct (allocationCreateInfo)
  pPBuffer <- ContT $ bracket (callocBytes @Buffer 8) free
  pPAllocation <- ContT $ bracket (callocBytes @Allocation 8) free
  pPAllocationInfo <- ContT (withZeroCStruct @AllocationInfo)
  r <- lift $ (ffiVmaCreateBuffer) (allocator) pBufferCreateInfo pAllocationCreateInfo (pPBuffer) (pPAllocation) (pPAllocationInfo)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pBuffer <- lift $ peek @Buffer pPBuffer
  pAllocation <- lift $ peek @Allocation pPAllocation
  pAllocationInfo <- lift $ peekCStruct @AllocationInfo pPAllocationInfo
  pure $ (pBuffer, pAllocation, pAllocationInfo)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaMakePoolAllocationsLost" ffiVmaMakePoolAllocationsLost
  :: Allocator -> Pool -> Ptr CSize -> IO ()

-- No documentation found for TopLevel "vmaMakePoolAllocationsLost"
makePoolAllocationsLost :: Allocator -> Pool -> IO (("lostAllocationCount" ::: Word64))
makePoolAllocationsLost allocator pool = evalContT $ do
  pPLostAllocationCount <- ContT $ bracket (callocBytes @CSize 8) free
  lift $ (ffiVmaMakePoolAllocationsLost) (allocator) (pool) (pPLostAllocationCount)
  pLostAllocationCount <- lift $ peek @CSize pPLostAllocationCount
  pure $ (((\(CSize a) -> a) pLostAllocationCount))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaSetPoolName" ffiVmaSetPoolName
  :: Allocator -> Pool -> Ptr CChar -> IO ()

-- No documentation found for TopLevel "vmaSetPoolName"
setPoolName :: Allocator -> Pool -> ("name" ::: Maybe ByteString) -> IO ()
setPoolName allocator pool name = evalContT $ do
  pName <- case (name) of
    Nothing -> pure nullPtr
    Just j -> ContT $ useAsCString (j)
  lift $ (ffiVmaSetPoolName) (allocator) (pool) pName
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaDestroyImage" ffiVmaDestroyImage
  :: Allocator -> Image -> Allocation -> IO ()

-- No documentation found for TopLevel "vmaDestroyImage"
destroyImage :: Allocator -> Image -> Allocation -> IO ()
destroyImage allocator image allocation = do
  (ffiVmaDestroyImage) (allocator) (image) (allocation)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaDestroyPool" ffiVmaDestroyPool
  :: Allocator -> Pool -> IO ()

-- No documentation found for TopLevel "vmaDestroyPool"
destroyPool :: Allocator -> Pool -> IO ()
destroyPool allocator pool = do
  (ffiVmaDestroyPool) (allocator) (pool)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaCreatePool" ffiVmaCreatePool
  :: Allocator -> Ptr PoolCreateInfo -> Ptr Pool -> IO Result

-- No documentation found for TopLevel "vmaCreatePool"
createPool :: Allocator -> PoolCreateInfo -> IO (Pool)
createPool allocator createInfo = evalContT $ do
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pPPool <- ContT $ bracket (callocBytes @Pool 8) free
  r <- lift $ (ffiVmaCreatePool) (allocator) pCreateInfo (pPPool)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPool <- lift $ peek @Pool pPPool
  pure $ (pPool)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaGetPoolStats" ffiVmaGetPoolStats
  :: Allocator -> Pool -> Ptr PoolStats -> IO ()

-- No documentation found for TopLevel "vmaGetPoolStats"
getPoolStats :: Allocator -> Pool -> IO (PoolStats)
getPoolStats allocator pool = evalContT $ do
  pPPoolStats <- ContT (withZeroCStruct @PoolStats)
  lift $ (ffiVmaGetPoolStats) (allocator) (pool) (pPPoolStats)
  pPoolStats <- lift $ peekCStruct @PoolStats pPPoolStats
  pure $ (pPoolStats)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaMapMemory" ffiVmaMapMemory
  :: Allocator -> Allocation -> Ptr (Ptr ()) -> IO Result

-- No documentation found for TopLevel "vmaMapMemory"
mapMemory :: Allocator -> Allocation -> IO (("data" ::: Ptr ()))
mapMemory allocator allocation = evalContT $ do
  pPpData <- ContT $ bracket (callocBytes @(Ptr ()) 8) free
  r <- lift $ (ffiVmaMapMemory) (allocator) (allocation) (pPpData)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  ppData <- lift $ peek @(Ptr ()) pPpData
  pure $ (ppData)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaResizeAllocation" ffiVmaResizeAllocation
  :: Allocator -> Allocation -> DeviceSize -> IO Result

-- No documentation found for TopLevel "vmaResizeAllocation"
resizeAllocation :: Allocator -> Allocation -> ("newSize" ::: DeviceSize) -> IO ()
resizeAllocation allocator allocation newSize = do
  r <- (ffiVmaResizeAllocation) (allocator) (allocation) (newSize)
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaGetBudget" ffiVmaGetBudget
  :: Allocator -> Ptr Budget -> IO ()

-- No documentation found for TopLevel "vmaGetBudget"
getBudget :: Allocator -> IO (Budget)
getBudget allocator = evalContT $ do
  pPBudget <- ContT (withZeroCStruct @Budget)
  lift $ (ffiVmaGetBudget) (allocator) (pPBudget)
  pBudget <- lift $ peekCStruct @Budget pPBudget
  pure $ (pBudget)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaUnmapMemory" ffiVmaUnmapMemory
  :: Allocator -> Allocation -> IO ()

-- No documentation found for TopLevel "vmaUnmapMemory"
unmapMemory :: Allocator -> Allocation -> IO ()
unmapMemory allocator allocation = do
  (ffiVmaUnmapMemory) (allocator) (allocation)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaGetPhysicalDeviceProperties" ffiVmaGetPhysicalDeviceProperties
  :: Allocator -> Ptr (Ptr PhysicalDeviceProperties) -> IO ()

-- No documentation found for TopLevel "vmaGetPhysicalDeviceProperties"
getPhysicalDeviceProperties :: Allocator -> IO (Ptr PhysicalDeviceProperties)
getPhysicalDeviceProperties allocator = evalContT $ do
  pPpPhysicalDeviceProperties <- ContT $ bracket (callocBytes @(Ptr PhysicalDeviceProperties) 8) free
  lift $ (ffiVmaGetPhysicalDeviceProperties) (allocator) (pPpPhysicalDeviceProperties)
  ppPhysicalDeviceProperties <- lift $ peek @(Ptr PhysicalDeviceProperties) pPpPhysicalDeviceProperties
  pure $ (ppPhysicalDeviceProperties)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaAllocateMemoryForBuffer" ffiVmaAllocateMemoryForBuffer
  :: Allocator -> Buffer -> Ptr AllocationCreateInfo -> Ptr Allocation -> Ptr AllocationInfo -> IO Result

-- No documentation found for TopLevel "vmaAllocateMemoryForBuffer"
allocateMemoryForBuffer :: Allocator -> Buffer -> AllocationCreateInfo -> IO (Allocation, AllocationInfo)
allocateMemoryForBuffer allocator buffer createInfo = evalContT $ do
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pPAllocation <- ContT $ bracket (callocBytes @Allocation 8) free
  pPAllocationInfo <- ContT (withZeroCStruct @AllocationInfo)
  r <- lift $ (ffiVmaAllocateMemoryForBuffer) (allocator) (buffer) pCreateInfo (pPAllocation) (pPAllocationInfo)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pAllocation <- lift $ peek @Allocation pPAllocation
  pAllocationInfo <- lift $ peekCStruct @AllocationInfo pPAllocationInfo
  pure $ (pAllocation, pAllocationInfo)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vmaAllocateMemoryForImage" ffiVmaAllocateMemoryForImage
  :: Allocator -> Image -> Ptr AllocationCreateInfo -> Ptr Allocation -> Ptr AllocationInfo -> IO Result

-- No documentation found for TopLevel "vmaAllocateMemoryForImage"
allocateMemoryForImage :: Allocator -> Image -> AllocationCreateInfo -> IO (Allocation, AllocationInfo)
allocateMemoryForImage allocator image createInfo = evalContT $ do
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pPAllocation <- ContT $ bracket (callocBytes @Allocation 8) free
  pPAllocationInfo <- ContT (withZeroCStruct @AllocationInfo)
  r <- lift $ (ffiVmaAllocateMemoryForImage) (allocator) (image) pCreateInfo (pPAllocation) (pPAllocationInfo)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pAllocation <- lift $ peek @Allocation pPAllocation
  pAllocationInfo <- lift $ peekCStruct @AllocationInfo pPAllocationInfo
  pure $ (pAllocation, pAllocationInfo)


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


-- No documentation found for TopLevel "VmaAllocatorCreateFlagBits"
newtype AllocatorCreateFlagBits = AllocatorCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- No documentation found for Nested "VmaAllocatorCreateFlagBits" "VMA_ALLOCATOR_CREATE_EXTERNALLY_SYNCHRONIZED_BIT"
pattern ALLOCATOR_CREATE_EXTERNALLY_SYNCHRONIZED_BIT = AllocatorCreateFlagBits 0x00000001
-- No documentation found for Nested "VmaAllocatorCreateFlagBits" "VMA_ALLOCATOR_CREATE_KHR_DEDICATED_ALLOCATION_BIT"
pattern ALLOCATOR_CREATE_KHR_DEDICATED_ALLOCATION_BIT = AllocatorCreateFlagBits 0x00000002
-- No documentation found for Nested "VmaAllocatorCreateFlagBits" "VMA_ALLOCATOR_CREATE_KHR_BIND_MEMORY2_BIT"
pattern ALLOCATOR_CREATE_KHR_BIND_MEMORY2_BIT = AllocatorCreateFlagBits 0x00000004
-- No documentation found for Nested "VmaAllocatorCreateFlagBits" "VMA_ALLOCATOR_CREATE_EXT_MEMORY_BUDGET_BIT"
pattern ALLOCATOR_CREATE_EXT_MEMORY_BUDGET_BIT = AllocatorCreateFlagBits 0x00000008
-- No documentation found for Nested "VmaAllocatorCreateFlagBits" "VMA_ALLOCATOR_CREATE_AMD_DEVICE_COHERENT_MEMORY_BIT"
pattern ALLOCATOR_CREATE_AMD_DEVICE_COHERENT_MEMORY_BIT = AllocatorCreateFlagBits 0x00000010
-- No documentation found for Nested "VmaAllocatorCreateFlagBits" "VMA_ALLOCATOR_CREATE_BUFFER_DEVICE_ADDRESS_BIT"
pattern ALLOCATOR_CREATE_BUFFER_DEVICE_ADDRESS_BIT = AllocatorCreateFlagBits 0x00000020

type AllocatorCreateFlags = AllocatorCreateFlagBits

instance Show AllocatorCreateFlagBits where
  showsPrec p = \case
    ALLOCATOR_CREATE_EXTERNALLY_SYNCHRONIZED_BIT -> showString "ALLOCATOR_CREATE_EXTERNALLY_SYNCHRONIZED_BIT"
    ALLOCATOR_CREATE_KHR_DEDICATED_ALLOCATION_BIT -> showString "ALLOCATOR_CREATE_KHR_DEDICATED_ALLOCATION_BIT"
    ALLOCATOR_CREATE_KHR_BIND_MEMORY2_BIT -> showString "ALLOCATOR_CREATE_KHR_BIND_MEMORY2_BIT"
    ALLOCATOR_CREATE_EXT_MEMORY_BUDGET_BIT -> showString "ALLOCATOR_CREATE_EXT_MEMORY_BUDGET_BIT"
    ALLOCATOR_CREATE_AMD_DEVICE_COHERENT_MEMORY_BIT -> showString "ALLOCATOR_CREATE_AMD_DEVICE_COHERENT_MEMORY_BIT"
    ALLOCATOR_CREATE_BUFFER_DEVICE_ADDRESS_BIT -> showString "ALLOCATOR_CREATE_BUFFER_DEVICE_ADDRESS_BIT"
    AllocatorCreateFlagBits x -> showParen (p >= 11) (showString "AllocatorCreateFlagBits 0x" . showHex x)

instance Read AllocatorCreateFlagBits where
  readPrec = parens (choose [("ALLOCATOR_CREATE_EXTERNALLY_SYNCHRONIZED_BIT", pure ALLOCATOR_CREATE_EXTERNALLY_SYNCHRONIZED_BIT)
                            , ("ALLOCATOR_CREATE_KHR_DEDICATED_ALLOCATION_BIT", pure ALLOCATOR_CREATE_KHR_DEDICATED_ALLOCATION_BIT)
                            , ("ALLOCATOR_CREATE_KHR_BIND_MEMORY2_BIT", pure ALLOCATOR_CREATE_KHR_BIND_MEMORY2_BIT)
                            , ("ALLOCATOR_CREATE_EXT_MEMORY_BUDGET_BIT", pure ALLOCATOR_CREATE_EXT_MEMORY_BUDGET_BIT)
                            , ("ALLOCATOR_CREATE_AMD_DEVICE_COHERENT_MEMORY_BIT", pure ALLOCATOR_CREATE_AMD_DEVICE_COHERENT_MEMORY_BIT)
                            , ("ALLOCATOR_CREATE_BUFFER_DEVICE_ADDRESS_BIT", pure ALLOCATOR_CREATE_BUFFER_DEVICE_ADDRESS_BIT)]
                     +++
                     prec 10 (do
                       expectP (Ident "AllocatorCreateFlagBits")
                       v <- step readPrec
                       pure (AllocatorCreateFlagBits v)))


-- No documentation found for TopLevel "VmaDefragmentationFlagBits"
newtype DefragmentationFlagBits = DefragmentationFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- No documentation found for Nested "VmaDefragmentationFlagBits" "VMA_DEFRAGMENTATION_FLAG_INCREMENTAL"
pattern DEFRAGMENTATION_FLAG_INCREMENTAL = DefragmentationFlagBits 0x00000001

type DefragmentationFlags = DefragmentationFlagBits

instance Show DefragmentationFlagBits where
  showsPrec p = \case
    DEFRAGMENTATION_FLAG_INCREMENTAL -> showString "DEFRAGMENTATION_FLAG_INCREMENTAL"
    DefragmentationFlagBits x -> showParen (p >= 11) (showString "DefragmentationFlagBits 0x" . showHex x)

instance Read DefragmentationFlagBits where
  readPrec = parens (choose [("DEFRAGMENTATION_FLAG_INCREMENTAL", pure DEFRAGMENTATION_FLAG_INCREMENTAL)]
                     +++
                     prec 10 (do
                       expectP (Ident "DefragmentationFlagBits")
                       v <- step readPrec
                       pure (DefragmentationFlagBits v)))


-- No documentation found for TopLevel "VmaPoolCreateFlagBits"
newtype PoolCreateFlagBits = PoolCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- No documentation found for Nested "VmaPoolCreateFlagBits" "VMA_POOL_CREATE_IGNORE_BUFFER_IMAGE_GRANULARITY_BIT"
pattern POOL_CREATE_IGNORE_BUFFER_IMAGE_GRANULARITY_BIT = PoolCreateFlagBits 0x00000002
-- No documentation found for Nested "VmaPoolCreateFlagBits" "VMA_POOL_CREATE_LINEAR_ALGORITHM_BIT"
pattern POOL_CREATE_LINEAR_ALGORITHM_BIT = PoolCreateFlagBits 0x00000004
-- No documentation found for Nested "VmaPoolCreateFlagBits" "VMA_POOL_CREATE_BUDDY_ALGORITHM_BIT"
pattern POOL_CREATE_BUDDY_ALGORITHM_BIT = PoolCreateFlagBits 0x00000008
-- No documentation found for Nested "VmaPoolCreateFlagBits" "VMA_POOL_CREATE_ALGORITHM_MASK"
pattern POOL_CREATE_ALGORITHM_MASK = PoolCreateFlagBits 0x0000000c

type PoolCreateFlags = PoolCreateFlagBits

instance Show PoolCreateFlagBits where
  showsPrec p = \case
    POOL_CREATE_IGNORE_BUFFER_IMAGE_GRANULARITY_BIT -> showString "POOL_CREATE_IGNORE_BUFFER_IMAGE_GRANULARITY_BIT"
    POOL_CREATE_LINEAR_ALGORITHM_BIT -> showString "POOL_CREATE_LINEAR_ALGORITHM_BIT"
    POOL_CREATE_BUDDY_ALGORITHM_BIT -> showString "POOL_CREATE_BUDDY_ALGORITHM_BIT"
    POOL_CREATE_ALGORITHM_MASK -> showString "POOL_CREATE_ALGORITHM_MASK"
    PoolCreateFlagBits x -> showParen (p >= 11) (showString "PoolCreateFlagBits 0x" . showHex x)

instance Read PoolCreateFlagBits where
  readPrec = parens (choose [("POOL_CREATE_IGNORE_BUFFER_IMAGE_GRANULARITY_BIT", pure POOL_CREATE_IGNORE_BUFFER_IMAGE_GRANULARITY_BIT)
                            , ("POOL_CREATE_LINEAR_ALGORITHM_BIT", pure POOL_CREATE_LINEAR_ALGORITHM_BIT)
                            , ("POOL_CREATE_BUDDY_ALGORITHM_BIT", pure POOL_CREATE_BUDDY_ALGORITHM_BIT)
                            , ("POOL_CREATE_ALGORITHM_MASK", pure POOL_CREATE_ALGORITHM_MASK)]
                     +++
                     prec 10 (do
                       expectP (Ident "PoolCreateFlagBits")
                       v <- step readPrec
                       pure (PoolCreateFlagBits v)))


-- No documentation found for TopLevel "VmaAllocationCreateFlagBits"
newtype AllocationCreateFlagBits = AllocationCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- No documentation found for Nested "VmaAllocationCreateFlagBits" "VMA_ALLOCATION_CREATE_DEDICATED_MEMORY_BIT"
pattern ALLOCATION_CREATE_DEDICATED_MEMORY_BIT = AllocationCreateFlagBits 0x00000001
-- No documentation found for Nested "VmaAllocationCreateFlagBits" "VMA_ALLOCATION_CREATE_NEVER_ALLOCATE_BIT"
pattern ALLOCATION_CREATE_NEVER_ALLOCATE_BIT = AllocationCreateFlagBits 0x00000002
-- No documentation found for Nested "VmaAllocationCreateFlagBits" "VMA_ALLOCATION_CREATE_MAPPED_BIT"
pattern ALLOCATION_CREATE_MAPPED_BIT = AllocationCreateFlagBits 0x00000004
-- No documentation found for Nested "VmaAllocationCreateFlagBits" "VMA_ALLOCATION_CREATE_CAN_BECOME_LOST_BIT"
pattern ALLOCATION_CREATE_CAN_BECOME_LOST_BIT = AllocationCreateFlagBits 0x00000008
-- No documentation found for Nested "VmaAllocationCreateFlagBits" "VMA_ALLOCATION_CREATE_CAN_MAKE_OTHER_LOST_BIT"
pattern ALLOCATION_CREATE_CAN_MAKE_OTHER_LOST_BIT = AllocationCreateFlagBits 0x00000010
-- No documentation found for Nested "VmaAllocationCreateFlagBits" "VMA_ALLOCATION_CREATE_USER_DATA_COPY_STRING_BIT"
pattern ALLOCATION_CREATE_USER_DATA_COPY_STRING_BIT = AllocationCreateFlagBits 0x00000020
-- No documentation found for Nested "VmaAllocationCreateFlagBits" "VMA_ALLOCATION_CREATE_UPPER_ADDRESS_BIT"
pattern ALLOCATION_CREATE_UPPER_ADDRESS_BIT = AllocationCreateFlagBits 0x00000040
-- No documentation found for Nested "VmaAllocationCreateFlagBits" "VMA_ALLOCATION_CREATE_DONT_BIND_BIT"
pattern ALLOCATION_CREATE_DONT_BIND_BIT = AllocationCreateFlagBits 0x00000080
-- No documentation found for Nested "VmaAllocationCreateFlagBits" "VMA_ALLOCATION_CREATE_WITHIN_BUDGET_BIT"
pattern ALLOCATION_CREATE_WITHIN_BUDGET_BIT = AllocationCreateFlagBits 0x00000100
-- No documentation found for Nested "VmaAllocationCreateFlagBits" "VMA_ALLOCATION_CREATE_STRATEGY_BEST_FIT_BIT"
pattern ALLOCATION_CREATE_STRATEGY_BEST_FIT_BIT = AllocationCreateFlagBits 0x00010000
-- No documentation found for Nested "VmaAllocationCreateFlagBits" "VMA_ALLOCATION_CREATE_STRATEGY_WORST_FIT_BIT"
pattern ALLOCATION_CREATE_STRATEGY_WORST_FIT_BIT = AllocationCreateFlagBits 0x00020000
-- No documentation found for Nested "VmaAllocationCreateFlagBits" "VMA_ALLOCATION_CREATE_STRATEGY_FIRST_FIT_BIT"
pattern ALLOCATION_CREATE_STRATEGY_FIRST_FIT_BIT = AllocationCreateFlagBits 0x00040000
-- No documentation found for Nested "VmaAllocationCreateFlagBits" "VMA_ALLOCATION_CREATE_STRATEGY_MIN_MEMORY_BIT"
pattern ALLOCATION_CREATE_STRATEGY_MIN_MEMORY_BIT = AllocationCreateFlagBits 0x00010000
-- No documentation found for Nested "VmaAllocationCreateFlagBits" "VMA_ALLOCATION_CREATE_STRATEGY_MIN_TIME_BIT"
pattern ALLOCATION_CREATE_STRATEGY_MIN_TIME_BIT = AllocationCreateFlagBits 0x00040000
-- No documentation found for Nested "VmaAllocationCreateFlagBits" "VMA_ALLOCATION_CREATE_STRATEGY_MIN_FRAGMENTATION_BIT"
pattern ALLOCATION_CREATE_STRATEGY_MIN_FRAGMENTATION_BIT = AllocationCreateFlagBits 0x00020000
-- No documentation found for Nested "VmaAllocationCreateFlagBits" "VMA_ALLOCATION_CREATE_STRATEGY_MASK"
pattern ALLOCATION_CREATE_STRATEGY_MASK = AllocationCreateFlagBits 0x00070000

type AllocationCreateFlags = AllocationCreateFlagBits

instance Show AllocationCreateFlagBits where
  showsPrec p = \case
    ALLOCATION_CREATE_DEDICATED_MEMORY_BIT -> showString "ALLOCATION_CREATE_DEDICATED_MEMORY_BIT"
    ALLOCATION_CREATE_NEVER_ALLOCATE_BIT -> showString "ALLOCATION_CREATE_NEVER_ALLOCATE_BIT"
    ALLOCATION_CREATE_MAPPED_BIT -> showString "ALLOCATION_CREATE_MAPPED_BIT"
    ALLOCATION_CREATE_CAN_BECOME_LOST_BIT -> showString "ALLOCATION_CREATE_CAN_BECOME_LOST_BIT"
    ALLOCATION_CREATE_CAN_MAKE_OTHER_LOST_BIT -> showString "ALLOCATION_CREATE_CAN_MAKE_OTHER_LOST_BIT"
    ALLOCATION_CREATE_USER_DATA_COPY_STRING_BIT -> showString "ALLOCATION_CREATE_USER_DATA_COPY_STRING_BIT"
    ALLOCATION_CREATE_UPPER_ADDRESS_BIT -> showString "ALLOCATION_CREATE_UPPER_ADDRESS_BIT"
    ALLOCATION_CREATE_DONT_BIND_BIT -> showString "ALLOCATION_CREATE_DONT_BIND_BIT"
    ALLOCATION_CREATE_WITHIN_BUDGET_BIT -> showString "ALLOCATION_CREATE_WITHIN_BUDGET_BIT"
    ALLOCATION_CREATE_STRATEGY_BEST_FIT_BIT -> showString "ALLOCATION_CREATE_STRATEGY_BEST_FIT_BIT"
    ALLOCATION_CREATE_STRATEGY_WORST_FIT_BIT -> showString "ALLOCATION_CREATE_STRATEGY_WORST_FIT_BIT"
    ALLOCATION_CREATE_STRATEGY_FIRST_FIT_BIT -> showString "ALLOCATION_CREATE_STRATEGY_FIRST_FIT_BIT"
    ALLOCATION_CREATE_STRATEGY_MIN_MEMORY_BIT -> showString "ALLOCATION_CREATE_STRATEGY_MIN_MEMORY_BIT"
    ALLOCATION_CREATE_STRATEGY_MIN_TIME_BIT -> showString "ALLOCATION_CREATE_STRATEGY_MIN_TIME_BIT"
    ALLOCATION_CREATE_STRATEGY_MIN_FRAGMENTATION_BIT -> showString "ALLOCATION_CREATE_STRATEGY_MIN_FRAGMENTATION_BIT"
    ALLOCATION_CREATE_STRATEGY_MASK -> showString "ALLOCATION_CREATE_STRATEGY_MASK"
    AllocationCreateFlagBits x -> showParen (p >= 11) (showString "AllocationCreateFlagBits 0x" . showHex x)

instance Read AllocationCreateFlagBits where
  readPrec = parens (choose [("ALLOCATION_CREATE_DEDICATED_MEMORY_BIT", pure ALLOCATION_CREATE_DEDICATED_MEMORY_BIT)
                            , ("ALLOCATION_CREATE_NEVER_ALLOCATE_BIT", pure ALLOCATION_CREATE_NEVER_ALLOCATE_BIT)
                            , ("ALLOCATION_CREATE_MAPPED_BIT", pure ALLOCATION_CREATE_MAPPED_BIT)
                            , ("ALLOCATION_CREATE_CAN_BECOME_LOST_BIT", pure ALLOCATION_CREATE_CAN_BECOME_LOST_BIT)
                            , ("ALLOCATION_CREATE_CAN_MAKE_OTHER_LOST_BIT", pure ALLOCATION_CREATE_CAN_MAKE_OTHER_LOST_BIT)
                            , ("ALLOCATION_CREATE_USER_DATA_COPY_STRING_BIT", pure ALLOCATION_CREATE_USER_DATA_COPY_STRING_BIT)
                            , ("ALLOCATION_CREATE_UPPER_ADDRESS_BIT", pure ALLOCATION_CREATE_UPPER_ADDRESS_BIT)
                            , ("ALLOCATION_CREATE_DONT_BIND_BIT", pure ALLOCATION_CREATE_DONT_BIND_BIT)
                            , ("ALLOCATION_CREATE_WITHIN_BUDGET_BIT", pure ALLOCATION_CREATE_WITHIN_BUDGET_BIT)
                            , ("ALLOCATION_CREATE_STRATEGY_BEST_FIT_BIT", pure ALLOCATION_CREATE_STRATEGY_BEST_FIT_BIT)
                            , ("ALLOCATION_CREATE_STRATEGY_WORST_FIT_BIT", pure ALLOCATION_CREATE_STRATEGY_WORST_FIT_BIT)
                            , ("ALLOCATION_CREATE_STRATEGY_FIRST_FIT_BIT", pure ALLOCATION_CREATE_STRATEGY_FIRST_FIT_BIT)
                            , ("ALLOCATION_CREATE_STRATEGY_MIN_MEMORY_BIT", pure ALLOCATION_CREATE_STRATEGY_MIN_MEMORY_BIT)
                            , ("ALLOCATION_CREATE_STRATEGY_MIN_TIME_BIT", pure ALLOCATION_CREATE_STRATEGY_MIN_TIME_BIT)
                            , ("ALLOCATION_CREATE_STRATEGY_MIN_FRAGMENTATION_BIT", pure ALLOCATION_CREATE_STRATEGY_MIN_FRAGMENTATION_BIT)
                            , ("ALLOCATION_CREATE_STRATEGY_MASK", pure ALLOCATION_CREATE_STRATEGY_MASK)]
                     +++
                     prec 10 (do
                       expectP (Ident "AllocationCreateFlagBits")
                       v <- step readPrec
                       pure (AllocationCreateFlagBits v)))


-- No documentation found for TopLevel "VmaRecordFlagBits"
newtype RecordFlagBits = RecordFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- No documentation found for Nested "VmaRecordFlagBits" "VMA_RECORD_FLUSH_AFTER_CALL_BIT"
pattern RECORD_FLUSH_AFTER_CALL_BIT = RecordFlagBits 0x00000001

type RecordFlags = RecordFlagBits

instance Show RecordFlagBits where
  showsPrec p = \case
    RECORD_FLUSH_AFTER_CALL_BIT -> showString "RECORD_FLUSH_AFTER_CALL_BIT"
    RecordFlagBits x -> showParen (p >= 11) (showString "RecordFlagBits 0x" . showHex x)

instance Read RecordFlagBits where
  readPrec = parens (choose [("RECORD_FLUSH_AFTER_CALL_BIT", pure RECORD_FLUSH_AFTER_CALL_BIT)]
                     +++
                     prec 10 (do
                       expectP (Ident "RecordFlagBits")
                       v <- step readPrec
                       pure (RecordFlagBits v)))


-- No documentation found for TopLevel "VmaMemoryUsage"
newtype MemoryUsage = MemoryUsage Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VmaMemoryUsage" "VMA_MEMORY_USAGE_UNKNOWN"
pattern MEMORY_USAGE_UNKNOWN = MemoryUsage 0
-- No documentation found for Nested "VmaMemoryUsage" "VMA_MEMORY_USAGE_GPU_ONLY"
pattern MEMORY_USAGE_GPU_ONLY = MemoryUsage 1
-- No documentation found for Nested "VmaMemoryUsage" "VMA_MEMORY_USAGE_CPU_ONLY"
pattern MEMORY_USAGE_CPU_ONLY = MemoryUsage 2
-- No documentation found for Nested "VmaMemoryUsage" "VMA_MEMORY_USAGE_CPU_TO_GPU"
pattern MEMORY_USAGE_CPU_TO_GPU = MemoryUsage 3
-- No documentation found for Nested "VmaMemoryUsage" "VMA_MEMORY_USAGE_GPU_TO_CPU"
pattern MEMORY_USAGE_GPU_TO_CPU = MemoryUsage 4
-- No documentation found for Nested "VmaMemoryUsage" "VMA_MEMORY_USAGE_CPU_COPY"
pattern MEMORY_USAGE_CPU_COPY = MemoryUsage 5
-- No documentation found for Nested "VmaMemoryUsage" "VMA_MEMORY_USAGE_GPU_LAZILY_ALLOCATED"
pattern MEMORY_USAGE_GPU_LAZILY_ALLOCATED = MemoryUsage 6
{-# complete MEMORY_USAGE_UNKNOWN,
             MEMORY_USAGE_GPU_ONLY,
             MEMORY_USAGE_CPU_ONLY,
             MEMORY_USAGE_CPU_TO_GPU,
             MEMORY_USAGE_GPU_TO_CPU,
             MEMORY_USAGE_CPU_COPY,
             MEMORY_USAGE_GPU_LAZILY_ALLOCATED :: MemoryUsage #-}

instance Show MemoryUsage where
  showsPrec p = \case
    MEMORY_USAGE_UNKNOWN -> showString "MEMORY_USAGE_UNKNOWN"
    MEMORY_USAGE_GPU_ONLY -> showString "MEMORY_USAGE_GPU_ONLY"
    MEMORY_USAGE_CPU_ONLY -> showString "MEMORY_USAGE_CPU_ONLY"
    MEMORY_USAGE_CPU_TO_GPU -> showString "MEMORY_USAGE_CPU_TO_GPU"
    MEMORY_USAGE_GPU_TO_CPU -> showString "MEMORY_USAGE_GPU_TO_CPU"
    MEMORY_USAGE_CPU_COPY -> showString "MEMORY_USAGE_CPU_COPY"
    MEMORY_USAGE_GPU_LAZILY_ALLOCATED -> showString "MEMORY_USAGE_GPU_LAZILY_ALLOCATED"
    MemoryUsage x -> showParen (p >= 11) (showString "MemoryUsage " . showsPrec 11 x)

instance Read MemoryUsage where
  readPrec = parens (choose [("MEMORY_USAGE_UNKNOWN", pure MEMORY_USAGE_UNKNOWN)
                            , ("MEMORY_USAGE_GPU_ONLY", pure MEMORY_USAGE_GPU_ONLY)
                            , ("MEMORY_USAGE_CPU_ONLY", pure MEMORY_USAGE_CPU_ONLY)
                            , ("MEMORY_USAGE_CPU_TO_GPU", pure MEMORY_USAGE_CPU_TO_GPU)
                            , ("MEMORY_USAGE_GPU_TO_CPU", pure MEMORY_USAGE_GPU_TO_CPU)
                            , ("MEMORY_USAGE_CPU_COPY", pure MEMORY_USAGE_CPU_COPY)
                            , ("MEMORY_USAGE_GPU_LAZILY_ALLOCATED", pure MEMORY_USAGE_GPU_LAZILY_ALLOCATED)]
                     +++
                     prec 10 (do
                       expectP (Ident "MemoryUsage")
                       v <- step readPrec
                       pure (MemoryUsage v)))


-- No documentation found for TopLevel "VmaAllocationCreateInfo"
data AllocationCreateInfo = AllocationCreateInfo
  { -- No documentation found for Nested "VmaAllocationCreateInfo" "flags"
    flags :: AllocationCreateFlags
  , -- No documentation found for Nested "VmaAllocationCreateInfo" "usage"
    usage :: MemoryUsage
  , -- No documentation found for Nested "VmaAllocationCreateInfo" "requiredFlags"
    requiredFlags :: MemoryPropertyFlags
  , -- No documentation found for Nested "VmaAllocationCreateInfo" "preferredFlags"
    preferredFlags :: MemoryPropertyFlags
  , -- No documentation found for Nested "VmaAllocationCreateInfo" "memoryTypeBits"
    memoryTypeBits :: Word32
  , -- No documentation found for Nested "VmaAllocationCreateInfo" "pool"
    pool :: Pool
  , -- No documentation found for Nested "VmaAllocationCreateInfo" "pUserData"
    userData :: Ptr ()
  }
  deriving (Typeable)
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


-- No documentation found for TopLevel "VmaDefragmentationPassInfo"
data DefragmentationPassInfo = DefragmentationPassInfo
  { -- No documentation found for Nested "VmaDefragmentationPassInfo" "moveCount"
    moveCount :: Word32
  , -- No documentation found for Nested "VmaDefragmentationPassInfo" "pMoves"
    moves :: Ptr DefragmentationPassMoveInfo
  }
  deriving (Typeable)
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


-- No documentation found for TopLevel "VmaAllocatorCreateInfo"
data AllocatorCreateInfo = AllocatorCreateInfo
  { -- No documentation found for Nested "VmaAllocatorCreateInfo" "flags"
    flags :: AllocatorCreateFlags
  , -- No documentation found for Nested "VmaAllocatorCreateInfo" "physicalDevice"
    physicalDevice :: Ptr PhysicalDevice_T
  , -- No documentation found for Nested "VmaAllocatorCreateInfo" "device"
    device :: Ptr Device_T
  , -- No documentation found for Nested "VmaAllocatorCreateInfo" "preferredLargeHeapBlockSize"
    preferredLargeHeapBlockSize :: DeviceSize
  , -- No documentation found for Nested "VmaAllocatorCreateInfo" "pAllocationCallbacks"
    allocationCallbacks :: Maybe AllocationCallbacks
  , -- No documentation found for Nested "VmaAllocatorCreateInfo" "pDeviceMemoryCallbacks"
    deviceMemoryCallbacks :: Maybe DeviceMemoryCallbacks
  , -- No documentation found for Nested "VmaAllocatorCreateInfo" "frameInUseCount"
    frameInUseCount :: Word32
  , -- No documentation found for Nested "VmaAllocatorCreateInfo" "pHeapSizeLimit"
    heapSizeLimit :: Ptr DeviceSize
  , -- No documentation found for Nested "VmaAllocatorCreateInfo" "pVulkanFunctions"
    vulkanFunctions :: Maybe VulkanFunctions
  , -- No documentation found for Nested "VmaAllocatorCreateInfo" "pRecordSettings"
    recordSettings :: Maybe RecordSettings
  , -- No documentation found for Nested "VmaAllocatorCreateInfo" "instance"
    instance' :: Ptr Instance_T
  , -- No documentation found for Nested "VmaAllocatorCreateInfo" "vulkanApiVersion"
    vulkanApiVersion :: Word32
  }
  deriving (Typeable)
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


-- No documentation found for TopLevel "VmaDefragmentationInfo"
data DefragmentationInfo = DefragmentationInfo
  { -- No documentation found for Nested "VmaDefragmentationInfo" "maxBytesToMove"
    maxBytesToMove :: DeviceSize
  , -- No documentation found for Nested "VmaDefragmentationInfo" "maxAllocationsToMove"
    maxAllocationsToMove :: Word32
  }
  deriving (Typeable)
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


-- No documentation found for TopLevel "VmaDefragmentationInfo2"
data DefragmentationInfo2 = DefragmentationInfo2
  { -- No documentation found for Nested "VmaDefragmentationInfo2" "flags"
    flags :: DefragmentationFlags
  , -- No documentation found for Nested "VmaDefragmentationInfo2" "pAllocations"
    allocations :: Vector Allocation
  , -- No documentation found for Nested "VmaDefragmentationInfo2" "pAllocationsChanged"
    allocationsChanged :: Ptr Bool32
  , -- No documentation found for Nested "VmaDefragmentationInfo2" "pPools"
    pools :: Vector Pool
  , -- No documentation found for Nested "VmaDefragmentationInfo2" "maxCpuBytesToMove"
    maxCpuBytesToMove :: DeviceSize
  , -- No documentation found for Nested "VmaDefragmentationInfo2" "maxCpuAllocationsToMove"
    maxCpuAllocationsToMove :: Word32
  , -- No documentation found for Nested "VmaDefragmentationInfo2" "maxGpuBytesToMove"
    maxGpuBytesToMove :: DeviceSize
  , -- No documentation found for Nested "VmaDefragmentationInfo2" "maxGpuAllocationsToMove"
    maxGpuAllocationsToMove :: Word32
  , -- No documentation found for Nested "VmaDefragmentationInfo2" "commandBuffer"
    commandBuffer :: Ptr CommandBuffer_T
  }
  deriving (Typeable)
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


-- No documentation found for TopLevel "VmaPoolCreateInfo"
data PoolCreateInfo = PoolCreateInfo
  { -- No documentation found for Nested "VmaPoolCreateInfo" "memoryTypeIndex"
    memoryTypeIndex :: Word32
  , -- No documentation found for Nested "VmaPoolCreateInfo" "flags"
    flags :: PoolCreateFlags
  , -- No documentation found for Nested "VmaPoolCreateInfo" "blockSize"
    blockSize :: DeviceSize
  , -- No documentation found for Nested "VmaPoolCreateInfo" "minBlockCount"
    minBlockCount :: Word64
  , -- No documentation found for Nested "VmaPoolCreateInfo" "maxBlockCount"
    maxBlockCount :: Word64
  , -- No documentation found for Nested "VmaPoolCreateInfo" "frameInUseCount"
    frameInUseCount :: Word32
  }
  deriving (Typeable)
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
             memoryTypeIndex flags blockSize ((\(CSize a) -> a) minBlockCount) ((\(CSize a) -> a) maxBlockCount) frameInUseCount

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


-- No documentation found for TopLevel "VmaDefragmentationStats"
data DefragmentationStats = DefragmentationStats
  { -- No documentation found for Nested "VmaDefragmentationStats" "bytesMoved"
    bytesMoved :: DeviceSize
  , -- No documentation found for Nested "VmaDefragmentationStats" "bytesFreed"
    bytesFreed :: DeviceSize
  , -- No documentation found for Nested "VmaDefragmentationStats" "allocationsMoved"
    allocationsMoved :: Word32
  , -- No documentation found for Nested "VmaDefragmentationStats" "deviceMemoryBlocksFreed"
    deviceMemoryBlocksFreed :: Word32
  }
  deriving (Typeable)
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


-- No documentation found for TopLevel "VmaAllocationInfo"
data AllocationInfo = AllocationInfo
  { -- No documentation found for Nested "VmaAllocationInfo" "memoryType"
    memoryType :: Word32
  , -- No documentation found for Nested "VmaAllocationInfo" "deviceMemory"
    deviceMemory :: DeviceMemory
  , -- No documentation found for Nested "VmaAllocationInfo" "offset"
    offset :: DeviceSize
  , -- No documentation found for Nested "VmaAllocationInfo" "size"
    size :: DeviceSize
  , -- No documentation found for Nested "VmaAllocationInfo" "pMappedData"
    mappedData :: Ptr ()
  , -- No documentation found for Nested "VmaAllocationInfo" "pUserData"
    userData :: Ptr ()
  }
  deriving (Typeable)
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


-- No documentation found for TopLevel "VmaAllocatorInfo"
data AllocatorInfo = AllocatorInfo
  { -- No documentation found for Nested "VmaAllocatorInfo" "instance"
    instance' :: Ptr Instance_T
  , -- No documentation found for Nested "VmaAllocatorInfo" "physicalDevice"
    physicalDevice :: Ptr PhysicalDevice_T
  , -- No documentation found for Nested "VmaAllocatorInfo" "device"
    device :: Ptr Device_T
  }
  deriving (Typeable)
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


-- No documentation found for TopLevel "VmaVulkanFunctions"
data VulkanFunctions = VulkanFunctions
  { -- No documentation found for Nested "VmaVulkanFunctions" "vkGetPhysicalDeviceProperties"
    vkGetPhysicalDeviceProperties :: PFN_vkGetPhysicalDeviceProperties
  , -- No documentation found for Nested "VmaVulkanFunctions" "vkGetPhysicalDeviceMemoryProperties"
    vkGetPhysicalDeviceMemoryProperties :: PFN_vkGetPhysicalDeviceMemoryProperties
  , -- No documentation found for Nested "VmaVulkanFunctions" "vkAllocateMemory"
    vkAllocateMemory :: PFN_vkAllocateMemory
  , -- No documentation found for Nested "VmaVulkanFunctions" "vkFreeMemory"
    vkFreeMemory :: PFN_vkFreeMemory
  , -- No documentation found for Nested "VmaVulkanFunctions" "vkMapMemory"
    vkMapMemory :: PFN_vkMapMemory
  , -- No documentation found for Nested "VmaVulkanFunctions" "vkUnmapMemory"
    vkUnmapMemory :: PFN_vkUnmapMemory
  , -- No documentation found for Nested "VmaVulkanFunctions" "vkFlushMappedMemoryRanges"
    vkFlushMappedMemoryRanges :: PFN_vkFlushMappedMemoryRanges
  , -- No documentation found for Nested "VmaVulkanFunctions" "vkInvalidateMappedMemoryRanges"
    vkInvalidateMappedMemoryRanges :: PFN_vkInvalidateMappedMemoryRanges
  , -- No documentation found for Nested "VmaVulkanFunctions" "vkBindBufferMemory"
    vkBindBufferMemory :: PFN_vkBindBufferMemory
  , -- No documentation found for Nested "VmaVulkanFunctions" "vkBindImageMemory"
    vkBindImageMemory :: PFN_vkBindImageMemory
  , -- No documentation found for Nested "VmaVulkanFunctions" "vkGetBufferMemoryRequirements"
    vkGetBufferMemoryRequirements :: PFN_vkGetBufferMemoryRequirements
  , -- No documentation found for Nested "VmaVulkanFunctions" "vkGetImageMemoryRequirements"
    vkGetImageMemoryRequirements :: PFN_vkGetImageMemoryRequirements
  , -- No documentation found for Nested "VmaVulkanFunctions" "vkCreateBuffer"
    vkCreateBuffer :: PFN_vkCreateBuffer
  , -- No documentation found for Nested "VmaVulkanFunctions" "vkDestroyBuffer"
    vkDestroyBuffer :: PFN_vkDestroyBuffer
  , -- No documentation found for Nested "VmaVulkanFunctions" "vkCreateImage"
    vkCreateImage :: PFN_vkCreateImage
  , -- No documentation found for Nested "VmaVulkanFunctions" "vkDestroyImage"
    vkDestroyImage :: PFN_vkDestroyImage
  , -- No documentation found for Nested "VmaVulkanFunctions" "vkCmdCopyBuffer"
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
  deriving (Typeable)
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
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr PFN_vkGetPhysicalDeviceProperties)) (zero)
    poke ((p `plusPtr` 8 :: Ptr PFN_vkGetPhysicalDeviceMemoryProperties)) (zero)
    poke ((p `plusPtr` 16 :: Ptr PFN_vkAllocateMemory)) (zero)
    poke ((p `plusPtr` 24 :: Ptr PFN_vkFreeMemory)) (zero)
    poke ((p `plusPtr` 32 :: Ptr PFN_vkMapMemory)) (zero)
    poke ((p `plusPtr` 40 :: Ptr PFN_vkUnmapMemory)) (zero)
    poke ((p `plusPtr` 48 :: Ptr PFN_vkFlushMappedMemoryRanges)) (zero)
    poke ((p `plusPtr` 56 :: Ptr PFN_vkInvalidateMappedMemoryRanges)) (zero)
    poke ((p `plusPtr` 64 :: Ptr PFN_vkBindBufferMemory)) (zero)
    poke ((p `plusPtr` 72 :: Ptr PFN_vkBindImageMemory)) (zero)
    poke ((p `plusPtr` 80 :: Ptr PFN_vkGetBufferMemoryRequirements)) (zero)
    poke ((p `plusPtr` 88 :: Ptr PFN_vkGetImageMemoryRequirements)) (zero)
    poke ((p `plusPtr` 96 :: Ptr PFN_vkCreateBuffer)) (zero)
    poke ((p `plusPtr` 104 :: Ptr PFN_vkDestroyBuffer)) (zero)
    poke ((p `plusPtr` 112 :: Ptr PFN_vkCreateImage)) (zero)
    poke ((p `plusPtr` 120 :: Ptr PFN_vkDestroyImage)) (zero)
    poke ((p `plusPtr` 128 :: Ptr PFN_vkCmdCopyBuffer)) (zero)
    poke ((p `plusPtr` 136 :: Ptr PFN_vkGetBufferMemoryRequirements2KHR)) (zero)
    poke ((p `plusPtr` 144 :: Ptr PFN_vkGetImageMemoryRequirements2KHR)) (zero)
    poke ((p `plusPtr` 152 :: Ptr PFN_vkBindBufferMemory2KHR)) (zero)
    poke ((p `plusPtr` 160 :: Ptr PFN_vkBindImageMemory2KHR)) (zero)
    poke ((p `plusPtr` 168 :: Ptr PFN_vkGetPhysicalDeviceMemoryProperties2KHR)) (zero)
    f

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


-- No documentation found for TopLevel "VmaPoolStats"
data PoolStats = PoolStats
  { -- No documentation found for Nested "VmaPoolStats" "size"
    size :: DeviceSize
  , -- No documentation found for Nested "VmaPoolStats" "unusedSize"
    unusedSize :: DeviceSize
  , -- No documentation found for Nested "VmaPoolStats" "allocationCount"
    allocationCount :: Word64
  , -- No documentation found for Nested "VmaPoolStats" "unusedRangeCount"
    unusedRangeCount :: Word64
  , -- No documentation found for Nested "VmaPoolStats" "unusedRangeSizeMax"
    unusedRangeSizeMax :: DeviceSize
  , -- No documentation found for Nested "VmaPoolStats" "blockCount"
    blockCount :: Word64
  }
  deriving (Typeable)
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
             size unusedSize ((\(CSize a) -> a) allocationCount) ((\(CSize a) -> a) unusedRangeCount) unusedRangeSizeMax ((\(CSize a) -> a) blockCount)

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


-- No documentation found for TopLevel "VmaStatInfo"
data StatInfo = StatInfo
  { -- No documentation found for Nested "VmaStatInfo" "blockCount"
    blockCount :: Word32
  , -- No documentation found for Nested "VmaStatInfo" "allocationCount"
    allocationCount :: Word32
  , -- No documentation found for Nested "VmaStatInfo" "unusedRangeCount"
    unusedRangeCount :: Word32
  , -- No documentation found for Nested "VmaStatInfo" "usedBytes"
    usedBytes :: DeviceSize
  , -- No documentation found for Nested "VmaStatInfo" "unusedBytes"
    unusedBytes :: DeviceSize
  , -- No documentation found for Nested "VmaStatInfo" "allocationSizeMin"
    allocationSizeMin :: DeviceSize
  , -- No documentation found for Nested "VmaStatInfo" "allocationSizeAvg"
    allocationSizeAvg :: DeviceSize
  , -- No documentation found for Nested "VmaStatInfo" "allocationSizeMax"
    allocationSizeMax :: DeviceSize
  , -- No documentation found for Nested "VmaStatInfo" "unusedRangeSizeMin"
    unusedRangeSizeMin :: DeviceSize
  , -- No documentation found for Nested "VmaStatInfo" "unusedRangeSizeAvg"
    unusedRangeSizeAvg :: DeviceSize
  , -- No documentation found for Nested "VmaStatInfo" "unusedRangeSizeMax"
    unusedRangeSizeMax :: DeviceSize
  }
  deriving (Typeable)
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


-- No documentation found for TopLevel "VmaRecordSettings"
data RecordSettings = RecordSettings
  { -- No documentation found for Nested "VmaRecordSettings" "flags"
    flags :: RecordFlags
  , -- No documentation found for Nested "VmaRecordSettings" "pFilePath"
    filePath :: ByteString
  }
  deriving (Typeable)
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


-- No documentation found for TopLevel "VmaBudget"
data Budget = Budget
  { -- No documentation found for Nested "VmaBudget" "blockBytes"
    blockBytes :: DeviceSize
  , -- No documentation found for Nested "VmaBudget" "allocationBytes"
    allocationBytes :: DeviceSize
  , -- No documentation found for Nested "VmaBudget" "usage"
    usage :: DeviceSize
  , -- No documentation found for Nested "VmaBudget" "budget"
    budget :: DeviceSize
  }
  deriving (Typeable)
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


-- No documentation found for TopLevel "VmaDeviceMemoryCallbacks"
data DeviceMemoryCallbacks = DeviceMemoryCallbacks
  { -- No documentation found for Nested "VmaDeviceMemoryCallbacks" "pfnAllocate"
    pfnAllocate :: PFN_vmaAllocateDeviceMemoryFunction
  , -- No documentation found for Nested "VmaDeviceMemoryCallbacks" "pfnFree"
    pfnFree :: PFN_vmaFreeDeviceMemoryFunction
  , -- No documentation found for Nested "VmaDeviceMemoryCallbacks" "pUserData"
    userData :: Ptr ()
  }
  deriving (Typeable)
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


-- No documentation found for TopLevel "VmaDefragmentationPassMoveInfo"
data DefragmentationPassMoveInfo = DefragmentationPassMoveInfo
  { -- No documentation found for Nested "VmaDefragmentationPassMoveInfo" "allocation"
    allocation :: Allocation
  , -- No documentation found for Nested "VmaDefragmentationPassMoveInfo" "memory"
    memory :: DeviceMemory
  , -- No documentation found for Nested "VmaDefragmentationPassMoveInfo" "offset"
    offset :: DeviceSize
  }
  deriving (Typeable)
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


-- No documentation found for TopLevel "VmaStats"
data Stats = Stats
  { -- No documentation found for Nested "VmaStats" "memoryType"
    memoryType :: Vector StatInfo
  , -- No documentation found for Nested "VmaStats" "memoryHeap"
    memoryHeap :: Vector StatInfo
  , -- No documentation found for Nested "VmaStats" "total"
    total :: StatInfo
  }
  deriving (Typeable)
deriving instance Show Stats

instance ToCStruct Stats where
  withCStruct x f = allocaBytesAligned 3920 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p Stats{..} f = do
    unless ((Data.Vector.length $ (memoryType)) <= MAX_MEMORY_TYPES) $
      throwIO $ IOError Nothing InvalidArgument "" "memoryType is too long, a maximum of MAX_MEMORY_TYPES elements are allowed" Nothing Nothing
    Data.Vector.imapM_ (\i e -> poke ((lowerArrayPtr ((p `plusPtr` 0 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_MEMORY_TYPES StatInfo)))) `plusPtr` (80 * (i)) :: Ptr StatInfo) (e)) (memoryType)
    unless ((Data.Vector.length $ (memoryHeap)) <= MAX_MEMORY_HEAPS) $
      throwIO $ IOError Nothing InvalidArgument "" "memoryHeap is too long, a maximum of MAX_MEMORY_HEAPS elements are allowed" Nothing Nothing
    Data.Vector.imapM_ (\i e -> poke ((lowerArrayPtr ((p `plusPtr` 2560 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_MEMORY_HEAPS StatInfo)))) `plusPtr` (80 * (i)) :: Ptr StatInfo) (e)) (memoryHeap)
    poke ((p `plusPtr` 3840 :: Ptr StatInfo)) (total)
    f
  cStructSize = 3920
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    unless ((Data.Vector.length $ (mempty)) <= MAX_MEMORY_TYPES) $
      throwIO $ IOError Nothing InvalidArgument "" "memoryType is too long, a maximum of MAX_MEMORY_TYPES elements are allowed" Nothing Nothing
    Data.Vector.imapM_ (\i e -> poke ((lowerArrayPtr ((p `plusPtr` 0 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_MEMORY_TYPES StatInfo)))) `plusPtr` (80 * (i)) :: Ptr StatInfo) (e)) (mempty)
    unless ((Data.Vector.length $ (mempty)) <= MAX_MEMORY_HEAPS) $
      throwIO $ IOError Nothing InvalidArgument "" "memoryHeap is too long, a maximum of MAX_MEMORY_HEAPS elements are allowed" Nothing Nothing
    Data.Vector.imapM_ (\i e -> poke ((lowerArrayPtr ((p `plusPtr` 2560 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_MEMORY_HEAPS StatInfo)))) `plusPtr` (80 * (i)) :: Ptr StatInfo) (e)) (mempty)
    poke ((p `plusPtr` 3840 :: Ptr StatInfo)) (zero)
    f

instance FromCStruct Stats where
  peekCStruct p = do
    memoryType <- generateM (MAX_MEMORY_TYPES) (\i -> peekCStruct @StatInfo (((lowerArrayPtr @StatInfo ((p `plusPtr` 0 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_MEMORY_TYPES StatInfo)))) `advancePtrBytes` (80 * (i)) :: Ptr StatInfo)))
    memoryHeap <- generateM (MAX_MEMORY_HEAPS) (\i -> peekCStruct @StatInfo (((lowerArrayPtr @StatInfo ((p `plusPtr` 2560 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_MEMORY_HEAPS StatInfo)))) `advancePtrBytes` (80 * (i)) :: Ptr StatInfo)))
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


-- No documentation found for TopLevel "VmaPool"
newtype Pool = Pool Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show Pool where
  showsPrec p (Pool x) = showParen (p >= 11) (showString "Pool 0x" . showHex x)


-- No documentation found for TopLevel "VmaDefragmentationContext"
newtype DefragmentationContext = DefragmentationContext Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show DefragmentationContext where
  showsPrec p (DefragmentationContext x) = showParen (p >= 11) (showString "DefragmentationContext 0x" . showHex x)


-- No documentation found for TopLevel "VmaAllocation"
newtype Allocation = Allocation Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show Allocation where
  showsPrec p (Allocation x) = showParen (p >= 11) (showString "Allocation 0x" . showHex x)


-- No documentation found for TopLevel "VmaAllocator"
newtype Allocator = Allocator Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show Allocator where
  showsPrec p (Allocator x) = showParen (p >= 11) (showString "Allocator 0x" . showHex x)


type FN_vmaFreeDeviceMemoryFunction = Allocator -> ("memoryType" ::: Word32) -> DeviceMemory -> DeviceSize -> ("pUserData" ::: Ptr ()) -> IO ()
-- No documentation found for TopLevel "PFN_vmaFreeDeviceMemoryFunction"
type PFN_vmaFreeDeviceMemoryFunction = FunPtr FN_vmaFreeDeviceMemoryFunction


type FN_vmaAllocateDeviceMemoryFunction = Allocator -> ("memoryType" ::: Word32) -> DeviceMemory -> DeviceSize -> ("pUserData" ::: Ptr ()) -> IO ()
-- No documentation found for TopLevel "PFN_vmaAllocateDeviceMemoryFunction"
type PFN_vmaAllocateDeviceMemoryFunction = FunPtr FN_vmaAllocateDeviceMemoryFunction

