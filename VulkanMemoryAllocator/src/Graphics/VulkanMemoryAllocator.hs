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
  :: Allocator -> Ptr Allocation -> CSize -> Ptr Bool32 -> Ptr DefragmentationInfo -> Ptr DefragmentationStats -> IO Result

-- No documentation found for TopLevel "vmaDefragment"
defragment :: Allocator -> ("allocations" ::: Vector Allocation) -> ("defragmentationInfo" ::: Maybe DefragmentationInfo) -> IO (("allocationsChanged" ::: Vector Bool32), DefragmentationStats)
defragment allocator allocations defragmentationInfo = evalContT $ do
  pPAllocations <- ContT $ allocaBytesAligned @Allocation ((Data.Vector.length (allocations)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPAllocations `plusPtr` (8 * (i)) :: Ptr Allocation) (e)) (allocations)
  pPAllocationsChanged <- ContT $ bracket (callocBytes @Bool32 ((fromIntegral ((fromIntegral (Data.Vector.length $ (allocations)) :: CSize))) * 4)) free
  pDefragmentationInfo <- case (defragmentationInfo) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPDefragmentationStats <- ContT (withZeroCStruct @DefragmentationStats)
  r <- lift $ (ffiVmaDefragment) (allocator) (pPAllocations) ((fromIntegral (Data.Vector.length $ (allocations)) :: CSize)) (pPAllocationsChanged) pDefragmentationInfo (pPDefragmentationStats)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pAllocationsChanged <- lift $ generateM (fromIntegral ((fromIntegral (Data.Vector.length $ (allocations)) :: CSize))) (\i -> peek @Bool32 ((pPAllocationsChanged `advancePtrBytes` (4 * (i)) :: Ptr Bool32)))
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


-- | VmaAllocationCreateInfo
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1a5225e5e11f8376f6a31a1791f3d6e817 VmaAllocationCreateFlags>
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocation_create_info_1add09658ac14fe290ace25470ddd6d41b flags>
--
--     Use
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1a4fceecc301f4064dc808d3cd6c038941 VmaAllocationCreateFlagBits>
--     enum.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1aa5846affa1e9da3800e3e78fae2305cc VmaMemoryUsage>
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocation_create_info_1accb8b06b1f677d858cb9af20705fa910 usage>
--
--     Intended usage of memory.
--
-- -   VkMemoryPropertyFlags
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocation_create_info_1a9166390303ff42d783305bc31c2b6b90 requiredFlags>
--
--     Flags that must be set in a Memory Type chosen for an allocation.
--
-- -   VkMemoryPropertyFlags
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocation_create_info_1a7fe8d81a1ad10b2a2faacacee5b15d6d preferredFlags>
--
--     Flags that preferably should be set in a memory type chosen for an
--     allocation.
--
-- -   uint32_t
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocation_create_info_1a3bf940c0271d85d6ba32a4d820075055 memoryTypeBits>
--
--     Bitmask containing one bit set for every memory type acceptable for
--     this allocation.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_pool VmaPool>
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocation_create_info_1a6272c0555cfd1fe28bff1afeb6190150 pool>
--
--     Pool that this allocation should be created in.
--
-- -   void *
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocation_create_info_1a8259e85c272683434f4abb4ddddffe19 pUserData>
--
--     Custom general-purpose pointer that will be stored in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocation VmaAllocation>,
--     can be read as
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocation_info_1adc507656149c04de7ed95d0042ba2a13 VmaAllocationInfo::pUserData>
--     and changed using
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1af9147d31ffc11d62fc187bde283ed14f vmaSetAllocationUserData()>.
--
-- === flags
--
-- flags
-- VmaAllocationCreateInfo
-- VmaAllocationCreateInfo
-- flags
-- @VmaAllocationCreateFlags VmaAllocationCreateInfo::flags@
--
-- Use
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1a4fceecc301f4064dc808d3cd6c038941 VmaAllocationCreateFlagBits>
-- enum.
--
-- === memoryTypeBits
--
-- memoryTypeBits
-- VmaAllocationCreateInfo
-- VmaAllocationCreateInfo
-- memoryTypeBits
-- @uint32_t VmaAllocationCreateInfo::memoryTypeBits@
--
-- Bitmask containing one bit set for every memory type acceptable for this
-- allocation.
--
-- Value 0 is equivalent to @UINT32_MAX@ - it means any memory type is
-- accepted if it meets other requirements specified by this structure,
-- with no further restrictions on memory type index.   If 'Pool' is not
-- null, this member is ignored.
--
-- === pool
--
-- pool
-- VmaAllocationCreateInfo
-- VmaAllocationCreateInfo
-- pool
-- @VmaPool VmaAllocationCreateInfo::pool@
--
-- Pool that this allocation should be created in.
--
-- Leave @VK_NULL_HANDLE@ to allocate from default pool. If not null,
-- members: @usage@, @requiredFlags@, @preferredFlags@, @memoryTypeBits@
-- are ignored.
--
-- === preferredFlags
--
-- preferredFlags
-- VmaAllocationCreateInfo
-- VmaAllocationCreateInfo
-- preferredFlags
-- @VkMemoryPropertyFlags VmaAllocationCreateInfo::preferredFlags@
--
-- Flags that preferably should be set in a memory type chosen for an
-- allocation.
--
-- Set to 0 if no additional flags are prefered.   If 'Pool' is not null,
-- this member is ignored.
--
-- === pUserData
--
-- pUserData
-- VmaAllocationCreateInfo
-- VmaAllocationCreateInfo
-- pUserData
-- @void* VmaAllocationCreateInfo::pUserData@
--
-- Custom general-purpose pointer that will be stored in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocation VmaAllocation>,
-- can be read as
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocation_info_1adc507656149c04de7ed95d0042ba2a13 VmaAllocationInfo::pUserData>
-- and changed using
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1af9147d31ffc11d62fc187bde283ed14f vmaSetAllocationUserData()>.
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1ad9889c10c798b040d59c92f257cae597aa6f24f821cd6a7c5e4a443f7bf59c520 VMA_ALLOCATION_CREATE_USER_DATA_COPY_STRING_BIT>
-- is used, it must be either null or pointer to a null-terminated string.
-- The string will be then copied to internal buffer, so it doesn\'t need
-- to be valid after allocation call.
--
-- === requiredFlags
--
-- requiredFlags
-- VmaAllocationCreateInfo
-- VmaAllocationCreateInfo
-- requiredFlags
-- @VkMemoryPropertyFlags VmaAllocationCreateInfo::requiredFlags@
--
-- Flags that must be set in a Memory Type chosen for an allocation.
--
-- Leave 0 if you specify memory requirements in other way.   If 'Pool' is
-- not null, this member is ignored.
--
-- === usage
--
-- usage
-- VmaAllocationCreateInfo
-- VmaAllocationCreateInfo
-- usage
-- @VmaMemoryUsage VmaAllocationCreateInfo::usage@
--
-- Intended usage of memory.
--
-- You can leave
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1aa5846affa1e9da3800e3e78fae2305ccaf50d27e34e0925cf3a63db8c839121dd VMA_MEMORY_USAGE_UNKNOWN>
-- if you specify memory requirements in other way.   If 'Pool' is not
-- null, this member is ignored.
--
-- The documentation for this struct was generated from the following file:
--
-- vk_mem_alloc.h
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


-- | VmaDefragmentationPassInfo
--
-- Parameters for incremental defragmentation steps.
--
-- -   uint32_t
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_defragmentation_pass_info_1ac1086e657ba995f8d1f4e49b83dcfb6c moveCount>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_defragmentation_pass_move_info VmaDefragmentationPassMoveInfo>
--     *
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_defragmentation_pass_info_1acbd42d4a3357999da130a95cd99a3792 pMoves>
--
-- == Detailed Description
--
-- Parameters for incremental defragmentation steps.
--
-- To be used with function
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1ac0f01545b6262f7d4d128fc8f8e5c77b vmaBeginDefragmentationPass()>.
--
-- === moveCount
--
-- moveCount
-- VmaDefragmentationPassInfo
-- VmaDefragmentationPassInfo
-- moveCount
-- @uint32_t VmaDefragmentationPassInfo::moveCount@
--
-- === pMoves
--
-- pMoves
-- VmaDefragmentationPassInfo
-- VmaDefragmentationPassInfo
-- pMoves
-- @VmaDefragmentationPassMoveInfo* VmaDefragmentationPassInfo::pMoves@
--
-- The documentation for this struct was generated from the following file:
--
-- vk_mem_alloc.h
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


-- | VmaAllocatorCreateInfo
--
-- Description of a Allocator to be created.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1acfe6863e160722c2c1bbcf7573fddc4d VmaAllocatorCreateFlags>
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocator_create_info_1a392ea2ecbaff93f91a7c49f735ad4346 flags>
--
--     Flags for created allocator. Use
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1afd73b95e737ee7e76f827cb5472f559f VmaAllocatorCreateFlagBits>
--     enum.
--
-- -   VkPhysicalDevice
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocator_create_info_1a08230f04ae6ccf8a78150a9e829a7156 physicalDevice>
--
--     Vulkan physical device.
--
-- -   VkDevice
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocator_create_info_1ad924ddd77b04039c88d0c09b0ffcd500 device>
--
--     Vulkan device.
--
-- -   VkDeviceSize
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocator_create_info_1a8e4714298e3121cdd8b214a1ae7a637a preferredLargeHeapBlockSize>
--
--     Preferred size of a single @VkDeviceMemory@ block to be allocated
--     from large heaps > 1 GiB. Optional.
--
-- -   const VkAllocationCallbacks *
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocator_create_info_1a6e409087e3be55400d0e4ccbe43c608d pAllocationCallbacks>
--
--     Custom CPU memory allocation callbacks. Optional.
--
-- -   const
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_device_memory_callbacks VmaDeviceMemoryCallbacks>
--     *
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocator_create_info_1af1380969b5e1ea4c3184a877892d260e pDeviceMemoryCallbacks>
--
--     Informative callbacks for @vkAllocateMemory@, @vkFreeMemory@.
--     Optional.
--
-- -   uint32_t
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocator_create_info_1a21ea188dd212b8171cb9ecbed4a2a3a7 frameInUseCount>
--
--     Maximum number of additional frames that are in use at the same time
--     as current frame.
--
-- -   const VkDeviceSize *
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocator_create_info_1a31c192aa6cbffa33279f6d9f0c47c44b pHeapSizeLimit>
--
--     Either null or a pointer to an array of limits on maximum number of
--     bytes that can be allocated out of particular Vulkan memory heap.
--
-- -   const
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_vulkan_functions VmaVulkanFunctions>
--     *
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocator_create_info_1a3dc197be3227da7338b1643f70db36bd pVulkanFunctions>
--
--     Pointers to Vulkan functions. Can be null.
--
-- -   const
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_record_settings VmaRecordSettings>
--     *
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocator_create_info_1ace2aa4877b16a42b0b7673d4e26000ee pRecordSettings>
--
--     Parameters for recording of VMA calls. Can be null.
--
-- -   VkInstance
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocator_create_info_1a70dd42e29b1df1d1b9b61532ae0b370b instance>
--
--     Handle to Vulkan instance object.
--
-- -   uint32_t
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocator_create_info_1ae0ffc55139b54520a6bb704b29ffc285 vulkanApiVersion>
--
--     Optional. The highest version of Vulkan that the application is
--     designed to use.
--
-- == Detailed Description
--
-- Description of a Allocator to be created.
--
-- === device
--
-- device
-- VmaAllocatorCreateInfo
-- VmaAllocatorCreateInfo
-- device
-- @VkDevice VmaAllocatorCreateInfo::device@
--
-- Vulkan device.
--
-- It must be valid throughout whole lifetime of created allocator.
--
-- === flags
--
-- flags
-- VmaAllocatorCreateInfo
-- VmaAllocatorCreateInfo
-- flags
-- @VmaAllocatorCreateFlags VmaAllocatorCreateInfo::flags@
--
-- Flags for created allocator. Use
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1afd73b95e737ee7e76f827cb5472f559f VmaAllocatorCreateFlagBits>
-- enum.
--
-- === frameInUseCount
--
-- frameInUseCount
-- VmaAllocatorCreateInfo
-- VmaAllocatorCreateInfo
-- frameInUseCount
-- @uint32_t VmaAllocatorCreateInfo::frameInUseCount@
--
-- Maximum number of additional frames that are in use at the same time as
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
--
-- === instance
--
-- instance
-- VmaAllocatorCreateInfo
-- VmaAllocatorCreateInfo
-- instance
-- @VkInstance VmaAllocatorCreateInfo::instance@
--
-- Handle to Vulkan instance object.
--
-- Starting from version 3.0.0 this member is no longer optional, it must
-- be set!
--
-- === pAllocationCallbacks
--
-- pAllocationCallbacks
-- VmaAllocatorCreateInfo
-- VmaAllocatorCreateInfo
-- pAllocationCallbacks
-- @const VkAllocationCallbacks* VmaAllocatorCreateInfo::pAllocationCallbacks@
--
-- Custom CPU memory allocation callbacks. Optional.
--
-- Optional, can be null. When specified, will also be used for all
-- CPU-side memory allocations.
--
-- === pDeviceMemoryCallbacks
--
-- pDeviceMemoryCallbacks
-- VmaAllocatorCreateInfo
-- VmaAllocatorCreateInfo
-- pDeviceMemoryCallbacks
-- @const VmaDeviceMemoryCallbacks* VmaAllocatorCreateInfo::pDeviceMemoryCallbacks@
--
-- Informative callbacks for @vkAllocateMemory@, @vkFreeMemory@. Optional.
--
-- Optional, can be null.
--
-- === pHeapSizeLimit
--
-- pHeapSizeLimit
-- VmaAllocatorCreateInfo
-- VmaAllocatorCreateInfo
-- pHeapSizeLimit
-- @const VkDeviceSize* VmaAllocatorCreateInfo::pHeapSizeLimit@
--
-- Either null or a pointer to an array of limits on maximum number of
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
--     instead when using
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1ab88db292a17974f911182543fda52d19 vmaGetMemoryProperties()>.
--
-- Warning! Using this feature may not be equivalent to installing a GPU
-- with smaller amount of memory, because graphics driver doesn\'t
-- necessary fail new allocations with @VK_ERROR_OUT_OF_DEVICE_MEMORY@
-- result when memory capacity is exceeded. It may return success and just
-- silently migrate some device memory blocks to system RAM. This driver
-- behavior can also be controlled using
-- VK_AMD_memory_overallocation_behavior extension.
--
-- === physicalDevice
--
-- physicalDevice
-- VmaAllocatorCreateInfo
-- VmaAllocatorCreateInfo
-- physicalDevice
-- @VkPhysicalDevice VmaAllocatorCreateInfo::physicalDevice@
--
-- Vulkan physical device.
--
-- It must be valid throughout whole lifetime of created allocator.
--
-- === pRecordSettings
--
-- pRecordSettings
-- VmaAllocatorCreateInfo
-- VmaAllocatorCreateInfo
-- pRecordSettings
-- @const VmaRecordSettings* VmaAllocatorCreateInfo::pRecordSettings@
--
-- Parameters for recording of VMA calls. Can be null.
--
-- If not null, it enables recording of calls to VMA functions to a file.
-- If support for recording is not enabled using @VMA_RECORDING_ENABLED@
-- macro, creation of the allocator object fails with
-- @VK_ERROR_FEATURE_NOT_PRESENT@.
--
-- === preferredLargeHeapBlockSize
--
-- preferredLargeHeapBlockSize
-- VmaAllocatorCreateInfo
-- VmaAllocatorCreateInfo
-- preferredLargeHeapBlockSize
-- @VkDeviceSize VmaAllocatorCreateInfo::preferredLargeHeapBlockSize@
--
-- Preferred size of a single @VkDeviceMemory@ block to be allocated from
-- large heaps > 1 GiB. Optional.
--
-- Set to 0 to use default, which is currently 256 MiB.
--
-- === pVulkanFunctions
--
-- pVulkanFunctions
-- VmaAllocatorCreateInfo
-- VmaAllocatorCreateInfo
-- pVulkanFunctions
-- @const VmaVulkanFunctions* VmaAllocatorCreateInfo::pVulkanFunctions@
--
-- Pointers to Vulkan functions. Can be null.
--
-- For details see
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_configuration_1config_Vulkan_functions Pointers to Vulkan functions>.
--
-- === vulkanApiVersion
--
-- vulkanApiVersion
-- VmaAllocatorCreateInfo
-- VmaAllocatorCreateInfo
-- vulkanApiVersion
-- @uint32_t VmaAllocatorCreateInfo::vulkanApiVersion@
--
-- Optional. The highest version of Vulkan that the application is designed
-- to use.
--
-- It must be a value in the format as created by macro @VK_MAKE_VERSION@
-- or a constant like: @VK_API_VERSION_1_1@, @VK_API_VERSION_1_0@. The
-- patch version number specified is ignored. Only the major and minor
-- versions are considered. It must be less or equal (preferably equal) to
-- value as passed to @vkCreateInstance@ as
-- @VkApplicationInfo::apiVersion@. Only versions 1.0 and 1.1 are supported
-- by the current implementation. Leaving it initialized to zero is
-- equivalent to @VK_API_VERSION_1_0@.
--
-- The documentation for this struct was generated from the following file:
--
-- vk_mem_alloc.h
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


-- | VmaDefragmentationInfo
--
-- Deprecated. Optional configuration parameters to be passed to function
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1a9f0f8f56db5f7f57fe4454f465142dac vmaDefragment()>.
--
-- -   VkDeviceSize
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_defragmentation_info_1acb311c940a777270e67e1b81c5ab6a1d maxBytesToMove>
--
--     Maximum total numbers of bytes that can be copied while moving
--     allocations to different places.
--
-- -   uint32_t
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_defragmentation_info_1aa7c7304e13c71f604c907196c4e28fbc maxAllocationsToMove>
--
--     Maximum number of allocations that can be moved to different place.
--
-- == Detailed Description
--
-- Deprecated. Optional configuration parameters to be passed to function
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1a9f0f8f56db5f7f57fe4454f465142dac vmaDefragment()>.
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_deprecated_1_deprecated000002 Deprecated>
--
-- This is a part of the old interface. It is recommended to use structure
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_defragmentation_info2 VmaDefragmentationInfo2>
-- and function
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1a36ba776fd7fd5cb1e9359fdc0d8e6e8a vmaDefragmentationBegin()>
-- instead.
--
-- === maxAllocationsToMove
--
-- maxAllocationsToMove
-- VmaDefragmentationInfo
-- VmaDefragmentationInfo
-- maxAllocationsToMove
-- @uint32_t VmaDefragmentationInfo::maxAllocationsToMove@
--
-- Maximum number of allocations that can be moved to different place.
--
-- Default is @UINT32_MAX@, which means no limit.
--
-- === maxBytesToMove
--
-- maxBytesToMove
-- VmaDefragmentationInfo
-- VmaDefragmentationInfo
-- maxBytesToMove
-- @VkDeviceSize VmaDefragmentationInfo::maxBytesToMove@
--
-- Maximum total numbers of bytes that can be copied while moving
-- allocations to different places.
--
-- Default is @VK_WHOLE_SIZE@, which means no limit.
--
-- The documentation for this struct was generated from the following file:
--
-- vk_mem_alloc.h
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


-- | VmaDefragmentationInfo2
--
-- Parameters for defragmentation.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1a88a77cef37e5d3c4fc9eb328885d048d VmaDefragmentationFlags>
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_defragmentation_info2_1a53e844ee5633e229cf6daf14b2d9fff9 flags>
--
--     Reserved for future use. Should be 0.
--
-- -   uint32_t
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_defragmentation_info2_1a3cf86ab32c1da779b4923d301a3056ba allocationCount>
--
--     Number of allocations in @pAllocations@ array.
--
-- -   const
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocation VmaAllocation>
--     *
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_defragmentation_info2_1ab6d288f29d028156cf73542d630a2e32 pAllocations>
--
--     Pointer to array of allocations that can be defragmented.
--
-- -   VkBool32 *
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_defragmentation_info2_1a76d51a644dc7f5405d0cdd0025ecd0cc pAllocationsChanged>
--
--     Optional, output. Pointer to array that will be filled with
--     information whether the allocation at certain index has been changed
--     during defragmentation.
--
-- -   uint32_t
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_defragmentation_info2_1a7e70aa2a1081d849dcc7829b19d3ec9d poolCount>
--
--     Numer of pools in @pPools@ array.
--
-- -   const
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_pool VmaPool>
--     *
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_defragmentation_info2_1a3c9c6aa5c97d5670f8e362b3a6f3029b pPools>
--
--     Either null or pointer to array of pools to be defragmented.
--
-- -   VkDeviceSize
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_defragmentation_info2_1af78e1ea40c22d85137b65f6b384a4d0a maxCpuBytesToMove>
--
--     Maximum total numbers of bytes that can be copied while moving
--     allocations to different places using transfers on CPU side, like
--     @memcpy()@, @memmove()@.
--
-- -   uint32_t
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_defragmentation_info2_1a94c2c7223d52878445a8cccce396b671 maxCpuAllocationsToMove>
--
--     Maximum number of allocations that can be moved to a different place
--     using transfers on CPU side, like @memcpy()@, @memmove()@.
--
-- -   VkDeviceSize
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_defragmentation_info2_1a4ddbc898d0afe1518f863a3763628f08 maxGpuBytesToMove>
--
--     Maximum total numbers of bytes that can be copied while moving
--     allocations to different places using transfers on GPU side, posted
--     to @commandBuffer@.
--
-- -   uint32_t
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_defragmentation_info2_1a40d53d33e71ba0b66f844ed63c05a3f6 maxGpuAllocationsToMove>
--
--     Maximum number of allocations that can be moved to a different place
--     using transfers on GPU side, posted to @commandBuffer@.
--
-- -   VkCommandBuffer
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_defragmentation_info2_1a7f71f39590c5316771493d2333f9c1bd commandBuffer>
--
--     Optional. Command buffer where GPU copy commands will be posted.
--
-- == Detailed Description
--
-- Parameters for defragmentation.
--
-- To be used with function
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1a36ba776fd7fd5cb1e9359fdc0d8e6e8a vmaDefragmentationBegin()>.
--
-- === allocationCount
--
-- allocationCount
-- VmaDefragmentationInfo2
-- VmaDefragmentationInfo2
-- allocationCount
-- @uint32_t VmaDefragmentationInfo2::allocationCount@
--
-- Number of allocations in @pAllocations@ array.
--
-- === commandBuffer
--
-- commandBuffer
-- VmaDefragmentationInfo2
-- VmaDefragmentationInfo2
-- commandBuffer
-- @VkCommandBuffer VmaDefragmentationInfo2::commandBuffer@
--
-- Optional. Command buffer where GPU copy commands will be posted.
--
-- If not null, it must be a valid command buffer handle that supports
-- Transfer queue type. It must be in the recording state and outside of a
-- render pass instance. You need to submit it and make sure it finished
-- execution before calling
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1a8774e20e91e245aae959ba63efa15dd2 vmaDefragmentationEnd()>.
--
-- Passing null means that only CPU defragmentation will be performed.
--
-- === flags
--
-- flags
-- VmaDefragmentationInfo2
-- VmaDefragmentationInfo2
-- flags
-- @VmaDefragmentationFlags VmaDefragmentationInfo2::flags@
--
-- Reserved for future use. Should be 0.
--
-- === maxCpuAllocationsToMove
--
-- maxCpuAllocationsToMove
-- VmaDefragmentationInfo2
-- VmaDefragmentationInfo2
-- maxCpuAllocationsToMove
-- @uint32_t VmaDefragmentationInfo2::maxCpuAllocationsToMove@
--
-- Maximum number of allocations that can be moved to a different place
-- using transfers on CPU side, like @memcpy()@, @memmove()@.
--
-- @UINT32_MAX@ means no limit.
--
-- === maxCpuBytesToMove
--
-- maxCpuBytesToMove
-- VmaDefragmentationInfo2
-- VmaDefragmentationInfo2
-- maxCpuBytesToMove
-- @VkDeviceSize VmaDefragmentationInfo2::maxCpuBytesToMove@
--
-- Maximum total numbers of bytes that can be copied while moving
-- allocations to different places using transfers on CPU side, like
-- @memcpy()@, @memmove()@.
--
-- @VK_WHOLE_SIZE@ means no limit.
--
-- === maxGpuAllocationsToMove
--
-- maxGpuAllocationsToMove
-- VmaDefragmentationInfo2
-- VmaDefragmentationInfo2
-- maxGpuAllocationsToMove
-- @uint32_t VmaDefragmentationInfo2::maxGpuAllocationsToMove@
--
-- Maximum number of allocations that can be moved to a different place
-- using transfers on GPU side, posted to @commandBuffer@.
--
-- @UINT32_MAX@ means no limit.
--
-- === maxGpuBytesToMove
--
-- maxGpuBytesToMove
-- VmaDefragmentationInfo2
-- VmaDefragmentationInfo2
-- maxGpuBytesToMove
-- @VkDeviceSize VmaDefragmentationInfo2::maxGpuBytesToMove@
--
-- Maximum total numbers of bytes that can be copied while moving
-- allocations to different places using transfers on GPU side, posted to
-- @commandBuffer@.
--
-- @VK_WHOLE_SIZE@ means no limit.
--
-- === pAllocations
--
-- pAllocations
-- VmaDefragmentationInfo2
-- VmaDefragmentationInfo2
-- pAllocations
-- @const VmaAllocation* VmaDefragmentationInfo2::pAllocations@
--
-- Pointer to array of allocations that can be defragmented.
--
-- The array should have @allocationCount@ elements. The array should not
-- contain nulls. Elements in the array should be unique - same allocation
-- cannot occur twice. It is safe to pass allocations that are in the lost
-- state - they are ignored. All allocations not present in this array are
-- considered non-moveable during this defragmentation.
--
-- === pAllocationsChanged
--
-- pAllocationsChanged
-- VmaDefragmentationInfo2
-- VmaDefragmentationInfo2
-- pAllocationsChanged
-- @VkBool32* VmaDefragmentationInfo2::pAllocationsChanged@
--
-- Optional, output. Pointer to array that will be filled with information
-- whether the allocation at certain index has been changed during
-- defragmentation.
--
-- The array should have @allocationCount@ elements. You can pass null if
-- you are not interested in this information.
--
-- === poolCount
--
-- poolCount
-- VmaDefragmentationInfo2
-- VmaDefragmentationInfo2
-- poolCount
-- @uint32_t VmaDefragmentationInfo2::poolCount@
--
-- Numer of pools in @pPools@ array.
--
-- === pPools
--
-- pPools
-- VmaDefragmentationInfo2
-- VmaDefragmentationInfo2
-- pPools
-- @const VmaPool* VmaDefragmentationInfo2::pPools@
--
-- Either null or pointer to array of pools to be defragmented.
--
-- All the allocations in the specified pools can be moved during
-- defragmentation and there is no way to check if they were really moved
-- as in @pAllocationsChanged@, so you must query all the allocations in
-- all these pools for new @VkDeviceMemory@ and offset using
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1a86dd08aba8633bfa4ad0df2e76481d8b vmaGetAllocationInfo()>
-- if you might need to recreate buffers and images bound to them.
--
-- The array should have @poolCount@ elements. The array should not contain
-- nulls. Elements in the array should be unique - same pool cannot occur
-- twice.
--
-- Using this array is equivalent to specifying all allocations from the
-- pools in @pAllocations@. It might be more efficient.
--
-- The documentation for this struct was generated from the following file:
--
-- vk_mem_alloc.h
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


-- | VmaPoolCreateInfo
--
-- Describes parameter of created
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_pool VmaPool>.
--
-- -   uint32_t
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_pool_create_info_1a596fa76b685d3f1f688f84a709a5b319 memoryTypeIndex>
--
--     Vulkan memory type index to allocate this pool from.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1a2770e325ea42e087c1b91fdf46d0292a VmaPoolCreateFlags>
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_pool_create_info_1a8405139f63d078340ae74513a59f5446 flags>
--
--     Use combination of
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1a4d4f2efc2509157a9e4ecd4fd7942303 VmaPoolCreateFlagBits>.
--
-- -   VkDeviceSize
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_pool_create_info_1aa4265160536cdb9be821b7686c16c676 blockSize>
--
--     Size of a single @VkDeviceMemory@ block to be allocated as part of
--     this pool, in bytes. Optional.
--
-- -   size_t
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_pool_create_info_1ad8006fb803185c0a699d30f3e9a865ae minBlockCount>
--
--     Minimum number of blocks to be always allocated in this pool, even
--     if they stay empty.
--
-- -   size_t
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_pool_create_info_1ae41142f2834fcdc82baa4883c187b75c maxBlockCount>
--
--     Maximum number of blocks that can be allocated in this pool.
--     Optional.
--
-- -   uint32_t
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_pool_create_info_1a9437e43ffbb644dbbf7fc4e50cfad6aa frameInUseCount>
--
--     Maximum number of additional frames that are in use at the same time
--     as current frame.
--
-- == Detailed Description
--
-- Describes parameter of created
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_pool VmaPool>.
--
-- === blockSize
--
-- blockSize
-- VmaPoolCreateInfo
-- VmaPoolCreateInfo
-- blockSize
-- @VkDeviceSize VmaPoolCreateInfo::blockSize@
--
-- Size of a single @VkDeviceMemory@ block to be allocated as part of this
-- pool, in bytes. Optional.
--
-- Specify nonzero to set explicit, constant size of memory blocks used by
-- this pool.
--
-- Leave 0 to use default and let the library manage block sizes
-- automatically. Sizes of particular blocks may vary.
--
-- === flags
--
-- flags
-- VmaPoolCreateInfo
-- VmaPoolCreateInfo
-- flags
-- @VmaPoolCreateFlags VmaPoolCreateInfo::flags@
--
-- Use combination of
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1a4d4f2efc2509157a9e4ecd4fd7942303 VmaPoolCreateFlagBits>.
--
-- === frameInUseCount
--
-- frameInUseCount
-- VmaPoolCreateInfo
-- VmaPoolCreateInfo
-- frameInUseCount
-- @uint32_t VmaPoolCreateInfo::frameInUseCount@
--
-- Maximum number of additional frames that are in use at the same time as
-- current frame.
--
-- This value is used only when you make allocations with
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1ad9889c10c798b040d59c92f257cae597a5f436af6c8fe8540573a6d22627a6fd2 VMA_ALLOCATION_CREATE_CAN_BECOME_LOST_BIT>
-- flag. Such allocation cannot become lost if allocation.lastUseFrameIndex
-- >= allocator.currentFrameIndex - frameInUseCount.
--
-- For example, if you double-buffer your command buffers, so resources
-- used for rendering in previous frame may still be in use by the GPU at
-- the moment you allocate resources needed for the current frame, set this
-- value to 1.
--
-- If you want to allow any allocations other than used in the current
-- frame to become lost, set this value to 0.
--
-- === maxBlockCount
--
-- maxBlockCount
-- VmaPoolCreateInfo
-- VmaPoolCreateInfo
-- maxBlockCount
-- @size_t VmaPoolCreateInfo::maxBlockCount@
--
-- Maximum number of blocks that can be allocated in this pool. Optional.
--
-- Set to 0 to use default, which is @SIZE_MAX@, which means no limit.
--
-- Set to same value as
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_pool_create_info_1ad8006fb803185c0a699d30f3e9a865ae VmaPoolCreateInfo::minBlockCount>
-- to have fixed amount of memory allocated throughout whole lifetime of
-- this pool.
--
-- === memoryTypeIndex
--
-- memoryTypeIndex
-- VmaPoolCreateInfo
-- VmaPoolCreateInfo
-- memoryTypeIndex
-- @uint32_t VmaPoolCreateInfo::memoryTypeIndex@
--
-- Vulkan memory type index to allocate this pool from.
--
-- === minBlockCount
--
-- minBlockCount
-- VmaPoolCreateInfo
-- VmaPoolCreateInfo
-- minBlockCount
-- @size_t VmaPoolCreateInfo::minBlockCount@
--
-- Minimum number of blocks to be always allocated in this pool, even if
-- they stay empty.
--
-- Set to 0 to have no preallocated blocks and allow the pool be completely
-- empty.
--
-- The documentation for this struct was generated from the following file:
--
-- vk_mem_alloc.h
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


-- | VmaDefragmentationStats
--
-- Statistics returned by function
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1a9f0f8f56db5f7f57fe4454f465142dac vmaDefragment()>.
--
-- -   VkDeviceSize
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_defragmentation_stats_1a36f9d5df2a10ba2a36b16e126d60572d bytesMoved>
--
--     Total number of bytes that have been copied while moving allocations
--     to different places.
--
-- -   VkDeviceSize
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_defragmentation_stats_1ab0cb9ac0dbc106c77e384ea676422f28 bytesFreed>
--
--     Total number of bytes that have been released to the system by
--     freeing empty @VkDeviceMemory@ objects.
--
-- -   uint32_t
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_defragmentation_stats_1aefeabf130022008eadd75999478af3f9 allocationsMoved>
--
--     Number of allocations that have been moved to different places.
--
-- -   uint32_t
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_defragmentation_stats_1a0113f1877904a5d1ee8f409216ff276b deviceMemoryBlocksFreed>
--
--     Number of empty @VkDeviceMemory@ objects that have been released to
--     the system.
--
-- == Detailed Description
--
-- Statistics returned by function
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1a9f0f8f56db5f7f57fe4454f465142dac vmaDefragment()>.
--
-- === allocationsMoved
--
-- allocationsMoved
-- VmaDefragmentationStats
-- VmaDefragmentationStats
-- allocationsMoved
-- @uint32_t VmaDefragmentationStats::allocationsMoved@
--
-- Number of allocations that have been moved to different places.
--
-- === bytesFreed
--
-- bytesFreed
-- VmaDefragmentationStats
-- VmaDefragmentationStats
-- bytesFreed
-- @VkDeviceSize VmaDefragmentationStats::bytesFreed@
--
-- Total number of bytes that have been released to the system by freeing
-- empty @VkDeviceMemory@ objects.
--
-- === bytesMoved
--
-- bytesMoved
-- VmaDefragmentationStats
-- VmaDefragmentationStats
-- bytesMoved
-- @VkDeviceSize VmaDefragmentationStats::bytesMoved@
--
-- Total number of bytes that have been copied while moving allocations to
-- different places.
--
-- === deviceMemoryBlocksFreed
--
-- deviceMemoryBlocksFreed
-- VmaDefragmentationStats
-- VmaDefragmentationStats
-- deviceMemoryBlocksFreed
-- @uint32_t VmaDefragmentationStats::deviceMemoryBlocksFreed@
--
-- Number of empty @VkDeviceMemory@ objects that have been released to the
-- system.
--
-- The documentation for this struct was generated from the following file:
--
-- vk_mem_alloc.h
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


-- | VmaAllocationInfo
--
-- Parameters of
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocation VmaAllocation>
-- objects, that can be retrieved using function
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1a86dd08aba8633bfa4ad0df2e76481d8b vmaGetAllocationInfo()>.
--
-- -   uint32_t
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocation_info_1a7f6b0aa58c135e488e6b40a388dad9d5 memoryType>
--
--     Memory type index that this allocation was allocated from.
--
-- -   VkDeviceMemory
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocation_info_1ae0bfb7dfdf79a76ffefc9a94677a2f67 deviceMemory>
--
--     Handle to Vulkan memory object.
--
-- -   VkDeviceSize
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocation_info_1a4a3c732388dbdc7a23f9365b00825268 offset>
--
--     Offset into deviceMemory object to the beginning of this allocation,
--     in bytes. (deviceMemory, offset) pair is unique to this allocation.
--
-- -   VkDeviceSize
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocation_info_1aac76d113a6a5ccbb09fea00fb25fd18f size>
--
--     Size of this allocation, in bytes.
--
-- -   void *
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocation_info_1a5eeffbe2d2f30f53370ff14aefbadbe2 pMappedData>
--
--     Pointer to the beginning of this allocation as mapped data.
--
-- -   void *
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocation_info_1adc507656149c04de7ed95d0042ba2a13 pUserData>
--
--     Custom general-purpose pointer that was passed as
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocation_create_info_1a8259e85c272683434f4abb4ddddffe19 VmaAllocationCreateInfo::pUserData>
--     or set using
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1af9147d31ffc11d62fc187bde283ed14f vmaSetAllocationUserData()>.
--
-- == Detailed Description
--
-- Parameters of
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocation VmaAllocation>
-- objects, that can be retrieved using function
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1a86dd08aba8633bfa4ad0df2e76481d8b vmaGetAllocationInfo()>.
--
-- === deviceMemory
--
-- deviceMemory
-- VmaAllocationInfo
-- VmaAllocationInfo
-- deviceMemory
-- @VkDeviceMemory VmaAllocationInfo::deviceMemory@
--
-- Handle to Vulkan memory object.
--
-- Same memory object can be shared by multiple allocations.
--
-- It can change after call to
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1a9f0f8f56db5f7f57fe4454f465142dac vmaDefragment()>
-- if this allocation is passed to the function, or if allocation is lost.
--
-- If the allocation is lost, it is equal to @VK_NULL_HANDLE@.
--
-- === memoryType
--
-- memoryType
-- VmaAllocationInfo
-- VmaAllocationInfo
-- memoryType
-- @uint32_t VmaAllocationInfo::memoryType@
--
-- Memory type index that this allocation was allocated from.
--
-- It never changes.
--
-- === offset
--
-- offset
-- VmaAllocationInfo
-- VmaAllocationInfo
-- offset
-- @VkDeviceSize VmaAllocationInfo::offset@
--
-- Offset into deviceMemory object to the beginning of this allocation, in
-- bytes. (deviceMemory, offset) pair is unique to this allocation.
--
-- It can change after call to
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1a9f0f8f56db5f7f57fe4454f465142dac vmaDefragment()>
-- if this allocation is passed to the function, or if allocation is lost.
--
-- === pMappedData
--
-- pMappedData
-- VmaAllocationInfo
-- VmaAllocationInfo
-- pMappedData
-- @void* VmaAllocationInfo::pMappedData@
--
-- Pointer to the beginning of this allocation as mapped data.
--
-- If the allocation hasn\'t been mapped using
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1ad5bd1243512d099706de88168992f069 vmaMapMemory()>
-- and hasn\'t been created with
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1ad9889c10c798b040d59c92f257cae597a11da372cc3a82931c5e5d6146cd9dd1f VMA_ALLOCATION_CREATE_MAPPED_BIT>
-- flag, this value is null.
--
-- It can change after call to
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1ad5bd1243512d099706de88168992f069 vmaMapMemory()>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1a9bc268595cb33f6ec4d519cfce81ff45 vmaUnmapMemory()>.
-- It can also change after call to
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1a9f0f8f56db5f7f57fe4454f465142dac vmaDefragment()>
-- if this allocation is passed to the function.
--
-- === pUserData
--
-- pUserData
-- VmaAllocationInfo
-- VmaAllocationInfo
-- pUserData
-- @void* VmaAllocationInfo::pUserData@
--
-- Custom general-purpose pointer that was passed as
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocation_create_info_1a8259e85c272683434f4abb4ddddffe19 VmaAllocationCreateInfo::pUserData>
-- or set using
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1af9147d31ffc11d62fc187bde283ed14f vmaSetAllocationUserData()>.
--
-- It can change after call to
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1af9147d31ffc11d62fc187bde283ed14f vmaSetAllocationUserData()>
-- for this allocation.
--
-- === size
--
-- size
-- VmaAllocationInfo
-- VmaAllocationInfo
-- size
-- @VkDeviceSize VmaAllocationInfo::size@
--
-- Size of this allocation, in bytes.
--
-- It never changes, unless allocation is lost.
--
-- The documentation for this struct was generated from the following file:
--
-- vk_mem_alloc.h
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


-- | VmaAllocatorInfo
--
-- Information about existing
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocator VmaAllocator>
-- object.
--
-- -   VkInstance
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocator_info_1a2ed6a4d2d3fea039d66a13f15d0ce5fe instance>
--
--     Handle to Vulkan instance object.
--
-- -   VkPhysicalDevice
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocator_info_1aba2b703f96e51d567717e1fb2935b47a physicalDevice>
--
--     Handle to Vulkan physical device object.
--
-- -   VkDevice
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocator_info_1a012b4c485bf3b0ea8921352c5ee0c357 device>
--
--     Handle to Vulkan device object.
--
-- == Detailed Description
--
-- Information about existing
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocator VmaAllocator>
-- object.
--
-- === device
--
-- device
-- VmaAllocatorInfo
-- VmaAllocatorInfo
-- device
-- @VkDevice VmaAllocatorInfo::device@
--
-- Handle to Vulkan device object.
--
-- This is the same value as has been passed through
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocator_create_info_1ad924ddd77b04039c88d0c09b0ffcd500 VmaAllocatorCreateInfo::device>.
--
-- === instance
--
-- instance
-- VmaAllocatorInfo
-- VmaAllocatorInfo
-- instance
-- @VkInstance VmaAllocatorInfo::instance@
--
-- Handle to Vulkan instance object.
--
-- This is the same value as has been passed through
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocator_create_info_1a70dd42e29b1df1d1b9b61532ae0b370b VmaAllocatorCreateInfo::instance>.
--
-- === physicalDevice
--
-- physicalDevice
-- VmaAllocatorInfo
-- VmaAllocatorInfo
-- physicalDevice
-- @VkPhysicalDevice VmaAllocatorInfo::physicalDevice@
--
-- Handle to Vulkan physical device object.
--
-- This is the same value as has been passed through
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocator_create_info_1a08230f04ae6ccf8a78150a9e829a7156 VmaAllocatorCreateInfo::physicalDevice>.
--
-- The documentation for this struct was generated from the following file:
--
-- vk_mem_alloc.h
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


-- | VmaVulkanFunctions
--
-- Pointers to some Vulkan functions - a subset used by the library.
--
-- -   PFN_vkGetPhysicalDeviceProperties
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_vulkan_functions_1a77b7a74082823e865dd6546623468f96 vkGetPhysicalDeviceProperties>
--
-- -   PFN_vkGetPhysicalDeviceMemoryProperties
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_vulkan_functions_1a60d25c33bba06bb8592e6875cbaa9830 vkGetPhysicalDeviceMemoryProperties>
--
-- -   PFN_vkAllocateMemory
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_vulkan_functions_1a2943bf99dfd784a0e8f599d987e22e6c vkAllocateMemory>
--
-- -   PFN_vkFreeMemory
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_vulkan_functions_1a4c658701778564d62034255b5dda91b4 vkFreeMemory>
--
-- -   PFN_vkMapMemory
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_vulkan_functions_1ab5c1f38dea3a2cf00dc9eb4f57218c49 vkMapMemory>
--
-- -   PFN_vkUnmapMemory
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_vulkan_functions_1acc798589736f0becb317fc2196c1d8b9 vkUnmapMemory>
--
-- -   PFN_vkFlushMappedMemoryRanges
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_vulkan_functions_1a33c322f4c4ad2810f8a9c97a277572f9 vkFlushMappedMemoryRanges>
--
-- -   PFN_vkInvalidateMappedMemoryRanges
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_vulkan_functions_1a5c1093bc32386a8060c37c9f282078a1 vkInvalidateMappedMemoryRanges>
--
-- -   PFN_vkBindBufferMemory
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_vulkan_functions_1a94fc4f3a605d9880bb3c0ba2c2fc80b2 vkBindBufferMemory>
--
-- -   PFN_vkBindImageMemory
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_vulkan_functions_1a1338d96a128a5ade648b8d934907c637 vkBindImageMemory>
--
-- -   PFN_vkGetBufferMemoryRequirements
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_vulkan_functions_1a5b92901df89a4194b0d12f6071d4d143 vkGetBufferMemoryRequirements>
--
-- -   PFN_vkGetImageMemoryRequirements
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_vulkan_functions_1a475f6f49f8debe4d10800592606d53f4 vkGetImageMemoryRequirements>
--
-- -   PFN_vkCreateBuffer
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_vulkan_functions_1ae8084315a25006271a2edfc3a447519f vkCreateBuffer>
--
-- -   PFN_vkDestroyBuffer
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_vulkan_functions_1a7e054606faddb07f0e8556f3ed317d45 vkDestroyBuffer>
--
-- -   PFN_vkCreateImage
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_vulkan_functions_1a23ebe70be515b9b5010a1d691200e325 vkCreateImage>
--
-- -   PFN_vkDestroyImage
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_vulkan_functions_1a90b898227039b1dcb3520f6e91f09ffa vkDestroyImage>
--
-- -   PFN_vkCmdCopyBuffer
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_vulkan_functions_1ae5c0db8c89a3b82593dc16aa6a49fa3a vkCmdCopyBuffer>
--
-- == Detailed Description
--
-- Pointers to some Vulkan functions - a subset used by the library.
--
-- Used in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocator_create_info_1a3dc197be3227da7338b1643f70db36bd VmaAllocatorCreateInfo::pVulkanFunctions>.
--
-- === vkAllocateMemory
--
-- vkAllocateMemory
-- VmaVulkanFunctions
-- VmaVulkanFunctions
-- vkAllocateMemory
-- @PFN_vkAllocateMemory VmaVulkanFunctions::vkAllocateMemory@
--
-- === vkBindBufferMemory
--
-- vkBindBufferMemory
-- VmaVulkanFunctions
-- VmaVulkanFunctions
-- vkBindBufferMemory
-- @PFN_vkBindBufferMemory VmaVulkanFunctions::vkBindBufferMemory@
--
-- === vkBindImageMemory
--
-- vkBindImageMemory
-- VmaVulkanFunctions
-- VmaVulkanFunctions
-- vkBindImageMemory
-- @PFN_vkBindImageMemory VmaVulkanFunctions::vkBindImageMemory@
--
-- === vkCmdCopyBuffer
--
-- vkCmdCopyBuffer
-- VmaVulkanFunctions
-- VmaVulkanFunctions
-- vkCmdCopyBuffer
-- @PFN_vkCmdCopyBuffer VmaVulkanFunctions::vkCmdCopyBuffer@
--
-- === vkCreateBuffer
--
-- vkCreateBuffer
-- VmaVulkanFunctions
-- VmaVulkanFunctions
-- vkCreateBuffer
-- @PFN_vkCreateBuffer VmaVulkanFunctions::vkCreateBuffer@
--
-- === vkCreateImage
--
-- vkCreateImage
-- VmaVulkanFunctions
-- VmaVulkanFunctions
-- vkCreateImage
-- @PFN_vkCreateImage VmaVulkanFunctions::vkCreateImage@
--
-- === vkDestroyBuffer
--
-- vkDestroyBuffer
-- VmaVulkanFunctions
-- VmaVulkanFunctions
-- vkDestroyBuffer
-- @PFN_vkDestroyBuffer VmaVulkanFunctions::vkDestroyBuffer@
--
-- === vkDestroyImage
--
-- vkDestroyImage
-- VmaVulkanFunctions
-- VmaVulkanFunctions
-- vkDestroyImage
-- @PFN_vkDestroyImage VmaVulkanFunctions::vkDestroyImage@
--
-- === vkFlushMappedMemoryRanges
--
-- vkFlushMappedMemoryRanges
-- VmaVulkanFunctions
-- VmaVulkanFunctions
-- vkFlushMappedMemoryRanges
-- @PFN_vkFlushMappedMemoryRanges VmaVulkanFunctions::vkFlushMappedMemoryRanges@
--
-- === vkFreeMemory
--
-- vkFreeMemory
-- VmaVulkanFunctions
-- VmaVulkanFunctions
-- vkFreeMemory
-- @PFN_vkFreeMemory VmaVulkanFunctions::vkFreeMemory@
--
-- === vkGetBufferMemoryRequirements
--
-- vkGetBufferMemoryRequirements
-- VmaVulkanFunctions
-- VmaVulkanFunctions
-- vkGetBufferMemoryRequirements
-- @PFN_vkGetBufferMemoryRequirements VmaVulkanFunctions::vkGetBufferMemoryRequirements@
--
-- === vkGetImageMemoryRequirements
--
-- vkGetImageMemoryRequirements
-- VmaVulkanFunctions
-- VmaVulkanFunctions
-- vkGetImageMemoryRequirements
-- @PFN_vkGetImageMemoryRequirements VmaVulkanFunctions::vkGetImageMemoryRequirements@
--
-- === vkGetPhysicalDeviceMemoryProperties
--
-- vkGetPhysicalDeviceMemoryProperties
-- VmaVulkanFunctions
-- VmaVulkanFunctions
-- vkGetPhysicalDeviceMemoryProperties
-- @PFN_vkGetPhysicalDeviceMemoryProperties VmaVulkanFunctions::vkGetPhysicalDeviceMemoryProperties@
--
-- === vkGetPhysicalDeviceProperties
--
-- vkGetPhysicalDeviceProperties
-- VmaVulkanFunctions
-- VmaVulkanFunctions
-- vkGetPhysicalDeviceProperties
-- @PFN_vkGetPhysicalDeviceProperties VmaVulkanFunctions::vkGetPhysicalDeviceProperties@
--
-- === vkInvalidateMappedMemoryRanges
--
-- vkInvalidateMappedMemoryRanges
-- VmaVulkanFunctions
-- VmaVulkanFunctions
-- vkInvalidateMappedMemoryRanges
-- @PFN_vkInvalidateMappedMemoryRanges VmaVulkanFunctions::vkInvalidateMappedMemoryRanges@
--
-- === vkMapMemory
--
-- vkMapMemory
-- VmaVulkanFunctions
-- VmaVulkanFunctions
-- vkMapMemory
-- @PFN_vkMapMemory VmaVulkanFunctions::vkMapMemory@
--
-- === vkUnmapMemory
--
-- vkUnmapMemory
-- VmaVulkanFunctions
-- VmaVulkanFunctions
-- vkUnmapMemory
-- @PFN_vkUnmapMemory VmaVulkanFunctions::vkUnmapMemory@
--
-- The documentation for this struct was generated from the following file:
--
-- vk_mem_alloc.h
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


-- | VmaPoolStats
--
-- Describes parameter of existing
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_pool VmaPool>.
--
-- -   VkDeviceSize
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_pool_stats_1a326807b2de2b0931cee4ed9a5f2e420c size>
--
--     Total amount of @VkDeviceMemory@ allocated from Vulkan for this
--     pool, in bytes.
--
-- -   VkDeviceSize
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_pool_stats_1ad7c54874724fce7b06aba526202d82a8 unusedSize>
--
--     Total number of bytes in the pool not used by any
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocation VmaAllocation>.
--
-- -   size_t
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_pool_stats_1ad1924eb54fffa45e9e0e65670c8fe5eb allocationCount>
--
--     Number of
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocation VmaAllocation>
--     objects created from this pool that were not destroyed or lost.
--
-- -   size_t
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_pool_stats_1ae4f3546ffa4d1e598b64d8e6134854f4 unusedRangeCount>
--
--     Number of continuous memory ranges in the pool not used by any
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocation VmaAllocation>.
--
-- -   VkDeviceSize
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_pool_stats_1ab4c8f52dd42ab01998f60f0b6acc722b unusedRangeSizeMax>
--
--     Size of the largest continuous free memory region available for new
--     allocation.
--
-- -   size_t
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_pool_stats_1aa0b5cb45cef6f18571cefb03b9a230e7 blockCount>
--
--     Number of @VkDeviceMemory@ blocks allocated for this pool.
--
-- == Detailed Description
--
-- Describes parameter of existing
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_pool VmaPool>.
--
-- === allocationCount
--
-- allocationCount
-- VmaPoolStats
-- VmaPoolStats
-- allocationCount
-- @size_t VmaPoolStats::allocationCount@
--
-- Number of
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocation VmaAllocation>
-- objects created from this pool that were not destroyed or lost.
--
-- === blockCount
--
-- blockCount
-- VmaPoolStats
-- VmaPoolStats
-- blockCount
-- @size_t VmaPoolStats::blockCount@
--
-- Number of @VkDeviceMemory@ blocks allocated for this pool.
--
-- === size
--
-- size
-- VmaPoolStats
-- VmaPoolStats
-- size
-- @VkDeviceSize VmaPoolStats::size@
--
-- Total amount of @VkDeviceMemory@ allocated from Vulkan for this pool, in
-- bytes.
--
-- === unusedRangeCount
--
-- unusedRangeCount
-- VmaPoolStats
-- VmaPoolStats
-- unusedRangeCount
-- @size_t VmaPoolStats::unusedRangeCount@
--
-- Number of continuous memory ranges in the pool not used by any
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocation VmaAllocation>.
--
-- === unusedRangeSizeMax
--
-- unusedRangeSizeMax
-- VmaPoolStats
-- VmaPoolStats
-- unusedRangeSizeMax
-- @VkDeviceSize VmaPoolStats::unusedRangeSizeMax@
--
-- Size of the largest continuous free memory region available for new
-- allocation.
--
-- Making a new allocation of that size is not guaranteed to succeed
-- because of possible additional margin required to respect alignment and
-- buffer\/image granularity.
--
-- === unusedSize
--
-- unusedSize
-- VmaPoolStats
-- VmaPoolStats
-- unusedSize
-- @VkDeviceSize VmaPoolStats::unusedSize@
--
-- Total number of bytes in the pool not used by any
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocation VmaAllocation>.
--
-- The documentation for this struct was generated from the following file:
--
-- vk_mem_alloc.h
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


-- | VmaStatInfo
--
-- Calculated statistics of memory usage in entire allocator.
--
-- -   uint32_t
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_stat_info_1abc4bb7cd611900778464c56e50c970a4 blockCount>
--
--     Number of @VkDeviceMemory@ Vulkan memory blocks allocated.
--
-- -   uint32_t
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_stat_info_1a537741e4d5cdddc1c0ab95ec650afaff allocationCount>
--
--     Number of
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocation VmaAllocation>
--     allocation objects allocated.
--
-- -   uint32_t
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_stat_info_1ae06129c771bfebfd6468a7f4276502a9 unusedRangeCount>
--
--     Number of free ranges of memory between allocations.
--
-- -   VkDeviceSize
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_stat_info_1ab0c6c73837e5a70c749fbd4f6064895a usedBytes>
--
--     Total number of bytes occupied by all allocations.
--
-- -   VkDeviceSize
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_stat_info_1a1859d290aca2cd582d8dc25922092669 unusedBytes>
--
--     Total number of bytes occupied by unused ranges.
--
-- -   VkDeviceSize
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_stat_info_1ade8b40bd3139c04aabd2fc538a356fea allocationSizeMin>
--
-- -   VkDeviceSize
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_stat_info_1a1081a039964e566c672e7a2347f9e599 allocationSizeAvg>
--
-- -   VkDeviceSize
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_stat_info_1a17e9733a5ecd76287d4db6e66f71f50c allocationSizeMax>
--
-- -   VkDeviceSize
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_stat_info_1aedeba931324f16589cd2416c0d2dd0d4 unusedRangeSizeMin>
--
-- -   VkDeviceSize
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_stat_info_1a2f9b3452af90c9768a30b7fb6ae194fc unusedRangeSizeAvg>
--
-- -   VkDeviceSize
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_stat_info_1a5ba1a2476c4d39b10f7e2f7ebbb72ac4 unusedRangeSizeMax>
--
-- == Detailed Description
--
-- Calculated statistics of memory usage in entire allocator.
--
-- === allocationCount
--
-- allocationCount
-- VmaStatInfo
-- VmaStatInfo
-- allocationCount
-- @uint32_t VmaStatInfo::allocationCount@
--
-- Number of
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocation VmaAllocation>
-- allocation objects allocated.
--
-- === allocationSizeAvg
--
-- allocationSizeAvg
-- VmaStatInfo
-- VmaStatInfo
-- allocationSizeAvg
-- @VkDeviceSize VmaStatInfo::allocationSizeAvg@
--
-- === allocationSizeMax
--
-- allocationSizeMax
-- VmaStatInfo
-- VmaStatInfo
-- allocationSizeMax
-- @VkDeviceSize VmaStatInfo::allocationSizeMax@
--
-- === allocationSizeMin
--
-- allocationSizeMin
-- VmaStatInfo
-- VmaStatInfo
-- allocationSizeMin
-- @VkDeviceSize VmaStatInfo::allocationSizeMin@
--
-- === blockCount
--
-- blockCount
-- VmaStatInfo
-- VmaStatInfo
-- blockCount
-- @uint32_t VmaStatInfo::blockCount@
--
-- Number of @VkDeviceMemory@ Vulkan memory blocks allocated.
--
-- === unusedBytes
--
-- unusedBytes
-- VmaStatInfo
-- VmaStatInfo
-- unusedBytes
-- @VkDeviceSize VmaStatInfo::unusedBytes@
--
-- Total number of bytes occupied by unused ranges.
--
-- === unusedRangeCount
--
-- unusedRangeCount
-- VmaStatInfo
-- VmaStatInfo
-- unusedRangeCount
-- @uint32_t VmaStatInfo::unusedRangeCount@
--
-- Number of free ranges of memory between allocations.
--
-- === unusedRangeSizeAvg
--
-- unusedRangeSizeAvg
-- VmaStatInfo
-- VmaStatInfo
-- unusedRangeSizeAvg
-- @VkDeviceSize VmaStatInfo::unusedRangeSizeAvg@
--
-- === unusedRangeSizeMax
--
-- unusedRangeSizeMax
-- VmaStatInfo
-- VmaStatInfo
-- unusedRangeSizeMax
-- @VkDeviceSize VmaStatInfo::unusedRangeSizeMax@
--
-- === unusedRangeSizeMin
--
-- unusedRangeSizeMin
-- VmaStatInfo
-- VmaStatInfo
-- unusedRangeSizeMin
-- @VkDeviceSize VmaStatInfo::unusedRangeSizeMin@
--
-- === usedBytes
--
-- usedBytes
-- VmaStatInfo
-- VmaStatInfo
-- usedBytes
-- @VkDeviceSize VmaStatInfo::usedBytes@
--
-- Total number of bytes occupied by all allocations.
--
-- The documentation for this struct was generated from the following file:
--
-- vk_mem_alloc.h
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


-- | VmaRecordSettings
--
-- Parameters for recording calls to VMA functions. To be used in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocator_create_info_1ace2aa4877b16a42b0b7673d4e26000ee VmaAllocatorCreateInfo::pRecordSettings>.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1af3929a1a4547c592fc0b0e55ef452828 VmaRecordFlags>
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_record_settings_1ad8fdcc92119ae7a8c08c1a564c01d63a flags>
--
--     Flags for recording. Use
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1acd24d5eb58abff7e1f43cb32a1ba1413 VmaRecordFlagBits>
--     enum.
--
-- -   const char *
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_record_settings_1a6cb1fdbf6bcb610b68f2010dd629e89d pFilePath>
--
--     Path to the file that should be written by the recording.
--
-- == Detailed Description
--
-- Parameters for recording calls to VMA functions. To be used in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocator_create_info_1ace2aa4877b16a42b0b7673d4e26000ee VmaAllocatorCreateInfo::pRecordSettings>.
--
-- === flags
--
-- flags
-- VmaRecordSettings
-- VmaRecordSettings
-- flags
-- @VmaRecordFlags VmaRecordSettings::flags@
--
-- Flags for recording. Use
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1acd24d5eb58abff7e1f43cb32a1ba1413 VmaRecordFlagBits>
-- enum.
--
-- === pFilePath
--
-- pFilePath
-- VmaRecordSettings
-- VmaRecordSettings
-- pFilePath
-- @const char* VmaRecordSettings::pFilePath@
--
-- Path to the file that should be written by the recording.
--
-- Suggested extension: \"csv\". If the file already exists, it will be
-- overwritten. It will be opened for the whole time
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocator VmaAllocator>
-- object is alive. If opening this file fails, creation of the whole
-- allocator object fails.
--
-- The documentation for this struct was generated from the following file:
--
-- vk_mem_alloc.h
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


-- | VmaBudget
--
-- Statistics of current memory usage and available budget, in bytes, for
-- specific memory heap.
--
-- -   VkDeviceSize
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_budget_1a58b492901baab685f466199124e514a0 blockBytes>
--
--     Sum size of all @VkDeviceMemory@ blocks allocated from particular
--     heap, in bytes.
--
-- -   VkDeviceSize
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_budget_1a7e2a6583ebd63e194951c542563804d8 allocationBytes>
--
--     Sum size of all allocations created in particular heap, in bytes.
--
-- -   VkDeviceSize
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_budget_1a84dd1ecca8b0110259eb206dbadb11f6 usage>
--
--     Estimated current memory usage of the program, in bytes.
--
-- -   VkDeviceSize
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_budget_1ab82e1d1754c2d210d0bdf90220bc6cdd budget>
--
--     Estimated amount of memory available to the program, in bytes.
--
-- == Detailed Description
--
-- Statistics of current memory usage and available budget, in bytes, for
-- specific memory heap.
--
-- === allocationBytes
--
-- allocationBytes
-- VmaBudget
-- VmaBudget
-- allocationBytes
-- @VkDeviceSize VmaBudget::allocationBytes@
--
-- Sum size of all allocations created in particular heap, in bytes.
--
-- Usually less or equal than @blockBytes@. Difference
-- @blockBytes - allocationBytes@ is the amount of memory allocated but
-- unused - available for new allocations or wasted due to fragmentation.
--
-- It might be greater than @blockBytes@ if there are some allocations in
-- lost state, as they account to this value as well.
--
-- === blockBytes
--
-- blockBytes
-- VmaBudget
-- VmaBudget
-- blockBytes
-- @VkDeviceSize VmaBudget::blockBytes@
--
-- Sum size of all @VkDeviceMemory@ blocks allocated from particular heap,
-- in bytes.
--
-- === budget
--
-- budget
-- VmaBudget
-- VmaBudget
-- budget
-- @VkDeviceSize VmaBudget::budget@
--
-- Estimated amount of memory available to the program, in bytes.
--
-- Fetched from system using @VK_EXT_memory_budget@ extension if enabled.
--
-- It might be different (most probably smaller) than
-- @VkMemoryHeap::size[heapIndex]@ due to factors external to the program,
-- like other programs also consuming system resources. Difference
-- @budget - usage@ is the amount of additional memory that can probably be
-- allocated without problems. Exceeding the budget may result in various
-- problems.
--
-- === usage
--
-- usage
-- VmaBudget
-- VmaBudget
-- usage
-- @VkDeviceSize VmaBudget::usage@
--
-- Estimated current memory usage of the program, in bytes.
--
-- Fetched from system using @VK_EXT_memory_budget@ extension if enabled.
--
-- It might be different than @blockBytes@ (usually higher) due to
-- additional implicit objects also occupying the memory, like swapchain,
-- pipelines, descriptor heaps, command buffers, or @VkDeviceMemory@ blocks
-- allocated outside of this library, if any.
--
-- The documentation for this struct was generated from the following file:
--
-- vk_mem_alloc.h
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


-- | VmaDeviceMemoryCallbacks
--
-- Set of callbacks that the library will call for @vkAllocateMemory@ and
-- @vkFreeMemory@.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1a7e1ed85f7799600b03ad51a77acc21f3 PFN_vmaAllocateDeviceMemoryFunction>
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_device_memory_callbacks_1a4f17f7b255101e733b44d5633aceabfb pfnAllocate>
--
--     Optional, can be null.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1a154ccaaf53dc2c36378f80f0c4f3679b PFN_vmaFreeDeviceMemoryFunction>
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_device_memory_callbacks_1abe8a3328bbc916f6f712fdb6b299444c pfnFree>
--
--     Optional, can be null.
--
-- -   void *
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_device_memory_callbacks_1a24052de0937ddd54015a2df0363903c6 pUserData>
--
--     Optional, can be null.
--
-- == Detailed Description
--
-- Set of callbacks that the library will call for @vkAllocateMemory@ and
-- @vkFreeMemory@.
--
-- Provided for informative purpose, e.g. to gather statistics about number
-- of allocations or total amount of memory allocated in Vulkan.
--
-- Used in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocator_create_info_1af1380969b5e1ea4c3184a877892d260e VmaAllocatorCreateInfo::pDeviceMemoryCallbacks>.
--
-- === pfnAllocate
--
-- pfnAllocate
-- VmaDeviceMemoryCallbacks
-- VmaDeviceMemoryCallbacks
-- pfnAllocate
-- @PFN_vmaAllocateDeviceMemoryFunction VmaDeviceMemoryCallbacks::pfnAllocate@
--
-- Optional, can be null.
--
-- === pfnFree
--
-- pfnFree
-- VmaDeviceMemoryCallbacks
-- VmaDeviceMemoryCallbacks
-- pfnFree
-- @PFN_vmaFreeDeviceMemoryFunction VmaDeviceMemoryCallbacks::pfnFree@
--
-- Optional, can be null.
--
-- === pUserData
--
-- pUserData
-- VmaDeviceMemoryCallbacks
-- VmaDeviceMemoryCallbacks
-- pUserData
-- @void* VmaDeviceMemoryCallbacks::pUserData@
--
-- Optional, can be null.
--
-- The documentation for this struct was generated from the following file:
--
-- vk_mem_alloc.h
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


-- | VmaDefragmentationPassMoveInfo
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocation VmaAllocation>
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_defragmentation_pass_move_info_1ae885c861c2dd8d622e6c19e281d035cc allocation>
--
-- -   VkDeviceMemory
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_defragmentation_pass_move_info_1a06eb0c8690aa0d3478a036753492e769 memory>
--
-- -   VkDeviceSize
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_defragmentation_pass_move_info_1a8ab4508bc03625b0653c880576be96c6 offset>
--
-- === allocation
--
-- allocation
-- VmaDefragmentationPassMoveInfo
-- VmaDefragmentationPassMoveInfo
-- allocation
-- @VmaAllocation VmaDefragmentationPassMoveInfo::allocation@
--
-- === memory
--
-- memory
-- VmaDefragmentationPassMoveInfo
-- VmaDefragmentationPassMoveInfo
-- memory
-- @VkDeviceMemory VmaDefragmentationPassMoveInfo::memory@
--
-- === offset
--
-- offset
-- VmaDefragmentationPassMoveInfo
-- VmaDefragmentationPassMoveInfo
-- offset
-- @VkDeviceSize VmaDefragmentationPassMoveInfo::offset@
--
-- The documentation for this struct was generated from the following file:
--
-- vk_mem_alloc.h
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


-- | VmaStats
--
-- General statistics from current state of Allocator.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_stat_info VmaStatInfo>
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_stats_1a13e3caf754be79352c42408756309331 memoryType>
--     [VK_MAX_MEMORY_TYPES]
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_stat_info VmaStatInfo>
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_stats_1a0e6611508c29a187f0fd14ff1a0329c0 memoryHeap>
--     [VK_MAX_MEMORY_HEAPS]
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_stat_info VmaStatInfo>
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_stats_1a2e8f5b3353f2fefef3c27f29e245a1f9 total>
--
-- == Detailed Description
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
--
-- === total
--
-- total
-- VmaStats
-- VmaStats
-- total
-- @VmaStatInfo VmaStats::total@
--
-- The documentation for this struct was generated from the following file:
--
-- vk_mem_alloc.h
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


-- | VmaPool
--
-- Represents custom memory pool.
--
-- == Detailed Description
--
-- Represents custom memory pool.
--
-- Fill structure
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_pool_create_info VmaPoolCreateInfo>
-- and call function
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1a5c8770ded7c59c8caac6de0c2cb00b50 vmaCreatePool()>
-- to create it. Call function
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1a5485779c8f1948238fc4e92232fa65e1 vmaDestroyPool()>
-- to destroy it.
--
-- For more information see
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_choosing_memory_type_1choosing_memory_type_custom_memory_pools Custom memory pools>.
--
-- The documentation for this struct was generated from the following file:
--
-- vk_mem_alloc.h
newtype Pool = Pool Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show Pool where
  showsPrec p (Pool x) = showParen (p >= 11) (showString "Pool 0x" . showHex x)


-- | VmaDefragmentationContext
--
-- Represents Opaque object that represents started defragmentation
-- process.
--
-- == Detailed Description
--
-- Represents Opaque object that represents started defragmentation
-- process.
--
-- Fill structure
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_defragmentation_info2 VmaDefragmentationInfo2>
-- and call function
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1a36ba776fd7fd5cb1e9359fdc0d8e6e8a vmaDefragmentationBegin()>
-- to create it. Call function
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1a8774e20e91e245aae959ba63efa15dd2 vmaDefragmentationEnd()>
-- to destroy it.
--
-- The documentation for this struct was generated from the following file:
--
-- vk_mem_alloc.h
newtype DefragmentationContext = DefragmentationContext Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show DefragmentationContext where
  showsPrec p (DefragmentationContext x) = showParen (p >= 11) (showString "DefragmentationContext 0x" . showHex x)


-- | VmaAllocation
--
-- Represents single memory allocation.
--
-- == Detailed Description
--
-- Represents single memory allocation.
--
-- It may be either dedicated block of @VkDeviceMemory@ or a specific
-- region of a bigger block of this type plus unique offset.
--
-- There are multiple ways to create such object. You need to fill
-- structure
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocation_create_info VmaAllocationCreateInfo>.
-- For more information see
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_choosing_memory_type Choosing memory type>.
--
-- Although the library provides convenience functions that create Vulkan
-- buffer or image, allocate memory for it and bind them together, binding
-- of the allocation to a buffer or an image is out of scope of the
-- allocation itself. Allocation object can exist without buffer\/image
-- bound, binding can be done manually by the user, and destruction of it
-- can be done independently of destruction of the allocation.
--
-- The object also remembers its size and some other information. To
-- retrieve this information, use function
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1a86dd08aba8633bfa4ad0df2e76481d8b vmaGetAllocationInfo()>
-- and inspect returned structure
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocation_info VmaAllocationInfo>.
--
-- Some kinds allocations can be in lost state. For more information, see
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_lost_allocations Lost allocations>.
--
-- The documentation for this struct was generated from the following file:
--
-- vk_mem_alloc.h
newtype Allocation = Allocation Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show Allocation where
  showsPrec p (Allocation x) = showParen (p >= 11) (showString "Allocation 0x" . showHex x)


-- | VmaAllocator
--
-- Represents main object of this library initialized.
--
-- == Detailed Description
--
-- Represents main object of this library initialized.
--
-- Fill structure
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_struct_vma_allocator_create_info VmaAllocatorCreateInfo>
-- and call function
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1a200692051ddb34240248234f5f4c17bb vmaCreateAllocator()>
-- to create it. Call function
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#_vk__mem__alloc_8h_1aa8d164061c88f22fb1fd3c8f3534bc1d vmaDestroyAllocator()>
-- to destroy it.
--
-- It is recommended to create just one object of this type per @VkDevice@
-- object, right after Vulkan is initialized and keep it alive until before
-- Vulkan device is destroyed.
--
-- The documentation for this struct was generated from the following file:
--
-- vk_mem_alloc.h
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

