{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Memory
  ( DeviceMemory
#if defined(VK_USE_PLATFORM_GGP)
  , MappedMemoryRange(..)
  , MemoryAllocateInfo(..)
#endif
  , MemoryMapFlags
  , allocateMemory
  , flushMappedMemoryRanges
  , freeMemory
  , getDeviceMemoryCommitment
  , invalidateMappedMemoryRanges
  , mapMemory
  , unmapMemory
  , withMappedMemory
  , withMemory
  ) where

import Control.Exception
  ( bracket
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( length
  )

#if defined(VK_USE_PLATFORM_GGP)
import Data.Word
  ( Word32
  )
#endif
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybeWith
  , with
  )
import Foreign.Ptr
  ( Ptr
  )
import Foreign.Storable
  ( peek
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Core10.Memory
  ( VkMemoryMapFlags(..)
  , VkDeviceMemory
  , vkAllocateMemory
  , vkFlushMappedMemoryRanges
  , vkFreeMemory
  , vkGetDeviceMemoryCommitment
  , vkInvalidateMappedMemoryRanges
  , vkMapMemory
  , vkUnmapMemory
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , DeviceSize
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif


-- No documentation found for TopLevel "DeviceMemory"
type DeviceMemory = VkDeviceMemory


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkMappedMemoryRange"
data MappedMemoryRange = MappedMemoryRange
  { -- No documentation found for Nested "MappedMemoryRange" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MappedMemoryRange" "memory"
  memory :: DeviceMemory
  , -- No documentation found for Nested "MappedMemoryRange" "offset"
  offset :: DeviceSize
  , -- No documentation found for Nested "MappedMemoryRange" "size"
  size :: DeviceSize
  }
  deriving (Show, Eq)

instance Zero MappedMemoryRange where
  zero = MappedMemoryRange Nothing
                           zero
                           zero
                           zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkMemoryAllocateInfo"
data MemoryAllocateInfo = MemoryAllocateInfo
  { -- No documentation found for Nested "MemoryAllocateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MemoryAllocateInfo" "allocationSize"
  allocationSize :: DeviceSize
  , -- No documentation found for Nested "MemoryAllocateInfo" "memoryTypeIndex"
  memoryTypeIndex :: Word32
  }
  deriving (Show, Eq)

instance Zero MemoryAllocateInfo where
  zero = MemoryAllocateInfo Nothing
                            zero
                            zero

#endif

-- No documentation found for TopLevel "MemoryMapFlags"
type MemoryMapFlags = VkMemoryMapFlags


-- No complete pragma for MemoryMapFlags as it has no patterns


-- No documentation found for TopLevel "vkAllocateMemory"
allocateMemory :: Device ->  MemoryAllocateInfo ->  Maybe AllocationCallbacks ->  IO (DeviceMemory)
allocateMemory = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkFlushMappedMemoryRanges"
flushMappedMemoryRanges :: Device ->  Vector MappedMemoryRange ->  IO ()
flushMappedMemoryRanges = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkFreeMemory"
freeMemory :: Device ->  DeviceMemory ->  Maybe AllocationCallbacks ->  IO ()
freeMemory = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkGetDeviceMemoryCommitment"
getDeviceMemoryCommitment :: Device ->  DeviceMemory ->  IO (DeviceSize)
getDeviceMemoryCommitment = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkInvalidateMappedMemoryRanges"
invalidateMappedMemoryRanges :: Device ->  Vector MappedMemoryRange ->  IO ()
invalidateMappedMemoryRanges = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkMapMemory"
mapMemory :: Device ->  DeviceMemory ->  DeviceSize ->  DeviceSize ->  MemoryMapFlags ->  IO (Ptr ())
mapMemory = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkUnmapMemory"
unmapMemory :: Device ->  DeviceMemory ->  IO ()
unmapMemory = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- | A safe wrapper for 'mapMemory' and 'unmapMemory' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withMappedMemory
  :: Device -> DeviceMemory -> DeviceSize -> DeviceSize -> MemoryMapFlags -> (Ptr () -> IO a) -> IO a
withMappedMemory device deviceMemory offset' size' flags' = bracket
  (mapMemory device deviceMemory offset' size' flags')
  (\_ -> unmapMemory device deviceMemory)

-- | A safe wrapper for 'allocateMemory' and 'freeMemory' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withMemory
  :: Device -> MemoryAllocateInfo -> Maybe AllocationCallbacks -> (DeviceMemory -> IO a) -> IO a
withMemory device memoryAllocateInfo allocationCallbacks = bracket
  (allocateMemory device memoryAllocateInfo allocationCallbacks)
  (\o -> freeMemory device o allocationCallbacks)
