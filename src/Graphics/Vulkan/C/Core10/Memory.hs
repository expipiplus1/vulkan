{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.Memory
  ( VkDeviceMemory
  , VkMappedMemoryRange(..)
  , VkMemoryAllocateInfo(..)
  , VkMemoryMapFlags(..)
  , FN_vkAllocateMemory
  , PFN_vkAllocateMemory
  , vkAllocateMemory
  , FN_vkFlushMappedMemoryRanges
  , PFN_vkFlushMappedMemoryRanges
  , vkFlushMappedMemoryRanges
  , FN_vkFreeMemory
  , PFN_vkFreeMemory
  , vkFreeMemory
  , FN_vkGetDeviceMemoryCommitment
  , PFN_vkGetDeviceMemoryCommitment
  , vkGetDeviceMemoryCommitment
  , FN_vkInvalidateMappedMemoryRanges
  , PFN_vkInvalidateMappedMemoryRanges
  , vkInvalidateMappedMemoryRanges
  , FN_vkMapMemory
  , PFN_vkMapMemory
  , vkMapMemory
  , FN_vkUnmapMemory
  , PFN_vkUnmapMemory
  , vkUnmapMemory
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import GHC.Read
  ( choose
  , expectP
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  , VkFlags
  , pattern VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE
  , pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  , VkDeviceSize
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | Dummy data to tag the 'Ptr' with
data VkDeviceMemory_T
-- No documentation found for TopLevel "VkDeviceMemory"
type VkDeviceMemory = Ptr VkDeviceMemory_T

-- No documentation found for TopLevel "VkMappedMemoryRange"
data VkMappedMemoryRange = VkMappedMemoryRange
  { -- No documentation found for Nested "VkMappedMemoryRange" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkMappedMemoryRange" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkMappedMemoryRange" "memory"
  vkMemory :: VkDeviceMemory
  , -- No documentation found for Nested "VkMappedMemoryRange" "offset"
  vkOffset :: VkDeviceSize
  , -- No documentation found for Nested "VkMappedMemoryRange" "size"
  vkSize :: VkDeviceSize
  }
  deriving (Eq, Show)

instance Storable VkMappedMemoryRange where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkMappedMemoryRange <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 8)
                                 <*> peek (ptr `plusPtr` 16)
                                 <*> peek (ptr `plusPtr` 24)
                                 <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMappedMemoryRange))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMappedMemoryRange))
                *> poke (ptr `plusPtr` 16) (vkMemory (poked :: VkMappedMemoryRange))
                *> poke (ptr `plusPtr` 24) (vkOffset (poked :: VkMappedMemoryRange))
                *> poke (ptr `plusPtr` 32) (vkSize (poked :: VkMappedMemoryRange))

instance Zero VkMappedMemoryRange where
  zero = VkMappedMemoryRange VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE
                             zero
                             zero
                             zero
                             zero

-- No documentation found for TopLevel "VkMemoryAllocateInfo"
data VkMemoryAllocateInfo = VkMemoryAllocateInfo
  { -- No documentation found for Nested "VkMemoryAllocateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkMemoryAllocateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkMemoryAllocateInfo" "allocationSize"
  vkAllocationSize :: VkDeviceSize
  , -- No documentation found for Nested "VkMemoryAllocateInfo" "memoryTypeIndex"
  vkMemoryTypeIndex :: Word32
  }
  deriving (Eq, Show)

instance Storable VkMemoryAllocateInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkMemoryAllocateInfo <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
                                  <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMemoryAllocateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMemoryAllocateInfo))
                *> poke (ptr `plusPtr` 16) (vkAllocationSize (poked :: VkMemoryAllocateInfo))
                *> poke (ptr `plusPtr` 24) (vkMemoryTypeIndex (poked :: VkMemoryAllocateInfo))

instance Zero VkMemoryAllocateInfo where
  zero = VkMemoryAllocateInfo VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
                              zero
                              zero
                              zero

-- ** VkMemoryMapFlags

-- No documentation found for TopLevel "VkMemoryMapFlags"
newtype VkMemoryMapFlags = VkMemoryMapFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkMemoryMapFlags where
  
  showsPrec p (VkMemoryMapFlags x) = showParen (p >= 11) (showString "VkMemoryMapFlags " . showsPrec 11 x)

instance Read VkMemoryMapFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkMemoryMapFlags")
                        v <- step readPrec
                        pure (VkMemoryMapFlags v)
                        )
                    )



-- No documentation found for TopLevel "vkAllocateMemory"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkAllocateMemory" vkAllocateMemory :: ("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkMemoryAllocateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMemory" ::: Ptr VkDeviceMemory) -> IO VkResult
#else
vkAllocateMemory :: DeviceCmds -> ("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkMemoryAllocateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMemory" ::: Ptr VkDeviceMemory) -> IO VkResult
vkAllocateMemory deviceCmds = mkVkAllocateMemory (pVkAllocateMemory deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkAllocateMemory
  :: FunPtr (("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkMemoryAllocateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMemory" ::: Ptr VkDeviceMemory) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkMemoryAllocateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMemory" ::: Ptr VkDeviceMemory) -> IO VkResult)
#endif

type FN_vkAllocateMemory = ("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkMemoryAllocateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMemory" ::: Ptr VkDeviceMemory) -> IO VkResult
type PFN_vkAllocateMemory = FunPtr FN_vkAllocateMemory

-- No documentation found for TopLevel "vkFlushMappedMemoryRanges"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkFlushMappedMemoryRanges" vkFlushMappedMemoryRanges :: ("device" ::: VkDevice) -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr VkMappedMemoryRange) -> IO VkResult
#else
vkFlushMappedMemoryRanges :: DeviceCmds -> ("device" ::: VkDevice) -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr VkMappedMemoryRange) -> IO VkResult
vkFlushMappedMemoryRanges deviceCmds = mkVkFlushMappedMemoryRanges (pVkFlushMappedMemoryRanges deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkFlushMappedMemoryRanges
  :: FunPtr (("device" ::: VkDevice) -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr VkMappedMemoryRange) -> IO VkResult) -> (("device" ::: VkDevice) -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr VkMappedMemoryRange) -> IO VkResult)
#endif

type FN_vkFlushMappedMemoryRanges = ("device" ::: VkDevice) -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr VkMappedMemoryRange) -> IO VkResult
type PFN_vkFlushMappedMemoryRanges = FunPtr FN_vkFlushMappedMemoryRanges

-- No documentation found for TopLevel "vkFreeMemory"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkFreeMemory" vkFreeMemory :: ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
#else
vkFreeMemory :: DeviceCmds -> ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
vkFreeMemory deviceCmds = mkVkFreeMemory (pVkFreeMemory deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkFreeMemory
  :: FunPtr (("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
#endif

type FN_vkFreeMemory = ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkFreeMemory = FunPtr FN_vkFreeMemory

-- No documentation found for TopLevel "vkGetDeviceMemoryCommitment"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetDeviceMemoryCommitment" vkGetDeviceMemoryCommitment :: ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("pCommittedMemoryInBytes" ::: Ptr VkDeviceSize) -> IO ()
#else
vkGetDeviceMemoryCommitment :: DeviceCmds -> ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("pCommittedMemoryInBytes" ::: Ptr VkDeviceSize) -> IO ()
vkGetDeviceMemoryCommitment deviceCmds = mkVkGetDeviceMemoryCommitment (pVkGetDeviceMemoryCommitment deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceMemoryCommitment
  :: FunPtr (("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("pCommittedMemoryInBytes" ::: Ptr VkDeviceSize) -> IO ()) -> (("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("pCommittedMemoryInBytes" ::: Ptr VkDeviceSize) -> IO ())
#endif

type FN_vkGetDeviceMemoryCommitment = ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("pCommittedMemoryInBytes" ::: Ptr VkDeviceSize) -> IO ()
type PFN_vkGetDeviceMemoryCommitment = FunPtr FN_vkGetDeviceMemoryCommitment

-- No documentation found for TopLevel "vkInvalidateMappedMemoryRanges"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkInvalidateMappedMemoryRanges" vkInvalidateMappedMemoryRanges :: ("device" ::: VkDevice) -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr VkMappedMemoryRange) -> IO VkResult
#else
vkInvalidateMappedMemoryRanges :: DeviceCmds -> ("device" ::: VkDevice) -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr VkMappedMemoryRange) -> IO VkResult
vkInvalidateMappedMemoryRanges deviceCmds = mkVkInvalidateMappedMemoryRanges (pVkInvalidateMappedMemoryRanges deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkInvalidateMappedMemoryRanges
  :: FunPtr (("device" ::: VkDevice) -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr VkMappedMemoryRange) -> IO VkResult) -> (("device" ::: VkDevice) -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr VkMappedMemoryRange) -> IO VkResult)
#endif

type FN_vkInvalidateMappedMemoryRanges = ("device" ::: VkDevice) -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr VkMappedMemoryRange) -> IO VkResult
type PFN_vkInvalidateMappedMemoryRanges = FunPtr FN_vkInvalidateMappedMemoryRanges

-- No documentation found for TopLevel "vkMapMemory"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkMapMemory" vkMapMemory :: ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("offset" ::: VkDeviceSize) -> ("size" ::: VkDeviceSize) -> ("flags" ::: VkMemoryMapFlags) -> ("ppData" ::: Ptr (Ptr ())) -> IO VkResult
#else
vkMapMemory :: DeviceCmds -> ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("offset" ::: VkDeviceSize) -> ("size" ::: VkDeviceSize) -> ("flags" ::: VkMemoryMapFlags) -> ("ppData" ::: Ptr (Ptr ())) -> IO VkResult
vkMapMemory deviceCmds = mkVkMapMemory (pVkMapMemory deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkMapMemory
  :: FunPtr (("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("offset" ::: VkDeviceSize) -> ("size" ::: VkDeviceSize) -> ("flags" ::: VkMemoryMapFlags) -> ("ppData" ::: Ptr (Ptr ())) -> IO VkResult) -> (("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("offset" ::: VkDeviceSize) -> ("size" ::: VkDeviceSize) -> ("flags" ::: VkMemoryMapFlags) -> ("ppData" ::: Ptr (Ptr ())) -> IO VkResult)
#endif

type FN_vkMapMemory = ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("offset" ::: VkDeviceSize) -> ("size" ::: VkDeviceSize) -> ("flags" ::: VkMemoryMapFlags) -> ("ppData" ::: Ptr (Ptr ())) -> IO VkResult
type PFN_vkMapMemory = FunPtr FN_vkMapMemory

-- No documentation found for TopLevel "vkUnmapMemory"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkUnmapMemory" vkUnmapMemory :: ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> IO ()
#else
vkUnmapMemory :: DeviceCmds -> ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> IO ()
vkUnmapMemory deviceCmds = mkVkUnmapMemory (pVkUnmapMemory deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkUnmapMemory
  :: FunPtr (("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> IO ()) -> (("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> IO ())
#endif

type FN_vkUnmapMemory = ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> IO ()
type PFN_vkUnmapMemory = FunPtr FN_vkUnmapMemory
