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
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkAllocateMemory
#endif
  , FN_vkAllocateMemory
  , PFN_vkAllocateMemory
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkFlushMappedMemoryRanges
#endif
  , FN_vkFlushMappedMemoryRanges
  , PFN_vkFlushMappedMemoryRanges
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkFreeMemory
#endif
  , FN_vkFreeMemory
  , PFN_vkFreeMemory
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkGetDeviceMemoryCommitment
#endif
  , FN_vkGetDeviceMemoryCommitment
  , PFN_vkGetDeviceMemoryCommitment
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkInvalidateMappedMemoryRanges
#endif
  , FN_vkInvalidateMappedMemoryRanges
  , PFN_vkInvalidateMappedMemoryRanges
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkMapMemory
#endif
  , FN_vkMapMemory
  , PFN_vkMapMemory
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkUnmapMemory
#endif
  , FN_vkUnmapMemory
  , PFN_vkUnmapMemory
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
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  , VkDeviceSize
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
  zero = VkMappedMemoryRange zero
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
  zero = VkMemoryAllocateInfo zero
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


#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkAllocateMemory"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkAllocateMemory" vkAllocateMemory :: ("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkMemoryAllocateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMemory" ::: Ptr VkDeviceMemory) -> IO VkResult

#endif
type FN_vkAllocateMemory = ("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkMemoryAllocateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMemory" ::: Ptr VkDeviceMemory) -> IO VkResult
type PFN_vkAllocateMemory = FunPtr FN_vkAllocateMemory
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkFlushMappedMemoryRanges"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkFlushMappedMemoryRanges" vkFlushMappedMemoryRanges :: ("device" ::: VkDevice) -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr VkMappedMemoryRange) -> IO VkResult

#endif
type FN_vkFlushMappedMemoryRanges = ("device" ::: VkDevice) -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr VkMappedMemoryRange) -> IO VkResult
type PFN_vkFlushMappedMemoryRanges = FunPtr FN_vkFlushMappedMemoryRanges
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkFreeMemory"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkFreeMemory" vkFreeMemory :: ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()

#endif
type FN_vkFreeMemory = ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkFreeMemory = FunPtr FN_vkFreeMemory
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkGetDeviceMemoryCommitment"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetDeviceMemoryCommitment" vkGetDeviceMemoryCommitment :: ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("pCommittedMemoryInBytes" ::: Ptr VkDeviceSize) -> IO ()

#endif
type FN_vkGetDeviceMemoryCommitment = ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("pCommittedMemoryInBytes" ::: Ptr VkDeviceSize) -> IO ()
type PFN_vkGetDeviceMemoryCommitment = FunPtr FN_vkGetDeviceMemoryCommitment
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkInvalidateMappedMemoryRanges"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkInvalidateMappedMemoryRanges" vkInvalidateMappedMemoryRanges :: ("device" ::: VkDevice) -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr VkMappedMemoryRange) -> IO VkResult

#endif
type FN_vkInvalidateMappedMemoryRanges = ("device" ::: VkDevice) -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr VkMappedMemoryRange) -> IO VkResult
type PFN_vkInvalidateMappedMemoryRanges = FunPtr FN_vkInvalidateMappedMemoryRanges
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkMapMemory"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkMapMemory" vkMapMemory :: ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("offset" ::: VkDeviceSize) -> ("size" ::: VkDeviceSize) -> ("flags" ::: VkMemoryMapFlags) -> ("ppData" ::: Ptr (Ptr ())) -> IO VkResult

#endif
type FN_vkMapMemory = ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("offset" ::: VkDeviceSize) -> ("size" ::: VkDeviceSize) -> ("flags" ::: VkMemoryMapFlags) -> ("ppData" ::: Ptr (Ptr ())) -> IO VkResult
type PFN_vkMapMemory = FunPtr FN_vkMapMemory
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkUnmapMemory"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkUnmapMemory" vkUnmapMemory :: ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> IO ()

#endif
type FN_vkUnmapMemory = ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> IO ()
type PFN_vkUnmapMemory = FunPtr FN_vkUnmapMemory
