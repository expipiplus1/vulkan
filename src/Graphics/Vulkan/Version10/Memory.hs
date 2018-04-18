{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Version10.Memory
  ( VkMemoryMapFlags(..)
  , VkDeviceMemory
  , vkAllocateMemory
  , vkFreeMemory
  , vkMapMemory
  , vkUnmapMemory
  , vkFlushMappedMemoryRanges
  , vkInvalidateMappedMemoryRanges
  , vkGetDeviceMemoryCommitment
  , VkMemoryAllocateInfo(..)
  , VkMappedMemoryRange(..)
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )
import GHC.Read
  ( expectP
  , choose
  )
import Graphics.Vulkan.NamedType
  ( (:::)
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


import Graphics.Vulkan.Version10.Core
  ( VkStructureType(..)
  , VkResult(..)
  , VkFlags
  )
import Graphics.Vulkan.Version10.DeviceInitialization
  ( VkDeviceSize
  , VkAllocationCallbacks(..)
  , VkDevice
  )


-- ** VkMemoryMapFlags

-- | 
newtype VkMemoryMapFlags = VkMemoryMapFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

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


-- |
data VkDeviceMemory_T
type VkDeviceMemory = Ptr VkDeviceMemory_T
-- | 
foreign import ccall "vkAllocateMemory" vkAllocateMemory :: ("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkMemoryAllocateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMemory" ::: Ptr VkDeviceMemory) -> IO VkResult
-- | 
foreign import ccall "vkFreeMemory" vkFreeMemory :: ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | 
foreign import ccall "vkMapMemory" vkMapMemory :: ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("offset" ::: VkDeviceSize) -> ("size" ::: VkDeviceSize) -> ("flags" ::: VkMemoryMapFlags) -> ("ppData" ::: Ptr (Ptr ())) -> IO VkResult
-- | 
foreign import ccall "vkUnmapMemory" vkUnmapMemory :: ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> IO ()
-- | 
foreign import ccall "vkFlushMappedMemoryRanges" vkFlushMappedMemoryRanges :: ("device" ::: VkDevice) -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr VkMappedMemoryRange) -> IO VkResult
-- | 
foreign import ccall "vkInvalidateMappedMemoryRanges" vkInvalidateMappedMemoryRanges :: ("device" ::: VkDevice) -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr VkMappedMemoryRange) -> IO VkResult
-- | 
foreign import ccall "vkGetDeviceMemoryCommitment" vkGetDeviceMemoryCommitment :: ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("pCommittedMemoryInBytes" ::: Ptr VkDeviceSize) -> IO ()
-- | TODO: Struct comments
data VkMemoryAllocateInfo = VkMemoryAllocateInfo
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkAllocationSize :: VkDeviceSize
  , vkMemoryTypeIndex :: Word32
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
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkMemoryAllocateInfo))
                *> poke (ptr `plusPtr` 16) (vkAllocationSize (poked :: VkMemoryAllocateInfo))
                *> poke (ptr `plusPtr` 24) (vkMemoryTypeIndex (poked :: VkMemoryAllocateInfo))
-- | TODO: Struct comments
data VkMappedMemoryRange = VkMappedMemoryRange
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkMemory :: VkDeviceMemory
  , vkOffset :: VkDeviceSize
  , vkSize :: VkDeviceSize
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
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkMappedMemoryRange))
                *> poke (ptr `plusPtr` 16) (vkMemory (poked :: VkMappedMemoryRange))
                *> poke (ptr `plusPtr` 24) (vkOffset (poked :: VkMappedMemoryRange))
                *> poke (ptr `plusPtr` 32) (vkSize (poked :: VkMappedMemoryRange))
