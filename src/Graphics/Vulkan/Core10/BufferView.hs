{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.BufferView
  ( VkBufferViewCreateFlags(..)
  , VkBufferView
  , vkCreateBufferView
  , vkDestroyBufferView
  , VkBufferViewCreateInfo(..)
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
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


import Graphics.Vulkan.Core10.Core
  ( VkFormat(..)
  , VkStructureType(..)
  , VkResult(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkDeviceSize
  , VkAllocationCallbacks(..)
  , VkDevice
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( VkBuffer
  )


-- ** VkBufferViewCreateFlags

-- | 
newtype VkBufferViewCreateFlags = VkBufferViewCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkBufferViewCreateFlags where
  
  showsPrec p (VkBufferViewCreateFlags x) = showParen (p >= 11) (showString "VkBufferViewCreateFlags " . showsPrec 11 x)

instance Read VkBufferViewCreateFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkBufferViewCreateFlags")
                        v <- step readPrec
                        pure (VkBufferViewCreateFlags v)
                        )
                    )


-- |
data VkBufferView_T
type VkBufferView = Ptr VkBufferView_T
-- | 
foreign import ccall "vkCreateBufferView" vkCreateBufferView :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkBufferViewCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pView" ::: Ptr VkBufferView) -> IO VkResult
-- | 
foreign import ccall "vkDestroyBufferView" vkDestroyBufferView :: ("device" ::: VkDevice) -> ("bufferView" ::: VkBufferView) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | TODO: Struct comments
data VkBufferViewCreateInfo = VkBufferViewCreateInfo
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkFlags :: VkBufferViewCreateFlags
  , vkBuffer :: VkBuffer
  , vkFormat :: VkFormat
  , vkOffset :: VkDeviceSize
  , vkRange :: VkDeviceSize
  }
  deriving (Eq, Show)

instance Storable VkBufferViewCreateInfo where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = VkBufferViewCreateInfo <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 8)
                                    <*> peek (ptr `plusPtr` 16)
                                    <*> peek (ptr `plusPtr` 24)
                                    <*> peek (ptr `plusPtr` 32)
                                    <*> peek (ptr `plusPtr` 40)
                                    <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBufferViewCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBufferViewCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkBufferViewCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkBuffer (poked :: VkBufferViewCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkFormat (poked :: VkBufferViewCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkOffset (poked :: VkBufferViewCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkRange (poked :: VkBufferViewCreateInfo))
