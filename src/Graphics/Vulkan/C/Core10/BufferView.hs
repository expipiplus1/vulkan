{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.BufferView
  ( VkBufferView
  , VkBufferViewCreateFlags(..)
  , VkBufferViewCreateInfo(..)
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCreateBufferView
#endif
  , FN_vkCreateBufferView
  , PFN_vkCreateBufferView
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkDestroyBufferView
#endif
  , FN_vkDestroyBufferView
  , PFN_vkDestroyBufferView
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
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
  ( VkFormat(..)
  , VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  , VkFlags
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  , VkDeviceSize
  )
import Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkBuffer
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | Dummy data to tag the 'Ptr' with
data VkBufferView_T
-- No documentation found for TopLevel "VkBufferView"
type VkBufferView = Ptr VkBufferView_T
-- ** VkBufferViewCreateFlags

-- No documentation found for TopLevel "VkBufferViewCreateFlags"
newtype VkBufferViewCreateFlags = VkBufferViewCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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


-- No documentation found for TopLevel "VkBufferViewCreateInfo"
data VkBufferViewCreateInfo = VkBufferViewCreateInfo
  { -- No documentation found for Nested "VkBufferViewCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkBufferViewCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkBufferViewCreateInfo" "flags"
  vkFlags :: VkBufferViewCreateFlags
  , -- No documentation found for Nested "VkBufferViewCreateInfo" "buffer"
  vkBuffer :: VkBuffer
  , -- No documentation found for Nested "VkBufferViewCreateInfo" "format"
  vkFormat :: VkFormat
  , -- No documentation found for Nested "VkBufferViewCreateInfo" "offset"
  vkOffset :: VkDeviceSize
  , -- No documentation found for Nested "VkBufferViewCreateInfo" "range"
  vkRange :: VkDeviceSize
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

instance Zero VkBufferViewCreateInfo where
  zero = VkBufferViewCreateInfo zero
                                zero
                                zero
                                zero
                                zero
                                zero
                                zero
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCreateBufferView"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateBufferView" vkCreateBufferView :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkBufferViewCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pView" ::: Ptr VkBufferView) -> IO VkResult

#endif
type FN_vkCreateBufferView = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkBufferViewCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pView" ::: Ptr VkBufferView) -> IO VkResult
type PFN_vkCreateBufferView = FunPtr FN_vkCreateBufferView
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkDestroyBufferView"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyBufferView" vkDestroyBufferView :: ("device" ::: VkDevice) -> ("bufferView" ::: VkBufferView) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()

#endif
type FN_vkDestroyBufferView = ("device" ::: VkDevice) -> ("bufferView" ::: VkBufferView) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyBufferView = FunPtr FN_vkDestroyBufferView
