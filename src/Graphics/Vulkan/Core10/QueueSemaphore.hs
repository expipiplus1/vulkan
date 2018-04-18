{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.QueueSemaphore
  ( VkSemaphoreCreateFlags(..)
  , vkCreateSemaphore
  , vkDestroySemaphore
  , VkSemaphoreCreateInfo(..)
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
  ( VkStructureType(..)
  , VkResult(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  )
import Graphics.Vulkan.Core10.Queue
  ( VkSemaphore
  )


-- ** VkSemaphoreCreateFlags

-- | 
newtype VkSemaphoreCreateFlags = VkSemaphoreCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkSemaphoreCreateFlags where
  
  showsPrec p (VkSemaphoreCreateFlags x) = showParen (p >= 11) (showString "VkSemaphoreCreateFlags " . showsPrec 11 x)

instance Read VkSemaphoreCreateFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSemaphoreCreateFlags")
                        v <- step readPrec
                        pure (VkSemaphoreCreateFlags v)
                        )
                    )


-- | 
foreign import ccall "vkCreateSemaphore" vkCreateSemaphore :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSemaphoreCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSemaphore" ::: Ptr VkSemaphore) -> IO VkResult
-- | 
foreign import ccall "vkDestroySemaphore" vkDestroySemaphore :: ("device" ::: VkDevice) -> ("semaphore" ::: VkSemaphore) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | TODO: Struct comments
data VkSemaphoreCreateInfo = VkSemaphoreCreateInfo
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkFlags :: VkSemaphoreCreateFlags
  }
  deriving (Eq, Show)

instance Storable VkSemaphoreCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSemaphoreCreateInfo <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSemaphoreCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSemaphoreCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkSemaphoreCreateInfo))
