{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.Fence where

import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import Data.Word( Word64
                , Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Data.Bits( Bits
                , FiniteBits
                )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )


data VkFenceCreateInfo =
  VkFenceCreateInfo{ sType :: VkStructureType 
                   , pNext :: Ptr Void 
                   , flags :: VkFenceCreateFlags 
                   }
  deriving (Eq)

instance Storable VkFenceCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkFenceCreateInfo <$> peek (ptr `plusPtr` 0)
                               <*> peek (ptr `plusPtr` 8)
                               <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: VkFenceCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: VkFenceCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: VkFenceCreateInfo))


-- ** vkResetFences
foreign import ccall "vkResetFences" vkResetFences ::
  Device -> Word32 -> Ptr Fence -> IO VkResult

-- ** vkDestroyFence
foreign import ccall "vkDestroyFence" vkDestroyFence ::
  Device -> Fence -> Ptr VkAllocationCallbacks -> IO ()

-- ** vkWaitForFences
foreign import ccall "vkWaitForFences" vkWaitForFences ::
  Device -> Word32 -> Ptr Fence -> VkBool32 -> Word64 -> IO VkResult

-- ** vkGetFenceStatus
foreign import ccall "vkGetFenceStatus" vkGetFenceStatus ::
  Device -> Fence -> IO VkResult

-- ** VkFenceCreateFlags

newtype VkFenceCreateFlagBits = VkFenceCreateFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)

-- | Alias for VkFenceCreateFlagBits
type VkFenceCreateFlags = VkFenceCreateFlagBits

instance Show VkFenceCreateFlagBits where
  showsPrec _ VK_FENCE_CREATE_SIGNALED_BIT = showString "VK_FENCE_CREATE_SIGNALED_BIT"
  
  showsPrec p (VkFenceCreateFlagBits x) = showParen (p >= 11) (showString "VkFenceCreateFlagBits " . showsPrec 11 x)

instance Read VkFenceCreateFlagBits where
  readPrec = parens ( choose [ ("VK_FENCE_CREATE_SIGNALED_BIT", pure VK_FENCE_CREATE_SIGNALED_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkFenceCreateFlagBits")
                        v <- step readPrec
                        pure (VkFenceCreateFlagBits v)
                        )
                    )


pattern VK_FENCE_CREATE_SIGNALED_BIT = VkFenceCreateFlagBits 0x1


-- ** vkCreateFence
foreign import ccall "vkCreateFence" vkCreateFence ::
  Device ->
  Ptr VkFenceCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr Fence -> IO VkResult

newtype Fence = Fence Word64
  deriving (Eq, Storable)

