{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.Fence where

import Graphics.Vulkan.Device( Device(..)
                             )
import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import Data.Word( Word64(..)
                , Word32(..)
                )
import Foreign.Ptr( Ptr(..)
                  , plusPtr
                  )
import Data.Bits( Bits
                , FiniteBits
                )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void(..)
                )
import Graphics.Vulkan.Memory( AllocationCallbacks(..)
                             )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )
import Graphics.Vulkan.Core( VkFlags(..)
                           , StructureType(..)
                           , VkBool32(..)
                           , Result(..)
                           )


data FenceCreateInfo =
  FenceCreateInfo{ sType :: StructureType 
                 , pNext :: Ptr Void 
                 , flags :: FenceCreateFlags 
                 }
  deriving (Eq)

instance Storable FenceCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = FenceCreateInfo <$> peek (ptr `plusPtr` 0)
                             <*> peek (ptr `plusPtr` 8)
                             <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: FenceCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: FenceCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: FenceCreateInfo))


-- ** vkResetFences
foreign import ccall "vkResetFences" vkResetFences ::
  Device -> Word32 -> Ptr Fence -> IO Result

-- ** vkDestroyFence
foreign import ccall "vkDestroyFence" vkDestroyFence ::
  Device -> Fence -> Ptr AllocationCallbacks -> IO ()

-- ** vkWaitForFences
foreign import ccall "vkWaitForFences" vkWaitForFences ::
  Device -> Word32 -> Ptr Fence -> VkBool32 -> Word64 -> IO Result

-- ** vkGetFenceStatus
foreign import ccall "vkGetFenceStatus" vkGetFenceStatus ::
  Device -> Fence -> IO Result

-- ** VkFenceCreateFlags

newtype FenceCreateFlags = FenceCreateFlags VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show FenceCreateFlags where
  showsPrec _ VK_FENCE_CREATE_SIGNALED_BIT = showString "VK_FENCE_CREATE_SIGNALED_BIT"
  
  showsPrec p (FenceCreateFlags x) = showParen (p >= 11) (showString "FenceCreateFlags " . showsPrec 11 x)

instance Read FenceCreateFlags where
  readPrec = parens ( choose [ ("VK_FENCE_CREATE_SIGNALED_BIT", pure VK_FENCE_CREATE_SIGNALED_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "FenceCreateFlags")
                        v <- step readPrec
                        pure (FenceCreateFlags v)
                        )
                    )


pattern VK_FENCE_CREATE_SIGNALED_BIT = FenceCreateFlags 0x1


-- ** vkCreateFence
foreign import ccall "vkCreateFence" vkCreateFence ::
  Device ->
  Ptr FenceCreateInfo ->
    Ptr AllocationCallbacks -> Ptr Fence -> IO Result

newtype Fence = Fence Word64
  deriving (Eq, Storable)

