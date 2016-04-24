{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.CommandPool where

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
import Graphics.Vulkan.Core( StructureType(..)
                           , Result(..)
                           , Flags(..)
                           )


data CommandPoolCreateInfo =
  CommandPoolCreateInfo{ sType :: StructureType 
                       , pNext :: Ptr Void 
                       , flags :: CommandPoolCreateFlags 
                       , queueFamilyIndex :: Word32 
                       }
  deriving (Eq)

instance Storable CommandPoolCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = CommandPoolCreateInfo <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
                                   <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: CommandPoolCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: CommandPoolCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: CommandPoolCreateInfo))
                *> poke (ptr `plusPtr` 20) (queueFamilyIndex (poked :: CommandPoolCreateInfo))


-- ** destroyCommandPool
foreign import ccall "vkDestroyCommandPool" destroyCommandPool ::
  Device -> CommandPool -> Ptr AllocationCallbacks -> IO ()

-- ** resetCommandPool
foreign import ccall "vkResetCommandPool" resetCommandPool ::
  Device -> CommandPool -> CommandPoolResetFlags -> IO Result

-- ** CommandPoolCreateFlags

newtype CommandPoolCreateFlags = CommandPoolCreateFlags Flags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show CommandPoolCreateFlags where
  showsPrec _ VK_COMMAND_POOL_CREATE_TRANSIENT_BIT = showString "VK_COMMAND_POOL_CREATE_TRANSIENT_BIT"
  showsPrec _ VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT = showString "VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT"
  
  showsPrec p (CommandPoolCreateFlags x) = showParen (p >= 11) (showString "CommandPoolCreateFlags " . showsPrec 11 x)

instance Read CommandPoolCreateFlags where
  readPrec = parens ( choose [ ("VK_COMMAND_POOL_CREATE_TRANSIENT_BIT", pure VK_COMMAND_POOL_CREATE_TRANSIENT_BIT)
                             , ("VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT", pure VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "CommandPoolCreateFlags")
                        v <- step readPrec
                        pure (CommandPoolCreateFlags v)
                        )
                    )

-- | Command buffers have a short lifetime
pattern VK_COMMAND_POOL_CREATE_TRANSIENT_BIT = CommandPoolCreateFlags 0x1
-- | Command buffers may release their memory individually
pattern VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT = CommandPoolCreateFlags 0x2


-- ** createCommandPool
foreign import ccall "vkCreateCommandPool" createCommandPool ::
  Device ->
  Ptr CommandPoolCreateInfo ->
    Ptr AllocationCallbacks -> Ptr CommandPool -> IO Result

-- ** CommandPoolResetFlags

newtype CommandPoolResetFlags = CommandPoolResetFlags Flags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show CommandPoolResetFlags where
  showsPrec _ VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT = showString "VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT"
  
  showsPrec p (CommandPoolResetFlags x) = showParen (p >= 11) (showString "CommandPoolResetFlags " . showsPrec 11 x)

instance Read CommandPoolResetFlags where
  readPrec = parens ( choose [ ("VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT", pure VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "CommandPoolResetFlags")
                        v <- step readPrec
                        pure (CommandPoolResetFlags v)
                        )
                    )

-- | Release resources owned by the pool
pattern VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT = CommandPoolResetFlags 0x1


newtype CommandPool = CommandPool Word64
  deriving (Eq, Storable)

