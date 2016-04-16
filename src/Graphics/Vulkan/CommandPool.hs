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
import Graphics.Vulkan.Memory( VkAllocationCallbacks(..)
                             )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )
import Graphics.Vulkan.Core( VkStructureType(..)
                           , VkFlags(..)
                           , VkResult(..)
                           )


data VkCommandPoolCreateInfo =
  VkCommandPoolCreateInfo{ sType :: VkStructureType 
                         , pNext :: Ptr Void 
                         , flags :: VkCommandPoolCreateFlags 
                         , queueFamilyIndex :: Word32 
                         }
  deriving (Eq)

instance Storable VkCommandPoolCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkCommandPoolCreateInfo <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: VkCommandPoolCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: VkCommandPoolCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: VkCommandPoolCreateInfo))
                *> poke (ptr `plusPtr` 20) (queueFamilyIndex (poked :: VkCommandPoolCreateInfo))


-- ** vkDestroyCommandPool
foreign import ccall "vkDestroyCommandPool" vkDestroyCommandPool ::
  Device -> CommandPool -> Ptr VkAllocationCallbacks -> IO ()

-- ** vkResetCommandPool
foreign import ccall "vkResetCommandPool" vkResetCommandPool ::
  Device -> CommandPool -> VkCommandPoolResetFlags -> IO VkResult

-- ** VkCommandPoolCreateFlags

newtype VkCommandPoolCreateFlags = VkCommandPoolCreateFlags VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show VkCommandPoolCreateFlags where
  showsPrec _ VK_COMMAND_POOL_CREATE_TRANSIENT_BIT = showString "VK_COMMAND_POOL_CREATE_TRANSIENT_BIT"
  showsPrec _ VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT = showString "VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT"
  
  showsPrec p (VkCommandPoolCreateFlags x) = showParen (p >= 11) (showString "VkCommandPoolCreateFlags " . showsPrec 11 x)

instance Read VkCommandPoolCreateFlags where
  readPrec = parens ( choose [ ("VK_COMMAND_POOL_CREATE_TRANSIENT_BIT", pure VK_COMMAND_POOL_CREATE_TRANSIENT_BIT)
                             , ("VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT", pure VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkCommandPoolCreateFlags")
                        v <- step readPrec
                        pure (VkCommandPoolCreateFlags v)
                        )
                    )

-- | Command buffers have a short lifetime
pattern VK_COMMAND_POOL_CREATE_TRANSIENT_BIT = VkCommandPoolCreateFlags 0x1
-- | Command buffers may release their memory individually
pattern VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT = VkCommandPoolCreateFlags 0x2


-- ** vkCreateCommandPool
foreign import ccall "vkCreateCommandPool" vkCreateCommandPool ::
  Device ->
  Ptr VkCommandPoolCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr CommandPool -> IO VkResult

-- ** VkCommandPoolResetFlags

newtype VkCommandPoolResetFlags = VkCommandPoolResetFlags VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show VkCommandPoolResetFlags where
  showsPrec _ VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT = showString "VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT"
  
  showsPrec p (VkCommandPoolResetFlags x) = showParen (p >= 11) (showString "VkCommandPoolResetFlags " . showsPrec 11 x)

instance Read VkCommandPoolResetFlags where
  readPrec = parens ( choose [ ("VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT", pure VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkCommandPoolResetFlags")
                        v <- step readPrec
                        pure (VkCommandPoolResetFlags v)
                        )
                    )

-- | Release resources owned by the pool
pattern VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT = VkCommandPoolResetFlags 0x1


newtype CommandPool = CommandPool Word64
  deriving (Eq, Storable)

