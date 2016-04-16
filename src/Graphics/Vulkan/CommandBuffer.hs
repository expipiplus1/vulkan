{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.CommandBuffer where

import Graphics.Vulkan.Device( Device(..)
                             )
import Graphics.Vulkan.Pass( RenderPass(..)
                           , Framebuffer(..)
                           )
import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import Data.Word( Word32(..)
                )
import Foreign.Ptr( Ptr(..)
                  , Ptr
                  , plusPtr
                  )
import Data.Int( Int32
               )
import Data.Bits( Bits
                , FiniteBits
                )
import Foreign.Storable( Storable(..)
                       )
import Graphics.Vulkan.CommandPool( CommandPool(..)
                                  )
import Data.Void( Void(..)
                )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )
import Graphics.Vulkan.Query( VkQueryPipelineStatisticFlags(..)
                            , VkQueryControlFlags(..)
                            )
import Graphics.Vulkan.Core( VkStructureType(..)
                           , VkFlags(..)
                           , VkBool32(..)
                           , VkResult(..)
                           )

-- ** VkCommandBufferLevel

newtype VkCommandBufferLevel = VkCommandBufferLevel Int32
  deriving (Eq, Storable)

instance Show VkCommandBufferLevel where
  showsPrec _ VK_COMMAND_BUFFER_LEVEL_PRIMARY = showString "VK_COMMAND_BUFFER_LEVEL_PRIMARY"
  showsPrec _ VK_COMMAND_BUFFER_LEVEL_SECONDARY = showString "VK_COMMAND_BUFFER_LEVEL_SECONDARY"
  showsPrec p (VkCommandBufferLevel x) = showParen (p >= 11) (showString "VkCommandBufferLevel " . showsPrec 11 x)

instance Read VkCommandBufferLevel where
  readPrec = parens ( choose [ ("VK_COMMAND_BUFFER_LEVEL_PRIMARY", pure VK_COMMAND_BUFFER_LEVEL_PRIMARY)
                             , ("VK_COMMAND_BUFFER_LEVEL_SECONDARY", pure VK_COMMAND_BUFFER_LEVEL_SECONDARY)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkCommandBufferLevel")
                        v <- step readPrec
                        pure (VkCommandBufferLevel v)
                        )
                    )


pattern VK_COMMAND_BUFFER_LEVEL_PRIMARY = VkCommandBufferLevel 0

pattern VK_COMMAND_BUFFER_LEVEL_SECONDARY = VkCommandBufferLevel 1

-- ** vkAllocateCommandBuffers
foreign import ccall "vkAllocateCommandBuffers" vkAllocateCommandBuffers ::
  Device ->
  Ptr VkCommandBufferAllocateInfo -> Ptr CommandBuffer -> IO VkResult

-- ** vkResetCommandBuffer
foreign import ccall "vkResetCommandBuffer" vkResetCommandBuffer ::
  CommandBuffer -> VkCommandBufferResetFlags -> IO VkResult

-- ** vkFreeCommandBuffers
foreign import ccall "vkFreeCommandBuffers" vkFreeCommandBuffers ::
  Device -> CommandPool -> Word32 -> Ptr CommandBuffer -> IO ()

-- ** VkCommandBufferUsageFlags

newtype VkCommandBufferUsageFlags = VkCommandBufferUsageFlags VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show VkCommandBufferUsageFlags where
  showsPrec _ VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT = showString "VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT"
  showsPrec _ VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT = showString "VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT"
  showsPrec _ VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT = showString "VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT"
  
  showsPrec p (VkCommandBufferUsageFlags x) = showParen (p >= 11) (showString "VkCommandBufferUsageFlags " . showsPrec 11 x)

instance Read VkCommandBufferUsageFlags where
  readPrec = parens ( choose [ ("VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT", pure VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT)
                             , ("VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT", pure VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT)
                             , ("VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT", pure VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkCommandBufferUsageFlags")
                        v <- step readPrec
                        pure (VkCommandBufferUsageFlags v)
                        )
                    )


pattern VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT = VkCommandBufferUsageFlags 0x1

pattern VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT = VkCommandBufferUsageFlags 0x2
-- | Command buffer may be submitted/executed more than once simultaneously
pattern VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT = VkCommandBufferUsageFlags 0x4



data VkCommandBufferBeginInfo =
  VkCommandBufferBeginInfo{ sType :: VkStructureType 
                          , pNext :: Ptr Void 
                          , flags :: VkCommandBufferUsageFlags 
                          , pInheritanceInfo :: Ptr VkCommandBufferInheritanceInfo 
                          }
  deriving (Eq)

instance Storable VkCommandBufferBeginInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkCommandBufferBeginInfo <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 16)
                                      <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: VkCommandBufferBeginInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: VkCommandBufferBeginInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: VkCommandBufferBeginInfo))
                *> poke (ptr `plusPtr` 24) (pInheritanceInfo (poked :: VkCommandBufferBeginInfo))



data VkCommandBufferInheritanceInfo =
  VkCommandBufferInheritanceInfo{ sType :: VkStructureType 
                                , pNext :: Ptr Void 
                                , renderPass :: RenderPass 
                                , subpass :: Word32 
                                , framebuffer :: Framebuffer 
                                , occlusionQueryEnable :: VkBool32 
                                , queryFlags :: VkQueryControlFlags 
                                , pipelineStatistics :: VkQueryPipelineStatisticFlags 
                                }
  deriving (Eq)

instance Storable VkCommandBufferInheritanceInfo where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = VkCommandBufferInheritanceInfo <$> peek (ptr `plusPtr` 0)
                                            <*> peek (ptr `plusPtr` 8)
                                            <*> peek (ptr `plusPtr` 16)
                                            <*> peek (ptr `plusPtr` 24)
                                            <*> peek (ptr `plusPtr` 32)
                                            <*> peek (ptr `plusPtr` 40)
                                            <*> peek (ptr `plusPtr` 44)
                                            <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: VkCommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: VkCommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 16) (renderPass (poked :: VkCommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 24) (subpass (poked :: VkCommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 32) (framebuffer (poked :: VkCommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 40) (occlusionQueryEnable (poked :: VkCommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 44) (queryFlags (poked :: VkCommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 48) (pipelineStatistics (poked :: VkCommandBufferInheritanceInfo))


data VkCommandBuffer_T
type CommandBuffer = Ptr VkCommandBuffer_T

-- ** VkCommandBufferResetFlags

newtype VkCommandBufferResetFlags = VkCommandBufferResetFlags VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show VkCommandBufferResetFlags where
  showsPrec _ VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT = showString "VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT"
  
  showsPrec p (VkCommandBufferResetFlags x) = showParen (p >= 11) (showString "VkCommandBufferResetFlags " . showsPrec 11 x)

instance Read VkCommandBufferResetFlags where
  readPrec = parens ( choose [ ("VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT", pure VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkCommandBufferResetFlags")
                        v <- step readPrec
                        pure (VkCommandBufferResetFlags v)
                        )
                    )

-- | Release resources owned by the buffer
pattern VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT = VkCommandBufferResetFlags 0x1


-- ** vkEndCommandBuffer
foreign import ccall "vkEndCommandBuffer" vkEndCommandBuffer ::
  CommandBuffer -> IO VkResult

-- ** vkBeginCommandBuffer
foreign import ccall "vkBeginCommandBuffer" vkBeginCommandBuffer ::
  CommandBuffer -> Ptr VkCommandBufferBeginInfo -> IO VkResult


data VkCommandBufferAllocateInfo =
  VkCommandBufferAllocateInfo{ sType :: VkStructureType 
                             , pNext :: Ptr Void 
                             , commandPool :: CommandPool 
                             , level :: VkCommandBufferLevel 
                             , commandBufferCount :: Word32 
                             }
  deriving (Eq)

instance Storable VkCommandBufferAllocateInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkCommandBufferAllocateInfo <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
                                         <*> peek (ptr `plusPtr` 16)
                                         <*> peek (ptr `plusPtr` 24)
                                         <*> peek (ptr `plusPtr` 28)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: VkCommandBufferAllocateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: VkCommandBufferAllocateInfo))
                *> poke (ptr `plusPtr` 16) (commandPool (poked :: VkCommandBufferAllocateInfo))
                *> poke (ptr `plusPtr` 24) (level (poked :: VkCommandBufferAllocateInfo))
                *> poke (ptr `plusPtr` 28) (commandBufferCount (poked :: VkCommandBufferAllocateInfo))


