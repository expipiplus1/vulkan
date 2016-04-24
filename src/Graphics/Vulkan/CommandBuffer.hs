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
import Graphics.Vulkan.Query( QueryPipelineStatisticFlags(..)
                            , QueryControlFlags(..)
                            )
import Graphics.Vulkan.Core( Bool32(..)
                           , StructureType(..)
                           , Result(..)
                           , Flags(..)
                           )

-- ** CommandBufferLevel

newtype CommandBufferLevel = CommandBufferLevel Int32
  deriving (Eq, Storable)

instance Show CommandBufferLevel where
  showsPrec _ CommandBufferLevelPrimary = showString "CommandBufferLevelPrimary"
  showsPrec _ CommandBufferLevelSecondary = showString "CommandBufferLevelSecondary"
  showsPrec p (CommandBufferLevel x) = showParen (p >= 11) (showString "CommandBufferLevel " . showsPrec 11 x)

instance Read CommandBufferLevel where
  readPrec = parens ( choose [ ("CommandBufferLevelPrimary", pure CommandBufferLevelPrimary)
                             , ("CommandBufferLevelSecondary", pure CommandBufferLevelSecondary)
                             ] +++
                      prec 10 (do
                        expectP (Ident "CommandBufferLevel")
                        v <- step readPrec
                        pure (CommandBufferLevel v)
                        )
                    )


pattern CommandBufferLevelPrimary = CommandBufferLevel 0

pattern CommandBufferLevelSecondary = CommandBufferLevel 1

-- ** allocateCommandBuffers
foreign import ccall "vkAllocateCommandBuffers" allocateCommandBuffers ::
  Device ->
  Ptr CommandBufferAllocateInfo -> Ptr CommandBuffer -> IO Result

-- ** resetCommandBuffer
foreign import ccall "vkResetCommandBuffer" resetCommandBuffer ::
  CommandBuffer -> CommandBufferResetFlags -> IO Result

-- ** freeCommandBuffers
foreign import ccall "vkFreeCommandBuffers" freeCommandBuffers ::
  Device -> CommandPool -> Word32 -> Ptr CommandBuffer -> IO ()

-- ** CommandBufferUsageFlags

newtype CommandBufferUsageFlags = CommandBufferUsageFlags Flags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show CommandBufferUsageFlags where
  showsPrec _ VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT = showString "VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT"
  showsPrec _ VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT = showString "VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT"
  showsPrec _ VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT = showString "VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT"
  
  showsPrec p (CommandBufferUsageFlags x) = showParen (p >= 11) (showString "CommandBufferUsageFlags " . showsPrec 11 x)

instance Read CommandBufferUsageFlags where
  readPrec = parens ( choose [ ("VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT", pure VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT)
                             , ("VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT", pure VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT)
                             , ("VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT", pure VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "CommandBufferUsageFlags")
                        v <- step readPrec
                        pure (CommandBufferUsageFlags v)
                        )
                    )


pattern VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT = CommandBufferUsageFlags 0x1

pattern VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT = CommandBufferUsageFlags 0x2
-- | Command buffer may be submitted/executed more than once simultaneously
pattern VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT = CommandBufferUsageFlags 0x4



data CommandBufferBeginInfo =
  CommandBufferBeginInfo{ sType :: StructureType 
                        , pNext :: Ptr Void 
                        , flags :: CommandBufferUsageFlags 
                        , pInheritanceInfo :: Ptr CommandBufferInheritanceInfo 
                        }
  deriving (Eq)

instance Storable CommandBufferBeginInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = CommandBufferBeginInfo <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 8)
                                    <*> peek (ptr `plusPtr` 16)
                                    <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: CommandBufferBeginInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: CommandBufferBeginInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: CommandBufferBeginInfo))
                *> poke (ptr `plusPtr` 24) (pInheritanceInfo (poked :: CommandBufferBeginInfo))



data CommandBufferInheritanceInfo =
  CommandBufferInheritanceInfo{ sType :: StructureType 
                              , pNext :: Ptr Void 
                              , renderPass :: RenderPass 
                              , subpass :: Word32 
                              , framebuffer :: Framebuffer 
                              , occlusionQueryEnable :: Bool32 
                              , queryFlags :: QueryControlFlags 
                              , pipelineStatistics :: QueryPipelineStatisticFlags 
                              }
  deriving (Eq)

instance Storable CommandBufferInheritanceInfo where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = CommandBufferInheritanceInfo <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 16)
                                          <*> peek (ptr `plusPtr` 24)
                                          <*> peek (ptr `plusPtr` 32)
                                          <*> peek (ptr `plusPtr` 40)
                                          <*> peek (ptr `plusPtr` 44)
                                          <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: CommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: CommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 16) (renderPass (poked :: CommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 24) (subpass (poked :: CommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 32) (framebuffer (poked :: CommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 40) (occlusionQueryEnable (poked :: CommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 44) (queryFlags (poked :: CommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 48) (pipelineStatistics (poked :: CommandBufferInheritanceInfo))


data VkCommandBuffer_T
type CommandBuffer = Ptr VkCommandBuffer_T

-- ** CommandBufferResetFlags

newtype CommandBufferResetFlags = CommandBufferResetFlags Flags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show CommandBufferResetFlags where
  showsPrec _ VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT = showString "VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT"
  
  showsPrec p (CommandBufferResetFlags x) = showParen (p >= 11) (showString "CommandBufferResetFlags " . showsPrec 11 x)

instance Read CommandBufferResetFlags where
  readPrec = parens ( choose [ ("VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT", pure VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "CommandBufferResetFlags")
                        v <- step readPrec
                        pure (CommandBufferResetFlags v)
                        )
                    )

-- | Release resources owned by the buffer
pattern VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT = CommandBufferResetFlags 0x1


-- ** endCommandBuffer
foreign import ccall "vkEndCommandBuffer" endCommandBuffer ::
  CommandBuffer -> IO Result

-- ** beginCommandBuffer
foreign import ccall "vkBeginCommandBuffer" beginCommandBuffer ::
  CommandBuffer -> Ptr CommandBufferBeginInfo -> IO Result


data CommandBufferAllocateInfo =
  CommandBufferAllocateInfo{ sType :: StructureType 
                           , pNext :: Ptr Void 
                           , commandPool :: CommandPool 
                           , level :: CommandBufferLevel 
                           , commandBufferCount :: Word32 
                           }
  deriving (Eq)

instance Storable CommandBufferAllocateInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = CommandBufferAllocateInfo <$> peek (ptr `plusPtr` 0)
                                       <*> peek (ptr `plusPtr` 8)
                                       <*> peek (ptr `plusPtr` 16)
                                       <*> peek (ptr `plusPtr` 24)
                                       <*> peek (ptr `plusPtr` 28)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: CommandBufferAllocateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: CommandBufferAllocateInfo))
                *> poke (ptr `plusPtr` 16) (commandPool (poked :: CommandBufferAllocateInfo))
                *> poke (ptr `plusPtr` 24) (level (poked :: CommandBufferAllocateInfo))
                *> poke (ptr `plusPtr` 28) (commandBufferCount (poked :: CommandBufferAllocateInfo))


