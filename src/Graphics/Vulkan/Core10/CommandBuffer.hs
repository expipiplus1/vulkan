{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.CommandBuffer
  ( VkCommandBufferLevel(..)
  , pattern VK_COMMAND_BUFFER_LEVEL_PRIMARY
  , pattern VK_COMMAND_BUFFER_LEVEL_SECONDARY
  , VkQueryControlFlagBits(..)
  , pattern VK_QUERY_CONTROL_PRECISE_BIT
  , VkCommandBufferResetFlagBits(..)
  , pattern VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT
  , VkCommandBufferUsageFlagBits(..)
  , pattern VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
  , pattern VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT
  , pattern VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT
  , vkAllocateCommandBuffers
  , vkFreeCommandBuffers
  , vkBeginCommandBuffer
  , vkEndCommandBuffer
  , vkResetCommandBuffer
  , VkCommandBufferAllocateInfo(..)
  , VkCommandBufferInheritanceInfo(..)
  , VkCommandBufferBeginInfo(..)
  , VkQueryControlFlags
  , VkCommandBufferResetFlags
  , VkCommandBufferUsageFlags
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Int
  ( Int32
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


import Graphics.Vulkan.Core10.CommandPool
  ( VkCommandPool
  )
import Graphics.Vulkan.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  , VkResult(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkDevice
  )
import Graphics.Vulkan.Core10.Pass
  ( VkFramebuffer
  )
import Graphics.Vulkan.Core10.Pipeline
  ( VkRenderPass
  )
import Graphics.Vulkan.Core10.Query
  ( VkQueryPipelineStatisticFlags
  )
import Graphics.Vulkan.Core10.Queue
  ( VkCommandBuffer
  )


-- ** VkCommandBufferLevel

-- | 
newtype VkCommandBufferLevel = VkCommandBufferLevel Int32
  deriving (Eq, Ord, Storable)

instance Show VkCommandBufferLevel where
  showsPrec _ VK_COMMAND_BUFFER_LEVEL_PRIMARY = showString "VK_COMMAND_BUFFER_LEVEL_PRIMARY"
  showsPrec _ VK_COMMAND_BUFFER_LEVEL_SECONDARY = showString "VK_COMMAND_BUFFER_LEVEL_SECONDARY"
  showsPrec p (VkCommandBufferLevel x) = showParen (p >= 11) (showString "VkCommandBufferLevel " . showsPrec 11 x)

instance Read VkCommandBufferLevel where
  readPrec = parens ( choose [ ("VK_COMMAND_BUFFER_LEVEL_PRIMARY",   pure VK_COMMAND_BUFFER_LEVEL_PRIMARY)
                             , ("VK_COMMAND_BUFFER_LEVEL_SECONDARY", pure VK_COMMAND_BUFFER_LEVEL_SECONDARY)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkCommandBufferLevel")
                        v <- step readPrec
                        pure (VkCommandBufferLevel v)
                        )
                    )

-- | 
pattern VK_COMMAND_BUFFER_LEVEL_PRIMARY :: VkCommandBufferLevel
pattern VK_COMMAND_BUFFER_LEVEL_PRIMARY = VkCommandBufferLevel 0

-- | 
pattern VK_COMMAND_BUFFER_LEVEL_SECONDARY :: VkCommandBufferLevel
pattern VK_COMMAND_BUFFER_LEVEL_SECONDARY = VkCommandBufferLevel 1
-- ** VkQueryControlFlagBits

-- | 
newtype VkQueryControlFlagBits = VkQueryControlFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkQueryControlFlagBits where
  showsPrec _ VK_QUERY_CONTROL_PRECISE_BIT = showString "VK_QUERY_CONTROL_PRECISE_BIT"
  showsPrec p (VkQueryControlFlagBits x) = showParen (p >= 11) (showString "VkQueryControlFlagBits " . showsPrec 11 x)

instance Read VkQueryControlFlagBits where
  readPrec = parens ( choose [ ("VK_QUERY_CONTROL_PRECISE_BIT", pure VK_QUERY_CONTROL_PRECISE_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkQueryControlFlagBits")
                        v <- step readPrec
                        pure (VkQueryControlFlagBits v)
                        )
                    )

-- | Require precise results to be collected by the query
pattern VK_QUERY_CONTROL_PRECISE_BIT :: VkQueryControlFlagBits
pattern VK_QUERY_CONTROL_PRECISE_BIT = VkQueryControlFlagBits 0x00000001
-- ** VkCommandBufferResetFlagBits

-- | 
newtype VkCommandBufferResetFlagBits = VkCommandBufferResetFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkCommandBufferResetFlagBits where
  showsPrec _ VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT = showString "VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT"
  showsPrec p (VkCommandBufferResetFlagBits x) = showParen (p >= 11) (showString "VkCommandBufferResetFlagBits " . showsPrec 11 x)

instance Read VkCommandBufferResetFlagBits where
  readPrec = parens ( choose [ ("VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT", pure VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkCommandBufferResetFlagBits")
                        v <- step readPrec
                        pure (VkCommandBufferResetFlagBits v)
                        )
                    )

-- | Release resources owned by the buffer
pattern VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT :: VkCommandBufferResetFlagBits
pattern VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT = VkCommandBufferResetFlagBits 0x00000001
-- ** VkCommandBufferUsageFlagBits

-- | 
newtype VkCommandBufferUsageFlagBits = VkCommandBufferUsageFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkCommandBufferUsageFlagBits where
  showsPrec _ VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT = showString "VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT"
  showsPrec _ VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT = showString "VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT"
  showsPrec _ VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT = showString "VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT"
  showsPrec p (VkCommandBufferUsageFlagBits x) = showParen (p >= 11) (showString "VkCommandBufferUsageFlagBits " . showsPrec 11 x)

instance Read VkCommandBufferUsageFlagBits where
  readPrec = parens ( choose [ ("VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT",      pure VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT)
                             , ("VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT", pure VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT)
                             , ("VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT",     pure VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkCommandBufferUsageFlagBits")
                        v <- step readPrec
                        pure (VkCommandBufferUsageFlagBits v)
                        )
                    )

-- | 
pattern VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT :: VkCommandBufferUsageFlagBits
pattern VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT = VkCommandBufferUsageFlagBits 0x00000001

-- | 
pattern VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT :: VkCommandBufferUsageFlagBits
pattern VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT = VkCommandBufferUsageFlagBits 0x00000002

-- | Command buffer may be submitted/executed more than once simultaneously
pattern VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT :: VkCommandBufferUsageFlagBits
pattern VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT = VkCommandBufferUsageFlagBits 0x00000004
-- | 
foreign import ccall "vkAllocateCommandBuffers" vkAllocateCommandBuffers :: ("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkCommandBufferAllocateInfo) -> ("pCommandBuffers" ::: Ptr VkCommandBuffer) -> IO VkResult
-- | 
foreign import ccall "vkFreeCommandBuffers" vkFreeCommandBuffers :: ("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("commandBufferCount" ::: Word32) -> ("pCommandBuffers" ::: Ptr VkCommandBuffer) -> IO ()
-- | 
foreign import ccall "vkBeginCommandBuffer" vkBeginCommandBuffer :: ("commandBuffer" ::: VkCommandBuffer) -> ("pBeginInfo" ::: Ptr VkCommandBufferBeginInfo) -> IO VkResult
-- | 
foreign import ccall "vkEndCommandBuffer" vkEndCommandBuffer :: ("commandBuffer" ::: VkCommandBuffer) -> IO VkResult
-- | 
foreign import ccall "vkResetCommandBuffer" vkResetCommandBuffer :: ("commandBuffer" ::: VkCommandBuffer) -> ("flags" ::: VkCommandBufferResetFlags) -> IO VkResult
-- | TODO: Struct comments
data VkCommandBufferAllocateInfo = VkCommandBufferAllocateInfo
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkCommandPool :: VkCommandPool
  , vkLevel :: VkCommandBufferLevel
  , vkCommandBufferCount :: Word32
  }
  deriving (Eq, Show)

instance Storable VkCommandBufferAllocateInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkCommandBufferAllocateInfo <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
                                         <*> peek (ptr `plusPtr` 16)
                                         <*> peek (ptr `plusPtr` 24)
                                         <*> peek (ptr `plusPtr` 28)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkCommandBufferAllocateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkCommandBufferAllocateInfo))
                *> poke (ptr `plusPtr` 16) (vkCommandPool (poked :: VkCommandBufferAllocateInfo))
                *> poke (ptr `plusPtr` 24) (vkLevel (poked :: VkCommandBufferAllocateInfo))
                *> poke (ptr `plusPtr` 28) (vkCommandBufferCount (poked :: VkCommandBufferAllocateInfo))
-- | TODO: Struct comments
data VkCommandBufferInheritanceInfo = VkCommandBufferInheritanceInfo
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkRenderPass :: VkRenderPass
  , vkSubpass :: Word32
  , vkFramebuffer :: VkFramebuffer
  , vkOcclusionQueryEnable :: VkBool32
  , vkQueryFlags :: VkQueryControlFlags
  , vkPipelineStatistics :: VkQueryPipelineStatisticFlags
  }
  deriving (Eq, Show)

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
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkCommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkCommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 16) (vkRenderPass (poked :: VkCommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 24) (vkSubpass (poked :: VkCommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 32) (vkFramebuffer (poked :: VkCommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 40) (vkOcclusionQueryEnable (poked :: VkCommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 44) (vkQueryFlags (poked :: VkCommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 48) (vkPipelineStatistics (poked :: VkCommandBufferInheritanceInfo))
-- | TODO: Struct comments
data VkCommandBufferBeginInfo = VkCommandBufferBeginInfo
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkFlags :: VkCommandBufferUsageFlags
  , vkPInheritanceInfo :: Ptr VkCommandBufferInheritanceInfo
  }
  deriving (Eq, Show)

instance Storable VkCommandBufferBeginInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkCommandBufferBeginInfo <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 16)
                                      <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkCommandBufferBeginInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkCommandBufferBeginInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkCommandBufferBeginInfo))
                *> poke (ptr `plusPtr` 24) (vkPInheritanceInfo (poked :: VkCommandBufferBeginInfo))
type VkQueryControlFlags = VkQueryControlFlagBits
type VkCommandBufferResetFlags = VkCommandBufferResetFlagBits
type VkCommandBufferUsageFlags = VkCommandBufferUsageFlagBits
