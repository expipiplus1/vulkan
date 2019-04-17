{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.CommandBuffer
  ( VkCommandBufferAllocateInfo(..)
  , VkCommandBufferBeginInfo(..)
  , VkCommandBufferInheritanceInfo(..)
  , VkCommandBufferLevel(..)
  , pattern VK_COMMAND_BUFFER_LEVEL_PRIMARY
  , pattern VK_COMMAND_BUFFER_LEVEL_SECONDARY
  , VkCommandBufferResetFlagBits(..)
  , pattern VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT
  , VkCommandBufferResetFlags
  , VkCommandBufferUsageFlagBits(..)
  , pattern VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
  , pattern VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT
  , pattern VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT
  , VkCommandBufferUsageFlags
  , VkQueryControlFlagBits(..)
  , pattern VK_QUERY_CONTROL_PRECISE_BIT
  , VkQueryControlFlags
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkAllocateCommandBuffers
#endif
  , FN_vkAllocateCommandBuffers
  , PFN_vkAllocateCommandBuffers
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkBeginCommandBuffer
#endif
  , FN_vkBeginCommandBuffer
  , PFN_vkBeginCommandBuffer
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkEndCommandBuffer
#endif
  , FN_vkEndCommandBuffer
  , PFN_vkEndCommandBuffer
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkFreeCommandBuffers
#endif
  , FN_vkFreeCommandBuffers
  , PFN_vkFreeCommandBuffers
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkResetCommandBuffer
#endif
  , FN_vkResetCommandBuffer
  , PFN_vkResetCommandBuffer
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


import Graphics.Vulkan.C.Core10.CommandPool
  ( VkCommandPool
  )
import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  , VkFlags
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  )
import Graphics.Vulkan.C.Core10.Pass
  ( VkFramebuffer
  )
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkRenderPass
  )
import Graphics.Vulkan.C.Core10.Query
  ( VkQueryPipelineStatisticFlags
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkCommandBuffer
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkCommandBufferAllocateInfo"
data VkCommandBufferAllocateInfo = VkCommandBufferAllocateInfo
  { -- No documentation found for Nested "VkCommandBufferAllocateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkCommandBufferAllocateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkCommandBufferAllocateInfo" "commandPool"
  vkCommandPool :: VkCommandPool
  , -- No documentation found for Nested "VkCommandBufferAllocateInfo" "level"
  vkLevel :: VkCommandBufferLevel
  , -- No documentation found for Nested "VkCommandBufferAllocateInfo" "commandBufferCount"
  vkCommandBufferCount :: Word32
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

instance Zero VkCommandBufferAllocateInfo where
  zero = VkCommandBufferAllocateInfo zero
                                     zero
                                     zero
                                     zero
                                     zero
-- No documentation found for TopLevel "VkCommandBufferBeginInfo"
data VkCommandBufferBeginInfo = VkCommandBufferBeginInfo
  { -- No documentation found for Nested "VkCommandBufferBeginInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkCommandBufferBeginInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkCommandBufferBeginInfo" "flags"
  vkFlags :: VkCommandBufferUsageFlags
  , -- No documentation found for Nested "VkCommandBufferBeginInfo" "pInheritanceInfo"
  vkPInheritanceInfo :: Ptr VkCommandBufferInheritanceInfo
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

instance Zero VkCommandBufferBeginInfo where
  zero = VkCommandBufferBeginInfo zero
                                  zero
                                  zero
                                  zero
-- No documentation found for TopLevel "VkCommandBufferInheritanceInfo"
data VkCommandBufferInheritanceInfo = VkCommandBufferInheritanceInfo
  { -- No documentation found for Nested "VkCommandBufferInheritanceInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkCommandBufferInheritanceInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkCommandBufferInheritanceInfo" "renderPass"
  vkRenderPass :: VkRenderPass
  , -- No documentation found for Nested "VkCommandBufferInheritanceInfo" "subpass"
  vkSubpass :: Word32
  , -- No documentation found for Nested "VkCommandBufferInheritanceInfo" "framebuffer"
  vkFramebuffer :: VkFramebuffer
  , -- No documentation found for Nested "VkCommandBufferInheritanceInfo" "occlusionQueryEnable"
  vkOcclusionQueryEnable :: VkBool32
  , -- No documentation found for Nested "VkCommandBufferInheritanceInfo" "queryFlags"
  vkQueryFlags :: VkQueryControlFlags
  , -- No documentation found for Nested "VkCommandBufferInheritanceInfo" "pipelineStatistics"
  vkPipelineStatistics :: VkQueryPipelineStatisticFlags
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

instance Zero VkCommandBufferInheritanceInfo where
  zero = VkCommandBufferInheritanceInfo zero
                                        zero
                                        zero
                                        zero
                                        zero
                                        zero
                                        zero
                                        zero
-- ** VkCommandBufferLevel

-- No documentation found for TopLevel "VkCommandBufferLevel"
newtype VkCommandBufferLevel = VkCommandBufferLevel Int32
  deriving (Eq, Ord, Storable, Zero)

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

-- No documentation found for Nested "VkCommandBufferLevel" "VK_COMMAND_BUFFER_LEVEL_PRIMARY"
pattern VK_COMMAND_BUFFER_LEVEL_PRIMARY :: VkCommandBufferLevel
pattern VK_COMMAND_BUFFER_LEVEL_PRIMARY = VkCommandBufferLevel 0

-- No documentation found for Nested "VkCommandBufferLevel" "VK_COMMAND_BUFFER_LEVEL_SECONDARY"
pattern VK_COMMAND_BUFFER_LEVEL_SECONDARY :: VkCommandBufferLevel
pattern VK_COMMAND_BUFFER_LEVEL_SECONDARY = VkCommandBufferLevel 1
-- ** VkCommandBufferResetFlagBits

-- No documentation found for TopLevel "VkCommandBufferResetFlagBits"
newtype VkCommandBufferResetFlagBits = VkCommandBufferResetFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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

-- No documentation found for Nested "VkCommandBufferResetFlagBits" "VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT"
pattern VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT :: VkCommandBufferResetFlagBits
pattern VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT = VkCommandBufferResetFlagBits 0x00000001
-- No documentation found for TopLevel "VkCommandBufferResetFlags"
type VkCommandBufferResetFlags = VkCommandBufferResetFlagBits
-- ** VkCommandBufferUsageFlagBits

-- No documentation found for TopLevel "VkCommandBufferUsageFlagBits"
newtype VkCommandBufferUsageFlagBits = VkCommandBufferUsageFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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

-- No documentation found for Nested "VkCommandBufferUsageFlagBits" "VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT"
pattern VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT :: VkCommandBufferUsageFlagBits
pattern VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT = VkCommandBufferUsageFlagBits 0x00000001

-- No documentation found for Nested "VkCommandBufferUsageFlagBits" "VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT"
pattern VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT :: VkCommandBufferUsageFlagBits
pattern VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT = VkCommandBufferUsageFlagBits 0x00000002

-- No documentation found for Nested "VkCommandBufferUsageFlagBits" "VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT"
pattern VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT :: VkCommandBufferUsageFlagBits
pattern VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT = VkCommandBufferUsageFlagBits 0x00000004
-- No documentation found for TopLevel "VkCommandBufferUsageFlags"
type VkCommandBufferUsageFlags = VkCommandBufferUsageFlagBits
-- ** VkQueryControlFlagBits

-- No documentation found for TopLevel "VkQueryControlFlagBits"
newtype VkQueryControlFlagBits = VkQueryControlFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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

-- No documentation found for Nested "VkQueryControlFlagBits" "VK_QUERY_CONTROL_PRECISE_BIT"
pattern VK_QUERY_CONTROL_PRECISE_BIT :: VkQueryControlFlagBits
pattern VK_QUERY_CONTROL_PRECISE_BIT = VkQueryControlFlagBits 0x00000001
-- No documentation found for TopLevel "VkQueryControlFlags"
type VkQueryControlFlags = VkQueryControlFlagBits
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkAllocateCommandBuffers"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkAllocateCommandBuffers" vkAllocateCommandBuffers :: ("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkCommandBufferAllocateInfo) -> ("pCommandBuffers" ::: Ptr VkCommandBuffer) -> IO VkResult

#endif
type FN_vkAllocateCommandBuffers = ("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkCommandBufferAllocateInfo) -> ("pCommandBuffers" ::: Ptr VkCommandBuffer) -> IO VkResult
type PFN_vkAllocateCommandBuffers = FunPtr FN_vkAllocateCommandBuffers
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkBeginCommandBuffer"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkBeginCommandBuffer" vkBeginCommandBuffer :: ("commandBuffer" ::: VkCommandBuffer) -> ("pBeginInfo" ::: Ptr VkCommandBufferBeginInfo) -> IO VkResult

#endif
type FN_vkBeginCommandBuffer = ("commandBuffer" ::: VkCommandBuffer) -> ("pBeginInfo" ::: Ptr VkCommandBufferBeginInfo) -> IO VkResult
type PFN_vkBeginCommandBuffer = FunPtr FN_vkBeginCommandBuffer
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkEndCommandBuffer"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkEndCommandBuffer" vkEndCommandBuffer :: ("commandBuffer" ::: VkCommandBuffer) -> IO VkResult

#endif
type FN_vkEndCommandBuffer = ("commandBuffer" ::: VkCommandBuffer) -> IO VkResult
type PFN_vkEndCommandBuffer = FunPtr FN_vkEndCommandBuffer
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkFreeCommandBuffers"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkFreeCommandBuffers" vkFreeCommandBuffers :: ("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("commandBufferCount" ::: Word32) -> ("pCommandBuffers" ::: Ptr VkCommandBuffer) -> IO ()

#endif
type FN_vkFreeCommandBuffers = ("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("commandBufferCount" ::: Word32) -> ("pCommandBuffers" ::: Ptr VkCommandBuffer) -> IO ()
type PFN_vkFreeCommandBuffers = FunPtr FN_vkFreeCommandBuffers
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkResetCommandBuffer"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkResetCommandBuffer" vkResetCommandBuffer :: ("commandBuffer" ::: VkCommandBuffer) -> ("flags" ::: VkCommandBufferResetFlags) -> IO VkResult

#endif
type FN_vkResetCommandBuffer = ("commandBuffer" ::: VkCommandBuffer) -> ("flags" ::: VkCommandBufferResetFlags) -> IO VkResult
type PFN_vkResetCommandBuffer = FunPtr FN_vkResetCommandBuffer
