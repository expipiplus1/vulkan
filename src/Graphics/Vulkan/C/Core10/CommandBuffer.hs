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
  , FN_vkAllocateCommandBuffers
  , PFN_vkAllocateCommandBuffers
  , vkAllocateCommandBuffers
  , FN_vkBeginCommandBuffer
  , PFN_vkBeginCommandBuffer
  , vkBeginCommandBuffer
  , FN_vkEndCommandBuffer
  , PFN_vkEndCommandBuffer
  , vkEndCommandBuffer
  , FN_vkFreeCommandBuffers
  , PFN_vkFreeCommandBuffers
  , vkFreeCommandBuffers
  , FN_vkResetCommandBuffer
  , PFN_vkResetCommandBuffer
  , vkResetCommandBuffer
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
  , pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
  , pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
  , pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO
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
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | VkCommandBufferAllocateInfo - Structure specifying the allocation
-- parameters for command buffer object
--
-- == Valid Usage
--
-- Unresolved directive in VkCommandBufferAllocateInfo.txt -
-- include::{generated}\/validity\/structs\/VkCommandBufferAllocateInfo.txt[]
--
-- = See Also
--
-- 'VkCommandBufferLevel',
-- 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkAllocateCommandBuffers'
data VkCommandBufferAllocateInfo = VkCommandBufferAllocateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @commandPool@ is the command pool from which the command buffers are
  -- allocated.
  vkCommandPool :: VkCommandPool
  , -- | @level@ is a 'VkCommandBufferLevel' value specifying the command buffer
  -- level.
  vkLevel :: VkCommandBufferLevel
  , -- | @commandBufferCount@ /must/ be greater than @0@
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
  zero = VkCommandBufferAllocateInfo VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
                                     zero
                                     zero
                                     zero
                                     zero

-- | VkCommandBufferBeginInfo - Structure specifying a command buffer begin
-- operation
--
-- == Valid Usage
--
-- -   If @flags@ contains
--     'VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT', the @renderPass@
--     member of @pInheritanceInfo@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Pipeline.VkRenderPass'
--
-- -   If @flags@ contains
--     'VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT', the @subpass@
--     member of @pInheritanceInfo@ /must/ be a valid subpass index within
--     the @renderPass@ member of @pInheritanceInfo@
--
-- -   If @flags@ contains
--     'VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT', the
--     @framebuffer@ member of @pInheritanceInfo@ /must/ be either
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', or a valid
--     'Graphics.Vulkan.C.Core10.Pass.VkFramebuffer' that is compatible
--     with the @renderPass@ member of @pInheritanceInfo@
--
-- Unresolved directive in VkCommandBufferBeginInfo.txt -
-- include::{generated}\/validity\/structs\/VkCommandBufferBeginInfo.txt[]
--
-- = See Also
--
-- 'VkCommandBufferInheritanceInfo', 'VkCommandBufferUsageFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType', 'vkBeginCommandBuffer'
data VkCommandBufferBeginInfo = VkCommandBufferBeginInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is a bitmask of 'VkCommandBufferUsageFlagBits' specifying usage
  -- behavior for the command buffer.
  vkFlags :: VkCommandBufferUsageFlags
  , -- | @pInheritanceInfo@ is a pointer to a 'VkCommandBufferInheritanceInfo'
  -- structure, which is used if @commandBuffer@ is a secondary command
  -- buffer. If this is a primary command buffer, then this value is ignored.
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
  zero = VkCommandBufferBeginInfo VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
                                  zero
                                  zero
                                  zero

-- | VkCommandBufferInheritanceInfo - Structure specifying command buffer
-- inheritance info
--
-- == Valid Usage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-inheritedQueries inherited queries>
--     feature is not enabled, @occlusionQueryEnable@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_FALSE'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-inheritedQueries inherited queries>
--     feature is enabled, @queryFlags@ /must/ be a valid combination of
--     'VkQueryControlFlagBits' values
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-pipelineStatisticsQuery pipeline statistics queries>
--     feature is not enabled, @pipelineStatistics@ /must/ be @0@
--
-- Unresolved directive in VkCommandBufferInheritanceInfo.txt -
-- include::{generated}\/validity\/structs\/VkCommandBufferInheritanceInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32', 'VkCommandBufferBeginInfo',
-- 'Graphics.Vulkan.C.Core10.Pass.VkFramebuffer', 'VkQueryControlFlags',
-- 'Graphics.Vulkan.C.Core10.Query.VkQueryPipelineStatisticFlags',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkRenderPass',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkCommandBufferInheritanceInfo = VkCommandBufferInheritanceInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @renderPass@ is a 'Graphics.Vulkan.C.Core10.Pipeline.VkRenderPass'
  -- object defining which render passes the
  -- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' will be
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#renderpass-compatibility compatible>
  -- with and /can/ be executed within. If the
  -- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' will not be executed
  -- within a render pass instance, @renderPass@ is ignored.
  vkRenderPass :: VkRenderPass
  , -- | @subpass@ is the index of the subpass within the render pass instance
  -- that the 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' will be
  -- executed within. If the 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer'
  -- will not be executed within a render pass instance, @subpass@ is
  -- ignored.
  vkSubpass :: Word32
  , -- | @framebuffer@ optionally refers to the
  -- 'Graphics.Vulkan.C.Core10.Pass.VkFramebuffer' object that the
  -- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' will be rendering to if
  -- it is executed within a render pass instance. It /can/ be
  -- 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE' if the framebuffer
  -- is not known, or if the 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer'
  -- will not be executed within a render pass instance.
  --
  -- __Note__
  --
  -- Specifying the exact framebuffer that the secondary command buffer will
  -- be executed with /may/ result in better performance at command buffer
  -- execution time.
  vkFramebuffer :: VkFramebuffer
  , -- | @occlusionQueryEnable@ specifies whether the command buffer /can/ be
  -- executed while an occlusion query is active in the primary command
  -- buffer. If this is 'Graphics.Vulkan.C.Core10.Core.VK_TRUE', then this
  -- command buffer /can/ be executed whether the primary command buffer has
  -- an occlusion query active or not. If this is
  -- 'Graphics.Vulkan.C.Core10.Core.VK_FALSE', then the primary command
  -- buffer /must/ not have an occlusion query active.
  vkOcclusionQueryEnable :: VkBool32
  , -- | @queryFlags@ specifies the query flags that /can/ be used by an active
  -- occlusion query in the primary command buffer when this secondary
  -- command buffer is executed. If this value includes the
  -- 'VK_QUERY_CONTROL_PRECISE_BIT' bit, then the active query /can/ return
  -- boolean results or actual sample counts. If this bit is not set, then
  -- the active query /must/ not use the 'VK_QUERY_CONTROL_PRECISE_BIT' bit.
  vkQueryFlags :: VkQueryControlFlags
  , -- | @pipelineStatistics@ is a bitmask of
  -- 'Graphics.Vulkan.C.Core10.Query.VkQueryPipelineStatisticFlagBits'
  -- specifying the set of pipeline statistics that /can/ be counted by an
  -- active query in the primary command buffer when this secondary command
  -- buffer is executed. If this value includes a given bit, then this
  -- command buffer /can/ be executed whether the primary command buffer has
  -- a pipeline statistics query active that includes this bit or not. If
  -- this value excludes a given bit, then the active pipeline statistics
  -- query /must/ not be from a query pool that counts that statistic.
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
  zero = VkCommandBufferInheritanceInfo VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO
                                        zero
                                        zero
                                        zero
                                        zero
                                        zero
                                        zero
                                        zero

-- ** VkCommandBufferLevel

-- | VkCommandBufferLevel - Enumerant specifying a command buffer level
--
-- = See Also
--
-- 'VkCommandBufferAllocateInfo'
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

-- | 'VK_COMMAND_BUFFER_LEVEL_PRIMARY' specifies a primary command buffer.
pattern VK_COMMAND_BUFFER_LEVEL_PRIMARY :: VkCommandBufferLevel
pattern VK_COMMAND_BUFFER_LEVEL_PRIMARY = VkCommandBufferLevel 0

-- | 'VK_COMMAND_BUFFER_LEVEL_SECONDARY' specifies a secondary command
-- buffer.
pattern VK_COMMAND_BUFFER_LEVEL_SECONDARY :: VkCommandBufferLevel
pattern VK_COMMAND_BUFFER_LEVEL_SECONDARY = VkCommandBufferLevel 1

-- ** VkCommandBufferResetFlagBits

-- | VkCommandBufferResetFlagBits - Bitmask controlling behavior of a command
-- buffer reset
--
-- = See Also
--
-- 'VkCommandBufferResetFlags'
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

-- | 'VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT' specifies that most or
-- all memory resources currently owned by the command buffer /should/ be
-- returned to the parent command pool. If this flag is not set, then the
-- command buffer /may/ hold onto memory resources and reuse them when
-- recording commands. @commandBuffer@ is moved to the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle initial state>.
pattern VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT :: VkCommandBufferResetFlagBits
pattern VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT = VkCommandBufferResetFlagBits 0x00000001

-- | VkCommandBufferResetFlags - Bitmask of VkCommandBufferResetFlagBits
--
-- = Description
--
-- 'VkCommandBufferResetFlags' is a bitmask type for setting a mask of zero
-- or more 'VkCommandBufferResetFlagBits'.
--
-- = See Also
--
-- 'VkCommandBufferResetFlagBits', 'vkResetCommandBuffer'
type VkCommandBufferResetFlags = VkCommandBufferResetFlagBits

-- ** VkCommandBufferUsageFlagBits

-- | VkCommandBufferUsageFlagBits - Bitmask specifying usage behavior for
-- command buffer
--
-- = See Also
--
-- 'VkCommandBufferUsageFlags'
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

-- | 'VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT' specifies that each
-- recording of the command buffer will only be submitted once, and the
-- command buffer will be reset and recorded again between each submission.
pattern VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT :: VkCommandBufferUsageFlagBits
pattern VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT = VkCommandBufferUsageFlagBits 0x00000001

-- | 'VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT' specifies that a
-- secondary command buffer is considered to be entirely inside a render
-- pass. If this is a primary command buffer, then this bit is ignored.
pattern VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT :: VkCommandBufferUsageFlagBits
pattern VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT = VkCommandBufferUsageFlagBits 0x00000002

-- | 'VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT' specifies that a command
-- buffer /can/ be resubmitted to a queue while it is in the /pending
-- state/, and recorded into multiple primary command buffers.
pattern VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT :: VkCommandBufferUsageFlagBits
pattern VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT = VkCommandBufferUsageFlagBits 0x00000004

-- | VkCommandBufferUsageFlags - Bitmask of VkCommandBufferUsageFlagBits
--
-- = Description
--
-- 'VkCommandBufferUsageFlags' is a bitmask type for setting a mask of zero
-- or more 'VkCommandBufferUsageFlagBits'.
--
-- = See Also
--
-- 'VkCommandBufferBeginInfo', 'VkCommandBufferUsageFlagBits'
type VkCommandBufferUsageFlags = VkCommandBufferUsageFlagBits

-- ** VkQueryControlFlagBits

-- | VkQueryControlFlagBits - Bitmask specifying constraints on a query
--
-- = See Also
--
-- 'VkQueryControlFlags'
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

-- | 'VK_QUERY_CONTROL_PRECISE_BIT' specifies the precision of
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#queries-occlusion occlusion queries>.
pattern VK_QUERY_CONTROL_PRECISE_BIT :: VkQueryControlFlagBits
pattern VK_QUERY_CONTROL_PRECISE_BIT = VkQueryControlFlagBits 0x00000001

-- | VkQueryControlFlags - Bitmask of VkQueryControlFlagBits
--
-- = Description
--
-- 'VkQueryControlFlags' is a bitmask type for setting a mask of zero or
-- more 'VkQueryControlFlagBits'.
--
-- = See Also
--
-- 'VkCommandBufferInheritanceInfo', 'VkQueryControlFlagBits',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBeginQuery'
type VkQueryControlFlags = VkQueryControlFlagBits

-- | vkAllocateCommandBuffers - Allocate command buffers from an existing
-- command pool
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the command pool.
--
-- -   @pAllocateInfo@ is a pointer to an instance of the
--     'VkCommandBufferAllocateInfo' structure describing parameters of the
--     allocation.
--
-- -   @pCommandBuffers@ is a pointer to an array of
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handles in which
--     the resulting command buffer objects are returned. The array /must/
--     be at least the length specified by the @commandBufferCount@ member
--     of @pAllocateInfo@. Each allocated command buffer begins in the
--     initial state.
--
-- = Description
--
-- 'vkAllocateCommandBuffers' /can/ be used to create multiple command
-- buffers. If the creation of any of those command buffers fails, the
-- implementation /must/ destroy all successfully created command buffer
-- objects from this command, set all entries of the @pCommandBuffers@
-- array to @NULL@ and return the error.
--
-- When command buffers are first allocated, they are in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle initial state>.
--
-- Unresolved directive in vkAllocateCommandBuffers.txt -
-- include::{generated}\/validity\/protos\/vkAllocateCommandBuffers.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'VkCommandBufferAllocateInfo',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkAllocateCommandBuffers" vkAllocateCommandBuffers :: ("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkCommandBufferAllocateInfo) -> ("pCommandBuffers" ::: Ptr VkCommandBuffer) -> IO VkResult
#else
vkAllocateCommandBuffers :: DeviceCmds -> ("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkCommandBufferAllocateInfo) -> ("pCommandBuffers" ::: Ptr VkCommandBuffer) -> IO VkResult
vkAllocateCommandBuffers deviceCmds = mkVkAllocateCommandBuffers (pVkAllocateCommandBuffers deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkAllocateCommandBuffers
  :: FunPtr (("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkCommandBufferAllocateInfo) -> ("pCommandBuffers" ::: Ptr VkCommandBuffer) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkCommandBufferAllocateInfo) -> ("pCommandBuffers" ::: Ptr VkCommandBuffer) -> IO VkResult)
#endif

type FN_vkAllocateCommandBuffers = ("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkCommandBufferAllocateInfo) -> ("pCommandBuffers" ::: Ptr VkCommandBuffer) -> IO VkResult
type PFN_vkAllocateCommandBuffers = FunPtr FN_vkAllocateCommandBuffers

-- | vkBeginCommandBuffer - Start recording a command buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the handle of the command buffer which is to be
--     put in the recording state.
--
-- -   @pBeginInfo@ is an instance of the 'VkCommandBufferBeginInfo'
--     structure, which defines additional information about how the
--     command buffer begins recording.
--
-- == Valid Usage
--
-- -   @commandBuffer@ /must/ not be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle recording or pending state>.
--
-- -   If @commandBuffer@ was allocated from a
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' which did not
--     have the
--     'Graphics.Vulkan.C.Core10.CommandPool.VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT'
--     flag set, @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle initial state>.
--
-- -   If @commandBuffer@ is a secondary command buffer, the
--     @pInheritanceInfo@ member of @pBeginInfo@ /must/ be a valid
--     'VkCommandBufferInheritanceInfo' structure
--
-- -   If @commandBuffer@ is a secondary command buffer and either the
--     @occlusionQueryEnable@ member of the @pInheritanceInfo@ member of
--     @pBeginInfo@ is 'Graphics.Vulkan.C.Core10.Core.VK_FALSE', or the
--     precise occlusion queries feature is not enabled, the @queryFlags@
--     member of the @pInheritanceInfo@ member @pBeginInfo@ /must/ not
--     contain 'VK_QUERY_CONTROL_PRECISE_BIT'
--
-- Unresolved directive in vkBeginCommandBuffer.txt -
-- include::{generated}\/validity\/protos\/vkBeginCommandBuffer.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'VkCommandBufferBeginInfo'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkBeginCommandBuffer" vkBeginCommandBuffer :: ("commandBuffer" ::: VkCommandBuffer) -> ("pBeginInfo" ::: Ptr VkCommandBufferBeginInfo) -> IO VkResult
#else
vkBeginCommandBuffer :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> ("pBeginInfo" ::: Ptr VkCommandBufferBeginInfo) -> IO VkResult
vkBeginCommandBuffer deviceCmds = mkVkBeginCommandBuffer (pVkBeginCommandBuffer deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkBeginCommandBuffer
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pBeginInfo" ::: Ptr VkCommandBufferBeginInfo) -> IO VkResult) -> (("commandBuffer" ::: VkCommandBuffer) -> ("pBeginInfo" ::: Ptr VkCommandBufferBeginInfo) -> IO VkResult)
#endif

type FN_vkBeginCommandBuffer = ("commandBuffer" ::: VkCommandBuffer) -> ("pBeginInfo" ::: Ptr VkCommandBufferBeginInfo) -> IO VkResult
type PFN_vkBeginCommandBuffer = FunPtr FN_vkBeginCommandBuffer

-- | vkEndCommandBuffer - Finish recording a command buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer to complete recording.
--
-- = Description
--
-- If there was an error during recording, the application will be notified
-- by an unsuccessful return code returned by 'vkEndCommandBuffer'. If the
-- application wishes to further use the command buffer, the command buffer
-- /must/ be reset. The command buffer /must/ have been in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>,
-- and is moved to the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle executable state>.
--
-- == Valid Usage
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>.
--
-- -   If @commandBuffer@ is a primary command buffer, there /must/ not be
--     an active render pass instance
--
-- -   All queries made
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#queries-operation-active active>
--     during the recording of @commandBuffer@ /must/ have been made
--     inactive
--
-- -   Conditional rendering must not be
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#active-conditional-rendering active>
--
-- -   If @commandBuffer@ is a secondary command buffer, there /must/ not
--     be an outstanding
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.vkCmdBeginDebugUtilsLabelEXT'
--     command recorded to @commandBuffer@ that has not previously been
--     ended by a call to
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.vkCmdEndDebugUtilsLabelEXT'.
--
-- -   If @commandBuffer@ is a secondary command buffer, there /must/ not
--     be an outstanding
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_marker.vkCmdDebugMarkerBeginEXT'
--     command recorded to @commandBuffer@ that has not previously been
--     ended by a call to
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_marker.vkCmdDebugMarkerEndEXT'.
--
-- Unresolved directive in vkEndCommandBuffer.txt -
-- include::{generated}\/validity\/protos\/vkEndCommandBuffer.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkEndCommandBuffer" vkEndCommandBuffer :: ("commandBuffer" ::: VkCommandBuffer) -> IO VkResult
#else
vkEndCommandBuffer :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> IO VkResult
vkEndCommandBuffer deviceCmds = mkVkEndCommandBuffer (pVkEndCommandBuffer deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEndCommandBuffer
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> IO VkResult) -> (("commandBuffer" ::: VkCommandBuffer) -> IO VkResult)
#endif

type FN_vkEndCommandBuffer = ("commandBuffer" ::: VkCommandBuffer) -> IO VkResult
type PFN_vkEndCommandBuffer = FunPtr FN_vkEndCommandBuffer

-- | vkFreeCommandBuffers - Free command buffers
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the command pool.
--
-- -   @commandPool@ is the command pool from which the command buffers
--     were allocated.
--
-- -   @commandBufferCount@ is the length of the @pCommandBuffers@ array.
--
-- -   @pCommandBuffers@ is an array of handles of command buffers to free.
--
-- = Description
--
-- Any primary command buffer that is in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle recording or executable state>
-- and has any element of @pCommandBuffers@ recorded into it, becomes
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle invalid>.
--
-- == Valid Usage
--
-- -   All elements of @pCommandBuffers@ /must/ not be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>
--
-- -   @pCommandBuffers@ /must/ be a valid pointer to an array of
--     @commandBufferCount@
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handles, each
--     element of which /must/ either be a valid handle or @NULL@
--
-- Unresolved directive in vkFreeCommandBuffers.txt -
-- include::{generated}\/validity\/protos\/vkFreeCommandBuffers.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkFreeCommandBuffers" vkFreeCommandBuffers :: ("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("commandBufferCount" ::: Word32) -> ("pCommandBuffers" ::: Ptr VkCommandBuffer) -> IO ()
#else
vkFreeCommandBuffers :: DeviceCmds -> ("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("commandBufferCount" ::: Word32) -> ("pCommandBuffers" ::: Ptr VkCommandBuffer) -> IO ()
vkFreeCommandBuffers deviceCmds = mkVkFreeCommandBuffers (pVkFreeCommandBuffers deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkFreeCommandBuffers
  :: FunPtr (("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("commandBufferCount" ::: Word32) -> ("pCommandBuffers" ::: Ptr VkCommandBuffer) -> IO ()) -> (("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("commandBufferCount" ::: Word32) -> ("pCommandBuffers" ::: Ptr VkCommandBuffer) -> IO ())
#endif

type FN_vkFreeCommandBuffers = ("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("commandBufferCount" ::: Word32) -> ("pCommandBuffers" ::: Ptr VkCommandBuffer) -> IO ()
type PFN_vkFreeCommandBuffers = FunPtr FN_vkFreeCommandBuffers

-- | vkResetCommandBuffer - Reset a command buffer to the initial state
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer to reset. The command buffer
--     /can/ be in any state other than
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle pending>,
--     and is moved into the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle initial state>.
--
-- -   @flags@ is a bitmask of 'VkCommandBufferResetFlagBits' controlling
--     the reset operation.
--
-- = Description
--
-- Any primary command buffer that is in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle recording or executable state>
-- and has @commandBuffer@ recorded into it, becomes
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle invalid>.
--
-- == Valid Usage
--
-- Unresolved directive in vkResetCommandBuffer.txt -
-- include::{generated}\/validity\/protos\/vkResetCommandBuffer.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'VkCommandBufferResetFlags'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkResetCommandBuffer" vkResetCommandBuffer :: ("commandBuffer" ::: VkCommandBuffer) -> ("flags" ::: VkCommandBufferResetFlags) -> IO VkResult
#else
vkResetCommandBuffer :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> ("flags" ::: VkCommandBufferResetFlags) -> IO VkResult
vkResetCommandBuffer deviceCmds = mkVkResetCommandBuffer (pVkResetCommandBuffer deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkResetCommandBuffer
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("flags" ::: VkCommandBufferResetFlags) -> IO VkResult) -> (("commandBuffer" ::: VkCommandBuffer) -> ("flags" ::: VkCommandBufferResetFlags) -> IO VkResult)
#endif

type FN_vkResetCommandBuffer = ("commandBuffer" ::: VkCommandBuffer) -> ("flags" ::: VkCommandBufferResetFlags) -> IO VkResult
type PFN_vkResetCommandBuffer = FunPtr FN_vkResetCommandBuffer
