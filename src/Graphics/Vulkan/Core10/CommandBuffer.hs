{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.CommandBuffer
  ( withCStructCommandBufferAllocateInfo
  , fromCStructCommandBufferAllocateInfo
  , CommandBufferAllocateInfo(..)
  , withCStructCommandBufferBeginInfo
  , fromCStructCommandBufferBeginInfo
  , CommandBufferBeginInfo(..)
  , withCStructCommandBufferInheritanceInfo
  , fromCStructCommandBufferInheritanceInfo
  , CommandBufferInheritanceInfo(..)
  , CommandBufferLevel
  , CommandBufferResetFlagBits
  , CommandBufferResetFlags
  , CommandBufferUsageFlagBits
  , CommandBufferUsageFlags
  , QueryControlFlagBits
  , QueryControlFlags
  , allocateCommandBuffers
  , beginCommandBuffer
  , endCommandBuffer
  , freeCommandBuffers
  , resetCommandBuffer
  , useCommandBuffer
  , withCommandBuffers
  ) where

import Control.Exception
  ( bracket
  , bracket_
  , throwIO
  )
import Control.Monad
  ( (<=<)
  , when
  )
import Data.Function
  ( (&)
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( generateM
  , length
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Array
  ( allocaArray
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( allocateCommandBuffers
  , beginCommandBuffer
  , endCommandBuffer
  , freeCommandBuffers
  , resetCommandBuffer
  )


import Graphics.Vulkan.C.Core10.CommandBuffer
  ( VkCommandBufferAllocateInfo(..)
  , VkCommandBufferBeginInfo(..)
  , VkCommandBufferInheritanceInfo(..)
  , VkCommandBufferLevel(..)
  , VkCommandBufferResetFlagBits(..)
  , VkCommandBufferUsageFlagBits(..)
  , VkQueryControlFlagBits(..)
  )
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
  , pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
  , pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.Core10.CommandPool
  ( CommandPool
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )
import Graphics.Vulkan.Core10.Pass
  ( Framebuffer
  )
import Graphics.Vulkan.Core10.Pipeline
  ( RenderPass
  )
import Graphics.Vulkan.Core10.Query
  ( QueryPipelineStatisticFlags
  )
import Graphics.Vulkan.Core10.Queue
  ( CommandBuffer(..)
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )


-- No documentation found for TopLevel "CommandBufferAllocateInfo"
data CommandBufferAllocateInfo = CommandBufferAllocateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "CommandBufferAllocateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "CommandBufferAllocateInfo" "commandPool"
  vkCommandPool :: CommandPool
  , -- No documentation found for Nested "CommandBufferAllocateInfo" "level"
  vkLevel :: CommandBufferLevel
  , -- No documentation found for Nested "CommandBufferAllocateInfo" "commandBufferCount"
  vkCommandBufferCount :: Word32
  }
  deriving (Show, Eq)
withCStructCommandBufferAllocateInfo :: CommandBufferAllocateInfo -> (VkCommandBufferAllocateInfo -> IO a) -> IO a
withCStructCommandBufferAllocateInfo from cont = maybeWith withSomeVkStruct (vkPNext (from :: CommandBufferAllocateInfo)) (\pPNext -> cont (VkCommandBufferAllocateInfo VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO pPNext (vkCommandPool (from :: CommandBufferAllocateInfo)) (vkLevel (from :: CommandBufferAllocateInfo)) (vkCommandBufferCount (from :: CommandBufferAllocateInfo))))
fromCStructCommandBufferAllocateInfo :: VkCommandBufferAllocateInfo -> IO CommandBufferAllocateInfo
fromCStructCommandBufferAllocateInfo c = CommandBufferAllocateInfo <$> -- Univalued Member elided
                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkCommandBufferAllocateInfo)))
                                                                   <*> pure (vkCommandPool (c :: VkCommandBufferAllocateInfo))
                                                                   <*> pure (vkLevel (c :: VkCommandBufferAllocateInfo))
                                                                   <*> pure (vkCommandBufferCount (c :: VkCommandBufferAllocateInfo))
instance Zero CommandBufferAllocateInfo where
  zero = CommandBufferAllocateInfo Nothing
                                   zero
                                   zero
                                   zero
-- No documentation found for TopLevel "CommandBufferBeginInfo"
data CommandBufferBeginInfo = CommandBufferBeginInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "CommandBufferBeginInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "CommandBufferBeginInfo" "flags"
  vkFlags :: CommandBufferUsageFlags
  , -- No documentation found for Nested "CommandBufferBeginInfo" "pInheritanceInfo"
  vkPInheritanceInfo :: Maybe CommandBufferInheritanceInfo
  }
  deriving (Show, Eq)
withCStructCommandBufferBeginInfo :: CommandBufferBeginInfo -> (VkCommandBufferBeginInfo -> IO a) -> IO a
withCStructCommandBufferBeginInfo from cont = maybeWith (\a -> withCStructCommandBufferInheritanceInfo a . flip with) (vkPInheritanceInfo (from :: CommandBufferBeginInfo)) (\pInheritanceInfo -> maybeWith withSomeVkStruct (vkPNext (from :: CommandBufferBeginInfo)) (\pPNext -> cont (VkCommandBufferBeginInfo VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO pPNext (vkFlags (from :: CommandBufferBeginInfo)) pInheritanceInfo)))
fromCStructCommandBufferBeginInfo :: VkCommandBufferBeginInfo -> IO CommandBufferBeginInfo
fromCStructCommandBufferBeginInfo c = CommandBufferBeginInfo <$> -- Univalued Member elided
                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkCommandBufferBeginInfo)))
                                                             <*> pure (vkFlags (c :: VkCommandBufferBeginInfo))
                                                             <*> maybePeek (fromCStructCommandBufferInheritanceInfo <=< peek) (vkPInheritanceInfo (c :: VkCommandBufferBeginInfo))
instance Zero CommandBufferBeginInfo where
  zero = CommandBufferBeginInfo Nothing
                                zero
                                Nothing
-- No documentation found for TopLevel "CommandBufferInheritanceInfo"
data CommandBufferInheritanceInfo = CommandBufferInheritanceInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "CommandBufferInheritanceInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "CommandBufferInheritanceInfo" "renderPass"
  vkRenderPass :: RenderPass
  , -- No documentation found for Nested "CommandBufferInheritanceInfo" "subpass"
  vkSubpass :: Word32
  , -- No documentation found for Nested "CommandBufferInheritanceInfo" "framebuffer"
  vkFramebuffer :: Framebuffer
  , -- No documentation found for Nested "CommandBufferInheritanceInfo" "occlusionQueryEnable"
  vkOcclusionQueryEnable :: Bool
  , -- No documentation found for Nested "CommandBufferInheritanceInfo" "queryFlags"
  vkQueryFlags :: QueryControlFlags
  , -- No documentation found for Nested "CommandBufferInheritanceInfo" "pipelineStatistics"
  vkPipelineStatistics :: QueryPipelineStatisticFlags
  }
  deriving (Show, Eq)
withCStructCommandBufferInheritanceInfo :: CommandBufferInheritanceInfo -> (VkCommandBufferInheritanceInfo -> IO a) -> IO a
withCStructCommandBufferInheritanceInfo from cont = maybeWith withSomeVkStruct (vkPNext (from :: CommandBufferInheritanceInfo)) (\pPNext -> cont (VkCommandBufferInheritanceInfo VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO pPNext (vkRenderPass (from :: CommandBufferInheritanceInfo)) (vkSubpass (from :: CommandBufferInheritanceInfo)) (vkFramebuffer (from :: CommandBufferInheritanceInfo)) (boolToBool32 (vkOcclusionQueryEnable (from :: CommandBufferInheritanceInfo))) (vkQueryFlags (from :: CommandBufferInheritanceInfo)) (vkPipelineStatistics (from :: CommandBufferInheritanceInfo))))
fromCStructCommandBufferInheritanceInfo :: VkCommandBufferInheritanceInfo -> IO CommandBufferInheritanceInfo
fromCStructCommandBufferInheritanceInfo c = CommandBufferInheritanceInfo <$> -- Univalued Member elided
                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkCommandBufferInheritanceInfo)))
                                                                         <*> pure (vkRenderPass (c :: VkCommandBufferInheritanceInfo))
                                                                         <*> pure (vkSubpass (c :: VkCommandBufferInheritanceInfo))
                                                                         <*> pure (vkFramebuffer (c :: VkCommandBufferInheritanceInfo))
                                                                         <*> pure (bool32ToBool (vkOcclusionQueryEnable (c :: VkCommandBufferInheritanceInfo)))
                                                                         <*> pure (vkQueryFlags (c :: VkCommandBufferInheritanceInfo))
                                                                         <*> pure (vkPipelineStatistics (c :: VkCommandBufferInheritanceInfo))
instance Zero CommandBufferInheritanceInfo where
  zero = CommandBufferInheritanceInfo Nothing
                                      zero
                                      zero
                                      zero
                                      False
                                      zero
                                      zero
-- No documentation found for TopLevel "CommandBufferLevel"
type CommandBufferLevel = VkCommandBufferLevel
-- No documentation found for TopLevel "CommandBufferResetFlagBits"
type CommandBufferResetFlagBits = VkCommandBufferResetFlagBits
-- No documentation found for TopLevel "CommandBufferResetFlags"
type CommandBufferResetFlags = CommandBufferResetFlagBits
-- No documentation found for TopLevel "CommandBufferUsageFlagBits"
type CommandBufferUsageFlagBits = VkCommandBufferUsageFlagBits
-- No documentation found for TopLevel "CommandBufferUsageFlags"
type CommandBufferUsageFlags = CommandBufferUsageFlagBits
-- No documentation found for TopLevel "QueryControlFlagBits"
type QueryControlFlagBits = VkQueryControlFlagBits
-- No documentation found for TopLevel "QueryControlFlags"
type QueryControlFlags = QueryControlFlagBits

-- | Wrapper for 'vkAllocateCommandBuffers'
allocateCommandBuffers :: Device ->  CommandBufferAllocateInfo ->  IO (Vector CommandBuffer)
allocateCommandBuffers = \(Device device commandTable) -> \allocateInfo -> allocaArray (fromIntegral (vkCommandBufferCount (allocateInfo :: CommandBufferAllocateInfo))) (\pCommandBuffers -> (\a -> withCStructCommandBufferAllocateInfo a . flip with) allocateInfo (\pAllocateInfo -> Graphics.Vulkan.C.Dynamic.allocateCommandBuffers commandTable device pAllocateInfo pCommandBuffers >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((Data.Vector.generateM (fromIntegral (vkCommandBufferCount (allocateInfo :: CommandBufferAllocateInfo))) ((\p i -> CommandBuffer <$> peekElemOff p i <*> pure commandTable) pCommandBuffers))))))

-- | Wrapper for 'vkBeginCommandBuffer'
beginCommandBuffer :: CommandBuffer ->  CommandBufferBeginInfo ->  IO ()
beginCommandBuffer = \(CommandBuffer commandBuffer commandTable) -> \beginInfo -> (\a -> withCStructCommandBufferBeginInfo a . flip with) beginInfo (\pBeginInfo -> Graphics.Vulkan.C.Dynamic.beginCommandBuffer commandTable commandBuffer pBeginInfo >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure ())))

-- | Wrapper for 'vkEndCommandBuffer'
endCommandBuffer :: CommandBuffer ->  IO ()
endCommandBuffer = \(CommandBuffer commandBuffer commandTable) -> Graphics.Vulkan.C.Dynamic.endCommandBuffer commandTable commandBuffer >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure ()))

-- | Wrapper for 'vkFreeCommandBuffers'
freeCommandBuffers :: Device ->  CommandPool ->  Vector CommandBuffer ->  IO ()
freeCommandBuffers = \(Device device commandTable) -> \commandPool -> \commandBuffers -> withVec ((&) . commandBufferHandle) commandBuffers (\pCommandBuffers -> Graphics.Vulkan.C.Dynamic.freeCommandBuffers commandTable device commandPool (fromIntegral $ Data.Vector.length commandBuffers) pCommandBuffers *> (pure ()))

-- | Wrapper for 'vkResetCommandBuffer'
resetCommandBuffer :: CommandBuffer ->  CommandBufferResetFlags ->  IO ()
resetCommandBuffer = \(CommandBuffer commandBuffer commandTable) -> \flags -> Graphics.Vulkan.C.Dynamic.resetCommandBuffer commandTable commandBuffer flags >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure ()))
-- | Wrapper for 'beginCommandBuffer' and 'endCommandBuffer' using 'bracket_'
useCommandBuffer
  :: CommandBuffer -> CommandBufferBeginInfo -> IO a -> IO a
useCommandBuffer commandBuffer commandBufferBeginInfo = bracket_
  (beginCommandBuffer commandBuffer commandBufferBeginInfo)
  (endCommandBuffer commandBuffer)
-- | Wrapper for 'allocateCommandBuffers' and 'freeCommandBuffers' using 'bracket'
withCommandBuffers
  :: Device -> CommandBufferAllocateInfo -> (Vector (CommandBuffer) -> IO a) -> IO a
withCommandBuffers device commandBufferAllocateInfo = bracket
  (allocateCommandBuffers device commandBufferAllocateInfo)
  (\o -> freeCommandBuffers device (vkCommandPool (commandBufferAllocateInfo :: CommandBufferAllocateInfo))  o)
