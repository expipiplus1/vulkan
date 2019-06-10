{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Core10.CommandBuffer
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  CommandBufferAllocateInfo(..)
  , 
  CommandBufferBeginInfo(..)
  , CommandBufferInheritanceInfo(..)
#endif
  , CommandBufferLevel
  , pattern COMMAND_BUFFER_LEVEL_PRIMARY
  , pattern COMMAND_BUFFER_LEVEL_SECONDARY
  , CommandBufferResetFlagBits
  , pattern COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT
  , CommandBufferResetFlags
  , CommandBufferUsageFlagBits
  , pattern COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
  , pattern COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT
  , pattern COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT
  , CommandBufferUsageFlags
  , QueryControlFlagBits
  , pattern QUERY_CONTROL_PRECISE_BIT
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

#if defined(VK_USE_PLATFORM_GGP)
import Data.Word
  ( Word32
  )
#endif
import Foreign.Marshal.Array
  ( allocaArray
  )
import Foreign.Marshal.Utils
  ( with
  )
import Foreign.Storable
  ( peekElemOff
  )


import Graphics.Vulkan.C.Core10.CommandBuffer
  ( VkCommandBufferLevel(..)
  , VkCommandBufferResetFlagBits(..)
  , VkCommandBufferUsageFlagBits(..)
  , VkQueryControlFlagBits(..)
  , vkAllocateCommandBuffers
  , vkBeginCommandBuffer
  , vkEndCommandBuffer
  , vkFreeCommandBuffers
  , vkResetCommandBuffer
  , pattern VK_COMMAND_BUFFER_LEVEL_PRIMARY
  , pattern VK_COMMAND_BUFFER_LEVEL_SECONDARY
  , pattern VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT
  , pattern VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
  , pattern VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT
  , pattern VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT
  , pattern VK_QUERY_CONTROL_PRECISE_BIT
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.Core10.CommandPool
  ( CommandPool
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Pass
  ( Framebuffer
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Pipeline
  ( RenderPass
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Query
  ( QueryPipelineStatisticFlags
  )
#endif
import Graphics.Vulkan.Core10.Queue
  ( CommandBuffer(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkCommandBufferAllocateInfo"
data CommandBufferAllocateInfo = CommandBufferAllocateInfo
  { -- No documentation found for Nested "CommandBufferAllocateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "CommandBufferAllocateInfo" "commandPool"
  commandPool :: CommandPool
  , -- No documentation found for Nested "CommandBufferAllocateInfo" "level"
  level :: CommandBufferLevel
  , -- No documentation found for Nested "CommandBufferAllocateInfo" "commandBufferCount"
  commandBufferCount :: Word32
  }
  deriving (Show, Eq)

instance Zero CommandBufferAllocateInfo where
  zero = CommandBufferAllocateInfo Nothing
                                   zero
                                   zero
                                   zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkCommandBufferBeginInfo"
data CommandBufferBeginInfo = CommandBufferBeginInfo
  { -- No documentation found for Nested "CommandBufferBeginInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "CommandBufferBeginInfo" "flags"
  flags :: CommandBufferUsageFlags
  , -- No documentation found for Nested "CommandBufferBeginInfo" "pInheritanceInfo"
  inheritanceInfo :: Maybe CommandBufferInheritanceInfo
  }
  deriving (Show, Eq)

instance Zero CommandBufferBeginInfo where
  zero = CommandBufferBeginInfo Nothing
                                zero
                                Nothing

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkCommandBufferInheritanceInfo"
data CommandBufferInheritanceInfo = CommandBufferInheritanceInfo
  { -- No documentation found for Nested "CommandBufferInheritanceInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "CommandBufferInheritanceInfo" "renderPass"
  renderPass :: RenderPass
  , -- No documentation found for Nested "CommandBufferInheritanceInfo" "subpass"
  subpass :: Word32
  , -- No documentation found for Nested "CommandBufferInheritanceInfo" "framebuffer"
  framebuffer :: Framebuffer
  , -- No documentation found for Nested "CommandBufferInheritanceInfo" "occlusionQueryEnable"
  occlusionQueryEnable :: Bool
  , -- No documentation found for Nested "CommandBufferInheritanceInfo" "queryFlags"
  queryFlags :: QueryControlFlags
  , -- No documentation found for Nested "CommandBufferInheritanceInfo" "pipelineStatistics"
  pipelineStatistics :: QueryPipelineStatisticFlags
  }
  deriving (Show, Eq)

instance Zero CommandBufferInheritanceInfo where
  zero = CommandBufferInheritanceInfo Nothing
                                      zero
                                      zero
                                      zero
                                      False
                                      zero
                                      zero

#endif

-- No documentation found for TopLevel "CommandBufferLevel"
type CommandBufferLevel = VkCommandBufferLevel


{-# complete COMMAND_BUFFER_LEVEL_PRIMARY, COMMAND_BUFFER_LEVEL_SECONDARY :: CommandBufferLevel #-}


-- No documentation found for Nested "CommandBufferLevel" "COMMAND_BUFFER_LEVEL_PRIMARY"
pattern COMMAND_BUFFER_LEVEL_PRIMARY :: (a ~ CommandBufferLevel) => a
pattern COMMAND_BUFFER_LEVEL_PRIMARY = VK_COMMAND_BUFFER_LEVEL_PRIMARY


-- No documentation found for Nested "CommandBufferLevel" "COMMAND_BUFFER_LEVEL_SECONDARY"
pattern COMMAND_BUFFER_LEVEL_SECONDARY :: (a ~ CommandBufferLevel) => a
pattern COMMAND_BUFFER_LEVEL_SECONDARY = VK_COMMAND_BUFFER_LEVEL_SECONDARY

-- No documentation found for TopLevel "CommandBufferResetFlagBits"
type CommandBufferResetFlagBits = VkCommandBufferResetFlagBits


{-# complete COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT :: CommandBufferResetFlagBits #-}


-- No documentation found for Nested "CommandBufferResetFlagBits" "COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT"
pattern COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT :: (a ~ CommandBufferResetFlagBits) => a
pattern COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT = VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT

-- No documentation found for TopLevel "CommandBufferResetFlags"
type CommandBufferResetFlags = CommandBufferResetFlagBits

-- No documentation found for TopLevel "CommandBufferUsageFlagBits"
type CommandBufferUsageFlagBits = VkCommandBufferUsageFlagBits


{-# complete COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT, COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT, COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT :: CommandBufferUsageFlagBits #-}


-- No documentation found for Nested "CommandBufferUsageFlagBits" "COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT"
pattern COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT :: (a ~ CommandBufferUsageFlagBits) => a
pattern COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT


-- No documentation found for Nested "CommandBufferUsageFlagBits" "COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT"
pattern COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT :: (a ~ CommandBufferUsageFlagBits) => a
pattern COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT = VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT


-- No documentation found for Nested "CommandBufferUsageFlagBits" "COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT"
pattern COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT :: (a ~ CommandBufferUsageFlagBits) => a
pattern COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT = VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT

-- No documentation found for TopLevel "CommandBufferUsageFlags"
type CommandBufferUsageFlags = CommandBufferUsageFlagBits

-- No documentation found for TopLevel "QueryControlFlagBits"
type QueryControlFlagBits = VkQueryControlFlagBits


{-# complete QUERY_CONTROL_PRECISE_BIT :: QueryControlFlagBits #-}


-- No documentation found for Nested "QueryControlFlagBits" "QUERY_CONTROL_PRECISE_BIT"
pattern QUERY_CONTROL_PRECISE_BIT :: (a ~ QueryControlFlagBits) => a
pattern QUERY_CONTROL_PRECISE_BIT = VK_QUERY_CONTROL_PRECISE_BIT

-- No documentation found for TopLevel "QueryControlFlags"
type QueryControlFlags = QueryControlFlagBits


-- No documentation found for TopLevel "vkAllocateCommandBuffers"
allocateCommandBuffers :: Device ->  CommandBufferAllocateInfo ->  IO (Vector CommandBuffer)
allocateCommandBuffers = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkBeginCommandBuffer"
beginCommandBuffer :: CommandBuffer ->  CommandBufferBeginInfo ->  IO ()
beginCommandBuffer = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkEndCommandBuffer"
endCommandBuffer :: CommandBuffer ->  IO ()
endCommandBuffer = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkFreeCommandBuffers"
freeCommandBuffers :: Device ->  CommandPool ->  Vector CommandBuffer ->  IO ()
freeCommandBuffers = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkResetCommandBuffer"
resetCommandBuffer :: CommandBuffer ->  CommandBufferResetFlags ->  IO ()
resetCommandBuffer = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- | A safe wrapper for 'beginCommandBuffer' and 'endCommandBuffer' using 'bracket_'
--
-- The allocated value must not be returned from the provided computation
useCommandBuffer
  :: CommandBuffer -> CommandBufferBeginInfo -> IO a -> IO a
useCommandBuffer commandBuffer commandBufferBeginInfo = bracket_
  (beginCommandBuffer commandBuffer commandBufferBeginInfo)
  ( endCommandBuffer commandBuffer)

-- | A safe wrapper for 'allocateCommandBuffers' and 'freeCommandBuffers' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withCommandBuffers
  :: Device -> CommandBufferAllocateInfo -> (Vector CommandBuffer -> IO a) -> IO a
withCommandBuffers device commandBufferAllocateInfo = bracket
  (allocateCommandBuffers device commandBufferAllocateInfo)
  (\o -> freeCommandBuffers device (commandPool (commandBufferAllocateInfo :: CommandBufferAllocateInfo))  o)
