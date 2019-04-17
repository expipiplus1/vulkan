{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_conditional_rendering
  ( withCStructCommandBufferInheritanceConditionalRenderingInfoEXT
  , fromCStructCommandBufferInheritanceConditionalRenderingInfoEXT
  , CommandBufferInheritanceConditionalRenderingInfoEXT(..)
  , withCStructConditionalRenderingBeginInfoEXT
  , fromCStructConditionalRenderingBeginInfoEXT
  , ConditionalRenderingBeginInfoEXT(..)
  , ConditionalRenderingFlagBitsEXT
  , ConditionalRenderingFlagsEXT
  , withCStructPhysicalDeviceConditionalRenderingFeaturesEXT
  , fromCStructPhysicalDeviceConditionalRenderingFeaturesEXT
  , PhysicalDeviceConditionalRenderingFeaturesEXT(..)
  , cmdBeginConditionalRenderingEXT
  , cmdEndConditionalRenderingEXT
  , pattern VK_EXT_CONDITIONAL_RENDERING_SPEC_VERSION
  , pattern VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT
  , pattern VK_ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT
  , pattern VK_BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT
  , pattern VK_PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT
  ) where

import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( cmdBeginConditionalRenderingEXT
  , cmdEndConditionalRenderingEXT
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering
  ( VkCommandBufferInheritanceConditionalRenderingInfoEXT(..)
  , VkConditionalRenderingBeginInfoEXT(..)
  , VkConditionalRenderingFlagBitsEXT(..)
  , VkPhysicalDeviceConditionalRenderingFeaturesEXT(..)
  , pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( DeviceSize
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( Buffer
  )
import Graphics.Vulkan.Core10.Queue
  ( CommandBuffer(..)
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering
  ( pattern VK_ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT
  , pattern VK_BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT
  , pattern VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME
  , pattern VK_EXT_CONDITIONAL_RENDERING_SPEC_VERSION
  , pattern VK_PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT
  )


-- No documentation found for TopLevel "CommandBufferInheritanceConditionalRenderingInfoEXT"
data CommandBufferInheritanceConditionalRenderingInfoEXT = CommandBufferInheritanceConditionalRenderingInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "CommandBufferInheritanceConditionalRenderingInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "CommandBufferInheritanceConditionalRenderingInfoEXT" "conditionalRenderingEnable"
  vkConditionalRenderingEnable :: Bool
  }
  deriving (Show, Eq)
withCStructCommandBufferInheritanceConditionalRenderingInfoEXT :: CommandBufferInheritanceConditionalRenderingInfoEXT -> (VkCommandBufferInheritanceConditionalRenderingInfoEXT -> IO a) -> IO a
withCStructCommandBufferInheritanceConditionalRenderingInfoEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: CommandBufferInheritanceConditionalRenderingInfoEXT)) (\pPNext -> cont (VkCommandBufferInheritanceConditionalRenderingInfoEXT VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT pPNext (boolToBool32 (vkConditionalRenderingEnable (from :: CommandBufferInheritanceConditionalRenderingInfoEXT)))))
fromCStructCommandBufferInheritanceConditionalRenderingInfoEXT :: VkCommandBufferInheritanceConditionalRenderingInfoEXT -> IO CommandBufferInheritanceConditionalRenderingInfoEXT
fromCStructCommandBufferInheritanceConditionalRenderingInfoEXT c = CommandBufferInheritanceConditionalRenderingInfoEXT <$> -- Univalued Member elided
                                                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkCommandBufferInheritanceConditionalRenderingInfoEXT)))
                                                                                                                       <*> pure (bool32ToBool (vkConditionalRenderingEnable (c :: VkCommandBufferInheritanceConditionalRenderingInfoEXT)))
instance Zero CommandBufferInheritanceConditionalRenderingInfoEXT where
  zero = CommandBufferInheritanceConditionalRenderingInfoEXT Nothing
                                                             False
-- No documentation found for TopLevel "ConditionalRenderingBeginInfoEXT"
data ConditionalRenderingBeginInfoEXT = ConditionalRenderingBeginInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "ConditionalRenderingBeginInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ConditionalRenderingBeginInfoEXT" "buffer"
  vkBuffer :: Buffer
  , -- No documentation found for Nested "ConditionalRenderingBeginInfoEXT" "offset"
  vkOffset :: DeviceSize
  , -- No documentation found for Nested "ConditionalRenderingBeginInfoEXT" "flags"
  vkFlags :: ConditionalRenderingFlagsEXT
  }
  deriving (Show, Eq)
withCStructConditionalRenderingBeginInfoEXT :: ConditionalRenderingBeginInfoEXT -> (VkConditionalRenderingBeginInfoEXT -> IO a) -> IO a
withCStructConditionalRenderingBeginInfoEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: ConditionalRenderingBeginInfoEXT)) (\pPNext -> cont (VkConditionalRenderingBeginInfoEXT VK_STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT pPNext (vkBuffer (from :: ConditionalRenderingBeginInfoEXT)) (vkOffset (from :: ConditionalRenderingBeginInfoEXT)) (vkFlags (from :: ConditionalRenderingBeginInfoEXT))))
fromCStructConditionalRenderingBeginInfoEXT :: VkConditionalRenderingBeginInfoEXT -> IO ConditionalRenderingBeginInfoEXT
fromCStructConditionalRenderingBeginInfoEXT c = ConditionalRenderingBeginInfoEXT <$> -- Univalued Member elided
                                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkConditionalRenderingBeginInfoEXT)))
                                                                                 <*> pure (vkBuffer (c :: VkConditionalRenderingBeginInfoEXT))
                                                                                 <*> pure (vkOffset (c :: VkConditionalRenderingBeginInfoEXT))
                                                                                 <*> pure (vkFlags (c :: VkConditionalRenderingBeginInfoEXT))
instance Zero ConditionalRenderingBeginInfoEXT where
  zero = ConditionalRenderingBeginInfoEXT Nothing
                                          zero
                                          zero
                                          zero
-- No documentation found for TopLevel "ConditionalRenderingFlagBitsEXT"
type ConditionalRenderingFlagBitsEXT = VkConditionalRenderingFlagBitsEXT
-- No documentation found for TopLevel "ConditionalRenderingFlagsEXT"
type ConditionalRenderingFlagsEXT = ConditionalRenderingFlagBitsEXT
-- No documentation found for TopLevel "PhysicalDeviceConditionalRenderingFeaturesEXT"
data PhysicalDeviceConditionalRenderingFeaturesEXT = PhysicalDeviceConditionalRenderingFeaturesEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceConditionalRenderingFeaturesEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceConditionalRenderingFeaturesEXT" "conditionalRendering"
  vkConditionalRendering :: Bool
  , -- No documentation found for Nested "PhysicalDeviceConditionalRenderingFeaturesEXT" "inheritedConditionalRendering"
  vkInheritedConditionalRendering :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceConditionalRenderingFeaturesEXT :: PhysicalDeviceConditionalRenderingFeaturesEXT -> (VkPhysicalDeviceConditionalRenderingFeaturesEXT -> IO a) -> IO a
withCStructPhysicalDeviceConditionalRenderingFeaturesEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceConditionalRenderingFeaturesEXT)) (\pPNext -> cont (VkPhysicalDeviceConditionalRenderingFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT pPNext (boolToBool32 (vkConditionalRendering (from :: PhysicalDeviceConditionalRenderingFeaturesEXT))) (boolToBool32 (vkInheritedConditionalRendering (from :: PhysicalDeviceConditionalRenderingFeaturesEXT)))))
fromCStructPhysicalDeviceConditionalRenderingFeaturesEXT :: VkPhysicalDeviceConditionalRenderingFeaturesEXT -> IO PhysicalDeviceConditionalRenderingFeaturesEXT
fromCStructPhysicalDeviceConditionalRenderingFeaturesEXT c = PhysicalDeviceConditionalRenderingFeaturesEXT <$> -- Univalued Member elided
                                                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceConditionalRenderingFeaturesEXT)))
                                                                                                           <*> pure (bool32ToBool (vkConditionalRendering (c :: VkPhysicalDeviceConditionalRenderingFeaturesEXT)))
                                                                                                           <*> pure (bool32ToBool (vkInheritedConditionalRendering (c :: VkPhysicalDeviceConditionalRenderingFeaturesEXT)))
instance Zero PhysicalDeviceConditionalRenderingFeaturesEXT where
  zero = PhysicalDeviceConditionalRenderingFeaturesEXT Nothing
                                                       False
                                                       False

-- | Wrapper for 'vkCmdBeginConditionalRenderingEXT'
cmdBeginConditionalRenderingEXT :: CommandBuffer ->  ConditionalRenderingBeginInfoEXT ->  IO ()
cmdBeginConditionalRenderingEXT = \(CommandBuffer commandBuffer commandTable) -> \conditionalRenderingBegin -> (\a -> withCStructConditionalRenderingBeginInfoEXT a . flip with) conditionalRenderingBegin (\pConditionalRenderingBegin -> Graphics.Vulkan.C.Dynamic.cmdBeginConditionalRenderingEXT commandTable commandBuffer pConditionalRenderingBegin *> (pure ()))

-- | Wrapper for 'vkCmdEndConditionalRenderingEXT'
cmdEndConditionalRenderingEXT :: CommandBuffer ->  IO ()
cmdEndConditionalRenderingEXT = \(CommandBuffer commandBuffer commandTable) -> Graphics.Vulkan.C.Dynamic.cmdEndConditionalRenderingEXT commandTable commandBuffer *> (pure ())
