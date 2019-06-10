{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Extensions.VK_EXT_conditional_rendering
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  CommandBufferInheritanceConditionalRenderingInfoEXT(..)
  , 
  ConditionalRenderingBeginInfoEXT(..)
#endif
  , ConditionalRenderingFlagBitsEXT
  , pattern CONDITIONAL_RENDERING_INVERTED_BIT_EXT
  , ConditionalRenderingFlagsEXT
#if defined(VK_USE_PLATFORM_GGP)
  , PhysicalDeviceConditionalRenderingFeaturesEXT(..)
#endif
  , cmdBeginConditionalRenderingEXT
  , cmdEndConditionalRenderingEXT
  , pattern EXT_CONDITIONAL_RENDERING_EXTENSION_NAME
  , pattern EXT_CONDITIONAL_RENDERING_SPEC_VERSION
  , pattern STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT
  , pattern STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT
  , pattern ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT
  , pattern BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT
  , pattern PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT
  ) where

import Data.String
  ( IsString
  )
import Foreign.Marshal.Utils
  ( with
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering
  ( VkConditionalRenderingFlagBitsEXT(..)
  , vkCmdBeginConditionalRenderingEXT
  , vkCmdEndConditionalRenderingEXT
  , pattern VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT
  , pattern VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME
  , pattern VK_EXT_CONDITIONAL_RENDERING_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DeviceInitialization
  ( DeviceSize
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.MemoryManagement
  ( Buffer
  )
#endif
import Graphics.Vulkan.Core10.Queue
  ( CommandBuffer(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Buffer
  ( pattern BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT
  , pattern STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT
  )
import Graphics.Vulkan.Core10.Pass
  ( pattern ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT
  )
import Graphics.Vulkan.Core10.Queue
  ( pattern PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkCommandBufferInheritanceConditionalRenderingInfoEXT"
data CommandBufferInheritanceConditionalRenderingInfoEXT = CommandBufferInheritanceConditionalRenderingInfoEXT
  { -- No documentation found for Nested "CommandBufferInheritanceConditionalRenderingInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "CommandBufferInheritanceConditionalRenderingInfoEXT" "conditionalRenderingEnable"
  conditionalRenderingEnable :: Bool
  }
  deriving (Show, Eq)

instance Zero CommandBufferInheritanceConditionalRenderingInfoEXT where
  zero = CommandBufferInheritanceConditionalRenderingInfoEXT Nothing
                                                             False

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkConditionalRenderingBeginInfoEXT"
data ConditionalRenderingBeginInfoEXT = ConditionalRenderingBeginInfoEXT
  { -- No documentation found for Nested "ConditionalRenderingBeginInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ConditionalRenderingBeginInfoEXT" "buffer"
  buffer :: Buffer
  , -- No documentation found for Nested "ConditionalRenderingBeginInfoEXT" "offset"
  offset :: DeviceSize
  , -- No documentation found for Nested "ConditionalRenderingBeginInfoEXT" "flags"
  flags :: ConditionalRenderingFlagsEXT
  }
  deriving (Show, Eq)

instance Zero ConditionalRenderingBeginInfoEXT where
  zero = ConditionalRenderingBeginInfoEXT Nothing
                                          zero
                                          zero
                                          zero

#endif

-- No documentation found for TopLevel "ConditionalRenderingFlagBitsEXT"
type ConditionalRenderingFlagBitsEXT = VkConditionalRenderingFlagBitsEXT


{-# complete CONDITIONAL_RENDERING_INVERTED_BIT_EXT :: ConditionalRenderingFlagBitsEXT #-}


-- No documentation found for Nested "ConditionalRenderingFlagBitsEXT" "CONDITIONAL_RENDERING_INVERTED_BIT_EXT"
pattern CONDITIONAL_RENDERING_INVERTED_BIT_EXT :: (a ~ ConditionalRenderingFlagBitsEXT) => a
pattern CONDITIONAL_RENDERING_INVERTED_BIT_EXT = VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT

-- No documentation found for TopLevel "ConditionalRenderingFlagsEXT"
type ConditionalRenderingFlagsEXT = ConditionalRenderingFlagBitsEXT


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceConditionalRenderingFeaturesEXT"
data PhysicalDeviceConditionalRenderingFeaturesEXT = PhysicalDeviceConditionalRenderingFeaturesEXT
  { -- No documentation found for Nested "PhysicalDeviceConditionalRenderingFeaturesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceConditionalRenderingFeaturesEXT" "conditionalRendering"
  conditionalRendering :: Bool
  , -- No documentation found for Nested "PhysicalDeviceConditionalRenderingFeaturesEXT" "inheritedConditionalRendering"
  inheritedConditionalRendering :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceConditionalRenderingFeaturesEXT where
  zero = PhysicalDeviceConditionalRenderingFeaturesEXT Nothing
                                                       False
                                                       False

#endif


-- No documentation found for TopLevel "vkCmdBeginConditionalRenderingEXT"
cmdBeginConditionalRenderingEXT :: CommandBuffer ->  ConditionalRenderingBeginInfoEXT ->  IO ()
cmdBeginConditionalRenderingEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdEndConditionalRenderingEXT"
cmdEndConditionalRenderingEXT :: CommandBuffer ->  IO ()
cmdEndConditionalRenderingEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME"
pattern EXT_CONDITIONAL_RENDERING_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_CONDITIONAL_RENDERING_EXTENSION_NAME = VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_CONDITIONAL_RENDERING_SPEC_VERSION"
pattern EXT_CONDITIONAL_RENDERING_SPEC_VERSION :: Integral a => a
pattern EXT_CONDITIONAL_RENDERING_SPEC_VERSION = VK_EXT_CONDITIONAL_RENDERING_SPEC_VERSION
