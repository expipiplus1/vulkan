{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Extensions.VK_EXT_conditional_rendering
  ( withCStructCommandBufferInheritanceConditionalRenderingInfoEXT
  , fromCStructCommandBufferInheritanceConditionalRenderingInfoEXT
  , CommandBufferInheritanceConditionalRenderingInfoEXT(..)
  , withCStructConditionalRenderingBeginInfoEXT
  , fromCStructConditionalRenderingBeginInfoEXT
  , ConditionalRenderingBeginInfoEXT(..)
  , ConditionalRenderingFlagBitsEXT
  , pattern CONDITIONAL_RENDERING_INVERTED_BIT_EXT
  , ConditionalRenderingFlagsEXT
  , withCStructPhysicalDeviceConditionalRenderingFeaturesEXT
  , fromCStructPhysicalDeviceConditionalRenderingFeaturesEXT
  , PhysicalDeviceConditionalRenderingFeaturesEXT(..)
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
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering
  ( VkCommandBufferInheritanceConditionalRenderingInfoEXT(..)
  , VkConditionalRenderingBeginInfoEXT(..)
  , VkConditionalRenderingFlagBitsEXT(..)
  , VkPhysicalDeviceConditionalRenderingFeaturesEXT(..)
  , vkCmdBeginConditionalRenderingEXT
  , vkCmdEndConditionalRenderingEXT
  , pattern VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT
  , pattern VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME
  , pattern VK_EXT_CONDITIONAL_RENDERING_SPEC_VERSION
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



-- | VkCommandBufferInheritanceConditionalRenderingInfoEXT - Structure
-- specifying command buffer inheritance info
--
-- = Description
--
-- If this structure is not present, the behavior is as if
-- @conditionalRenderingEnable@ is
-- 'Graphics.Vulkan.C.Core10.Core.VK_FALSE'.
--
-- == Valid Usage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-inheritedConditionalRendering inherited conditional rendering>
--     feature is not enabled, @conditionalRenderingEnable@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_FALSE'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering.VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data CommandBufferInheritanceConditionalRenderingInfoEXT = CommandBufferInheritanceConditionalRenderingInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "CommandBufferInheritanceConditionalRenderingInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "CommandBufferInheritanceConditionalRenderingInfoEXT" "conditionalRenderingEnable"
  conditionalRenderingEnable :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkCommandBufferInheritanceConditionalRenderingInfoEXT' and
-- marshal a 'CommandBufferInheritanceConditionalRenderingInfoEXT' into it. The 'VkCommandBufferInheritanceConditionalRenderingInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructCommandBufferInheritanceConditionalRenderingInfoEXT :: CommandBufferInheritanceConditionalRenderingInfoEXT -> (VkCommandBufferInheritanceConditionalRenderingInfoEXT -> IO a) -> IO a
withCStructCommandBufferInheritanceConditionalRenderingInfoEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: CommandBufferInheritanceConditionalRenderingInfoEXT)) (\pPNext -> cont (VkCommandBufferInheritanceConditionalRenderingInfoEXT VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT pPNext (boolToBool32 (conditionalRenderingEnable (marshalled :: CommandBufferInheritanceConditionalRenderingInfoEXT)))))

-- | A function to read a 'VkCommandBufferInheritanceConditionalRenderingInfoEXT' and all additional
-- structures in the pointer chain into a 'CommandBufferInheritanceConditionalRenderingInfoEXT'.
fromCStructCommandBufferInheritanceConditionalRenderingInfoEXT :: VkCommandBufferInheritanceConditionalRenderingInfoEXT -> IO CommandBufferInheritanceConditionalRenderingInfoEXT
fromCStructCommandBufferInheritanceConditionalRenderingInfoEXT c = CommandBufferInheritanceConditionalRenderingInfoEXT <$> -- Univalued Member elided
                                                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkCommandBufferInheritanceConditionalRenderingInfoEXT)))
                                                                                                                       <*> pure (bool32ToBool (vkConditionalRenderingEnable (c :: VkCommandBufferInheritanceConditionalRenderingInfoEXT)))

instance Zero CommandBufferInheritanceConditionalRenderingInfoEXT where
  zero = CommandBufferInheritanceConditionalRenderingInfoEXT Nothing
                                                             False



-- | VkConditionalRenderingBeginInfoEXT - Structure specifying conditional
-- rendering begin info
--
-- = Description
--
-- If the 32-bit value at @offset@ in @buffer@ memory is zero, then the
-- rendering commands are discarded, otherwise they are executed as normal.
-- If the value of the predicate in buffer memory changes while conditional
-- rendering is active, the rendering commands /may/ be discarded in an
-- implementation-dependent way. Some implementations may latch the value
-- of the predicate upon beginning conditional rendering while others may
-- read it before every rendering command.
--
-- == Valid Usage
--
-- -   If @buffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- -   @buffer@ /must/ have been created with the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering.VK_BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT'
--     bit set
--
-- -   @offset@ /must/ be less than the size of @buffer@ by at least 32
--     bits.
--
-- -   @offset@ /must/ be a multiple of 4
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering.VK_STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @buffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- -   @flags@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering.VkConditionalRenderingFlagBitsEXT'
--     values
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering.VkConditionalRenderingFlagsEXT',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering.vkCmdBeginConditionalRenderingEXT'
data ConditionalRenderingBeginInfoEXT = ConditionalRenderingBeginInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "ConditionalRenderingBeginInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ConditionalRenderingBeginInfoEXT" "buffer"
  buffer :: Buffer
  , -- No documentation found for Nested "ConditionalRenderingBeginInfoEXT" "offset"
  offset :: DeviceSize
  , -- No documentation found for Nested "ConditionalRenderingBeginInfoEXT" "flags"
  flags :: ConditionalRenderingFlagsEXT
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkConditionalRenderingBeginInfoEXT' and
-- marshal a 'ConditionalRenderingBeginInfoEXT' into it. The 'VkConditionalRenderingBeginInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructConditionalRenderingBeginInfoEXT :: ConditionalRenderingBeginInfoEXT -> (VkConditionalRenderingBeginInfoEXT -> IO a) -> IO a
withCStructConditionalRenderingBeginInfoEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: ConditionalRenderingBeginInfoEXT)) (\pPNext -> cont (VkConditionalRenderingBeginInfoEXT VK_STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT pPNext (buffer (marshalled :: ConditionalRenderingBeginInfoEXT)) (offset (marshalled :: ConditionalRenderingBeginInfoEXT)) (flags (marshalled :: ConditionalRenderingBeginInfoEXT))))

-- | A function to read a 'VkConditionalRenderingBeginInfoEXT' and all additional
-- structures in the pointer chain into a 'ConditionalRenderingBeginInfoEXT'.
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


-- | VkConditionalRenderingFlagBitsEXT - Specify the behavior of conditional
-- rendering
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering.VkConditionalRenderingFlagsEXT'
type ConditionalRenderingFlagBitsEXT = VkConditionalRenderingFlagBitsEXT


{-# complete CONDITIONAL_RENDERING_INVERTED_BIT_EXT :: ConditionalRenderingFlagBitsEXT #-}


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering.VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT'
-- specifies the condition used to determine whether to discard rendering
-- commands or not. That is, if the 32-bit predicate read from @buffer@
-- memory at @offset@ is zero, the rendering commands are not discarded,
-- and if non zero, then they are discarded.
pattern CONDITIONAL_RENDERING_INVERTED_BIT_EXT :: (a ~ ConditionalRenderingFlagBitsEXT) => a
pattern CONDITIONAL_RENDERING_INVERTED_BIT_EXT = VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT

-- | VkConditionalRenderingFlagsEXT - Bitmask of
-- VkConditionalRenderingFlagBitsEXT
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering.VkConditionalRenderingFlagsEXT'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering.VkConditionalRenderingFlagBitsEXT'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering.VkConditionalRenderingBeginInfoEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering.VkConditionalRenderingFlagBitsEXT'
type ConditionalRenderingFlagsEXT = ConditionalRenderingFlagBitsEXT


-- | VkPhysicalDeviceConditionalRenderingFeaturesEXT - Structure describing
-- if a secondary command buffer can be executed if conditional rendering
-- is active in the primary command buffer
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering.VkPhysicalDeviceConditionalRenderingFeaturesEXT'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2',
-- it is filled with values indicating the implementation-dependent
-- behavior.
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering.VkPhysicalDeviceConditionalRenderingFeaturesEXT'
-- /can/ also be used in @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to enable the
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PhysicalDeviceConditionalRenderingFeaturesEXT = PhysicalDeviceConditionalRenderingFeaturesEXT
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceConditionalRenderingFeaturesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceConditionalRenderingFeaturesEXT" "conditionalRendering"
  conditionalRendering :: Bool
  , -- No documentation found for Nested "PhysicalDeviceConditionalRenderingFeaturesEXT" "inheritedConditionalRendering"
  inheritedConditionalRendering :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceConditionalRenderingFeaturesEXT' and
-- marshal a 'PhysicalDeviceConditionalRenderingFeaturesEXT' into it. The 'VkPhysicalDeviceConditionalRenderingFeaturesEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceConditionalRenderingFeaturesEXT :: PhysicalDeviceConditionalRenderingFeaturesEXT -> (VkPhysicalDeviceConditionalRenderingFeaturesEXT -> IO a) -> IO a
withCStructPhysicalDeviceConditionalRenderingFeaturesEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceConditionalRenderingFeaturesEXT)) (\pPNext -> cont (VkPhysicalDeviceConditionalRenderingFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT pPNext (boolToBool32 (conditionalRendering (marshalled :: PhysicalDeviceConditionalRenderingFeaturesEXT))) (boolToBool32 (inheritedConditionalRendering (marshalled :: PhysicalDeviceConditionalRenderingFeaturesEXT)))))

-- | A function to read a 'VkPhysicalDeviceConditionalRenderingFeaturesEXT' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceConditionalRenderingFeaturesEXT'.
fromCStructPhysicalDeviceConditionalRenderingFeaturesEXT :: VkPhysicalDeviceConditionalRenderingFeaturesEXT -> IO PhysicalDeviceConditionalRenderingFeaturesEXT
fromCStructPhysicalDeviceConditionalRenderingFeaturesEXT c = PhysicalDeviceConditionalRenderingFeaturesEXT <$> -- Univalued Member elided
                                                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceConditionalRenderingFeaturesEXT)))
                                                                                                           <*> pure (bool32ToBool (vkConditionalRendering (c :: VkPhysicalDeviceConditionalRenderingFeaturesEXT)))
                                                                                                           <*> pure (bool32ToBool (vkInheritedConditionalRendering (c :: VkPhysicalDeviceConditionalRenderingFeaturesEXT)))

instance Zero PhysicalDeviceConditionalRenderingFeaturesEXT where
  zero = PhysicalDeviceConditionalRenderingFeaturesEXT Nothing
                                                       False
                                                       False



-- | vkCmdBeginConditionalRenderingEXT - Define the beginning of a
-- conditional rendering block
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which this command will
--     be recorded.
--
-- -   @pConditionalRenderingBegin@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering.VkConditionalRenderingBeginInfoEXT'
--     structure specifying the parameters of conditional rendering.
--
-- == Valid Usage
--
-- -   Conditional rendering /must/ not already be
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#active-conditional-rendering active>
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @pConditionalRenderingBegin@ /must/ be a valid pointer to a valid
--     'Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering.VkConditionalRenderingBeginInfoEXT'
--     structure
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics, or
--     compute operations
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering.VkConditionalRenderingBeginInfoEXT'
cmdBeginConditionalRenderingEXT :: CommandBuffer ->  ConditionalRenderingBeginInfoEXT ->  IO ()
cmdBeginConditionalRenderingEXT = \(CommandBuffer commandBuffer' commandTable) -> \conditionalRenderingBegin' -> (\marshalled -> withCStructConditionalRenderingBeginInfoEXT marshalled . flip with) conditionalRenderingBegin' (\pConditionalRenderingBegin' -> vkCmdBeginConditionalRenderingEXT commandTable commandBuffer' pConditionalRenderingBegin' *> (pure ()))


-- | vkCmdEndConditionalRenderingEXT - Define the end of a conditional
-- rendering block
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which this command will
--     be recorded.
--
-- = Description
--
-- Once ended, conditional rendering becomes inactive.
--
-- == Valid Usage
--
-- -   Conditional rendering /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#active-conditional-rendering active>
--
-- -   If conditional rendering was made
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#active-conditional-rendering active>
--     outside of a render pass instance, it must not be ended inside a
--     render pass instance
--
-- -   If conditional rendering was made
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#active-conditional-rendering active>
--     within a subpass it must be ended in the same subpass
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics, or
--     compute operations
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer'
cmdEndConditionalRenderingEXT :: CommandBuffer ->  IO ()
cmdEndConditionalRenderingEXT = \(CommandBuffer commandBuffer' commandTable) -> vkCmdEndConditionalRenderingEXT commandTable commandBuffer' *> (pure ())

-- No documentation found for TopLevel "VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME"
pattern EXT_CONDITIONAL_RENDERING_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_CONDITIONAL_RENDERING_EXTENSION_NAME = VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_CONDITIONAL_RENDERING_SPEC_VERSION"
pattern EXT_CONDITIONAL_RENDERING_SPEC_VERSION :: Integral a => a
pattern EXT_CONDITIONAL_RENDERING_SPEC_VERSION = VK_EXT_CONDITIONAL_RENDERING_SPEC_VERSION
