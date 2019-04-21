{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering
  ( VkCommandBufferInheritanceConditionalRenderingInfoEXT(..)
  , VkConditionalRenderingBeginInfoEXT(..)
  , VkConditionalRenderingFlagBitsEXT(..)
  , pattern VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT
  , VkConditionalRenderingFlagsEXT
  , VkPhysicalDeviceConditionalRenderingFeaturesEXT(..)
  , FN_vkCmdBeginConditionalRenderingEXT
  , PFN_vkCmdBeginConditionalRenderingEXT
  , vkCmdBeginConditionalRenderingEXT
  , FN_vkCmdEndConditionalRenderingEXT
  , PFN_vkCmdEndConditionalRenderingEXT
  , vkCmdEndConditionalRenderingEXT
  , pattern VK_ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT
  , pattern VK_BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT
  , pattern VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME
  , pattern VK_EXT_CONDITIONAL_RENDERING_SPEC_VERSION
  , pattern VK_PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT
  , pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.String
  ( IsString
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


import Graphics.Vulkan.C.Core10.Buffer
  ( VkBufferUsageFlagBits(..)
  )
import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  , Zero(..)
  , VkFlags
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDeviceSize
  )
import Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkBuffer
  )
import Graphics.Vulkan.C.Core10.Pass
  ( VkAccessFlagBits(..)
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkPipelineStageFlagBits(..)
  , VkCommandBuffer
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
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
-- Unresolved directive in
-- VkCommandBufferInheritanceConditionalRenderingInfoEXT.txt -
-- include::{generated}\/validity\/structs\/VkCommandBufferInheritanceConditionalRenderingInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkCommandBufferInheritanceConditionalRenderingInfoEXT = VkCommandBufferInheritanceConditionalRenderingInfoEXT
  { -- | @sType@ is the type of this structure
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure
  vkPNext :: Ptr ()
  , -- | @conditionalRenderingEnable@ specifies whether the command buffer /can/
  -- be executed while conditional rendering is active in the primary command
  -- buffer. If this is 'Graphics.Vulkan.C.Core10.Core.VK_TRUE', then this
  -- command buffer /can/ be executed whether the primary command buffer has
  -- active conditional rendering or not. If this is
  -- 'Graphics.Vulkan.C.Core10.Core.VK_FALSE', then the primary command
  -- buffer /must/ not have conditional rendering active.
  vkConditionalRenderingEnable :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkCommandBufferInheritanceConditionalRenderingInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkCommandBufferInheritanceConditionalRenderingInfoEXT <$> peek (ptr `plusPtr` 0)
                                                                   <*> peek (ptr `plusPtr` 8)
                                                                   <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkCommandBufferInheritanceConditionalRenderingInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkCommandBufferInheritanceConditionalRenderingInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkConditionalRenderingEnable (poked :: VkCommandBufferInheritanceConditionalRenderingInfoEXT))

instance Zero VkCommandBufferInheritanceConditionalRenderingInfoEXT where
  zero = VkCommandBufferInheritanceConditionalRenderingInfoEXT VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT
                                                               zero
                                                               zero

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
--     'VK_BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT' bit set
--
-- -   @offset@ /must/ be less than the size of @buffer@ by at least 32
--     bits.
--
-- -   @offset@ /must/ be a multiple of 4
--
-- Unresolved directive in VkConditionalRenderingBeginInfoEXT.txt -
-- include::{generated}\/validity\/structs\/VkConditionalRenderingBeginInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkConditionalRenderingBeginInfoEXT = VkConditionalRenderingBeginInfoEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @buffer@ is a buffer containing the predicate for conditional rendering.
  vkBuffer :: VkBuffer
  , -- | @offset@ is the byte offset into @buffer@ where the predicate is
  -- located.
  vkOffset :: VkDeviceSize
  , -- | @flags@ is a bitmask of 'VkConditionalRenderingFlagsEXT' specifying the
  -- behavior of conditional rendering.
  vkFlags :: VkConditionalRenderingFlagsEXT
  }
  deriving (Eq, Show)

instance Storable VkConditionalRenderingBeginInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkConditionalRenderingBeginInfoEXT <$> peek (ptr `plusPtr` 0)
                                                <*> peek (ptr `plusPtr` 8)
                                                <*> peek (ptr `plusPtr` 16)
                                                <*> peek (ptr `plusPtr` 24)
                                                <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkConditionalRenderingBeginInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkConditionalRenderingBeginInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkBuffer (poked :: VkConditionalRenderingBeginInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkOffset (poked :: VkConditionalRenderingBeginInfoEXT))
                *> poke (ptr `plusPtr` 32) (vkFlags (poked :: VkConditionalRenderingBeginInfoEXT))

instance Zero VkConditionalRenderingBeginInfoEXT where
  zero = VkConditionalRenderingBeginInfoEXT VK_STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT
                                            zero
                                            zero
                                            zero
                                            zero

-- ** VkConditionalRenderingFlagBitsEXT

-- | VkConditionalRenderingFlagBitsEXT - Specify the behavior of conditional
-- rendering
--
-- = See Also
--
-- No cross-references are available
newtype VkConditionalRenderingFlagBitsEXT = VkConditionalRenderingFlagBitsEXT VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkConditionalRenderingFlagBitsEXT where
  showsPrec _ VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT = showString "VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT"
  showsPrec p (VkConditionalRenderingFlagBitsEXT x) = showParen (p >= 11) (showString "VkConditionalRenderingFlagBitsEXT " . showsPrec 11 x)

instance Read VkConditionalRenderingFlagBitsEXT where
  readPrec = parens ( choose [ ("VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT", pure VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkConditionalRenderingFlagBitsEXT")
                        v <- step readPrec
                        pure (VkConditionalRenderingFlagBitsEXT v)
                        )
                    )

-- | 'VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT' specifies the condition used
-- to determine whether to discard rendering commands or not. That is, if
-- the 32-bit predicate read from @buffer@ memory at @offset@ is zero, the
-- rendering commands are not discarded, and if non zero, then they are
-- discarded.
pattern VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT :: VkConditionalRenderingFlagBitsEXT
pattern VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT = VkConditionalRenderingFlagBitsEXT 0x00000001

-- | VkConditionalRenderingFlagsEXT - Bitmask of
-- VkConditionalRenderingFlagBitsEXT
--
-- = Description
--
-- 'VkConditionalRenderingFlagsEXT' is a bitmask type for setting a mask of
-- zero or more 'VkConditionalRenderingFlagBitsEXT'.
--
-- = See Also
--
-- No cross-references are available
type VkConditionalRenderingFlagsEXT = VkConditionalRenderingFlagBitsEXT

-- | VkPhysicalDeviceConditionalRenderingFeaturesEXT - Structure describing
-- if a secondary command buffer can be executed if conditional rendering
-- is active in the primary command buffer
--
-- = Description
--
-- If the 'VkPhysicalDeviceConditionalRenderingFeaturesEXT' structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2',
-- it is filled with values indicating the implementation-dependent
-- behavior. 'VkPhysicalDeviceConditionalRenderingFeaturesEXT' /can/ also
-- be used in @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to enable the
-- features.
--
-- Unresolved directive in
-- VkPhysicalDeviceConditionalRenderingFeaturesEXT.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceConditionalRenderingFeaturesEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkPhysicalDeviceConditionalRenderingFeaturesEXT = VkPhysicalDeviceConditionalRenderingFeaturesEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @conditionalRendering@ specifies whether conditional rendering is
  -- supported.
  vkConditionalRendering :: VkBool32
  , -- | @inheritedConditionalRendering@ specifies whether a secondary command
  -- buffer /can/ be executed while conditional rendering is active in the
  -- primary command buffer.
  vkInheritedConditionalRendering :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceConditionalRenderingFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceConditionalRenderingFeaturesEXT <$> peek (ptr `plusPtr` 0)
                                                             <*> peek (ptr `plusPtr` 8)
                                                             <*> peek (ptr `plusPtr` 16)
                                                             <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceConditionalRenderingFeaturesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceConditionalRenderingFeaturesEXT))
                *> poke (ptr `plusPtr` 16) (vkConditionalRendering (poked :: VkPhysicalDeviceConditionalRenderingFeaturesEXT))
                *> poke (ptr `plusPtr` 20) (vkInheritedConditionalRendering (poked :: VkPhysicalDeviceConditionalRenderingFeaturesEXT))

instance Zero VkPhysicalDeviceConditionalRenderingFeaturesEXT where
  zero = VkPhysicalDeviceConditionalRenderingFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT
                                                         zero
                                                         zero
                                                         zero

-- | vkCmdBeginConditionalRenderingEXT - Define the beginning of a
-- conditional rendering block
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which this command will
--     be recorded.
--
-- -   @pConditionalRenderingBegin@ is a pointer to an instance of the
--     'VkConditionalRenderingBeginInfoEXT' structure specifying the
--     parameters of conditional rendering.
--
-- == Valid Usage
--
-- -   Conditional rendering /must/ not already be
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#active-conditional-rendering active>
--
-- Unresolved directive in vkCmdBeginConditionalRenderingEXT.txt -
-- include::{generated}\/validity\/protos\/vkCmdBeginConditionalRenderingEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdBeginConditionalRenderingEXT" vkCmdBeginConditionalRenderingEXT :: ("commandBuffer" ::: VkCommandBuffer) -> ("pConditionalRenderingBegin" ::: Ptr VkConditionalRenderingBeginInfoEXT) -> IO ()
#else
vkCmdBeginConditionalRenderingEXT :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> ("pConditionalRenderingBegin" ::: Ptr VkConditionalRenderingBeginInfoEXT) -> IO ()
vkCmdBeginConditionalRenderingEXT deviceCmds = mkVkCmdBeginConditionalRenderingEXT (pVkCmdBeginConditionalRenderingEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBeginConditionalRenderingEXT
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pConditionalRenderingBegin" ::: Ptr VkConditionalRenderingBeginInfoEXT) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("pConditionalRenderingBegin" ::: Ptr VkConditionalRenderingBeginInfoEXT) -> IO ())
#endif

type FN_vkCmdBeginConditionalRenderingEXT = ("commandBuffer" ::: VkCommandBuffer) -> ("pConditionalRenderingBegin" ::: Ptr VkConditionalRenderingBeginInfoEXT) -> IO ()
type PFN_vkCmdBeginConditionalRenderingEXT = FunPtr FN_vkCmdBeginConditionalRenderingEXT

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
-- Unresolved directive in vkCmdEndConditionalRenderingEXT.txt -
-- include::{generated}\/validity\/protos\/vkCmdEndConditionalRenderingEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdEndConditionalRenderingEXT" vkCmdEndConditionalRenderingEXT :: ("commandBuffer" ::: VkCommandBuffer) -> IO ()
#else
vkCmdEndConditionalRenderingEXT :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> IO ()
vkCmdEndConditionalRenderingEXT deviceCmds = mkVkCmdEndConditionalRenderingEXT (pVkCmdEndConditionalRenderingEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdEndConditionalRenderingEXT
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> IO ())
#endif

type FN_vkCmdEndConditionalRenderingEXT = ("commandBuffer" ::: VkCommandBuffer) -> IO ()
type PFN_vkCmdEndConditionalRenderingEXT = FunPtr FN_vkCmdEndConditionalRenderingEXT

-- | 'VK_ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT' specifies read access to
-- a predicate as part of conditional rendering.
pattern VK_ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT :: VkAccessFlagBits
pattern VK_ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT = VkAccessFlagBits 0x00100000

-- | 'VK_BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT' specifies that the
-- buffer is suitable for passing as the @buffer@ parameter to
-- 'vkCmdBeginConditionalRenderingEXT'.
pattern VK_BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT = VkBufferUsageFlagBits 0x00000200

-- No documentation found for TopLevel "VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME"
pattern VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME = "VK_EXT_conditional_rendering"

-- No documentation found for TopLevel "VK_EXT_CONDITIONAL_RENDERING_SPEC_VERSION"
pattern VK_EXT_CONDITIONAL_RENDERING_SPEC_VERSION :: Integral a => a
pattern VK_EXT_CONDITIONAL_RENDERING_SPEC_VERSION = 1

-- | 'VK_PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT' specifies the stage of
-- the pipeline where the predicate of conditional rendering is consumed.
pattern VK_PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT = VkPipelineStageFlagBits 0x00040000

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT"
pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT = VkStructureType 1000081000

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT"
pattern VK_STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT = VkStructureType 1000081002

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT = VkStructureType 1000081001
