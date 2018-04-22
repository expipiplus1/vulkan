{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_discard_rectangles
  ( VkDiscardRectangleModeEXT(..)
  , pattern VK_DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT
  , pattern VK_DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT
  , VkPipelineDiscardRectangleStateCreateFlagsEXT(..)
  , pattern VK_DYNAMIC_STATE_DISCARD_RECTANGLE_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT
  , pattern VK_EXT_DISCARD_RECTANGLES_SPEC_VERSION
  , pattern VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME
  , vkCmdSetDiscardRectangleEXT
  , VkPhysicalDeviceDiscardRectanglePropertiesEXT(..)
  , VkPipelineDiscardRectangleStateCreateInfoEXT(..)
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Int
  ( Int32
  )
import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( Ptr
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


import Graphics.Vulkan.Core10.Core
  ( VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.Pipeline
  ( VkDynamicState(..)
  , VkRect2D(..)
  )
import Graphics.Vulkan.Core10.Queue
  ( VkCommandBuffer
  )


-- ** VkDiscardRectangleModeEXT

-- | VkDiscardRectangleModeEXT - Specify the discard rectangle mode
--
-- = See Also
--
-- 'VkPipelineDiscardRectangleStateCreateInfoEXT'
newtype VkDiscardRectangleModeEXT = VkDiscardRectangleModeEXT Int32
  deriving (Eq, Ord, Storable)

instance Show VkDiscardRectangleModeEXT where
  showsPrec _ VK_DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT = showString "VK_DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT"
  showsPrec _ VK_DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT = showString "VK_DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT"
  showsPrec p (VkDiscardRectangleModeEXT x) = showParen (p >= 11) (showString "VkDiscardRectangleModeEXT " . showsPrec 11 x)

instance Read VkDiscardRectangleModeEXT where
  readPrec = parens ( choose [ ("VK_DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT", pure VK_DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT)
                             , ("VK_DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT", pure VK_DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDiscardRectangleModeEXT")
                        v <- step readPrec
                        pure (VkDiscardRectangleModeEXT v)
                        )
                    )

-- | @VK_DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT@ specifies that a fragment
-- within any discard rectangle satisfies the test.
pattern VK_DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT :: VkDiscardRectangleModeEXT
pattern VK_DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT = VkDiscardRectangleModeEXT 0

-- | @VK_DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT@ specifies that a fragment not
-- within any of the discard rectangles satisfies the test.
pattern VK_DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT :: VkDiscardRectangleModeEXT
pattern VK_DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT = VkDiscardRectangleModeEXT 1
-- ** VkPipelineDiscardRectangleStateCreateFlagsEXT

-- | VkPipelineDiscardRectangleStateCreateFlagsEXT - Reserved for future use
--
-- = Description
--
-- @VkPipelineDiscardRectangleStateCreateFlagsEXT@ is a bitmask type for
-- setting a mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'VkPipelineDiscardRectangleStateCreateInfoEXT'
newtype VkPipelineDiscardRectangleStateCreateFlagsEXT = VkPipelineDiscardRectangleStateCreateFlagsEXT VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkPipelineDiscardRectangleStateCreateFlagsEXT where
  
  showsPrec p (VkPipelineDiscardRectangleStateCreateFlagsEXT x) = showParen (p >= 11) (showString "VkPipelineDiscardRectangleStateCreateFlagsEXT " . showsPrec 11 x)

instance Read VkPipelineDiscardRectangleStateCreateFlagsEXT where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPipelineDiscardRectangleStateCreateFlagsEXT")
                        v <- step readPrec
                        pure (VkPipelineDiscardRectangleStateCreateFlagsEXT v)
                        )
                    )


-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_DISCARD_RECTANGLE_EXT"
pattern VK_DYNAMIC_STATE_DISCARD_RECTANGLE_EXT :: VkDynamicState
pattern VK_DYNAMIC_STATE_DISCARD_RECTANGLE_EXT = VkDynamicState 1000099000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT = VkStructureType 1000099000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT = VkStructureType 1000099001
-- No documentation found for TopLevel "VK_EXT_DISCARD_RECTANGLES_SPEC_VERSION"
pattern VK_EXT_DISCARD_RECTANGLES_SPEC_VERSION :: Integral a => a
pattern VK_EXT_DISCARD_RECTANGLES_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME"
pattern VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME = "VK_EXT_discard_rectangles"
-- | vkCmdSetDiscardRectangleEXT - Set discard rectangles dynamically
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @firstDiscardRectangle@ is the index of the first discard rectangle
--     whose state is updated by the command.
--
-- -   @discardRectangleCount@ is the number of discard rectangles whose
--     state are updated by the command.
--
-- -   @pDiscardRectangles@ is a pointer to an array of
--     'Graphics.Vulkan.Core10.Pipeline.VkRect2D' structures specifying
--     discard rectangles.
--
-- = Description
--
-- The discard rectangle taken from element i of @pDiscardRectangles@
-- replace the current state for the discard rectangle index
-- @firstDiscardRectangle@ + i, for i in [0, @discardRectangleCount@).
--
-- == Valid Usage
--
-- -   The bound graphics pipeline /must/ have been created with the
--     @VK_DYNAMIC_STATE_DISCARD_RECTANGLE_EXT@ dynamic state enabled
--
-- -   The sum of @firstDiscardRectangle@ and @discardRectangleCount@
--     /must/ be less than or equal to
--     'VkPhysicalDeviceDiscardRectanglePropertiesEXT'::@maxDiscardRectangles@
--
-- -   The @x@ and @y@ member of @offset@ in each
--     'Graphics.Vulkan.Core10.Pipeline.VkRect2D' element of
--     @pDiscardRectangles@ /must/ be greater than or equal to @0@
--
-- -   Evaluation of (@offset.x@ + @extent.width@) in each
--     'Graphics.Vulkan.Core10.Pipeline.VkRect2D' element of
--     @pDiscardRectangles@ /must/ not cause a signed integer addition
--     overflow
--
-- -   Evaluation of (@offset.y@ + @extent.height@) in each
--     'Graphics.Vulkan.Core10.Pipeline.VkRect2D' element of
--     @pDiscardRectangles@ /must/ not cause a signed integer addition
--     overflow
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @pDiscardRectangles@ /must/ be a valid pointer to an array of
--     @discardRectangleCount@ @VkRect2D@ structures
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics operations
--
-- -   @discardRectangleCount@ /must/ be greater than @0@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Both                                                                                                       | Graphics                                                                                              |                                                                                                                            |
-- | Secondary                                                                                                   |                                                                                                            |                                                                                                       |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.Pipeline.VkRect2D'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdSetDiscardRectangleEXT" vkCmdSetDiscardRectangleEXT :: ("commandBuffer" ::: VkCommandBuffer) -> ("firstDiscardRectangle" ::: Word32) -> ("discardRectangleCount" ::: Word32) -> ("pDiscardRectangles" ::: Ptr VkRect2D) -> IO ()
-- | VkPhysicalDeviceDiscardRectanglePropertiesEXT - Structure describing
-- discard rectangle limits that can be supported by an implementation
--
-- = Members
--
-- The members of the @VkPhysicalDeviceDiscardRectanglePropertiesEXT@
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- -   @maxDiscardRectangles@ is the maximum number of discard rectangles
--     that /can/ be specified.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT@
--
-- If the @VkPhysicalDeviceDiscardRectanglePropertiesEXT@ structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPhysicalDeviceDiscardRectanglePropertiesEXT = VkPhysicalDeviceDiscardRectanglePropertiesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceDiscardRectanglePropertiesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceDiscardRectanglePropertiesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceDiscardRectanglePropertiesEXT" "maxDiscardRectangles"
  vkMaxDiscardRectangles :: Word32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceDiscardRectanglePropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceDiscardRectanglePropertiesEXT <$> peek (ptr `plusPtr` 0)
                                                           <*> peek (ptr `plusPtr` 8)
                                                           <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceDiscardRectanglePropertiesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceDiscardRectanglePropertiesEXT))
                *> poke (ptr `plusPtr` 16) (vkMaxDiscardRectangles (poked :: VkPhysicalDeviceDiscardRectanglePropertiesEXT))
-- | VkPipelineDiscardRectangleStateCreateInfoEXT - Structure specifying
-- discard rectangle
--
-- == Valid Usage
--
-- -   @discardRectangleCount@ /must/ be between @0@ and
--     @VkPhysicalDeviceDiscardRectanglePropertiesEXT@::@maxDiscardRectangles@,
--     inclusive
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT@
--
-- -   @flags@ /must/ be @0@
--
-- -   @discardRectangleMode@ /must/ be a valid 'VkDiscardRectangleModeEXT'
--     value
--
-- = See Also
--
-- 'VkDiscardRectangleModeEXT',
-- 'VkPipelineDiscardRectangleStateCreateFlagsEXT',
-- 'Graphics.Vulkan.Core10.Pipeline.VkRect2D',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPipelineDiscardRectangleStateCreateInfoEXT = VkPipelineDiscardRectangleStateCreateInfoEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use.
  vkFlags :: VkPipelineDiscardRectangleStateCreateFlagsEXT
  , -- | @discardRectangleMode@ is the mode used to determine whether fragments
  -- that lie within the discard rectangle are discarded or not.
  vkDiscardRectangleMode :: VkDiscardRectangleModeEXT
  , -- | @discardRectangleCount@ is the number of discard rectangles used by the
  -- pipeline.
  vkDiscardRectangleCount :: Word32
  , -- | @pDiscardRectangles@ is a pointer to an array of
  -- 'Graphics.Vulkan.Core10.Pipeline.VkRect2D' structures, defining the
  -- discard rectangles. If the discard rectangle state is dynamic, this
  -- member is ignored.
  vkPDiscardRectangles :: Ptr VkRect2D
  }
  deriving (Eq, Show)

instance Storable VkPipelineDiscardRectangleStateCreateInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkPipelineDiscardRectangleStateCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                                          <*> peek (ptr `plusPtr` 8)
                                                          <*> peek (ptr `plusPtr` 16)
                                                          <*> peek (ptr `plusPtr` 20)
                                                          <*> peek (ptr `plusPtr` 24)
                                                          <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineDiscardRectangleStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineDiscardRectangleStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineDiscardRectangleStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 20) (vkDiscardRectangleMode (poked :: VkPipelineDiscardRectangleStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkDiscardRectangleCount (poked :: VkPipelineDiscardRectangleStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 32) (vkPDiscardRectangles (poked :: VkPipelineDiscardRectangleStateCreateInfoEXT))
