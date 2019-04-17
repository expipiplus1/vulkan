{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils
  ( PFN_vkDebugUtilsMessengerCallbackEXT
  , VkDebugUtilsLabelEXT(..)
  , VkDebugUtilsMessageSeverityFlagBitsEXT(..)
  , pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT
  , pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT
  , pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
  , pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
  , VkDebugUtilsMessageSeverityFlagsEXT
  , VkDebugUtilsMessageTypeFlagBitsEXT(..)
  , pattern VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
  , pattern VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
  , pattern VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
  , VkDebugUtilsMessageTypeFlagsEXT
  , VkDebugUtilsMessengerCallbackDataEXT(..)
  , VkDebugUtilsMessengerCallbackDataFlagsEXT(..)
  , VkDebugUtilsMessengerCreateFlagsEXT(..)
  , VkDebugUtilsMessengerCreateInfoEXT(..)
  , VkDebugUtilsMessengerEXT
  , VkDebugUtilsObjectNameInfoEXT(..)
  , VkDebugUtilsObjectTagInfoEXT(..)
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkCmdBeginDebugUtilsLabelEXT
#endif
  , FN_vkCmdBeginDebugUtilsLabelEXT
  , PFN_vkCmdBeginDebugUtilsLabelEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkCmdEndDebugUtilsLabelEXT
#endif
  , FN_vkCmdEndDebugUtilsLabelEXT
  , PFN_vkCmdEndDebugUtilsLabelEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkCmdInsertDebugUtilsLabelEXT
#endif
  , FN_vkCmdInsertDebugUtilsLabelEXT
  , PFN_vkCmdInsertDebugUtilsLabelEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkCreateDebugUtilsMessengerEXT
#endif
  , FN_vkCreateDebugUtilsMessengerEXT
  , PFN_vkCreateDebugUtilsMessengerEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkDestroyDebugUtilsMessengerEXT
#endif
  , FN_vkDestroyDebugUtilsMessengerEXT
  , PFN_vkDestroyDebugUtilsMessengerEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkQueueBeginDebugUtilsLabelEXT
#endif
  , FN_vkQueueBeginDebugUtilsLabelEXT
  , PFN_vkQueueBeginDebugUtilsLabelEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkQueueEndDebugUtilsLabelEXT
#endif
  , FN_vkQueueEndDebugUtilsLabelEXT
  , PFN_vkQueueEndDebugUtilsLabelEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkQueueInsertDebugUtilsLabelEXT
#endif
  , FN_vkQueueInsertDebugUtilsLabelEXT
  , PFN_vkQueueInsertDebugUtilsLabelEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkSetDebugUtilsObjectNameEXT
#endif
  , FN_vkSetDebugUtilsObjectNameEXT
  , PFN_vkSetDebugUtilsObjectNameEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkSetDebugUtilsObjectTagEXT
#endif
  , FN_vkSetDebugUtilsObjectTagEXT
  , PFN_vkSetDebugUtilsObjectTagEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkSubmitDebugUtilsMessageEXT
#endif
  , FN_vkSubmitDebugUtilsMessageEXT
  , PFN_vkSubmitDebugUtilsMessageEXT
  , pattern VK_EXT_DEBUG_UTILS_EXTENSION_NAME
  , pattern VK_EXT_DEBUG_UTILS_SPEC_VERSION
  , pattern VK_OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT
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
import Data.Vector.Storable.Sized
  ( Vector
  )
import Data.Word
  ( Word32
  , Word64
  )
import Foreign.C.Types
  ( CChar(..)
  , CFloat(..)
  , CSize(..)
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


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkObjectType(..)
  , VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  , VkFlags
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  , VkInstance
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkCommandBuffer
  , VkQueue
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | PFN_vkDebugUtilsMessengerCallbackEXT - Application-defined debug
-- messenger callback function
--
-- = Parameters
--
-- -   @messageSeverity@ specifies the
--     'VkDebugUtilsMessageSeverityFlagBitsEXT' that triggered this
--     callback.
--
-- -   @messageTypes@ is a bitmask of 'VkDebugUtilsMessageTypeFlagBitsEXT'
--     specifying which type of event(s) triggered this callback.
--
-- -   @pCallbackData@ contains all the callback related data in the
--     'VkDebugUtilsMessengerCallbackDataEXT' structure.
--
-- -   @pUserData@ is the user data provided when the
--     'VkDebugUtilsMessengerEXT' was created.
--
-- = Description
--
-- The callback /must/ not call 'vkDestroyDebugUtilsMessengerEXT'.
--
-- The callback returns a @VkBool32@, which is interpreted in a
-- layer-specified manner. The application /should/ always return
-- @VK_FALSE@. The @VK_TRUE@ value is reserved for use in layer
-- development.
--
-- = See Also
--
-- No cross-references are available
type PFN_vkDebugUtilsMessengerCallbackEXT = Ptr (("messageSeverity" ::: VkDebugUtilsMessageSeverityFlagBitsEXT) -> ("messageTypes" ::: VkDebugUtilsMessageTypeFlagsEXT) -> ("pCallbackData" ::: Ptr VkDebugUtilsMessengerCallbackDataEXT) -> ("pUserData" ::: Ptr ()) -> IO VkBool32)
-- | VkDebugUtilsLabelEXT - Specify parameters of a label region
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- No cross-references are available
data VkDebugUtilsLabelEXT = VkDebugUtilsLabelEXT
  { -- | @sType@ /must/ be @VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT@
  vkSType :: VkStructureType
  , -- | @pNext@ /must/ be @NULL@
  vkPNext :: Ptr ()
  , -- | @pLabelName@ /must/ be a null-terminated UTF-8 string
  vkPLabelName :: Ptr CChar
  , -- | @color@ is an optional RGBA color value that can be associated with the
  -- label. A particular implementation /may/ choose to ignore this color
  -- value. The values contain RGBA values in order, in the range 0.0 to 1.0.
  -- If all elements in @color@ are set to 0.0 then it is ignored.
  vkColor :: Vector 4 CFloat
  }
  deriving (Eq, Show)

instance Storable VkDebugUtilsLabelEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkDebugUtilsLabelEXT <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
                                  <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDebugUtilsLabelEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDebugUtilsLabelEXT))
                *> poke (ptr `plusPtr` 16) (vkPLabelName (poked :: VkDebugUtilsLabelEXT))
                *> poke (ptr `plusPtr` 24) (vkColor (poked :: VkDebugUtilsLabelEXT))

instance Zero VkDebugUtilsLabelEXT where
  zero = VkDebugUtilsLabelEXT zero
                              zero
                              zero
                              zero
-- ** VkDebugUtilsMessageSeverityFlagBitsEXT

-- | VkDebugUtilsMessageSeverityFlagBitsEXT - Bitmask specifying which
-- severities of events cause a debug messenger callback
--
-- = See Also
--
-- No cross-references are available
newtype VkDebugUtilsMessageSeverityFlagBitsEXT = VkDebugUtilsMessageSeverityFlagBitsEXT VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkDebugUtilsMessageSeverityFlagBitsEXT where
  showsPrec _ VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT = showString "VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT"
  showsPrec _ VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT = showString "VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT"
  showsPrec _ VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT = showString "VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT"
  showsPrec _ VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT = showString "VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT"
  showsPrec p (VkDebugUtilsMessageSeverityFlagBitsEXT x) = showParen (p >= 11) (showString "VkDebugUtilsMessageSeverityFlagBitsEXT " . showsPrec 11 x)

instance Read VkDebugUtilsMessageSeverityFlagBitsEXT where
  readPrec = parens ( choose [ ("VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT", pure VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT)
                             , ("VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT",    pure VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT)
                             , ("VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT", pure VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT)
                             , ("VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT",   pure VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDebugUtilsMessageSeverityFlagBitsEXT")
                        v <- step readPrec
                        pure (VkDebugUtilsMessageSeverityFlagBitsEXT v)
                        )
                    )

-- | @VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT@ specifies the most
-- verbose output indicating all diagnostic messages from the Vulkan
-- loader, layers, and drivers should be captured.
pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT :: VkDebugUtilsMessageSeverityFlagBitsEXT
pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT = VkDebugUtilsMessageSeverityFlagBitsEXT 0x00000001

-- | @VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT@ specifies an
-- informational message such as resource details that may be handy when
-- debugging an application.
pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT :: VkDebugUtilsMessageSeverityFlagBitsEXT
pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT = VkDebugUtilsMessageSeverityFlagBitsEXT 0x00000010

-- | @VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT@ specifies use of
-- Vulkan that /may/ expose an app bug. Such cases may not be immediately
-- harmful, such as a fragment shader outputting to a location with no
-- attachment. Other cases /may/ point to behavior that is almost certainly
-- bad when unintended such as using an image whose memory has not been
-- filled. In general if you see a warning but you know that the behavior
-- is intended\/desired, then simply ignore the warning.
pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT :: VkDebugUtilsMessageSeverityFlagBitsEXT
pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT = VkDebugUtilsMessageSeverityFlagBitsEXT 0x00000100

-- | @VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT@ specifies that the
-- application has violated a valid usage condition of the specification.
pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT :: VkDebugUtilsMessageSeverityFlagBitsEXT
pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT = VkDebugUtilsMessageSeverityFlagBitsEXT 0x00001000
-- | VkDebugUtilsMessageSeverityFlagsEXT - Bitmask of
-- VkDebugUtilsMessageSeverityFlagBitsEXT
--
-- = Description
--
-- @VkDebugUtilsMessageSeverityFlagsEXT@ is a bitmask type for setting a
-- mask of zero or more 'VkDebugUtilsMessageSeverityFlagBitsEXT'.
--
-- = See Also
--
-- No cross-references are available
type VkDebugUtilsMessageSeverityFlagsEXT = VkDebugUtilsMessageSeverityFlagBitsEXT
-- ** VkDebugUtilsMessageTypeFlagBitsEXT

-- | VkDebugUtilsMessageTypeFlagBitsEXT - Bitmask specifying which types of
-- events cause a debug messenger callback
--
-- = See Also
--
-- No cross-references are available
newtype VkDebugUtilsMessageTypeFlagBitsEXT = VkDebugUtilsMessageTypeFlagBitsEXT VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkDebugUtilsMessageTypeFlagBitsEXT where
  showsPrec _ VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT = showString "VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT"
  showsPrec _ VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT = showString "VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT"
  showsPrec _ VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT = showString "VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT"
  showsPrec p (VkDebugUtilsMessageTypeFlagBitsEXT x) = showParen (p >= 11) (showString "VkDebugUtilsMessageTypeFlagBitsEXT " . showsPrec 11 x)

instance Read VkDebugUtilsMessageTypeFlagBitsEXT where
  readPrec = parens ( choose [ ("VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT",     pure VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT)
                             , ("VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT",  pure VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT)
                             , ("VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT", pure VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDebugUtilsMessageTypeFlagBitsEXT")
                        v <- step readPrec
                        pure (VkDebugUtilsMessageTypeFlagBitsEXT v)
                        )
                    )

-- | @VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT@ specifies that some
-- general event has occurred. This is typically a non-specification,
-- non-performance event.
pattern VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT :: VkDebugUtilsMessageTypeFlagBitsEXT
pattern VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT = VkDebugUtilsMessageTypeFlagBitsEXT 0x00000001

-- | @VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT@ specifies that
-- something has occurred during validation against the Vulkan
-- specification that may indicate invalid behavior.
pattern VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT :: VkDebugUtilsMessageTypeFlagBitsEXT
pattern VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT = VkDebugUtilsMessageTypeFlagBitsEXT 0x00000002

-- | @VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT@ specifies a
-- potentially non-optimal use of Vulkan, e.g. using
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdClearColorImage'
-- when setting
-- 'Graphics.Vulkan.C.Core10.Pass.VkAttachmentDescription'::@loadOp@ to
-- @VK_ATTACHMENT_LOAD_OP_CLEAR@ would have worked.
pattern VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT :: VkDebugUtilsMessageTypeFlagBitsEXT
pattern VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT = VkDebugUtilsMessageTypeFlagBitsEXT 0x00000004
-- | VkDebugUtilsMessageTypeFlagsEXT - Bitmask of
-- VkDebugUtilsMessageTypeFlagBitsEXT
--
-- = Description
--
-- @VkDebugUtilsMessageTypeFlagsEXT@ is a bitmask type for setting a mask
-- of zero or more 'VkDebugUtilsMessageTypeFlagBitsEXT'.
--
-- = See Also
--
-- No cross-references are available
type VkDebugUtilsMessageTypeFlagsEXT = VkDebugUtilsMessageTypeFlagBitsEXT
-- | VkDebugUtilsMessengerCallbackDataEXT - Structure specifying parameters
-- returned to the callback
--
-- = Description
--
-- __Note__
--
-- This structure should only be considered valid during the lifetime of
-- the triggered callback.
--
-- Since adding queue and command buffer labels behaves like pushing and
-- popping onto a stack, the order of both @pQueueLabels@ and
-- @pCmdBufLabels@ is based on the order the labels were defined. The
-- result is that the first label in either @pQueueLabels@ or
-- @pCmdBufLabels@ will be the first defined (and therefore the oldest)
-- while the last label in each list will be the most recent.
--
-- __Note__
--
-- @pQueueLabels@ will only be non-NULL if one of the objects in @pObjects@
-- can be related directly to a defined @VkQueue@ which has had one or more
-- labels associated with it.
--
-- Likewise, @pCmdBufLabels@ will only be non-NULL if one of the objects in
-- @pObjects@ can be related directly to a defined @VkCommandBuffer@ which
-- has had one or more labels associated with it. Additionally, while
-- command buffer labels allow for beginning and ending across different
-- command buffers, the debug messaging framework /cannot/ guarantee that
-- labels in @pCmdBufLables@ will contain those defined outside of the
-- associated command buffer. This is partially due to the fact that the
-- association of one command buffer with another may not have been defined
-- at the time the debug message is triggered.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @flags@ /must/ be @0@
--
-- -   If @pMessageIdName@ is not @NULL@, @pMessageIdName@ /must/ be a
--     null-terminated UTF-8 string
--
-- -   @pMessage@ /must/ be a null-terminated UTF-8 string
--
-- -   @objectCount@ /must/ be greater than @0@
--
-- = See Also
--
-- No cross-references are available
data VkDebugUtilsMessengerCallbackDataEXT = VkDebugUtilsMessengerCallbackDataEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is 0 and reserved for future use.
  vkFlags :: VkDebugUtilsMessengerCallbackDataFlagsEXT
  , -- | @pMessageIdName@ is a null-terminated string that identifies the
  -- particular message ID that is associated with the provided message. If
  -- the message corresponds to a validation layer message, then this string
  -- may contain the portion of the Vulkan specification that is believed to
  -- have been violated.
  vkPMessageIdName :: Ptr CChar
  , -- | @messageIdNumber@ is the ID number of the triggering message. If the
  -- message corresponds to a validation layer message, then this number is
  -- related to the internal number associated with the message being
  -- triggered.
  vkMessageIdNumber :: Int32
  , -- | @pMessage@ is a null-terminated string detailing the trigger conditions.
  vkPMessage :: Ptr CChar
  , -- | @queueLabelCount@ is a count of items contained in the @pQueueLabels@
  -- array.
  vkQueueLabelCount :: Word32
  , -- | @pQueueLabels@ is NULL or a pointer to an array of
  -- 'VkDebugUtilsLabelEXT' active in the current @VkQueue@ at the time the
  -- callback was triggered. Refer to
  -- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#debugging-queue-labels Queue Labels>
  -- for more information.
  vkPQueueLabels :: Ptr VkDebugUtilsLabelEXT
  , -- | @cmdBufLabelCount@ is a count of items contained in the @pCmdBufLabels@
  -- array.
  vkCmdBufLabelCount :: Word32
  , -- | @pCmdBufLabels@ is NULL or a pointer to an array of
  -- 'VkDebugUtilsLabelEXT' active in the current @VkCommandBuffer@ at the
  -- time the callback was triggered. Refer to
  -- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#debugging-command-buffer-labels Command Buffer Labels>
  -- for more information.
  vkPCmdBufLabels :: Ptr VkDebugUtilsLabelEXT
  , -- | @objectCount@ is a count of items contained in the @pObjects@ array.
  vkObjectCount :: Word32
  , -- | @pObjects@ is a pointer to an array of 'VkDebugUtilsObjectNameInfoEXT'
  -- objects related to the detected issue. The array is roughly in order or
  -- importance, but the 0th element is always guaranteed to be the most
  -- important object for this message.
  vkPObjects :: Ptr VkDebugUtilsObjectNameInfoEXT
  }
  deriving (Eq, Show)

instance Storable VkDebugUtilsMessengerCallbackDataEXT where
  sizeOf ~_ = 96
  alignment ~_ = 8
  peek ptr = VkDebugUtilsMessengerCallbackDataEXT <$> peek (ptr `plusPtr` 0)
                                                  <*> peek (ptr `plusPtr` 8)
                                                  <*> peek (ptr `plusPtr` 16)
                                                  <*> peek (ptr `plusPtr` 24)
                                                  <*> peek (ptr `plusPtr` 32)
                                                  <*> peek (ptr `plusPtr` 40)
                                                  <*> peek (ptr `plusPtr` 48)
                                                  <*> peek (ptr `plusPtr` 56)
                                                  <*> peek (ptr `plusPtr` 64)
                                                  <*> peek (ptr `plusPtr` 72)
                                                  <*> peek (ptr `plusPtr` 80)
                                                  <*> peek (ptr `plusPtr` 88)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDebugUtilsMessengerCallbackDataEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDebugUtilsMessengerCallbackDataEXT))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkDebugUtilsMessengerCallbackDataEXT))
                *> poke (ptr `plusPtr` 24) (vkPMessageIdName (poked :: VkDebugUtilsMessengerCallbackDataEXT))
                *> poke (ptr `plusPtr` 32) (vkMessageIdNumber (poked :: VkDebugUtilsMessengerCallbackDataEXT))
                *> poke (ptr `plusPtr` 40) (vkPMessage (poked :: VkDebugUtilsMessengerCallbackDataEXT))
                *> poke (ptr `plusPtr` 48) (vkQueueLabelCount (poked :: VkDebugUtilsMessengerCallbackDataEXT))
                *> poke (ptr `plusPtr` 56) (vkPQueueLabels (poked :: VkDebugUtilsMessengerCallbackDataEXT))
                *> poke (ptr `plusPtr` 64) (vkCmdBufLabelCount (poked :: VkDebugUtilsMessengerCallbackDataEXT))
                *> poke (ptr `plusPtr` 72) (vkPCmdBufLabels (poked :: VkDebugUtilsMessengerCallbackDataEXT))
                *> poke (ptr `plusPtr` 80) (vkObjectCount (poked :: VkDebugUtilsMessengerCallbackDataEXT))
                *> poke (ptr `plusPtr` 88) (vkPObjects (poked :: VkDebugUtilsMessengerCallbackDataEXT))

instance Zero VkDebugUtilsMessengerCallbackDataEXT where
  zero = VkDebugUtilsMessengerCallbackDataEXT zero
                                              zero
                                              zero
                                              zero
                                              zero
                                              zero
                                              zero
                                              zero
                                              zero
                                              zero
                                              zero
                                              zero
-- ** VkDebugUtilsMessengerCallbackDataFlagsEXT

-- No documentation found for TopLevel "VkDebugUtilsMessengerCallbackDataFlagsEXT"
newtype VkDebugUtilsMessengerCallbackDataFlagsEXT = VkDebugUtilsMessengerCallbackDataFlagsEXT VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkDebugUtilsMessengerCallbackDataFlagsEXT where
  
  showsPrec p (VkDebugUtilsMessengerCallbackDataFlagsEXT x) = showParen (p >= 11) (showString "VkDebugUtilsMessengerCallbackDataFlagsEXT " . showsPrec 11 x)

instance Read VkDebugUtilsMessengerCallbackDataFlagsEXT where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDebugUtilsMessengerCallbackDataFlagsEXT")
                        v <- step readPrec
                        pure (VkDebugUtilsMessengerCallbackDataFlagsEXT v)
                        )
                    )


-- ** VkDebugUtilsMessengerCreateFlagsEXT

-- No documentation found for TopLevel "VkDebugUtilsMessengerCreateFlagsEXT"
newtype VkDebugUtilsMessengerCreateFlagsEXT = VkDebugUtilsMessengerCreateFlagsEXT VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkDebugUtilsMessengerCreateFlagsEXT where
  
  showsPrec p (VkDebugUtilsMessengerCreateFlagsEXT x) = showParen (p >= 11) (showString "VkDebugUtilsMessengerCreateFlagsEXT " . showsPrec 11 x)

instance Read VkDebugUtilsMessengerCreateFlagsEXT where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDebugUtilsMessengerCreateFlagsEXT")
                        v <- step readPrec
                        pure (VkDebugUtilsMessengerCreateFlagsEXT v)
                        )
                    )


-- | VkDebugUtilsMessengerCreateInfoEXT - Structure specifying parameters of
-- a newly created debug messenger
--
-- = Description
--
-- For each @VkDebugUtilsMessengerEXT@ that is created the
-- @VkDebugUtilsMessengerCreateInfoEXT@::@messageSeverity@ and
-- @VkDebugUtilsMessengerCreateInfoEXT@::@messageType@ determine when that
-- @VkDebugUtilsMessengerCreateInfoEXT@::@pfnUserCallback@ is called. The
-- process to determine if the user’s @pfnUserCallback@ is triggered when
-- an event occurs is as follows:
--
-- 1.  The implementation will perform a bitwise AND of the event’s
--     'VkDebugUtilsMessageSeverityFlagBitsEXT' with the @messageSeverity@
--     provided during creation of the 'VkDebugUtilsMessengerEXT' object.
--
--     1.  If the value is 0, the message is skipped.
--
-- 2.  The implementation will perform bitwise AND of the event’s
--     'VkDebugUtilsMessageTypeFlagBitsEXT' with the @messageType@ provided
--     during the creation of the 'VkDebugUtilsMessengerEXT' object.
--
--     1.  If the value is 0, the message is skipped.
--
-- 3.  The callback will trigger a debug message for the current event
--
-- The callback will come directly from the component that detected the
-- event, unless some other layer intercepts the calls for its own purposes
-- (filter them in a different way, log to a system error log, etc.).
--
-- An application /can/ receive multiple callbacks if multiple
-- @VkDebugUtilsMessengerEXT@ objects are created. A callback will always
-- be executed in the same thread as the originating Vulkan call.
--
-- A callback /can/ be called from multiple threads simultaneously (if the
-- application is making Vulkan calls from multiple threads).
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- No cross-references are available
data VkDebugUtilsMessengerCreateInfoEXT = VkDebugUtilsMessengerCreateInfoEXT
  { -- | @sType@ /must/ be
  -- @VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT@
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ /must/ be @0@
  vkFlags :: VkDebugUtilsMessengerCreateFlagsEXT
  , -- | @messageSeverity@ /must/ not be @0@
  vkMessageSeverity :: VkDebugUtilsMessageSeverityFlagsEXT
  , -- | @messageType@ /must/ not be @0@
  vkMessageType :: VkDebugUtilsMessageTypeFlagsEXT
  , -- | @pfnUserCallback@ /must/ be a valid
  -- 'PFN_vkDebugUtilsMessengerCallbackEXT'
  vkPfnUserCallback :: PFN_vkDebugUtilsMessengerCallbackEXT
  , -- | @pUserData@ is user data to be passed to the callback.
  vkPUserData :: Ptr ()
  }
  deriving (Eq, Show)

instance Storable VkDebugUtilsMessengerCreateInfoEXT where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkDebugUtilsMessengerCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                                <*> peek (ptr `plusPtr` 8)
                                                <*> peek (ptr `plusPtr` 16)
                                                <*> peek (ptr `plusPtr` 20)
                                                <*> peek (ptr `plusPtr` 24)
                                                <*> peek (ptr `plusPtr` 32)
                                                <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDebugUtilsMessengerCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDebugUtilsMessengerCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkDebugUtilsMessengerCreateInfoEXT))
                *> poke (ptr `plusPtr` 20) (vkMessageSeverity (poked :: VkDebugUtilsMessengerCreateInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkMessageType (poked :: VkDebugUtilsMessengerCreateInfoEXT))
                *> poke (ptr `plusPtr` 32) (vkPfnUserCallback (poked :: VkDebugUtilsMessengerCreateInfoEXT))
                *> poke (ptr `plusPtr` 40) (vkPUserData (poked :: VkDebugUtilsMessengerCreateInfoEXT))

instance Zero VkDebugUtilsMessengerCreateInfoEXT where
  zero = VkDebugUtilsMessengerCreateInfoEXT zero
                                            zero
                                            zero
                                            zero
                                            zero
                                            zero
                                            zero
-- | Dummy data to tag the 'Ptr' with
data VkDebugUtilsMessengerEXT_T
-- | VkDebugUtilsMessengerEXT - Opaque handle to a debug messenger object
--
-- = Description
--
-- The debug messenger will provide detailed feedback on the application’s
-- use of Vulkan when events of interest occur. When an event of interest
-- does occur, the debug messenger will submit a debug message to the debug
-- callback that was provided during its creation. Additionally, the debug
-- messenger is responsible with filtering out debug messages that the
-- callback is not interested in and will only provide desired debug
-- messages.
--
-- = See Also
--
-- No cross-references are available
type VkDebugUtilsMessengerEXT = Ptr VkDebugUtilsMessengerEXT_T
-- | VkDebugUtilsObjectNameInfoEXT - Specify parameters of a name to give to
-- an object
--
-- = Description
--
-- Applications /may/ change the name associated with an object simply by
-- calling @vkSetDebugUtilsObjectNameEXT@ again with a new string. If
-- @pObjectName@ is an empty string, then any previously set name is
-- removed.
--
-- == Valid Usage
--
-- -   If @objectType@ is @VK_OBJECT_TYPE_UNKNOWN@, @objectHandle@ /must/
--     not be 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE'
--
-- -   If @objectType@ is not @VK_OBJECT_TYPE_UNKNOWN@, @objectHandle@
--     /must/ be 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE' or a
--     valid Vulkan handle of the type associated with @objectType@ as
--     defined in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#debugging-object-types VkObjectType and Vulkan Handle Relationship>
--     table
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @objectType@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Core.VkObjectType' value
--
-- -   If @pObjectName@ is not @NULL@, @pObjectName@ /must/ be a
--     null-terminated UTF-8 string
--
-- = See Also
--
-- No cross-references are available
data VkDebugUtilsObjectNameInfoEXT = VkDebugUtilsObjectNameInfoEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @objectType@ is a 'Graphics.Vulkan.C.Core10.Core.VkObjectType'
  -- specifying the type of the object to be named.
  vkObjectType :: VkObjectType
  , -- | @objectHandle@ is the object to be named.
  vkObjectHandle :: Word64
  , -- | @pObjectName@ is a null-terminated UTF-8 string specifying the name to
  -- apply to @objectHandle@.
  vkPObjectName :: Ptr CChar
  }
  deriving (Eq, Show)

instance Storable VkDebugUtilsObjectNameInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkDebugUtilsObjectNameInfoEXT <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 8)
                                           <*> peek (ptr `plusPtr` 16)
                                           <*> peek (ptr `plusPtr` 24)
                                           <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDebugUtilsObjectNameInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDebugUtilsObjectNameInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkObjectType (poked :: VkDebugUtilsObjectNameInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkObjectHandle (poked :: VkDebugUtilsObjectNameInfoEXT))
                *> poke (ptr `plusPtr` 32) (vkPObjectName (poked :: VkDebugUtilsObjectNameInfoEXT))

instance Zero VkDebugUtilsObjectNameInfoEXT where
  zero = VkDebugUtilsObjectNameInfoEXT zero
                                       zero
                                       zero
                                       zero
                                       zero
-- | VkDebugUtilsObjectTagInfoEXT - Specify parameters of a tag to attach to
-- an object
--
-- = Description
--
-- The @tagName@ parameter gives a name or identifier to the type of data
-- being tagged. This can be used by debugging layers to easily filter for
-- only data that can be used by that implementation.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- No cross-references are available
data VkDebugUtilsObjectTagInfoEXT = VkDebugUtilsObjectTagInfoEXT
  { -- | @sType@ /must/ be @VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT@
  vkSType :: VkStructureType
  , -- | @pNext@ /must/ be @NULL@
  vkPNext :: Ptr ()
  , -- | @objectType@ /must/ be a valid
  -- 'Graphics.Vulkan.C.Core10.Core.VkObjectType' value
  vkObjectType :: VkObjectType
  , -- | @objectHandle@ /must/ be a valid Vulkan handle of the type associated
  -- with @objectType@ as defined in the
  -- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#debugging-object-types VkObjectType and Vulkan Handle Relationship>
  -- table
  vkObjectHandle :: Word64
  , -- | @tagName@ is a numerical identifier of the tag.
  vkTagName :: Word64
  , -- | @tagSize@ /must/ be greater than @0@
  vkTagSize :: CSize
  , -- | @pTag@ /must/ be a valid pointer to an array of @tagSize@ bytes
  vkPTag :: Ptr ()
  }
  deriving (Eq, Show)

instance Storable VkDebugUtilsObjectTagInfoEXT where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = VkDebugUtilsObjectTagInfoEXT <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 16)
                                          <*> peek (ptr `plusPtr` 24)
                                          <*> peek (ptr `plusPtr` 32)
                                          <*> peek (ptr `plusPtr` 40)
                                          <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDebugUtilsObjectTagInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDebugUtilsObjectTagInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkObjectType (poked :: VkDebugUtilsObjectTagInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkObjectHandle (poked :: VkDebugUtilsObjectTagInfoEXT))
                *> poke (ptr `plusPtr` 32) (vkTagName (poked :: VkDebugUtilsObjectTagInfoEXT))
                *> poke (ptr `plusPtr` 40) (vkTagSize (poked :: VkDebugUtilsObjectTagInfoEXT))
                *> poke (ptr `plusPtr` 48) (vkPTag (poked :: VkDebugUtilsObjectTagInfoEXT))

instance Zero VkDebugUtilsObjectTagInfoEXT where
  zero = VkDebugUtilsObjectTagInfoEXT zero
                                      zero
                                      zero
                                      zero
                                      zero
                                      zero
                                      zero
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- | vkCmdBeginDebugUtilsLabelEXT - Open a command buffer debug label region
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @pLabelInfo@ is a pointer to an instance of the
--     'VkDebugUtilsLabelEXT' structure specifying the parameters of the
--     label region to open.
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @pLabelInfo@ /must/ be a valid pointer to a valid
--     @VkDebugUtilsLabelEXT@ structure
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics, or compute operations
--
-- == Host Synchronization
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
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
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdBeginDebugUtilsLabelEXT" vkCmdBeginDebugUtilsLabelEXT :: ("commandBuffer" ::: VkCommandBuffer) -> ("pLabelInfo" ::: Ptr VkDebugUtilsLabelEXT) -> IO ()

#endif
type FN_vkCmdBeginDebugUtilsLabelEXT = ("commandBuffer" ::: VkCommandBuffer) -> ("pLabelInfo" ::: Ptr VkDebugUtilsLabelEXT) -> IO ()
type PFN_vkCmdBeginDebugUtilsLabelEXT = FunPtr FN_vkCmdBeginDebugUtilsLabelEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- | vkCmdEndDebugUtilsLabelEXT - Close a command buffer label region
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- = Description
--
-- An application /may/ open a debug label region in one command buffer and
-- close it in another, or otherwise split debug label regions across
-- multiple command buffers or multiple queue submissions. When viewed from
-- the linear series of submissions to a single queue, the calls to
-- 'vkCmdBeginDebugUtilsLabelEXT' and 'vkCmdEndDebugUtilsLabelEXT' /must/
-- be matched and balanced.
--
-- == Valid Usage
--
-- -   There /must/ be an outstanding @vkCmdBeginDebugUtilsLabelEXT@
--     command prior to the @vkCmdEndDebugUtilsLabelEXT@ on the queue that
--     @commandBuffer@ is submitted to
--
-- -   If @commandBuffer@ is a secondary command buffer, there /must/ be an
--     outstanding @vkCmdBeginDebugUtilsLabelEXT@ command recorded to
--     @commandBuffer@ that has not previously been ended by a call to
--     @vkCmdEndDebugUtilsLabelEXT@.
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics, or compute operations
--
-- == Host Synchronization
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
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
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdEndDebugUtilsLabelEXT" vkCmdEndDebugUtilsLabelEXT :: ("commandBuffer" ::: VkCommandBuffer) -> IO ()

#endif
type FN_vkCmdEndDebugUtilsLabelEXT = ("commandBuffer" ::: VkCommandBuffer) -> IO ()
type PFN_vkCmdEndDebugUtilsLabelEXT = FunPtr FN_vkCmdEndDebugUtilsLabelEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- | vkCmdInsertDebugUtilsLabelEXT - Insert a label into a command buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @pInfo@ is a pointer to an instance of the 'VkDebugUtilsLabelEXT'
--     structure specifying the parameters of the label to insert.
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @pLabelInfo@ /must/ be a valid pointer to a valid
--     @VkDebugUtilsLabelEXT@ structure
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics, or compute operations
--
-- == Host Synchronization
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
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
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdInsertDebugUtilsLabelEXT" vkCmdInsertDebugUtilsLabelEXT :: ("commandBuffer" ::: VkCommandBuffer) -> ("pLabelInfo" ::: Ptr VkDebugUtilsLabelEXT) -> IO ()

#endif
type FN_vkCmdInsertDebugUtilsLabelEXT = ("commandBuffer" ::: VkCommandBuffer) -> ("pLabelInfo" ::: Ptr VkDebugUtilsLabelEXT) -> IO ()
type PFN_vkCmdInsertDebugUtilsLabelEXT = FunPtr FN_vkCmdInsertDebugUtilsLabelEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- | vkCreateDebugUtilsMessengerEXT - Create a debug messenger object
--
-- = Parameters
--
-- -   @instance@ the instance the messenger will be used with.
--
-- -   @pCreateInfo@ points to a 'VkDebugUtilsMessengerCreateInfoEXT'
--     structure which contains the callback pointer as well as defines the
--     conditions under which this messenger will trigger the callback.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pMessenger@ is a pointer to record the @VkDebugUtilsMessengerEXT@
--     object created.
--
-- == Valid Usage (Implicit)
--
-- -   @instance@ /must/ be a valid @VkInstance@ handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     @VkDebugUtilsMessengerCreateInfoEXT@ structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   @pMessenger@ /must/ be a valid pointer to a
--     @VkDebugUtilsMessengerEXT@ handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
-- The application /must/ ensure that 'vkCreateDebugUtilsMessengerEXT' is
-- not executed in parallel with any Vulkan command that is also called
-- with @instance@ or child of @instance@ as the dispatchable argument.
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateDebugUtilsMessengerEXT" vkCreateDebugUtilsMessengerEXT :: ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkDebugUtilsMessengerCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMessenger" ::: Ptr VkDebugUtilsMessengerEXT) -> IO VkResult

#endif
type FN_vkCreateDebugUtilsMessengerEXT = ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkDebugUtilsMessengerCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMessenger" ::: Ptr VkDebugUtilsMessengerEXT) -> IO VkResult
type PFN_vkCreateDebugUtilsMessengerEXT = FunPtr FN_vkCreateDebugUtilsMessengerEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- | vkDestroyDebugUtilsMessengerEXT - Destroy a debug messenger object
--
-- = Parameters
--
-- -   @instance@ the instance where the callback was created.
--
-- -   @messenger@ the 'VkDebugUtilsMessengerEXT' object to destroy.
--     @messenger@ is an externally synchronized object and /must/ not be
--     used on more than one thread at a time. This means that
--     @vkDestroyDebugUtilsMessengerEXT@ /must/ not be called when a
--     callback is active.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   If @VkAllocationCallbacks@ were provided when @messenger@ was
--     created, a compatible set of callbacks /must/ be provided here
--
-- -   If no @VkAllocationCallbacks@ were provided when @messenger@ was
--     created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @instance@ /must/ be a valid @VkInstance@ handle
--
-- -   @messenger@ /must/ be a valid @VkDebugUtilsMessengerEXT@ handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   @messenger@ /must/ have been created, allocated, or retrieved from
--     @instance@
--
-- == Host Synchronization
--
-- -   Host access to @messenger@ /must/ be externally synchronized
--
-- The application /must/ ensure that 'vkDestroyDebugUtilsMessengerEXT' is
-- not executed in parallel with any Vulkan command that is also called
-- with @instance@ or child of @instance@ as the dispatchable argument.
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyDebugUtilsMessengerEXT" vkDestroyDebugUtilsMessengerEXT :: ("instance" ::: VkInstance) -> ("messenger" ::: VkDebugUtilsMessengerEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()

#endif
type FN_vkDestroyDebugUtilsMessengerEXT = ("instance" ::: VkInstance) -> ("messenger" ::: VkDebugUtilsMessengerEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyDebugUtilsMessengerEXT = FunPtr FN_vkDestroyDebugUtilsMessengerEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- | vkQueueBeginDebugUtilsLabelEXT - Open a queue debug label region
--
-- = Parameters
--
-- -   @queue@ is the queue in which to start a debug label region.
--
-- -   @pLabelInfo@ is a pointer to an instance of the
--     'VkDebugUtilsLabelEXT' structure specifying the parameters of the
--     label region to open.
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
-- > | -               | -               | Any             | -               |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkQueueBeginDebugUtilsLabelEXT" vkQueueBeginDebugUtilsLabelEXT :: ("queue" ::: VkQueue) -> ("pLabelInfo" ::: Ptr VkDebugUtilsLabelEXT) -> IO ()

#endif
type FN_vkQueueBeginDebugUtilsLabelEXT = ("queue" ::: VkQueue) -> ("pLabelInfo" ::: Ptr VkDebugUtilsLabelEXT) -> IO ()
type PFN_vkQueueBeginDebugUtilsLabelEXT = FunPtr FN_vkQueueBeginDebugUtilsLabelEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- | vkQueueEndDebugUtilsLabelEXT - Close a queue debug label region
--
-- = Parameters
--
-- -   @queue@ is the queue in which a debug label region should be closed.
--
-- = Description
--
-- The calls to 'vkQueueBeginDebugUtilsLabelEXT' and
-- 'vkQueueEndDebugUtilsLabelEXT' /must/ be matched and balanced.
--
-- == Valid Usage
--
-- -   There /must/ be an outstanding @vkQueueBeginDebugUtilsLabelEXT@
--     command prior to the @vkQueueEndDebugUtilsLabelEXT@ on the queue
--
-- == Valid Usage (Implicit)
--
-- -   @queue@ /must/ be a valid @VkQueue@ handle
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
-- > | -               | -               | Any             | -               |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkQueueEndDebugUtilsLabelEXT" vkQueueEndDebugUtilsLabelEXT :: ("queue" ::: VkQueue) -> IO ()

#endif
type FN_vkQueueEndDebugUtilsLabelEXT = ("queue" ::: VkQueue) -> IO ()
type PFN_vkQueueEndDebugUtilsLabelEXT = FunPtr FN_vkQueueEndDebugUtilsLabelEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- | vkQueueInsertDebugUtilsLabelEXT - Insert a label into a queue
--
-- = Parameters
--
-- -   @queue@ is the queue into which a debug label will be inserted.
--
-- -   @pLabelInfo@ is a pointer to an instance of the
--     'VkDebugUtilsLabelEXT' structure specifying the parameters of the
--     label to insert.
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
-- > | -               | -               | Any             | -               |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkQueueInsertDebugUtilsLabelEXT" vkQueueInsertDebugUtilsLabelEXT :: ("queue" ::: VkQueue) -> ("pLabelInfo" ::: Ptr VkDebugUtilsLabelEXT) -> IO ()

#endif
type FN_vkQueueInsertDebugUtilsLabelEXT = ("queue" ::: VkQueue) -> ("pLabelInfo" ::: Ptr VkDebugUtilsLabelEXT) -> IO ()
type PFN_vkQueueInsertDebugUtilsLabelEXT = FunPtr FN_vkQueueInsertDebugUtilsLabelEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- | vkSetDebugUtilsObjectNameEXT - Give a user-friendly name to an object
--
-- = Parameters
--
-- -   @device@ is the device that created the object.
--
-- -   @pNameInfo@ is a pointer to an instance of the
--     'VkDebugUtilsObjectNameInfoEXT' structure specifying the parameters
--     of the name to set on the object.
--
-- == Valid Usage
--
-- -   @pNameInfo@->@objectType@ /must/ not be @VK_OBJECT_TYPE_UNKNOWN@
--
-- -   @pNameInfo@->@objectHandle@ /must/ not be
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE'
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pNameInfo@ /must/ be a valid pointer to a valid
--     @VkDebugUtilsObjectNameInfoEXT@ structure
--
-- == Host Synchronization
--
-- -   Host access to @pNameInfo.objectHandle@ /must/ be externally
--     synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkSetDebugUtilsObjectNameEXT" vkSetDebugUtilsObjectNameEXT :: ("device" ::: VkDevice) -> ("pNameInfo" ::: Ptr VkDebugUtilsObjectNameInfoEXT) -> IO VkResult

#endif
type FN_vkSetDebugUtilsObjectNameEXT = ("device" ::: VkDevice) -> ("pNameInfo" ::: Ptr VkDebugUtilsObjectNameInfoEXT) -> IO VkResult
type PFN_vkSetDebugUtilsObjectNameEXT = FunPtr FN_vkSetDebugUtilsObjectNameEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- | vkSetDebugUtilsObjectTagEXT - Attach arbitrary data to an object
--
-- = Parameters
--
-- -   @device@ is the device that created the object.
--
-- -   @pTagInfo@ is a pointer to an instance of the
--     'VkDebugUtilsObjectTagInfoEXT' structure specifying the parameters
--     of the tag to attach to the object.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pTagInfo@ /must/ be a valid pointer to a valid
--     @VkDebugUtilsObjectTagInfoEXT@ structure
--
-- == Host Synchronization
--
-- -   Host access to @pTagInfo.objectHandle@ /must/ be externally
--     synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkSetDebugUtilsObjectTagEXT" vkSetDebugUtilsObjectTagEXT :: ("device" ::: VkDevice) -> ("pTagInfo" ::: Ptr VkDebugUtilsObjectTagInfoEXT) -> IO VkResult

#endif
type FN_vkSetDebugUtilsObjectTagEXT = ("device" ::: VkDevice) -> ("pTagInfo" ::: Ptr VkDebugUtilsObjectTagInfoEXT) -> IO VkResult
type PFN_vkSetDebugUtilsObjectTagEXT = FunPtr FN_vkSetDebugUtilsObjectTagEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- | vkSubmitDebugUtilsMessageEXT - Inject a message into a debug stream
--
-- = Parameters
--
-- -   @instance@ is the debug stream’s
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance'.
--
-- -   @messageSeverity@ is the 'VkDebugUtilsMessageSeverityFlagBitsEXT'
--     severity of this event\/message.
--
-- -   @messageTypes@ is a bitmask of 'VkDebugUtilsMessageTypeFlagBitsEXT'
--     specifying which type of event(s) to identify with this message.
--
-- -   @pCallbackData@ contains all the callback related data in the
--     'VkDebugUtilsMessengerCallbackDataEXT' structure.
--
-- = Description
--
-- The call will propagate through the layers and generate callback(s) as
-- indicated by the message’s flags. The parameters are passed on to the
-- callback in addition to the @pUserData@ value that was defined at the
-- time the messenger was registered.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkSubmitDebugUtilsMessageEXT" vkSubmitDebugUtilsMessageEXT :: ("instance" ::: VkInstance) -> ("messageSeverity" ::: VkDebugUtilsMessageSeverityFlagBitsEXT) -> ("messageTypes" ::: VkDebugUtilsMessageTypeFlagsEXT) -> ("pCallbackData" ::: Ptr VkDebugUtilsMessengerCallbackDataEXT) -> IO ()

#endif
type FN_vkSubmitDebugUtilsMessageEXT = ("instance" ::: VkInstance) -> ("messageSeverity" ::: VkDebugUtilsMessageSeverityFlagBitsEXT) -> ("messageTypes" ::: VkDebugUtilsMessageTypeFlagsEXT) -> ("pCallbackData" ::: Ptr VkDebugUtilsMessengerCallbackDataEXT) -> IO ()
type PFN_vkSubmitDebugUtilsMessageEXT = FunPtr FN_vkSubmitDebugUtilsMessageEXT
-- No documentation found for TopLevel "VK_EXT_DEBUG_UTILS_EXTENSION_NAME"
pattern VK_EXT_DEBUG_UTILS_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_DEBUG_UTILS_EXTENSION_NAME = "VK_EXT_debug_utils"
-- No documentation found for TopLevel "VK_EXT_DEBUG_UTILS_SPEC_VERSION"
pattern VK_EXT_DEBUG_UTILS_SPEC_VERSION :: Integral a => a
pattern VK_EXT_DEBUG_UTILS_SPEC_VERSION = 1
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT"
pattern VK_OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT :: VkObjectType
pattern VK_OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT = VkObjectType 1000128000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT"
pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT = VkStructureType 1000128002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT"
pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT = VkStructureType 1000128003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT = VkStructureType 1000128004
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT"
pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT = VkStructureType 1000128000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT"
pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT = VkStructureType 1000128001
