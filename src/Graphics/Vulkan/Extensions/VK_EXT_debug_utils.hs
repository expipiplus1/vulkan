{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Extensions.VK_EXT_debug_utils
  ( withCStructDebugUtilsLabelEXT
  , fromCStructDebugUtilsLabelEXT
  , DebugUtilsLabelEXT(..)
  , DebugUtilsMessageSeverityFlagBitsEXT
  , pattern DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT
  , pattern DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT
  , pattern DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
  , pattern DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
  , DebugUtilsMessageSeverityFlagsEXT
  , DebugUtilsMessageTypeFlagBitsEXT
  , pattern DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
  , pattern DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
  , pattern DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
  , DebugUtilsMessageTypeFlagsEXT
  , withCStructDebugUtilsMessengerCallbackDataEXT
  , fromCStructDebugUtilsMessengerCallbackDataEXT
  , DebugUtilsMessengerCallbackDataEXT(..)
  , DebugUtilsMessengerCallbackDataFlagsEXT
  , DebugUtilsMessengerCreateFlagsEXT
  , withCStructDebugUtilsMessengerCreateInfoEXT
  , fromCStructDebugUtilsMessengerCreateInfoEXT
  , DebugUtilsMessengerCreateInfoEXT(..)
  , DebugUtilsMessengerEXT
  , withCStructDebugUtilsObjectNameInfoEXT
  , fromCStructDebugUtilsObjectNameInfoEXT
  , DebugUtilsObjectNameInfoEXT(..)
  , withCStructDebugUtilsObjectTagInfoEXT
  , fromCStructDebugUtilsObjectTagInfoEXT
  , DebugUtilsObjectTagInfoEXT(..)
  , cmdBeginDebugUtilsLabelEXT
  , cmdEndDebugUtilsLabelEXT
  , cmdInsertDebugUtilsLabelEXT
  , createDebugUtilsMessengerEXT
  , destroyDebugUtilsMessengerEXT
  , queueBeginDebugUtilsLabelEXT
  , queueEndDebugUtilsLabelEXT
  , queueInsertDebugUtilsLabelEXT
  , setDebugUtilsObjectNameEXT
  , setDebugUtilsObjectTagEXT
  , submitDebugUtilsMessageEXT
  , withDebugUtilsMessengerEXT
  , pattern VK_EXT_DEBUG_UTILS_SPEC_VERSION
  , pattern VK_EXT_DEBUG_UTILS_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT
  , pattern VK_OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT
  ) where

import Control.Exception
  ( bracket
  , throwIO
  )
import Control.Monad
  ( (<=<)
  , when
  )
import Data.ByteString
  ( ByteString
  , packCString
  , packCStringLen
  , useAsCString
  )
import qualified Data.ByteString
  ( empty
  , length
  )
import Data.ByteString.Unsafe
  ( unsafeUseAsCString
  )
import Data.Int
  ( Int32
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( empty
  , generateM
  , length
  )
import Data.Vector.Generic.Sized
  ( fromTuple
  )
import qualified Data.Vector.Storable.Sized
  ( unsafeIndex
  )
import Data.Word
  ( Word64
  )
import Foreign.C.Types
  ( CFloat(..)
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( Ptr
  , castPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils
  ( VkDebugUtilsLabelEXT(..)
  , VkDebugUtilsMessageSeverityFlagBitsEXT(..)
  , VkDebugUtilsMessageTypeFlagBitsEXT(..)
  , VkDebugUtilsMessengerCallbackDataEXT(..)
  , VkDebugUtilsMessengerCallbackDataFlagsEXT(..)
  , VkDebugUtilsMessengerCreateFlagsEXT(..)
  , VkDebugUtilsMessengerCreateInfoEXT(..)
  , VkDebugUtilsObjectNameInfoEXT(..)
  , VkDebugUtilsObjectTagInfoEXT(..)
  , PFN_vkDebugUtilsMessengerCallbackEXT
  , VkDebugUtilsMessengerEXT
  , vkCmdBeginDebugUtilsLabelEXT
  , vkCmdEndDebugUtilsLabelEXT
  , vkCmdInsertDebugUtilsLabelEXT
  , vkCreateDebugUtilsMessengerEXT
  , vkDestroyDebugUtilsMessengerEXT
  , vkQueueBeginDebugUtilsLabelEXT
  , vkQueueEndDebugUtilsLabelEXT
  , vkQueueInsertDebugUtilsLabelEXT
  , vkSetDebugUtilsObjectNameEXT
  , vkSetDebugUtilsObjectTagEXT
  , vkSubmitDebugUtilsMessageEXT
  , pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
  , pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT
  , pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT
  , pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
  , pattern VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
  , pattern VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
  , pattern VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT
  )
import Graphics.Vulkan.Core10.Core
  ( ObjectType
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , Instance(..)
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Core10.Queue
  ( CommandBuffer(..)
  , Queue(..)
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
import Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils
  ( pattern VK_EXT_DEBUG_UTILS_EXTENSION_NAME
  , pattern VK_EXT_DEBUG_UTILS_SPEC_VERSION
  , pattern VK_OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT
  )



-- | VkDebugUtilsLabelEXT - Specify parameters of a label region
--
-- = Description
--
-- Unresolved directive in VkDebugUtilsLabelEXT.txt -
-- include::{generated}\/validity\/structs\/VkDebugUtilsLabelEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data DebugUtilsLabelEXT = DebugUtilsLabelEXT
  { -- Univalued member elided
  -- No documentation found for Nested "DebugUtilsLabelEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DebugUtilsLabelEXT" "pLabelName"
  labelName :: ByteString
  , -- No documentation found for Nested "DebugUtilsLabelEXT" "color"
  color :: (CFloat, CFloat, CFloat, CFloat)
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDebugUtilsLabelEXT' and
-- marshal a 'DebugUtilsLabelEXT' into it. The 'VkDebugUtilsLabelEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDebugUtilsLabelEXT :: DebugUtilsLabelEXT -> (VkDebugUtilsLabelEXT -> IO a) -> IO a
withCStructDebugUtilsLabelEXT marshalled cont = useAsCString (labelName (marshalled :: DebugUtilsLabelEXT)) (\pPLabelName -> maybeWith withSomeVkStruct (next (marshalled :: DebugUtilsLabelEXT)) (\pPNext -> cont (VkDebugUtilsLabelEXT VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT pPNext pPLabelName (fromTuple (color (marshalled :: DebugUtilsLabelEXT))))))

-- | A function to read a 'VkDebugUtilsLabelEXT' and all additional
-- structures in the pointer chain into a 'DebugUtilsLabelEXT'.
fromCStructDebugUtilsLabelEXT :: VkDebugUtilsLabelEXT -> IO DebugUtilsLabelEXT
fromCStructDebugUtilsLabelEXT c = DebugUtilsLabelEXT <$> -- Univalued Member elided
                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDebugUtilsLabelEXT)))
                                                     <*> packCString (vkPLabelName (c :: VkDebugUtilsLabelEXT))
                                                     <*> pure (let v = (vkColor (c :: VkDebugUtilsLabelEXT)) in ( Data.Vector.Storable.Sized.unsafeIndex v 0
                                                     , Data.Vector.Storable.Sized.unsafeIndex v 1
                                                     , Data.Vector.Storable.Sized.unsafeIndex v 2
                                                     , Data.Vector.Storable.Sized.unsafeIndex v 3 ))

instance Zero DebugUtilsLabelEXT where
  zero = DebugUtilsLabelEXT Nothing
                            Data.ByteString.empty
                            (zero, zero, zero, zero)


-- | VkDebugUtilsMessageSeverityFlagBitsEXT - Bitmask specifying which
-- severities of events cause a debug messenger callback
--
-- = See Also
--
-- No cross-references are available
type DebugUtilsMessageSeverityFlagBitsEXT = VkDebugUtilsMessageSeverityFlagBitsEXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT'
-- specifies the most verbose output indicating all diagnostic messages
-- from the Vulkan loader, layers, and drivers should be captured.
pattern DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT :: (a ~ DebugUtilsMessageSeverityFlagBitsEXT) => a
pattern DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT = VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT'
-- specifies an informational message such as resource details that may be
-- handy when debugging an application.
pattern DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT :: (a ~ DebugUtilsMessageSeverityFlagBitsEXT) => a
pattern DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT = VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT'
-- specifies use of Vulkan that /may/ expose an app bug. Such cases may not
-- be immediately harmful, such as a fragment shader outputting to a
-- location with no attachment. Other cases /may/ point to behavior that is
-- almost certainly bad when unintended such as using an image whose memory
-- has not been filled. In general if you see a warning but you know that
-- the behavior is intended\/desired, then simply ignore the warning.
pattern DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT :: (a ~ DebugUtilsMessageSeverityFlagBitsEXT) => a
pattern DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT = VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT'
-- specifies that the application has violated a valid usage condition of
-- the specification.
pattern DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT :: (a ~ DebugUtilsMessageSeverityFlagBitsEXT) => a
pattern DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT = VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT

-- | VkDebugUtilsMessageSeverityFlagsEXT - Bitmask of
-- VkDebugUtilsMessageSeverityFlagBitsEXT
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VkDebugUtilsMessageSeverityFlagsEXT'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VkDebugUtilsMessageSeverityFlagBitsEXT'.
--
-- = See Also
--
-- No cross-references are available
type DebugUtilsMessageSeverityFlagsEXT = DebugUtilsMessageSeverityFlagBitsEXT

-- | VkDebugUtilsMessageTypeFlagBitsEXT - Bitmask specifying which types of
-- events cause a debug messenger callback
--
-- = See Also
--
-- No cross-references are available
type DebugUtilsMessageTypeFlagBitsEXT = VkDebugUtilsMessageTypeFlagBitsEXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT'
-- specifies that some general event has occurred. This is typically a
-- non-specification, non-performance event.
pattern DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT :: (a ~ DebugUtilsMessageTypeFlagBitsEXT) => a
pattern DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT = VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT'
-- specifies that something has occurred during validation against the
-- Vulkan specification that may indicate invalid behavior.
pattern DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT :: (a ~ DebugUtilsMessageTypeFlagBitsEXT) => a
pattern DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT = VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT'
-- specifies a potentially non-optimal use of Vulkan, e.g. using
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdClearColorImage'
-- when setting
-- 'Graphics.Vulkan.C.Core10.Pass.VkAttachmentDescription'::@loadOp@ to
-- 'Graphics.Vulkan.C.Core10.Pass.VK_ATTACHMENT_LOAD_OP_CLEAR' would have
-- worked.
pattern DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT :: (a ~ DebugUtilsMessageTypeFlagBitsEXT) => a
pattern DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT = VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT

-- | VkDebugUtilsMessageTypeFlagsEXT - Bitmask of
-- VkDebugUtilsMessageTypeFlagBitsEXT
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VkDebugUtilsMessageTypeFlagsEXT'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VkDebugUtilsMessageTypeFlagBitsEXT'.
--
-- = See Also
--
-- No cross-references are available
type DebugUtilsMessageTypeFlagsEXT = DebugUtilsMessageTypeFlagBitsEXT


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
-- can be related directly to a defined
-- 'Graphics.Vulkan.C.Core10.Queue.VkQueue' which has had one or more
-- labels associated with it.
--
-- Likewise, @pCmdBufLabels@ will only be non-NULL if one of the objects in
-- @pObjects@ can be related directly to a defined
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' which has had one or
-- more labels associated with it. Additionally, while command buffer
-- labels allow for beginning and ending across different command buffers,
-- the debug messaging framework /cannot/ guarantee that labels in
-- @pCmdBufLables@ will contain those defined outside of the associated
-- command buffer. This is partially due to the fact that the association
-- of one command buffer with another may not have been defined at the time
-- the debug message is triggered.
--
-- Unresolved directive in VkDebugUtilsMessengerCallbackDataEXT.txt -
-- include::{generated}\/validity\/structs\/VkDebugUtilsMessengerCallbackDataEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data DebugUtilsMessengerCallbackDataEXT = DebugUtilsMessengerCallbackDataEXT
  { -- Univalued member elided
  -- No documentation found for Nested "DebugUtilsMessengerCallbackDataEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DebugUtilsMessengerCallbackDataEXT" "flags"
  flags :: DebugUtilsMessengerCallbackDataFlagsEXT
  , -- No documentation found for Nested "DebugUtilsMessengerCallbackDataEXT" "pMessageIdName"
  messageIdName :: Maybe ByteString
  , -- No documentation found for Nested "DebugUtilsMessengerCallbackDataEXT" "messageIdNumber"
  messageIdNumber :: Int32
  , -- No documentation found for Nested "DebugUtilsMessengerCallbackDataEXT" "pMessage"
  message :: ByteString
  -- Length valued member elided
  , -- No documentation found for Nested "DebugUtilsMessengerCallbackDataEXT" "pQueueLabels"
  queueLabels :: Vector DebugUtilsLabelEXT
  -- Length valued member elided
  , -- No documentation found for Nested "DebugUtilsMessengerCallbackDataEXT" "pCmdBufLabels"
  cmdBufLabels :: Vector DebugUtilsLabelEXT
  -- Length valued member elided
  , -- No documentation found for Nested "DebugUtilsMessengerCallbackDataEXT" "pObjects"
  objects :: Vector DebugUtilsObjectNameInfoEXT
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDebugUtilsMessengerCallbackDataEXT' and
-- marshal a 'DebugUtilsMessengerCallbackDataEXT' into it. The 'VkDebugUtilsMessengerCallbackDataEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDebugUtilsMessengerCallbackDataEXT :: DebugUtilsMessengerCallbackDataEXT -> (VkDebugUtilsMessengerCallbackDataEXT -> IO a) -> IO a
withCStructDebugUtilsMessengerCallbackDataEXT marshalled cont = withVec withCStructDebugUtilsObjectNameInfoEXT (objects (marshalled :: DebugUtilsMessengerCallbackDataEXT)) (\pPObjects -> withVec withCStructDebugUtilsLabelEXT (cmdBufLabels (marshalled :: DebugUtilsMessengerCallbackDataEXT)) (\pPCmdBufLabels -> withVec withCStructDebugUtilsLabelEXT (queueLabels (marshalled :: DebugUtilsMessengerCallbackDataEXT)) (\pPQueueLabels -> useAsCString (message (marshalled :: DebugUtilsMessengerCallbackDataEXT)) (\pPMessage -> maybeWith useAsCString (messageIdName (marshalled :: DebugUtilsMessengerCallbackDataEXT)) (\pPMessageIdName -> maybeWith withSomeVkStruct (next (marshalled :: DebugUtilsMessengerCallbackDataEXT)) (\pPNext -> cont (VkDebugUtilsMessengerCallbackDataEXT VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT pPNext (flags (marshalled :: DebugUtilsMessengerCallbackDataEXT)) pPMessageIdName (messageIdNumber (marshalled :: DebugUtilsMessengerCallbackDataEXT)) pPMessage (fromIntegral (Data.Vector.length (queueLabels (marshalled :: DebugUtilsMessengerCallbackDataEXT)))) pPQueueLabels (fromIntegral (Data.Vector.length (cmdBufLabels (marshalled :: DebugUtilsMessengerCallbackDataEXT)))) pPCmdBufLabels (fromIntegral (Data.Vector.length (objects (marshalled :: DebugUtilsMessengerCallbackDataEXT)))) pPObjects)))))))

-- | A function to read a 'VkDebugUtilsMessengerCallbackDataEXT' and all additional
-- structures in the pointer chain into a 'DebugUtilsMessengerCallbackDataEXT'.
fromCStructDebugUtilsMessengerCallbackDataEXT :: VkDebugUtilsMessengerCallbackDataEXT -> IO DebugUtilsMessengerCallbackDataEXT
fromCStructDebugUtilsMessengerCallbackDataEXT c = DebugUtilsMessengerCallbackDataEXT <$> -- Univalued Member elided
                                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDebugUtilsMessengerCallbackDataEXT)))
                                                                                     <*> pure (vkFlags (c :: VkDebugUtilsMessengerCallbackDataEXT))
                                                                                     <*> maybePeek packCString (vkPMessageIdName (c :: VkDebugUtilsMessengerCallbackDataEXT))
                                                                                     <*> pure (vkMessageIdNumber (c :: VkDebugUtilsMessengerCallbackDataEXT))
                                                                                     <*> packCString (vkPMessage (c :: VkDebugUtilsMessengerCallbackDataEXT))
                                                                                     -- Length valued member elided
                                                                                     <*> (Data.Vector.generateM (fromIntegral (vkQueueLabelCount (c :: VkDebugUtilsMessengerCallbackDataEXT))) (((fromCStructDebugUtilsLabelEXT <=<) . peekElemOff) (vkPQueueLabels (c :: VkDebugUtilsMessengerCallbackDataEXT))))
                                                                                     -- Length valued member elided
                                                                                     <*> (Data.Vector.generateM (fromIntegral (vkCmdBufLabelCount (c :: VkDebugUtilsMessengerCallbackDataEXT))) (((fromCStructDebugUtilsLabelEXT <=<) . peekElemOff) (vkPCmdBufLabels (c :: VkDebugUtilsMessengerCallbackDataEXT))))
                                                                                     -- Length valued member elided
                                                                                     <*> (Data.Vector.generateM (fromIntegral (vkObjectCount (c :: VkDebugUtilsMessengerCallbackDataEXT))) (((fromCStructDebugUtilsObjectNameInfoEXT <=<) . peekElemOff) (vkPObjects (c :: VkDebugUtilsMessengerCallbackDataEXT))))

instance Zero DebugUtilsMessengerCallbackDataEXT where
  zero = DebugUtilsMessengerCallbackDataEXT Nothing
                                            zero
                                            Nothing
                                            zero
                                            Data.ByteString.empty
                                            Data.Vector.empty
                                            Data.Vector.empty
                                            Data.Vector.empty


-- No documentation found for TopLevel "DebugUtilsMessengerCallbackDataFlagsEXT"
type DebugUtilsMessengerCallbackDataFlagsEXT = VkDebugUtilsMessengerCallbackDataFlagsEXT

-- No documentation found for TopLevel "DebugUtilsMessengerCreateFlagsEXT"
type DebugUtilsMessengerCreateFlagsEXT = VkDebugUtilsMessengerCreateFlagsEXT


-- | VkDebugUtilsMessengerCreateInfoEXT - Structure specifying parameters of
-- a newly created debug messenger
--
-- = Description
--
-- For each
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VkDebugUtilsMessengerEXT'
-- that is created the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VkDebugUtilsMessengerCreateInfoEXT'::@messageSeverity@
-- and
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VkDebugUtilsMessengerCreateInfoEXT'::@messageType@
-- determine when that
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VkDebugUtilsMessengerCreateInfoEXT'::@pfnUserCallback@
-- is called. The process to determine if the user’s @pfnUserCallback@ is
-- triggered when an event occurs is as follows:
--
-- 1.  The implementation will perform a bitwise AND of the event’s
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VkDebugUtilsMessageSeverityFlagBitsEXT'
--     with the @messageSeverity@ provided during creation of the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VkDebugUtilsMessengerEXT'
--     object.
--
--     1.  If the value is 0, the message is skipped.
--
-- 2.  The implementation will perform bitwise AND of the event’s
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VkDebugUtilsMessageTypeFlagBitsEXT'
--     with the @messageType@ provided during the creation of the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VkDebugUtilsMessengerEXT'
--     object.
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
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VkDebugUtilsMessengerEXT'
-- objects are created. A callback will always be executed in the same
-- thread as the originating Vulkan call.
--
-- A callback /can/ be called from multiple threads simultaneously (if the
-- application is making Vulkan calls from multiple threads).
--
-- == Valid Usage
--
-- Unresolved directive in VkDebugUtilsMessengerCreateInfoEXT.txt -
-- include::{generated}\/validity\/structs\/VkDebugUtilsMessengerCreateInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data DebugUtilsMessengerCreateInfoEXT = DebugUtilsMessengerCreateInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "DebugUtilsMessengerCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DebugUtilsMessengerCreateInfoEXT" "flags"
  flags :: DebugUtilsMessengerCreateFlagsEXT
  , -- No documentation found for Nested "DebugUtilsMessengerCreateInfoEXT" "messageSeverity"
  messageSeverity :: DebugUtilsMessageSeverityFlagsEXT
  , -- No documentation found for Nested "DebugUtilsMessengerCreateInfoEXT" "messageType"
  messageType :: DebugUtilsMessageTypeFlagsEXT
  , -- No documentation found for Nested "DebugUtilsMessengerCreateInfoEXT" "pfnUserCallback"
  pfnUserCallback :: PFN_vkDebugUtilsMessengerCallbackEXT
  , -- No documentation found for Nested "DebugUtilsMessengerCreateInfoEXT" "pUserData"
  userData :: Ptr ()
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDebugUtilsMessengerCreateInfoEXT' and
-- marshal a 'DebugUtilsMessengerCreateInfoEXT' into it. The 'VkDebugUtilsMessengerCreateInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDebugUtilsMessengerCreateInfoEXT :: DebugUtilsMessengerCreateInfoEXT -> (VkDebugUtilsMessengerCreateInfoEXT -> IO a) -> IO a
withCStructDebugUtilsMessengerCreateInfoEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: DebugUtilsMessengerCreateInfoEXT)) (\pPNext -> cont (VkDebugUtilsMessengerCreateInfoEXT VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT pPNext (flags (marshalled :: DebugUtilsMessengerCreateInfoEXT)) (messageSeverity (marshalled :: DebugUtilsMessengerCreateInfoEXT)) (messageType (marshalled :: DebugUtilsMessengerCreateInfoEXT)) (pfnUserCallback (marshalled :: DebugUtilsMessengerCreateInfoEXT)) (userData (marshalled :: DebugUtilsMessengerCreateInfoEXT))))

-- | A function to read a 'VkDebugUtilsMessengerCreateInfoEXT' and all additional
-- structures in the pointer chain into a 'DebugUtilsMessengerCreateInfoEXT'.
fromCStructDebugUtilsMessengerCreateInfoEXT :: VkDebugUtilsMessengerCreateInfoEXT -> IO DebugUtilsMessengerCreateInfoEXT
fromCStructDebugUtilsMessengerCreateInfoEXT c = DebugUtilsMessengerCreateInfoEXT <$> -- Univalued Member elided
                                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDebugUtilsMessengerCreateInfoEXT)))
                                                                                 <*> pure (vkFlags (c :: VkDebugUtilsMessengerCreateInfoEXT))
                                                                                 <*> pure (vkMessageSeverity (c :: VkDebugUtilsMessengerCreateInfoEXT))
                                                                                 <*> pure (vkMessageType (c :: VkDebugUtilsMessengerCreateInfoEXT))
                                                                                 <*> pure (vkPfnUserCallback (c :: VkDebugUtilsMessengerCreateInfoEXT))
                                                                                 <*> pure (vkPUserData (c :: VkDebugUtilsMessengerCreateInfoEXT))

instance Zero DebugUtilsMessengerCreateInfoEXT where
  zero = DebugUtilsMessengerCreateInfoEXT Nothing
                                          zero
                                          zero
                                          zero
                                          zero
                                          zero


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
type DebugUtilsMessengerEXT = VkDebugUtilsMessengerEXT


-- | VkDebugUtilsObjectNameInfoEXT - Specify parameters of a name to give to
-- an object
--
-- = Description
--
-- Applications /may/ change the name associated with an object simply by
-- calling
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.vkSetDebugUtilsObjectNameEXT'
-- again with a new string. If @pObjectName@ is an empty string, then any
-- previously set name is removed.
--
-- == Valid Usage
--
-- -   If @objectType@ is
--     'Graphics.Vulkan.C.Core10.Core.VK_OBJECT_TYPE_UNKNOWN',
--     @objectHandle@ /must/ not be
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE'
--
-- -   If @objectType@ is not
--     'Graphics.Vulkan.C.Core10.Core.VK_OBJECT_TYPE_UNKNOWN',
--     @objectHandle@ /must/ be
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE' or a valid
--     Vulkan handle of the type associated with @objectType@ as defined in
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#debugging-object-types VkObjectType and Vulkan Handle Relationship>
--     table
--
-- Unresolved directive in VkDebugUtilsObjectNameInfoEXT.txt -
-- include::{generated}\/validity\/structs\/VkDebugUtilsObjectNameInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data DebugUtilsObjectNameInfoEXT = DebugUtilsObjectNameInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "DebugUtilsObjectNameInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DebugUtilsObjectNameInfoEXT" "objectType"
  objectType :: ObjectType
  , -- No documentation found for Nested "DebugUtilsObjectNameInfoEXT" "objectHandle"
  objectHandle :: Word64
  , -- No documentation found for Nested "DebugUtilsObjectNameInfoEXT" "pObjectName"
  objectName :: Maybe ByteString
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDebugUtilsObjectNameInfoEXT' and
-- marshal a 'DebugUtilsObjectNameInfoEXT' into it. The 'VkDebugUtilsObjectNameInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDebugUtilsObjectNameInfoEXT :: DebugUtilsObjectNameInfoEXT -> (VkDebugUtilsObjectNameInfoEXT -> IO a) -> IO a
withCStructDebugUtilsObjectNameInfoEXT marshalled cont = maybeWith useAsCString (objectName (marshalled :: DebugUtilsObjectNameInfoEXT)) (\pPObjectName -> maybeWith withSomeVkStruct (next (marshalled :: DebugUtilsObjectNameInfoEXT)) (\pPNext -> cont (VkDebugUtilsObjectNameInfoEXT VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT pPNext (objectType (marshalled :: DebugUtilsObjectNameInfoEXT)) (objectHandle (marshalled :: DebugUtilsObjectNameInfoEXT)) pPObjectName)))

-- | A function to read a 'VkDebugUtilsObjectNameInfoEXT' and all additional
-- structures in the pointer chain into a 'DebugUtilsObjectNameInfoEXT'.
fromCStructDebugUtilsObjectNameInfoEXT :: VkDebugUtilsObjectNameInfoEXT -> IO DebugUtilsObjectNameInfoEXT
fromCStructDebugUtilsObjectNameInfoEXT c = DebugUtilsObjectNameInfoEXT <$> -- Univalued Member elided
                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDebugUtilsObjectNameInfoEXT)))
                                                                       <*> pure (vkObjectType (c :: VkDebugUtilsObjectNameInfoEXT))
                                                                       <*> pure (vkObjectHandle (c :: VkDebugUtilsObjectNameInfoEXT))
                                                                       <*> maybePeek packCString (vkPObjectName (c :: VkDebugUtilsObjectNameInfoEXT))

instance Zero DebugUtilsObjectNameInfoEXT where
  zero = DebugUtilsObjectNameInfoEXT Nothing
                                     zero
                                     zero
                                     Nothing



-- | VkDebugUtilsObjectTagInfoEXT - Specify parameters of a tag to attach to
-- an object
--
-- = Description
--
-- The @tagName@ parameter gives a name or identifier to the type of data
-- being tagged. This can be used by debugging layers to easily filter for
-- only data that can be used by that implementation.
--
-- == Valid Usage
--
-- Unresolved directive in VkDebugUtilsObjectTagInfoEXT.txt -
-- include::{generated}\/validity\/structs\/VkDebugUtilsObjectTagInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data DebugUtilsObjectTagInfoEXT = DebugUtilsObjectTagInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "DebugUtilsObjectTagInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DebugUtilsObjectTagInfoEXT" "objectType"
  objectType :: ObjectType
  , -- No documentation found for Nested "DebugUtilsObjectTagInfoEXT" "objectHandle"
  objectHandle :: Word64
  , -- No documentation found for Nested "DebugUtilsObjectTagInfoEXT" "tagName"
  tagName :: Word64
  -- Bytestring length valued member elided
  , -- No documentation found for Nested "DebugUtilsObjectTagInfoEXT" "pTag"
  tag :: ByteString
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDebugUtilsObjectTagInfoEXT' and
-- marshal a 'DebugUtilsObjectTagInfoEXT' into it. The 'VkDebugUtilsObjectTagInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDebugUtilsObjectTagInfoEXT :: DebugUtilsObjectTagInfoEXT -> (VkDebugUtilsObjectTagInfoEXT -> IO a) -> IO a
withCStructDebugUtilsObjectTagInfoEXT marshalled cont = unsafeUseAsCString (tag (marshalled :: DebugUtilsObjectTagInfoEXT)) (\pPTag -> maybeWith withSomeVkStruct (next (marshalled :: DebugUtilsObjectTagInfoEXT)) (\pPNext -> cont (VkDebugUtilsObjectTagInfoEXT VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT pPNext (objectType (marshalled :: DebugUtilsObjectTagInfoEXT)) (objectHandle (marshalled :: DebugUtilsObjectTagInfoEXT)) (tagName (marshalled :: DebugUtilsObjectTagInfoEXT)) (fromIntegral (Data.ByteString.length (tag (marshalled :: DebugUtilsObjectTagInfoEXT)))) (castPtr pPTag))))

-- | A function to read a 'VkDebugUtilsObjectTagInfoEXT' and all additional
-- structures in the pointer chain into a 'DebugUtilsObjectTagInfoEXT'.
fromCStructDebugUtilsObjectTagInfoEXT :: VkDebugUtilsObjectTagInfoEXT -> IO DebugUtilsObjectTagInfoEXT
fromCStructDebugUtilsObjectTagInfoEXT c = DebugUtilsObjectTagInfoEXT <$> -- Univalued Member elided
                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDebugUtilsObjectTagInfoEXT)))
                                                                     <*> pure (vkObjectType (c :: VkDebugUtilsObjectTagInfoEXT))
                                                                     <*> pure (vkObjectHandle (c :: VkDebugUtilsObjectTagInfoEXT))
                                                                     <*> pure (vkTagName (c :: VkDebugUtilsObjectTagInfoEXT))
                                                                     -- Bytestring length valued member elided
                                                                     <*> packCStringLen (castPtr (vkPTag (c :: VkDebugUtilsObjectTagInfoEXT)), fromIntegral (vkTagSize (c :: VkDebugUtilsObjectTagInfoEXT)))

instance Zero DebugUtilsObjectTagInfoEXT where
  zero = DebugUtilsObjectTagInfoEXT Nothing
                                    zero
                                    zero
                                    zero
                                    Data.ByteString.empty



-- | vkCmdBeginDebugUtilsLabelEXT - Open a command buffer debug label region
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @pLabelInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VkDebugUtilsLabelEXT'
--     structure specifying the parameters of the label region to open.
--
-- = Description
--
-- Unresolved directive in vkCmdBeginDebugUtilsLabelEXT.txt -
-- include::{generated}\/validity\/protos\/vkCmdBeginDebugUtilsLabelEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
cmdBeginDebugUtilsLabelEXT :: CommandBuffer ->  DebugUtilsLabelEXT ->  IO ()
cmdBeginDebugUtilsLabelEXT = \(CommandBuffer commandBuffer' commandTable) -> \labelInfo' -> (\marshalled -> withCStructDebugUtilsLabelEXT marshalled . flip with) labelInfo' (\pLabelInfo' -> vkCmdBeginDebugUtilsLabelEXT commandTable commandBuffer' pLabelInfo' *> (pure ()))


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
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.vkCmdBeginDebugUtilsLabelEXT'
-- and
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.vkCmdEndDebugUtilsLabelEXT'
-- /must/ be matched and balanced.
--
-- == Valid Usage
--
-- -   There /must/ be an outstanding
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.vkCmdBeginDebugUtilsLabelEXT'
--     command prior to the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.vkCmdEndDebugUtilsLabelEXT'
--     on the queue that @commandBuffer@ is submitted to
--
-- -   If @commandBuffer@ is a secondary command buffer, there /must/ be an
--     outstanding
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.vkCmdBeginDebugUtilsLabelEXT'
--     command recorded to @commandBuffer@ that has not previously been
--     ended by a call to
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.vkCmdEndDebugUtilsLabelEXT'.
--
-- Unresolved directive in vkCmdEndDebugUtilsLabelEXT.txt -
-- include::{generated}\/validity\/protos\/vkCmdEndDebugUtilsLabelEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
cmdEndDebugUtilsLabelEXT :: CommandBuffer ->  IO ()
cmdEndDebugUtilsLabelEXT = \(CommandBuffer commandBuffer' commandTable) -> vkCmdEndDebugUtilsLabelEXT commandTable commandBuffer' *> (pure ())


-- | vkCmdInsertDebugUtilsLabelEXT - Insert a label into a command buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @pInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VkDebugUtilsLabelEXT'
--     structure specifying the parameters of the label to insert.
--
-- = Description
--
-- Unresolved directive in vkCmdInsertDebugUtilsLabelEXT.txt -
-- include::{generated}\/validity\/protos\/vkCmdInsertDebugUtilsLabelEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
cmdInsertDebugUtilsLabelEXT :: CommandBuffer ->  DebugUtilsLabelEXT ->  IO ()
cmdInsertDebugUtilsLabelEXT = \(CommandBuffer commandBuffer' commandTable) -> \labelInfo' -> (\marshalled -> withCStructDebugUtilsLabelEXT marshalled . flip with) labelInfo' (\pLabelInfo' -> vkCmdInsertDebugUtilsLabelEXT commandTable commandBuffer' pLabelInfo' *> (pure ()))


-- | vkCreateDebugUtilsMessengerEXT - Create a debug messenger object
--
-- = Parameters
--
-- -   @instance@ the instance the messenger will be used with.
--
-- -   @pCreateInfo@ points to a
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VkDebugUtilsMessengerCreateInfoEXT'
--     structure which contains the callback pointer as well as defines the
--     conditions under which this messenger will trigger the callback.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pMessenger@ is a pointer to record the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VkDebugUtilsMessengerEXT'
--     object created.
--
-- = Description
--
-- Unresolved directive in vkCreateDebugUtilsMessengerEXT.txt -
-- include::{generated}\/validity\/protos\/vkCreateDebugUtilsMessengerEXT.txt[]
--
-- The application /must/ ensure that
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.vkCreateDebugUtilsMessengerEXT'
-- is not executed in parallel with any Vulkan command that is also called
-- with @instance@ or child of @instance@ as the dispatchable argument.
--
-- = See Also
--
-- No cross-references are available
createDebugUtilsMessengerEXT :: Instance ->  DebugUtilsMessengerCreateInfoEXT ->  Maybe AllocationCallbacks ->  IO (DebugUtilsMessengerEXT)
createDebugUtilsMessengerEXT = \(Instance instance' commandTable) -> \createInfo' -> \allocator -> alloca (\pMessenger' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructDebugUtilsMessengerCreateInfoEXT marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateDebugUtilsMessengerEXT commandTable instance' pCreateInfo' pAllocator pMessenger' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pMessenger')))))


-- | vkDestroyDebugUtilsMessengerEXT - Destroy a debug messenger object
--
-- = Parameters
--
-- -   @instance@ the instance where the callback was created.
--
-- -   @messenger@ the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VkDebugUtilsMessengerEXT'
--     object to destroy. @messenger@ is an externally synchronized object
--     and /must/ not be used on more than one thread at a time. This means
--     that
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.vkDestroyDebugUtilsMessengerEXT'
--     /must/ not be called when a callback is active.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   If
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @messenger@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @messenger@ was created, @pAllocator@ /must/ be
--     @NULL@
--
-- Unresolved directive in vkDestroyDebugUtilsMessengerEXT.txt -
-- include::{generated}\/validity\/protos\/vkDestroyDebugUtilsMessengerEXT.txt[]
--
-- The application /must/ ensure that
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.vkDestroyDebugUtilsMessengerEXT'
-- is not executed in parallel with any Vulkan command that is also called
-- with @instance@ or child of @instance@ as the dispatchable argument.
--
-- = See Also
--
-- No cross-references are available
destroyDebugUtilsMessengerEXT :: Instance ->  DebugUtilsMessengerEXT ->  Maybe AllocationCallbacks ->  IO ()
destroyDebugUtilsMessengerEXT = \(Instance instance' commandTable) -> \messenger' -> \allocator -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> vkDestroyDebugUtilsMessengerEXT commandTable instance' messenger' pAllocator *> (pure ()))


-- | vkQueueBeginDebugUtilsLabelEXT - Open a queue debug label region
--
-- = Parameters
--
-- -   @queue@ is the queue in which to start a debug label region.
--
-- -   @pLabelInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VkDebugUtilsLabelEXT'
--     structure specifying the parameters of the label region to open.
--
-- = Description
--
-- Unresolved directive in vkQueueBeginDebugUtilsLabelEXT.txt -
-- include::{generated}\/validity\/protos\/vkQueueBeginDebugUtilsLabelEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
queueBeginDebugUtilsLabelEXT :: Queue ->  DebugUtilsLabelEXT ->  IO ()
queueBeginDebugUtilsLabelEXT = \(Queue queue' commandTable) -> \labelInfo' -> (\marshalled -> withCStructDebugUtilsLabelEXT marshalled . flip with) labelInfo' (\pLabelInfo' -> vkQueueBeginDebugUtilsLabelEXT commandTable queue' pLabelInfo' *> (pure ()))


-- | vkQueueEndDebugUtilsLabelEXT - Close a queue debug label region
--
-- = Parameters
--
-- -   @queue@ is the queue in which a debug label region should be closed.
--
-- = Description
--
-- The calls to
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.vkQueueBeginDebugUtilsLabelEXT'
-- and
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.vkQueueEndDebugUtilsLabelEXT'
-- /must/ be matched and balanced.
--
-- == Valid Usage
--
-- -   There /must/ be an outstanding
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.vkQueueBeginDebugUtilsLabelEXT'
--     command prior to the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.vkQueueEndDebugUtilsLabelEXT'
--     on the queue
--
-- Unresolved directive in vkQueueEndDebugUtilsLabelEXT.txt -
-- include::{generated}\/validity\/protos\/vkQueueEndDebugUtilsLabelEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
queueEndDebugUtilsLabelEXT :: Queue ->  IO ()
queueEndDebugUtilsLabelEXT = \(Queue queue' commandTable) -> vkQueueEndDebugUtilsLabelEXT commandTable queue' *> (pure ())


-- | vkQueueInsertDebugUtilsLabelEXT - Insert a label into a queue
--
-- = Parameters
--
-- -   @queue@ is the queue into which a debug label will be inserted.
--
-- -   @pLabelInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VkDebugUtilsLabelEXT'
--     structure specifying the parameters of the label to insert.
--
-- = Description
--
-- Unresolved directive in vkQueueInsertDebugUtilsLabelEXT.txt -
-- include::{generated}\/validity\/protos\/vkQueueInsertDebugUtilsLabelEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
queueInsertDebugUtilsLabelEXT :: Queue ->  DebugUtilsLabelEXT ->  IO ()
queueInsertDebugUtilsLabelEXT = \(Queue queue' commandTable) -> \labelInfo' -> (\marshalled -> withCStructDebugUtilsLabelEXT marshalled . flip with) labelInfo' (\pLabelInfo' -> vkQueueInsertDebugUtilsLabelEXT commandTable queue' pLabelInfo' *> (pure ()))


-- | vkSetDebugUtilsObjectNameEXT - Give a user-friendly name to an object
--
-- = Parameters
--
-- -   @device@ is the device that created the object.
--
-- -   @pNameInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VkDebugUtilsObjectNameInfoEXT'
--     structure specifying the parameters of the name to set on the
--     object.
--
-- == Valid Usage
--
-- Unresolved directive in vkSetDebugUtilsObjectNameEXT.txt -
-- include::{generated}\/validity\/protos\/vkSetDebugUtilsObjectNameEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
setDebugUtilsObjectNameEXT :: Device ->  DebugUtilsObjectNameInfoEXT ->  IO ()
setDebugUtilsObjectNameEXT = \(Device device' commandTable) -> \nameInfo' -> (\marshalled -> withCStructDebugUtilsObjectNameInfoEXT marshalled . flip with) nameInfo' (\pNameInfo' -> vkSetDebugUtilsObjectNameEXT commandTable device' pNameInfo' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ())))


-- | vkSetDebugUtilsObjectTagEXT - Attach arbitrary data to an object
--
-- = Parameters
--
-- -   @device@ is the device that created the object.
--
-- -   @pTagInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VkDebugUtilsObjectTagInfoEXT'
--     structure specifying the parameters of the tag to attach to the
--     object.
--
-- = Description
--
-- Unresolved directive in vkSetDebugUtilsObjectTagEXT.txt -
-- include::{generated}\/validity\/protos\/vkSetDebugUtilsObjectTagEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
setDebugUtilsObjectTagEXT :: Device ->  DebugUtilsObjectTagInfoEXT ->  IO ()
setDebugUtilsObjectTagEXT = \(Device device' commandTable) -> \tagInfo' -> (\marshalled -> withCStructDebugUtilsObjectTagInfoEXT marshalled . flip with) tagInfo' (\pTagInfo' -> vkSetDebugUtilsObjectTagEXT commandTable device' pTagInfo' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ())))


-- | vkSubmitDebugUtilsMessageEXT - Inject a message into a debug stream
--
-- = Parameters
--
-- -   @instance@ is the debug stream’s
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance'.
--
-- -   @messageSeverity@ is the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VkDebugUtilsMessageSeverityFlagBitsEXT'
--     severity of this event\/message.
--
-- -   @messageTypes@ is a bitmask of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VkDebugUtilsMessageTypeFlagBitsEXT'
--     specifying which type of event(s) to identify with this message.
--
-- -   @pCallbackData@ contains all the callback related data in the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VkDebugUtilsMessengerCallbackDataEXT'
--     structure.
--
-- = Description
--
-- The call will propagate through the layers and generate callback(s) as
-- indicated by the message’s flags. The parameters are passed on to the
-- callback in addition to the @pUserData@ value that was defined at the
-- time the messenger was registered.
--
-- == Valid Usage
--
-- Unresolved directive in vkSubmitDebugUtilsMessageEXT.txt -
-- include::{generated}\/validity\/protos\/vkSubmitDebugUtilsMessageEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
submitDebugUtilsMessageEXT :: Instance ->  DebugUtilsMessageSeverityFlagBitsEXT ->  DebugUtilsMessageTypeFlagsEXT ->  DebugUtilsMessengerCallbackDataEXT ->  IO ()
submitDebugUtilsMessageEXT = \(Instance instance' commandTable) -> \messageSeverity' -> \messageTypes' -> \callbackData' -> (\marshalled -> withCStructDebugUtilsMessengerCallbackDataEXT marshalled . flip with) callbackData' (\pCallbackData' -> vkSubmitDebugUtilsMessageEXT commandTable instance' messageSeverity' messageTypes' pCallbackData' *> (pure ()))

-- | A safe wrapper for 'createDebugUtilsMessengerEXT' and 'destroyDebugUtilsMessengerEXT' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withDebugUtilsMessengerEXT
  :: Instance -> DebugUtilsMessengerCreateInfoEXT -> Maybe (AllocationCallbacks) -> (DebugUtilsMessengerEXT -> IO a) -> IO a
withDebugUtilsMessengerEXT instance' debugUtilsMessengerCreateInfoEXT allocationCallbacks = bracket
  (createDebugUtilsMessengerEXT instance' debugUtilsMessengerCreateInfoEXT allocationCallbacks)
  (\o -> destroyDebugUtilsMessengerEXT instance' o allocationCallbacks)
