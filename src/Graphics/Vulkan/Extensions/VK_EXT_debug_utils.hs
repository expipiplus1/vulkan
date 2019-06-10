{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Extensions.VK_EXT_debug_utils
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  DebugUtilsLabelEXT(..)
  , 
#endif
  DebugUtilsMessageSeverityFlagBitsEXT
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
#if defined(VK_USE_PLATFORM_GGP)
  , DebugUtilsMessengerCallbackDataEXT(..)
#endif
  , DebugUtilsMessengerCallbackDataFlagsEXT
  , DebugUtilsMessengerCreateFlagsEXT
#if defined(VK_USE_PLATFORM_GGP)
  , DebugUtilsMessengerCreateInfoEXT(..)
#endif
  , DebugUtilsMessengerEXT
#if defined(VK_USE_PLATFORM_GGP)
  , DebugUtilsObjectNameInfoEXT(..)
  , DebugUtilsObjectTagInfoEXT(..)
#endif
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
  , pattern EXT_DEBUG_UTILS_EXTENSION_NAME
  , pattern EXT_DEBUG_UTILS_SPEC_VERSION
  , pattern STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT
  , pattern STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT
  , pattern STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT
  , pattern STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT
  , pattern STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT
  , pattern OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT
  ) where

import Control.Exception
  ( bracket
  )

#if defined(VK_USE_PLATFORM_GGP)
import Data.ByteString
  ( ByteString
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Data.Int
  ( Int32
  )
#endif
import Data.String
  ( IsString
  )

#if defined(VK_USE_PLATFORM_GGP)
import Data.Vector
  ( Vector
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Data.Word
  ( Word64
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.C.Types
  ( CSize(..)
  )
#endif
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybeWith
  , with
  )

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Ptr
  ( Ptr
  , nullPtr
  )
#endif
import Foreign.Storable
  ( peek
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils
  ( VkDebugUtilsMessageSeverityFlagBitsEXT(..)
  , VkDebugUtilsMessageTypeFlagBitsEXT(..)
  , VkDebugUtilsMessengerCallbackDataFlagsEXT(..)
  , VkDebugUtilsMessengerCreateFlagsEXT(..)
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
  , pattern VK_EXT_DEBUG_UTILS_EXTENSION_NAME
  , pattern VK_EXT_DEBUG_UTILS_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils
  ( PFN_vkDebugUtilsMessengerCallbackEXT
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Core
  ( ObjectType
  )
#endif
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , Instance(..)
  )
import Graphics.Vulkan.Core10.Queue
  ( CommandBuffer(..)
  , Queue(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT
  , pattern STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT
  , pattern STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT
  , pattern STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT
  , pattern STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDebugUtilsLabelEXT"
data DebugUtilsLabelEXT = DebugUtilsLabelEXT
  { -- No documentation found for Nested "DebugUtilsLabelEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DebugUtilsLabelEXT" "pLabelName"
  labelName :: ByteString
  , -- No documentation found for Nested "DebugUtilsLabelEXT" "color"
  color :: (Float, Float, Float, Float)
  }
  deriving (Show, Eq)

instance Zero DebugUtilsLabelEXT where
  zero = DebugUtilsLabelEXT Nothing
                            mempty
                            (zero, zero, zero, zero)

#endif

-- No documentation found for TopLevel "DebugUtilsMessageSeverityFlagBitsEXT"
type DebugUtilsMessageSeverityFlagBitsEXT = VkDebugUtilsMessageSeverityFlagBitsEXT


{-# complete DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT, DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT, DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT, DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT :: DebugUtilsMessageSeverityFlagBitsEXT #-}


-- No documentation found for Nested "DebugUtilsMessageSeverityFlagBitsEXT" "DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT"
pattern DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT :: (a ~ DebugUtilsMessageSeverityFlagBitsEXT) => a
pattern DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT = VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT


-- No documentation found for Nested "DebugUtilsMessageSeverityFlagBitsEXT" "DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT"
pattern DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT :: (a ~ DebugUtilsMessageSeverityFlagBitsEXT) => a
pattern DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT = VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT


-- No documentation found for Nested "DebugUtilsMessageSeverityFlagBitsEXT" "DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT"
pattern DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT :: (a ~ DebugUtilsMessageSeverityFlagBitsEXT) => a
pattern DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT = VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT


-- No documentation found for Nested "DebugUtilsMessageSeverityFlagBitsEXT" "DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT"
pattern DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT :: (a ~ DebugUtilsMessageSeverityFlagBitsEXT) => a
pattern DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT = VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT

-- No documentation found for TopLevel "DebugUtilsMessageSeverityFlagsEXT"
type DebugUtilsMessageSeverityFlagsEXT = DebugUtilsMessageSeverityFlagBitsEXT

-- No documentation found for TopLevel "DebugUtilsMessageTypeFlagBitsEXT"
type DebugUtilsMessageTypeFlagBitsEXT = VkDebugUtilsMessageTypeFlagBitsEXT


{-# complete DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT, DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT, DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT :: DebugUtilsMessageTypeFlagBitsEXT #-}


-- No documentation found for Nested "DebugUtilsMessageTypeFlagBitsEXT" "DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT"
pattern DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT :: (a ~ DebugUtilsMessageTypeFlagBitsEXT) => a
pattern DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT = VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT


-- No documentation found for Nested "DebugUtilsMessageTypeFlagBitsEXT" "DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT"
pattern DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT :: (a ~ DebugUtilsMessageTypeFlagBitsEXT) => a
pattern DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT = VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT


-- No documentation found for Nested "DebugUtilsMessageTypeFlagBitsEXT" "DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT"
pattern DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT :: (a ~ DebugUtilsMessageTypeFlagBitsEXT) => a
pattern DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT = VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT

-- No documentation found for TopLevel "DebugUtilsMessageTypeFlagsEXT"
type DebugUtilsMessageTypeFlagsEXT = DebugUtilsMessageTypeFlagBitsEXT


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDebugUtilsMessengerCallbackDataEXT"
data DebugUtilsMessengerCallbackDataEXT = DebugUtilsMessengerCallbackDataEXT
  { -- No documentation found for Nested "DebugUtilsMessengerCallbackDataEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DebugUtilsMessengerCallbackDataEXT" "flags"
  flags :: DebugUtilsMessengerCallbackDataFlagsEXT
  , -- No documentation found for Nested "DebugUtilsMessengerCallbackDataEXT" "pMessageIdName"
  messageIdName :: Maybe ByteString
  , -- No documentation found for Nested "DebugUtilsMessengerCallbackDataEXT" "messageIdNumber"
  messageIdNumber :: Int32
  , -- No documentation found for Nested "DebugUtilsMessengerCallbackDataEXT" "pMessage"
  message :: ByteString
  , -- No documentation found for Nested "DebugUtilsMessengerCallbackDataEXT" "pQueueLabels"
  queueLabels :: Vector DebugUtilsLabelEXT
  , -- No documentation found for Nested "DebugUtilsMessengerCallbackDataEXT" "pCmdBufLabels"
  cmdBufLabels :: Vector DebugUtilsLabelEXT
  , -- No documentation found for Nested "DebugUtilsMessengerCallbackDataEXT" "pObjects"
  objects :: Vector DebugUtilsObjectNameInfoEXT
  }
  deriving (Show, Eq)

instance Zero DebugUtilsMessengerCallbackDataEXT where
  zero = DebugUtilsMessengerCallbackDataEXT Nothing
                                            zero
                                            mempty
                                            zero
                                            mempty
                                            mempty
                                            mempty
                                            mempty

#endif

-- No documentation found for TopLevel "DebugUtilsMessengerCallbackDataFlagsEXT"
type DebugUtilsMessengerCallbackDataFlagsEXT = VkDebugUtilsMessengerCallbackDataFlagsEXT


-- No complete pragma for DebugUtilsMessengerCallbackDataFlagsEXT as it has no patterns

-- No documentation found for TopLevel "DebugUtilsMessengerCreateFlagsEXT"
type DebugUtilsMessengerCreateFlagsEXT = VkDebugUtilsMessengerCreateFlagsEXT


-- No complete pragma for DebugUtilsMessengerCreateFlagsEXT as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDebugUtilsMessengerCreateInfoEXT"
data DebugUtilsMessengerCreateInfoEXT = DebugUtilsMessengerCreateInfoEXT
  { -- No documentation found for Nested "DebugUtilsMessengerCreateInfoEXT" "pNext"
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

instance Zero DebugUtilsMessengerCreateInfoEXT where
  zero = DebugUtilsMessengerCreateInfoEXT Nothing
                                          zero
                                          zero
                                          zero
                                          zero
                                          nullPtr

#endif

-- No documentation found for TopLevel "DebugUtilsMessengerEXT"
type DebugUtilsMessengerEXT = VkDebugUtilsMessengerEXT


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDebugUtilsObjectNameInfoEXT"
data DebugUtilsObjectNameInfoEXT = DebugUtilsObjectNameInfoEXT
  { -- No documentation found for Nested "DebugUtilsObjectNameInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DebugUtilsObjectNameInfoEXT" "objectType"
  objectType :: ObjectType
  , -- No documentation found for Nested "DebugUtilsObjectNameInfoEXT" "objectHandle"
  objectHandle :: Word64
  , -- No documentation found for Nested "DebugUtilsObjectNameInfoEXT" "pObjectName"
  objectName :: Maybe ByteString
  }
  deriving (Show, Eq)

instance Zero DebugUtilsObjectNameInfoEXT where
  zero = DebugUtilsObjectNameInfoEXT Nothing
                                     zero
                                     zero
                                     mempty

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDebugUtilsObjectTagInfoEXT"
data DebugUtilsObjectTagInfoEXT = DebugUtilsObjectTagInfoEXT
  { -- No documentation found for Nested "DebugUtilsObjectTagInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DebugUtilsObjectTagInfoEXT" "objectType"
  objectType :: ObjectType
  , -- No documentation found for Nested "DebugUtilsObjectTagInfoEXT" "objectHandle"
  objectHandle :: Word64
  , -- No documentation found for Nested "DebugUtilsObjectTagInfoEXT" "tagName"
  tagName :: Word64
  , -- No documentation found for Nested "DebugUtilsObjectTagInfoEXT" "tagSize"
  tagSize :: CSize
  , -- No documentation found for Nested "DebugUtilsObjectTagInfoEXT" "pTag"
  tag :: Ptr ()
  }
  deriving (Show, Eq)

instance Zero DebugUtilsObjectTagInfoEXT where
  zero = DebugUtilsObjectTagInfoEXT Nothing
                                    zero
                                    zero
                                    zero
                                    zero
                                    nullPtr

#endif


-- No documentation found for TopLevel "vkCmdBeginDebugUtilsLabelEXT"
cmdBeginDebugUtilsLabelEXT :: CommandBuffer ->  DebugUtilsLabelEXT ->  IO ()
cmdBeginDebugUtilsLabelEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdEndDebugUtilsLabelEXT"
cmdEndDebugUtilsLabelEXT :: CommandBuffer ->  IO ()
cmdEndDebugUtilsLabelEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdInsertDebugUtilsLabelEXT"
cmdInsertDebugUtilsLabelEXT :: CommandBuffer ->  DebugUtilsLabelEXT ->  IO ()
cmdInsertDebugUtilsLabelEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCreateDebugUtilsMessengerEXT"
createDebugUtilsMessengerEXT :: Instance ->  DebugUtilsMessengerCreateInfoEXT ->  Maybe AllocationCallbacks ->  IO (DebugUtilsMessengerEXT)
createDebugUtilsMessengerEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkDestroyDebugUtilsMessengerEXT"
destroyDebugUtilsMessengerEXT :: Instance ->  DebugUtilsMessengerEXT ->  Maybe AllocationCallbacks ->  IO ()
destroyDebugUtilsMessengerEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkQueueBeginDebugUtilsLabelEXT"
queueBeginDebugUtilsLabelEXT :: Queue ->  DebugUtilsLabelEXT ->  IO ()
queueBeginDebugUtilsLabelEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkQueueEndDebugUtilsLabelEXT"
queueEndDebugUtilsLabelEXT :: Queue ->  IO ()
queueEndDebugUtilsLabelEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkQueueInsertDebugUtilsLabelEXT"
queueInsertDebugUtilsLabelEXT :: Queue ->  DebugUtilsLabelEXT ->  IO ()
queueInsertDebugUtilsLabelEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkSetDebugUtilsObjectNameEXT"
setDebugUtilsObjectNameEXT :: Device ->  DebugUtilsObjectNameInfoEXT ->  IO ()
setDebugUtilsObjectNameEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkSetDebugUtilsObjectTagEXT"
setDebugUtilsObjectTagEXT :: Device ->  DebugUtilsObjectTagInfoEXT ->  IO ()
setDebugUtilsObjectTagEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkSubmitDebugUtilsMessageEXT"
submitDebugUtilsMessageEXT :: Instance ->  DebugUtilsMessageSeverityFlagBitsEXT ->  DebugUtilsMessageTypeFlagsEXT ->  DebugUtilsMessengerCallbackDataEXT ->  IO ()
submitDebugUtilsMessageEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- | A safe wrapper for 'createDebugUtilsMessengerEXT' and 'destroyDebugUtilsMessengerEXT' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withDebugUtilsMessengerEXT
  :: Instance -> DebugUtilsMessengerCreateInfoEXT -> Maybe AllocationCallbacks -> (DebugUtilsMessengerEXT -> IO a) -> IO a
withDebugUtilsMessengerEXT instance' debugUtilsMessengerCreateInfoEXT allocationCallbacks = bracket
  (createDebugUtilsMessengerEXT instance' debugUtilsMessengerCreateInfoEXT allocationCallbacks)
  (\o -> destroyDebugUtilsMessengerEXT instance' o allocationCallbacks)

-- No documentation found for TopLevel "VK_EXT_DEBUG_UTILS_EXTENSION_NAME"
pattern EXT_DEBUG_UTILS_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_DEBUG_UTILS_EXTENSION_NAME = VK_EXT_DEBUG_UTILS_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_DEBUG_UTILS_SPEC_VERSION"
pattern EXT_DEBUG_UTILS_SPEC_VERSION :: Integral a => a
pattern EXT_DEBUG_UTILS_SPEC_VERSION = VK_EXT_DEBUG_UTILS_SPEC_VERSION
