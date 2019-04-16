{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_debug_utils
  ( withCStructDebugUtilsLabelEXT
  , fromCStructDebugUtilsLabelEXT
  , DebugUtilsLabelEXT(..)
  , DebugUtilsMessageSeverityFlagBitsEXT
  , DebugUtilsMessageSeverityFlagsEXT
  , DebugUtilsMessageTypeFlagBitsEXT
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
  ( throwIO
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
  ( length
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
  ( generateM
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
import qualified Graphics.Vulkan.C.Dynamic
  ( cmdBeginDebugUtilsLabelEXT
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
  )


import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_SUCCESS
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


-- No documentation found for TopLevel "DebugUtilsLabelEXT"
data DebugUtilsLabelEXT = DebugUtilsLabelEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "DebugUtilsLabelEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DebugUtilsLabelEXT" "pLabelName"
  vkPLabelName :: ByteString
  , -- No documentation found for Nested "DebugUtilsLabelEXT" "color"
  vkColor :: (CFloat, CFloat, CFloat, CFloat)
  }
  deriving (Show, Eq)
withCStructDebugUtilsLabelEXT :: DebugUtilsLabelEXT -> (VkDebugUtilsLabelEXT -> IO a) -> IO a
withCStructDebugUtilsLabelEXT from cont = useAsCString (vkPLabelName (from :: DebugUtilsLabelEXT)) (\pLabelName -> maybeWith withSomeVkStruct (vkPNext (from :: DebugUtilsLabelEXT)) (\pPNext -> cont (VkDebugUtilsLabelEXT VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT pPNext pLabelName (fromTuple (vkColor (from :: DebugUtilsLabelEXT))))))
fromCStructDebugUtilsLabelEXT :: VkDebugUtilsLabelEXT -> IO DebugUtilsLabelEXT
fromCStructDebugUtilsLabelEXT c = DebugUtilsLabelEXT <$> -- Univalued Member elided
                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDebugUtilsLabelEXT)))
                                                     <*> packCString (vkPLabelName (c :: VkDebugUtilsLabelEXT))
                                                     <*> pure (let x = (vkColor (c :: VkDebugUtilsLabelEXT)) in ( Data.Vector.Storable.Sized.unsafeIndex x 0
                                                     , Data.Vector.Storable.Sized.unsafeIndex x 1
                                                     , Data.Vector.Storable.Sized.unsafeIndex x 2
                                                     , Data.Vector.Storable.Sized.unsafeIndex x 3 ))
-- No documentation found for TopLevel "DebugUtilsMessageSeverityFlagBitsEXT"
type DebugUtilsMessageSeverityFlagBitsEXT = VkDebugUtilsMessageSeverityFlagBitsEXT
-- No documentation found for TopLevel "DebugUtilsMessageSeverityFlagsEXT"
type DebugUtilsMessageSeverityFlagsEXT = DebugUtilsMessageSeverityFlagBitsEXT
-- No documentation found for TopLevel "DebugUtilsMessageTypeFlagBitsEXT"
type DebugUtilsMessageTypeFlagBitsEXT = VkDebugUtilsMessageTypeFlagBitsEXT
-- No documentation found for TopLevel "DebugUtilsMessageTypeFlagsEXT"
type DebugUtilsMessageTypeFlagsEXT = DebugUtilsMessageTypeFlagBitsEXT
-- No documentation found for TopLevel "DebugUtilsMessengerCallbackDataEXT"
data DebugUtilsMessengerCallbackDataEXT = DebugUtilsMessengerCallbackDataEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "DebugUtilsMessengerCallbackDataEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DebugUtilsMessengerCallbackDataEXT" "flags"
  vkFlags :: DebugUtilsMessengerCallbackDataFlagsEXT
  , -- No documentation found for Nested "DebugUtilsMessengerCallbackDataEXT" "pMessageIdName"
  vkPMessageIdName :: Maybe ByteString
  , -- No documentation found for Nested "DebugUtilsMessengerCallbackDataEXT" "messageIdNumber"
  vkMessageIdNumber :: Int32
  , -- No documentation found for Nested "DebugUtilsMessengerCallbackDataEXT" "pMessage"
  vkPMessage :: ByteString
  -- Length valued member elided
  , -- No documentation found for Nested "DebugUtilsMessengerCallbackDataEXT" "pQueueLabels"
  vkPQueueLabels :: Vector DebugUtilsLabelEXT
  -- Length valued member elided
  , -- No documentation found for Nested "DebugUtilsMessengerCallbackDataEXT" "pCmdBufLabels"
  vkPCmdBufLabels :: Vector DebugUtilsLabelEXT
  -- Length valued member elided
  , -- No documentation found for Nested "DebugUtilsMessengerCallbackDataEXT" "pObjects"
  vkPObjects :: Vector DebugUtilsObjectNameInfoEXT
  }
  deriving (Show, Eq)
withCStructDebugUtilsMessengerCallbackDataEXT :: DebugUtilsMessengerCallbackDataEXT -> (VkDebugUtilsMessengerCallbackDataEXT -> IO a) -> IO a
withCStructDebugUtilsMessengerCallbackDataEXT from cont = withVec withCStructDebugUtilsObjectNameInfoEXT (vkPObjects (from :: DebugUtilsMessengerCallbackDataEXT)) (\pObjects -> withVec withCStructDebugUtilsLabelEXT (vkPCmdBufLabels (from :: DebugUtilsMessengerCallbackDataEXT)) (\pCmdBufLabels -> withVec withCStructDebugUtilsLabelEXT (vkPQueueLabels (from :: DebugUtilsMessengerCallbackDataEXT)) (\pQueueLabels -> useAsCString (vkPMessage (from :: DebugUtilsMessengerCallbackDataEXT)) (\pMessage -> maybeWith useAsCString (vkPMessageIdName (from :: DebugUtilsMessengerCallbackDataEXT)) (\pMessageIdName -> maybeWith withSomeVkStruct (vkPNext (from :: DebugUtilsMessengerCallbackDataEXT)) (\pPNext -> cont (VkDebugUtilsMessengerCallbackDataEXT VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT pPNext (vkFlags (from :: DebugUtilsMessengerCallbackDataEXT)) pMessageIdName (vkMessageIdNumber (from :: DebugUtilsMessengerCallbackDataEXT)) pMessage (fromIntegral (Data.Vector.length (vkPQueueLabels (from :: DebugUtilsMessengerCallbackDataEXT)))) pQueueLabels (fromIntegral (Data.Vector.length (vkPCmdBufLabels (from :: DebugUtilsMessengerCallbackDataEXT)))) pCmdBufLabels (fromIntegral (Data.Vector.length (vkPObjects (from :: DebugUtilsMessengerCallbackDataEXT)))) pObjects)))))))
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
-- No documentation found for TopLevel "DebugUtilsMessengerCallbackDataFlagsEXT"
type DebugUtilsMessengerCallbackDataFlagsEXT = VkDebugUtilsMessengerCallbackDataFlagsEXT
-- No documentation found for TopLevel "DebugUtilsMessengerCreateFlagsEXT"
type DebugUtilsMessengerCreateFlagsEXT = VkDebugUtilsMessengerCreateFlagsEXT
-- No documentation found for TopLevel "DebugUtilsMessengerCreateInfoEXT"
data DebugUtilsMessengerCreateInfoEXT = DebugUtilsMessengerCreateInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "DebugUtilsMessengerCreateInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DebugUtilsMessengerCreateInfoEXT" "flags"
  vkFlags :: DebugUtilsMessengerCreateFlagsEXT
  , -- No documentation found for Nested "DebugUtilsMessengerCreateInfoEXT" "messageSeverity"
  vkMessageSeverity :: DebugUtilsMessageSeverityFlagsEXT
  , -- No documentation found for Nested "DebugUtilsMessengerCreateInfoEXT" "messageType"
  vkMessageType :: DebugUtilsMessageTypeFlagsEXT
  , -- No documentation found for Nested "DebugUtilsMessengerCreateInfoEXT" "pfnUserCallback"
  vkPfnUserCallback :: PFN_vkDebugUtilsMessengerCallbackEXT
  , -- No documentation found for Nested "DebugUtilsMessengerCreateInfoEXT" "pUserData"
  vkPUserData :: Ptr ()
  }
  deriving (Show, Eq)
withCStructDebugUtilsMessengerCreateInfoEXT :: DebugUtilsMessengerCreateInfoEXT -> (VkDebugUtilsMessengerCreateInfoEXT -> IO a) -> IO a
withCStructDebugUtilsMessengerCreateInfoEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: DebugUtilsMessengerCreateInfoEXT)) (\pPNext -> cont (VkDebugUtilsMessengerCreateInfoEXT VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT pPNext (vkFlags (from :: DebugUtilsMessengerCreateInfoEXT)) (vkMessageSeverity (from :: DebugUtilsMessengerCreateInfoEXT)) (vkMessageType (from :: DebugUtilsMessengerCreateInfoEXT)) (vkPfnUserCallback (from :: DebugUtilsMessengerCreateInfoEXT)) (vkPUserData (from :: DebugUtilsMessengerCreateInfoEXT))))
fromCStructDebugUtilsMessengerCreateInfoEXT :: VkDebugUtilsMessengerCreateInfoEXT -> IO DebugUtilsMessengerCreateInfoEXT
fromCStructDebugUtilsMessengerCreateInfoEXT c = DebugUtilsMessengerCreateInfoEXT <$> -- Univalued Member elided
                                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDebugUtilsMessengerCreateInfoEXT)))
                                                                                 <*> pure (vkFlags (c :: VkDebugUtilsMessengerCreateInfoEXT))
                                                                                 <*> pure (vkMessageSeverity (c :: VkDebugUtilsMessengerCreateInfoEXT))
                                                                                 <*> pure (vkMessageType (c :: VkDebugUtilsMessengerCreateInfoEXT))
                                                                                 <*> pure (vkPfnUserCallback (c :: VkDebugUtilsMessengerCreateInfoEXT))
                                                                                 <*> pure (vkPUserData (c :: VkDebugUtilsMessengerCreateInfoEXT))
-- No documentation found for TopLevel "DebugUtilsMessengerEXT"
type DebugUtilsMessengerEXT = VkDebugUtilsMessengerEXT
-- No documentation found for TopLevel "DebugUtilsObjectNameInfoEXT"
data DebugUtilsObjectNameInfoEXT = DebugUtilsObjectNameInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "DebugUtilsObjectNameInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DebugUtilsObjectNameInfoEXT" "objectType"
  vkObjectType :: ObjectType
  , -- No documentation found for Nested "DebugUtilsObjectNameInfoEXT" "objectHandle"
  vkObjectHandle :: Word64
  , -- No documentation found for Nested "DebugUtilsObjectNameInfoEXT" "pObjectName"
  vkPObjectName :: Maybe ByteString
  }
  deriving (Show, Eq)
withCStructDebugUtilsObjectNameInfoEXT :: DebugUtilsObjectNameInfoEXT -> (VkDebugUtilsObjectNameInfoEXT -> IO a) -> IO a
withCStructDebugUtilsObjectNameInfoEXT from cont = maybeWith useAsCString (vkPObjectName (from :: DebugUtilsObjectNameInfoEXT)) (\pObjectName -> maybeWith withSomeVkStruct (vkPNext (from :: DebugUtilsObjectNameInfoEXT)) (\pPNext -> cont (VkDebugUtilsObjectNameInfoEXT VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT pPNext (vkObjectType (from :: DebugUtilsObjectNameInfoEXT)) (vkObjectHandle (from :: DebugUtilsObjectNameInfoEXT)) pObjectName)))
fromCStructDebugUtilsObjectNameInfoEXT :: VkDebugUtilsObjectNameInfoEXT -> IO DebugUtilsObjectNameInfoEXT
fromCStructDebugUtilsObjectNameInfoEXT c = DebugUtilsObjectNameInfoEXT <$> -- Univalued Member elided
                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDebugUtilsObjectNameInfoEXT)))
                                                                       <*> pure (vkObjectType (c :: VkDebugUtilsObjectNameInfoEXT))
                                                                       <*> pure (vkObjectHandle (c :: VkDebugUtilsObjectNameInfoEXT))
                                                                       <*> maybePeek packCString (vkPObjectName (c :: VkDebugUtilsObjectNameInfoEXT))
-- No documentation found for TopLevel "DebugUtilsObjectTagInfoEXT"
data DebugUtilsObjectTagInfoEXT = DebugUtilsObjectTagInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "DebugUtilsObjectTagInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DebugUtilsObjectTagInfoEXT" "objectType"
  vkObjectType :: ObjectType
  , -- No documentation found for Nested "DebugUtilsObjectTagInfoEXT" "objectHandle"
  vkObjectHandle :: Word64
  , -- No documentation found for Nested "DebugUtilsObjectTagInfoEXT" "tagName"
  vkTagName :: Word64
  -- Bytestring length valued member elided
  , -- No documentation found for Nested "DebugUtilsObjectTagInfoEXT" "pTag"
  vkPTag :: ByteString
  }
  deriving (Show, Eq)
withCStructDebugUtilsObjectTagInfoEXT :: DebugUtilsObjectTagInfoEXT -> (VkDebugUtilsObjectTagInfoEXT -> IO a) -> IO a
withCStructDebugUtilsObjectTagInfoEXT from cont = unsafeUseAsCString (vkPTag (from :: DebugUtilsObjectTagInfoEXT)) (\pTag -> maybeWith withSomeVkStruct (vkPNext (from :: DebugUtilsObjectTagInfoEXT)) (\pPNext -> cont (VkDebugUtilsObjectTagInfoEXT VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT pPNext (vkObjectType (from :: DebugUtilsObjectTagInfoEXT)) (vkObjectHandle (from :: DebugUtilsObjectTagInfoEXT)) (vkTagName (from :: DebugUtilsObjectTagInfoEXT)) (fromIntegral (Data.ByteString.length (vkPTag (from :: DebugUtilsObjectTagInfoEXT)))) (castPtr pTag))))
fromCStructDebugUtilsObjectTagInfoEXT :: VkDebugUtilsObjectTagInfoEXT -> IO DebugUtilsObjectTagInfoEXT
fromCStructDebugUtilsObjectTagInfoEXT c = DebugUtilsObjectTagInfoEXT <$> -- Univalued Member elided
                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDebugUtilsObjectTagInfoEXT)))
                                                                     <*> pure (vkObjectType (c :: VkDebugUtilsObjectTagInfoEXT))
                                                                     <*> pure (vkObjectHandle (c :: VkDebugUtilsObjectTagInfoEXT))
                                                                     <*> pure (vkTagName (c :: VkDebugUtilsObjectTagInfoEXT))
                                                                     -- Bytestring length valued member elided
                                                                     <*> packCStringLen (castPtr (vkPTag (c :: VkDebugUtilsObjectTagInfoEXT)), fromIntegral (vkTagSize (c :: VkDebugUtilsObjectTagInfoEXT)))

-- | Wrapper for vkCmdBeginDebugUtilsLabelEXT
cmdBeginDebugUtilsLabelEXT :: CommandBuffer ->  DebugUtilsLabelEXT ->  IO ()
cmdBeginDebugUtilsLabelEXT = \(CommandBuffer commandBuffer commandTable) -> \labelInfo -> (\a -> withCStructDebugUtilsLabelEXT a . flip with) labelInfo (\pLabelInfo -> Graphics.Vulkan.C.Dynamic.cmdBeginDebugUtilsLabelEXT commandTable commandBuffer pLabelInfo *> (pure ()))

-- | Wrapper for vkCmdEndDebugUtilsLabelEXT
cmdEndDebugUtilsLabelEXT :: CommandBuffer ->  IO ()
cmdEndDebugUtilsLabelEXT = \(CommandBuffer commandBuffer commandTable) -> Graphics.Vulkan.C.Dynamic.cmdEndDebugUtilsLabelEXT commandTable commandBuffer *> (pure ())

-- | Wrapper for vkCmdInsertDebugUtilsLabelEXT
cmdInsertDebugUtilsLabelEXT :: CommandBuffer ->  DebugUtilsLabelEXT ->  IO ()
cmdInsertDebugUtilsLabelEXT = \(CommandBuffer commandBuffer commandTable) -> \labelInfo -> (\a -> withCStructDebugUtilsLabelEXT a . flip with) labelInfo (\pLabelInfo -> Graphics.Vulkan.C.Dynamic.cmdInsertDebugUtilsLabelEXT commandTable commandBuffer pLabelInfo *> (pure ()))

-- | Wrapper for vkCreateDebugUtilsMessengerEXT
createDebugUtilsMessengerEXT :: Instance ->  DebugUtilsMessengerCreateInfoEXT ->  Maybe AllocationCallbacks ->  IO (DebugUtilsMessengerEXT)
createDebugUtilsMessengerEXT = \(Instance instance' commandTable) -> \createInfo -> \allocator -> alloca (\pMessenger -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructDebugUtilsMessengerCreateInfoEXT a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createDebugUtilsMessengerEXT commandTable instance' pCreateInfo pAllocator pMessenger >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pMessenger)))))

-- | Wrapper for vkDestroyDebugUtilsMessengerEXT
destroyDebugUtilsMessengerEXT :: Instance ->  DebugUtilsMessengerEXT ->  Maybe AllocationCallbacks ->  IO ()
destroyDebugUtilsMessengerEXT = \(Instance instance' commandTable) -> \messenger -> \allocator -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> Graphics.Vulkan.C.Dynamic.destroyDebugUtilsMessengerEXT commandTable instance' messenger pAllocator *> (pure ()))

-- | Wrapper for vkQueueBeginDebugUtilsLabelEXT
queueBeginDebugUtilsLabelEXT :: Queue ->  DebugUtilsLabelEXT ->  IO ()
queueBeginDebugUtilsLabelEXT = \(Queue queue commandTable) -> \labelInfo -> (\a -> withCStructDebugUtilsLabelEXT a . flip with) labelInfo (\pLabelInfo -> Graphics.Vulkan.C.Dynamic.queueBeginDebugUtilsLabelEXT commandTable queue pLabelInfo *> (pure ()))

-- | Wrapper for vkQueueEndDebugUtilsLabelEXT
queueEndDebugUtilsLabelEXT :: Queue ->  IO ()
queueEndDebugUtilsLabelEXT = \(Queue queue commandTable) -> Graphics.Vulkan.C.Dynamic.queueEndDebugUtilsLabelEXT commandTable queue *> (pure ())

-- | Wrapper for vkQueueInsertDebugUtilsLabelEXT
queueInsertDebugUtilsLabelEXT :: Queue ->  DebugUtilsLabelEXT ->  IO ()
queueInsertDebugUtilsLabelEXT = \(Queue queue commandTable) -> \labelInfo -> (\a -> withCStructDebugUtilsLabelEXT a . flip with) labelInfo (\pLabelInfo -> Graphics.Vulkan.C.Dynamic.queueInsertDebugUtilsLabelEXT commandTable queue pLabelInfo *> (pure ()))

-- | Wrapper for vkSetDebugUtilsObjectNameEXT
setDebugUtilsObjectNameEXT :: Device ->  DebugUtilsObjectNameInfoEXT ->  IO ()
setDebugUtilsObjectNameEXT = \(Device device commandTable) -> \nameInfo -> (\a -> withCStructDebugUtilsObjectNameInfoEXT a . flip with) nameInfo (\pNameInfo -> Graphics.Vulkan.C.Dynamic.setDebugUtilsObjectNameEXT commandTable device pNameInfo >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure ())))

-- | Wrapper for vkSetDebugUtilsObjectTagEXT
setDebugUtilsObjectTagEXT :: Device ->  DebugUtilsObjectTagInfoEXT ->  IO ()
setDebugUtilsObjectTagEXT = \(Device device commandTable) -> \tagInfo -> (\a -> withCStructDebugUtilsObjectTagInfoEXT a . flip with) tagInfo (\pTagInfo -> Graphics.Vulkan.C.Dynamic.setDebugUtilsObjectTagEXT commandTable device pTagInfo >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure ())))

-- | Wrapper for vkSubmitDebugUtilsMessageEXT
submitDebugUtilsMessageEXT :: Instance ->  DebugUtilsMessageSeverityFlagBitsEXT ->  DebugUtilsMessageTypeFlagsEXT ->  DebugUtilsMessengerCallbackDataEXT ->  IO ()
submitDebugUtilsMessageEXT = \(Instance instance' commandTable) -> \messageSeverity -> \messageTypes -> \callbackData -> (\a -> withCStructDebugUtilsMessengerCallbackDataEXT a . flip with) callbackData (\pCallbackData -> Graphics.Vulkan.C.Dynamic.submitDebugUtilsMessageEXT commandTable instance' messageSeverity messageTypes pCallbackData *> (pure ()))
