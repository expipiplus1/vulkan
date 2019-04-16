{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_debug_marker
  ( withCStructDebugMarkerMarkerInfoEXT
  , fromCStructDebugMarkerMarkerInfoEXT
  , DebugMarkerMarkerInfoEXT(..)
  , withCStructDebugMarkerObjectNameInfoEXT
  , fromCStructDebugMarkerObjectNameInfoEXT
  , DebugMarkerObjectNameInfoEXT(..)
  , withCStructDebugMarkerObjectTagInfoEXT
  , fromCStructDebugMarkerObjectTagInfoEXT
  , DebugMarkerObjectTagInfoEXT(..)
  , cmdDebugMarkerBeginEXT
  , cmdDebugMarkerEndEXT
  , cmdDebugMarkerInsertEXT
  , debugMarkerSetObjectNameEXT
  , debugMarkerSetObjectTagEXT
  , pattern VK_EXT_DEBUG_MARKER_SPEC_VERSION
  , pattern VK_EXT_DEBUG_MARKER_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT
  , DebugReportObjectTypeEXT
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( when
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
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( cmdDebugMarkerBeginEXT
  , cmdDebugMarkerEndEXT
  , cmdDebugMarkerInsertEXT
  , debugMarkerSetObjectNameEXT
  , debugMarkerSetObjectTagEXT
  )


import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_debug_marker
  ( VkDebugMarkerMarkerInfoEXT(..)
  , VkDebugMarkerObjectNameInfoEXT(..)
  , VkDebugMarkerObjectTagInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )
import Graphics.Vulkan.Core10.Queue
  ( CommandBuffer(..)
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_debug_report
  ( DebugReportObjectTypeEXT
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_debug_marker
  ( pattern VK_EXT_DEBUG_MARKER_EXTENSION_NAME
  , pattern VK_EXT_DEBUG_MARKER_SPEC_VERSION
  )


-- No documentation found for TopLevel "DebugMarkerMarkerInfoEXT"
data DebugMarkerMarkerInfoEXT = DebugMarkerMarkerInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "DebugMarkerMarkerInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DebugMarkerMarkerInfoEXT" "pMarkerName"
  vkPMarkerName :: ByteString
  , -- No documentation found for Nested "DebugMarkerMarkerInfoEXT" "color"
  vkColor :: (CFloat, CFloat, CFloat, CFloat)
  }
  deriving (Show, Eq)
withCStructDebugMarkerMarkerInfoEXT :: DebugMarkerMarkerInfoEXT -> (VkDebugMarkerMarkerInfoEXT -> IO a) -> IO a
withCStructDebugMarkerMarkerInfoEXT from cont = useAsCString (vkPMarkerName (from :: DebugMarkerMarkerInfoEXT)) (\pMarkerName -> maybeWith withSomeVkStruct (vkPNext (from :: DebugMarkerMarkerInfoEXT)) (\pPNext -> cont (VkDebugMarkerMarkerInfoEXT VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT pPNext pMarkerName (fromTuple (vkColor (from :: DebugMarkerMarkerInfoEXT))))))
fromCStructDebugMarkerMarkerInfoEXT :: VkDebugMarkerMarkerInfoEXT -> IO DebugMarkerMarkerInfoEXT
fromCStructDebugMarkerMarkerInfoEXT c = DebugMarkerMarkerInfoEXT <$> -- Univalued Member elided
                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDebugMarkerMarkerInfoEXT)))
                                                                 <*> packCString (vkPMarkerName (c :: VkDebugMarkerMarkerInfoEXT))
                                                                 <*> pure (let x = (vkColor (c :: VkDebugMarkerMarkerInfoEXT)) in ( Data.Vector.Storable.Sized.unsafeIndex x 0
                                                                 , Data.Vector.Storable.Sized.unsafeIndex x 1
                                                                 , Data.Vector.Storable.Sized.unsafeIndex x 2
                                                                 , Data.Vector.Storable.Sized.unsafeIndex x 3 ))
-- No documentation found for TopLevel "DebugMarkerObjectNameInfoEXT"
data DebugMarkerObjectNameInfoEXT = DebugMarkerObjectNameInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "DebugMarkerObjectNameInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DebugMarkerObjectNameInfoEXT" "objectType"
  vkObjectType :: DebugReportObjectTypeEXT
  , -- No documentation found for Nested "DebugMarkerObjectNameInfoEXT" "object"
  vkObject :: Word64
  , -- No documentation found for Nested "DebugMarkerObjectNameInfoEXT" "pObjectName"
  vkPObjectName :: ByteString
  }
  deriving (Show, Eq)
withCStructDebugMarkerObjectNameInfoEXT :: DebugMarkerObjectNameInfoEXT -> (VkDebugMarkerObjectNameInfoEXT -> IO a) -> IO a
withCStructDebugMarkerObjectNameInfoEXT from cont = useAsCString (vkPObjectName (from :: DebugMarkerObjectNameInfoEXT)) (\pObjectName -> maybeWith withSomeVkStruct (vkPNext (from :: DebugMarkerObjectNameInfoEXT)) (\pPNext -> cont (VkDebugMarkerObjectNameInfoEXT VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT pPNext (vkObjectType (from :: DebugMarkerObjectNameInfoEXT)) (vkObject (from :: DebugMarkerObjectNameInfoEXT)) pObjectName)))
fromCStructDebugMarkerObjectNameInfoEXT :: VkDebugMarkerObjectNameInfoEXT -> IO DebugMarkerObjectNameInfoEXT
fromCStructDebugMarkerObjectNameInfoEXT c = DebugMarkerObjectNameInfoEXT <$> -- Univalued Member elided
                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDebugMarkerObjectNameInfoEXT)))
                                                                         <*> pure (vkObjectType (c :: VkDebugMarkerObjectNameInfoEXT))
                                                                         <*> pure (vkObject (c :: VkDebugMarkerObjectNameInfoEXT))
                                                                         <*> packCString (vkPObjectName (c :: VkDebugMarkerObjectNameInfoEXT))
-- No documentation found for TopLevel "DebugMarkerObjectTagInfoEXT"
data DebugMarkerObjectTagInfoEXT = DebugMarkerObjectTagInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "DebugMarkerObjectTagInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DebugMarkerObjectTagInfoEXT" "objectType"
  vkObjectType :: DebugReportObjectTypeEXT
  , -- No documentation found for Nested "DebugMarkerObjectTagInfoEXT" "object"
  vkObject :: Word64
  , -- No documentation found for Nested "DebugMarkerObjectTagInfoEXT" "tagName"
  vkTagName :: Word64
  -- Bytestring length valued member elided
  , -- No documentation found for Nested "DebugMarkerObjectTagInfoEXT" "pTag"
  vkPTag :: ByteString
  }
  deriving (Show, Eq)
withCStructDebugMarkerObjectTagInfoEXT :: DebugMarkerObjectTagInfoEXT -> (VkDebugMarkerObjectTagInfoEXT -> IO a) -> IO a
withCStructDebugMarkerObjectTagInfoEXT from cont = unsafeUseAsCString (vkPTag (from :: DebugMarkerObjectTagInfoEXT)) (\pTag -> maybeWith withSomeVkStruct (vkPNext (from :: DebugMarkerObjectTagInfoEXT)) (\pPNext -> cont (VkDebugMarkerObjectTagInfoEXT VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT pPNext (vkObjectType (from :: DebugMarkerObjectTagInfoEXT)) (vkObject (from :: DebugMarkerObjectTagInfoEXT)) (vkTagName (from :: DebugMarkerObjectTagInfoEXT)) (fromIntegral (Data.ByteString.length (vkPTag (from :: DebugMarkerObjectTagInfoEXT)))) (castPtr pTag))))
fromCStructDebugMarkerObjectTagInfoEXT :: VkDebugMarkerObjectTagInfoEXT -> IO DebugMarkerObjectTagInfoEXT
fromCStructDebugMarkerObjectTagInfoEXT c = DebugMarkerObjectTagInfoEXT <$> -- Univalued Member elided
                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDebugMarkerObjectTagInfoEXT)))
                                                                       <*> pure (vkObjectType (c :: VkDebugMarkerObjectTagInfoEXT))
                                                                       <*> pure (vkObject (c :: VkDebugMarkerObjectTagInfoEXT))
                                                                       <*> pure (vkTagName (c :: VkDebugMarkerObjectTagInfoEXT))
                                                                       -- Bytestring length valued member elided
                                                                       <*> packCStringLen (castPtr (vkPTag (c :: VkDebugMarkerObjectTagInfoEXT)), fromIntegral (vkTagSize (c :: VkDebugMarkerObjectTagInfoEXT)))

-- | Wrapper for 'vkCmdDebugMarkerBeginEXT'
cmdDebugMarkerBeginEXT :: CommandBuffer ->  DebugMarkerMarkerInfoEXT ->  IO ()
cmdDebugMarkerBeginEXT = \(CommandBuffer commandBuffer commandTable) -> \markerInfo -> (\a -> withCStructDebugMarkerMarkerInfoEXT a . flip with) markerInfo (\pMarkerInfo -> Graphics.Vulkan.C.Dynamic.cmdDebugMarkerBeginEXT commandTable commandBuffer pMarkerInfo *> (pure ()))

-- | Wrapper for 'vkCmdDebugMarkerEndEXT'
cmdDebugMarkerEndEXT :: CommandBuffer ->  IO ()
cmdDebugMarkerEndEXT = \(CommandBuffer commandBuffer commandTable) -> Graphics.Vulkan.C.Dynamic.cmdDebugMarkerEndEXT commandTable commandBuffer *> (pure ())

-- | Wrapper for 'vkCmdDebugMarkerInsertEXT'
cmdDebugMarkerInsertEXT :: CommandBuffer ->  DebugMarkerMarkerInfoEXT ->  IO ()
cmdDebugMarkerInsertEXT = \(CommandBuffer commandBuffer commandTable) -> \markerInfo -> (\a -> withCStructDebugMarkerMarkerInfoEXT a . flip with) markerInfo (\pMarkerInfo -> Graphics.Vulkan.C.Dynamic.cmdDebugMarkerInsertEXT commandTable commandBuffer pMarkerInfo *> (pure ()))

-- | Wrapper for 'vkDebugMarkerSetObjectNameEXT'
debugMarkerSetObjectNameEXT :: Device ->  DebugMarkerObjectNameInfoEXT ->  IO ()
debugMarkerSetObjectNameEXT = \(Device device commandTable) -> \nameInfo -> (\a -> withCStructDebugMarkerObjectNameInfoEXT a . flip with) nameInfo (\pNameInfo -> Graphics.Vulkan.C.Dynamic.debugMarkerSetObjectNameEXT commandTable device pNameInfo >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure ())))

-- | Wrapper for 'vkDebugMarkerSetObjectTagEXT'
debugMarkerSetObjectTagEXT :: Device ->  DebugMarkerObjectTagInfoEXT ->  IO ()
debugMarkerSetObjectTagEXT = \(Device device commandTable) -> \tagInfo -> (\a -> withCStructDebugMarkerObjectTagInfoEXT a . flip with) tagInfo (\pTagInfo -> Graphics.Vulkan.C.Dynamic.debugMarkerSetObjectTagEXT commandTable device pTagInfo >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure ())))
