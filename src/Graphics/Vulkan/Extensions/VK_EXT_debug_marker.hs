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
  ( empty
  , length
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


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_debug_marker
  ( VkDebugMarkerMarkerInfoEXT(..)
  , VkDebugMarkerObjectNameInfoEXT(..)
  , VkDebugMarkerObjectTagInfoEXT(..)
  , vkCmdDebugMarkerBeginEXT
  , vkCmdDebugMarkerEndEXT
  , vkCmdDebugMarkerInsertEXT
  , vkDebugMarkerSetObjectNameEXT
  , vkDebugMarkerSetObjectTagEXT
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



-- | VkDebugMarkerMarkerInfoEXT - Specify parameters of a command buffer
-- marker region
--
-- = Description
--
-- Unresolved directive in VkDebugMarkerMarkerInfoEXT.txt -
-- include::{generated}\/validity\/structs\/VkDebugMarkerMarkerInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data DebugMarkerMarkerInfoEXT = DebugMarkerMarkerInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "DebugMarkerMarkerInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DebugMarkerMarkerInfoEXT" "pMarkerName"
  markerName :: ByteString
  , -- No documentation found for Nested "DebugMarkerMarkerInfoEXT" "color"
  color :: (CFloat, CFloat, CFloat, CFloat)
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDebugMarkerMarkerInfoEXT' and
-- marshal a 'DebugMarkerMarkerInfoEXT' into it. The 'VkDebugMarkerMarkerInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDebugMarkerMarkerInfoEXT :: DebugMarkerMarkerInfoEXT -> (VkDebugMarkerMarkerInfoEXT -> IO a) -> IO a
withCStructDebugMarkerMarkerInfoEXT marshalled cont = useAsCString (markerName (marshalled :: DebugMarkerMarkerInfoEXT)) (\pPMarkerName -> maybeWith withSomeVkStruct (next (marshalled :: DebugMarkerMarkerInfoEXT)) (\pPNext -> cont (VkDebugMarkerMarkerInfoEXT VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT pPNext pPMarkerName (fromTuple (color (marshalled :: DebugMarkerMarkerInfoEXT))))))

-- | A function to read a 'VkDebugMarkerMarkerInfoEXT' and all additional
-- structures in the pointer chain into a 'DebugMarkerMarkerInfoEXT'.
fromCStructDebugMarkerMarkerInfoEXT :: VkDebugMarkerMarkerInfoEXT -> IO DebugMarkerMarkerInfoEXT
fromCStructDebugMarkerMarkerInfoEXT c = DebugMarkerMarkerInfoEXT <$> -- Univalued Member elided
                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDebugMarkerMarkerInfoEXT)))
                                                                 <*> packCString (vkPMarkerName (c :: VkDebugMarkerMarkerInfoEXT))
                                                                 <*> pure (let v = (vkColor (c :: VkDebugMarkerMarkerInfoEXT)) in ( Data.Vector.Storable.Sized.unsafeIndex v 0
                                                                 , Data.Vector.Storable.Sized.unsafeIndex v 1
                                                                 , Data.Vector.Storable.Sized.unsafeIndex v 2
                                                                 , Data.Vector.Storable.Sized.unsafeIndex v 3 ))

instance Zero DebugMarkerMarkerInfoEXT where
  zero = DebugMarkerMarkerInfoEXT Nothing
                                  Data.ByteString.empty
                                  (zero, zero, zero, zero)



-- | VkDebugMarkerObjectNameInfoEXT - Specify parameters of a name to give to
-- an object
--
-- = Description
--
-- Applications /may/ change the name associated with an object simply by
-- calling
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_marker.vkDebugMarkerSetObjectNameEXT'
-- again with a new string. To remove a previously set name, @pObjectName@
-- /should/ be set to an empty string.
--
-- == Valid Usage
--
-- Unresolved directive in VkDebugMarkerObjectNameInfoEXT.txt -
-- include::{generated}\/validity\/structs\/VkDebugMarkerObjectNameInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data DebugMarkerObjectNameInfoEXT = DebugMarkerObjectNameInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "DebugMarkerObjectNameInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DebugMarkerObjectNameInfoEXT" "objectType"
  objectType :: DebugReportObjectTypeEXT
  , -- No documentation found for Nested "DebugMarkerObjectNameInfoEXT" "object"
  object :: Word64
  , -- No documentation found for Nested "DebugMarkerObjectNameInfoEXT" "pObjectName"
  objectName :: ByteString
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDebugMarkerObjectNameInfoEXT' and
-- marshal a 'DebugMarkerObjectNameInfoEXT' into it. The 'VkDebugMarkerObjectNameInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDebugMarkerObjectNameInfoEXT :: DebugMarkerObjectNameInfoEXT -> (VkDebugMarkerObjectNameInfoEXT -> IO a) -> IO a
withCStructDebugMarkerObjectNameInfoEXT marshalled cont = useAsCString (objectName (marshalled :: DebugMarkerObjectNameInfoEXT)) (\pPObjectName -> maybeWith withSomeVkStruct (next (marshalled :: DebugMarkerObjectNameInfoEXT)) (\pPNext -> cont (VkDebugMarkerObjectNameInfoEXT VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT pPNext (objectType (marshalled :: DebugMarkerObjectNameInfoEXT)) (object (marshalled :: DebugMarkerObjectNameInfoEXT)) pPObjectName)))

-- | A function to read a 'VkDebugMarkerObjectNameInfoEXT' and all additional
-- structures in the pointer chain into a 'DebugMarkerObjectNameInfoEXT'.
fromCStructDebugMarkerObjectNameInfoEXT :: VkDebugMarkerObjectNameInfoEXT -> IO DebugMarkerObjectNameInfoEXT
fromCStructDebugMarkerObjectNameInfoEXT c = DebugMarkerObjectNameInfoEXT <$> -- Univalued Member elided
                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDebugMarkerObjectNameInfoEXT)))
                                                                         <*> pure (vkObjectType (c :: VkDebugMarkerObjectNameInfoEXT))
                                                                         <*> pure (vkObject (c :: VkDebugMarkerObjectNameInfoEXT))
                                                                         <*> packCString (vkPObjectName (c :: VkDebugMarkerObjectNameInfoEXT))

instance Zero DebugMarkerObjectNameInfoEXT where
  zero = DebugMarkerObjectNameInfoEXT Nothing
                                      zero
                                      zero
                                      Data.ByteString.empty



-- | VkDebugMarkerObjectTagInfoEXT - Specify parameters of a tag to attach to
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
-- Unresolved directive in VkDebugMarkerObjectTagInfoEXT.txt -
-- include::{generated}\/validity\/structs\/VkDebugMarkerObjectTagInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data DebugMarkerObjectTagInfoEXT = DebugMarkerObjectTagInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "DebugMarkerObjectTagInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DebugMarkerObjectTagInfoEXT" "objectType"
  objectType :: DebugReportObjectTypeEXT
  , -- No documentation found for Nested "DebugMarkerObjectTagInfoEXT" "object"
  object :: Word64
  , -- No documentation found for Nested "DebugMarkerObjectTagInfoEXT" "tagName"
  tagName :: Word64
  -- Bytestring length valued member elided
  , -- No documentation found for Nested "DebugMarkerObjectTagInfoEXT" "pTag"
  tag :: ByteString
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDebugMarkerObjectTagInfoEXT' and
-- marshal a 'DebugMarkerObjectTagInfoEXT' into it. The 'VkDebugMarkerObjectTagInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDebugMarkerObjectTagInfoEXT :: DebugMarkerObjectTagInfoEXT -> (VkDebugMarkerObjectTagInfoEXT -> IO a) -> IO a
withCStructDebugMarkerObjectTagInfoEXT marshalled cont = unsafeUseAsCString (tag (marshalled :: DebugMarkerObjectTagInfoEXT)) (\pPTag -> maybeWith withSomeVkStruct (next (marshalled :: DebugMarkerObjectTagInfoEXT)) (\pPNext -> cont (VkDebugMarkerObjectTagInfoEXT VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT pPNext (objectType (marshalled :: DebugMarkerObjectTagInfoEXT)) (object (marshalled :: DebugMarkerObjectTagInfoEXT)) (tagName (marshalled :: DebugMarkerObjectTagInfoEXT)) (fromIntegral (Data.ByteString.length (tag (marshalled :: DebugMarkerObjectTagInfoEXT)))) (castPtr pPTag))))

-- | A function to read a 'VkDebugMarkerObjectTagInfoEXT' and all additional
-- structures in the pointer chain into a 'DebugMarkerObjectTagInfoEXT'.
fromCStructDebugMarkerObjectTagInfoEXT :: VkDebugMarkerObjectTagInfoEXT -> IO DebugMarkerObjectTagInfoEXT
fromCStructDebugMarkerObjectTagInfoEXT c = DebugMarkerObjectTagInfoEXT <$> -- Univalued Member elided
                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDebugMarkerObjectTagInfoEXT)))
                                                                       <*> pure (vkObjectType (c :: VkDebugMarkerObjectTagInfoEXT))
                                                                       <*> pure (vkObject (c :: VkDebugMarkerObjectTagInfoEXT))
                                                                       <*> pure (vkTagName (c :: VkDebugMarkerObjectTagInfoEXT))
                                                                       -- Bytestring length valued member elided
                                                                       <*> packCStringLen (castPtr (vkPTag (c :: VkDebugMarkerObjectTagInfoEXT)), fromIntegral (vkTagSize (c :: VkDebugMarkerObjectTagInfoEXT)))

instance Zero DebugMarkerObjectTagInfoEXT where
  zero = DebugMarkerObjectTagInfoEXT Nothing
                                     zero
                                     zero
                                     zero
                                     Data.ByteString.empty



-- | vkCmdDebugMarkerBeginEXT - Open a command buffer marker region
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @pMarkerInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_marker.VkDebugMarkerMarkerInfoEXT'
--     structure specifying the parameters of the marker region to open.
--
-- = Description
--
-- Unresolved directive in vkCmdDebugMarkerBeginEXT.txt -
-- include::{generated}\/validity\/protos\/vkCmdDebugMarkerBeginEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
cmdDebugMarkerBeginEXT :: CommandBuffer ->  DebugMarkerMarkerInfoEXT ->  IO ()
cmdDebugMarkerBeginEXT = \(CommandBuffer commandBuffer' commandTable) -> \markerInfo' -> (\marshalled -> withCStructDebugMarkerMarkerInfoEXT marshalled . flip with) markerInfo' (\pMarkerInfo' -> vkCmdDebugMarkerBeginEXT commandTable commandBuffer' pMarkerInfo' *> (pure ()))


-- | vkCmdDebugMarkerEndEXT - Close a command buffer marker region
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- = Description
--
-- An application /may/ open a marker region in one command buffer and
-- close it in another, or otherwise split marker regions across multiple
-- command buffers or multiple queue submissions. When viewed from the
-- linear series of submissions to a single queue, the calls to
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_marker.vkCmdDebugMarkerBeginEXT'
-- and
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_marker.vkCmdDebugMarkerEndEXT'
-- /must/ be matched and balanced.
--
-- == Valid Usage
--
-- -   There /must/ be an outstanding
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_marker.vkCmdDebugMarkerBeginEXT'
--     command prior to the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_marker.vkCmdDebugMarkerEndEXT'
--     on the queue that @commandBuffer@ is submitted to
--
-- -   If @commandBuffer@ is a secondary command buffer, there /must/ be an
--     outstanding
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_marker.vkCmdDebugMarkerBeginEXT'
--     command recorded to @commandBuffer@ that has not previously been
--     ended by a call to
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_marker.vkCmdDebugMarkerEndEXT'.
--
-- Unresolved directive in vkCmdDebugMarkerEndEXT.txt -
-- include::{generated}\/validity\/protos\/vkCmdDebugMarkerEndEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
cmdDebugMarkerEndEXT :: CommandBuffer ->  IO ()
cmdDebugMarkerEndEXT = \(CommandBuffer commandBuffer' commandTable) -> vkCmdDebugMarkerEndEXT commandTable commandBuffer' *> (pure ())


-- | vkCmdDebugMarkerInsertEXT - Insert a marker label into a command buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @pMarkerInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_marker.VkDebugMarkerMarkerInfoEXT'
--     structure specifying the parameters of the marker to insert.
--
-- = Description
--
-- Unresolved directive in vkCmdDebugMarkerInsertEXT.txt -
-- include::{generated}\/validity\/protos\/vkCmdDebugMarkerInsertEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
cmdDebugMarkerInsertEXT :: CommandBuffer ->  DebugMarkerMarkerInfoEXT ->  IO ()
cmdDebugMarkerInsertEXT = \(CommandBuffer commandBuffer' commandTable) -> \markerInfo' -> (\marshalled -> withCStructDebugMarkerMarkerInfoEXT marshalled . flip with) markerInfo' (\pMarkerInfo' -> vkCmdDebugMarkerInsertEXT commandTable commandBuffer' pMarkerInfo' *> (pure ()))


-- | vkDebugMarkerSetObjectNameEXT - Give a user-friendly name to an object
--
-- = Parameters
--
-- -   @device@ is the device that created the object.
--
-- -   @pNameInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_marker.VkDebugMarkerObjectNameInfoEXT'
--     structure specifying the parameters of the name to set on the
--     object.
--
-- = Description
--
-- Unresolved directive in vkDebugMarkerSetObjectNameEXT.txt -
-- include::{generated}\/validity\/protos\/vkDebugMarkerSetObjectNameEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
debugMarkerSetObjectNameEXT :: Device ->  DebugMarkerObjectNameInfoEXT ->  IO ()
debugMarkerSetObjectNameEXT = \(Device device' commandTable) -> \nameInfo' -> (\marshalled -> withCStructDebugMarkerObjectNameInfoEXT marshalled . flip with) nameInfo' (\pNameInfo' -> vkDebugMarkerSetObjectNameEXT commandTable device' pNameInfo' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ())))


-- | vkDebugMarkerSetObjectTagEXT - Attach arbitrary data to an object
--
-- = Parameters
--
-- -   @device@ is the device that created the object.
--
-- -   @pTagInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_marker.VkDebugMarkerObjectTagInfoEXT'
--     structure specifying the parameters of the tag to attach to the
--     object.
--
-- = Description
--
-- Unresolved directive in vkDebugMarkerSetObjectTagEXT.txt -
-- include::{generated}\/validity\/protos\/vkDebugMarkerSetObjectTagEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
debugMarkerSetObjectTagEXT :: Device ->  DebugMarkerObjectTagInfoEXT ->  IO ()
debugMarkerSetObjectTagEXT = \(Device device' commandTable) -> \tagInfo' -> (\marshalled -> withCStructDebugMarkerObjectTagInfoEXT marshalled . flip with) tagInfo' (\pTagInfo' -> vkDebugMarkerSetObjectTagEXT commandTable device' pTagInfo' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ())))
