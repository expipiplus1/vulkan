{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_debug_marker
  ( pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT
  , pattern VK_EXT_DEBUG_MARKER_SPEC_VERSION
  , pattern VK_EXT_DEBUG_MARKER_EXTENSION_NAME
  , vkDebugMarkerSetObjectNameEXT
  , vkDebugMarkerSetObjectTagEXT
  , vkCmdDebugMarkerBeginEXT
  , vkCmdDebugMarkerEndEXT
  , vkCmdDebugMarkerInsertEXT
  , VkDebugMarkerObjectNameInfoEXT(..)
  , VkDebugMarkerObjectTagInfoEXT(..)
  , VkDebugMarkerMarkerInfoEXT(..)
  ) where

import Data.String
  ( IsString
  )
import Data.Vector.Storable.Sized
  ( Vector
  )
import Data.Word
  ( Word64
  )
import Foreign.C.Types
  ( CFloat(..)
  , CChar(..)
  , CSize(..)
  )
import Foreign.Ptr
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkDevice
  )
import Graphics.Vulkan.Core10.Queue
  ( VkCommandBuffer
  )
import Graphics.Vulkan.Extensions.VK_EXT_debug_report
  ( VkDebugReportObjectTypeEXT(..)
  )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT"
pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT = VkStructureType 1000022000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT"
pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT = VkStructureType 1000022001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT"
pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT = VkStructureType 1000022002
-- No documentation found for TopLevel "VK_EXT_DEBUG_MARKER_SPEC_VERSION"
pattern VK_EXT_DEBUG_MARKER_SPEC_VERSION :: Integral a => a
pattern VK_EXT_DEBUG_MARKER_SPEC_VERSION = 4
-- No documentation found for TopLevel "VK_EXT_DEBUG_MARKER_EXTENSION_NAME"
pattern VK_EXT_DEBUG_MARKER_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_DEBUG_MARKER_EXTENSION_NAME = "VK_EXT_debug_marker"
-- | vkDebugMarkerSetObjectNameEXT - Give a user-friendly name to an object
--
-- = Parameters
-- #_parameters#
--
-- -   @device@ is the device that created the object.
--
-- -   @pNameInfo@ is a pointer to an instance of the
--     'VkDebugMarkerObjectNameInfoEXT' structure specifying the parameters
--     of the name to set on the object.
--
-- = Description
-- #_description#
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pNameInfo@ /must/ be a valid pointer to a valid
--     @VkDebugMarkerObjectNameInfoEXT@ structure
--
-- == Host Synchronization
--
-- -   Host access to @pNameInfo.object@ /must/ be externally synchronized
--
-- == Return Codes
--
-- [<#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
-- #_see_also#
--
-- 'VkDebugMarkerObjectNameInfoEXT',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice'
foreign import ccall "vkDebugMarkerSetObjectNameEXT" vkDebugMarkerSetObjectNameEXT :: ("device" ::: VkDevice) -> ("pNameInfo" ::: Ptr VkDebugMarkerObjectNameInfoEXT) -> IO VkResult
-- | vkDebugMarkerSetObjectTagEXT - Attach arbitrary data to an object
--
-- = Parameters
-- #_parameters#
--
-- -   @device@ is the device that created the object.
--
-- -   @pTagInfo@ is a pointer to an instance of the
--     'VkDebugMarkerObjectTagInfoEXT' structure specifying the parameters
--     of the tag to attach to the object.
--
-- = Description
-- #_description#
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pTagInfo@ /must/ be a valid pointer to a valid
--     @VkDebugMarkerObjectTagInfoEXT@ structure
--
-- == Host Synchronization
--
-- -   Host access to @pTagInfo.object@ /must/ be externally synchronized
--
-- == Return Codes
--
-- [<#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
-- #_see_also#
--
-- 'VkDebugMarkerObjectTagInfoEXT',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice'
foreign import ccall "vkDebugMarkerSetObjectTagEXT" vkDebugMarkerSetObjectTagEXT :: ("device" ::: VkDevice) -> ("pTagInfo" ::: Ptr VkDebugMarkerObjectTagInfoEXT) -> IO VkResult
-- | vkCmdDebugMarkerBeginEXT - Open a command buffer marker region
--
-- = Parameters
-- #_parameters#
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @pMarkerInfo@ is a pointer to an instance of the
--     'VkDebugMarkerMarkerInfoEXT' structure specifying the parameters of
--     the marker region to open.
--
-- = Description
-- #_description#
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @pMarkerInfo@ /must/ be a valid pointer to a valid
--     @VkDebugMarkerMarkerInfoEXT@ structure
--
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'VkDebugMarkerMarkerInfoEXT'
foreign import ccall "vkCmdDebugMarkerBeginEXT" vkCmdDebugMarkerBeginEXT :: ("commandBuffer" ::: VkCommandBuffer) -> ("pMarkerInfo" ::: Ptr VkDebugMarkerMarkerInfoEXT) -> IO ()
-- | vkCmdDebugMarkerEndEXT - Close a command buffer marker region
--
-- = Parameters
-- #_parameters#
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- = Description
-- #_description#
--
-- An application /may/ open a marker region in one command buffer and
-- close it in another, or otherwise split marker regions across multiple
-- command buffers or multiple queue submissions. When viewed from the
-- linear series of submissions to a single queue, the calls to
-- @vkCmdDebugMarkerBeginEXT@ and @vkCmdDebugMarkerEndEXT@ /must/ be
-- matched and balanced.
--
-- == Valid Usage
--
-- -   There /must/ be an outstanding 'vkCmdDebugMarkerBeginEXT' command
--     prior to the @vkCmdDebugMarkerEndEXT@ on the queue that
--     @commandBuffer@ is submitted to
--
-- -   If @commandBuffer@ is a secondary command buffer, there /must/ be an
--     outstanding 'vkCmdDebugMarkerBeginEXT' command recorded to
--     @commandBuffer@ that has not previously been ended by a call to
--     'vkCmdDebugMarkerEndEXT'.
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer'
foreign import ccall "vkCmdDebugMarkerEndEXT" vkCmdDebugMarkerEndEXT :: ("commandBuffer" ::: VkCommandBuffer) -> IO ()
-- | vkCmdDebugMarkerInsertEXT - Insert a marker label into a command buffer
--
-- = Parameters
-- #_parameters#
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @pMarkerInfo@ is a pointer to an instance of the
--     'VkDebugMarkerMarkerInfoEXT' structure specifying the parameters of
--     the marker to insert.
--
-- = Description
-- #_description#
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @pMarkerInfo@ /must/ be a valid pointer to a valid
--     @VkDebugMarkerMarkerInfoEXT@ structure
--
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'VkDebugMarkerMarkerInfoEXT'
foreign import ccall "vkCmdDebugMarkerInsertEXT" vkCmdDebugMarkerInsertEXT :: ("commandBuffer" ::: VkCommandBuffer) -> ("pMarkerInfo" ::: Ptr VkDebugMarkerMarkerInfoEXT) -> IO ()
-- | VkDebugMarkerObjectNameInfoEXT - Specify parameters of a name to give to
-- an object
--
-- = Description
-- #_description#
--
-- Applications /may/ change the name associated with an object simply by
-- calling @vkDebugMarkerSetObjectNameEXT@ again with a new string. To
-- remove a previously set name, @pObjectName@ /should/ be set to an empty
-- string.
--
-- == Valid Usage
--
-- -   @objectType@ /must/ not be @VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT@
--
-- -   @object@ /must/ not be
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE'
--
-- -   @object@ /must/ be a Vulkan object of the type associated with
--     @objectType@ as defined in
--     <{html_spec_relative}#debug-report-object-types {html_spec_relative}#debug-report-object-types>.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @objectType@ /must/ be a valid
--     'Graphics.Vulkan.Extensions.VK_EXT_debug_report.VkDebugReportObjectTypeEXT'
--     value
--
-- -   @pObjectName@ /must/ be a null-terminated UTF-8 string
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_report.VkDebugReportObjectTypeEXT',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkDebugMarkerSetObjectNameEXT'
data VkDebugMarkerObjectNameInfoEXT = VkDebugMarkerObjectNameInfoEXT
  { -- No documentation found for Nested "VkDebugMarkerObjectNameInfoEXT" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDebugMarkerObjectNameInfoEXT" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDebugMarkerObjectNameInfoEXT" "vkObjectType"
  vkObjectType :: VkDebugReportObjectTypeEXT
  , -- No documentation found for Nested "VkDebugMarkerObjectNameInfoEXT" "vkObject"
  vkObject :: Word64
  , -- No documentation found for Nested "VkDebugMarkerObjectNameInfoEXT" "vkPObjectName"
  vkPObjectName :: Ptr CChar
  }
  deriving (Eq, Show)

instance Storable VkDebugMarkerObjectNameInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkDebugMarkerObjectNameInfoEXT <$> peek (ptr `plusPtr` 0)
                                            <*> peek (ptr `plusPtr` 8)
                                            <*> peek (ptr `plusPtr` 16)
                                            <*> peek (ptr `plusPtr` 24)
                                            <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDebugMarkerObjectNameInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDebugMarkerObjectNameInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkObjectType (poked :: VkDebugMarkerObjectNameInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkObject (poked :: VkDebugMarkerObjectNameInfoEXT))
                *> poke (ptr `plusPtr` 32) (vkPObjectName (poked :: VkDebugMarkerObjectNameInfoEXT))
-- | VkDebugMarkerObjectTagInfoEXT - Specify parameters of a tag to attach to
-- an object
--
-- = Description
-- #_description#
--
-- The @tagName@ parameter gives a name or identifier to the type of data
-- being tagged. This can be used by debugging layers to easily filter for
-- only data that can be used by that implementation.
--
-- == Valid Usage
--
-- -   @objectType@ /must/ not be @VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT@
--
-- -   @object@ /must/ not be
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE'
--
-- -   @object@ /must/ be a Vulkan object of the type associated with
--     @objectType@ as defined in
--     <{html_spec_relative}#debug-report-object-types {html_spec_relative}#debug-report-object-types>.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @objectType@ /must/ be a valid
--     'Graphics.Vulkan.Extensions.VK_EXT_debug_report.VkDebugReportObjectTypeEXT'
--     value
--
-- -   @pTag@ /must/ be a valid pointer to an array of @tagSize@ bytes
--
-- -   @tagSize@ /must/ be greater than @0@
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_report.VkDebugReportObjectTypeEXT',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkDebugMarkerSetObjectTagEXT'
data VkDebugMarkerObjectTagInfoEXT = VkDebugMarkerObjectTagInfoEXT
  { -- No documentation found for Nested "VkDebugMarkerObjectTagInfoEXT" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDebugMarkerObjectTagInfoEXT" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDebugMarkerObjectTagInfoEXT" "vkObjectType"
  vkObjectType :: VkDebugReportObjectTypeEXT
  , -- No documentation found for Nested "VkDebugMarkerObjectTagInfoEXT" "vkObject"
  vkObject :: Word64
  , -- No documentation found for Nested "VkDebugMarkerObjectTagInfoEXT" "vkTagName"
  vkTagName :: Word64
  , -- No documentation found for Nested "VkDebugMarkerObjectTagInfoEXT" "vkTagSize"
  vkTagSize :: CSize
  , -- No documentation found for Nested "VkDebugMarkerObjectTagInfoEXT" "vkPTag"
  vkPTag :: Ptr ()
  }
  deriving (Eq, Show)

instance Storable VkDebugMarkerObjectTagInfoEXT where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = VkDebugMarkerObjectTagInfoEXT <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 8)
                                           <*> peek (ptr `plusPtr` 16)
                                           <*> peek (ptr `plusPtr` 24)
                                           <*> peek (ptr `plusPtr` 32)
                                           <*> peek (ptr `plusPtr` 40)
                                           <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDebugMarkerObjectTagInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDebugMarkerObjectTagInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkObjectType (poked :: VkDebugMarkerObjectTagInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkObject (poked :: VkDebugMarkerObjectTagInfoEXT))
                *> poke (ptr `plusPtr` 32) (vkTagName (poked :: VkDebugMarkerObjectTagInfoEXT))
                *> poke (ptr `plusPtr` 40) (vkTagSize (poked :: VkDebugMarkerObjectTagInfoEXT))
                *> poke (ptr `plusPtr` 48) (vkPTag (poked :: VkDebugMarkerObjectTagInfoEXT))
-- | VkDebugMarkerMarkerInfoEXT - Specify parameters of a command buffer
-- marker region
--
-- = Description
-- #_description#
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @pMarkerName@ /must/ be a null-terminated UTF-8 string
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkCmdDebugMarkerBeginEXT', 'vkCmdDebugMarkerInsertEXT'
data VkDebugMarkerMarkerInfoEXT = VkDebugMarkerMarkerInfoEXT
  { -- No documentation found for Nested "VkDebugMarkerMarkerInfoEXT" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDebugMarkerMarkerInfoEXT" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDebugMarkerMarkerInfoEXT" "vkPMarkerName"
  vkPMarkerName :: Ptr CChar
  , -- No documentation found for Nested "VkDebugMarkerMarkerInfoEXT" "vkColor"
  vkColor :: Vector 4 CFloat
  }
  deriving (Eq, Show)

instance Storable VkDebugMarkerMarkerInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkDebugMarkerMarkerInfoEXT <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
                                        <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDebugMarkerMarkerInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDebugMarkerMarkerInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkPMarkerName (poked :: VkDebugMarkerMarkerInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkColor (poked :: VkDebugMarkerMarkerInfoEXT))
