{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_EXT_debug_marker
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  DebugMarkerMarkerInfoEXT(..)
  , 
  DebugMarkerObjectNameInfoEXT(..)
  , DebugMarkerObjectTagInfoEXT(..)
#endif
  , cmdDebugMarkerBeginEXT
  , cmdDebugMarkerEndEXT
  , cmdDebugMarkerInsertEXT
  , debugMarkerSetObjectNameEXT
  , debugMarkerSetObjectTagEXT
  , pattern EXT_DEBUG_MARKER_EXTENSION_NAME
  , pattern EXT_DEBUG_MARKER_SPEC_VERSION
  , DebugReportObjectTypeEXT
  , pattern STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT
  , pattern STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT
  , pattern STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT
  ) where


#if defined(VK_USE_PLATFORM_GGP)
import Data.ByteString
  ( ByteString
  )
#endif
import Data.String
  ( IsString
  )

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
import Foreign.Marshal.Utils
  ( with
  )

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Ptr
  ( Ptr
  , nullPtr
  )
#endif



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_debug_marker
  ( vkCmdDebugMarkerBeginEXT
  , vkCmdDebugMarkerEndEXT
  , vkCmdDebugMarkerInsertEXT
  , vkDebugMarkerSetObjectNameEXT
  , vkDebugMarkerSetObjectTagEXT
  , pattern VK_EXT_DEBUG_MARKER_EXTENSION_NAME
  , pattern VK_EXT_DEBUG_MARKER_SPEC_VERSION
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )
import Graphics.Vulkan.Core10.Queue
  ( CommandBuffer(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_EXT_debug_report
  ( DebugReportObjectTypeEXT
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT
  , pattern STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT
  , pattern STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_debug_report
  ( DebugReportObjectTypeEXT
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDebugMarkerMarkerInfoEXT"
data DebugMarkerMarkerInfoEXT = DebugMarkerMarkerInfoEXT
  { -- No documentation found for Nested "DebugMarkerMarkerInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DebugMarkerMarkerInfoEXT" "pMarkerName"
  markerName :: ByteString
  , -- No documentation found for Nested "DebugMarkerMarkerInfoEXT" "color"
  color :: (Float, Float, Float, Float)
  }
  deriving (Show, Eq)

instance Zero DebugMarkerMarkerInfoEXT where
  zero = DebugMarkerMarkerInfoEXT Nothing
                                  mempty
                                  (zero, zero, zero, zero)

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDebugMarkerObjectNameInfoEXT"
data DebugMarkerObjectNameInfoEXT = DebugMarkerObjectNameInfoEXT
  { -- No documentation found for Nested "DebugMarkerObjectNameInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DebugMarkerObjectNameInfoEXT" "objectType"
  objectType :: DebugReportObjectTypeEXT
  , -- No documentation found for Nested "DebugMarkerObjectNameInfoEXT" "object"
  object :: Word64
  , -- No documentation found for Nested "DebugMarkerObjectNameInfoEXT" "pObjectName"
  objectName :: ByteString
  }
  deriving (Show, Eq)

instance Zero DebugMarkerObjectNameInfoEXT where
  zero = DebugMarkerObjectNameInfoEXT Nothing
                                      zero
                                      zero
                                      mempty

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDebugMarkerObjectTagInfoEXT"
data DebugMarkerObjectTagInfoEXT = DebugMarkerObjectTagInfoEXT
  { -- No documentation found for Nested "DebugMarkerObjectTagInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DebugMarkerObjectTagInfoEXT" "objectType"
  objectType :: DebugReportObjectTypeEXT
  , -- No documentation found for Nested "DebugMarkerObjectTagInfoEXT" "object"
  object :: Word64
  , -- No documentation found for Nested "DebugMarkerObjectTagInfoEXT" "tagName"
  tagName :: Word64
  , -- No documentation found for Nested "DebugMarkerObjectTagInfoEXT" "tagSize"
  tagSize :: CSize
  , -- No documentation found for Nested "DebugMarkerObjectTagInfoEXT" "pTag"
  tag :: Ptr ()
  }
  deriving (Show, Eq)

instance Zero DebugMarkerObjectTagInfoEXT where
  zero = DebugMarkerObjectTagInfoEXT Nothing
                                     zero
                                     zero
                                     zero
                                     zero
                                     nullPtr

#endif


-- No documentation found for TopLevel "vkCmdDebugMarkerBeginEXT"
cmdDebugMarkerBeginEXT :: CommandBuffer ->  DebugMarkerMarkerInfoEXT ->  IO ()
cmdDebugMarkerBeginEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdDebugMarkerEndEXT"
cmdDebugMarkerEndEXT :: CommandBuffer ->  IO ()
cmdDebugMarkerEndEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdDebugMarkerInsertEXT"
cmdDebugMarkerInsertEXT :: CommandBuffer ->  DebugMarkerMarkerInfoEXT ->  IO ()
cmdDebugMarkerInsertEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkDebugMarkerSetObjectNameEXT"
debugMarkerSetObjectNameEXT :: Device ->  DebugMarkerObjectNameInfoEXT ->  IO ()
debugMarkerSetObjectNameEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkDebugMarkerSetObjectTagEXT"
debugMarkerSetObjectTagEXT :: Device ->  DebugMarkerObjectTagInfoEXT ->  IO ()
debugMarkerSetObjectTagEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_EXT_DEBUG_MARKER_EXTENSION_NAME"
pattern EXT_DEBUG_MARKER_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_DEBUG_MARKER_EXTENSION_NAME = VK_EXT_DEBUG_MARKER_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_DEBUG_MARKER_SPEC_VERSION"
pattern EXT_DEBUG_MARKER_SPEC_VERSION :: Integral a => a
pattern EXT_DEBUG_MARKER_SPEC_VERSION = VK_EXT_DEBUG_MARKER_SPEC_VERSION
