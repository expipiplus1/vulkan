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


-- | Nothing
pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT = VkStructureType 1000022000
-- | Nothing
pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT = VkStructureType 1000022001
-- | Nothing
pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT = VkStructureType 1000022002
pattern VK_EXT_DEBUG_MARKER_SPEC_VERSION :: Integral a => a
pattern VK_EXT_DEBUG_MARKER_SPEC_VERSION = 4
pattern VK_EXT_DEBUG_MARKER_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_DEBUG_MARKER_EXTENSION_NAME = "VK_EXT_debug_marker"
-- | 
foreign import ccall "vkDebugMarkerSetObjectNameEXT" vkDebugMarkerSetObjectNameEXT :: ("device" ::: VkDevice) -> ("pNameInfo" ::: Ptr VkDebugMarkerObjectNameInfoEXT) -> IO VkResult
-- | 
foreign import ccall "vkDebugMarkerSetObjectTagEXT" vkDebugMarkerSetObjectTagEXT :: ("device" ::: VkDevice) -> ("pTagInfo" ::: Ptr VkDebugMarkerObjectTagInfoEXT) -> IO VkResult
-- | 
foreign import ccall "vkCmdDebugMarkerBeginEXT" vkCmdDebugMarkerBeginEXT :: ("commandBuffer" ::: VkCommandBuffer) -> ("pMarkerInfo" ::: Ptr VkDebugMarkerMarkerInfoEXT) -> IO ()
-- | 
foreign import ccall "vkCmdDebugMarkerEndEXT" vkCmdDebugMarkerEndEXT :: ("commandBuffer" ::: VkCommandBuffer) -> IO ()
-- | 
foreign import ccall "vkCmdDebugMarkerInsertEXT" vkCmdDebugMarkerInsertEXT :: ("commandBuffer" ::: VkCommandBuffer) -> ("pMarkerInfo" ::: Ptr VkDebugMarkerMarkerInfoEXT) -> IO ()
-- | TODO: Struct comments
data VkDebugMarkerObjectNameInfoEXT = VkDebugMarkerObjectNameInfoEXT
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkObjectType :: VkDebugReportObjectTypeEXT
  , vkObject :: Word64
  , vkPObjectName :: Ptr CChar
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
-- | TODO: Struct comments
data VkDebugMarkerObjectTagInfoEXT = VkDebugMarkerObjectTagInfoEXT
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkObjectType :: VkDebugReportObjectTypeEXT
  , vkObject :: Word64
  , vkTagName :: Word64
  , vkTagSize :: CSize
  , vkPTag :: Ptr ()
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
-- | TODO: Struct comments
data VkDebugMarkerMarkerInfoEXT = VkDebugMarkerMarkerInfoEXT
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkPMarkerName :: Ptr CChar
  , vkColor :: Vector 4 CFloat
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
