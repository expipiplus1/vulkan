{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_debug_marker
  ( VkDebugMarkerMarkerInfoEXT(..)
  , VkDebugMarkerObjectNameInfoEXT(..)
  , VkDebugMarkerObjectTagInfoEXT(..)
  , FN_vkCmdDebugMarkerBeginEXT
  , PFN_vkCmdDebugMarkerBeginEXT
  , vkCmdDebugMarkerBeginEXT
  , FN_vkCmdDebugMarkerEndEXT
  , PFN_vkCmdDebugMarkerEndEXT
  , vkCmdDebugMarkerEndEXT
  , FN_vkCmdDebugMarkerInsertEXT
  , PFN_vkCmdDebugMarkerInsertEXT
  , vkCmdDebugMarkerInsertEXT
  , FN_vkDebugMarkerSetObjectNameEXT
  , PFN_vkDebugMarkerSetObjectNameEXT
  , vkDebugMarkerSetObjectNameEXT
  , FN_vkDebugMarkerSetObjectTagEXT
  , PFN_vkDebugMarkerSetObjectTagEXT
  , vkDebugMarkerSetObjectTagEXT
  , pattern VK_EXT_DEBUG_MARKER_EXTENSION_NAME
  , pattern VK_EXT_DEBUG_MARKER_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT
  , VkDebugReportObjectTypeEXT(..)
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


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkCommandBuffer
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_debug_report
  ( VkDebugReportObjectTypeEXT(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkDebugMarkerMarkerInfoEXT"
data VkDebugMarkerMarkerInfoEXT = VkDebugMarkerMarkerInfoEXT
  { -- No documentation found for Nested "VkDebugMarkerMarkerInfoEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDebugMarkerMarkerInfoEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDebugMarkerMarkerInfoEXT" "pMarkerName"
  vkPMarkerName :: Ptr CChar
  , -- No documentation found for Nested "VkDebugMarkerMarkerInfoEXT" "color"
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

instance Zero VkDebugMarkerMarkerInfoEXT where
  zero = VkDebugMarkerMarkerInfoEXT VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT
                                    zero
                                    zero
                                    zero

-- No documentation found for TopLevel "VkDebugMarkerObjectNameInfoEXT"
data VkDebugMarkerObjectNameInfoEXT = VkDebugMarkerObjectNameInfoEXT
  { -- No documentation found for Nested "VkDebugMarkerObjectNameInfoEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDebugMarkerObjectNameInfoEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDebugMarkerObjectNameInfoEXT" "objectType"
  vkObjectType :: VkDebugReportObjectTypeEXT
  , -- No documentation found for Nested "VkDebugMarkerObjectNameInfoEXT" "object"
  vkObject :: Word64
  , -- No documentation found for Nested "VkDebugMarkerObjectNameInfoEXT" "pObjectName"
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

instance Zero VkDebugMarkerObjectNameInfoEXT where
  zero = VkDebugMarkerObjectNameInfoEXT VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT
                                        zero
                                        zero
                                        zero
                                        zero

-- No documentation found for TopLevel "VkDebugMarkerObjectTagInfoEXT"
data VkDebugMarkerObjectTagInfoEXT = VkDebugMarkerObjectTagInfoEXT
  { -- No documentation found for Nested "VkDebugMarkerObjectTagInfoEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDebugMarkerObjectTagInfoEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDebugMarkerObjectTagInfoEXT" "objectType"
  vkObjectType :: VkDebugReportObjectTypeEXT
  , -- No documentation found for Nested "VkDebugMarkerObjectTagInfoEXT" "object"
  vkObject :: Word64
  , -- No documentation found for Nested "VkDebugMarkerObjectTagInfoEXT" "tagName"
  vkTagName :: Word64
  , -- No documentation found for Nested "VkDebugMarkerObjectTagInfoEXT" "tagSize"
  vkTagSize :: CSize
  , -- No documentation found for Nested "VkDebugMarkerObjectTagInfoEXT" "pTag"
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

instance Zero VkDebugMarkerObjectTagInfoEXT where
  zero = VkDebugMarkerObjectTagInfoEXT VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT
                                       zero
                                       zero
                                       zero
                                       zero
                                       zero
                                       zero

-- No documentation found for TopLevel "vkCmdDebugMarkerBeginEXT"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdDebugMarkerBeginEXT" vkCmdDebugMarkerBeginEXT :: ("commandBuffer" ::: VkCommandBuffer) -> ("pMarkerInfo" ::: Ptr VkDebugMarkerMarkerInfoEXT) -> IO ()
#else
vkCmdDebugMarkerBeginEXT :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> ("pMarkerInfo" ::: Ptr VkDebugMarkerMarkerInfoEXT) -> IO ()
vkCmdDebugMarkerBeginEXT deviceCmds = mkVkCmdDebugMarkerBeginEXT (pVkCmdDebugMarkerBeginEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDebugMarkerBeginEXT
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pMarkerInfo" ::: Ptr VkDebugMarkerMarkerInfoEXT) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("pMarkerInfo" ::: Ptr VkDebugMarkerMarkerInfoEXT) -> IO ())
#endif

type FN_vkCmdDebugMarkerBeginEXT = ("commandBuffer" ::: VkCommandBuffer) -> ("pMarkerInfo" ::: Ptr VkDebugMarkerMarkerInfoEXT) -> IO ()
type PFN_vkCmdDebugMarkerBeginEXT = FunPtr FN_vkCmdDebugMarkerBeginEXT

-- No documentation found for TopLevel "vkCmdDebugMarkerEndEXT"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdDebugMarkerEndEXT" vkCmdDebugMarkerEndEXT :: ("commandBuffer" ::: VkCommandBuffer) -> IO ()
#else
vkCmdDebugMarkerEndEXT :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> IO ()
vkCmdDebugMarkerEndEXT deviceCmds = mkVkCmdDebugMarkerEndEXT (pVkCmdDebugMarkerEndEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDebugMarkerEndEXT
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> IO ())
#endif

type FN_vkCmdDebugMarkerEndEXT = ("commandBuffer" ::: VkCommandBuffer) -> IO ()
type PFN_vkCmdDebugMarkerEndEXT = FunPtr FN_vkCmdDebugMarkerEndEXT

-- No documentation found for TopLevel "vkCmdDebugMarkerInsertEXT"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdDebugMarkerInsertEXT" vkCmdDebugMarkerInsertEXT :: ("commandBuffer" ::: VkCommandBuffer) -> ("pMarkerInfo" ::: Ptr VkDebugMarkerMarkerInfoEXT) -> IO ()
#else
vkCmdDebugMarkerInsertEXT :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> ("pMarkerInfo" ::: Ptr VkDebugMarkerMarkerInfoEXT) -> IO ()
vkCmdDebugMarkerInsertEXT deviceCmds = mkVkCmdDebugMarkerInsertEXT (pVkCmdDebugMarkerInsertEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDebugMarkerInsertEXT
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pMarkerInfo" ::: Ptr VkDebugMarkerMarkerInfoEXT) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("pMarkerInfo" ::: Ptr VkDebugMarkerMarkerInfoEXT) -> IO ())
#endif

type FN_vkCmdDebugMarkerInsertEXT = ("commandBuffer" ::: VkCommandBuffer) -> ("pMarkerInfo" ::: Ptr VkDebugMarkerMarkerInfoEXT) -> IO ()
type PFN_vkCmdDebugMarkerInsertEXT = FunPtr FN_vkCmdDebugMarkerInsertEXT

-- No documentation found for TopLevel "vkDebugMarkerSetObjectNameEXT"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDebugMarkerSetObjectNameEXT" vkDebugMarkerSetObjectNameEXT :: ("device" ::: VkDevice) -> ("pNameInfo" ::: Ptr VkDebugMarkerObjectNameInfoEXT) -> IO VkResult
#else
vkDebugMarkerSetObjectNameEXT :: DeviceCmds -> ("device" ::: VkDevice) -> ("pNameInfo" ::: Ptr VkDebugMarkerObjectNameInfoEXT) -> IO VkResult
vkDebugMarkerSetObjectNameEXT deviceCmds = mkVkDebugMarkerSetObjectNameEXT (pVkDebugMarkerSetObjectNameEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDebugMarkerSetObjectNameEXT
  :: FunPtr (("device" ::: VkDevice) -> ("pNameInfo" ::: Ptr VkDebugMarkerObjectNameInfoEXT) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pNameInfo" ::: Ptr VkDebugMarkerObjectNameInfoEXT) -> IO VkResult)
#endif

type FN_vkDebugMarkerSetObjectNameEXT = ("device" ::: VkDevice) -> ("pNameInfo" ::: Ptr VkDebugMarkerObjectNameInfoEXT) -> IO VkResult
type PFN_vkDebugMarkerSetObjectNameEXT = FunPtr FN_vkDebugMarkerSetObjectNameEXT

-- No documentation found for TopLevel "vkDebugMarkerSetObjectTagEXT"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDebugMarkerSetObjectTagEXT" vkDebugMarkerSetObjectTagEXT :: ("device" ::: VkDevice) -> ("pTagInfo" ::: Ptr VkDebugMarkerObjectTagInfoEXT) -> IO VkResult
#else
vkDebugMarkerSetObjectTagEXT :: DeviceCmds -> ("device" ::: VkDevice) -> ("pTagInfo" ::: Ptr VkDebugMarkerObjectTagInfoEXT) -> IO VkResult
vkDebugMarkerSetObjectTagEXT deviceCmds = mkVkDebugMarkerSetObjectTagEXT (pVkDebugMarkerSetObjectTagEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDebugMarkerSetObjectTagEXT
  :: FunPtr (("device" ::: VkDevice) -> ("pTagInfo" ::: Ptr VkDebugMarkerObjectTagInfoEXT) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pTagInfo" ::: Ptr VkDebugMarkerObjectTagInfoEXT) -> IO VkResult)
#endif

type FN_vkDebugMarkerSetObjectTagEXT = ("device" ::: VkDevice) -> ("pTagInfo" ::: Ptr VkDebugMarkerObjectTagInfoEXT) -> IO VkResult
type PFN_vkDebugMarkerSetObjectTagEXT = FunPtr FN_vkDebugMarkerSetObjectTagEXT

-- No documentation found for TopLevel "VK_EXT_DEBUG_MARKER_EXTENSION_NAME"
pattern VK_EXT_DEBUG_MARKER_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_EXT_DEBUG_MARKER_EXTENSION_NAME = "VK_EXT_debug_marker"

-- No documentation found for TopLevel "VK_EXT_DEBUG_MARKER_SPEC_VERSION"
pattern VK_EXT_DEBUG_MARKER_SPEC_VERSION :: Integral a => a
pattern VK_EXT_DEBUG_MARKER_SPEC_VERSION = 4

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT"
pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT = VkStructureType 1000022002

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT"
pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT = VkStructureType 1000022000

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT"
pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT = VkStructureType 1000022001
