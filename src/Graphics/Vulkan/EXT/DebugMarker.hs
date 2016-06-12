{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}
module Graphics.Vulkan.EXT.DebugMarker where

import Data.Vector.Storable.Sized( Vector
                                 )
import Graphics.Vulkan.Device( VkDevice(..)
                             )
import Data.Word( Word64
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Graphics.Vulkan.CommandBuffer( VkCommandBuffer(..)
                                    )
import Graphics.Vulkan.EXT.DebugReport( VkDebugReportObjectTypeEXT(..)
                                      )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )
import Graphics.Vulkan.Core( VkResult(..)
                           , VkStructureType(..)
                           )
import Foreign.C.Types( CSize
                      , CFloat
                      , CFloat(..)
                      , CChar
                      , CSize(..)
                      )


data VkDebugMarkerObjectNameInfoEXT =
  VkDebugMarkerObjectNameInfoEXT{ vkSType :: VkStructureType 
                                , vkPNext :: Ptr Void 
                                , vkObjectType :: VkDebugReportObjectTypeEXT 
                                , vkObject :: Word64 
                                , vkPObjectName :: Ptr CChar 
                                }
  deriving (Eq, Ord, Show)

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



data VkDebugMarkerMarkerInfoEXT =
  VkDebugMarkerMarkerInfoEXT{ vkSType :: VkStructureType 
                            , vkPNext :: Ptr Void 
                            , vkPMarkerName :: Ptr CChar 
                            , vkColor :: Vector 4 CFloat 
                            }
  deriving (Eq, Ord, Show)

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


-- ** vkCmdDebugMarkerInsertEXT
foreign import ccall "vkCmdDebugMarkerInsertEXT" vkCmdDebugMarkerInsertEXT ::
  VkCommandBuffer -> Ptr VkDebugMarkerMarkerInfoEXT -> IO ()

-- ** vkCmdDebugMarkerBeginEXT
foreign import ccall "vkCmdDebugMarkerBeginEXT" vkCmdDebugMarkerBeginEXT ::
  VkCommandBuffer -> Ptr VkDebugMarkerMarkerInfoEXT -> IO ()

-- ** vkDebugMarkerSetObjectTagEXT
foreign import ccall "vkDebugMarkerSetObjectTagEXT" vkDebugMarkerSetObjectTagEXT ::
  VkDevice -> Ptr VkDebugMarkerObjectTagInfoEXT -> IO VkResult

-- ** vkCmdDebugMarkerEndEXT
foreign import ccall "vkCmdDebugMarkerEndEXT" vkCmdDebugMarkerEndEXT ::
  VkCommandBuffer -> IO ()


data VkDebugMarkerObjectTagInfoEXT =
  VkDebugMarkerObjectTagInfoEXT{ vkSType :: VkStructureType 
                               , vkPNext :: Ptr Void 
                               , vkObjectType :: VkDebugReportObjectTypeEXT 
                               , vkObject :: Word64 
                               , vkTagName :: Word64 
                               , vkTagSize :: CSize 
                               , vkPTag :: Ptr Void 
                               }
  deriving (Eq, Ord, Show)

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


-- ** vkDebugMarkerSetObjectNameEXT
foreign import ccall "vkDebugMarkerSetObjectNameEXT" vkDebugMarkerSetObjectNameEXT ::
  VkDevice -> Ptr VkDebugMarkerObjectNameInfoEXT -> IO VkResult

