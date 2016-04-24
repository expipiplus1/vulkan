{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}
module Graphics.Vulkan.LayerDiscovery where

import Data.Vector.Storable.Sized( Vector(..)
                                 )
import Graphics.Vulkan.Device( PhysicalDevice(..)
                             )
import Data.Word( Word32(..)
                )
import Foreign.Ptr( Ptr(..)
                  , plusPtr
                  )
import Foreign.Storable( Storable(..)
                       )
import Graphics.Vulkan.Constants( VK_MAX_DESCRIPTION_SIZE
                                , VK_MAX_EXTENSION_NAME_SIZE
                                )
import Graphics.Vulkan.Core( Result(..)
                           )
import Foreign.C.Types( CChar(..)
                      )


data LayerProperties =
  LayerProperties{ layerName :: Vector VK_MAX_EXTENSION_NAME_SIZE CChar 
                 , specVersion :: Word32 
                 , implementationVersion :: Word32 
                 , description :: Vector VK_MAX_DESCRIPTION_SIZE CChar 
                 }
  deriving (Eq)

instance Storable LayerProperties where
  sizeOf ~_ = 520
  alignment ~_ = 4
  peek ptr = LayerProperties <$> peek (ptr `plusPtr` 0)
                             <*> peek (ptr `plusPtr` 256)
                             <*> peek (ptr `plusPtr` 260)
                             <*> peek (ptr `plusPtr` 264)
  poke ptr poked = poke (ptr `plusPtr` 0) (layerName (poked :: LayerProperties))
                *> poke (ptr `plusPtr` 256) (specVersion (poked :: LayerProperties))
                *> poke (ptr `plusPtr` 260) (implementationVersion (poked :: LayerProperties))
                *> poke (ptr `plusPtr` 264) (description (poked :: LayerProperties))


-- ** vkEnumerateInstanceLayerProperties
foreign import ccall "vkEnumerateInstanceLayerProperties" vkEnumerateInstanceLayerProperties ::
  Ptr Word32 -> Ptr LayerProperties -> IO Result

-- ** vkEnumerateDeviceLayerProperties
foreign import ccall "vkEnumerateDeviceLayerProperties" vkEnumerateDeviceLayerProperties ::
  PhysicalDevice -> Ptr Word32 -> Ptr LayerProperties -> IO Result

