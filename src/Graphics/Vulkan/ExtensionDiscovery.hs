{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}
module Graphics.Vulkan.ExtensionDiscovery where

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
import Graphics.Vulkan.Constants( MaxExtensionNameSize
                                )
import Graphics.Vulkan.Core( Result(..)
                           )
import Foreign.C.Types( CChar(..)
                      )


data ExtensionProperties =
  ExtensionProperties{ extensionName :: Vector MaxExtensionNameSize CChar 
                     , specVersion :: Word32 
                     }
  deriving (Eq, Ord)

instance Storable ExtensionProperties where
  sizeOf ~_ = 260
  alignment ~_ = 4
  peek ptr = ExtensionProperties <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 256)
  poke ptr poked = poke (ptr `plusPtr` 0) (extensionName (poked :: ExtensionProperties))
                *> poke (ptr `plusPtr` 256) (specVersion (poked :: ExtensionProperties))


-- ** enumerateInstanceExtensionProperties
foreign import ccall "vkEnumerateInstanceExtensionProperties" enumerateInstanceExtensionProperties ::
  Ptr CChar -> Ptr Word32 -> Ptr ExtensionProperties -> IO Result

-- ** enumerateDeviceExtensionProperties
foreign import ccall "vkEnumerateDeviceExtensionProperties" enumerateDeviceExtensionProperties ::
  PhysicalDevice ->
  Ptr CChar -> Ptr Word32 -> Ptr ExtensionProperties -> IO Result

