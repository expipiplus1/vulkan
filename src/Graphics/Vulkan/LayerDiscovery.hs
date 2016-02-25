{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}
module Graphics.Vulkan.LayerDiscovery where
import Graphics.Vulkan.Device( VkPhysicalDevice(..)
                             )
import Data.Word( Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Data.Vector.Fixed.Cont( ToPeano
                             )
import Foreign.Storable( Storable(..)
                       )
import Graphics.Vulkan.Constants( VK_MAX_EXTENSION_NAME_SIZE
                                , VK_MAX_DESCRIPTION_SIZE
                                )
import Data.Vector.Fixed.Storable( Vec
                                 )
import Graphics.Vulkan.Core( VkResult(..)
                           )
import Foreign.C.Types( CChar
                      )

data VkLayerProperties =
  VkLayerProperties{ vkLayerName :: Vec (ToPeano VK_MAX_EXTENSION_NAME_SIZE) CChar 
                   , vkSpecVersion :: Word32 
                   , vkImplementationVersion :: Word32 
                   , vkDescription :: Vec (ToPeano VK_MAX_DESCRIPTION_SIZE) CChar 
                   }
  deriving (Eq)

instance Storable VkLayerProperties where
  sizeOf ~_ = 520
  alignment ~_ = 4
  peek ptr = VkLayerProperties <$> peek (ptr `plusPtr` 0)
                               <*> peek (ptr `plusPtr` 256)
                               <*> peek (ptr `plusPtr` 260)
                               <*> peek (ptr `plusPtr` 264)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkLayerName (poked :: VkLayerProperties))
                *> poke (ptr `plusPtr` 256) (vkSpecVersion (poked :: VkLayerProperties))
                *> poke (ptr `plusPtr` 260) (vkImplementationVersion (poked :: VkLayerProperties))
                *> poke (ptr `plusPtr` 264) (vkDescription (poked :: VkLayerProperties))


-- ** vkEnumerateInstanceLayerProperties
foreign import ccall "vkEnumerateInstanceLayerProperties" vkEnumerateInstanceLayerProperties :: 
  Ptr Word32 -> Ptr VkLayerProperties -> IO VkResult

-- ** vkEnumerateDeviceLayerProperties
foreign import ccall "vkEnumerateDeviceLayerProperties" vkEnumerateDeviceLayerProperties :: 
  VkPhysicalDevice ->
  Ptr Word32 -> Ptr VkLayerProperties -> IO VkResult

