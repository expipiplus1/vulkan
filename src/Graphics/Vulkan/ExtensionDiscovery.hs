{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}
module Graphics.Vulkan.ExtensionDiscovery where
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
                                )
import Data.Vector.Fixed.Storable( Vec
                                 )
import Graphics.Vulkan.Core( VkResult(..)
                           )
import Foreign.C.Types( CChar
                      )

data VkExtensionProperties =
  VkExtensionProperties{ vkExtensionName :: Vec (ToPeano VK_MAX_EXTENSION_NAME_SIZE) CChar 
                       , vkSpecVersion :: Word32 
                       }
  deriving (Eq)

instance Storable VkExtensionProperties where
  sizeOf ~_ = 260
  alignment ~_ = 4
  peek ptr = VkExtensionProperties <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 256)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkExtensionName (poked :: VkExtensionProperties))
                *> poke (ptr `plusPtr` 256) (vkSpecVersion (poked :: VkExtensionProperties))


-- ** vkEnumerateInstanceExtensionProperties
foreign import ccall "vkEnumerateInstanceExtensionProperties" vkEnumerateInstanceExtensionProperties :: 
  Ptr CChar -> Ptr Word32 -> Ptr VkExtensionProperties -> IO VkResult

-- ** vkEnumerateDeviceExtensionProperties
foreign import ccall "vkEnumerateDeviceExtensionProperties" vkEnumerateDeviceExtensionProperties :: 
  VkPhysicalDevice ->
  Ptr CChar -> Ptr Word32 -> Ptr VkExtensionProperties -> IO VkResult

