{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Image
  ( withCStructImageCreateInfo
  , fromCStructImageCreateInfo
  , ImageCreateInfo(..)
  , ImageLayout
  , withCStructSubresourceLayout
  , fromCStructSubresourceLayout
  , SubresourceLayout(..)
  , createImage
  , destroyImage
  , getImageSubresourceLayout
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( (<=<)
  , when
  )
import Data.Function
  ( (&)
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( generateM
  , length
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( createImage
  , destroyImage
  , getImageSubresourceLayout
  )


import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core10.Image
  ( VkImageCreateInfo(..)
  , VkImageLayout(..)
  , VkSubresourceLayout(..)
  )
import Graphics.Vulkan.Core10.Buffer
  ( SharingMode
  )
import Graphics.Vulkan.Core10.Core
  ( Format
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , Extent3D(..)
  , DeviceSize
  , ImageCreateFlags
  , ImageTiling
  , ImageType
  , ImageUsageFlags
  , SampleCountFlagBits
  , fromCStructExtent3D
  , withCStructAllocationCallbacks
  , withCStructExtent3D
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( Image
  )
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( ImageSubresource(..)
  , withCStructImageSubresource
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )


-- No documentation found for TopLevel "ImageCreateInfo"
data ImageCreateInfo = ImageCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "ImageCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageCreateInfo" "flags"
  vkFlags :: ImageCreateFlags
  , -- No documentation found for Nested "ImageCreateInfo" "imageType"
  vkImageType :: ImageType
  , -- No documentation found for Nested "ImageCreateInfo" "format"
  vkFormat :: Format
  , -- No documentation found for Nested "ImageCreateInfo" "extent"
  vkExtent :: Extent3D
  , -- No documentation found for Nested "ImageCreateInfo" "mipLevels"
  vkMipLevels :: Word32
  , -- No documentation found for Nested "ImageCreateInfo" "arrayLayers"
  vkArrayLayers :: Word32
  , -- No documentation found for Nested "ImageCreateInfo" "samples"
  vkSamples :: SampleCountFlagBits
  , -- No documentation found for Nested "ImageCreateInfo" "tiling"
  vkTiling :: ImageTiling
  , -- No documentation found for Nested "ImageCreateInfo" "usage"
  vkUsage :: ImageUsageFlags
  , -- No documentation found for Nested "ImageCreateInfo" "sharingMode"
  vkSharingMode :: SharingMode
  -- Length valued member elided
  , -- No documentation found for Nested "ImageCreateInfo" "pQueueFamilyIndices"
  vkPQueueFamilyIndices :: Vector Word32
  , -- No documentation found for Nested "ImageCreateInfo" "initialLayout"
  vkInitialLayout :: ImageLayout
  }
  deriving (Show, Eq)
withCStructImageCreateInfo :: ImageCreateInfo -> (VkImageCreateInfo -> IO a) -> IO a
withCStructImageCreateInfo from cont = withVec (&) (vkPQueueFamilyIndices (from :: ImageCreateInfo)) (\pQueueFamilyIndices -> withCStructExtent3D (vkExtent (from :: ImageCreateInfo)) (\extent -> maybeWith withSomeVkStruct (vkPNext (from :: ImageCreateInfo)) (\pPNext -> cont (VkImageCreateInfo VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO pPNext (vkFlags (from :: ImageCreateInfo)) (vkImageType (from :: ImageCreateInfo)) (vkFormat (from :: ImageCreateInfo)) extent (vkMipLevels (from :: ImageCreateInfo)) (vkArrayLayers (from :: ImageCreateInfo)) (vkSamples (from :: ImageCreateInfo)) (vkTiling (from :: ImageCreateInfo)) (vkUsage (from :: ImageCreateInfo)) (vkSharingMode (from :: ImageCreateInfo)) (fromIntegral (Data.Vector.length (vkPQueueFamilyIndices (from :: ImageCreateInfo)))) pQueueFamilyIndices (vkInitialLayout (from :: ImageCreateInfo))))))
fromCStructImageCreateInfo :: VkImageCreateInfo -> IO ImageCreateInfo
fromCStructImageCreateInfo c = ImageCreateInfo <$> -- Univalued Member elided
                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImageCreateInfo)))
                                               <*> pure (vkFlags (c :: VkImageCreateInfo))
                                               <*> pure (vkImageType (c :: VkImageCreateInfo))
                                               <*> pure (vkFormat (c :: VkImageCreateInfo))
                                               <*> (fromCStructExtent3D (vkExtent (c :: VkImageCreateInfo)))
                                               <*> pure (vkMipLevels (c :: VkImageCreateInfo))
                                               <*> pure (vkArrayLayers (c :: VkImageCreateInfo))
                                               <*> pure (vkSamples (c :: VkImageCreateInfo))
                                               <*> pure (vkTiling (c :: VkImageCreateInfo))
                                               <*> pure (vkUsage (c :: VkImageCreateInfo))
                                               <*> pure (vkSharingMode (c :: VkImageCreateInfo))
                                               -- Length valued member elided
                                               <*> (Data.Vector.generateM (fromIntegral (vkQueueFamilyIndexCount (c :: VkImageCreateInfo))) (peekElemOff (vkPQueueFamilyIndices (c :: VkImageCreateInfo))))
                                               <*> pure (vkInitialLayout (c :: VkImageCreateInfo))
-- No documentation found for TopLevel "ImageLayout"
type ImageLayout = VkImageLayout
-- No documentation found for TopLevel "SubresourceLayout"
data SubresourceLayout = SubresourceLayout
  { -- No documentation found for Nested "SubresourceLayout" "offset"
  vkOffset :: DeviceSize
  , -- No documentation found for Nested "SubresourceLayout" "size"
  vkSize :: DeviceSize
  , -- No documentation found for Nested "SubresourceLayout" "rowPitch"
  vkRowPitch :: DeviceSize
  , -- No documentation found for Nested "SubresourceLayout" "arrayPitch"
  vkArrayPitch :: DeviceSize
  , -- No documentation found for Nested "SubresourceLayout" "depthPitch"
  vkDepthPitch :: DeviceSize
  }
  deriving (Show, Eq)
withCStructSubresourceLayout :: SubresourceLayout -> (VkSubresourceLayout -> IO a) -> IO a
withCStructSubresourceLayout from cont = cont (VkSubresourceLayout (vkOffset (from :: SubresourceLayout)) (vkSize (from :: SubresourceLayout)) (vkRowPitch (from :: SubresourceLayout)) (vkArrayPitch (from :: SubresourceLayout)) (vkDepthPitch (from :: SubresourceLayout)))
fromCStructSubresourceLayout :: VkSubresourceLayout -> IO SubresourceLayout
fromCStructSubresourceLayout c = SubresourceLayout <$> pure (vkOffset (c :: VkSubresourceLayout))
                                                   <*> pure (vkSize (c :: VkSubresourceLayout))
                                                   <*> pure (vkRowPitch (c :: VkSubresourceLayout))
                                                   <*> pure (vkArrayPitch (c :: VkSubresourceLayout))
                                                   <*> pure (vkDepthPitch (c :: VkSubresourceLayout))

-- | Wrapper for 'vkCreateImage'
createImage :: Device ->  ImageCreateInfo ->  Maybe AllocationCallbacks ->  IO (Image)
createImage = \(Device device commandTable) -> \createInfo -> \allocator -> alloca (\pImage -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructImageCreateInfo a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createImage commandTable device pCreateInfo pAllocator pImage >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pImage)))))

-- | Wrapper for 'vkDestroyImage'
destroyImage :: Device ->  Image ->  Maybe AllocationCallbacks ->  IO ()
destroyImage = \(Device device commandTable) -> \image -> \allocator -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> Graphics.Vulkan.C.Dynamic.destroyImage commandTable device image pAllocator *> (pure ()))

-- | Wrapper for 'vkGetImageSubresourceLayout'
getImageSubresourceLayout :: Device ->  Image ->  ImageSubresource ->  IO (SubresourceLayout)
getImageSubresourceLayout = \(Device device commandTable) -> \image -> \subresource -> alloca (\pLayout -> (\a -> withCStructImageSubresource a . flip with) subresource (\pSubresource -> Graphics.Vulkan.C.Dynamic.getImageSubresourceLayout commandTable device image pSubresource pLayout *> ((fromCStructSubresourceLayout <=< peek) pLayout)))
