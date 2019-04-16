{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Core10.ImageView
  ( withCStructComponentMapping
  , fromCStructComponentMapping
  , ComponentMapping(..)
  , ComponentSwizzle
  , withCStructImageSubresourceRange
  , fromCStructImageSubresourceRange
  , ImageSubresourceRange(..)
  , ImageView
  , ImageViewCreateFlagBits
  , ImageViewCreateFlags
  , withCStructImageViewCreateInfo
  , fromCStructImageViewCreateInfo
  , ImageViewCreateInfo(..)
  , ImageViewType
  , createImageView
  , destroyImageView
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( when
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
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( createImageView
  , destroyImageView
  )


import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core10.ImageView
  ( VkComponentMapping(..)
  , VkComponentSwizzle(..)
  , VkImageSubresourceRange(..)
  , VkImageViewCreateFlagBits(..)
  , VkImageViewCreateInfo(..)
  , VkImageViewType(..)
  , VkImageView
  )
import Graphics.Vulkan.Core10.Core
  ( Format
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( Image
  )
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( ImageAspectFlags
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )


-- No documentation found for TopLevel "ComponentMapping"
data ComponentMapping = ComponentMapping
  { -- No documentation found for Nested "ComponentMapping" "r"
  vkR :: ComponentSwizzle
  , -- No documentation found for Nested "ComponentMapping" "g"
  vkG :: ComponentSwizzle
  , -- No documentation found for Nested "ComponentMapping" "b"
  vkB :: ComponentSwizzle
  , -- No documentation found for Nested "ComponentMapping" "a"
  vkA :: ComponentSwizzle
  }
  deriving (Show, Eq)
withCStructComponentMapping :: ComponentMapping -> (VkComponentMapping -> IO a) -> IO a
withCStructComponentMapping from cont = cont (VkComponentMapping (vkR (from :: ComponentMapping)) (vkG (from :: ComponentMapping)) (vkB (from :: ComponentMapping)) (vkA (from :: ComponentMapping)))
fromCStructComponentMapping :: VkComponentMapping -> IO ComponentMapping
fromCStructComponentMapping c = ComponentMapping <$> pure (vkR (c :: VkComponentMapping))
                                                 <*> pure (vkG (c :: VkComponentMapping))
                                                 <*> pure (vkB (c :: VkComponentMapping))
                                                 <*> pure (vkA (c :: VkComponentMapping))
-- No documentation found for TopLevel "ComponentSwizzle"
type ComponentSwizzle = VkComponentSwizzle
-- No documentation found for TopLevel "ImageSubresourceRange"
data ImageSubresourceRange = ImageSubresourceRange
  { -- No documentation found for Nested "ImageSubresourceRange" "aspectMask"
  vkAspectMask :: ImageAspectFlags
  , -- No documentation found for Nested "ImageSubresourceRange" "baseMipLevel"
  vkBaseMipLevel :: Word32
  , -- No documentation found for Nested "ImageSubresourceRange" "levelCount"
  vkLevelCount :: Word32
  , -- No documentation found for Nested "ImageSubresourceRange" "baseArrayLayer"
  vkBaseArrayLayer :: Word32
  , -- No documentation found for Nested "ImageSubresourceRange" "layerCount"
  vkLayerCount :: Word32
  }
  deriving (Show, Eq)
withCStructImageSubresourceRange :: ImageSubresourceRange -> (VkImageSubresourceRange -> IO a) -> IO a
withCStructImageSubresourceRange from cont = cont (VkImageSubresourceRange (vkAspectMask (from :: ImageSubresourceRange)) (vkBaseMipLevel (from :: ImageSubresourceRange)) (vkLevelCount (from :: ImageSubresourceRange)) (vkBaseArrayLayer (from :: ImageSubresourceRange)) (vkLayerCount (from :: ImageSubresourceRange)))
fromCStructImageSubresourceRange :: VkImageSubresourceRange -> IO ImageSubresourceRange
fromCStructImageSubresourceRange c = ImageSubresourceRange <$> pure (vkAspectMask (c :: VkImageSubresourceRange))
                                                           <*> pure (vkBaseMipLevel (c :: VkImageSubresourceRange))
                                                           <*> pure (vkLevelCount (c :: VkImageSubresourceRange))
                                                           <*> pure (vkBaseArrayLayer (c :: VkImageSubresourceRange))
                                                           <*> pure (vkLayerCount (c :: VkImageSubresourceRange))
-- No documentation found for TopLevel "ImageView"
type ImageView = VkImageView
-- No documentation found for TopLevel "ImageViewCreateFlagBits"
type ImageViewCreateFlagBits = VkImageViewCreateFlagBits
-- No documentation found for TopLevel "ImageViewCreateFlags"
type ImageViewCreateFlags = ImageViewCreateFlagBits
-- No documentation found for TopLevel "ImageViewCreateInfo"
data ImageViewCreateInfo = ImageViewCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "ImageViewCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageViewCreateInfo" "flags"
  vkFlags :: ImageViewCreateFlags
  , -- No documentation found for Nested "ImageViewCreateInfo" "image"
  vkImage :: Image
  , -- No documentation found for Nested "ImageViewCreateInfo" "viewType"
  vkViewType :: ImageViewType
  , -- No documentation found for Nested "ImageViewCreateInfo" "format"
  vkFormat :: Format
  , -- No documentation found for Nested "ImageViewCreateInfo" "components"
  vkComponents :: ComponentMapping
  , -- No documentation found for Nested "ImageViewCreateInfo" "subresourceRange"
  vkSubresourceRange :: ImageSubresourceRange
  }
  deriving (Show, Eq)
withCStructImageViewCreateInfo :: ImageViewCreateInfo -> (VkImageViewCreateInfo -> IO a) -> IO a
withCStructImageViewCreateInfo from cont = withCStructImageSubresourceRange (vkSubresourceRange (from :: ImageViewCreateInfo)) (\subresourceRange -> withCStructComponentMapping (vkComponents (from :: ImageViewCreateInfo)) (\components -> maybeWith withSomeVkStruct (vkPNext (from :: ImageViewCreateInfo)) (\pPNext -> cont (VkImageViewCreateInfo VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO pPNext (vkFlags (from :: ImageViewCreateInfo)) (vkImage (from :: ImageViewCreateInfo)) (vkViewType (from :: ImageViewCreateInfo)) (vkFormat (from :: ImageViewCreateInfo)) components subresourceRange))))
fromCStructImageViewCreateInfo :: VkImageViewCreateInfo -> IO ImageViewCreateInfo
fromCStructImageViewCreateInfo c = ImageViewCreateInfo <$> -- Univalued Member elided
                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImageViewCreateInfo)))
                                                       <*> pure (vkFlags (c :: VkImageViewCreateInfo))
                                                       <*> pure (vkImage (c :: VkImageViewCreateInfo))
                                                       <*> pure (vkViewType (c :: VkImageViewCreateInfo))
                                                       <*> pure (vkFormat (c :: VkImageViewCreateInfo))
                                                       <*> (fromCStructComponentMapping (vkComponents (c :: VkImageViewCreateInfo)))
                                                       <*> (fromCStructImageSubresourceRange (vkSubresourceRange (c :: VkImageViewCreateInfo)))
-- No documentation found for TopLevel "ImageViewType"
type ImageViewType = VkImageViewType

-- | Wrapper for vkCreateImageView
createImageView :: Device ->  ImageViewCreateInfo ->  Maybe AllocationCallbacks ->  IO (ImageView)
createImageView = \(Device device commandTable) -> \createInfo -> \allocator -> alloca (\pView -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructImageViewCreateInfo a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createImageView commandTable device pCreateInfo pAllocator pView >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pView)))))

-- | Wrapper for vkDestroyImageView
destroyImageView :: Device ->  ImageView ->  Maybe AllocationCallbacks ->  IO ()
destroyImageView = \(Device device commandTable) -> \imageView -> \allocator -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> Graphics.Vulkan.C.Dynamic.destroyImageView commandTable device imageView pAllocator *> (pure ()))
