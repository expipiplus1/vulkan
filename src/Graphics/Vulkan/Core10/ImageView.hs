{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Core10.ImageView
  ( ComponentMapping(..)
  , ComponentSwizzle
  , pattern COMPONENT_SWIZZLE_IDENTITY
  , pattern COMPONENT_SWIZZLE_ZERO
  , pattern COMPONENT_SWIZZLE_ONE
  , pattern COMPONENT_SWIZZLE_R
  , pattern COMPONENT_SWIZZLE_G
  , pattern COMPONENT_SWIZZLE_B
  , pattern COMPONENT_SWIZZLE_A
  , ImageSubresourceRange(..)
  , ImageView
  , ImageViewCreateFlagBits
  , pattern IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT
  , ImageViewCreateFlags
#if defined(VK_USE_PLATFORM_GGP)
  , ImageViewCreateInfo(..)
#endif
  , ImageViewType
  , pattern IMAGE_VIEW_TYPE_1D
  , pattern IMAGE_VIEW_TYPE_2D
  , pattern IMAGE_VIEW_TYPE_3D
  , pattern IMAGE_VIEW_TYPE_CUBE
  , pattern IMAGE_VIEW_TYPE_1D_ARRAY
  , pattern IMAGE_VIEW_TYPE_2D_ARRAY
  , pattern IMAGE_VIEW_TYPE_CUBE_ARRAY
  , createImageView
  , destroyImageView
  , withImageView
  ) where

import Control.Exception
  ( bracket
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybeWith
  , with
  )
import Foreign.Storable
  ( peek
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Core10.ImageView
  ( VkComponentSwizzle(..)
  , VkImageViewCreateFlagBits(..)
  , VkImageViewType(..)
  , VkImageView
  , vkCreateImageView
  , vkDestroyImageView
  , pattern VK_COMPONENT_SWIZZLE_A
  , pattern VK_COMPONENT_SWIZZLE_B
  , pattern VK_COMPONENT_SWIZZLE_G
  , pattern VK_COMPONENT_SWIZZLE_IDENTITY
  , pattern VK_COMPONENT_SWIZZLE_ONE
  , pattern VK_COMPONENT_SWIZZLE_R
  , pattern VK_COMPONENT_SWIZZLE_ZERO
  , pattern VK_IMAGE_VIEW_TYPE_1D
  , pattern VK_IMAGE_VIEW_TYPE_1D_ARRAY
  , pattern VK_IMAGE_VIEW_TYPE_2D
  , pattern VK_IMAGE_VIEW_TYPE_2D_ARRAY
  , pattern VK_IMAGE_VIEW_TYPE_3D
  , pattern VK_IMAGE_VIEW_TYPE_CUBE
  , pattern VK_IMAGE_VIEW_TYPE_CUBE_ARRAY
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map
  ( pattern VK_IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Core
  ( Format
  )
#endif
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.MemoryManagement
  ( Image
  )
#endif
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( ImageAspectFlags
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif



-- No documentation found for TopLevel "VkComponentMapping"
data ComponentMapping = ComponentMapping
  { -- No documentation found for Nested "ComponentMapping" "r"
  r :: ComponentSwizzle
  , -- No documentation found for Nested "ComponentMapping" "g"
  g :: ComponentSwizzle
  , -- No documentation found for Nested "ComponentMapping" "b"
  b :: ComponentSwizzle
  , -- No documentation found for Nested "ComponentMapping" "a"
  a :: ComponentSwizzle
  }
  deriving (Show, Eq)

instance Zero ComponentMapping where
  zero = ComponentMapping zero
                          zero
                          zero
                          zero


-- No documentation found for TopLevel "ComponentSwizzle"
type ComponentSwizzle = VkComponentSwizzle


{-# complete COMPONENT_SWIZZLE_IDENTITY, COMPONENT_SWIZZLE_ZERO, COMPONENT_SWIZZLE_ONE, COMPONENT_SWIZZLE_R, COMPONENT_SWIZZLE_G, COMPONENT_SWIZZLE_B, COMPONENT_SWIZZLE_A :: ComponentSwizzle #-}


-- No documentation found for Nested "ComponentSwizzle" "COMPONENT_SWIZZLE_IDENTITY"
pattern COMPONENT_SWIZZLE_IDENTITY :: (a ~ ComponentSwizzle) => a
pattern COMPONENT_SWIZZLE_IDENTITY = VK_COMPONENT_SWIZZLE_IDENTITY


-- No documentation found for Nested "ComponentSwizzle" "COMPONENT_SWIZZLE_ZERO"
pattern COMPONENT_SWIZZLE_ZERO :: (a ~ ComponentSwizzle) => a
pattern COMPONENT_SWIZZLE_ZERO = VK_COMPONENT_SWIZZLE_ZERO


-- No documentation found for Nested "ComponentSwizzle" "COMPONENT_SWIZZLE_ONE"
pattern COMPONENT_SWIZZLE_ONE :: (a ~ ComponentSwizzle) => a
pattern COMPONENT_SWIZZLE_ONE = VK_COMPONENT_SWIZZLE_ONE


-- No documentation found for Nested "ComponentSwizzle" "COMPONENT_SWIZZLE_R"
pattern COMPONENT_SWIZZLE_R :: (a ~ ComponentSwizzle) => a
pattern COMPONENT_SWIZZLE_R = VK_COMPONENT_SWIZZLE_R


-- No documentation found for Nested "ComponentSwizzle" "COMPONENT_SWIZZLE_G"
pattern COMPONENT_SWIZZLE_G :: (a ~ ComponentSwizzle) => a
pattern COMPONENT_SWIZZLE_G = VK_COMPONENT_SWIZZLE_G


-- No documentation found for Nested "ComponentSwizzle" "COMPONENT_SWIZZLE_B"
pattern COMPONENT_SWIZZLE_B :: (a ~ ComponentSwizzle) => a
pattern COMPONENT_SWIZZLE_B = VK_COMPONENT_SWIZZLE_B


-- No documentation found for Nested "ComponentSwizzle" "COMPONENT_SWIZZLE_A"
pattern COMPONENT_SWIZZLE_A :: (a ~ ComponentSwizzle) => a
pattern COMPONENT_SWIZZLE_A = VK_COMPONENT_SWIZZLE_A


-- No documentation found for TopLevel "VkImageSubresourceRange"
data ImageSubresourceRange = ImageSubresourceRange
  { -- No documentation found for Nested "ImageSubresourceRange" "aspectMask"
  aspectMask :: ImageAspectFlags
  , -- No documentation found for Nested "ImageSubresourceRange" "baseMipLevel"
  baseMipLevel :: Word32
  , -- No documentation found for Nested "ImageSubresourceRange" "levelCount"
  levelCount :: Word32
  , -- No documentation found for Nested "ImageSubresourceRange" "baseArrayLayer"
  baseArrayLayer :: Word32
  , -- No documentation found for Nested "ImageSubresourceRange" "layerCount"
  layerCount :: Word32
  }
  deriving (Show, Eq)

instance Zero ImageSubresourceRange where
  zero = ImageSubresourceRange zero
                               zero
                               zero
                               zero
                               zero


-- No documentation found for TopLevel "ImageView"
type ImageView = VkImageView

-- No documentation found for TopLevel "ImageViewCreateFlagBits"
type ImageViewCreateFlagBits = VkImageViewCreateFlagBits


{-# complete IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT :: ImageViewCreateFlagBits #-}


-- No documentation found for Nested "ImageViewCreateFlagBits" "IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT"
pattern IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT :: (a ~ ImageViewCreateFlagBits) => a
pattern IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT = VK_IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT

-- No documentation found for TopLevel "ImageViewCreateFlags"
type ImageViewCreateFlags = ImageViewCreateFlagBits


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkImageViewCreateInfo"
data ImageViewCreateInfo = ImageViewCreateInfo
  { -- No documentation found for Nested "ImageViewCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageViewCreateInfo" "flags"
  flags :: ImageViewCreateFlags
  , -- No documentation found for Nested "ImageViewCreateInfo" "image"
  image :: Image
  , -- No documentation found for Nested "ImageViewCreateInfo" "viewType"
  viewType :: ImageViewType
  , -- No documentation found for Nested "ImageViewCreateInfo" "format"
  format :: Format
  , -- No documentation found for Nested "ImageViewCreateInfo" "components"
  components :: ComponentMapping
  , -- No documentation found for Nested "ImageViewCreateInfo" "subresourceRange"
  subresourceRange :: ImageSubresourceRange
  }
  deriving (Show, Eq)

instance Zero ImageViewCreateInfo where
  zero = ImageViewCreateInfo Nothing
                             zero
                             zero
                             zero
                             zero
                             zero
                             zero

#endif

-- No documentation found for TopLevel "ImageViewType"
type ImageViewType = VkImageViewType


{-# complete IMAGE_VIEW_TYPE_1D, IMAGE_VIEW_TYPE_2D, IMAGE_VIEW_TYPE_3D, IMAGE_VIEW_TYPE_CUBE, IMAGE_VIEW_TYPE_1D_ARRAY, IMAGE_VIEW_TYPE_2D_ARRAY, IMAGE_VIEW_TYPE_CUBE_ARRAY :: ImageViewType #-}


-- No documentation found for Nested "ImageViewType" "IMAGE_VIEW_TYPE_1D"
pattern IMAGE_VIEW_TYPE_1D :: (a ~ ImageViewType) => a
pattern IMAGE_VIEW_TYPE_1D = VK_IMAGE_VIEW_TYPE_1D


-- No documentation found for Nested "ImageViewType" "IMAGE_VIEW_TYPE_2D"
pattern IMAGE_VIEW_TYPE_2D :: (a ~ ImageViewType) => a
pattern IMAGE_VIEW_TYPE_2D = VK_IMAGE_VIEW_TYPE_2D


-- No documentation found for Nested "ImageViewType" "IMAGE_VIEW_TYPE_3D"
pattern IMAGE_VIEW_TYPE_3D :: (a ~ ImageViewType) => a
pattern IMAGE_VIEW_TYPE_3D = VK_IMAGE_VIEW_TYPE_3D


-- No documentation found for Nested "ImageViewType" "IMAGE_VIEW_TYPE_CUBE"
pattern IMAGE_VIEW_TYPE_CUBE :: (a ~ ImageViewType) => a
pattern IMAGE_VIEW_TYPE_CUBE = VK_IMAGE_VIEW_TYPE_CUBE


-- No documentation found for Nested "ImageViewType" "IMAGE_VIEW_TYPE_1D_ARRAY"
pattern IMAGE_VIEW_TYPE_1D_ARRAY :: (a ~ ImageViewType) => a
pattern IMAGE_VIEW_TYPE_1D_ARRAY = VK_IMAGE_VIEW_TYPE_1D_ARRAY


-- No documentation found for Nested "ImageViewType" "IMAGE_VIEW_TYPE_2D_ARRAY"
pattern IMAGE_VIEW_TYPE_2D_ARRAY :: (a ~ ImageViewType) => a
pattern IMAGE_VIEW_TYPE_2D_ARRAY = VK_IMAGE_VIEW_TYPE_2D_ARRAY


-- No documentation found for Nested "ImageViewType" "IMAGE_VIEW_TYPE_CUBE_ARRAY"
pattern IMAGE_VIEW_TYPE_CUBE_ARRAY :: (a ~ ImageViewType) => a
pattern IMAGE_VIEW_TYPE_CUBE_ARRAY = VK_IMAGE_VIEW_TYPE_CUBE_ARRAY


-- No documentation found for TopLevel "vkCreateImageView"
createImageView :: Device ->  ImageViewCreateInfo ->  Maybe AllocationCallbacks ->  IO (ImageView)
createImageView = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkDestroyImageView"
destroyImageView :: Device ->  ImageView ->  Maybe AllocationCallbacks ->  IO ()
destroyImageView = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- | A safe wrapper for 'createImageView' and 'destroyImageView' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withImageView
  :: Device -> ImageViewCreateInfo -> Maybe AllocationCallbacks -> (ImageView -> IO a) -> IO a
withImageView device imageViewCreateInfo allocationCallbacks = bracket
  (createImageView device imageViewCreateInfo allocationCallbacks)
  (\o -> destroyImageView device o allocationCallbacks)
