{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Core10.Image
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  ImageCreateInfo(..)
  , 
#endif
  ImageLayout
  , pattern IMAGE_LAYOUT_UNDEFINED
  , pattern IMAGE_LAYOUT_GENERAL
  , pattern IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
  , pattern IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
  , pattern IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL
  , pattern IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
  , pattern IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
  , pattern IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
  , pattern IMAGE_LAYOUT_PREINITIALIZED
  , pattern IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL
  , pattern IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL
  , pattern IMAGE_LAYOUT_PRESENT_SRC_KHR
  , pattern IMAGE_LAYOUT_SHARED_PRESENT_KHR
  , pattern IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV
  , pattern IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT
  , SubresourceLayout(..)
  , createImage
  , destroyImage
#if defined(VK_USE_PLATFORM_GGP)
  , getImageSubresourceLayout
#endif
  , withImage
  ) where

import Control.Exception
  ( bracket
  )

#if defined(VK_USE_PLATFORM_GGP)
import Control.Monad
  ( (<=<)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Data.Vector
  ( Vector
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Data.Word
  ( Word32
  )
#endif
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
import Graphics.Vulkan.C.Core10.Image
  ( VkImageLayout(..)
  , vkCreateImage
  , vkDestroyImage
  , pattern VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_GENERAL
  , pattern VK_IMAGE_LAYOUT_PREINITIALIZED
  , pattern VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_UNDEFINED
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Image
  ( vkGetImageSubresourceLayout
  )
#endif
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2
  ( pattern VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map
  ( pattern VK_IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_shared_presentable_image
  ( pattern VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_swapchain
  ( pattern VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
  )
import Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image
  ( pattern VK_IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Buffer
  ( SharingMode
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Core
  ( Format
  )
#endif
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , DeviceSize
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Extent3D(..)
  , ImageCreateFlags
  , ImageTiling
  , ImageType
  , ImageUsageFlags
  , SampleCountFlagBits
  )
#endif
import Graphics.Vulkan.Core10.MemoryManagement
  ( Image
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( ImageSubresource(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( FromCStruct(..)
  , SomeVkStruct
  )
#endif



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkImageCreateInfo"
data ImageCreateInfo = ImageCreateInfo
  { -- No documentation found for Nested "ImageCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageCreateInfo" "flags"
  flags :: ImageCreateFlags
  , -- No documentation found for Nested "ImageCreateInfo" "imageType"
  imageType :: ImageType
  , -- No documentation found for Nested "ImageCreateInfo" "format"
  format :: Format
  , -- No documentation found for Nested "ImageCreateInfo" "extent"
  extent :: Extent3D
  , -- No documentation found for Nested "ImageCreateInfo" "mipLevels"
  mipLevels :: Word32
  , -- No documentation found for Nested "ImageCreateInfo" "arrayLayers"
  arrayLayers :: Word32
  , -- No documentation found for Nested "ImageCreateInfo" "samples"
  samples :: SampleCountFlagBits
  , -- No documentation found for Nested "ImageCreateInfo" "tiling"
  tiling :: ImageTiling
  , -- No documentation found for Nested "ImageCreateInfo" "usage"
  usage :: ImageUsageFlags
  , -- No documentation found for Nested "ImageCreateInfo" "sharingMode"
  sharingMode :: SharingMode
  , -- No documentation found for Nested "ImageCreateInfo" "pQueueFamilyIndices"
  queueFamilyIndices :: Vector Word32
  , -- No documentation found for Nested "ImageCreateInfo" "initialLayout"
  initialLayout :: ImageLayout
  }
  deriving (Show, Eq)

instance Zero ImageCreateInfo where
  zero = ImageCreateInfo Nothing
                         zero
                         zero
                         zero
                         zero
                         zero
                         zero
                         zero
                         zero
                         zero
                         zero
                         mempty
                         zero

#endif

-- No documentation found for TopLevel "ImageLayout"
type ImageLayout = VkImageLayout


{-# complete IMAGE_LAYOUT_UNDEFINED, IMAGE_LAYOUT_GENERAL, IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL, IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL, IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL, IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL, IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL, IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, IMAGE_LAYOUT_PREINITIALIZED, IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL, IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL, IMAGE_LAYOUT_PRESENT_SRC_KHR, IMAGE_LAYOUT_SHARED_PRESENT_KHR, IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV, IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT :: ImageLayout #-}


-- No documentation found for Nested "ImageLayout" "IMAGE_LAYOUT_UNDEFINED"
pattern IMAGE_LAYOUT_UNDEFINED :: (a ~ ImageLayout) => a
pattern IMAGE_LAYOUT_UNDEFINED = VK_IMAGE_LAYOUT_UNDEFINED


-- No documentation found for Nested "ImageLayout" "IMAGE_LAYOUT_GENERAL"
pattern IMAGE_LAYOUT_GENERAL :: (a ~ ImageLayout) => a
pattern IMAGE_LAYOUT_GENERAL = VK_IMAGE_LAYOUT_GENERAL


-- No documentation found for Nested "ImageLayout" "IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL"
pattern IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL :: (a ~ ImageLayout) => a
pattern IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL


-- No documentation found for Nested "ImageLayout" "IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL"
pattern IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL :: (a ~ ImageLayout) => a
pattern IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL


-- No documentation found for Nested "ImageLayout" "IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL"
pattern IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL :: (a ~ ImageLayout) => a
pattern IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL = VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL


-- No documentation found for Nested "ImageLayout" "IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL"
pattern IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL :: (a ~ ImageLayout) => a
pattern IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL


-- No documentation found for Nested "ImageLayout" "IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL"
pattern IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL :: (a ~ ImageLayout) => a
pattern IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL = VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL


-- No documentation found for Nested "ImageLayout" "IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL"
pattern IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL :: (a ~ ImageLayout) => a
pattern IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL


-- No documentation found for Nested "ImageLayout" "IMAGE_LAYOUT_PREINITIALIZED"
pattern IMAGE_LAYOUT_PREINITIALIZED :: (a ~ ImageLayout) => a
pattern IMAGE_LAYOUT_PREINITIALIZED = VK_IMAGE_LAYOUT_PREINITIALIZED


-- No documentation found for Nested "ImageLayout" "IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL"
pattern IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL :: (a ~ ImageLayout) => a
pattern IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL = VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL


-- No documentation found for Nested "ImageLayout" "IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL"
pattern IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL :: (a ~ ImageLayout) => a
pattern IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL = VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL


-- No documentation found for Nested "ImageLayout" "IMAGE_LAYOUT_PRESENT_SRC_KHR"
pattern IMAGE_LAYOUT_PRESENT_SRC_KHR :: (a ~ ImageLayout) => a
pattern IMAGE_LAYOUT_PRESENT_SRC_KHR = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR


-- No documentation found for Nested "ImageLayout" "IMAGE_LAYOUT_SHARED_PRESENT_KHR"
pattern IMAGE_LAYOUT_SHARED_PRESENT_KHR :: (a ~ ImageLayout) => a
pattern IMAGE_LAYOUT_SHARED_PRESENT_KHR = VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR


-- No documentation found for Nested "ImageLayout" "IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV"
pattern IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV :: (a ~ ImageLayout) => a
pattern IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV = VK_IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV


-- No documentation found for Nested "ImageLayout" "IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT"
pattern IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT :: (a ~ ImageLayout) => a
pattern IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT = VK_IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT


-- No documentation found for TopLevel "VkSubresourceLayout"
data SubresourceLayout = SubresourceLayout
  { -- No documentation found for Nested "SubresourceLayout" "offset"
  offset :: DeviceSize
  , -- No documentation found for Nested "SubresourceLayout" "size"
  size :: DeviceSize
  , -- No documentation found for Nested "SubresourceLayout" "rowPitch"
  rowPitch :: DeviceSize
  , -- No documentation found for Nested "SubresourceLayout" "arrayPitch"
  arrayPitch :: DeviceSize
  , -- No documentation found for Nested "SubresourceLayout" "depthPitch"
  depthPitch :: DeviceSize
  }
  deriving (Show, Eq)

instance Zero SubresourceLayout where
  zero = SubresourceLayout zero
                           zero
                           zero
                           zero
                           zero



-- No documentation found for TopLevel "vkCreateImage"
createImage :: Device ->  ImageCreateInfo ->  Maybe AllocationCallbacks ->  IO (Image)
createImage = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkDestroyImage"
destroyImage :: Device ->  Image ->  Maybe AllocationCallbacks ->  IO ()
destroyImage = undefined {- {wrapped (pretty cName) :: Doc ()} -}


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetImageSubresourceLayout"
getImageSubresourceLayout :: Device ->  Image ->  ImageSubresource ->  IO (SubresourceLayout)
getImageSubresourceLayout = undefined {- {wrapped (pretty cName) :: Doc ()} -}
#endif

-- | A safe wrapper for 'createImage' and 'destroyImage' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withImage
  :: Device -> ImageCreateInfo -> Maybe AllocationCallbacks -> (Image -> IO a) -> IO a
withImage device imageCreateInfo allocationCallbacks = bracket
  (createImage device imageCreateInfo allocationCallbacks)
  (\o -> destroyImage device o allocationCallbacks)
