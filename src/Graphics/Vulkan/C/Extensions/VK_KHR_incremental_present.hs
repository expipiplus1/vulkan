{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_incremental_present
  ( VkPresentRegionKHR(..)
  , VkPresentRegionsKHR(..)
  , VkRectLayerKHR(..)
  , pattern VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME
  , pattern VK_KHR_INCREMENTAL_PRESENT_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkExtent2D(..)
  , VkOffset2D(..)
  )


-- | VkPresentRegionKHR - Structure containing rectangular region changed by
-- vkQueuePresentKHR for a given VkImage
--
-- = Description
--
-- Unresolved directive in VkPresentRegionKHR.txt -
-- include::{generated}\/validity\/structs\/VkPresentRegionKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkPresentRegionKHR = VkPresentRegionKHR
  { -- | @rectangleCount@ is the number of rectangles in @pRectangles@, or zero
  -- if the entire image has changed and should be presented.
  vkRectangleCount :: Word32
  , -- | @pRectangles@ is either @NULL@ or a pointer to an array of
  -- 'VkRectLayerKHR' structures. The 'VkRectLayerKHR' structure is the
  -- framebuffer coordinates, plus layer, of a portion of a presentable image
  -- that has changed and /must/ be presented. If non-@NULL@, each entry in
  -- @pRectangles@ is a rectangle of the given image that has changed since
  -- the last image was presented to the given swapchain.
  vkPRectangles :: Ptr VkRectLayerKHR
  }
  deriving (Eq, Show)

instance Storable VkPresentRegionKHR where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek ptr = VkPresentRegionKHR <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkRectangleCount (poked :: VkPresentRegionKHR))
                *> poke (ptr `plusPtr` 8) (vkPRectangles (poked :: VkPresentRegionKHR))

instance Zero VkPresentRegionKHR where
  zero = VkPresentRegionKHR zero
                            zero

-- | VkPresentRegionsKHR - Structure hint of rectangular regions changed by
-- vkQueuePresentKHR
--
-- == Valid Usage
--
-- Unresolved directive in VkPresentRegionsKHR.txt -
-- include::{generated}\/validity\/structs\/VkPresentRegionsKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkPresentRegionsKHR = VkPresentRegionsKHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @swapchainCount@ /must/ be the same value as
  -- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkPresentInfoKHR'::@swapchainCount@,
  -- where 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkPresentInfoKHR'
  -- is in the @pNext@ chain of this 'VkPresentRegionsKHR' structure
  vkSwapchainCount :: Word32
  , -- | @pRegions@ is @NULL@ or a pointer to an array of 'VkPresentRegionKHR'
  -- elements with @swapchainCount@ entries. If not @NULL@, each element of
  -- @pRegions@ contains the region that has changed since the last present
  -- to the swapchain in the corresponding entry in the
  -- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkPresentInfoKHR'::@pSwapchains@
  -- array.
  vkPRegions :: Ptr VkPresentRegionKHR
  }
  deriving (Eq, Show)

instance Storable VkPresentRegionsKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPresentRegionsKHR <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 8)
                                 <*> peek (ptr `plusPtr` 16)
                                 <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPresentRegionsKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPresentRegionsKHR))
                *> poke (ptr `plusPtr` 16) (vkSwapchainCount (poked :: VkPresentRegionsKHR))
                *> poke (ptr `plusPtr` 24) (vkPRegions (poked :: VkPresentRegionsKHR))

instance Zero VkPresentRegionsKHR where
  zero = VkPresentRegionsKHR VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR
                             zero
                             zero
                             zero

-- | VkRectLayerKHR - Structure containing a rectangle, including layer,
-- changed by vkQueuePresentKHR for a given VkImage
--
-- == Valid Usage
--
-- -   The sum of @offset@ and @extent@ /must/ be no greater than the
--     @imageExtent@ member of the
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainCreateInfoKHR'
--     structure given to
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkCreateSwapchainKHR'.
--
-- -   @layer@ /must/ be less than @imageArrayLayers@ member of the
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainCreateInfoKHR'
--     structure given to
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkCreateSwapchainKHR'.
--
-- Unresolved directive in VkRectLayerKHR.txt -
-- include::{generated}\/validity\/structs\/VkRectLayerKHR.txt[]
--
-- Some platforms allow the size of a surface to change, and then scale the
-- pixels of the image to fit the surface. 'VkRectLayerKHR' specifies
-- pixels of the swapchain’s image(s), which will be constant for the life
-- of the swapchain.
--
-- = See Also
--
-- No cross-references are available
data VkRectLayerKHR = VkRectLayerKHR
  { -- | @offset@ is the origin of the rectangle, in pixels.
  vkOffset :: VkOffset2D
  , -- | @extent@ is the size of the rectangle, in pixels.
  vkExtent :: VkExtent2D
  , -- | @layer@ is the layer of the image. For images with only one layer, the
  -- value of @layer@ /must/ be 0.
  vkLayer :: Word32
  }
  deriving (Eq, Show)

instance Storable VkRectLayerKHR where
  sizeOf ~_ = 20
  alignment ~_ = 4
  peek ptr = VkRectLayerKHR <$> peek (ptr `plusPtr` 0)
                            <*> peek (ptr `plusPtr` 8)
                            <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkOffset (poked :: VkRectLayerKHR))
                *> poke (ptr `plusPtr` 8) (vkExtent (poked :: VkRectLayerKHR))
                *> poke (ptr `plusPtr` 16) (vkLayer (poked :: VkRectLayerKHR))

instance Zero VkRectLayerKHR where
  zero = VkRectLayerKHR zero
                        zero
                        zero

-- No documentation found for TopLevel "VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME"
pattern VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME = "VK_KHR_incremental_present"

-- No documentation found for TopLevel "VK_KHR_INCREMENTAL_PRESENT_SPEC_VERSION"
pattern VK_KHR_INCREMENTAL_PRESENT_SPEC_VERSION :: Integral a => a
pattern VK_KHR_INCREMENTAL_PRESENT_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR"
pattern VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR = VkStructureType 1000084000
