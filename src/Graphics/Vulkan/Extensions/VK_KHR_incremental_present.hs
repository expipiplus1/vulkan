{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_incremental_present
  ( pattern VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR
  , pattern VK_KHR_INCREMENTAL_PRESENT_SPEC_VERSION
  , pattern VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME
  , VkPresentRegionsKHR(..)
  , VkPresentRegionKHR(..)
  , VkRectLayerKHR(..)
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )


import Graphics.Vulkan.Core10.Core
  ( VkStructureType(..)
  )
import Graphics.Vulkan.Core10.Pipeline
  ( VkExtent2D(..)
  , VkOffset2D(..)
  )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR"
pattern VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR = VkStructureType 1000084000
-- No documentation found for TopLevel "VK_KHR_INCREMENTAL_PRESENT_SPEC_VERSION"
pattern VK_KHR_INCREMENTAL_PRESENT_SPEC_VERSION :: Integral a => a
pattern VK_KHR_INCREMENTAL_PRESENT_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME"
pattern VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME = "VK_KHR_incremental_present"
-- | VkPresentRegionsKHR - Structure hint of rectangular regions changed by
-- vkQueuePresentKHR
--
-- = Description
-- #_description#
--
-- == Valid Usage
--
-- -   @swapchainCount@ /must/ be the same value as
--     @VkPresentInfoKHR@::@swapchainCount@, where @VkPresentInfoKHR@ is in
--     the pNext-chain of this @VkPresentRegionsKHR@ structure.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR@
--
-- -   If @pRegions@ is not @NULL@, @pRegions@ /must/ be a valid pointer to
--     an array of @swapchainCount@ valid @VkPresentRegionKHR@ structures
--
-- -   @swapchainCount@ /must/ be greater than @0@
--
-- = See Also
-- #_see_also#
--
-- 'VkPresentRegionKHR', 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPresentRegionsKHR = VkPresentRegionsKHR
  { -- No documentation found for Nested "VkPresentRegionsKHR" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPresentRegionsKHR" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPresentRegionsKHR" "vkSwapchainCount"
  vkSwapchainCount :: Word32
  , -- No documentation found for Nested "VkPresentRegionsKHR" "vkPRegions"
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
-- | VkPresentRegionKHR - Structure containing rectangular region changed by
-- vkQueuePresentKHR for a given VkImage
--
-- = Description
-- #_description#
--
-- == Valid Usage (Implicit)
--
-- -   If @rectangleCount@ is not @0@, and @pRectangles@ is not @NULL@,
--     @pRectangles@ /must/ be a valid pointer to an array of
--     @rectangleCount@ @VkRectLayerKHR@ structures
--
-- = See Also
-- #_see_also#
--
-- 'VkPresentRegionsKHR', 'VkRectLayerKHR'
data VkPresentRegionKHR = VkPresentRegionKHR
  { -- No documentation found for Nested "VkPresentRegionKHR" "vkRectangleCount"
  vkRectangleCount :: Word32
  , -- No documentation found for Nested "VkPresentRegionKHR" "vkPRectangles"
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
-- | VkRectLayerKHR - Structure containing a rectangle, including layer,
-- changed by vkQueuePresentKHR for a given VkImage
--
-- = Description
-- #_description#
--
-- == Valid Usage
--
-- -   The sum of @offset@ and @extent@ /must/ be no greater than the
--     @imageExtent@ member of the @VkSwapchainCreateInfoKHR@ structure
--     given to
--     'Graphics.Vulkan.Extensions.VK_KHR_swapchain.vkCreateSwapchainKHR'.
--
-- -   @layer@ /must/ be less than @imageArrayLayers@ member of the
--     @VkSwapchainCreateInfoKHR@ structure given to
--     'Graphics.Vulkan.Extensions.VK_KHR_swapchain.vkCreateSwapchainKHR'.
--
-- Some platforms allow the size of a surface to change, and then scale the
-- pixels of the image to fit the surface. @VkRectLayerKHR@ specifies
-- pixels of the swapchain’s image(s), which will be constant for the life
-- of the swapchain.
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Pipeline.VkExtent2D',
-- 'Graphics.Vulkan.Core10.Pipeline.VkOffset2D', 'VkPresentRegionKHR'
data VkRectLayerKHR = VkRectLayerKHR
  { -- No documentation found for Nested "VkRectLayerKHR" "vkOffset"
  vkOffset :: VkOffset2D
  , -- No documentation found for Nested "VkRectLayerKHR" "vkExtent"
  vkExtent :: VkExtent2D
  , -- No documentation found for Nested "VkRectLayerKHR" "vkLayer"
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
