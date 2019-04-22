{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_incremental_present
  ( withCStructPresentRegionKHR
  , fromCStructPresentRegionKHR
  , PresentRegionKHR(..)
  , withCStructPresentRegionsKHR
  , fromCStructPresentRegionsKHR
  , PresentRegionsKHR(..)
  , withCStructRectLayerKHR
  , fromCStructRectLayerKHR
  , RectLayerKHR(..)
  , pattern KHR_INCREMENTAL_PRESENT_EXTENSION_NAME
  , pattern KHR_INCREMENTAL_PRESENT_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PRESENT_REGIONS_KHR
  ) where

import Control.Monad
  ( (<=<)
  )
import Data.Maybe
  ( maybe
  )
import Data.String
  ( IsString
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
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_incremental_present
  ( VkPresentRegionKHR(..)
  , VkPresentRegionsKHR(..)
  , VkRectLayerKHR(..)
  , pattern VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME
  , pattern VK_KHR_INCREMENTAL_PRESENT_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR
  )
import Graphics.Vulkan.Core10.Pipeline
  ( Extent2D(..)
  , Offset2D(..)
  , fromCStructExtent2D
  , fromCStructOffset2D
  , withCStructExtent2D
  , withCStructOffset2D
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PRESENT_REGIONS_KHR
  )



-- | VkPresentRegionKHR - Structure containing rectangular region changed by
-- vkQueuePresentKHR for a given VkImage
--
-- == Valid Usage (Implicit)
--
-- -   If @rectangleCount@ is not @0@, and @pRectangles@ is not @NULL@,
--     @pRectangles@ /must/ be a valid pointer to an array of
--     @rectangleCount@ valid
--     'Graphics.Vulkan.C.Extensions.VK_KHR_incremental_present.VkRectLayerKHR'
--     structures
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_incremental_present.VkPresentRegionsKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_incremental_present.VkRectLayerKHR'
data PresentRegionKHR = PresentRegionKHR
  { -- Optional length valued member elided
  -- No documentation found for Nested "PresentRegionKHR" "pRectangles"
  rectangles :: Maybe (Vector RectLayerKHR)
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPresentRegionKHR' and
-- marshal a 'PresentRegionKHR' into it. The 'VkPresentRegionKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPresentRegionKHR :: PresentRegionKHR -> (VkPresentRegionKHR -> IO a) -> IO a
withCStructPresentRegionKHR marshalled cont = maybeWith (withVec withCStructRectLayerKHR) (rectangles (marshalled :: PresentRegionKHR)) (\pPRectangles -> cont (VkPresentRegionKHR (maybe 0 (fromIntegral . Data.Vector.length) (rectangles (marshalled :: PresentRegionKHR))) pPRectangles))

-- | A function to read a 'VkPresentRegionKHR' and all additional
-- structures in the pointer chain into a 'PresentRegionKHR'.
fromCStructPresentRegionKHR :: VkPresentRegionKHR -> IO PresentRegionKHR
fromCStructPresentRegionKHR c = PresentRegionKHR <$> -- Optional length valued member elided
                                                 maybePeek (\p -> Data.Vector.generateM (fromIntegral (vkRectangleCount (c :: VkPresentRegionKHR))) (((fromCStructRectLayerKHR <=<) . peekElemOff) p)) (vkPRectangles (c :: VkPresentRegionKHR))

instance Zero PresentRegionKHR where
  zero = PresentRegionKHR Nothing



-- | VkPresentRegionsKHR - Structure hint of rectangular regions changed by
-- vkQueuePresentKHR
--
-- == Valid Usage
--
-- -   @swapchainCount@ /must/ be the same value as
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkPresentInfoKHR'::@swapchainCount@,
--     where
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkPresentInfoKHR' is
--     in the @pNext@ chain of this
--     'Graphics.Vulkan.C.Extensions.VK_KHR_incremental_present.VkPresentRegionsKHR'
--     structure
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_KHR_incremental_present.VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR'
--
-- -   If @pRegions@ is not @NULL@, @pRegions@ /must/ be a valid pointer to
--     an array of @swapchainCount@ valid
--     'Graphics.Vulkan.C.Extensions.VK_KHR_incremental_present.VkPresentRegionKHR'
--     structures
--
-- -   @swapchainCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_incremental_present.VkPresentRegionKHR',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PresentRegionsKHR = PresentRegionsKHR
  { -- Univalued member elided
  -- No documentation found for Nested "PresentRegionsKHR" "pNext"
  next :: Maybe SomeVkStruct
  -- Optional length valued member elided
  , -- No documentation found for Nested "PresentRegionsKHR" "pRegions"
  regions :: Maybe (Vector PresentRegionKHR)
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPresentRegionsKHR' and
-- marshal a 'PresentRegionsKHR' into it. The 'VkPresentRegionsKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPresentRegionsKHR :: PresentRegionsKHR -> (VkPresentRegionsKHR -> IO a) -> IO a
withCStructPresentRegionsKHR marshalled cont = maybeWith (withVec withCStructPresentRegionKHR) (regions (marshalled :: PresentRegionsKHR)) (\pPRegions -> maybeWith withSomeVkStruct (next (marshalled :: PresentRegionsKHR)) (\pPNext -> cont (VkPresentRegionsKHR VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR pPNext (maybe 0 (fromIntegral . Data.Vector.length) (regions (marshalled :: PresentRegionsKHR))) pPRegions)))

-- | A function to read a 'VkPresentRegionsKHR' and all additional
-- structures in the pointer chain into a 'PresentRegionsKHR'.
fromCStructPresentRegionsKHR :: VkPresentRegionsKHR -> IO PresentRegionsKHR
fromCStructPresentRegionsKHR c = PresentRegionsKHR <$> -- Univalued Member elided
                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPresentRegionsKHR)))
                                                   -- Optional length valued member elided
                                                   <*> maybePeek (\p -> Data.Vector.generateM (fromIntegral (vkSwapchainCount (c :: VkPresentRegionsKHR))) (((fromCStructPresentRegionKHR <=<) . peekElemOff) p)) (vkPRegions (c :: VkPresentRegionsKHR))

instance Zero PresentRegionsKHR where
  zero = PresentRegionsKHR Nothing
                           Nothing



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
-- Some platforms allow the size of a surface to change, and then scale the
-- pixels of the image to fit the surface.
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_incremental_present.VkRectLayerKHR'
-- specifies pixels of the swapchain’s image(s), which will be constant for
-- the life of the swapchain.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkExtent2D',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkOffset2D',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_incremental_present.VkPresentRegionKHR'
data RectLayerKHR = RectLayerKHR
  { -- No documentation found for Nested "RectLayerKHR" "offset"
  offset :: Offset2D
  , -- No documentation found for Nested "RectLayerKHR" "extent"
  extent :: Extent2D
  , -- No documentation found for Nested "RectLayerKHR" "layer"
  layer :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkRectLayerKHR' and
-- marshal a 'RectLayerKHR' into it. The 'VkRectLayerKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructRectLayerKHR :: RectLayerKHR -> (VkRectLayerKHR -> IO a) -> IO a
withCStructRectLayerKHR marshalled cont = withCStructExtent2D (extent (marshalled :: RectLayerKHR)) (\extent'' -> withCStructOffset2D (offset (marshalled :: RectLayerKHR)) (\offset'' -> cont (VkRectLayerKHR offset'' extent'' (layer (marshalled :: RectLayerKHR)))))

-- | A function to read a 'VkRectLayerKHR' and all additional
-- structures in the pointer chain into a 'RectLayerKHR'.
fromCStructRectLayerKHR :: VkRectLayerKHR -> IO RectLayerKHR
fromCStructRectLayerKHR c = RectLayerKHR <$> (fromCStructOffset2D (vkOffset (c :: VkRectLayerKHR)))
                                         <*> (fromCStructExtent2D (vkExtent (c :: VkRectLayerKHR)))
                                         <*> pure (vkLayer (c :: VkRectLayerKHR))

instance Zero RectLayerKHR where
  zero = RectLayerKHR zero
                      zero
                      zero


-- No documentation found for TopLevel "VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME"
pattern KHR_INCREMENTAL_PRESENT_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_INCREMENTAL_PRESENT_EXTENSION_NAME = VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_INCREMENTAL_PRESENT_SPEC_VERSION"
pattern KHR_INCREMENTAL_PRESENT_SPEC_VERSION :: Integral a => a
pattern KHR_INCREMENTAL_PRESENT_SPEC_VERSION = VK_KHR_INCREMENTAL_PRESENT_SPEC_VERSION
