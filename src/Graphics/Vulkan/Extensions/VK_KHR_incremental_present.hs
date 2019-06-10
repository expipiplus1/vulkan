{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_incremental_present
  ( PresentRegionKHR(..)
#if defined(VK_USE_PLATFORM_GGP)
  , PresentRegionsKHR(..)
#endif
  , RectLayerKHR(..)
  , pattern KHR_INCREMENTAL_PRESENT_EXTENSION_NAME
  , pattern KHR_INCREMENTAL_PRESENT_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PRESENT_REGIONS_KHR
  ) where

import Data.String
  ( IsString
  )
import Data.Vector
  ( Vector
  )
import Data.Word
  ( Word32
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_incremental_present
  ( pattern VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME
  , pattern VK_KHR_INCREMENTAL_PRESENT_SPEC_VERSION
  )
import Graphics.Vulkan.Core10.Pipeline
  ( Extent2D(..)
  , Offset2D(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PRESENT_REGIONS_KHR
  )



-- No documentation found for TopLevel "VkPresentRegionKHR"
data PresentRegionKHR = PresentRegionKHR
  { -- No documentation found for Nested "PresentRegionKHR" "pRectangles"
  rectangles :: Either Word32 (Vector RectLayerKHR)
  }
  deriving (Show, Eq)

instance Zero PresentRegionKHR where
  zero = PresentRegionKHR (Left 0)



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPresentRegionsKHR"
data PresentRegionsKHR = PresentRegionsKHR
  { -- No documentation found for Nested "PresentRegionsKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PresentRegionsKHR" "pRegions"
  regions :: Either Word32 (Vector PresentRegionKHR)
  }
  deriving (Show, Eq)

instance Zero PresentRegionsKHR where
  zero = PresentRegionsKHR Nothing
                           (Left 0)

#endif


-- No documentation found for TopLevel "VkRectLayerKHR"
data RectLayerKHR = RectLayerKHR
  { -- No documentation found for Nested "RectLayerKHR" "offset"
  offset :: Offset2D
  , -- No documentation found for Nested "RectLayerKHR" "extent"
  extent :: Extent2D
  , -- No documentation found for Nested "RectLayerKHR" "layer"
  layer :: Word32
  }
  deriving (Show, Eq)

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
