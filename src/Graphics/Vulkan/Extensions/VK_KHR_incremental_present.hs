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
  , pattern VK_KHR_INCREMENTAL_PRESENT_SPEC_VERSION
  , pattern VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR
  ) where

import Control.Monad
  ( (<=<)
  )
import Data.Maybe
  ( maybe
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


import Graphics.Vulkan.C.Extensions.VK_KHR_incremental_present
  ( VkPresentRegionKHR(..)
  , VkPresentRegionsKHR(..)
  , VkRectLayerKHR(..)
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
import Graphics.Vulkan.C.Extensions.VK_KHR_incremental_present
  ( pattern VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME
  , pattern VK_KHR_INCREMENTAL_PRESENT_SPEC_VERSION
  )


-- No documentation found for TopLevel "PresentRegionKHR"
data PresentRegionKHR = PresentRegionKHR
  { -- Optional length valued member elided
  -- No documentation found for Nested "PresentRegionKHR" "pRectangles"
  vkPRectangles :: Maybe (Vector RectLayerKHR)
  }
  deriving (Show, Eq)
withCStructPresentRegionKHR :: PresentRegionKHR -> (VkPresentRegionKHR -> IO a) -> IO a
withCStructPresentRegionKHR from cont = maybeWith (withVec withCStructRectLayerKHR) (vkPRectangles (from :: PresentRegionKHR)) (\pRectangles -> cont (VkPresentRegionKHR (maybe 0 (fromIntegral . Data.Vector.length) (vkPRectangles (from :: PresentRegionKHR))) pRectangles))
fromCStructPresentRegionKHR :: VkPresentRegionKHR -> IO PresentRegionKHR
fromCStructPresentRegionKHR c = PresentRegionKHR <$> -- Optional length valued member elided
                                                 maybePeek (\p -> Data.Vector.generateM (fromIntegral (vkRectangleCount (c :: VkPresentRegionKHR))) (((fromCStructRectLayerKHR <=<) . peekElemOff) p)) (vkPRectangles (c :: VkPresentRegionKHR))
-- No documentation found for TopLevel "PresentRegionsKHR"
data PresentRegionsKHR = PresentRegionsKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "PresentRegionsKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  -- Optional length valued member elided
  , -- No documentation found for Nested "PresentRegionsKHR" "pRegions"
  vkPRegions :: Maybe (Vector PresentRegionKHR)
  }
  deriving (Show, Eq)
withCStructPresentRegionsKHR :: PresentRegionsKHR -> (VkPresentRegionsKHR -> IO a) -> IO a
withCStructPresentRegionsKHR from cont = maybeWith (withVec withCStructPresentRegionKHR) (vkPRegions (from :: PresentRegionsKHR)) (\pRegions -> maybeWith withSomeVkStruct (vkPNext (from :: PresentRegionsKHR)) (\pPNext -> cont (VkPresentRegionsKHR VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR pPNext (maybe 0 (fromIntegral . Data.Vector.length) (vkPRegions (from :: PresentRegionsKHR))) pRegions)))
fromCStructPresentRegionsKHR :: VkPresentRegionsKHR -> IO PresentRegionsKHR
fromCStructPresentRegionsKHR c = PresentRegionsKHR <$> -- Univalued Member elided
                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPresentRegionsKHR)))
                                                   -- Optional length valued member elided
                                                   <*> maybePeek (\p -> Data.Vector.generateM (fromIntegral (vkSwapchainCount (c :: VkPresentRegionsKHR))) (((fromCStructPresentRegionKHR <=<) . peekElemOff) p)) (vkPRegions (c :: VkPresentRegionsKHR))
-- No documentation found for TopLevel "RectLayerKHR"
data RectLayerKHR = RectLayerKHR
  { -- No documentation found for Nested "RectLayerKHR" "offset"
  vkOffset :: Offset2D
  , -- No documentation found for Nested "RectLayerKHR" "extent"
  vkExtent :: Extent2D
  , -- No documentation found for Nested "RectLayerKHR" "layer"
  vkLayer :: Word32
  }
  deriving (Show, Eq)
withCStructRectLayerKHR :: RectLayerKHR -> (VkRectLayerKHR -> IO a) -> IO a
withCStructRectLayerKHR from cont = withCStructExtent2D (vkExtent (from :: RectLayerKHR)) (\extent -> withCStructOffset2D (vkOffset (from :: RectLayerKHR)) (\offset -> cont (VkRectLayerKHR offset extent (vkLayer (from :: RectLayerKHR)))))
fromCStructRectLayerKHR :: VkRectLayerKHR -> IO RectLayerKHR
fromCStructRectLayerKHR c = RectLayerKHR <$> (fromCStructOffset2D (vkOffset (c :: VkRectLayerKHR)))
                                         <*> (fromCStructExtent2D (vkExtent (c :: VkRectLayerKHR)))
                                         <*> pure (vkLayer (c :: VkRectLayerKHR))
