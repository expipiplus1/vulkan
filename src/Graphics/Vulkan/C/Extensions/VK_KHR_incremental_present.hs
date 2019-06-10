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


-- No documentation found for TopLevel "VkPresentRegionKHR"
data VkPresentRegionKHR = VkPresentRegionKHR
  { -- No documentation found for Nested "VkPresentRegionKHR" "rectangleCount"
  vkRectangleCount :: Word32
  , -- No documentation found for Nested "VkPresentRegionKHR" "pRectangles"
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

-- No documentation found for TopLevel "VkPresentRegionsKHR"
data VkPresentRegionsKHR = VkPresentRegionsKHR
  { -- No documentation found for Nested "VkPresentRegionsKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPresentRegionsKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPresentRegionsKHR" "swapchainCount"
  vkSwapchainCount :: Word32
  , -- No documentation found for Nested "VkPresentRegionsKHR" "pRegions"
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

-- No documentation found for TopLevel "VkRectLayerKHR"
data VkRectLayerKHR = VkRectLayerKHR
  { -- No documentation found for Nested "VkRectLayerKHR" "offset"
  vkOffset :: VkOffset2D
  , -- No documentation found for Nested "VkRectLayerKHR" "extent"
  vkExtent :: VkExtent2D
  , -- No documentation found for Nested "VkRectLayerKHR" "layer"
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
pattern VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME = "VK_KHR_incremental_present"

-- No documentation found for TopLevel "VK_KHR_INCREMENTAL_PRESENT_SPEC_VERSION"
pattern VK_KHR_INCREMENTAL_PRESENT_SPEC_VERSION :: Integral a => a
pattern VK_KHR_INCREMENTAL_PRESENT_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR"
pattern VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR = VkStructureType 1000084000
