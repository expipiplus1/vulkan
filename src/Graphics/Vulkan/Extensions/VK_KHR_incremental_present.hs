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


-- | Nothing
pattern VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR = VkStructureType 1000084000
pattern VK_KHR_INCREMENTAL_PRESENT_SPEC_VERSION :: Integral a => a
pattern VK_KHR_INCREMENTAL_PRESENT_SPEC_VERSION = 1
pattern VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME = "VK_KHR_incremental_present"
-- | TODO: Struct comments
data VkPresentRegionsKHR = VkPresentRegionsKHR
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkSwapchainCount :: Word32
  , vkPRegions :: Ptr VkPresentRegionKHR
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
-- | TODO: Struct comments
data VkPresentRegionKHR = VkPresentRegionKHR
  { vkRectangleCount :: Word32
  , vkPRectangles :: Ptr VkRectLayerKHR
  }
  deriving (Eq, Show)

instance Storable VkPresentRegionKHR where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek ptr = VkPresentRegionKHR <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkRectangleCount (poked :: VkPresentRegionKHR))
                *> poke (ptr `plusPtr` 8) (vkPRectangles (poked :: VkPresentRegionKHR))
-- | TODO: Struct comments
data VkRectLayerKHR = VkRectLayerKHR
  { vkOffset :: VkOffset2D
  , vkExtent :: VkExtent2D
  , vkLayer :: Word32
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
