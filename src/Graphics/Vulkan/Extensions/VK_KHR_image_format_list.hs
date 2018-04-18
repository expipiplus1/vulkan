{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_image_format_list
  ( pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR
  , pattern VK_KHR_IMAGE_FORMAT_LIST_SPEC_VERSION
  , pattern VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME
  , VkImageFormatListCreateInfoKHR(..)
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
  ( VkFormat(..)
  , VkStructureType(..)
  )


-- | Nothing
pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR = VkStructureType 1000147000
pattern VK_KHR_IMAGE_FORMAT_LIST_SPEC_VERSION :: Integral a => a
pattern VK_KHR_IMAGE_FORMAT_LIST_SPEC_VERSION = 1
pattern VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME = "VK_KHR_image_format_list"
-- | TODO: Struct comments
data VkImageFormatListCreateInfoKHR = VkImageFormatListCreateInfoKHR
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkViewFormatCount :: Word32
  , vkPViewFormats :: Ptr VkFormat
  }
  deriving (Eq, Show)

instance Storable VkImageFormatListCreateInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkImageFormatListCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                            <*> peek (ptr `plusPtr` 8)
                                            <*> peek (ptr `plusPtr` 16)
                                            <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImageFormatListCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImageFormatListCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkViewFormatCount (poked :: VkImageFormatListCreateInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkPViewFormats (poked :: VkImageFormatListCreateInfoKHR))
