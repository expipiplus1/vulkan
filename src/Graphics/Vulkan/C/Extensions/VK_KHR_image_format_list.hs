{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_image_format_list
  ( VkImageFormatListCreateInfoKHR(..)
  , pattern VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME
  , pattern VK_KHR_IMAGE_FORMAT_LIST_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR
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
  ( VkFormat(..)
  , VkStructureType(..)
  , Zero(..)
  )


-- No documentation found for TopLevel "VkImageFormatListCreateInfoKHR"
data VkImageFormatListCreateInfoKHR = VkImageFormatListCreateInfoKHR
  { -- No documentation found for Nested "VkImageFormatListCreateInfoKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkImageFormatListCreateInfoKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkImageFormatListCreateInfoKHR" "viewFormatCount"
  vkViewFormatCount :: Word32
  , -- No documentation found for Nested "VkImageFormatListCreateInfoKHR" "pViewFormats"
  vkPViewFormats :: Ptr VkFormat
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

instance Zero VkImageFormatListCreateInfoKHR where
  zero = VkImageFormatListCreateInfoKHR zero
                                        zero
                                        zero
                                        zero
-- No documentation found for TopLevel "VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME"
pattern VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME = "VK_KHR_image_format_list"
-- No documentation found for TopLevel "VK_KHR_IMAGE_FORMAT_LIST_SPEC_VERSION"
pattern VK_KHR_IMAGE_FORMAT_LIST_SPEC_VERSION :: Integral a => a
pattern VK_KHR_IMAGE_FORMAT_LIST_SPEC_VERSION = 1
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR = VkStructureType 1000147000
