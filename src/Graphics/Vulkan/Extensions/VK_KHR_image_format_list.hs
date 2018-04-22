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
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.Core10.Core
  ( VkFormat(..)
  , VkStructureType(..)
  )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR = VkStructureType 1000147000
-- No documentation found for TopLevel "VK_KHR_IMAGE_FORMAT_LIST_SPEC_VERSION"
pattern VK_KHR_IMAGE_FORMAT_LIST_SPEC_VERSION :: Integral a => a
pattern VK_KHR_IMAGE_FORMAT_LIST_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME"
pattern VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME = "VK_KHR_image_format_list"
-- | VkImageFormatListCreateInfoKHR - Specify that an image /can/ be used
-- with a particular set of formats
--
-- = Description
--
-- If @viewFormatCount@ is zero, @pViewFormats@ is ignored and the image is
-- created as if the @VkImageFormatListCreateInfoKHR@ structure were not
-- included in the @pNext@ list of
-- 'Graphics.Vulkan.Core10.Image.VkImageCreateInfo'.
--
-- == Valid Usage
--
-- -   If @viewFormatCount@ is not @0@, all of the formats in the
--     @pViewFormats@ array /must/ be compatible with the format specified
--     in the @format@ field of @VkImageCreateInfo@, as described in the
--     [compatibility
--     table](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-views-compatibility).
--
-- -   If @VkImageCreateInfo@::@flags@ does not contain
--     @VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT@, @viewFormatCount@ /must/ be
--     @0@ or @1@.
--
-- -   If @viewFormatCount@ is not @0@, @VkImageCreateInfo@::@format@
--     /must/ be in @pViewFormats@.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR@
--
-- -   If @viewFormatCount@ is not @0@, @pViewFormats@ /must/ be a valid
--     pointer to an array of @viewFormatCount@ valid
--     'Graphics.Vulkan.Core10.Core.VkFormat' values
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Core.VkFormat',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkImageFormatListCreateInfoKHR = VkImageFormatListCreateInfoKHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @viewFormatCount@ is the number of entries in the @pViewFormats@ array.
  vkViewFormatCount :: Word32
  , -- | @pViewFormats@ is an array which lists of all formats which /can/ be
  -- used when creating views of this image.
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
