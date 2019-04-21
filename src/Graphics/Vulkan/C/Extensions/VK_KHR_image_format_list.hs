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


-- | VkImageFormatListCreateInfoKHR - Specify that an image /can/ be used
-- with a particular set of formats
--
-- = Description
--
-- If @viewFormatCount@ is zero, @pViewFormats@ is ignored and the image is
-- created as if the 'VkImageFormatListCreateInfoKHR' structure were not
-- included in the @pNext@ list of
-- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'.
--
-- == Valid Usage
--
-- -   If @viewFormatCount@ is not @0@, all of the formats in the
--     @pViewFormats@ array /must/ be compatible with the format specified
--     in the @format@ field of
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo', as described in
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#formats-compatibility compatibility table>.
--
-- -   If 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@flags@ does
--     not contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT',
--     @viewFormatCount@ /must/ be @0@ or @1@.
--
-- -   If @viewFormatCount@ is not @0@,
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@format@ /must/
--     be in @pViewFormats@.
--
-- Unresolved directive in VkImageFormatListCreateInfoKHR.txt -
-- include::{generated}\/validity\/structs\/VkImageFormatListCreateInfoKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
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

instance Zero VkImageFormatListCreateInfoKHR where
  zero = VkImageFormatListCreateInfoKHR VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR
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
