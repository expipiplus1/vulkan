{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_image_format_list
  ( withCStructImageFormatListCreateInfoKHR
  , fromCStructImageFormatListCreateInfoKHR
  , ImageFormatListCreateInfoKHR(..)
  , pattern KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME
  , pattern KHR_IMAGE_FORMAT_LIST_SPEC_VERSION
  , pattern STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR
  ) where

import Data.Function
  ( (&)
  )
import Data.String
  ( IsString
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( empty
  , generateM
  , length
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
import Graphics.Vulkan.C.Extensions.VK_KHR_image_format_list
  ( VkImageFormatListCreateInfoKHR(..)
  , pattern VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME
  , pattern VK_KHR_IMAGE_FORMAT_LIST_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR
  )
import Graphics.Vulkan.Core10.Core
  ( Format
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
  ( pattern STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR
  )



-- | VkImageFormatListCreateInfoKHR - Specify that an image /can/ be used
-- with a particular set of formats
--
-- = Description
--
-- If @viewFormatCount@ is zero, @pViewFormats@ is ignored and the image is
-- created as if the
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_image_format_list.VkImageFormatListCreateInfoKHR'
-- structure were not included in the @pNext@ list of
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
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_KHR_image_format_list.VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR'
--
-- -   If @viewFormatCount@ is not @0@, @pViewFormats@ /must/ be a valid
--     pointer to an array of @viewFormatCount@ valid
--     'Graphics.Vulkan.C.Core10.Core.VkFormat' values
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkFormat',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data ImageFormatListCreateInfoKHR = ImageFormatListCreateInfoKHR
  { -- Univalued member elided
  -- No documentation found for Nested "ImageFormatListCreateInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "ImageFormatListCreateInfoKHR" "pViewFormats"
  viewFormats :: Vector Format
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkImageFormatListCreateInfoKHR' and
-- marshal a 'ImageFormatListCreateInfoKHR' into it. The 'VkImageFormatListCreateInfoKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructImageFormatListCreateInfoKHR :: ImageFormatListCreateInfoKHR -> (VkImageFormatListCreateInfoKHR -> IO a) -> IO a
withCStructImageFormatListCreateInfoKHR marshalled cont = withVec (&) (viewFormats (marshalled :: ImageFormatListCreateInfoKHR)) (\pPViewFormats -> maybeWith withSomeVkStruct (next (marshalled :: ImageFormatListCreateInfoKHR)) (\pPNext -> cont (VkImageFormatListCreateInfoKHR VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR pPNext (fromIntegral (Data.Vector.length (viewFormats (marshalled :: ImageFormatListCreateInfoKHR)))) pPViewFormats)))

-- | A function to read a 'VkImageFormatListCreateInfoKHR' and all additional
-- structures in the pointer chain into a 'ImageFormatListCreateInfoKHR'.
fromCStructImageFormatListCreateInfoKHR :: VkImageFormatListCreateInfoKHR -> IO ImageFormatListCreateInfoKHR
fromCStructImageFormatListCreateInfoKHR c = ImageFormatListCreateInfoKHR <$> -- Univalued Member elided
                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImageFormatListCreateInfoKHR)))
                                                                         -- Length valued member elided
                                                                         <*> (Data.Vector.generateM (fromIntegral (vkViewFormatCount (c :: VkImageFormatListCreateInfoKHR))) (peekElemOff (vkPViewFormats (c :: VkImageFormatListCreateInfoKHR))))

instance Zero ImageFormatListCreateInfoKHR where
  zero = ImageFormatListCreateInfoKHR Nothing
                                      Data.Vector.empty


-- No documentation found for TopLevel "VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME"
pattern KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME = VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_IMAGE_FORMAT_LIST_SPEC_VERSION"
pattern KHR_IMAGE_FORMAT_LIST_SPEC_VERSION :: Integral a => a
pattern KHR_IMAGE_FORMAT_LIST_SPEC_VERSION = VK_KHR_IMAGE_FORMAT_LIST_SPEC_VERSION
