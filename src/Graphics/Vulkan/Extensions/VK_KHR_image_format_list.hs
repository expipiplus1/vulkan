{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_image_format_list
  ( withCStructImageFormatListCreateInfoKHR
  , fromCStructImageFormatListCreateInfoKHR
  , ImageFormatListCreateInfoKHR(..)
  , pattern VK_KHR_IMAGE_FORMAT_LIST_SPEC_VERSION
  , pattern VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR
  ) where

import Data.Function
  ( (&)
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( generateM
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


import Graphics.Vulkan.C.Extensions.VK_KHR_image_format_list
  ( VkImageFormatListCreateInfoKHR(..)
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
import Graphics.Vulkan.C.Extensions.VK_KHR_image_format_list
  ( pattern VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME
  , pattern VK_KHR_IMAGE_FORMAT_LIST_SPEC_VERSION
  )


-- No documentation found for TopLevel "ImageFormatListCreateInfoKHR"
data ImageFormatListCreateInfoKHR = ImageFormatListCreateInfoKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "ImageFormatListCreateInfoKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "ImageFormatListCreateInfoKHR" "pViewFormats"
  vkPViewFormats :: Vector Format
  }
  deriving (Show, Eq)
withCStructImageFormatListCreateInfoKHR :: ImageFormatListCreateInfoKHR -> (VkImageFormatListCreateInfoKHR -> IO a) -> IO a
withCStructImageFormatListCreateInfoKHR from cont = withVec (&) (vkPViewFormats (from :: ImageFormatListCreateInfoKHR)) (\pViewFormats -> maybeWith withSomeVkStruct (vkPNext (from :: ImageFormatListCreateInfoKHR)) (\pPNext -> cont (VkImageFormatListCreateInfoKHR VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR pPNext (fromIntegral (Data.Vector.length (vkPViewFormats (from :: ImageFormatListCreateInfoKHR)))) pViewFormats)))
fromCStructImageFormatListCreateInfoKHR :: VkImageFormatListCreateInfoKHR -> IO ImageFormatListCreateInfoKHR
fromCStructImageFormatListCreateInfoKHR c = ImageFormatListCreateInfoKHR <$> -- Univalued Member elided
                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImageFormatListCreateInfoKHR)))
                                                                         -- Length valued member elided
                                                                         <*> (Data.Vector.generateM (fromIntegral (vkViewFormatCount (c :: VkImageFormatListCreateInfoKHR))) (peekElemOff (vkPViewFormats (c :: VkImageFormatListCreateInfoKHR))))
