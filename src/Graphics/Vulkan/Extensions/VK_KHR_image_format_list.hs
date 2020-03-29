{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_KHR_image_format_list  ( pattern STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR
                                                            , ImageFormatListCreateInfoKHR
                                                            , KHR_IMAGE_FORMAT_LIST_SPEC_VERSION
                                                            , pattern KHR_IMAGE_FORMAT_LIST_SPEC_VERSION
                                                            , KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME
                                                            , pattern KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME
                                                            ) where

import Data.String (IsString)
import Graphics.Vulkan.Core12.Promoted_From_VK_KHR_image_format_list (ImageFormatListCreateInfo)
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR = STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO


-- No documentation found for TopLevel "VkImageFormatListCreateInfoKHR"
type ImageFormatListCreateInfoKHR = ImageFormatListCreateInfo


type KHR_IMAGE_FORMAT_LIST_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_IMAGE_FORMAT_LIST_SPEC_VERSION"
pattern KHR_IMAGE_FORMAT_LIST_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_IMAGE_FORMAT_LIST_SPEC_VERSION = 1


type KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME = "VK_KHR_image_format_list"

-- No documentation found for TopLevel "VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME"
pattern KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME = "VK_KHR_image_format_list"

