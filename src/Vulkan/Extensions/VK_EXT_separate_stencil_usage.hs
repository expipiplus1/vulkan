{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_separate_stencil_usage"
module Vulkan.Extensions.VK_EXT_separate_stencil_usage  ( pattern STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO_EXT
                                                        , ImageStencilUsageCreateInfoEXT
                                                        , EXT_SEPARATE_STENCIL_USAGE_SPEC_VERSION
                                                        , pattern EXT_SEPARATE_STENCIL_USAGE_SPEC_VERSION
                                                        , EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME
                                                        , pattern EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME
                                                        ) where

import Data.String (IsString)
import Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage (ImageStencilUsageCreateInfo)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO_EXT = STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO


-- No documentation found for TopLevel "VkImageStencilUsageCreateInfoEXT"
type ImageStencilUsageCreateInfoEXT = ImageStencilUsageCreateInfo


type EXT_SEPARATE_STENCIL_USAGE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_SEPARATE_STENCIL_USAGE_SPEC_VERSION"
pattern EXT_SEPARATE_STENCIL_USAGE_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_SEPARATE_STENCIL_USAGE_SPEC_VERSION = 1


type EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME = "VK_EXT_separate_stencil_usage"

-- No documentation found for TopLevel "VK_EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME"
pattern EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME = "VK_EXT_separate_stencil_usage"

