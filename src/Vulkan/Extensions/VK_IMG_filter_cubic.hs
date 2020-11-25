{-# language CPP #-}
-- | = Name
--
-- VK_IMG_filter_cubic - device extension
--
-- = Registered Extension Number
--
-- 16
--
-- = Revision
--
-- 1
--
-- = Extension and Version Dependencies
--
-- -   Requires Vulkan 1.0
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2016-02-23
--
-- [__Contributors__]
--
--     -   Tobias Hector, Imagination Technologies
--
-- == Description
--
-- @VK_IMG_filter_cubic@ adds an additional, high quality cubic filtering
-- mode to Vulkan, using a Catmull-Rom bicubic filter. Performing this kind
-- of filtering can be done in a shader by using 16 samples and a number of
-- instructions, but this can be inefficient. The cubic filter mode exposes
-- an optimized high quality texture sampling using fixed texture sampling
-- functionality.
--
-- == New Enum Constants
--
-- -   'IMG_FILTER_CUBIC_EXTENSION_NAME'
--
-- -   'IMG_FILTER_CUBIC_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.Filter.Filter':
--
--     -   'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_IMG'
--
-- -   Extending
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlagBits':
--
--     -   'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG'
--
-- == Example
--
-- Creating a sampler with the new filter for both magnification and
-- minification
--
-- >     VkSamplerCreateInfo createInfo =
-- >     {
-- >         VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO // sType
-- >         // Other members set to application-desired values
-- >     };
-- >
-- >     createInfo.magFilter = VK_FILTER_CUBIC_IMG;
-- >     createInfo.minFilter = VK_FILTER_CUBIC_IMG;
-- >
-- >     VkSampler sampler;
-- >     VkResult result = vkCreateSampler(
-- >         device,
-- >         &createInfo,
-- >         &sampler);
--
-- == Version History
--
-- -   Revision 1, 2016-02-23 (Tobias Hector)
--
--     -   Initial version
--
-- = See Also
--
-- No cross-references are available
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_IMG_filter_cubic Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_IMG_filter_cubic  ( IMG_FILTER_CUBIC_SPEC_VERSION
                                              , pattern IMG_FILTER_CUBIC_SPEC_VERSION
                                              , IMG_FILTER_CUBIC_EXTENSION_NAME
                                              , pattern IMG_FILTER_CUBIC_EXTENSION_NAME
                                              ) where

import Data.String (IsString)

type IMG_FILTER_CUBIC_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_IMG_FILTER_CUBIC_SPEC_VERSION"
pattern IMG_FILTER_CUBIC_SPEC_VERSION :: forall a . Integral a => a
pattern IMG_FILTER_CUBIC_SPEC_VERSION = 1


type IMG_FILTER_CUBIC_EXTENSION_NAME = "VK_IMG_filter_cubic"

-- No documentation found for TopLevel "VK_IMG_FILTER_CUBIC_EXTENSION_NAME"
pattern IMG_FILTER_CUBIC_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern IMG_FILTER_CUBIC_EXTENSION_NAME = "VK_IMG_filter_cubic"

