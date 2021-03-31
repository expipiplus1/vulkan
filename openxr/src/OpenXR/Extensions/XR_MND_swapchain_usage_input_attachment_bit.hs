{-# language CPP #-}
-- | = Name
--
-- XR_MND_swapchain_usage_input_attachment_bit - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_MND_swapchain_usage_input_attachment_bit  XR_MND_swapchain_usage_input_attachment_bit>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 97
--
-- = Revision
--
-- 2
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- = See Also
--
-- No cross-references are available
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_MND_swapchain_usage_input_attachment_bit OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_MND_swapchain_usage_input_attachment_bit  ( MND_swapchain_usage_input_attachment_bit_SPEC_VERSION
                                                                      , pattern MND_swapchain_usage_input_attachment_bit_SPEC_VERSION
                                                                      , MND_SWAPCHAIN_USAGE_INPUT_ATTACHMENT_BIT_EXTENSION_NAME
                                                                      , pattern MND_SWAPCHAIN_USAGE_INPUT_ATTACHMENT_BIT_EXTENSION_NAME
                                                                      ) where

import Data.String (IsString)

type MND_swapchain_usage_input_attachment_bit_SPEC_VERSION = 2

-- No documentation found for TopLevel "XR_MND_swapchain_usage_input_attachment_bit_SPEC_VERSION"
pattern MND_swapchain_usage_input_attachment_bit_SPEC_VERSION :: forall a . Integral a => a
pattern MND_swapchain_usage_input_attachment_bit_SPEC_VERSION = 2


type MND_SWAPCHAIN_USAGE_INPUT_ATTACHMENT_BIT_EXTENSION_NAME = "XR_MND_swapchain_usage_input_attachment_bit"

-- No documentation found for TopLevel "XR_MND_SWAPCHAIN_USAGE_INPUT_ATTACHMENT_BIT_EXTENSION_NAME"
pattern MND_SWAPCHAIN_USAGE_INPUT_ATTACHMENT_BIT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern MND_SWAPCHAIN_USAGE_INPUT_ATTACHMENT_BIT_EXTENSION_NAME = "XR_MND_swapchain_usage_input_attachment_bit"

