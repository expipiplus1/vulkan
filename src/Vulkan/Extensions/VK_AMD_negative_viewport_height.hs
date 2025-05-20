{-# language CPP #-}
-- | = Name
--
-- VK_AMD_negative_viewport_height - device extension
--
-- == VK_AMD_negative_viewport_height
--
-- [__Name String__]
--     @VK_AMD_negative_viewport_height@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     36
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     None
--
-- [__Deprecation State__]
--
--     -   /Obsoleted/ by
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance1 VK_KHR_maintenance1>
--         extension
--
--         -   Which in turn was /promoted/ to
--             <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1-promotions Vulkan 1.1>
--
-- [__Contact__]
--
--     -   Matthaeus G. Chajdas
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_AMD_negative_viewport_height] @anteru%0A*Here describe the issue or question you have about the VK_AMD_negative_viewport_height extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2016-09-02
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Matthaeus G. Chajdas, AMD
--
--     -   Graham Sellers, AMD
--
--     -   Baldur Karlsson
--
-- == Description
--
-- This extension allows an application to specify a negative viewport
-- height. The result is that the viewport transformation will flip along
-- the y-axis.
--
-- -   Allow negative height to be specified in the
--     'Vulkan.Core10.Pipeline.Viewport'::@height@ field to perform
--     y-inversion of the clip-space to framebuffer-space transform. This
--     allows apps to avoid having to use @gl_Position.y = -gl_Position.y@
--     in shaders also targeting other APIs.
--
-- == Obsoletion by @VK_KHR_maintenance1@ and Vulkan 1.1
--
-- Functionality in this extension is included in @VK_KHR_maintenance1@ and
-- subsequently Vulkan 1.1. Due to some slight behavioral differences, this
-- extension /must/ not be enabled alongside @VK_KHR_maintenance1@, or in
-- an instance created with version 1.1 or later requested in
-- 'Vulkan.Core10.DeviceInitialization.ApplicationInfo'::@apiVersion@.
--
-- == New Enum Constants
--
-- -   'AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME'
--
-- -   'AMD_NEGATIVE_VIEWPORT_HEIGHT_SPEC_VERSION'
--
-- == Version History
--
-- -   Revision 1, 2016-09-02 (Matthaeus Chajdas)
--
--     -   Initial draft
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_AMD_negative_viewport_height Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_AMD_negative_viewport_height  ( AMD_NEGATIVE_VIEWPORT_HEIGHT_SPEC_VERSION
                                                          , pattern AMD_NEGATIVE_VIEWPORT_HEIGHT_SPEC_VERSION
                                                          , AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME
                                                          , pattern AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME
                                                          ) where

import Data.String (IsString)

type AMD_NEGATIVE_VIEWPORT_HEIGHT_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_SPEC_VERSION"
pattern AMD_NEGATIVE_VIEWPORT_HEIGHT_SPEC_VERSION :: forall a . Integral a => a
pattern AMD_NEGATIVE_VIEWPORT_HEIGHT_SPEC_VERSION = 1


type AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME = "VK_AMD_negative_viewport_height"

-- No documentation found for TopLevel "VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME"
pattern AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME = "VK_AMD_negative_viewport_height"

