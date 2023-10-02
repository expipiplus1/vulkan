{-# language CPP #-}
-- | = Name
--
-- VK_GOOGLE_surfaceless_query - instance extension
--
-- == VK_GOOGLE_surfaceless_query
--
-- [__Name String__]
--     @VK_GOOGLE_surfaceless_query@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     434
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface VK_KHR_surface>
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse OpenGL \/ ES support>
--
-- [__Contact__]
--
--     -   Shahbaz Youssefi
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_GOOGLE_surfaceless_query] @syoussefi%0A*Here describe the issue or question you have about the VK_GOOGLE_surfaceless_query extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_GOOGLE_surfaceless_query.adoc VK_GOOGLE_surfaceless_query>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-08-03
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Ian Elliott, Google
--
--     -   Shahbaz Youssefi, Google
--
--     -   James Jones, NVIDIA
--
-- == Description
--
-- This extension allows the
-- 'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceFormatsKHR'
-- and
-- 'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfacePresentModesKHR'
-- functions to accept 'Vulkan.Core10.APIConstants.NULL_HANDLE' as their
-- @surface@ parameter, allowing potential surface formats, colorspaces and
-- present modes to be queried without providing a surface. Identically,
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.getPhysicalDeviceSurfaceFormats2KHR',
-- 'Vulkan.Extensions.VK_EXT_full_screen_exclusive.getPhysicalDeviceSurfacePresentModes2EXT',
-- and
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.getPhysicalDeviceSurfaceCapabilities2KHR'
-- would accept 'Vulkan.Core10.APIConstants.NULL_HANDLE' in
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.PhysicalDeviceSurfaceInfo2KHR'::@surface@.
-- __This can only be supported on platforms where the results of these
-- queries are surface-agnostic and a single presentation engine is the
-- implicit target of all present operations__.
--
-- == New Enum Constants
--
-- -   'GOOGLE_SURFACELESS_QUERY_EXTENSION_NAME'
--
-- -   'GOOGLE_SURFACELESS_QUERY_SPEC_VERSION'
--
-- == Version History
--
-- -   Revision 1, 2021-12-14 (Shahbaz Youssefi)
--
--     -   Internal revisions
--
-- -   Revision 2, 2022-08-03 (Shahbaz Youssefi)
--
--     -   Precisions to which parts of the query responses are defined
--         when surfaceless
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_GOOGLE_surfaceless_query Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_GOOGLE_surfaceless_query  ( GOOGLE_SURFACELESS_QUERY_SPEC_VERSION
                                                      , pattern GOOGLE_SURFACELESS_QUERY_SPEC_VERSION
                                                      , GOOGLE_SURFACELESS_QUERY_EXTENSION_NAME
                                                      , pattern GOOGLE_SURFACELESS_QUERY_EXTENSION_NAME
                                                      ) where

import Data.String (IsString)

type GOOGLE_SURFACELESS_QUERY_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_GOOGLE_SURFACELESS_QUERY_SPEC_VERSION"
pattern GOOGLE_SURFACELESS_QUERY_SPEC_VERSION :: forall a . Integral a => a
pattern GOOGLE_SURFACELESS_QUERY_SPEC_VERSION = 2


type GOOGLE_SURFACELESS_QUERY_EXTENSION_NAME = "VK_GOOGLE_surfaceless_query"

-- No documentation found for TopLevel "VK_GOOGLE_SURFACELESS_QUERY_EXTENSION_NAME"
pattern GOOGLE_SURFACELESS_QUERY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern GOOGLE_SURFACELESS_QUERY_EXTENSION_NAME = "VK_GOOGLE_surfaceless_query"

