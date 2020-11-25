{-# language CPP #-}
-- | = Name
--
-- VK_KHR_storage_buffer_storage_class - device extension
--
-- == VK_KHR_storage_buffer_storage_class
--
-- [__Name String__]
--     @VK_KHR_storage_buffer_storage_class@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     132
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1-promotions Vulkan 1.1>
--
-- [__Contact__]
--
--     -   Alexander Galazin
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_KHR_storage_buffer_storage_class:%20&body=@alegal-arm%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-09-05
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_storage_buffer_storage_class.html SPV_KHR_storage_buffer_storage_class>
--
--     -   Promoted to Vulkan 1.1 Core
--
-- [__Contributors__]
--
--     -   Alexander Galazin, ARM
--
--     -   David Neto, Google
--
-- == Description
--
-- This extension adds support for the following SPIR-V extension in
-- Vulkan:
--
-- -   @SPV_KHR_storage_buffer_storage_class@
--
-- This extension provides a new SPIR-V @StorageBuffer@ storage class. A
-- @Block@-decorated object in this class is equivalent to a
-- @BufferBlock@-decorated object in the @Uniform@ storage class.
--
-- == Promotion to Vulkan 1.1
--
-- All functionality in this extension is included in core Vulkan 1.1.
--
-- == New Enum Constants
--
-- -   'KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME'
--
-- -   'KHR_STORAGE_BUFFER_STORAGE_CLASS_SPEC_VERSION'
--
-- == Version History
--
-- -   Revision 1, 2017-03-23 (Alexander Galazin)
--
--     -   Initial draft
--
-- = See Also
--
-- No cross-references are available
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_storage_buffer_storage_class Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_storage_buffer_storage_class  ( KHR_STORAGE_BUFFER_STORAGE_CLASS_SPEC_VERSION
                                                              , pattern KHR_STORAGE_BUFFER_STORAGE_CLASS_SPEC_VERSION
                                                              , KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME
                                                              , pattern KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME
                                                              ) where

import Data.String (IsString)

type KHR_STORAGE_BUFFER_STORAGE_CLASS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_SPEC_VERSION"
pattern KHR_STORAGE_BUFFER_STORAGE_CLASS_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_STORAGE_BUFFER_STORAGE_CLASS_SPEC_VERSION = 1


type KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME = "VK_KHR_storage_buffer_storage_class"

-- No documentation found for TopLevel "VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME"
pattern KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME = "VK_KHR_storage_buffer_storage_class"

