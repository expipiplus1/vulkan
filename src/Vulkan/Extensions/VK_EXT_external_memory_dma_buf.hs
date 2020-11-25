{-# language CPP #-}
-- | = Name
--
-- VK_EXT_external_memory_dma_buf - device extension
--
-- == VK_EXT_external_memory_dma_buf
--
-- [__Name String__]
--     @VK_EXT_external_memory_dma_buf@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     126
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_external_memory_fd@
--
-- [__Contact__]
--
--     -   Chad Versace
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_external_memory_dma_buf:%20&body=@chadversary%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-10-10
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Chad Versace, Google
--
--     -   James Jones, NVIDIA
--
--     -   Jason Ekstrand, Intel
--
-- == Description
--
-- A @dma_buf@ is a type of file descriptor, defined by the Linux kernel,
-- that allows sharing memory across kernel device drivers and across
-- processes. This extension enables applications to import a @dma_buf@ as
-- 'Vulkan.Core10.Handles.DeviceMemory', to export
-- 'Vulkan.Core10.Handles.DeviceMemory' as a @dma_buf@, and to create
-- 'Vulkan.Core10.Handles.Buffer' objects that /can/ be bound to that
-- memory.
--
-- == New Enum Constants
--
-- -   'EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME'
--
-- -   'EXT_EXTERNAL_MEMORY_DMA_BUF_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits':
--
--     -   'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT'
--
-- == Issues
--
-- 1) How does the application, when creating a
-- 'Vulkan.Core10.Handles.Image' that it intends to bind to @dma_buf@
-- 'Vulkan.Core10.Handles.DeviceMemory' containing an externally produced
-- image, specify the memory layout (such as row pitch and DRM format
-- modifier) of the 'Vulkan.Core10.Handles.Image'? In other words, how does
-- the application achieve behavior comparable to that provided by
-- <https://www.khronos.org/registry/EGL/extensions/EXT/EGL_EXT_image_dma_buf_import.txt EGL_EXT_image_dma_buf_import>
-- and
-- <https://www.khronos.org/registry/EGL/extensions/EXT/EGL_EXT_image_dma_buf_import_modifiers.txt EGL_EXT_image_dma_buf_import_modifiers>
-- ?
--
-- __RESOLVED__: Features comparable to those in
-- <https://www.khronos.org/registry/EGL/extensions/EXT/EGL_EXT_image_dma_buf_import.txt EGL_EXT_image_dma_buf_import>
-- and
-- <https://www.khronos.org/registry/EGL/extensions/EXT/EGL_EXT_image_dma_buf_import_modifiers.txt EGL_EXT_image_dma_buf_import_modifiers>
-- will be provided by an extension layered atop this one.
--
-- 2) Without the ability to specify the memory layout of external
-- @dma_buf@ images, how is this extension useful?
--
-- __RESOLVED__: This extension provides exactly one new feature: the
-- ability to import\/export between @dma_buf@ and
-- 'Vulkan.Core10.Handles.DeviceMemory'. This feature, together with
-- features provided by @VK_KHR_external_memory_fd@, is sufficient to bind
-- a 'Vulkan.Core10.Handles.Buffer' to @dma_buf@.
--
-- == Version History
--
-- -   Revision 1, 2017-10-10 (Chad Versace)
--
--     -   Squashed internal revisions
--
-- = See Also
--
-- No cross-references are available
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_external_memory_dma_buf Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_external_memory_dma_buf  ( EXT_EXTERNAL_MEMORY_DMA_BUF_SPEC_VERSION
                                                         , pattern EXT_EXTERNAL_MEMORY_DMA_BUF_SPEC_VERSION
                                                         , EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME
                                                         , pattern EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME
                                                         ) where

import Data.String (IsString)

type EXT_EXTERNAL_MEMORY_DMA_BUF_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_EXTERNAL_MEMORY_DMA_BUF_SPEC_VERSION"
pattern EXT_EXTERNAL_MEMORY_DMA_BUF_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_EXTERNAL_MEMORY_DMA_BUF_SPEC_VERSION = 1


type EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME = "VK_EXT_external_memory_dma_buf"

-- No documentation found for TopLevel "VK_EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME"
pattern EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME = "VK_EXT_external_memory_dma_buf"

