{-# language CPP #-}
-- | = Name
--
-- VK_KHR_copy_commands2 - device extension
--
-- == VK_KHR_copy_commands2
--
-- [__Name String__]
--     @VK_KHR_copy_commands2@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     338
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3-promotions Vulkan 1.3>
--
-- [__Contact__]
--
--     -   Jeff Leger
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_copy_commands2] @jackohound%0A*Here describe the issue or question you have about the VK_KHR_copy_commands2 extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-07-06
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.3 Core
--
-- [__Interactions and External Dependencies__]
--
--     -   None
--
-- [__Contributors__]
--
--     -   Jeff Leger, Qualcomm
--
--     -   Tobias Hector, AMD
--
--     -   Jan-Harald Fredriksen, ARM
--
--     -   Tom Olson, ARM
--
-- == Description
--
-- This extension provides extensible versions of the Vulkan buffer and
-- image copy commands. The new commands are functionally identical to the
-- core commands, except that their copy parameters are specified using
-- extensible structures that can be used to pass extension-specific
-- information.
--
-- The following extensible copy commands are introduced with this
-- extension: 'cmdCopyBuffer2KHR', 'cmdCopyImage2KHR',
-- 'cmdCopyBufferToImage2KHR', 'cmdCopyImageToBuffer2KHR',
-- 'cmdBlitImage2KHR', and 'cmdResolveImage2KHR'. Each command contains an
-- @*Info2KHR@ structure parameter that includes @sType@\/@pNext@ members.
-- Lower level structures describing each region to be copied are also
-- extended with @sType@\/@pNext@ members.
--
-- == New Commands
--
-- -   'cmdBlitImage2KHR'
--
-- -   'cmdCopyBuffer2KHR'
--
-- -   'cmdCopyBufferToImage2KHR'
--
-- -   'cmdCopyImage2KHR'
--
-- -   'cmdCopyImageToBuffer2KHR'
--
-- -   'cmdResolveImage2KHR'
--
-- == New Structures
--
-- -   'BlitImageInfo2KHR'
--
-- -   'BufferCopy2KHR'
--
-- -   'BufferImageCopy2KHR'
--
-- -   'CopyBufferInfo2KHR'
--
-- -   'CopyBufferToImageInfo2KHR'
--
-- -   'CopyImageInfo2KHR'
--
-- -   'CopyImageToBufferInfo2KHR'
--
-- -   'ImageBlit2KHR'
--
-- -   'ImageCopy2KHR'
--
-- -   'ImageResolve2KHR'
--
-- -   'ResolveImageInfo2KHR'
--
-- == New Enum Constants
--
-- -   'KHR_COPY_COMMANDS_2_EXTENSION_NAME'
--
-- -   'KHR_COPY_COMMANDS_2_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_BLIT_IMAGE_INFO_2_KHR'
--
--     -   'STRUCTURE_TYPE_BUFFER_COPY_2_KHR'
--
--     -   'STRUCTURE_TYPE_BUFFER_IMAGE_COPY_2_KHR'
--
--     -   'STRUCTURE_TYPE_COPY_BUFFER_INFO_2_KHR'
--
--     -   'STRUCTURE_TYPE_COPY_BUFFER_TO_IMAGE_INFO_2_KHR'
--
--     -   'STRUCTURE_TYPE_COPY_IMAGE_INFO_2_KHR'
--
--     -   'STRUCTURE_TYPE_COPY_IMAGE_TO_BUFFER_INFO_2_KHR'
--
--     -   'STRUCTURE_TYPE_IMAGE_BLIT_2_KHR'
--
--     -   'STRUCTURE_TYPE_IMAGE_COPY_2_KHR'
--
--     -   'STRUCTURE_TYPE_IMAGE_RESOLVE_2_KHR'
--
--     -   'STRUCTURE_TYPE_RESOLVE_IMAGE_INFO_2_KHR'
--
-- == Promotion to Vulkan 1.3
--
-- Functionality in this extension is included in core Vulkan 1.3, with the
-- KHR suffix omitted. The original type, enum and command names are still
-- available as aliases of the core functionality.
--
-- == Version History
--
-- -   Revision 1, 2020-07-06 (Jeff Leger)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'BlitImageInfo2KHR', 'BufferCopy2KHR', 'BufferImageCopy2KHR',
-- 'CopyBufferInfo2KHR', 'CopyBufferToImageInfo2KHR', 'CopyImageInfo2KHR',
-- 'CopyImageToBufferInfo2KHR', 'ImageBlit2KHR', 'ImageCopy2KHR',
-- 'ImageResolve2KHR', 'ResolveImageInfo2KHR', 'cmdBlitImage2KHR',
-- 'cmdCopyBuffer2KHR', 'cmdCopyBufferToImage2KHR', 'cmdCopyImage2KHR',
-- 'cmdCopyImageToBuffer2KHR', 'cmdResolveImage2KHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_copy_commands2 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_copy_commands2  ( pattern STRUCTURE_TYPE_COPY_BUFFER_INFO_2_KHR
                                                , pattern STRUCTURE_TYPE_COPY_IMAGE_INFO_2_KHR
                                                , pattern STRUCTURE_TYPE_COPY_BUFFER_TO_IMAGE_INFO_2_KHR
                                                , pattern STRUCTURE_TYPE_COPY_IMAGE_TO_BUFFER_INFO_2_KHR
                                                , pattern STRUCTURE_TYPE_BLIT_IMAGE_INFO_2_KHR
                                                , pattern STRUCTURE_TYPE_RESOLVE_IMAGE_INFO_2_KHR
                                                , pattern STRUCTURE_TYPE_BUFFER_COPY_2_KHR
                                                , pattern STRUCTURE_TYPE_IMAGE_COPY_2_KHR
                                                , pattern STRUCTURE_TYPE_IMAGE_BLIT_2_KHR
                                                , pattern STRUCTURE_TYPE_BUFFER_IMAGE_COPY_2_KHR
                                                , pattern STRUCTURE_TYPE_IMAGE_RESOLVE_2_KHR
                                                , cmdCopyBuffer2KHR
                                                , cmdCopyImage2KHR
                                                , cmdBlitImage2KHR
                                                , cmdCopyBufferToImage2KHR
                                                , cmdCopyImageToBuffer2KHR
                                                , cmdResolveImage2KHR
                                                , BufferCopy2KHR
                                                , ImageCopy2KHR
                                                , ImageBlit2KHR
                                                , BufferImageCopy2KHR
                                                , ImageResolve2KHR
                                                , CopyBufferInfo2KHR
                                                , CopyImageInfo2KHR
                                                , BlitImageInfo2KHR
                                                , CopyBufferToImageInfo2KHR
                                                , CopyImageToBufferInfo2KHR
                                                , ResolveImageInfo2KHR
                                                , KHR_COPY_COMMANDS_2_SPEC_VERSION
                                                , pattern KHR_COPY_COMMANDS_2_SPEC_VERSION
                                                , KHR_COPY_COMMANDS_2_EXTENSION_NAME
                                                , pattern KHR_COPY_COMMANDS_2_EXTENSION_NAME
                                                ) where

import Data.String (IsString)
import Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2 (cmdBlitImage2)
import Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2 (cmdCopyBuffer2)
import Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2 (cmdCopyBufferToImage2)
import Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2 (cmdCopyImage2)
import Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2 (cmdCopyImageToBuffer2)
import Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2 (cmdResolveImage2)
import Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2 (BlitImageInfo2)
import Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2 (BufferCopy2)
import Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2 (BufferImageCopy2)
import Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2 (CopyBufferInfo2)
import Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2 (CopyBufferToImageInfo2)
import Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2 (CopyImageInfo2)
import Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2 (CopyImageToBufferInfo2)
import Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2 (ImageBlit2)
import Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2 (ImageCopy2)
import Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2 (ImageResolve2)
import Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2 (ResolveImageInfo2)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BLIT_IMAGE_INFO_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_COPY_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_IMAGE_COPY_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_BUFFER_INFO_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_BUFFER_TO_IMAGE_INFO_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_IMAGE_INFO_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_IMAGE_TO_BUFFER_INFO_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_BLIT_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_COPY_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_RESOLVE_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RESOLVE_IMAGE_INFO_2))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_COPY_BUFFER_INFO_2_KHR"
pattern STRUCTURE_TYPE_COPY_BUFFER_INFO_2_KHR = STRUCTURE_TYPE_COPY_BUFFER_INFO_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_COPY_IMAGE_INFO_2_KHR"
pattern STRUCTURE_TYPE_COPY_IMAGE_INFO_2_KHR = STRUCTURE_TYPE_COPY_IMAGE_INFO_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_COPY_BUFFER_TO_IMAGE_INFO_2_KHR"
pattern STRUCTURE_TYPE_COPY_BUFFER_TO_IMAGE_INFO_2_KHR = STRUCTURE_TYPE_COPY_BUFFER_TO_IMAGE_INFO_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_COPY_IMAGE_TO_BUFFER_INFO_2_KHR"
pattern STRUCTURE_TYPE_COPY_IMAGE_TO_BUFFER_INFO_2_KHR = STRUCTURE_TYPE_COPY_IMAGE_TO_BUFFER_INFO_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_BLIT_IMAGE_INFO_2_KHR"
pattern STRUCTURE_TYPE_BLIT_IMAGE_INFO_2_KHR = STRUCTURE_TYPE_BLIT_IMAGE_INFO_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_RESOLVE_IMAGE_INFO_2_KHR"
pattern STRUCTURE_TYPE_RESOLVE_IMAGE_INFO_2_KHR = STRUCTURE_TYPE_RESOLVE_IMAGE_INFO_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_BUFFER_COPY_2_KHR"
pattern STRUCTURE_TYPE_BUFFER_COPY_2_KHR = STRUCTURE_TYPE_BUFFER_COPY_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_IMAGE_COPY_2_KHR"
pattern STRUCTURE_TYPE_IMAGE_COPY_2_KHR = STRUCTURE_TYPE_IMAGE_COPY_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_IMAGE_BLIT_2_KHR"
pattern STRUCTURE_TYPE_IMAGE_BLIT_2_KHR = STRUCTURE_TYPE_IMAGE_BLIT_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_BUFFER_IMAGE_COPY_2_KHR"
pattern STRUCTURE_TYPE_BUFFER_IMAGE_COPY_2_KHR = STRUCTURE_TYPE_BUFFER_IMAGE_COPY_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_IMAGE_RESOLVE_2_KHR"
pattern STRUCTURE_TYPE_IMAGE_RESOLVE_2_KHR = STRUCTURE_TYPE_IMAGE_RESOLVE_2


-- No documentation found for TopLevel "vkCmdCopyBuffer2KHR"
cmdCopyBuffer2KHR = cmdCopyBuffer2


-- No documentation found for TopLevel "vkCmdCopyImage2KHR"
cmdCopyImage2KHR = cmdCopyImage2


-- No documentation found for TopLevel "vkCmdBlitImage2KHR"
cmdBlitImage2KHR = cmdBlitImage2


-- No documentation found for TopLevel "vkCmdCopyBufferToImage2KHR"
cmdCopyBufferToImage2KHR = cmdCopyBufferToImage2


-- No documentation found for TopLevel "vkCmdCopyImageToBuffer2KHR"
cmdCopyImageToBuffer2KHR = cmdCopyImageToBuffer2


-- No documentation found for TopLevel "vkCmdResolveImage2KHR"
cmdResolveImage2KHR = cmdResolveImage2


-- No documentation found for TopLevel "VkBufferCopy2KHR"
type BufferCopy2KHR = BufferCopy2


-- No documentation found for TopLevel "VkImageCopy2KHR"
type ImageCopy2KHR = ImageCopy2


-- No documentation found for TopLevel "VkImageBlit2KHR"
type ImageBlit2KHR = ImageBlit2


-- No documentation found for TopLevel "VkBufferImageCopy2KHR"
type BufferImageCopy2KHR = BufferImageCopy2


-- No documentation found for TopLevel "VkImageResolve2KHR"
type ImageResolve2KHR = ImageResolve2


-- No documentation found for TopLevel "VkCopyBufferInfo2KHR"
type CopyBufferInfo2KHR = CopyBufferInfo2


-- No documentation found for TopLevel "VkCopyImageInfo2KHR"
type CopyImageInfo2KHR = CopyImageInfo2


-- No documentation found for TopLevel "VkBlitImageInfo2KHR"
type BlitImageInfo2KHR = BlitImageInfo2


-- No documentation found for TopLevel "VkCopyBufferToImageInfo2KHR"
type CopyBufferToImageInfo2KHR = CopyBufferToImageInfo2


-- No documentation found for TopLevel "VkCopyImageToBufferInfo2KHR"
type CopyImageToBufferInfo2KHR = CopyImageToBufferInfo2


-- No documentation found for TopLevel "VkResolveImageInfo2KHR"
type ResolveImageInfo2KHR = ResolveImageInfo2


type KHR_COPY_COMMANDS_2_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_COPY_COMMANDS_2_SPEC_VERSION"
pattern KHR_COPY_COMMANDS_2_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_COPY_COMMANDS_2_SPEC_VERSION = 1


type KHR_COPY_COMMANDS_2_EXTENSION_NAME = "VK_KHR_copy_commands2"

-- No documentation found for TopLevel "VK_KHR_COPY_COMMANDS_2_EXTENSION_NAME"
pattern KHR_COPY_COMMANDS_2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_COPY_COMMANDS_2_EXTENSION_NAME = "VK_KHR_copy_commands2"

