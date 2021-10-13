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
--
--     -   Requires Vulkan 1.0
--
-- [__Contact__]
--
--     -   Jeff Leger
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_copy_commands2] @jackohound%0A<<Here describe the issue or question you have about the VK_KHR_copy_commands2 extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-07-06
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
-- Lower level structures that describe each region to be copied are also
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
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BLIT_IMAGE_INFO_2_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BUFFER_COPY_2_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BUFFER_IMAGE_COPY_2_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_BUFFER_INFO_2_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_BUFFER_TO_IMAGE_INFO_2_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_IMAGE_INFO_2_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_IMAGE_TO_BUFFER_INFO_2_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_BLIT_2_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_COPY_2_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_RESOLVE_2_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RESOLVE_IMAGE_INFO_2_KHR'
--
-- == Version History
--
-- -   Revision 1, 2020-07-06 (Jeff Leger)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'BlitImageInfo2KHR', 'BufferCopy2KHR', 'BufferImageCopy2KHR',
-- 'CopyBufferInfo2KHR', 'CopyBufferToImageInfo2KHR', 'CopyImageInfo2KHR',
-- 'CopyImageToBufferInfo2KHR', 'ImageBlit2KHR', 'ImageCopy2KHR',
-- 'ImageResolve2KHR', 'ResolveImageInfo2KHR', 'cmdBlitImage2KHR',
-- 'cmdCopyBuffer2KHR', 'cmdCopyBufferToImage2KHR', 'cmdCopyImage2KHR',
-- 'cmdCopyImageToBuffer2KHR', 'cmdResolveImage2KHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_copy_commands2 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_copy_commands2  ( BlitImageInfo2KHR
                                                , BufferCopy2KHR
                                                , BufferImageCopy2KHR
                                                , CopyBufferInfo2KHR
                                                , CopyBufferToImageInfo2KHR
                                                , CopyImageInfo2KHR
                                                , CopyImageToBufferInfo2KHR
                                                , ImageBlit2KHR
                                                , ImageCopy2KHR
                                                , ImageResolve2KHR
                                                , ResolveImageInfo2KHR
                                                ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
data BlitImageInfo2KHR

instance ToCStruct BlitImageInfo2KHR
instance Show BlitImageInfo2KHR

instance FromCStruct BlitImageInfo2KHR


data BufferCopy2KHR

instance ToCStruct BufferCopy2KHR
instance Show BufferCopy2KHR

instance FromCStruct BufferCopy2KHR


type role BufferImageCopy2KHR nominal
data BufferImageCopy2KHR (es :: [Type])

instance (Extendss BufferImageCopy2KHR es, PokeChain es) => ToCStruct (BufferImageCopy2KHR es)
instance Show (Chain es) => Show (BufferImageCopy2KHR es)

instance (Extendss BufferImageCopy2KHR es, PeekChain es) => FromCStruct (BufferImageCopy2KHR es)


data CopyBufferInfo2KHR

instance ToCStruct CopyBufferInfo2KHR
instance Show CopyBufferInfo2KHR

instance FromCStruct CopyBufferInfo2KHR


data CopyBufferToImageInfo2KHR

instance ToCStruct CopyBufferToImageInfo2KHR
instance Show CopyBufferToImageInfo2KHR

instance FromCStruct CopyBufferToImageInfo2KHR


data CopyImageInfo2KHR

instance ToCStruct CopyImageInfo2KHR
instance Show CopyImageInfo2KHR

instance FromCStruct CopyImageInfo2KHR


data CopyImageToBufferInfo2KHR

instance ToCStruct CopyImageToBufferInfo2KHR
instance Show CopyImageToBufferInfo2KHR

instance FromCStruct CopyImageToBufferInfo2KHR


type role ImageBlit2KHR nominal
data ImageBlit2KHR (es :: [Type])

instance (Extendss ImageBlit2KHR es, PokeChain es) => ToCStruct (ImageBlit2KHR es)
instance Show (Chain es) => Show (ImageBlit2KHR es)

instance (Extendss ImageBlit2KHR es, PeekChain es) => FromCStruct (ImageBlit2KHR es)


data ImageCopy2KHR

instance ToCStruct ImageCopy2KHR
instance Show ImageCopy2KHR

instance FromCStruct ImageCopy2KHR


data ImageResolve2KHR

instance ToCStruct ImageResolve2KHR
instance Show ImageResolve2KHR

instance FromCStruct ImageResolve2KHR


data ResolveImageInfo2KHR

instance ToCStruct ResolveImageInfo2KHR
instance Show ResolveImageInfo2KHR

instance FromCStruct ResolveImageInfo2KHR

