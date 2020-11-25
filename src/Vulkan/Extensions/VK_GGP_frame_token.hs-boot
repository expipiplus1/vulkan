{-# language CPP #-}
-- | = Name
--
-- VK_GGP_frame_token - device extension
--
-- = Registered Extension Number
--
-- 192
--
-- = Revision
--
-- 1
--
-- = Extension and Version Dependencies
--
-- -   Requires Vulkan 1.0
--
-- -   Requires @VK_KHR_swapchain@
--
-- -   Requires @VK_GGP_stream_descriptor_surface@
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-01-28
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Jean-Francois Roy, Google
--
--     -   Richard O’Grady, Google
--
-- == Description
--
-- This extension allows an application that uses the @VK_KHR_swapchain@
-- extension in combination with a Google Games Platform surface provided
-- by the @VK_GGP_stream_descriptor_surface@ extension to associate a
-- Google Games Platform frame token with a present operation.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR':
--
--     -   'PresentFrameTokenGGP'
--
-- == New Enum Constants
--
-- -   'GGP_FRAME_TOKEN_EXTENSION_NAME'
--
-- -   'GGP_FRAME_TOKEN_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP'
--
-- == Version History
--
-- -   Revision 1, 2018-11-26 (Jean-Francois Roy)
--
--     -   Initial revision.
--
-- = See Also
--
-- 'PresentFrameTokenGGP'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_GGP_frame_token Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_GGP_frame_token  (PresentFrameTokenGGP) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PresentFrameTokenGGP

instance ToCStruct PresentFrameTokenGGP
instance Show PresentFrameTokenGGP

instance FromCStruct PresentFrameTokenGGP

