{-# language CPP #-}
-- | = Name
--
-- VK_QCOM_rotated_copy_commands - device extension
--
-- == VK_QCOM_rotated_copy_commands
--
-- [__Name String__]
--     @VK_QCOM_rotated_copy_commands@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     334
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_copy_commands2 VK_KHR_copy_commands2>
--
-- [__Contact__]
--
--     -   Jeff Leger
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_QCOM_rotated_copy_commands] @jackohound%0A*Here describe the issue or question you have about the VK_QCOM_rotated_copy_commands extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-09-18
--
-- [__Interactions and External Dependencies__]
--
--     -   None
--
-- [__Contributors__]
--
--     -   Jeff Leger, Qualcomm Technologies, Inc.
--
-- == Description
--
-- This extension extends adds an optional rotation transform to copy
-- commands 'Vulkan.Extensions.VK_KHR_copy_commands2.cmdBlitImage2KHR',
-- 'Vulkan.Extensions.VK_KHR_copy_commands2.cmdCopyImageToBuffer2KHR' and
-- 'Vulkan.Extensions.VK_KHR_copy_commands2.cmdCopyBufferToImage2KHR'. When
-- copying between two resources, where one resource contains rotated
-- content and the other does not, a rotated copy may be desired. This
-- extension may be used in combination with VK_QCOM_render_pass_transform
-- which adds rotated render passes.
--
-- This extension adds an extension structure to the following commands:
-- vkCmdBlitImage2KHR, vkCmdCopyImageToBuffer2KHR and
-- vkCmdCopyBufferToImage2KHR
--
-- == Issues
--
-- 1) What is an appropriate name for the added extension structure? The
-- style guide says “Structures which extend other structures through the
-- @pNext@ chain should reflect the name of the base structure they
-- extend.”, but in this case a single extension structure is used to
-- extend three base structures (vkCmdBlitImage2KHR,
-- vkCmdCopyImageToBuffer2KHR and vkCmdCopyBufferToImage2KHR). Creating
-- three identical structures with unique names seemed undesirable.
--
-- __RESOLVED__: Deviate from the style guide for extension structure
-- naming.
--
-- 2) Should this extension add a rotation capability to
-- vkCmdCopyImage2KHR?
--
-- __RESOLVED__: No. Use of rotated vkCmdBlitImage2KHR can fully address
-- this use-case.
--
-- 3) Should this extension add a rotation capability to
-- vkCmdResolveImage2KHR?
--
-- __RESOLVED__ No. Use of vkCmdResolveImage2KHR is very slow and extremely
-- bandwidth intensive on Qualcomm’s GPU architecture and use of
-- pResolveAttachments in vkRenderPass is the strongly preferred approach.
-- Therefore, we choose not to introduce a rotation capability to
-- vkCmdResolveImage2KHR.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.BufferImageCopy2',
--     'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.ImageBlit2':
--
--     -   'CopyCommandTransformInfoQCOM'
--
-- == New Enum Constants
--
-- -   'QCOM_ROTATED_COPY_COMMANDS_EXTENSION_NAME'
--
-- -   'QCOM_ROTATED_COPY_COMMANDS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_COMMAND_TRANSFORM_INFO_QCOM'
--
-- == Version History
--
-- -   Revision 1, 2020-09-19 (Jeff Leger)
--
-- == See Also
--
-- 'CopyCommandTransformInfoQCOM'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_QCOM_rotated_copy_commands Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_QCOM_rotated_copy_commands  (CopyCommandTransformInfoQCOM) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data CopyCommandTransformInfoQCOM

instance ToCStruct CopyCommandTransformInfoQCOM
instance Show CopyCommandTransformInfoQCOM

instance FromCStruct CopyCommandTransformInfoQCOM

