{-# language CPP #-}
-- | = Name
--
-- VK_KHR_load_store_op_none - device extension
--
-- = VK_KHR_load_store_op_none
--
-- [__Name String__]
--     @VK_KHR_load_store_op_none@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     527
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     None
--
-- [__Contact__]
--
--     -   Shahbaz Youssefi
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_load_store_op_none] @syoussefi%0A*Here describe the issue or question you have about the VK_KHR_load_store_op_none extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_load_store_op_none.adoc VK_KHR_load_store_op_none>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-05-16
--
-- [__Contributors__]
--
--     -   Shahbaz Youssefi, Google
--
--     -   Bill Licea-Kane, Qualcomm Technologies, Inc.
--
--     -   Tobias Hector, AMD
--
-- == Description
--
-- This extension provides
-- 'Vulkan.Core10.Enums.AttachmentLoadOp.ATTACHMENT_LOAD_OP_NONE_KHR' and
-- 'Vulkan.Extensions.VK_KHR_dynamic_rendering.ATTACHMENT_STORE_OP_NONE_KHR',
-- which are identically promoted from the @VK_EXT_load_store_op_none@
-- extension.
--
-- == New Enum Constants
--
-- -   'KHR_LOAD_STORE_OP_NONE_EXTENSION_NAME'
--
-- -   'KHR_LOAD_STORE_OP_NONE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.AttachmentLoadOp.AttachmentLoadOp':
--
--     -   'Vulkan.Core10.Enums.AttachmentLoadOp.ATTACHMENT_LOAD_OP_NONE_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.AttachmentStoreOp.AttachmentStoreOp':
--
--     -   'Vulkan.Extensions.VK_KHR_dynamic_rendering.ATTACHMENT_STORE_OP_NONE_KHR'
--
-- While 'Vulkan.Core10.Enums.AttachmentStoreOp.ATTACHMENT_STORE_OP_NONE'
-- is part of Vulkan 1.3, this extension was not promoted to core either in
-- whole or in part. This functionality was promoted from
-- @VK_KHR_dynamic_rendering@.
--
-- == Version History
--
-- -   Revision 1, 2023-05-16 (Shahbaz Youssefi)
--
--     -   Initial revision, based on VK_EXT_load_store_op_none.
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_load_store_op_none Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_load_store_op_none  ( KHR_LOAD_STORE_OP_NONE_SPEC_VERSION
                                                    , pattern KHR_LOAD_STORE_OP_NONE_SPEC_VERSION
                                                    , KHR_LOAD_STORE_OP_NONE_EXTENSION_NAME
                                                    , pattern KHR_LOAD_STORE_OP_NONE_EXTENSION_NAME
                                                    , pattern ATTACHMENT_STORE_OP_NONE_KHR
                                                    ) where

import Data.String (IsString)
import Vulkan.Extensions.VK_KHR_dynamic_rendering (pattern ATTACHMENT_STORE_OP_NONE_KHR)
type KHR_LOAD_STORE_OP_NONE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_LOAD_STORE_OP_NONE_SPEC_VERSION"
pattern KHR_LOAD_STORE_OP_NONE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_LOAD_STORE_OP_NONE_SPEC_VERSION = 1


type KHR_LOAD_STORE_OP_NONE_EXTENSION_NAME = "VK_KHR_load_store_op_none"

-- No documentation found for TopLevel "VK_KHR_LOAD_STORE_OP_NONE_EXTENSION_NAME"
pattern KHR_LOAD_STORE_OP_NONE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_LOAD_STORE_OP_NONE_EXTENSION_NAME = "VK_KHR_load_store_op_none"

