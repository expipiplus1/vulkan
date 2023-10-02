{-# language CPP #-}
-- | = Name
--
-- VK_EXT_load_store_op_none - device extension
--
-- == VK_EXT_load_store_op_none
--
-- [__Name String__]
--     @VK_EXT_load_store_op_none@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     401
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__; __Contact__]
--
--     -   Shahbaz Youssefi
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_load_store_op_none] @syoussefi%0A*Here describe the issue or question you have about the VK_EXT_load_store_op_none extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-06-06
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
-- This extension incorporates 'ATTACHMENT_STORE_OP_NONE_EXT' from
-- @VK_QCOM_render_pass_store_ops@, enabling applications to avoid
-- unnecessary synchronization when an attachment is not written during a
-- render pass.
--
-- Additionally,
-- 'Vulkan.Core10.Enums.AttachmentLoadOp.ATTACHMENT_LOAD_OP_NONE_EXT' is
-- introduced to avoid unnecessary synchronization when an attachment is
-- not used during a render pass at all. In combination with
-- 'ATTACHMENT_STORE_OP_NONE_EXT', this is useful as an alternative to
-- preserve attachments in applications that cannot decide if an attachment
-- will be used in a render pass until after the necessary pipelines have
-- been created.
--
-- == New Enum Constants
--
-- -   'EXT_LOAD_STORE_OP_NONE_EXTENSION_NAME'
--
-- -   'EXT_LOAD_STORE_OP_NONE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.AttachmentLoadOp.AttachmentLoadOp':
--
--     -   'Vulkan.Core10.Enums.AttachmentLoadOp.ATTACHMENT_LOAD_OP_NONE_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.AttachmentStoreOp.AttachmentStoreOp':
--
--     -   'ATTACHMENT_STORE_OP_NONE_EXT'
--
-- == Version History
--
-- -   Revision 1, 2021-06-06 (Shahbaz Youssefi)
--
--     -   Initial revision, based on VK_QCOM_render_pass_store_ops.
--
--     -   Added VK_ATTACHMENT_LOAD_OP_NONE_EXT.
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_load_store_op_none Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_load_store_op_none  ( pattern ATTACHMENT_STORE_OP_NONE_EXT
                                                    , EXT_LOAD_STORE_OP_NONE_SPEC_VERSION
                                                    , pattern EXT_LOAD_STORE_OP_NONE_SPEC_VERSION
                                                    , EXT_LOAD_STORE_OP_NONE_EXTENSION_NAME
                                                    , pattern EXT_LOAD_STORE_OP_NONE_EXTENSION_NAME
                                                    ) where

import Data.String (IsString)
import Vulkan.Core10.Enums.AttachmentStoreOp (AttachmentStoreOp(ATTACHMENT_STORE_OP_NONE))
-- No documentation found for TopLevel "VK_ATTACHMENT_STORE_OP_NONE_EXT"
pattern ATTACHMENT_STORE_OP_NONE_EXT = ATTACHMENT_STORE_OP_NONE


type EXT_LOAD_STORE_OP_NONE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_LOAD_STORE_OP_NONE_SPEC_VERSION"
pattern EXT_LOAD_STORE_OP_NONE_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_LOAD_STORE_OP_NONE_SPEC_VERSION = 1


type EXT_LOAD_STORE_OP_NONE_EXTENSION_NAME = "VK_EXT_load_store_op_none"

-- No documentation found for TopLevel "VK_EXT_LOAD_STORE_OP_NONE_EXTENSION_NAME"
pattern EXT_LOAD_STORE_OP_NONE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_LOAD_STORE_OP_NONE_EXTENSION_NAME = "VK_EXT_load_store_op_none"

