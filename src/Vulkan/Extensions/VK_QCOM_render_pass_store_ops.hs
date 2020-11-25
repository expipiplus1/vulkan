{-# language CPP #-}
-- | = Name
--
-- VK_QCOM_render_pass_store_ops - device extension
--
-- == VK_QCOM_render_pass_store_ops
--
-- [__Name String__]
--     @VK_QCOM_render_pass_store_ops@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     302
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Contact__]
--
--     -   Bill Licea-Kane
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_QCOM_render_pass_store_ops:%20&body=@wwlk%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-03-25
--
-- [__Contributors__]
--
--     -   Bill Licea-Kane, Qualcomm Technologies, Inc.
--
-- == Description
--
-- Renderpass attachments /can/ be read-only for the duration of a
-- renderpass.
--
-- Examples include input attachments and depth attachments where depth
-- tests are enabled but depth writes are not enabled.
--
-- In such cases, there /can/ be no contents generated for an attachment
-- within the render area.
--
-- This extension adds a new
-- 'Vulkan.Core10.Enums.AttachmentStoreOp.AttachmentStoreOp'
-- 'Vulkan.Core10.Enums.AttachmentStoreOp.ATTACHMENT_STORE_OP_NONE_QCOM'
-- which specifies that the contents within the render area /may/ not be
-- written to memory, but that the prior contents of the attachment in
-- memory are preserved. However, if any contents were generated within the
-- render area during rendering, the contents of the attachment will be
-- undefined inside the render area.
--
-- Note
--
-- The 'Vulkan.Core10.Enums.AttachmentStoreOp.AttachmentStoreOp'
-- 'Vulkan.Core10.Enums.AttachmentStoreOp.ATTACHMENT_STORE_OP_STORE' /may/
-- force an implementation to assume that the attachment was written and
-- force an implementation to flush data to memory or to a higher level
-- cache. The 'Vulkan.Core10.Enums.AttachmentStoreOp.AttachmentStoreOp'
-- 'Vulkan.Core10.Enums.AttachmentStoreOp.ATTACHMENT_STORE_OP_NONE_QCOM'
-- /may/ allow an implementation to assume that the attachment was not
-- written and allow an implementation to avoid such a flush..
--
-- == New Enum Constants
--
-- -   'QCOM_render_pass_store_ops_EXTENSION_NAME'
--
-- -   'QCOM_render_pass_store_ops_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.AttachmentStoreOp.AttachmentStoreOp':
--
--     -   'Vulkan.Core10.Enums.AttachmentStoreOp.ATTACHMENT_STORE_OP_NONE_QCOM'
--
-- == Version History
--
-- -   Revision 1, 2019-12-20 (wwlk)
--
--     -   Initial version
--
-- -   Revision 2, 2020-03-25 (wwlk)
--
--     -   Minor renaming
--
-- = See Also
--
-- No cross-references are available
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_render_pass_store_ops Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_QCOM_render_pass_store_ops  ( QCOM_render_pass_store_ops_SPEC_VERSION
                                                        , pattern QCOM_render_pass_store_ops_SPEC_VERSION
                                                        , QCOM_render_pass_store_ops_EXTENSION_NAME
                                                        , pattern QCOM_render_pass_store_ops_EXTENSION_NAME
                                                        ) where

import Data.String (IsString)

type QCOM_render_pass_store_ops_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_QCOM_render_pass_store_ops_SPEC_VERSION"
pattern QCOM_render_pass_store_ops_SPEC_VERSION :: forall a . Integral a => a
pattern QCOM_render_pass_store_ops_SPEC_VERSION = 2


type QCOM_render_pass_store_ops_EXTENSION_NAME = "VK_QCOM_render_pass_store_ops"

-- No documentation found for TopLevel "VK_QCOM_render_pass_store_ops_EXTENSION_NAME"
pattern QCOM_render_pass_store_ops_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern QCOM_render_pass_store_ops_EXTENSION_NAME = "VK_QCOM_render_pass_store_ops"

