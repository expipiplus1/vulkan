{-# language CPP #-}
-- No documentation found for Chapter "AttachmentStoreOp"
module Vulkan.Core10.Enums.AttachmentStoreOp  (AttachmentStoreOp( ATTACHMENT_STORE_OP_STORE
                                                                , ATTACHMENT_STORE_OP_DONT_CARE
                                                                , ATTACHMENT_STORE_OP_NONE_QCOM
                                                                , ..
                                                                )) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))

-- | VkAttachmentStoreOp - Specify how contents of an attachment are treated
-- at the end of a subpass
--
-- = Description
--
-- Note
--
-- 'ATTACHMENT_STORE_OP_DONT_CARE' /can/ cause contents generated during
-- previous render passes to be discarded before reaching memory, even if
-- no write to the attachment occurs during the current render pass.
--
-- = See Also
--
-- 'Vulkan.Core10.Pass.AttachmentDescription',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.AttachmentDescription2'
newtype AttachmentStoreOp = AttachmentStoreOp Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'ATTACHMENT_STORE_OP_STORE' specifies the contents generated during the
-- render pass and within the render area are written to memory. For
-- attachments with a depth\/stencil format, this uses the access type
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT'.
-- For attachments with a color format, this uses the access type
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_COLOR_ATTACHMENT_WRITE_BIT'.
pattern ATTACHMENT_STORE_OP_STORE     = AttachmentStoreOp 0
-- | 'ATTACHMENT_STORE_OP_DONT_CARE' specifies the contents within the render
-- area are not needed after rendering, and /may/ be discarded; the
-- contents of the attachment will be undefined inside the render area. For
-- attachments with a depth\/stencil format, this uses the access type
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT'.
-- For attachments with a color format, this uses the access type
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_COLOR_ATTACHMENT_WRITE_BIT'.
pattern ATTACHMENT_STORE_OP_DONT_CARE = AttachmentStoreOp 1
-- | 'ATTACHMENT_STORE_OP_NONE_QCOM' specifies that the contents within the
-- render area were not written during rendering, and /may/ not be written
-- to memory. If the attachment was written to during the renderpass, the
-- contents of the attachment will be undefined inside the render area.
pattern ATTACHMENT_STORE_OP_NONE_QCOM = AttachmentStoreOp 1000301000
{-# complete ATTACHMENT_STORE_OP_STORE,
             ATTACHMENT_STORE_OP_DONT_CARE,
             ATTACHMENT_STORE_OP_NONE_QCOM :: AttachmentStoreOp #-}

conNameAttachmentStoreOp :: String
conNameAttachmentStoreOp = "AttachmentStoreOp"

enumPrefixAttachmentStoreOp :: String
enumPrefixAttachmentStoreOp = "ATTACHMENT_STORE_OP_"

showTableAttachmentStoreOp :: [(AttachmentStoreOp, String)]
showTableAttachmentStoreOp =
  [ (ATTACHMENT_STORE_OP_STORE    , "STORE")
  , (ATTACHMENT_STORE_OP_DONT_CARE, "DONT_CARE")
  , (ATTACHMENT_STORE_OP_NONE_QCOM, "NONE_QCOM")
  ]

instance Show AttachmentStoreOp where
  showsPrec = enumShowsPrec enumPrefixAttachmentStoreOp
                            showTableAttachmentStoreOp
                            conNameAttachmentStoreOp
                            (\(AttachmentStoreOp x) -> x)
                            (showsPrec 11)

instance Read AttachmentStoreOp where
  readPrec =
    enumReadPrec enumPrefixAttachmentStoreOp showTableAttachmentStoreOp conNameAttachmentStoreOp AttachmentStoreOp

