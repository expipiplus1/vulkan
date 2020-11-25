{-# language CPP #-}
-- No documentation found for Chapter "AttachmentLoadOp"
module Vulkan.Core10.Enums.AttachmentLoadOp  (AttachmentLoadOp( ATTACHMENT_LOAD_OP_LOAD
                                                              , ATTACHMENT_LOAD_OP_CLEAR
                                                              , ATTACHMENT_LOAD_OP_DONT_CARE
                                                              , ..
                                                              )) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Zero (Zero)
-- No documentation found for TopLevel "VkAttachmentLoadOp"
newtype AttachmentLoadOp = AttachmentLoadOp Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkAttachmentLoadOp" "VK_ATTACHMENT_LOAD_OP_LOAD"
pattern ATTACHMENT_LOAD_OP_LOAD      = AttachmentLoadOp 0
-- No documentation found for Nested "VkAttachmentLoadOp" "VK_ATTACHMENT_LOAD_OP_CLEAR"
pattern ATTACHMENT_LOAD_OP_CLEAR     = AttachmentLoadOp 1
-- No documentation found for Nested "VkAttachmentLoadOp" "VK_ATTACHMENT_LOAD_OP_DONT_CARE"
pattern ATTACHMENT_LOAD_OP_DONT_CARE = AttachmentLoadOp 2
{-# complete ATTACHMENT_LOAD_OP_LOAD,
             ATTACHMENT_LOAD_OP_CLEAR,
             ATTACHMENT_LOAD_OP_DONT_CARE :: AttachmentLoadOp #-}

conNameAttachmentLoadOp :: String
conNameAttachmentLoadOp = "AttachmentLoadOp"

enumPrefixAttachmentLoadOp :: String
enumPrefixAttachmentLoadOp = "ATTACHMENT_LOAD_OP_"

showTableAttachmentLoadOp :: [(AttachmentLoadOp, String)]
showTableAttachmentLoadOp =
  [(ATTACHMENT_LOAD_OP_LOAD, "LOAD"), (ATTACHMENT_LOAD_OP_CLEAR, "CLEAR"), (ATTACHMENT_LOAD_OP_DONT_CARE, "DONT_CARE")]


instance Show AttachmentLoadOp where
showsPrec = enumShowsPrec enumPrefixAttachmentLoadOp
                          showTableAttachmentLoadOp
                          conNameAttachmentLoadOp
                          (\(AttachmentLoadOp x) -> x)
                          (showsPrec 11)


instance Read AttachmentLoadOp where
  readPrec = enumReadPrec enumPrefixAttachmentLoadOp showTableAttachmentLoadOp conNameAttachmentLoadOp AttachmentLoadOp

