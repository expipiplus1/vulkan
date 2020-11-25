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
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Zero (Zero)
-- No documentation found for TopLevel "VkAttachmentStoreOp"
newtype AttachmentStoreOp = AttachmentStoreOp Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkAttachmentStoreOp" "VK_ATTACHMENT_STORE_OP_STORE"
pattern ATTACHMENT_STORE_OP_STORE     = AttachmentStoreOp 0
-- No documentation found for Nested "VkAttachmentStoreOp" "VK_ATTACHMENT_STORE_OP_DONT_CARE"
pattern ATTACHMENT_STORE_OP_DONT_CARE = AttachmentStoreOp 1
-- No documentation found for Nested "VkAttachmentStoreOp" "VK_ATTACHMENT_STORE_OP_NONE_QCOM"
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

