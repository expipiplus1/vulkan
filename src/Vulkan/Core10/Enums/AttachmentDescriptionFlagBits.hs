{-# language CPP #-}
-- No documentation found for Chapter "AttachmentDescriptionFlagBits"
module Vulkan.Core10.Enums.AttachmentDescriptionFlagBits  ( AttachmentDescriptionFlags
                                                          , AttachmentDescriptionFlagBits( ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT
                                                                                         , ..
                                                                                         )
                                                          ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
type AttachmentDescriptionFlags = AttachmentDescriptionFlagBits

-- | VkAttachmentDescriptionFlagBits - Bitmask specifying additional
-- properties of an attachment
--
-- = See Also
--
-- 'AttachmentDescriptionFlags'
newtype AttachmentDescriptionFlagBits = AttachmentDescriptionFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT' specifies that the attachment
-- aliases the same device memory as other attachments.
pattern ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT = AttachmentDescriptionFlagBits 0x00000001

conNameAttachmentDescriptionFlagBits :: String
conNameAttachmentDescriptionFlagBits = "AttachmentDescriptionFlagBits"

enumPrefixAttachmentDescriptionFlagBits :: String
enumPrefixAttachmentDescriptionFlagBits = "ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT"

showTableAttachmentDescriptionFlagBits :: [(AttachmentDescriptionFlagBits, String)]
showTableAttachmentDescriptionFlagBits = [(ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT, "")]

instance Show AttachmentDescriptionFlagBits where
  showsPrec = enumShowsPrec enumPrefixAttachmentDescriptionFlagBits
                            showTableAttachmentDescriptionFlagBits
                            conNameAttachmentDescriptionFlagBits
                            (\(AttachmentDescriptionFlagBits x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read AttachmentDescriptionFlagBits where
  readPrec = enumReadPrec enumPrefixAttachmentDescriptionFlagBits
                          showTableAttachmentDescriptionFlagBits
                          conNameAttachmentDescriptionFlagBits
                          AttachmentDescriptionFlagBits

