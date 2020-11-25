{-# language CPP #-}
-- No documentation found for Chapter "AttachmentDescriptionFlagBits"
module Vulkan.Core10.Enums.AttachmentDescriptionFlagBits  ( AttachmentDescriptionFlagBits( ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT
                                                                                         , ..
                                                                                         )
                                                          , AttachmentDescriptionFlags
                                                          ) where

import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import Numeric (showHex)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
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

type AttachmentDescriptionFlags = AttachmentDescriptionFlagBits

instance Show AttachmentDescriptionFlagBits where
  showsPrec p = \case
    ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT -> showString "ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT"
    AttachmentDescriptionFlagBits x -> showParen (p >= 11) (showString "AttachmentDescriptionFlagBits 0x" . showHex x)

instance Read AttachmentDescriptionFlagBits where
  readPrec = parens (choose [("ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT", pure ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT)]
                     +++
                     prec 10 (do
                       expectP (Ident "AttachmentDescriptionFlagBits")
                       v <- step readPrec
                       pure (AttachmentDescriptionFlagBits v)))

