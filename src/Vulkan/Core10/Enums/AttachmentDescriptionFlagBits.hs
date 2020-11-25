{-# language CPP #-}
-- No documentation found for Chapter "AttachmentDescriptionFlagBits"
module Vulkan.Core10.Enums.AttachmentDescriptionFlagBits  ( AttachmentDescriptionFlags
                                                          , AttachmentDescriptionFlagBits( ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT
                                                                                         , ..
                                                                                         )
                                                          ) where

import Data.Foldable (asum)
import GHC.Base ((<$))
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import Numeric (showHex)
import Text.ParserCombinators.ReadP (skipSpaces)
import Text.ParserCombinators.ReadP (string)
import Text.ParserCombinators.ReadPrec ((+++))
import qualified Text.ParserCombinators.ReadPrec (lift)
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
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
  showsPrec p e = case lookup e showTableAttachmentDescriptionFlagBits of
    Just s -> showString enumPrefixAttachmentDescriptionFlagBits . showString s
    Nothing ->
      let AttachmentDescriptionFlagBits x = e
      in  showParen (p >= 11) (showString conNameAttachmentDescriptionFlagBits . showString " 0x" . showHex x)

instance Read AttachmentDescriptionFlagBits where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixAttachmentDescriptionFlagBits
          asum ((\(e, s) -> e <$ string s) <$> showTableAttachmentDescriptionFlagBits)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameAttachmentDescriptionFlagBits)
            v <- step readPrec
            pure (AttachmentDescriptionFlagBits v)
          )
    )

