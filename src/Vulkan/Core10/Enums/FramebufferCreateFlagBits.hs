{-# language CPP #-}
-- No documentation found for Chapter "FramebufferCreateFlagBits"
module Vulkan.Core10.Enums.FramebufferCreateFlagBits  ( FramebufferCreateFlags
                                                      , FramebufferCreateFlagBits( FRAMEBUFFER_CREATE_IMAGELESS_BIT
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
type FramebufferCreateFlags = FramebufferCreateFlagBits

-- | VkFramebufferCreateFlagBits - Bitmask specifying framebuffer properties
--
-- = See Also
--
-- 'FramebufferCreateFlags'
newtype FramebufferCreateFlagBits = FramebufferCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'FRAMEBUFFER_CREATE_IMAGELESS_BIT' specifies that image views are not
-- specified, and only attachment compatibility information will be
-- provided via a
-- 'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.FramebufferAttachmentImageInfo'
-- structure.
pattern FRAMEBUFFER_CREATE_IMAGELESS_BIT = FramebufferCreateFlagBits 0x00000001

conNameFramebufferCreateFlagBits :: String
conNameFramebufferCreateFlagBits = "FramebufferCreateFlagBits"

enumPrefixFramebufferCreateFlagBits :: String
enumPrefixFramebufferCreateFlagBits = "FRAMEBUFFER_CREATE_IMAGELESS_BIT"

showTableFramebufferCreateFlagBits :: [(FramebufferCreateFlagBits, String)]
showTableFramebufferCreateFlagBits = [(FRAMEBUFFER_CREATE_IMAGELESS_BIT, "")]

instance Show FramebufferCreateFlagBits where
  showsPrec p e = case lookup e showTableFramebufferCreateFlagBits of
    Just s -> showString enumPrefixFramebufferCreateFlagBits . showString s
    Nothing ->
      let FramebufferCreateFlagBits x = e
      in  showParen (p >= 11) (showString conNameFramebufferCreateFlagBits . showString " 0x" . showHex x)

instance Read FramebufferCreateFlagBits where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixFramebufferCreateFlagBits
          asum ((\(e, s) -> e <$ string s) <$> showTableFramebufferCreateFlagBits)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameFramebufferCreateFlagBits)
            v <- step readPrec
            pure (FramebufferCreateFlagBits v)
          )
    )

