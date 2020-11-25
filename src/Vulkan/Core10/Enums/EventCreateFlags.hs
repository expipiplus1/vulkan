{-# language CPP #-}
-- No documentation found for Chapter "EventCreateFlags"
module Vulkan.Core10.Enums.EventCreateFlags  (EventCreateFlags(..)) where

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
-- | VkEventCreateFlags - Reserved for future use
--
-- = Description
--
-- 'EventCreateFlags' is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'Vulkan.Core10.Event.EventCreateInfo'
newtype EventCreateFlags = EventCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameEventCreateFlags :: String
conNameEventCreateFlags = "EventCreateFlags"

enumPrefixEventCreateFlags :: String
enumPrefixEventCreateFlags = ""

showTableEventCreateFlags :: [(EventCreateFlags, String)]
showTableEventCreateFlags = []

instance Show EventCreateFlags where
  showsPrec p e = case lookup e showTableEventCreateFlags of
    Just s -> showString enumPrefixEventCreateFlags . showString s
    Nothing ->
      let EventCreateFlags x = e
      in  showParen (p >= 11) (showString conNameEventCreateFlags . showString " 0x" . showHex x)

instance Read EventCreateFlags where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixEventCreateFlags
          asum ((\(e, s) -> e <$ string s) <$> showTableEventCreateFlags)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameEventCreateFlags)
            v <- step readPrec
            pure (EventCreateFlags v)
          )
    )

