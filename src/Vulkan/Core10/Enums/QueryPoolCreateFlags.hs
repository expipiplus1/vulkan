{-# language CPP #-}
-- No documentation found for Chapter "QueryPoolCreateFlags"
module Vulkan.Core10.Enums.QueryPoolCreateFlags  (QueryPoolCreateFlags(..)) where

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
-- | VkQueryPoolCreateFlags - Reserved for future use
--
-- = Description
--
-- 'QueryPoolCreateFlags' is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'Vulkan.Core10.Query.QueryPoolCreateInfo'
newtype QueryPoolCreateFlags = QueryPoolCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameQueryPoolCreateFlags :: String
conNameQueryPoolCreateFlags = "QueryPoolCreateFlags"

enumPrefixQueryPoolCreateFlags :: String
enumPrefixQueryPoolCreateFlags = ""

showTableQueryPoolCreateFlags :: [(QueryPoolCreateFlags, String)]
showTableQueryPoolCreateFlags = []

instance Show QueryPoolCreateFlags where
  showsPrec p e = case lookup e showTableQueryPoolCreateFlags of
    Just s -> showString enumPrefixQueryPoolCreateFlags . showString s
    Nothing ->
      let QueryPoolCreateFlags x = e
      in  showParen (p >= 11) (showString conNameQueryPoolCreateFlags . showString " 0x" . showHex x)

instance Read QueryPoolCreateFlags where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixQueryPoolCreateFlags
          asum ((\(e, s) -> e <$ string s) <$> showTableQueryPoolCreateFlags)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameQueryPoolCreateFlags)
            v <- step readPrec
            pure (QueryPoolCreateFlags v)
          )
    )

