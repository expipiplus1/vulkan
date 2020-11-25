{-# language CPP #-}
-- No documentation found for Chapter "QueryControlFlagBits"
module Vulkan.Core10.Enums.QueryControlFlagBits  ( QueryControlFlags
                                                 , QueryControlFlagBits( QUERY_CONTROL_PRECISE_BIT
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
type QueryControlFlags = QueryControlFlagBits

-- | VkQueryControlFlagBits - Bitmask specifying constraints on a query
--
-- = See Also
--
-- 'QueryControlFlags'
newtype QueryControlFlagBits = QueryControlFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'QUERY_CONTROL_PRECISE_BIT' specifies the precision of
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-occlusion occlusion queries>.
pattern QUERY_CONTROL_PRECISE_BIT = QueryControlFlagBits 0x00000001

conNameQueryControlFlagBits :: String
conNameQueryControlFlagBits = "QueryControlFlagBits"

enumPrefixQueryControlFlagBits :: String
enumPrefixQueryControlFlagBits = "QUERY_CONTROL_PRECISE_BIT"

showTableQueryControlFlagBits :: [(QueryControlFlagBits, String)]
showTableQueryControlFlagBits = [(QUERY_CONTROL_PRECISE_BIT, "")]

instance Show QueryControlFlagBits where
  showsPrec p e = case lookup e showTableQueryControlFlagBits of
    Just s -> showString enumPrefixQueryControlFlagBits . showString s
    Nothing ->
      let QueryControlFlagBits x = e
      in  showParen (p >= 11) (showString conNameQueryControlFlagBits . showString " 0x" . showHex x)

instance Read QueryControlFlagBits where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixQueryControlFlagBits
          asum ((\(e, s) -> e <$ string s) <$> showTableQueryControlFlagBits)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameQueryControlFlagBits)
            v <- step readPrec
            pure (QueryControlFlagBits v)
          )
    )

