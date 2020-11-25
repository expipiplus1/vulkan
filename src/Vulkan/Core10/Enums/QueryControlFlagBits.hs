{-# language CPP #-}
-- No documentation found for Chapter "QueryControlFlagBits"
module Vulkan.Core10.Enums.QueryControlFlagBits  ( QueryControlFlagBits( QUERY_CONTROL_PRECISE_BIT
                                                                       , ..
                                                                       )
                                                 , QueryControlFlags
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

type QueryControlFlags = QueryControlFlagBits

instance Show QueryControlFlagBits where
  showsPrec p = \case
    QUERY_CONTROL_PRECISE_BIT -> showString "QUERY_CONTROL_PRECISE_BIT"
    QueryControlFlagBits x -> showParen (p >= 11) (showString "QueryControlFlagBits 0x" . showHex x)

instance Read QueryControlFlagBits where
  readPrec = parens (choose [("QUERY_CONTROL_PRECISE_BIT", pure QUERY_CONTROL_PRECISE_BIT)]
                     +++
                     prec 10 (do
                       expectP (Ident "QueryControlFlagBits")
                       v <- step readPrec
                       pure (QueryControlFlagBits v)))

