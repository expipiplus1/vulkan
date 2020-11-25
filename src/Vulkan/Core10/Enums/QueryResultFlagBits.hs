{-# language CPP #-}
-- No documentation found for Chapter "QueryResultFlagBits"
module Vulkan.Core10.Enums.QueryResultFlagBits  ( QueryResultFlags
                                                , QueryResultFlagBits( QUERY_RESULT_64_BIT
                                                                     , QUERY_RESULT_WAIT_BIT
                                                                     , QUERY_RESULT_WITH_AVAILABILITY_BIT
                                                                     , QUERY_RESULT_PARTIAL_BIT
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
type QueryResultFlags = QueryResultFlagBits

-- | VkQueryResultFlagBits - Bitmask specifying how and when query results
-- are returned
--
-- = See Also
--
-- 'QueryResultFlags'
newtype QueryResultFlagBits = QueryResultFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'QUERY_RESULT_64_BIT' specifies the results will be written as an array
-- of 64-bit unsigned integer values. If this bit is not set, the results
-- will be written as an array of 32-bit unsigned integer values.
pattern QUERY_RESULT_64_BIT                = QueryResultFlagBits 0x00000001
-- | 'QUERY_RESULT_WAIT_BIT' specifies that Vulkan will wait for each query’s
-- status to become available before retrieving its results.
pattern QUERY_RESULT_WAIT_BIT              = QueryResultFlagBits 0x00000002
-- | 'QUERY_RESULT_WITH_AVAILABILITY_BIT' specifies that the availability
-- status accompanies the results.
pattern QUERY_RESULT_WITH_AVAILABILITY_BIT = QueryResultFlagBits 0x00000004
-- | 'QUERY_RESULT_PARTIAL_BIT' specifies that returning partial results is
-- acceptable.
pattern QUERY_RESULT_PARTIAL_BIT           = QueryResultFlagBits 0x00000008

conNameQueryResultFlagBits :: String
conNameQueryResultFlagBits = "QueryResultFlagBits"

enumPrefixQueryResultFlagBits :: String
enumPrefixQueryResultFlagBits = "QUERY_RESULT_"

showTableQueryResultFlagBits :: [(QueryResultFlagBits, String)]
showTableQueryResultFlagBits =
  [ (QUERY_RESULT_64_BIT               , "64_BIT")
  , (QUERY_RESULT_WAIT_BIT             , "WAIT_BIT")
  , (QUERY_RESULT_WITH_AVAILABILITY_BIT, "WITH_AVAILABILITY_BIT")
  , (QUERY_RESULT_PARTIAL_BIT          , "PARTIAL_BIT")
  ]

instance Show QueryResultFlagBits where
  showsPrec p e = case lookup e showTableQueryResultFlagBits of
    Just s -> showString enumPrefixQueryResultFlagBits . showString s
    Nothing ->
      let QueryResultFlagBits x = e
      in  showParen (p >= 11) (showString conNameQueryResultFlagBits . showString " 0x" . showHex x)

instance Read QueryResultFlagBits where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixQueryResultFlagBits
          asum ((\(e, s) -> e <$ string s) <$> showTableQueryResultFlagBits)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameQueryResultFlagBits)
            v <- step readPrec
            pure (QueryResultFlagBits v)
          )
    )

