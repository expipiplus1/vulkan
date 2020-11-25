{-# language CPP #-}
-- No documentation found for Chapter "QueryResultFlagBits"
module Vulkan.Core10.Enums.QueryResultFlagBits  ( QueryResultFlagBits( QUERY_RESULT_64_BIT
                                                                     , QUERY_RESULT_WAIT_BIT
                                                                     , QUERY_RESULT_WITH_AVAILABILITY_BIT
                                                                     , QUERY_RESULT_PARTIAL_BIT
                                                                     , ..
                                                                     )
                                                , QueryResultFlags
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
pattern QUERY_RESULT_64_BIT = QueryResultFlagBits 0x00000001
-- | 'QUERY_RESULT_WAIT_BIT' specifies that Vulkan will wait for each query’s
-- status to become available before retrieving its results.
pattern QUERY_RESULT_WAIT_BIT = QueryResultFlagBits 0x00000002
-- | 'QUERY_RESULT_WITH_AVAILABILITY_BIT' specifies that the availability
-- status accompanies the results.
pattern QUERY_RESULT_WITH_AVAILABILITY_BIT = QueryResultFlagBits 0x00000004
-- | 'QUERY_RESULT_PARTIAL_BIT' specifies that returning partial results is
-- acceptable.
pattern QUERY_RESULT_PARTIAL_BIT = QueryResultFlagBits 0x00000008

type QueryResultFlags = QueryResultFlagBits

instance Show QueryResultFlagBits where
  showsPrec p = \case
    QUERY_RESULT_64_BIT -> showString "QUERY_RESULT_64_BIT"
    QUERY_RESULT_WAIT_BIT -> showString "QUERY_RESULT_WAIT_BIT"
    QUERY_RESULT_WITH_AVAILABILITY_BIT -> showString "QUERY_RESULT_WITH_AVAILABILITY_BIT"
    QUERY_RESULT_PARTIAL_BIT -> showString "QUERY_RESULT_PARTIAL_BIT"
    QueryResultFlagBits x -> showParen (p >= 11) (showString "QueryResultFlagBits 0x" . showHex x)

instance Read QueryResultFlagBits where
  readPrec = parens (choose [("QUERY_RESULT_64_BIT", pure QUERY_RESULT_64_BIT)
                            , ("QUERY_RESULT_WAIT_BIT", pure QUERY_RESULT_WAIT_BIT)
                            , ("QUERY_RESULT_WITH_AVAILABILITY_BIT", pure QUERY_RESULT_WITH_AVAILABILITY_BIT)
                            , ("QUERY_RESULT_PARTIAL_BIT", pure QUERY_RESULT_PARTIAL_BIT)]
                     +++
                     prec 10 (do
                       expectP (Ident "QueryResultFlagBits")
                       v <- step readPrec
                       pure (QueryResultFlagBits v)))

