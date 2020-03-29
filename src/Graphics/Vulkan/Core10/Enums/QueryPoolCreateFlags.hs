{-# language CPP #-}
module Graphics.Vulkan.Core10.Enums.QueryPoolCreateFlags  (QueryPoolCreateFlags(..)) where

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
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Graphics.Vulkan.Core10.BaseType (Flags)
import Graphics.Vulkan.Zero (Zero)
-- | VkQueryPoolCreateFlags - Reserved for future use
--
-- = Description
--
-- 'QueryPoolCreateFlags' is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Query.QueryPoolCreateInfo'
newtype QueryPoolCreateFlags = QueryPoolCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)



instance Show QueryPoolCreateFlags where
  showsPrec p = \case
    QueryPoolCreateFlags x -> showParen (p >= 11) (showString "QueryPoolCreateFlags 0x" . showHex x)

instance Read QueryPoolCreateFlags where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "QueryPoolCreateFlags")
                       v <- step readPrec
                       pure (QueryPoolCreateFlags v)))

