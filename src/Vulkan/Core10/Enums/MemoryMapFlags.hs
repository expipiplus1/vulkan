{-# language CPP #-}
-- No documentation found for Chapter "MemoryMapFlags"
module Vulkan.Core10.Enums.MemoryMapFlags  (MemoryMapFlags(..)) where

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
-- | VkMemoryMapFlags - Reserved for future use
--
-- = Description
--
-- 'MemoryMapFlags' is a bitmask type for setting a mask, but is currently
-- reserved for future use.
--
-- = See Also
--
-- 'Vulkan.Core10.Memory.mapMemory'
newtype MemoryMapFlags = MemoryMapFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameMemoryMapFlags :: String
conNameMemoryMapFlags = "MemoryMapFlags"

enumPrefixMemoryMapFlags :: String
enumPrefixMemoryMapFlags = ""

showTableMemoryMapFlags :: [(MemoryMapFlags, String)]
showTableMemoryMapFlags = []

instance Show MemoryMapFlags where
  showsPrec p e = case lookup e showTableMemoryMapFlags of
    Just s -> showString enumPrefixMemoryMapFlags . showString s
    Nothing ->
      let MemoryMapFlags x = e in showParen (p >= 11) (showString conNameMemoryMapFlags . showString " 0x" . showHex x)

instance Read MemoryMapFlags where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixMemoryMapFlags
          asum ((\(e, s) -> e <$ string s) <$> showTableMemoryMapFlags)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameMemoryMapFlags)
            v <- step readPrec
            pure (MemoryMapFlags v)
          )
    )

