{-# language CPP #-}
-- No documentation found for Chapter "BufferViewCreateFlags"
module Vulkan.Core10.Enums.BufferViewCreateFlags  (BufferViewCreateFlags(..)) where

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
-- | VkBufferViewCreateFlags - Reserved for future use
--
-- = Description
--
-- 'BufferViewCreateFlags' is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'Vulkan.Core10.BufferView.BufferViewCreateInfo'
newtype BufferViewCreateFlags = BufferViewCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameBufferViewCreateFlags :: String
conNameBufferViewCreateFlags = "BufferViewCreateFlags"

enumPrefixBufferViewCreateFlags :: String
enumPrefixBufferViewCreateFlags = ""

showTableBufferViewCreateFlags :: [(BufferViewCreateFlags, String)]
showTableBufferViewCreateFlags = []

instance Show BufferViewCreateFlags where
  showsPrec p e = case lookup e showTableBufferViewCreateFlags of
    Just s -> showString enumPrefixBufferViewCreateFlags . showString s
    Nothing ->
      let BufferViewCreateFlags x = e
      in  showParen (p >= 11) (showString conNameBufferViewCreateFlags . showString " 0x" . showHex x)

instance Read BufferViewCreateFlags where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixBufferViewCreateFlags
          asum ((\(e, s) -> e <$ string s) <$> showTableBufferViewCreateFlags)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameBufferViewCreateFlags)
            v <- step readPrec
            pure (BufferViewCreateFlags v)
          )
    )

