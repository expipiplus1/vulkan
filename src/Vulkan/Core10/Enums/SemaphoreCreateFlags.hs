{-# language CPP #-}
-- No documentation found for Chapter "SemaphoreCreateFlags"
module Vulkan.Core10.Enums.SemaphoreCreateFlags  (SemaphoreCreateFlags(..)) where

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
-- | VkSemaphoreCreateFlags - Reserved for future use
--
-- = Description
--
-- 'SemaphoreCreateFlags' is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'Vulkan.Core10.QueueSemaphore.SemaphoreCreateInfo'
newtype SemaphoreCreateFlags = SemaphoreCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameSemaphoreCreateFlags :: String
conNameSemaphoreCreateFlags = "SemaphoreCreateFlags"

enumPrefixSemaphoreCreateFlags :: String
enumPrefixSemaphoreCreateFlags = ""

showTableSemaphoreCreateFlags :: [(SemaphoreCreateFlags, String)]
showTableSemaphoreCreateFlags = []

instance Show SemaphoreCreateFlags where
  showsPrec p e = case lookup e showTableSemaphoreCreateFlags of
    Just s -> showString enumPrefixSemaphoreCreateFlags . showString s
    Nothing ->
      let SemaphoreCreateFlags x = e
      in  showParen (p >= 11) (showString conNameSemaphoreCreateFlags . showString " 0x" . showHex x)

instance Read SemaphoreCreateFlags where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixSemaphoreCreateFlags
          asum ((\(e, s) -> e <$ string s) <$> showTableSemaphoreCreateFlags)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameSemaphoreCreateFlags)
            v <- step readPrec
            pure (SemaphoreCreateFlags v)
          )
    )

