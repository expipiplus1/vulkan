{-# language CPP #-}
-- No documentation found for Chapter "InstanceCreateFlags"
module Vulkan.Core10.Enums.InstanceCreateFlags  (InstanceCreateFlags(..)) where

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
-- | VkInstanceCreateFlags - Reserved for future use
--
-- = Description
--
-- 'InstanceCreateFlags' is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'Vulkan.Core10.DeviceInitialization.InstanceCreateInfo'
newtype InstanceCreateFlags = InstanceCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameInstanceCreateFlags :: String
conNameInstanceCreateFlags = "InstanceCreateFlags"

enumPrefixInstanceCreateFlags :: String
enumPrefixInstanceCreateFlags = ""

showTableInstanceCreateFlags :: [(InstanceCreateFlags, String)]
showTableInstanceCreateFlags = []

instance Show InstanceCreateFlags where
  showsPrec p e = case lookup e showTableInstanceCreateFlags of
    Just s -> showString enumPrefixInstanceCreateFlags . showString s
    Nothing ->
      let InstanceCreateFlags x = e
      in  showParen (p >= 11) (showString conNameInstanceCreateFlags . showString " 0x" . showHex x)

instance Read InstanceCreateFlags where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixInstanceCreateFlags
          asum ((\(e, s) -> e <$ string s) <$> showTableInstanceCreateFlags)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameInstanceCreateFlags)
            v <- step readPrec
            pure (InstanceCreateFlags v)
          )
    )

