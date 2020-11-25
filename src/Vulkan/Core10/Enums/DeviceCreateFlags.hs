{-# language CPP #-}
-- No documentation found for Chapter "DeviceCreateFlags"
module Vulkan.Core10.Enums.DeviceCreateFlags  (DeviceCreateFlags(..)) where

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
-- | VkDeviceCreateFlags - Reserved for future use
--
-- = Description
--
-- 'DeviceCreateFlags' is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'Vulkan.Core10.Device.DeviceCreateInfo'
newtype DeviceCreateFlags = DeviceCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameDeviceCreateFlags :: String
conNameDeviceCreateFlags = "DeviceCreateFlags"

enumPrefixDeviceCreateFlags :: String
enumPrefixDeviceCreateFlags = ""

showTableDeviceCreateFlags :: [(DeviceCreateFlags, String)]
showTableDeviceCreateFlags = []

instance Show DeviceCreateFlags where
  showsPrec p e = case lookup e showTableDeviceCreateFlags of
    Just s -> showString enumPrefixDeviceCreateFlags . showString s
    Nothing ->
      let DeviceCreateFlags x = e
      in  showParen (p >= 11) (showString conNameDeviceCreateFlags . showString " 0x" . showHex x)

instance Read DeviceCreateFlags where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixDeviceCreateFlags
          asum ((\(e, s) -> e <$ string s) <$> showTableDeviceCreateFlags)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameDeviceCreateFlags)
            v <- step readPrec
            pure (DeviceCreateFlags v)
          )
    )

