{-# language CPP #-}
module Graphics.Vulkan.Core10.Enums.DeviceCreateFlags  (DeviceCreateFlags(..)) where

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
-- | VkDeviceCreateFlags - Reserved for future use
--
-- = Description
--
-- 'DeviceCreateFlags' is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Device.DeviceCreateInfo'
newtype DeviceCreateFlags = DeviceCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)



instance Show DeviceCreateFlags where
  showsPrec p = \case
    DeviceCreateFlags x -> showParen (p >= 11) (showString "DeviceCreateFlags 0x" . showHex x)

instance Read DeviceCreateFlags where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "DeviceCreateFlags")
                       v <- step readPrec
                       pure (DeviceCreateFlags v)))

