{-# language CPP #-}
module Vulkan.Core10.Enums.InstanceCreateFlags  (InstanceCreateFlags(..)) where

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



instance Show InstanceCreateFlags where
  showsPrec p = \case
    InstanceCreateFlags x -> showParen (p >= 11) (showString "InstanceCreateFlags 0x" . showHex x)

instance Read InstanceCreateFlags where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "InstanceCreateFlags")
                       v <- step readPrec
                       pure (InstanceCreateFlags v)))

