{-# language CPP #-}
module Graphics.Vulkan.Core10.Enums.EventCreateFlags  (EventCreateFlags(..)) where

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
-- | VkEventCreateFlags - Reserved for future use
--
-- = Description
--
-- 'EventCreateFlags' is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Event.EventCreateInfo'
newtype EventCreateFlags = EventCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)



instance Show EventCreateFlags where
  showsPrec p = \case
    EventCreateFlags x -> showParen (p >= 11) (showString "EventCreateFlags 0x" . showHex x)

instance Read EventCreateFlags where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "EventCreateFlags")
                       v <- step readPrec
                       pure (EventCreateFlags v)))

