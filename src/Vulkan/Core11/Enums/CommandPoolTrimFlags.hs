{-# language CPP #-}
module Vulkan.Core11.Enums.CommandPoolTrimFlags  (CommandPoolTrimFlags(..)) where

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
import Vulkan.Core10.BaseType (Flags)
import Vulkan.Zero (Zero)
-- | VkCommandPoolTrimFlags - Reserved for future use
--
-- = Description
--
-- 'CommandPoolTrimFlags' is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'Vulkan.Core11.Promoted_From_VK_KHR_maintenance1.trimCommandPool',
-- 'Vulkan.Extensions.VK_KHR_maintenance1.trimCommandPoolKHR'
newtype CommandPoolTrimFlags = CommandPoolTrimFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)



instance Show CommandPoolTrimFlags where
  showsPrec p = \case
    CommandPoolTrimFlags x -> showParen (p >= 11) (showString "CommandPoolTrimFlags 0x" . showHex x)

instance Read CommandPoolTrimFlags where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "CommandPoolTrimFlags")
                       v <- step readPrec
                       pure (CommandPoolTrimFlags v)))

