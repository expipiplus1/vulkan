{-# language CPP #-}
module Vulkan.Core10.Enums.ShaderModuleCreateFlagBits  ( ShaderModuleCreateFlagBits(..)
                                                       , ShaderModuleCreateFlags
                                                       ) where

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
-- No documentation found for TopLevel "VkShaderModuleCreateFlagBits"
newtype ShaderModuleCreateFlagBits = ShaderModuleCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



type ShaderModuleCreateFlags = ShaderModuleCreateFlagBits

instance Show ShaderModuleCreateFlagBits where
  showsPrec p = \case
    ShaderModuleCreateFlagBits x -> showParen (p >= 11) (showString "ShaderModuleCreateFlagBits 0x" . showHex x)

instance Read ShaderModuleCreateFlagBits where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "ShaderModuleCreateFlagBits")
                       v <- step readPrec
                       pure (ShaderModuleCreateFlagBits v)))

