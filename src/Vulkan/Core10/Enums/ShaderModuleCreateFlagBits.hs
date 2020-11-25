{-# language CPP #-}
-- No documentation found for Chapter "ShaderModuleCreateFlagBits"
module Vulkan.Core10.Enums.ShaderModuleCreateFlagBits  ( ShaderModuleCreateFlags
                                                       , ShaderModuleCreateFlagBits(..)
                                                       ) where

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
type ShaderModuleCreateFlags = ShaderModuleCreateFlagBits

-- No documentation found for TopLevel "VkShaderModuleCreateFlagBits"
newtype ShaderModuleCreateFlagBits = ShaderModuleCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameShaderModuleCreateFlagBits :: String
conNameShaderModuleCreateFlagBits = "ShaderModuleCreateFlagBits"

enumPrefixShaderModuleCreateFlagBits :: String
enumPrefixShaderModuleCreateFlagBits = ""

showTableShaderModuleCreateFlagBits :: [(ShaderModuleCreateFlagBits, String)]
showTableShaderModuleCreateFlagBits = []

instance Show ShaderModuleCreateFlagBits where
  showsPrec p e = case lookup e showTableShaderModuleCreateFlagBits of
    Just s -> showString enumPrefixShaderModuleCreateFlagBits . showString s
    Nothing ->
      let ShaderModuleCreateFlagBits x = e
      in  showParen (p >= 11) (showString conNameShaderModuleCreateFlagBits . showString " 0x" . showHex x)

instance Read ShaderModuleCreateFlagBits where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixShaderModuleCreateFlagBits
          asum ((\(e, s) -> e <$ string s) <$> showTableShaderModuleCreateFlagBits)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameShaderModuleCreateFlagBits)
            v <- step readPrec
            pure (ShaderModuleCreateFlagBits v)
          )
    )

