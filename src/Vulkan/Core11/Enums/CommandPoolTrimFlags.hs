{-# language CPP #-}
-- No documentation found for Chapter "CommandPoolTrimFlags"
module Vulkan.Core11.Enums.CommandPoolTrimFlags  (CommandPoolTrimFlags(..)) where

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
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameCommandPoolTrimFlags :: String
conNameCommandPoolTrimFlags = "CommandPoolTrimFlags"

enumPrefixCommandPoolTrimFlags :: String
enumPrefixCommandPoolTrimFlags = ""

showTableCommandPoolTrimFlags :: [(CommandPoolTrimFlags, String)]
showTableCommandPoolTrimFlags = []

instance Show CommandPoolTrimFlags where
  showsPrec p e = case lookup e showTableCommandPoolTrimFlags of
    Just s -> showString enumPrefixCommandPoolTrimFlags . showString s
    Nothing ->
      let CommandPoolTrimFlags x = e
      in  showParen (p >= 11) (showString conNameCommandPoolTrimFlags . showString " 0x" . showHex x)

instance Read CommandPoolTrimFlags where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixCommandPoolTrimFlags
          asum ((\(e, s) -> e <$ string s) <$> showTableCommandPoolTrimFlags)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameCommandPoolTrimFlags)
            v <- step readPrec
            pure (CommandPoolTrimFlags v)
          )
    )

