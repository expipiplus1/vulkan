{-# language CPP #-}
-- No documentation found for Chapter "CommandPoolResetFlagBits"
module Vulkan.Core10.Enums.CommandPoolResetFlagBits  ( CommandPoolResetFlags
                                                     , CommandPoolResetFlagBits( COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT
                                                                               , ..
                                                                               )
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
type CommandPoolResetFlags = CommandPoolResetFlagBits

-- | VkCommandPoolResetFlagBits - Bitmask controlling behavior of a command
-- pool reset
--
-- = See Also
--
-- 'CommandPoolResetFlags'
newtype CommandPoolResetFlagBits = CommandPoolResetFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT' specifies that resetting a
-- command pool recycles all of the resources from the command pool back to
-- the system.
pattern COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT = CommandPoolResetFlagBits 0x00000001

conNameCommandPoolResetFlagBits :: String
conNameCommandPoolResetFlagBits = "CommandPoolResetFlagBits"

enumPrefixCommandPoolResetFlagBits :: String
enumPrefixCommandPoolResetFlagBits = "COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT"

showTableCommandPoolResetFlagBits :: [(CommandPoolResetFlagBits, String)]
showTableCommandPoolResetFlagBits = [(COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT, "")]

instance Show CommandPoolResetFlagBits where
  showsPrec p e = case lookup e showTableCommandPoolResetFlagBits of
    Just s -> showString enumPrefixCommandPoolResetFlagBits . showString s
    Nothing ->
      let CommandPoolResetFlagBits x = e
      in  showParen (p >= 11) (showString conNameCommandPoolResetFlagBits . showString " 0x" . showHex x)

instance Read CommandPoolResetFlagBits where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixCommandPoolResetFlagBits
          asum ((\(e, s) -> e <$ string s) <$> showTableCommandPoolResetFlagBits)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameCommandPoolResetFlagBits)
            v <- step readPrec
            pure (CommandPoolResetFlagBits v)
          )
    )

