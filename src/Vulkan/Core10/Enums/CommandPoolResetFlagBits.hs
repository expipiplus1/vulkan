{-# language CPP #-}
-- No documentation found for Chapter "CommandPoolResetFlagBits"
module Vulkan.Core10.Enums.CommandPoolResetFlagBits  ( CommandPoolResetFlagBits( COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT
                                                                               , ..
                                                                               )
                                                     , CommandPoolResetFlags
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

type CommandPoolResetFlags = CommandPoolResetFlagBits

instance Show CommandPoolResetFlagBits where
  showsPrec p = \case
    COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT -> showString "COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT"
    CommandPoolResetFlagBits x -> showParen (p >= 11) (showString "CommandPoolResetFlagBits 0x" . showHex x)

instance Read CommandPoolResetFlagBits where
  readPrec = parens (choose [("COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT", pure COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT)]
                     +++
                     prec 10 (do
                       expectP (Ident "CommandPoolResetFlagBits")
                       v <- step readPrec
                       pure (CommandPoolResetFlagBits v)))

