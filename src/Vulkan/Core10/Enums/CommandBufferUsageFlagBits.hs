{-# language CPP #-}
module Vulkan.Core10.Enums.CommandBufferUsageFlagBits  ( CommandBufferUsageFlagBits( COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
                                                                                   , COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT
                                                                                   , COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT
                                                                                   , ..
                                                                                   )
                                                       , CommandBufferUsageFlags
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
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Core10.BaseType (Flags)
import Vulkan.Zero (Zero)
-- | VkCommandBufferUsageFlagBits - Bitmask specifying usage behavior for
-- command buffer
--
-- = See Also
--
-- 'CommandBufferUsageFlags'
newtype CommandBufferUsageFlagBits = CommandBufferUsageFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT' specifies that each recording
-- of the command buffer will only be submitted once, and the command
-- buffer will be reset and recorded again between each submission.
pattern COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT = CommandBufferUsageFlagBits 0x00000001
-- | 'COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT' specifies that a
-- secondary command buffer is considered to be entirely inside a render
-- pass. If this is a primary command buffer, then this bit is ignored.
pattern COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT = CommandBufferUsageFlagBits 0x00000002
-- | 'COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT' specifies that a command
-- buffer /can/ be resubmitted to a queue while it is in the /pending
-- state/, and recorded into multiple primary command buffers.
pattern COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT = CommandBufferUsageFlagBits 0x00000004

type CommandBufferUsageFlags = CommandBufferUsageFlagBits

instance Show CommandBufferUsageFlagBits where
  showsPrec p = \case
    COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT -> showString "COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT"
    COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT -> showString "COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT"
    COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT -> showString "COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT"
    CommandBufferUsageFlagBits x -> showParen (p >= 11) (showString "CommandBufferUsageFlagBits 0x" . showHex x)

instance Read CommandBufferUsageFlagBits where
  readPrec = parens (choose [("COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT", pure COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT)
                            , ("COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT", pure COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT)
                            , ("COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT", pure COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT)]
                     +++
                     prec 10 (do
                       expectP (Ident "CommandBufferUsageFlagBits")
                       v <- step readPrec
                       pure (CommandBufferUsageFlagBits v)))

