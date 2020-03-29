{-# language CPP #-}
module Graphics.Vulkan.Core10.Enums.CommandBufferResetFlagBits  ( CommandBufferResetFlagBits( COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT
                                                                                            , ..
                                                                                            )
                                                                , CommandBufferResetFlags
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
import Graphics.Vulkan.Core10.BaseType (Flags)
import Graphics.Vulkan.Zero (Zero)
-- | VkCommandBufferResetFlagBits - Bitmask controlling behavior of a command
-- buffer reset
--
-- = See Also
--
-- 'CommandBufferResetFlags'
newtype CommandBufferResetFlagBits = CommandBufferResetFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT' specifies that most or all
-- memory resources currently owned by the command buffer /should/ be
-- returned to the parent command pool. If this flag is not set, then the
-- command buffer /may/ hold onto memory resources and reuse them when
-- recording commands. 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is
-- moved to the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle initial state>.
pattern COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT = CommandBufferResetFlagBits 0x00000001

type CommandBufferResetFlags = CommandBufferResetFlagBits

instance Show CommandBufferResetFlagBits where
  showsPrec p = \case
    COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT -> showString "COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT"
    CommandBufferResetFlagBits x -> showParen (p >= 11) (showString "CommandBufferResetFlagBits 0x" . showHex x)

instance Read CommandBufferResetFlagBits where
  readPrec = parens (choose [("COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT", pure COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT)]
                     +++
                     prec 10 (do
                       expectP (Ident "CommandBufferResetFlagBits")
                       v <- step readPrec
                       pure (CommandBufferResetFlagBits v)))

