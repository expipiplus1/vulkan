{-# language CPP #-}
-- No documentation found for Chapter "CommandBufferResetFlagBits"
module Vulkan.Core10.Enums.CommandBufferResetFlagBits  ( CommandBufferResetFlags
                                                       , CommandBufferResetFlagBits( COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT
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
type CommandBufferResetFlags = CommandBufferResetFlagBits

-- | VkCommandBufferResetFlagBits - Bitmask controlling behavior of a command
-- buffer reset
--
-- = See Also
--
-- 'CommandBufferResetFlags'
newtype CommandBufferResetFlagBits = CommandBufferResetFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT' specifies that most or all
-- memory resources currently owned by the command buffer /should/ be
-- returned to the parent command pool. If this flag is not set, then the
-- command buffer /may/ hold onto memory resources and reuse them when
-- recording commands. @commandBuffer@ is moved to the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle initial state>.
pattern COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT = CommandBufferResetFlagBits 0x00000001

conNameCommandBufferResetFlagBits :: String
conNameCommandBufferResetFlagBits = "CommandBufferResetFlagBits"

enumPrefixCommandBufferResetFlagBits :: String
enumPrefixCommandBufferResetFlagBits = "COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT"

showTableCommandBufferResetFlagBits :: [(CommandBufferResetFlagBits, String)]
showTableCommandBufferResetFlagBits = [(COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT, "")]

instance Show CommandBufferResetFlagBits where
  showsPrec p e = case lookup e showTableCommandBufferResetFlagBits of
    Just s -> showString enumPrefixCommandBufferResetFlagBits . showString s
    Nothing ->
      let CommandBufferResetFlagBits x = e
      in  showParen (p >= 11) (showString conNameCommandBufferResetFlagBits . showString " 0x" . showHex x)

instance Read CommandBufferResetFlagBits where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixCommandBufferResetFlagBits
          asum ((\(e, s) -> e <$ string s) <$> showTableCommandBufferResetFlagBits)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameCommandBufferResetFlagBits)
            v <- step readPrec
            pure (CommandBufferResetFlagBits v)
          )
    )

