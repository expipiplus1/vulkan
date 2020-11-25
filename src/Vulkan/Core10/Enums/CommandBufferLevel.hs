{-# language CPP #-}
-- No documentation found for Chapter "CommandBufferLevel"
module Vulkan.Core10.Enums.CommandBufferLevel  (CommandBufferLevel( COMMAND_BUFFER_LEVEL_PRIMARY
                                                                  , COMMAND_BUFFER_LEVEL_SECONDARY
                                                                  , ..
                                                                  )) where

import Data.Foldable (asum)
import GHC.Base ((<$))
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Text.ParserCombinators.ReadP (skipSpaces)
import Text.ParserCombinators.ReadP (string)
import Text.ParserCombinators.ReadPrec ((+++))
import qualified Text.ParserCombinators.ReadPrec (lift)
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Zero (Zero)
-- | VkCommandBufferLevel - Enumerant specifying a command buffer level
--
-- = See Also
--
-- 'Vulkan.Core10.CommandBuffer.CommandBufferAllocateInfo'
newtype CommandBufferLevel = CommandBufferLevel Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'COMMAND_BUFFER_LEVEL_PRIMARY' specifies a primary command buffer.
pattern COMMAND_BUFFER_LEVEL_PRIMARY   = CommandBufferLevel 0
-- | 'COMMAND_BUFFER_LEVEL_SECONDARY' specifies a secondary command buffer.
pattern COMMAND_BUFFER_LEVEL_SECONDARY = CommandBufferLevel 1
{-# complete COMMAND_BUFFER_LEVEL_PRIMARY,
             COMMAND_BUFFER_LEVEL_SECONDARY :: CommandBufferLevel #-}

conNameCommandBufferLevel :: String
conNameCommandBufferLevel = "CommandBufferLevel"

enumPrefixCommandBufferLevel :: String
enumPrefixCommandBufferLevel = "COMMAND_BUFFER_LEVEL_"

showTableCommandBufferLevel :: [(CommandBufferLevel, String)]
showTableCommandBufferLevel =
  [(COMMAND_BUFFER_LEVEL_PRIMARY, "PRIMARY"), (COMMAND_BUFFER_LEVEL_SECONDARY, "SECONDARY")]

instance Show CommandBufferLevel where
  showsPrec p e = case lookup e showTableCommandBufferLevel of
    Just s -> showString enumPrefixCommandBufferLevel . showString s
    Nothing ->
      let CommandBufferLevel x = e
      in  showParen (p >= 11) (showString conNameCommandBufferLevel . showString " " . showsPrec 11 x)

instance Read CommandBufferLevel where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixCommandBufferLevel
          asum ((\(e, s) -> e <$ string s) <$> showTableCommandBufferLevel)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameCommandBufferLevel)
            v <- step readPrec
            pure (CommandBufferLevel v)
          )
    )

