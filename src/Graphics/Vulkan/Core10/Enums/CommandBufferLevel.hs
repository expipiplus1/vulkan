{-# language CPP #-}
module Graphics.Vulkan.Core10.Enums.CommandBufferLevel  (CommandBufferLevel( COMMAND_BUFFER_LEVEL_PRIMARY
                                                                           , COMMAND_BUFFER_LEVEL_SECONDARY
                                                                           , ..
                                                                           )) where

import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Graphics.Vulkan.Zero (Zero)
-- | VkCommandBufferLevel - Enumerant specifying a command buffer level
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.CommandBuffer.CommandBufferAllocateInfo'
newtype CommandBufferLevel = CommandBufferLevel Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'COMMAND_BUFFER_LEVEL_PRIMARY' specifies a primary command buffer.
pattern COMMAND_BUFFER_LEVEL_PRIMARY = CommandBufferLevel 0
-- | 'COMMAND_BUFFER_LEVEL_SECONDARY' specifies a secondary command buffer.
pattern COMMAND_BUFFER_LEVEL_SECONDARY = CommandBufferLevel 1
{-# complete COMMAND_BUFFER_LEVEL_PRIMARY,
             COMMAND_BUFFER_LEVEL_SECONDARY :: CommandBufferLevel #-}

instance Show CommandBufferLevel where
  showsPrec p = \case
    COMMAND_BUFFER_LEVEL_PRIMARY -> showString "COMMAND_BUFFER_LEVEL_PRIMARY"
    COMMAND_BUFFER_LEVEL_SECONDARY -> showString "COMMAND_BUFFER_LEVEL_SECONDARY"
    CommandBufferLevel x -> showParen (p >= 11) (showString "CommandBufferLevel " . showsPrec 11 x)

instance Read CommandBufferLevel where
  readPrec = parens (choose [("COMMAND_BUFFER_LEVEL_PRIMARY", pure COMMAND_BUFFER_LEVEL_PRIMARY)
                            , ("COMMAND_BUFFER_LEVEL_SECONDARY", pure COMMAND_BUFFER_LEVEL_SECONDARY)]
                     +++
                     prec 10 (do
                       expectP (Ident "CommandBufferLevel")
                       v <- step readPrec
                       pure (CommandBufferLevel v)))

