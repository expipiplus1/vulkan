{-# language CPP #-}
-- No documentation found for Chapter "SubpassContents"
module Vulkan.Core10.Enums.SubpassContents  (SubpassContents( SUBPASS_CONTENTS_INLINE
                                                            , SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS
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
-- | VkSubpassContents - Specify how commands in the first subpass of a
-- render pass are provided
--
-- = See Also
--
-- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.SubpassBeginInfo',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBeginRenderPass',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdNextSubpass'
newtype SubpassContents = SubpassContents Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'SUBPASS_CONTENTS_INLINE' specifies that the contents of the subpass
-- will be recorded inline in the primary command buffer, and secondary
-- command buffers /must/ not be executed within the subpass.
pattern SUBPASS_CONTENTS_INLINE                    = SubpassContents 0
-- | 'SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS' specifies that the contents
-- are recorded in secondary command buffers that will be called from the
-- primary command buffer, and
-- 'Vulkan.Core10.CommandBufferBuilding.cmdExecuteCommands' is the only
-- valid command on the command buffer until
-- 'Vulkan.Core10.CommandBufferBuilding.cmdNextSubpass' or
-- 'Vulkan.Core10.CommandBufferBuilding.cmdEndRenderPass'.
pattern SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS = SubpassContents 1
{-# complete SUBPASS_CONTENTS_INLINE,
             SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS :: SubpassContents #-}

conNameSubpassContents :: String
conNameSubpassContents = "SubpassContents"

enumPrefixSubpassContents :: String
enumPrefixSubpassContents = "SUBPASS_CONTENTS_"

showTableSubpassContents :: [(SubpassContents, String)]
showTableSubpassContents =
  [(SUBPASS_CONTENTS_INLINE, "INLINE"), (SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS, "SECONDARY_COMMAND_BUFFERS")]

instance Show SubpassContents where
  showsPrec p e = case lookup e showTableSubpassContents of
    Just s -> showString enumPrefixSubpassContents . showString s
    Nothing ->
      let SubpassContents x = e
      in  showParen (p >= 11) (showString conNameSubpassContents . showString " " . showsPrec 11 x)

instance Read SubpassContents where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixSubpassContents
          asum ((\(e, s) -> e <$ string s) <$> showTableSubpassContents)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameSubpassContents)
            v <- step readPrec
            pure (SubpassContents v)
          )
    )

