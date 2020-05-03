{-# language CPP #-}
module Vulkan.Core10.Enums.SubpassContents  (SubpassContents( SUBPASS_CONTENTS_INLINE
                                                            , SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS
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
pattern SUBPASS_CONTENTS_INLINE = SubpassContents 0
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

instance Show SubpassContents where
  showsPrec p = \case
    SUBPASS_CONTENTS_INLINE -> showString "SUBPASS_CONTENTS_INLINE"
    SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS -> showString "SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS"
    SubpassContents x -> showParen (p >= 11) (showString "SubpassContents " . showsPrec 11 x)

instance Read SubpassContents where
  readPrec = parens (choose [("SUBPASS_CONTENTS_INLINE", pure SUBPASS_CONTENTS_INLINE)
                            , ("SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS", pure SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS)]
                     +++
                     prec 10 (do
                       expectP (Ident "SubpassContents")
                       v <- step readPrec
                       pure (SubpassContents v)))

