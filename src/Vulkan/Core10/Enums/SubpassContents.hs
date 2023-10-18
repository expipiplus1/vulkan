{-# language CPP #-}
-- No documentation found for Chapter "SubpassContents"
module Vulkan.Core10.Enums.SubpassContents  (SubpassContents( SUBPASS_CONTENTS_INLINE
                                                            , SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS
                                                            , SUBPASS_CONTENTS_INLINE_AND_SECONDARY_COMMAND_BUFFERS_EXT
                                                            , ..
                                                            )) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))

-- | VkSubpassContents - Specify how commands in the first subpass of a
-- render pass are provided
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
-- valid command in the command buffer until
-- 'Vulkan.Core10.CommandBufferBuilding.cmdNextSubpass' or
-- 'Vulkan.Core10.CommandBufferBuilding.cmdEndRenderPass'.
pattern SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS = SubpassContents 1

-- | 'SUBPASS_CONTENTS_INLINE_AND_SECONDARY_COMMAND_BUFFERS_EXT' specifies
-- that the contents of the subpass /can/ be recorded both inline and in
-- secondary command buffers executed from this command buffer with
-- 'Vulkan.Core10.CommandBufferBuilding.cmdExecuteCommands'.
pattern SUBPASS_CONTENTS_INLINE_AND_SECONDARY_COMMAND_BUFFERS_EXT = SubpassContents 1000451000

{-# COMPLETE
  SUBPASS_CONTENTS_INLINE
  , SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS
  , SUBPASS_CONTENTS_INLINE_AND_SECONDARY_COMMAND_BUFFERS_EXT ::
    SubpassContents
  #-}

conNameSubpassContents :: String
conNameSubpassContents = "SubpassContents"

enumPrefixSubpassContents :: String
enumPrefixSubpassContents = "SUBPASS_CONTENTS_"

showTableSubpassContents :: [(SubpassContents, String)]
showTableSubpassContents =
  [ (SUBPASS_CONTENTS_INLINE, "INLINE")
  ,
    ( SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS
    , "SECONDARY_COMMAND_BUFFERS"
    )
  ,
    ( SUBPASS_CONTENTS_INLINE_AND_SECONDARY_COMMAND_BUFFERS_EXT
    , "INLINE_AND_SECONDARY_COMMAND_BUFFERS_EXT"
    )
  ]

instance Show SubpassContents where
  showsPrec =
    enumShowsPrec
      enumPrefixSubpassContents
      showTableSubpassContents
      conNameSubpassContents
      (\(SubpassContents x) -> x)
      (showsPrec 11)

instance Read SubpassContents where
  readPrec =
    enumReadPrec
      enumPrefixSubpassContents
      showTableSubpassContents
      conNameSubpassContents
      SubpassContents
