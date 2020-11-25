{-# language CPP #-}
-- No documentation found for Chapter "SubpassContents"
module Vulkan.Core10.Enums.SubpassContents  (SubpassContents( SUBPASS_CONTENTS_INLINE
                                                            , SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS
                                                            , ..
                                                            )) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Zero (Zero)
-- No documentation found for TopLevel "VkSubpassContents"
newtype SubpassContents = SubpassContents Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkSubpassContents" "VK_SUBPASS_CONTENTS_INLINE"
pattern SUBPASS_CONTENTS_INLINE                    = SubpassContents 0
-- No documentation found for Nested "VkSubpassContents" "VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS"
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
showsPrec = enumShowsPrec enumPrefixSubpassContents
                          showTableSubpassContents
                          conNameSubpassContents
                          (\(SubpassContents x) -> x)
                          (showsPrec 11)


instance Read SubpassContents where
  readPrec = enumReadPrec enumPrefixSubpassContents showTableSubpassContents conNameSubpassContents SubpassContents

