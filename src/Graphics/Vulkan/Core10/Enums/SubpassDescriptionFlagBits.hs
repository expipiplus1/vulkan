{-# language CPP #-}
module Graphics.Vulkan.Core10.Enums.SubpassDescriptionFlagBits  ( SubpassDescriptionFlagBits( SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX
                                                                                            , SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX
                                                                                            , ..
                                                                                            )
                                                                , SubpassDescriptionFlags
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
-- | VkSubpassDescriptionFlagBits - Bitmask specifying usage of a subpass
--
-- = See Also
--
-- 'SubpassDescriptionFlags'
newtype SubpassDescriptionFlagBits = SubpassDescriptionFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX' specifies that
-- shaders compiled for this subpass use per-view positions which only
-- differ in value in the x component. Per-view viewport mask /can/ also be
-- used.
pattern SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX = SubpassDescriptionFlagBits 0x00000002
-- | 'SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX' specifies that shaders
-- compiled for this subpass write the attributes for all views in a single
-- invocation of each vertex processing stage. All pipelines compiled
-- against a subpass that includes this bit /must/ write per-view
-- attributes to the @*PerViewNV[]@ shader outputs, in addition to the
-- non-per-view (e.g. @Position@) outputs.
pattern SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX = SubpassDescriptionFlagBits 0x00000001

type SubpassDescriptionFlags = SubpassDescriptionFlagBits

instance Show SubpassDescriptionFlagBits where
  showsPrec p = \case
    SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX -> showString "SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX"
    SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX -> showString "SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX"
    SubpassDescriptionFlagBits x -> showParen (p >= 11) (showString "SubpassDescriptionFlagBits 0x" . showHex x)

instance Read SubpassDescriptionFlagBits where
  readPrec = parens (choose [("SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX", pure SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX)
                            , ("SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX", pure SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX)]
                     +++
                     prec 10 (do
                       expectP (Ident "SubpassDescriptionFlagBits")
                       v <- step readPrec
                       pure (SubpassDescriptionFlagBits v)))

