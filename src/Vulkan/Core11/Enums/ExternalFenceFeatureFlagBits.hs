{-# language CPP #-}
-- No documentation found for Chapter "ExternalFenceFeatureFlagBits"
module Vulkan.Core11.Enums.ExternalFenceFeatureFlagBits  ( ExternalFenceFeatureFlags
                                                         , ExternalFenceFeatureFlagBits( EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT
                                                                                       , EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT
                                                                                       , ..
                                                                                       )
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
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
type ExternalFenceFeatureFlags = ExternalFenceFeatureFlagBits

-- | VkExternalFenceFeatureFlagBits - Bitfield describing features of an
-- external fence handle type
--
-- = See Also
--
-- 'ExternalFenceFeatureFlags'
newtype ExternalFenceFeatureFlagBits = ExternalFenceFeatureFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT' specifies handles of this type
-- /can/ be exported from Vulkan fence objects.
pattern EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT = ExternalFenceFeatureFlagBits 0x00000001
-- | 'EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT' specifies handles of this type
-- /can/ be imported to Vulkan fence objects.
pattern EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT = ExternalFenceFeatureFlagBits 0x00000002

instance Show ExternalFenceFeatureFlagBits where
  showsPrec p = \case
    EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT -> showString "EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT"
    EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT -> showString "EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT"
    ExternalFenceFeatureFlagBits x -> showParen (p >= 11) (showString "ExternalFenceFeatureFlagBits 0x" . showHex x)

instance Read ExternalFenceFeatureFlagBits where
  readPrec = parens (choose [("EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT", pure EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT)
                            , ("EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT", pure EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT)]
                     +++
                     prec 10 (do
                       expectP (Ident "ExternalFenceFeatureFlagBits")
                       v <- step readPrec
                       pure (ExternalFenceFeatureFlagBits v)))

