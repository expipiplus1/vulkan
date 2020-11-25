{-# language CPP #-}
-- No documentation found for Chapter "ExternalFenceFeatureFlagBits"
module Vulkan.Core11.Enums.ExternalFenceFeatureFlagBits  ( ExternalFenceFeatureFlags
                                                         , ExternalFenceFeatureFlagBits( EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT
                                                                                       , EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT
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

conNameExternalFenceFeatureFlagBits :: String
conNameExternalFenceFeatureFlagBits = "ExternalFenceFeatureFlagBits"

enumPrefixExternalFenceFeatureFlagBits :: String
enumPrefixExternalFenceFeatureFlagBits = "EXTERNAL_FENCE_FEATURE_"

showTableExternalFenceFeatureFlagBits :: [(ExternalFenceFeatureFlagBits, String)]
showTableExternalFenceFeatureFlagBits =
  [(EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT, "EXPORTABLE_BIT"), (EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT, "IMPORTABLE_BIT")]

instance Show ExternalFenceFeatureFlagBits where
  showsPrec p e = case lookup e showTableExternalFenceFeatureFlagBits of
    Just s -> showString enumPrefixExternalFenceFeatureFlagBits . showString s
    Nothing ->
      let ExternalFenceFeatureFlagBits x = e
      in  showParen (p >= 11) (showString conNameExternalFenceFeatureFlagBits . showString " 0x" . showHex x)

instance Read ExternalFenceFeatureFlagBits where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixExternalFenceFeatureFlagBits
          asum ((\(e, s) -> e <$ string s) <$> showTableExternalFenceFeatureFlagBits)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameExternalFenceFeatureFlagBits)
            v <- step readPrec
            pure (ExternalFenceFeatureFlagBits v)
          )
    )

