{-# language CPP #-}
-- No documentation found for Chapter "ExternalSemaphoreFeatureFlagBits"
module Vulkan.Core11.Enums.ExternalSemaphoreFeatureFlagBits  ( ExternalSemaphoreFeatureFlags
                                                             , ExternalSemaphoreFeatureFlagBits( EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT
                                                                                               , EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT
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
type ExternalSemaphoreFeatureFlags = ExternalSemaphoreFeatureFlagBits

-- | VkExternalSemaphoreFeatureFlagBits - Bitfield describing features of an
-- external semaphore handle type
--
-- = See Also
--
-- 'ExternalSemaphoreFeatureFlags'
newtype ExternalSemaphoreFeatureFlagBits = ExternalSemaphoreFeatureFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT' specifies that handles of
-- this type /can/ be exported from Vulkan semaphore objects.
pattern EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT = ExternalSemaphoreFeatureFlagBits 0x00000001
-- | 'EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT' specifies that handles of
-- this type /can/ be imported as Vulkan semaphore objects.
pattern EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT = ExternalSemaphoreFeatureFlagBits 0x00000002

conNameExternalSemaphoreFeatureFlagBits :: String
conNameExternalSemaphoreFeatureFlagBits = "ExternalSemaphoreFeatureFlagBits"

enumPrefixExternalSemaphoreFeatureFlagBits :: String
enumPrefixExternalSemaphoreFeatureFlagBits = "EXTERNAL_SEMAPHORE_FEATURE_"

showTableExternalSemaphoreFeatureFlagBits :: [(ExternalSemaphoreFeatureFlagBits, String)]
showTableExternalSemaphoreFeatureFlagBits =
  [ (EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT, "EXPORTABLE_BIT")
  , (EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT, "IMPORTABLE_BIT")
  ]

instance Show ExternalSemaphoreFeatureFlagBits where
  showsPrec p e = case lookup e showTableExternalSemaphoreFeatureFlagBits of
    Just s -> showString enumPrefixExternalSemaphoreFeatureFlagBits . showString s
    Nothing ->
      let ExternalSemaphoreFeatureFlagBits x = e
      in  showParen (p >= 11) (showString conNameExternalSemaphoreFeatureFlagBits . showString " 0x" . showHex x)

instance Read ExternalSemaphoreFeatureFlagBits where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixExternalSemaphoreFeatureFlagBits
          asum ((\(e, s) -> e <$ string s) <$> showTableExternalSemaphoreFeatureFlagBits)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameExternalSemaphoreFeatureFlagBits)
            v <- step readPrec
            pure (ExternalSemaphoreFeatureFlagBits v)
          )
    )

