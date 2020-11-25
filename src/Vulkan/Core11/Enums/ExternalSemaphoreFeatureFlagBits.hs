{-# language CPP #-}
-- No documentation found for Chapter "ExternalSemaphoreFeatureFlagBits"
module Vulkan.Core11.Enums.ExternalSemaphoreFeatureFlagBits  ( ExternalSemaphoreFeatureFlagBits( EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT
                                                                                               , EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT
                                                                                               , ..
                                                                                               )
                                                             , ExternalSemaphoreFeatureFlags
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

type ExternalSemaphoreFeatureFlags = ExternalSemaphoreFeatureFlagBits

instance Show ExternalSemaphoreFeatureFlagBits where
  showsPrec p = \case
    EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT -> showString "EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT"
    EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT -> showString "EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT"
    ExternalSemaphoreFeatureFlagBits x -> showParen (p >= 11) (showString "ExternalSemaphoreFeatureFlagBits 0x" . showHex x)

instance Read ExternalSemaphoreFeatureFlagBits where
  readPrec = parens (choose [("EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT", pure EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT)
                            , ("EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT", pure EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT)]
                     +++
                     prec 10 (do
                       expectP (Ident "ExternalSemaphoreFeatureFlagBits")
                       v <- step readPrec
                       pure (ExternalSemaphoreFeatureFlagBits v)))

