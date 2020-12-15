{-# language CPP #-}
-- No documentation found for Chapter "ExternalFenceFeatureFlagBits"
module Vulkan.Core11.Enums.ExternalFenceFeatureFlagBits  ( ExternalFenceFeatureFlags
                                                         , ExternalFenceFeatureFlagBits( EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT
                                                                                       , EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT
                                                                                       , ..
                                                                                       )
                                                         ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Vulkan.Zero (Zero)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
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
  showsPrec = enumShowsPrec enumPrefixExternalFenceFeatureFlagBits
                            showTableExternalFenceFeatureFlagBits
                            conNameExternalFenceFeatureFlagBits
                            (\(ExternalFenceFeatureFlagBits x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read ExternalFenceFeatureFlagBits where
  readPrec = enumReadPrec enumPrefixExternalFenceFeatureFlagBits
                          showTableExternalFenceFeatureFlagBits
                          conNameExternalFenceFeatureFlagBits
                          ExternalFenceFeatureFlagBits

