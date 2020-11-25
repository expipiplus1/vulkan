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
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
type ExternalFenceFeatureFlags = ExternalFenceFeatureFlagBits

-- No documentation found for TopLevel "VkExternalFenceFeatureFlagBits"
newtype ExternalFenceFeatureFlagBits = ExternalFenceFeatureFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkExternalFenceFeatureFlagBits" "VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT"
pattern EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT = ExternalFenceFeatureFlagBits 0x00000001
-- No documentation found for Nested "VkExternalFenceFeatureFlagBits" "VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT"
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

