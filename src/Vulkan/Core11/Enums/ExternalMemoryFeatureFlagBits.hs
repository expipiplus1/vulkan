{-# language CPP #-}
-- No documentation found for Chapter "ExternalMemoryFeatureFlagBits"
module Vulkan.Core11.Enums.ExternalMemoryFeatureFlagBits  ( ExternalMemoryFeatureFlags
                                                          , ExternalMemoryFeatureFlagBits( EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT
                                                                                         , EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT
                                                                                         , EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT
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
type ExternalMemoryFeatureFlags = ExternalMemoryFeatureFlagBits

-- No documentation found for TopLevel "VkExternalMemoryFeatureFlagBits"
newtype ExternalMemoryFeatureFlagBits = ExternalMemoryFeatureFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkExternalMemoryFeatureFlagBits" "VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT"
pattern EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT = ExternalMemoryFeatureFlagBits 0x00000001
-- No documentation found for Nested "VkExternalMemoryFeatureFlagBits" "VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT"
pattern EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT     = ExternalMemoryFeatureFlagBits 0x00000002
-- No documentation found for Nested "VkExternalMemoryFeatureFlagBits" "VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT"
pattern EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT     = ExternalMemoryFeatureFlagBits 0x00000004

conNameExternalMemoryFeatureFlagBits :: String
conNameExternalMemoryFeatureFlagBits = "ExternalMemoryFeatureFlagBits"

enumPrefixExternalMemoryFeatureFlagBits :: String
enumPrefixExternalMemoryFeatureFlagBits = "EXTERNAL_MEMORY_FEATURE_"

showTableExternalMemoryFeatureFlagBits :: [(ExternalMemoryFeatureFlagBits, String)]
showTableExternalMemoryFeatureFlagBits =
  [ (EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT, "DEDICATED_ONLY_BIT")
  , (EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT    , "EXPORTABLE_BIT")
  , (EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT    , "IMPORTABLE_BIT")
  ]


instance Show ExternalMemoryFeatureFlagBits where
showsPrec = enumShowsPrec enumPrefixExternalMemoryFeatureFlagBits
                          showTableExternalMemoryFeatureFlagBits
                          conNameExternalMemoryFeatureFlagBits
                          (\(ExternalMemoryFeatureFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read ExternalMemoryFeatureFlagBits where
  readPrec = enumReadPrec enumPrefixExternalMemoryFeatureFlagBits
                          showTableExternalMemoryFeatureFlagBits
                          conNameExternalMemoryFeatureFlagBits
                          ExternalMemoryFeatureFlagBits

