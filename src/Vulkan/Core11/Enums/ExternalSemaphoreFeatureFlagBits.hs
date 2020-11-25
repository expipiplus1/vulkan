{-# language CPP #-}
-- No documentation found for Chapter "ExternalSemaphoreFeatureFlagBits"
module Vulkan.Core11.Enums.ExternalSemaphoreFeatureFlagBits  ( ExternalSemaphoreFeatureFlags
                                                             , ExternalSemaphoreFeatureFlagBits( EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT
                                                                                               , EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT
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
type ExternalSemaphoreFeatureFlags = ExternalSemaphoreFeatureFlagBits

-- No documentation found for TopLevel "VkExternalSemaphoreFeatureFlagBits"
newtype ExternalSemaphoreFeatureFlagBits = ExternalSemaphoreFeatureFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkExternalSemaphoreFeatureFlagBits" "VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT"
pattern EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT = ExternalSemaphoreFeatureFlagBits 0x00000001
-- No documentation found for Nested "VkExternalSemaphoreFeatureFlagBits" "VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT"
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
showsPrec = enumShowsPrec enumPrefixExternalSemaphoreFeatureFlagBits
                          showTableExternalSemaphoreFeatureFlagBits
                          conNameExternalSemaphoreFeatureFlagBits
                          (\(ExternalSemaphoreFeatureFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read ExternalSemaphoreFeatureFlagBits where
  readPrec = enumReadPrec enumPrefixExternalSemaphoreFeatureFlagBits
                          showTableExternalSemaphoreFeatureFlagBits
                          conNameExternalSemaphoreFeatureFlagBits
                          ExternalSemaphoreFeatureFlagBits

