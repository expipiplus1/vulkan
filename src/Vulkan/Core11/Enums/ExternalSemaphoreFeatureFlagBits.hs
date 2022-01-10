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
import Vulkan.Zero (Zero)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
type ExternalSemaphoreFeatureFlags = ExternalSemaphoreFeatureFlagBits

-- | VkExternalSemaphoreFeatureFlagBits - Bitfield describing features of an
-- external semaphore handle type
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
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

