{-# language CPP #-}
-- No documentation found for Chapter "PeerMemoryFeatureFlagBits"
module Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits  ( PeerMemoryFeatureFlags
                                                      , PeerMemoryFeatureFlagBits( PEER_MEMORY_FEATURE_COPY_SRC_BIT
                                                                                 , PEER_MEMORY_FEATURE_COPY_DST_BIT
                                                                                 , PEER_MEMORY_FEATURE_GENERIC_SRC_BIT
                                                                                 , PEER_MEMORY_FEATURE_GENERIC_DST_BIT
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
type PeerMemoryFeatureFlags = PeerMemoryFeatureFlagBits

-- No documentation found for TopLevel "VkPeerMemoryFeatureFlagBits"
newtype PeerMemoryFeatureFlagBits = PeerMemoryFeatureFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkPeerMemoryFeatureFlagBits" "VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT"
pattern PEER_MEMORY_FEATURE_COPY_SRC_BIT    = PeerMemoryFeatureFlagBits 0x00000001
-- No documentation found for Nested "VkPeerMemoryFeatureFlagBits" "VK_PEER_MEMORY_FEATURE_COPY_DST_BIT"
pattern PEER_MEMORY_FEATURE_COPY_DST_BIT    = PeerMemoryFeatureFlagBits 0x00000002
-- No documentation found for Nested "VkPeerMemoryFeatureFlagBits" "VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT"
pattern PEER_MEMORY_FEATURE_GENERIC_SRC_BIT = PeerMemoryFeatureFlagBits 0x00000004
-- No documentation found for Nested "VkPeerMemoryFeatureFlagBits" "VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT"
pattern PEER_MEMORY_FEATURE_GENERIC_DST_BIT = PeerMemoryFeatureFlagBits 0x00000008

conNamePeerMemoryFeatureFlagBits :: String
conNamePeerMemoryFeatureFlagBits = "PeerMemoryFeatureFlagBits"

enumPrefixPeerMemoryFeatureFlagBits :: String
enumPrefixPeerMemoryFeatureFlagBits = "PEER_MEMORY_FEATURE_"

showTablePeerMemoryFeatureFlagBits :: [(PeerMemoryFeatureFlagBits, String)]
showTablePeerMemoryFeatureFlagBits =
  [ (PEER_MEMORY_FEATURE_COPY_SRC_BIT   , "COPY_SRC_BIT")
  , (PEER_MEMORY_FEATURE_COPY_DST_BIT   , "COPY_DST_BIT")
  , (PEER_MEMORY_FEATURE_GENERIC_SRC_BIT, "GENERIC_SRC_BIT")
  , (PEER_MEMORY_FEATURE_GENERIC_DST_BIT, "GENERIC_DST_BIT")
  ]


instance Show PeerMemoryFeatureFlagBits where
showsPrec = enumShowsPrec enumPrefixPeerMemoryFeatureFlagBits
                          showTablePeerMemoryFeatureFlagBits
                          conNamePeerMemoryFeatureFlagBits
                          (\(PeerMemoryFeatureFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read PeerMemoryFeatureFlagBits where
  readPrec = enumReadPrec enumPrefixPeerMemoryFeatureFlagBits
                          showTablePeerMemoryFeatureFlagBits
                          conNamePeerMemoryFeatureFlagBits
                          PeerMemoryFeatureFlagBits

