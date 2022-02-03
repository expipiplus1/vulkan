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
import Vulkan.Zero (Zero)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
type PeerMemoryFeatureFlags = PeerMemoryFeatureFlagBits

-- | VkPeerMemoryFeatureFlagBits - Bitmask specifying supported peer memory
-- features
--
-- = Description
--
-- Note
--
-- The peer memory features of a memory heap also apply to any accesses
-- that /may/ be performed during
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transitions>.
--
-- 'PEER_MEMORY_FEATURE_COPY_DST_BIT' /must/ be supported for all host
-- local heaps and for at least one device-local memory heap.
--
-- If a device does not support a peer memory feature, it is still valid to
-- use a resource that includes both local and peer memory bindings with
-- the corresponding access type as long as only the local bindings are
-- actually accessed. For example, an application doing split-frame
-- rendering would use framebuffer attachments that include both local and
-- peer memory bindings, but would scissor the rendering to only update
-- local memory.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'PeerMemoryFeatureFlags'
newtype PeerMemoryFeatureFlagBits = PeerMemoryFeatureFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'PEER_MEMORY_FEATURE_COPY_SRC_BIT' specifies that the memory /can/ be
-- accessed as the source of any @vkCmdCopy*@ command.
pattern PEER_MEMORY_FEATURE_COPY_SRC_BIT    = PeerMemoryFeatureFlagBits 0x00000001
-- | 'PEER_MEMORY_FEATURE_COPY_DST_BIT' specifies that the memory /can/ be
-- accessed as the destination of any @vkCmdCopy*@ command.
pattern PEER_MEMORY_FEATURE_COPY_DST_BIT    = PeerMemoryFeatureFlagBits 0x00000002
-- | 'PEER_MEMORY_FEATURE_GENERIC_SRC_BIT' specifies that the memory /can/ be
-- read as any memory access type.
pattern PEER_MEMORY_FEATURE_GENERIC_SRC_BIT = PeerMemoryFeatureFlagBits 0x00000004
-- | 'PEER_MEMORY_FEATURE_GENERIC_DST_BIT' specifies that the memory /can/ be
-- written as any memory access type. Shader atomics are considered to be
-- writes.
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

