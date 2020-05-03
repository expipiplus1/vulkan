{-# language CPP #-}
module Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits  ( PeerMemoryFeatureFlagBits( PEER_MEMORY_FEATURE_COPY_SRC_BIT
                                                                                 , PEER_MEMORY_FEATURE_COPY_DST_BIT
                                                                                 , PEER_MEMORY_FEATURE_GENERIC_SRC_BIT
                                                                                 , PEER_MEMORY_FEATURE_GENERIC_DST_BIT
                                                                                 , ..
                                                                                 )
                                                      , PeerMemoryFeatureFlags
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
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Core10.BaseType (Flags)
import Vulkan.Zero (Zero)
-- | VkPeerMemoryFeatureFlagBits - Bitmask specifying supported peer memory
-- features
--
-- = Description
--
-- Note
--
-- The peer memory features of a memory heap also apply to any accesses
-- that /may/ be performed during
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transitions>.
--
-- 'PEER_MEMORY_FEATURE_COPY_DST_BIT' /must/ be supported for all host
-- local heaps and for at least one device local heap.
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
-- 'PeerMemoryFeatureFlags'
newtype PeerMemoryFeatureFlagBits = PeerMemoryFeatureFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'PEER_MEMORY_FEATURE_COPY_SRC_BIT' specifies that the memory /can/ be
-- accessed as the source of a
-- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyBuffer',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyImage',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyBufferToImage', or
-- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyImageToBuffer' command.
pattern PEER_MEMORY_FEATURE_COPY_SRC_BIT = PeerMemoryFeatureFlagBits 0x00000001
-- | 'PEER_MEMORY_FEATURE_COPY_DST_BIT' specifies that the memory /can/ be
-- accessed as the destination of a
-- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyBuffer',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyImage',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyBufferToImage', or
-- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyImageToBuffer' command.
pattern PEER_MEMORY_FEATURE_COPY_DST_BIT = PeerMemoryFeatureFlagBits 0x00000002
-- | 'PEER_MEMORY_FEATURE_GENERIC_SRC_BIT' specifies that the memory /can/ be
-- read as any memory access type.
pattern PEER_MEMORY_FEATURE_GENERIC_SRC_BIT = PeerMemoryFeatureFlagBits 0x00000004
-- | 'PEER_MEMORY_FEATURE_GENERIC_DST_BIT' specifies that the memory /can/ be
-- written as any memory access type. Shader atomics are considered to be
-- writes.
pattern PEER_MEMORY_FEATURE_GENERIC_DST_BIT = PeerMemoryFeatureFlagBits 0x00000008

type PeerMemoryFeatureFlags = PeerMemoryFeatureFlagBits

instance Show PeerMemoryFeatureFlagBits where
  showsPrec p = \case
    PEER_MEMORY_FEATURE_COPY_SRC_BIT -> showString "PEER_MEMORY_FEATURE_COPY_SRC_BIT"
    PEER_MEMORY_FEATURE_COPY_DST_BIT -> showString "PEER_MEMORY_FEATURE_COPY_DST_BIT"
    PEER_MEMORY_FEATURE_GENERIC_SRC_BIT -> showString "PEER_MEMORY_FEATURE_GENERIC_SRC_BIT"
    PEER_MEMORY_FEATURE_GENERIC_DST_BIT -> showString "PEER_MEMORY_FEATURE_GENERIC_DST_BIT"
    PeerMemoryFeatureFlagBits x -> showParen (p >= 11) (showString "PeerMemoryFeatureFlagBits 0x" . showHex x)

instance Read PeerMemoryFeatureFlagBits where
  readPrec = parens (choose [("PEER_MEMORY_FEATURE_COPY_SRC_BIT", pure PEER_MEMORY_FEATURE_COPY_SRC_BIT)
                            , ("PEER_MEMORY_FEATURE_COPY_DST_BIT", pure PEER_MEMORY_FEATURE_COPY_DST_BIT)
                            , ("PEER_MEMORY_FEATURE_GENERIC_SRC_BIT", pure PEER_MEMORY_FEATURE_GENERIC_SRC_BIT)
                            , ("PEER_MEMORY_FEATURE_GENERIC_DST_BIT", pure PEER_MEMORY_FEATURE_GENERIC_DST_BIT)]
                     +++
                     prec 10 (do
                       expectP (Ident "PeerMemoryFeatureFlagBits")
                       v <- step readPrec
                       pure (PeerMemoryFeatureFlagBits v)))

