{-# language CPP #-}
-- No documentation found for Chapter "QueueFlagBits"
module Vulkan.Core10.Enums.QueueFlagBits  ( QueueFlags
                                          , QueueFlagBits( QUEUE_GRAPHICS_BIT
                                                         , QUEUE_COMPUTE_BIT
                                                         , QUEUE_TRANSFER_BIT
                                                         , QUEUE_SPARSE_BINDING_BIT
                                                         , QUEUE_PROTECTED_BIT
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
type QueueFlags = QueueFlagBits

-- | VkQueueFlagBits - Bitmask specifying capabilities of queues in a queue
-- family
--
-- = Description
--
-- If an implementation exposes any queue family that supports graphics
-- operations, at least one queue family of at least one physical device
-- exposed by the implementation /must/ support both graphics and compute
-- operations.
--
-- Furthermore, if the protected memory physical device feature is
-- supported, then at least one queue family of at least one physical
-- device exposed by the implementation /must/ support graphics operations,
-- compute operations, and protected memory operations.
--
-- Note
--
-- All commands that are allowed on a queue that supports transfer
-- operations are also allowed on a queue that supports either graphics or
-- compute operations. Thus, if the capabilities of a queue family include
-- 'QUEUE_GRAPHICS_BIT' or 'QUEUE_COMPUTE_BIT', then reporting the
-- 'QUEUE_TRANSFER_BIT' capability separately for that queue family is
-- /optional/.
--
-- For further details see
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#devsandqueues-queues Queues>.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'QueueFlags'
newtype QueueFlagBits = QueueFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'QUEUE_GRAPHICS_BIT' specifies that queues in this queue family support
-- graphics operations.
pattern QUEUE_GRAPHICS_BIT       = QueueFlagBits 0x00000001
-- | 'QUEUE_COMPUTE_BIT' specifies that queues in this queue family support
-- compute operations.
pattern QUEUE_COMPUTE_BIT        = QueueFlagBits 0x00000002
-- | 'QUEUE_TRANSFER_BIT' specifies that queues in this queue family support
-- transfer operations.
pattern QUEUE_TRANSFER_BIT       = QueueFlagBits 0x00000004
-- | 'QUEUE_SPARSE_BINDING_BIT' specifies that queues in this queue family
-- support sparse memory management operations (see
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#sparsememory Sparse Resources>).
-- If any of the sparse resource features are enabled, then at least one
-- queue family /must/ support this bit.
pattern QUEUE_SPARSE_BINDING_BIT = QueueFlagBits 0x00000008
-- | 'QUEUE_PROTECTED_BIT' specifies that queues in this queue family support
-- the
-- 'Vulkan.Core10.Enums.DeviceQueueCreateFlagBits.DEVICE_QUEUE_CREATE_PROTECTED_BIT'
-- bit. (see
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-protected-memory Protected Memory>).
-- If the physical device supports the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-protectedMemory protectedMemory>
-- feature, at least one of its queue families /must/ support this bit.
pattern QUEUE_PROTECTED_BIT      = QueueFlagBits 0x00000010

conNameQueueFlagBits :: String
conNameQueueFlagBits = "QueueFlagBits"

enumPrefixQueueFlagBits :: String
enumPrefixQueueFlagBits = "QUEUE_"

showTableQueueFlagBits :: [(QueueFlagBits, String)]
showTableQueueFlagBits =
  [ (QUEUE_GRAPHICS_BIT      , "GRAPHICS_BIT")
  , (QUEUE_COMPUTE_BIT       , "COMPUTE_BIT")
  , (QUEUE_TRANSFER_BIT      , "TRANSFER_BIT")
  , (QUEUE_SPARSE_BINDING_BIT, "SPARSE_BINDING_BIT")
  , (QUEUE_PROTECTED_BIT     , "PROTECTED_BIT")
  ]

instance Show QueueFlagBits where
  showsPrec = enumShowsPrec enumPrefixQueueFlagBits
                            showTableQueueFlagBits
                            conNameQueueFlagBits
                            (\(QueueFlagBits x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read QueueFlagBits where
  readPrec = enumReadPrec enumPrefixQueueFlagBits showTableQueueFlagBits conNameQueueFlagBits QueueFlagBits

