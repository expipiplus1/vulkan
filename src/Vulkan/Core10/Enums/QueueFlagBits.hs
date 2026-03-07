{-# language CPP #-}
-- No documentation found for Chapter "QueueFlagBits"
module Vulkan.Core10.Enums.QueueFlagBits  ( QueueFlags
                                          , QueueFlagBits( QUEUE_GRAPHICS_BIT
                                                         , QUEUE_COMPUTE_BIT
                                                         , QUEUE_TRANSFER_BIT
                                                         , QUEUE_SPARSE_BINDING_BIT
                                                         , QUEUE_DATA_GRAPH_BIT_ARM
                                                         , QUEUE_OPTICAL_FLOW_BIT_NV
                                                         , QUEUE_PROTECTED_BIT
                                                         , ..
                                                         )
                                          ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Vulkan.Zero (Zero)
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
-- -   'QUEUE_GRAPHICS_BIT' specifies that queues in this queue family
--     support graphics operations.
--
-- -   'QUEUE_COMPUTE_BIT' specifies that queues in this queue family
--     support compute operations.
--
-- -   'QUEUE_TRANSFER_BIT' specifies that queues in this queue family
--     support transfer operations.
--
-- -   'QUEUE_SPARSE_BINDING_BIT' specifies that queues in this queue
--     family support sparse memory management operations (see
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#sparsememory Sparse Resources>).
--     If any of the sparse resource features are supported, then at least
--     one queue family /must/ support this bit.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits VK_QUEUE_VIDEO_DECODE_BIT_KHR>
--     specifies that queues in this queue family support
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#video-decode-operations video decode operations>.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits VK_QUEUE_VIDEO_ENCODE_BIT_KHR>
--     specifies that queues in this queue family support
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#video-encode-operations video encode operations>.
--
-- -   'QUEUE_OPTICAL_FLOW_BIT_NV' specifies that queues in this queue
--     family support optical flow operations.
--
-- -   'QUEUE_DATA_GRAPH_BIT_ARM' specifies that queues in this queue
--     family support
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#graphs-operations data graph operations>.
--
-- -   'QUEUE_PROTECTED_BIT' specifies that queues in this queue family
--     support the
--     'Vulkan.Core10.Enums.DeviceQueueCreateFlagBits.DEVICE_QUEUE_CREATE_PROTECTED_BIT'
--     bit. (see
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#memory-protected-memory Protected Memory>).
--     If the physical device supports the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-protectedMemory protectedMemory>
--     feature, at least one of its queue families /must/ support this bit.
--
-- At least one queue family of at least one physical device exposed by the
-- implementation /must/ support at least one of the following sets of
-- operations:
--
-- -   graphics operations
--
-- -   compute operations
--
-- -   video encode operations
--
-- -   video decode operations
--
-- If an implementation exposes any queue family that supports graphics
-- operations, at least one queue family of at least one physical device
-- exposed by the implementation /must/ support both graphics and compute
-- operations.
--
-- Furthermore, if the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-protectedMemory protectedMemory>
-- physical device feature is supported, then at least one queue family of
-- at least one physical device exposed by the implementation /must/
-- support graphics operations, compute operations, and protected memory
-- operations.
--
-- All commands that are allowed on a queue that supports transfer
-- operations are also allowed on a queue that supports either graphics or
-- compute operations. Thus, if the capabilities of a queue family include
-- 'QUEUE_GRAPHICS_BIT' or 'QUEUE_COMPUTE_BIT', then reporting the
-- 'QUEUE_TRANSFER_BIT' capability separately for that queue family is
-- /optional/.
--
-- For further details see
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#devsandqueues-queues Queues>.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'QueueFlags'
newtype QueueFlagBits = QueueFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkQueueFlagBits" "VK_QUEUE_GRAPHICS_BIT"
pattern QUEUE_GRAPHICS_BIT = QueueFlagBits 0x00000001

-- No documentation found for Nested "VkQueueFlagBits" "VK_QUEUE_COMPUTE_BIT"
pattern QUEUE_COMPUTE_BIT = QueueFlagBits 0x00000002

-- No documentation found for Nested "VkQueueFlagBits" "VK_QUEUE_TRANSFER_BIT"
pattern QUEUE_TRANSFER_BIT = QueueFlagBits 0x00000004

-- No documentation found for Nested "VkQueueFlagBits" "VK_QUEUE_SPARSE_BINDING_BIT"
pattern QUEUE_SPARSE_BINDING_BIT = QueueFlagBits 0x00000008

-- No documentation found for Nested "VkQueueFlagBits" "VK_QUEUE_DATA_GRAPH_BIT_ARM"
pattern QUEUE_DATA_GRAPH_BIT_ARM = QueueFlagBits 0x00000400

-- No documentation found for Nested "VkQueueFlagBits" "VK_QUEUE_OPTICAL_FLOW_BIT_NV"
pattern QUEUE_OPTICAL_FLOW_BIT_NV = QueueFlagBits 0x00000100

-- No documentation found for Nested "VkQueueFlagBits" "VK_QUEUE_PROTECTED_BIT"
pattern QUEUE_PROTECTED_BIT = QueueFlagBits 0x00000010

conNameQueueFlagBits :: String
conNameQueueFlagBits = "QueueFlagBits"

enumPrefixQueueFlagBits :: String
enumPrefixQueueFlagBits = "QUEUE_"

showTableQueueFlagBits :: [(QueueFlagBits, String)]
showTableQueueFlagBits =
  [ (QUEUE_GRAPHICS_BIT, "GRAPHICS_BIT")
  , (QUEUE_COMPUTE_BIT, "COMPUTE_BIT")
  , (QUEUE_TRANSFER_BIT, "TRANSFER_BIT")
  , (QUEUE_SPARSE_BINDING_BIT, "SPARSE_BINDING_BIT")
  , (QUEUE_DATA_GRAPH_BIT_ARM, "DATA_GRAPH_BIT_ARM")
  , (QUEUE_OPTICAL_FLOW_BIT_NV, "OPTICAL_FLOW_BIT_NV")
  , (QUEUE_PROTECTED_BIT, "PROTECTED_BIT")
  ]

instance Show QueueFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixQueueFlagBits
      showTableQueueFlagBits
      conNameQueueFlagBits
      (\(QueueFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read QueueFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixQueueFlagBits
      showTableQueueFlagBits
      conNameQueueFlagBits
      QueueFlagBits
