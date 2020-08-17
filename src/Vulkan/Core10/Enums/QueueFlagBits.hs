{-# language CPP #-}
module Vulkan.Core10.Enums.QueueFlagBits  ( QueueFlagBits( QUEUE_GRAPHICS_BIT
                                                         , QUEUE_COMPUTE_BIT
                                                         , QUEUE_TRANSFER_BIT
                                                         , QUEUE_SPARSE_BINDING_BIT
                                                         , QUEUE_PROTECTED_BIT
                                                         , ..
                                                         )
                                          , QueueFlags
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
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
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
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#sparsememory Sparse Resources>).
--     If any of the sparse resource features are enabled, then at least
--     one queue family /must/ support this bit.
--
-- -   if 'QUEUE_PROTECTED_BIT' is set, then the queues in this queue
--     family support the
--     'Vulkan.Core10.Enums.DeviceQueueCreateFlagBits.DEVICE_QUEUE_CREATE_PROTECTED_BIT'
--     bit. (see
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-protected-memory Protected Memory>).
--     If the physical device supports the @protectedMemory@ feature, at
--     least one of its queue families /must/ support this bit.
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-queues Queues>.
--
-- = See Also
--
-- 'QueueFlags'
newtype QueueFlagBits = QueueFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- No documentation found for Nested "VkQueueFlagBits" "VK_QUEUE_GRAPHICS_BIT"
pattern QUEUE_GRAPHICS_BIT = QueueFlagBits 0x00000001
-- No documentation found for Nested "VkQueueFlagBits" "VK_QUEUE_COMPUTE_BIT"
pattern QUEUE_COMPUTE_BIT = QueueFlagBits 0x00000002
-- No documentation found for Nested "VkQueueFlagBits" "VK_QUEUE_TRANSFER_BIT"
pattern QUEUE_TRANSFER_BIT = QueueFlagBits 0x00000004
-- No documentation found for Nested "VkQueueFlagBits" "VK_QUEUE_SPARSE_BINDING_BIT"
pattern QUEUE_SPARSE_BINDING_BIT = QueueFlagBits 0x00000008
-- No documentation found for Nested "VkQueueFlagBits" "VK_QUEUE_PROTECTED_BIT"
pattern QUEUE_PROTECTED_BIT = QueueFlagBits 0x00000010

type QueueFlags = QueueFlagBits

instance Show QueueFlagBits where
  showsPrec p = \case
    QUEUE_GRAPHICS_BIT -> showString "QUEUE_GRAPHICS_BIT"
    QUEUE_COMPUTE_BIT -> showString "QUEUE_COMPUTE_BIT"
    QUEUE_TRANSFER_BIT -> showString "QUEUE_TRANSFER_BIT"
    QUEUE_SPARSE_BINDING_BIT -> showString "QUEUE_SPARSE_BINDING_BIT"
    QUEUE_PROTECTED_BIT -> showString "QUEUE_PROTECTED_BIT"
    QueueFlagBits x -> showParen (p >= 11) (showString "QueueFlagBits 0x" . showHex x)

instance Read QueueFlagBits where
  readPrec = parens (choose [("QUEUE_GRAPHICS_BIT", pure QUEUE_GRAPHICS_BIT)
                            , ("QUEUE_COMPUTE_BIT", pure QUEUE_COMPUTE_BIT)
                            , ("QUEUE_TRANSFER_BIT", pure QUEUE_TRANSFER_BIT)
                            , ("QUEUE_SPARSE_BINDING_BIT", pure QUEUE_SPARSE_BINDING_BIT)
                            , ("QUEUE_PROTECTED_BIT", pure QUEUE_PROTECTED_BIT)]
                     +++
                     prec 10 (do
                       expectP (Ident "QueueFlagBits")
                       v <- step readPrec
                       pure (QueueFlagBits v)))

