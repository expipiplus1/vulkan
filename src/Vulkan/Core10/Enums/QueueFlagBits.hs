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
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
type QueueFlags = QueueFlagBits

-- No documentation found for TopLevel "VkQueueFlagBits"
newtype QueueFlagBits = QueueFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkQueueFlagBits" "VK_QUEUE_GRAPHICS_BIT"
pattern QUEUE_GRAPHICS_BIT       = QueueFlagBits 0x00000001
-- No documentation found for Nested "VkQueueFlagBits" "VK_QUEUE_COMPUTE_BIT"
pattern QUEUE_COMPUTE_BIT        = QueueFlagBits 0x00000002
-- No documentation found for Nested "VkQueueFlagBits" "VK_QUEUE_TRANSFER_BIT"
pattern QUEUE_TRANSFER_BIT       = QueueFlagBits 0x00000004
-- No documentation found for Nested "VkQueueFlagBits" "VK_QUEUE_SPARSE_BINDING_BIT"
pattern QUEUE_SPARSE_BINDING_BIT = QueueFlagBits 0x00000008
-- No documentation found for Nested "VkQueueFlagBits" "VK_QUEUE_PROTECTED_BIT"
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

