{-# language CPP #-}
-- No documentation found for Chapter "MemoryAllocateFlagBits"
module Vulkan.Core11.Enums.MemoryAllocateFlagBits  ( MemoryAllocateFlags
                                                   , MemoryAllocateFlagBits( MEMORY_ALLOCATE_DEVICE_MASK_BIT
                                                                           , MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT
                                                                           , MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT
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
type MemoryAllocateFlags = MemoryAllocateFlagBits

-- No documentation found for TopLevel "VkMemoryAllocateFlagBits"
newtype MemoryAllocateFlagBits = MemoryAllocateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkMemoryAllocateFlagBits" "VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT"
pattern MEMORY_ALLOCATE_DEVICE_MASK_BIT                   = MemoryAllocateFlagBits 0x00000001
-- No documentation found for Nested "VkMemoryAllocateFlagBits" "VK_MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT"
pattern MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT = MemoryAllocateFlagBits 0x00000004
-- No documentation found for Nested "VkMemoryAllocateFlagBits" "VK_MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT"
pattern MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT                = MemoryAllocateFlagBits 0x00000002

conNameMemoryAllocateFlagBits :: String
conNameMemoryAllocateFlagBits = "MemoryAllocateFlagBits"

enumPrefixMemoryAllocateFlagBits :: String
enumPrefixMemoryAllocateFlagBits = "MEMORY_ALLOCATE_DEVICE_"

showTableMemoryAllocateFlagBits :: [(MemoryAllocateFlagBits, String)]
showTableMemoryAllocateFlagBits =
  [ (MEMORY_ALLOCATE_DEVICE_MASK_BIT                  , "MASK_BIT")
  , (MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT, "ADDRESS_CAPTURE_REPLAY_BIT")
  , (MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT               , "ADDRESS_BIT")
  ]


instance Show MemoryAllocateFlagBits where
showsPrec = enumShowsPrec enumPrefixMemoryAllocateFlagBits
                          showTableMemoryAllocateFlagBits
                          conNameMemoryAllocateFlagBits
                          (\(MemoryAllocateFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read MemoryAllocateFlagBits where
  readPrec = enumReadPrec enumPrefixMemoryAllocateFlagBits
                          showTableMemoryAllocateFlagBits
                          conNameMemoryAllocateFlagBits
                          MemoryAllocateFlagBits

