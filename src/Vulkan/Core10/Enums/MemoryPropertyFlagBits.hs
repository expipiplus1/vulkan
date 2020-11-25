{-# language CPP #-}
-- No documentation found for Chapter "MemoryPropertyFlagBits"
module Vulkan.Core10.Enums.MemoryPropertyFlagBits  ( MemoryPropertyFlags
                                                   , MemoryPropertyFlagBits( MEMORY_PROPERTY_DEVICE_LOCAL_BIT
                                                                           , MEMORY_PROPERTY_HOST_VISIBLE_BIT
                                                                           , MEMORY_PROPERTY_HOST_COHERENT_BIT
                                                                           , MEMORY_PROPERTY_HOST_CACHED_BIT
                                                                           , MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT
                                                                           , MEMORY_PROPERTY_DEVICE_UNCACHED_BIT_AMD
                                                                           , MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD
                                                                           , MEMORY_PROPERTY_PROTECTED_BIT
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
type MemoryPropertyFlags = MemoryPropertyFlagBits

-- No documentation found for TopLevel "VkMemoryPropertyFlagBits"
newtype MemoryPropertyFlagBits = MemoryPropertyFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkMemoryPropertyFlagBits" "VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT"
pattern MEMORY_PROPERTY_DEVICE_LOCAL_BIT        = MemoryPropertyFlagBits 0x00000001
-- No documentation found for Nested "VkMemoryPropertyFlagBits" "VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT"
pattern MEMORY_PROPERTY_HOST_VISIBLE_BIT        = MemoryPropertyFlagBits 0x00000002
-- No documentation found for Nested "VkMemoryPropertyFlagBits" "VK_MEMORY_PROPERTY_HOST_COHERENT_BIT"
pattern MEMORY_PROPERTY_HOST_COHERENT_BIT       = MemoryPropertyFlagBits 0x00000004
-- No documentation found for Nested "VkMemoryPropertyFlagBits" "VK_MEMORY_PROPERTY_HOST_CACHED_BIT"
pattern MEMORY_PROPERTY_HOST_CACHED_BIT         = MemoryPropertyFlagBits 0x00000008
-- No documentation found for Nested "VkMemoryPropertyFlagBits" "VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT"
pattern MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT    = MemoryPropertyFlagBits 0x00000010
-- No documentation found for Nested "VkMemoryPropertyFlagBits" "VK_MEMORY_PROPERTY_DEVICE_UNCACHED_BIT_AMD"
pattern MEMORY_PROPERTY_DEVICE_UNCACHED_BIT_AMD = MemoryPropertyFlagBits 0x00000080
-- No documentation found for Nested "VkMemoryPropertyFlagBits" "VK_MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD"
pattern MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD = MemoryPropertyFlagBits 0x00000040
-- No documentation found for Nested "VkMemoryPropertyFlagBits" "VK_MEMORY_PROPERTY_PROTECTED_BIT"
pattern MEMORY_PROPERTY_PROTECTED_BIT           = MemoryPropertyFlagBits 0x00000020

conNameMemoryPropertyFlagBits :: String
conNameMemoryPropertyFlagBits = "MemoryPropertyFlagBits"

enumPrefixMemoryPropertyFlagBits :: String
enumPrefixMemoryPropertyFlagBits = "MEMORY_PROPERTY_"

showTableMemoryPropertyFlagBits :: [(MemoryPropertyFlagBits, String)]
showTableMemoryPropertyFlagBits =
  [ (MEMORY_PROPERTY_DEVICE_LOCAL_BIT       , "DEVICE_LOCAL_BIT")
  , (MEMORY_PROPERTY_HOST_VISIBLE_BIT       , "HOST_VISIBLE_BIT")
  , (MEMORY_PROPERTY_HOST_COHERENT_BIT      , "HOST_COHERENT_BIT")
  , (MEMORY_PROPERTY_HOST_CACHED_BIT        , "HOST_CACHED_BIT")
  , (MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT   , "LAZILY_ALLOCATED_BIT")
  , (MEMORY_PROPERTY_DEVICE_UNCACHED_BIT_AMD, "DEVICE_UNCACHED_BIT_AMD")
  , (MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD, "DEVICE_COHERENT_BIT_AMD")
  , (MEMORY_PROPERTY_PROTECTED_BIT          , "PROTECTED_BIT")
  ]


instance Show MemoryPropertyFlagBits where
showsPrec = enumShowsPrec enumPrefixMemoryPropertyFlagBits
                          showTableMemoryPropertyFlagBits
                          conNameMemoryPropertyFlagBits
                          (\(MemoryPropertyFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read MemoryPropertyFlagBits where
  readPrec = enumReadPrec enumPrefixMemoryPropertyFlagBits
                          showTableMemoryPropertyFlagBits
                          conNameMemoryPropertyFlagBits
                          MemoryPropertyFlagBits

