{-# language CPP #-}
-- No documentation found for Chapter "DescriptorPoolCreateFlagBits"
module Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits  ( DescriptorPoolCreateFlags
                                                         , DescriptorPoolCreateFlagBits( DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
                                                                                       , DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT
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
type DescriptorPoolCreateFlags = DescriptorPoolCreateFlagBits

-- No documentation found for TopLevel "VkDescriptorPoolCreateFlagBits"
newtype DescriptorPoolCreateFlagBits = DescriptorPoolCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkDescriptorPoolCreateFlagBits" "VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT"
pattern DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT = DescriptorPoolCreateFlagBits 0x00000001
-- No documentation found for Nested "VkDescriptorPoolCreateFlagBits" "VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT"
pattern DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT   = DescriptorPoolCreateFlagBits 0x00000002

conNameDescriptorPoolCreateFlagBits :: String
conNameDescriptorPoolCreateFlagBits = "DescriptorPoolCreateFlagBits"

enumPrefixDescriptorPoolCreateFlagBits :: String
enumPrefixDescriptorPoolCreateFlagBits = "DESCRIPTOR_POOL_CREATE_"

showTableDescriptorPoolCreateFlagBits :: [(DescriptorPoolCreateFlagBits, String)]
showTableDescriptorPoolCreateFlagBits =
  [ (DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT, "FREE_DESCRIPTOR_SET_BIT")
  , (DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT  , "UPDATE_AFTER_BIND_BIT")
  ]


instance Show DescriptorPoolCreateFlagBits where
showsPrec = enumShowsPrec enumPrefixDescriptorPoolCreateFlagBits
                          showTableDescriptorPoolCreateFlagBits
                          conNameDescriptorPoolCreateFlagBits
                          (\(DescriptorPoolCreateFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read DescriptorPoolCreateFlagBits where
  readPrec = enumReadPrec enumPrefixDescriptorPoolCreateFlagBits
                          showTableDescriptorPoolCreateFlagBits
                          conNameDescriptorPoolCreateFlagBits
                          DescriptorPoolCreateFlagBits

