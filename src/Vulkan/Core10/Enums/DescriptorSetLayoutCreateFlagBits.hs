{-# language CPP #-}
-- No documentation found for Chapter "DescriptorSetLayoutCreateFlagBits"
module Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits  ( DescriptorSetLayoutCreateFlags
                                                              , DescriptorSetLayoutCreateFlagBits( DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR
                                                                                                 , DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT
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
type DescriptorSetLayoutCreateFlags = DescriptorSetLayoutCreateFlagBits

-- No documentation found for TopLevel "VkDescriptorSetLayoutCreateFlagBits"
newtype DescriptorSetLayoutCreateFlagBits = DescriptorSetLayoutCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkDescriptorSetLayoutCreateFlagBits" "VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR"
pattern DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR    = DescriptorSetLayoutCreateFlagBits 0x00000001
-- No documentation found for Nested "VkDescriptorSetLayoutCreateFlagBits" "VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT"
pattern DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT = DescriptorSetLayoutCreateFlagBits 0x00000002

conNameDescriptorSetLayoutCreateFlagBits :: String
conNameDescriptorSetLayoutCreateFlagBits = "DescriptorSetLayoutCreateFlagBits"

enumPrefixDescriptorSetLayoutCreateFlagBits :: String
enumPrefixDescriptorSetLayoutCreateFlagBits = "DESCRIPTOR_SET_LAYOUT_CREATE_"

showTableDescriptorSetLayoutCreateFlagBits :: [(DescriptorSetLayoutCreateFlagBits, String)]
showTableDescriptorSetLayoutCreateFlagBits =
  [ (DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR   , "PUSH_DESCRIPTOR_BIT_KHR")
  , (DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT, "UPDATE_AFTER_BIND_POOL_BIT")
  ]


instance Show DescriptorSetLayoutCreateFlagBits where
showsPrec = enumShowsPrec enumPrefixDescriptorSetLayoutCreateFlagBits
                          showTableDescriptorSetLayoutCreateFlagBits
                          conNameDescriptorSetLayoutCreateFlagBits
                          (\(DescriptorSetLayoutCreateFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read DescriptorSetLayoutCreateFlagBits where
  readPrec = enumReadPrec enumPrefixDescriptorSetLayoutCreateFlagBits
                          showTableDescriptorSetLayoutCreateFlagBits
                          conNameDescriptorSetLayoutCreateFlagBits
                          DescriptorSetLayoutCreateFlagBits

