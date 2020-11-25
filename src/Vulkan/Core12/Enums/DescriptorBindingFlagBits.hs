{-# language CPP #-}
-- No documentation found for Chapter "DescriptorBindingFlagBits"
module Vulkan.Core12.Enums.DescriptorBindingFlagBits  ( DescriptorBindingFlags
                                                      , DescriptorBindingFlagBits( DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT
                                                                                 , DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT
                                                                                 , DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT
                                                                                 , DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT
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
type DescriptorBindingFlags = DescriptorBindingFlagBits

-- No documentation found for TopLevel "VkDescriptorBindingFlagBits"
newtype DescriptorBindingFlagBits = DescriptorBindingFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkDescriptorBindingFlagBits" "VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT"
pattern DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT           = DescriptorBindingFlagBits 0x00000001
-- No documentation found for Nested "VkDescriptorBindingFlagBits" "VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT"
pattern DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT = DescriptorBindingFlagBits 0x00000002
-- No documentation found for Nested "VkDescriptorBindingFlagBits" "VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT"
pattern DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT             = DescriptorBindingFlagBits 0x00000004
-- No documentation found for Nested "VkDescriptorBindingFlagBits" "VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT"
pattern DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT   = DescriptorBindingFlagBits 0x00000008

conNameDescriptorBindingFlagBits :: String
conNameDescriptorBindingFlagBits = "DescriptorBindingFlagBits"

enumPrefixDescriptorBindingFlagBits :: String
enumPrefixDescriptorBindingFlagBits = "DESCRIPTOR_BINDING_"

showTableDescriptorBindingFlagBits :: [(DescriptorBindingFlagBits, String)]
showTableDescriptorBindingFlagBits =
  [ (DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT          , "UPDATE_AFTER_BIND_BIT")
  , (DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT, "UPDATE_UNUSED_WHILE_PENDING_BIT")
  , (DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT            , "PARTIALLY_BOUND_BIT")
  , (DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT  , "VARIABLE_DESCRIPTOR_COUNT_BIT")
  ]


instance Show DescriptorBindingFlagBits where
showsPrec = enumShowsPrec enumPrefixDescriptorBindingFlagBits
                          showTableDescriptorBindingFlagBits
                          conNameDescriptorBindingFlagBits
                          (\(DescriptorBindingFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read DescriptorBindingFlagBits where
  readPrec = enumReadPrec enumPrefixDescriptorBindingFlagBits
                          showTableDescriptorBindingFlagBits
                          conNameDescriptorBindingFlagBits
                          DescriptorBindingFlagBits

