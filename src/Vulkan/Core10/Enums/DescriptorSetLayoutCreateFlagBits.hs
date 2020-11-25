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

-- | VkDescriptorSetLayoutCreateFlagBits - Bitmask specifying descriptor set
-- layout properties
--
-- = See Also
--
-- 'DescriptorSetLayoutCreateFlags'
newtype DescriptorSetLayoutCreateFlagBits = DescriptorSetLayoutCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR' specifies that
-- descriptor sets /must/ not be allocated using this layout, and
-- descriptors are instead pushed by
-- 'Vulkan.Extensions.VK_KHR_push_descriptor.cmdPushDescriptorSetKHR'.
pattern DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR    = DescriptorSetLayoutCreateFlagBits 0x00000001
-- | 'DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT' specifies that
-- descriptor sets using this layout /must/ be allocated from a descriptor
-- pool created with the
-- 'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT'
-- bit set. Descriptor set layouts created with this bit set have alternate
-- limits for the maximum number of descriptors per-stage and per-pipeline
-- layout. The non-UpdateAfterBind limits only count descriptors in sets
-- created without this flag. The UpdateAfterBind limits count all
-- descriptors, but the limits /may/ be higher than the non-UpdateAfterBind
-- limits.
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

