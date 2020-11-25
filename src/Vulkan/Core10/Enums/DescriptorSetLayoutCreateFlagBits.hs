{-# language CPP #-}
-- No documentation found for Chapter "DescriptorSetLayoutCreateFlagBits"
module Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits  ( DescriptorSetLayoutCreateFlagBits( DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR
                                                                                                 , DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT
                                                                                                 , ..
                                                                                                 )
                                                              , DescriptorSetLayoutCreateFlags
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
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
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
pattern DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR = DescriptorSetLayoutCreateFlagBits 0x00000001
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

type DescriptorSetLayoutCreateFlags = DescriptorSetLayoutCreateFlagBits

instance Show DescriptorSetLayoutCreateFlagBits where
  showsPrec p = \case
    DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR -> showString "DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR"
    DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT -> showString "DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT"
    DescriptorSetLayoutCreateFlagBits x -> showParen (p >= 11) (showString "DescriptorSetLayoutCreateFlagBits 0x" . showHex x)

instance Read DescriptorSetLayoutCreateFlagBits where
  readPrec = parens (choose [("DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR", pure DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR)
                            , ("DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT", pure DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT)]
                     +++
                     prec 10 (do
                       expectP (Ident "DescriptorSetLayoutCreateFlagBits")
                       v <- step readPrec
                       pure (DescriptorSetLayoutCreateFlagBits v)))

