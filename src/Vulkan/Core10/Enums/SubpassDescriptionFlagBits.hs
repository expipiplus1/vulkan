{-# language CPP #-}
-- No documentation found for Chapter "SubpassDescriptionFlagBits"
module Vulkan.Core10.Enums.SubpassDescriptionFlagBits  ( SubpassDescriptionFlags
                                                       , SubpassDescriptionFlagBits( SUBPASS_DESCRIPTION_SHADER_RESOLVE_BIT_QCOM
                                                                                   , SUBPASS_DESCRIPTION_FRAGMENT_REGION_BIT_QCOM
                                                                                   , SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX
                                                                                   , SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX
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
type SubpassDescriptionFlags = SubpassDescriptionFlagBits

-- No documentation found for TopLevel "VkSubpassDescriptionFlagBits"
newtype SubpassDescriptionFlagBits = SubpassDescriptionFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkSubpassDescriptionFlagBits" "VK_SUBPASS_DESCRIPTION_SHADER_RESOLVE_BIT_QCOM"
pattern SUBPASS_DESCRIPTION_SHADER_RESOLVE_BIT_QCOM          = SubpassDescriptionFlagBits 0x00000008
-- No documentation found for Nested "VkSubpassDescriptionFlagBits" "VK_SUBPASS_DESCRIPTION_FRAGMENT_REGION_BIT_QCOM"
pattern SUBPASS_DESCRIPTION_FRAGMENT_REGION_BIT_QCOM         = SubpassDescriptionFlagBits 0x00000004
-- No documentation found for Nested "VkSubpassDescriptionFlagBits" "VK_SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX"
pattern SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX = SubpassDescriptionFlagBits 0x00000002
-- No documentation found for Nested "VkSubpassDescriptionFlagBits" "VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX"
pattern SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX      = SubpassDescriptionFlagBits 0x00000001

conNameSubpassDescriptionFlagBits :: String
conNameSubpassDescriptionFlagBits = "SubpassDescriptionFlagBits"

enumPrefixSubpassDescriptionFlagBits :: String
enumPrefixSubpassDescriptionFlagBits = "SUBPASS_DESCRIPTION_"

showTableSubpassDescriptionFlagBits :: [(SubpassDescriptionFlagBits, String)]
showTableSubpassDescriptionFlagBits =
  [ (SUBPASS_DESCRIPTION_SHADER_RESOLVE_BIT_QCOM         , "SHADER_RESOLVE_BIT_QCOM")
  , (SUBPASS_DESCRIPTION_FRAGMENT_REGION_BIT_QCOM        , "FRAGMENT_REGION_BIT_QCOM")
  , (SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX, "PER_VIEW_POSITION_X_ONLY_BIT_NVX")
  , (SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX     , "PER_VIEW_ATTRIBUTES_BIT_NVX")
  ]


instance Show SubpassDescriptionFlagBits where
showsPrec = enumShowsPrec enumPrefixSubpassDescriptionFlagBits
                          showTableSubpassDescriptionFlagBits
                          conNameSubpassDescriptionFlagBits
                          (\(SubpassDescriptionFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read SubpassDescriptionFlagBits where
  readPrec = enumReadPrec enumPrefixSubpassDescriptionFlagBits
                          showTableSubpassDescriptionFlagBits
                          conNameSubpassDescriptionFlagBits
                          SubpassDescriptionFlagBits

