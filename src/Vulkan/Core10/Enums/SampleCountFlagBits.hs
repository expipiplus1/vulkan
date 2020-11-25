{-# language CPP #-}
-- No documentation found for Chapter "SampleCountFlagBits"
module Vulkan.Core10.Enums.SampleCountFlagBits  ( SampleCountFlags
                                                , SampleCountFlagBits( SAMPLE_COUNT_1_BIT
                                                                     , SAMPLE_COUNT_2_BIT
                                                                     , SAMPLE_COUNT_4_BIT
                                                                     , SAMPLE_COUNT_8_BIT
                                                                     , SAMPLE_COUNT_16_BIT
                                                                     , SAMPLE_COUNT_32_BIT
                                                                     , SAMPLE_COUNT_64_BIT
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
type SampleCountFlags = SampleCountFlagBits

-- No documentation found for TopLevel "VkSampleCountFlagBits"
newtype SampleCountFlagBits = SampleCountFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkSampleCountFlagBits" "VK_SAMPLE_COUNT_1_BIT"
pattern SAMPLE_COUNT_1_BIT  = SampleCountFlagBits 0x00000001
-- No documentation found for Nested "VkSampleCountFlagBits" "VK_SAMPLE_COUNT_2_BIT"
pattern SAMPLE_COUNT_2_BIT  = SampleCountFlagBits 0x00000002
-- No documentation found for Nested "VkSampleCountFlagBits" "VK_SAMPLE_COUNT_4_BIT"
pattern SAMPLE_COUNT_4_BIT  = SampleCountFlagBits 0x00000004
-- No documentation found for Nested "VkSampleCountFlagBits" "VK_SAMPLE_COUNT_8_BIT"
pattern SAMPLE_COUNT_8_BIT  = SampleCountFlagBits 0x00000008
-- No documentation found for Nested "VkSampleCountFlagBits" "VK_SAMPLE_COUNT_16_BIT"
pattern SAMPLE_COUNT_16_BIT = SampleCountFlagBits 0x00000010
-- No documentation found for Nested "VkSampleCountFlagBits" "VK_SAMPLE_COUNT_32_BIT"
pattern SAMPLE_COUNT_32_BIT = SampleCountFlagBits 0x00000020
-- No documentation found for Nested "VkSampleCountFlagBits" "VK_SAMPLE_COUNT_64_BIT"
pattern SAMPLE_COUNT_64_BIT = SampleCountFlagBits 0x00000040

conNameSampleCountFlagBits :: String
conNameSampleCountFlagBits = "SampleCountFlagBits"

enumPrefixSampleCountFlagBits :: String
enumPrefixSampleCountFlagBits = "SAMPLE_COUNT_"

showTableSampleCountFlagBits :: [(SampleCountFlagBits, String)]
showTableSampleCountFlagBits =
  [ (SAMPLE_COUNT_1_BIT , "1_BIT")
  , (SAMPLE_COUNT_2_BIT , "2_BIT")
  , (SAMPLE_COUNT_4_BIT , "4_BIT")
  , (SAMPLE_COUNT_8_BIT , "8_BIT")
  , (SAMPLE_COUNT_16_BIT, "16_BIT")
  , (SAMPLE_COUNT_32_BIT, "32_BIT")
  , (SAMPLE_COUNT_64_BIT, "64_BIT")
  ]


instance Show SampleCountFlagBits where
showsPrec = enumShowsPrec enumPrefixSampleCountFlagBits
                          showTableSampleCountFlagBits
                          conNameSampleCountFlagBits
                          (\(SampleCountFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read SampleCountFlagBits where
  readPrec = enumReadPrec enumPrefixSampleCountFlagBits
                          showTableSampleCountFlagBits
                          conNameSampleCountFlagBits
                          SampleCountFlagBits

