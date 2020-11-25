{-# language CPP #-}
-- No documentation found for Chapter "ColorComponentFlagBits"
module Vulkan.Core10.Enums.ColorComponentFlagBits  ( ColorComponentFlags
                                                   , ColorComponentFlagBits( COLOR_COMPONENT_R_BIT
                                                                           , COLOR_COMPONENT_G_BIT
                                                                           , COLOR_COMPONENT_B_BIT
                                                                           , COLOR_COMPONENT_A_BIT
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
type ColorComponentFlags = ColorComponentFlagBits

-- No documentation found for TopLevel "VkColorComponentFlagBits"
newtype ColorComponentFlagBits = ColorComponentFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkColorComponentFlagBits" "VK_COLOR_COMPONENT_R_BIT"
pattern COLOR_COMPONENT_R_BIT = ColorComponentFlagBits 0x00000001
-- No documentation found for Nested "VkColorComponentFlagBits" "VK_COLOR_COMPONENT_G_BIT"
pattern COLOR_COMPONENT_G_BIT = ColorComponentFlagBits 0x00000002
-- No documentation found for Nested "VkColorComponentFlagBits" "VK_COLOR_COMPONENT_B_BIT"
pattern COLOR_COMPONENT_B_BIT = ColorComponentFlagBits 0x00000004
-- No documentation found for Nested "VkColorComponentFlagBits" "VK_COLOR_COMPONENT_A_BIT"
pattern COLOR_COMPONENT_A_BIT = ColorComponentFlagBits 0x00000008

conNameColorComponentFlagBits :: String
conNameColorComponentFlagBits = "ColorComponentFlagBits"

enumPrefixColorComponentFlagBits :: String
enumPrefixColorComponentFlagBits = "COLOR_COMPONENT_"

showTableColorComponentFlagBits :: [(ColorComponentFlagBits, String)]
showTableColorComponentFlagBits =
  [ (COLOR_COMPONENT_R_BIT, "R_BIT")
  , (COLOR_COMPONENT_G_BIT, "G_BIT")
  , (COLOR_COMPONENT_B_BIT, "B_BIT")
  , (COLOR_COMPONENT_A_BIT, "A_BIT")
  ]


instance Show ColorComponentFlagBits where
showsPrec = enumShowsPrec enumPrefixColorComponentFlagBits
                          showTableColorComponentFlagBits
                          conNameColorComponentFlagBits
                          (\(ColorComponentFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read ColorComponentFlagBits where
  readPrec = enumReadPrec enumPrefixColorComponentFlagBits
                          showTableColorComponentFlagBits
                          conNameColorComponentFlagBits
                          ColorComponentFlagBits

