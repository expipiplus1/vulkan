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
import Vulkan.Zero (Zero)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
type ColorComponentFlags = ColorComponentFlagBits

-- | VkColorComponentFlagBits - Bitmask controlling which components are
-- written to the framebuffer
--
-- = Description
--
-- The color write mask operation is applied regardless of whether blending
-- is enabled.
--
-- The color write mask operation is applied only if
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#framebuffer-color-write-enable Color Write Enable>
-- is enabled for the respective attachment. Otherwise the color write mask
-- is ignored and writes to all components of the attachment are disabled.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'ColorComponentFlags'
newtype ColorComponentFlagBits = ColorComponentFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'COLOR_COMPONENT_R_BIT' specifies that the R value is written to the
-- color attachment for the appropriate sample. Otherwise, the value in
-- memory is unmodified.
pattern COLOR_COMPONENT_R_BIT = ColorComponentFlagBits 0x00000001
-- | 'COLOR_COMPONENT_G_BIT' specifies that the G value is written to the
-- color attachment for the appropriate sample. Otherwise, the value in
-- memory is unmodified.
pattern COLOR_COMPONENT_G_BIT = ColorComponentFlagBits 0x00000002
-- | 'COLOR_COMPONENT_B_BIT' specifies that the B value is written to the
-- color attachment for the appropriate sample. Otherwise, the value in
-- memory is unmodified.
pattern COLOR_COMPONENT_B_BIT = ColorComponentFlagBits 0x00000004
-- | 'COLOR_COMPONENT_A_BIT' specifies that the A value is written to the
-- color attachment for the appropriate sample. Otherwise, the value in
-- memory is unmodified.
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

