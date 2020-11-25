{-# language CPP #-}
-- No documentation found for Chapter "CullModeFlagBits"
module Vulkan.Core10.Enums.CullModeFlagBits  ( CullModeFlags
                                             , CullModeFlagBits( CULL_MODE_NONE
                                                               , CULL_MODE_FRONT_BIT
                                                               , CULL_MODE_BACK_BIT
                                                               , CULL_MODE_FRONT_AND_BACK
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
type CullModeFlags = CullModeFlagBits

-- No documentation found for TopLevel "VkCullModeFlagBits"
newtype CullModeFlagBits = CullModeFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkCullModeFlagBits" "VK_CULL_MODE_NONE"
pattern CULL_MODE_NONE           = CullModeFlagBits 0x00000000
-- No documentation found for Nested "VkCullModeFlagBits" "VK_CULL_MODE_FRONT_BIT"
pattern CULL_MODE_FRONT_BIT      = CullModeFlagBits 0x00000001
-- No documentation found for Nested "VkCullModeFlagBits" "VK_CULL_MODE_BACK_BIT"
pattern CULL_MODE_BACK_BIT       = CullModeFlagBits 0x00000002
-- No documentation found for Nested "VkCullModeFlagBits" "VK_CULL_MODE_FRONT_AND_BACK"
pattern CULL_MODE_FRONT_AND_BACK = CullModeFlagBits 0x00000003

conNameCullModeFlagBits :: String
conNameCullModeFlagBits = "CullModeFlagBits"

enumPrefixCullModeFlagBits :: String
enumPrefixCullModeFlagBits = "CULL_MODE_"

showTableCullModeFlagBits :: [(CullModeFlagBits, String)]
showTableCullModeFlagBits =
  [ (CULL_MODE_NONE          , "NONE")
  , (CULL_MODE_FRONT_BIT     , "FRONT_BIT")
  , (CULL_MODE_BACK_BIT      , "BACK_BIT")
  , (CULL_MODE_FRONT_AND_BACK, "FRONT_AND_BACK")
  ]


instance Show CullModeFlagBits where
showsPrec = enumShowsPrec enumPrefixCullModeFlagBits
                          showTableCullModeFlagBits
                          conNameCullModeFlagBits
                          (\(CullModeFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read CullModeFlagBits where
  readPrec = enumReadPrec enumPrefixCullModeFlagBits showTableCullModeFlagBits conNameCullModeFlagBits CullModeFlagBits

