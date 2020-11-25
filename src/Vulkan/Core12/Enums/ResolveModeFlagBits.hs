{-# language CPP #-}
-- No documentation found for Chapter "ResolveModeFlagBits"
module Vulkan.Core12.Enums.ResolveModeFlagBits  ( ResolveModeFlags
                                                , ResolveModeFlagBits( RESOLVE_MODE_NONE
                                                                     , RESOLVE_MODE_SAMPLE_ZERO_BIT
                                                                     , RESOLVE_MODE_AVERAGE_BIT
                                                                     , RESOLVE_MODE_MIN_BIT
                                                                     , RESOLVE_MODE_MAX_BIT
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
type ResolveModeFlags = ResolveModeFlagBits

-- No documentation found for TopLevel "VkResolveModeFlagBits"
newtype ResolveModeFlagBits = ResolveModeFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkResolveModeFlagBits" "VK_RESOLVE_MODE_NONE"
pattern RESOLVE_MODE_NONE            = ResolveModeFlagBits 0x00000000
-- No documentation found for Nested "VkResolveModeFlagBits" "VK_RESOLVE_MODE_SAMPLE_ZERO_BIT"
pattern RESOLVE_MODE_SAMPLE_ZERO_BIT = ResolveModeFlagBits 0x00000001
-- No documentation found for Nested "VkResolveModeFlagBits" "VK_RESOLVE_MODE_AVERAGE_BIT"
pattern RESOLVE_MODE_AVERAGE_BIT     = ResolveModeFlagBits 0x00000002
-- No documentation found for Nested "VkResolveModeFlagBits" "VK_RESOLVE_MODE_MIN_BIT"
pattern RESOLVE_MODE_MIN_BIT         = ResolveModeFlagBits 0x00000004
-- No documentation found for Nested "VkResolveModeFlagBits" "VK_RESOLVE_MODE_MAX_BIT"
pattern RESOLVE_MODE_MAX_BIT         = ResolveModeFlagBits 0x00000008

conNameResolveModeFlagBits :: String
conNameResolveModeFlagBits = "ResolveModeFlagBits"

enumPrefixResolveModeFlagBits :: String
enumPrefixResolveModeFlagBits = "RESOLVE_MODE_"

showTableResolveModeFlagBits :: [(ResolveModeFlagBits, String)]
showTableResolveModeFlagBits =
  [ (RESOLVE_MODE_NONE           , "NONE")
  , (RESOLVE_MODE_SAMPLE_ZERO_BIT, "SAMPLE_ZERO_BIT")
  , (RESOLVE_MODE_AVERAGE_BIT    , "AVERAGE_BIT")
  , (RESOLVE_MODE_MIN_BIT        , "MIN_BIT")
  , (RESOLVE_MODE_MAX_BIT        , "MAX_BIT")
  ]


instance Show ResolveModeFlagBits where
showsPrec = enumShowsPrec enumPrefixResolveModeFlagBits
                          showTableResolveModeFlagBits
                          conNameResolveModeFlagBits
                          (\(ResolveModeFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read ResolveModeFlagBits where
  readPrec = enumReadPrec enumPrefixResolveModeFlagBits
                          showTableResolveModeFlagBits
                          conNameResolveModeFlagBits
                          ResolveModeFlagBits

