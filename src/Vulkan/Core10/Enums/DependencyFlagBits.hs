{-# language CPP #-}
-- No documentation found for Chapter "DependencyFlagBits"
module Vulkan.Core10.Enums.DependencyFlagBits  ( DependencyFlags
                                               , DependencyFlagBits( DEPENDENCY_BY_REGION_BIT
                                                                   , DEPENDENCY_VIEW_LOCAL_BIT
                                                                   , DEPENDENCY_DEVICE_GROUP_BIT
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
type DependencyFlags = DependencyFlagBits

-- No documentation found for TopLevel "VkDependencyFlagBits"
newtype DependencyFlagBits = DependencyFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkDependencyFlagBits" "VK_DEPENDENCY_BY_REGION_BIT"
pattern DEPENDENCY_BY_REGION_BIT    = DependencyFlagBits 0x00000001
-- No documentation found for Nested "VkDependencyFlagBits" "VK_DEPENDENCY_VIEW_LOCAL_BIT"
pattern DEPENDENCY_VIEW_LOCAL_BIT   = DependencyFlagBits 0x00000002
-- No documentation found for Nested "VkDependencyFlagBits" "VK_DEPENDENCY_DEVICE_GROUP_BIT"
pattern DEPENDENCY_DEVICE_GROUP_BIT = DependencyFlagBits 0x00000004

conNameDependencyFlagBits :: String
conNameDependencyFlagBits = "DependencyFlagBits"

enumPrefixDependencyFlagBits :: String
enumPrefixDependencyFlagBits = "DEPENDENCY_"

showTableDependencyFlagBits :: [(DependencyFlagBits, String)]
showTableDependencyFlagBits =
  [ (DEPENDENCY_BY_REGION_BIT   , "BY_REGION_BIT")
  , (DEPENDENCY_VIEW_LOCAL_BIT  , "VIEW_LOCAL_BIT")
  , (DEPENDENCY_DEVICE_GROUP_BIT, "DEVICE_GROUP_BIT")
  ]


instance Show DependencyFlagBits where
showsPrec = enumShowsPrec enumPrefixDependencyFlagBits
                          showTableDependencyFlagBits
                          conNameDependencyFlagBits
                          (\(DependencyFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read DependencyFlagBits where
  readPrec =
    enumReadPrec enumPrefixDependencyFlagBits showTableDependencyFlagBits conNameDependencyFlagBits DependencyFlagBits

