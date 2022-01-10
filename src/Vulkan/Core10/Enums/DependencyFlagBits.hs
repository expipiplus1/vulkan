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
import Vulkan.Zero (Zero)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
type DependencyFlags = DependencyFlagBits

-- | VkDependencyFlagBits - Bitmask specifying how execution and memory
-- dependencies are formed
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'DependencyFlags'
newtype DependencyFlagBits = DependencyFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'DEPENDENCY_BY_REGION_BIT' specifies that dependencies will be
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-framebuffer-regions framebuffer-local>.
pattern DEPENDENCY_BY_REGION_BIT    = DependencyFlagBits 0x00000001
-- | 'DEPENDENCY_VIEW_LOCAL_BIT' specifies that a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-barriers-subpass-self-dependencies subpass has more than one view>.
pattern DEPENDENCY_VIEW_LOCAL_BIT   = DependencyFlagBits 0x00000002
-- | 'DEPENDENCY_DEVICE_GROUP_BIT' specifies that dependencies are
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-device-local-dependencies non-device-local>.
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

