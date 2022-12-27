{-# language CPP #-}
-- No documentation found for Chapter "InstanceCreateFlagBits"
module Vulkan.Core10.Enums.InstanceCreateFlagBits  ( InstanceCreateFlags
                                                   , InstanceCreateFlagBits( INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR
                                                                           , ..
                                                                           )
                                                   ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import GHC.Bits (Bits)
import GHC.Bits (FiniteBits)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
type InstanceCreateFlags = InstanceCreateFlagBits

-- | VkInstanceCreateFlagBits - Bitmask specifying behavior of the instance
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'InstanceCreateFlags'
newtype InstanceCreateFlagBits = InstanceCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR' specifies that the
-- instance will enumerate available Vulkan Portability-compliant physical
-- devices and groups in addition to the Vulkan physical devices and groups
-- that are enumerated by default.
pattern INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR = InstanceCreateFlagBits 0x00000001

conNameInstanceCreateFlagBits :: String
conNameInstanceCreateFlagBits = "InstanceCreateFlagBits"

enumPrefixInstanceCreateFlagBits :: String
enumPrefixInstanceCreateFlagBits = "INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR"

showTableInstanceCreateFlagBits :: [(InstanceCreateFlagBits, String)]
showTableInstanceCreateFlagBits =
  [
    ( INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR
    , ""
    )
  ]

instance Show InstanceCreateFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixInstanceCreateFlagBits
      showTableInstanceCreateFlagBits
      conNameInstanceCreateFlagBits
      (\(InstanceCreateFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read InstanceCreateFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixInstanceCreateFlagBits
      showTableInstanceCreateFlagBits
      conNameInstanceCreateFlagBits
      InstanceCreateFlagBits
