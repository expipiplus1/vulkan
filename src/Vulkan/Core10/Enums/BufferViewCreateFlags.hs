{-# language CPP #-}
-- No documentation found for Chapter "BufferViewCreateFlags"
module Vulkan.Core10.Enums.BufferViewCreateFlags  (BufferViewCreateFlags(..)) where

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
-- | VkBufferViewCreateFlags - Reserved for future use
--
-- = Description
--
-- 'BufferViewCreateFlags' is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'Vulkan.Core10.BufferView.BufferViewCreateInfo'
newtype BufferViewCreateFlags = BufferViewCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameBufferViewCreateFlags :: String
conNameBufferViewCreateFlags = "BufferViewCreateFlags"

enumPrefixBufferViewCreateFlags :: String
enumPrefixBufferViewCreateFlags = ""

showTableBufferViewCreateFlags :: [(BufferViewCreateFlags, String)]
showTableBufferViewCreateFlags = []

instance Show BufferViewCreateFlags where
  showsPrec = enumShowsPrec enumPrefixBufferViewCreateFlags
                            showTableBufferViewCreateFlags
                            conNameBufferViewCreateFlags
                            (\(BufferViewCreateFlags x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read BufferViewCreateFlags where
  readPrec = enumReadPrec enumPrefixBufferViewCreateFlags
                          showTableBufferViewCreateFlags
                          conNameBufferViewCreateFlags
                          BufferViewCreateFlags

