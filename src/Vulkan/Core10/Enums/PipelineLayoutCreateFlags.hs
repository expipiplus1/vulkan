{-# language CPP #-}
-- No documentation found for Chapter "PipelineLayoutCreateFlags"
module Vulkan.Core10.Enums.PipelineLayoutCreateFlags  (PipelineLayoutCreateFlags(..)) where

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
-- | VkPipelineLayoutCreateFlags - Reserved for future use
--
-- = Description
--
-- 'PipelineLayoutCreateFlags' is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo'
newtype PipelineLayoutCreateFlags = PipelineLayoutCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNamePipelineLayoutCreateFlags :: String
conNamePipelineLayoutCreateFlags = "PipelineLayoutCreateFlags"

enumPrefixPipelineLayoutCreateFlags :: String
enumPrefixPipelineLayoutCreateFlags = ""

showTablePipelineLayoutCreateFlags :: [(PipelineLayoutCreateFlags, String)]
showTablePipelineLayoutCreateFlags = []

instance Show PipelineLayoutCreateFlags where
  showsPrec = enumShowsPrec enumPrefixPipelineLayoutCreateFlags
                            showTablePipelineLayoutCreateFlags
                            conNamePipelineLayoutCreateFlags
                            (\(PipelineLayoutCreateFlags x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read PipelineLayoutCreateFlags where
  readPrec = enumReadPrec enumPrefixPipelineLayoutCreateFlags
                          showTablePipelineLayoutCreateFlags
                          conNamePipelineLayoutCreateFlags
                          PipelineLayoutCreateFlags

