{-# language CPP #-}
-- No documentation found for Chapter "PipelineLayoutCreateFlagBits"
module Vulkan.Core10.Enums.PipelineLayoutCreateFlagBits  ( PipelineLayoutCreateFlags
                                                         , PipelineLayoutCreateFlagBits(..)
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
type PipelineLayoutCreateFlags = PipelineLayoutCreateFlagBits

-- No documentation found for TopLevel "VkPipelineLayoutCreateFlagBits"
newtype PipelineLayoutCreateFlagBits = PipelineLayoutCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNamePipelineLayoutCreateFlagBits :: String
conNamePipelineLayoutCreateFlagBits = "PipelineLayoutCreateFlagBits"

enumPrefixPipelineLayoutCreateFlagBits :: String
enumPrefixPipelineLayoutCreateFlagBits = ""

showTablePipelineLayoutCreateFlagBits :: [(PipelineLayoutCreateFlagBits, String)]
showTablePipelineLayoutCreateFlagBits = []

instance Show PipelineLayoutCreateFlagBits where
  showsPrec = enumShowsPrec enumPrefixPipelineLayoutCreateFlagBits
                            showTablePipelineLayoutCreateFlagBits
                            conNamePipelineLayoutCreateFlagBits
                            (\(PipelineLayoutCreateFlagBits x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read PipelineLayoutCreateFlagBits where
  readPrec = enumReadPrec enumPrefixPipelineLayoutCreateFlagBits
                          showTablePipelineLayoutCreateFlagBits
                          conNamePipelineLayoutCreateFlagBits
                          PipelineLayoutCreateFlagBits

