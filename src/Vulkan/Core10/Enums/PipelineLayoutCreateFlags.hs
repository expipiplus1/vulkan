{-# language CPP #-}
-- No documentation found for Chapter "PipelineLayoutCreateFlags"
module Vulkan.Core10.Enums.PipelineLayoutCreateFlags  (PipelineLayoutCreateFlags(..)) where

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
-- No documentation found for TopLevel "VkPipelineLayoutCreateFlags"
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

