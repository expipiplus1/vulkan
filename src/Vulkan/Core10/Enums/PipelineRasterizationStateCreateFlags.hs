{-# language CPP #-}
-- No documentation found for Chapter "PipelineRasterizationStateCreateFlags"
module Vulkan.Core10.Enums.PipelineRasterizationStateCreateFlags  (PipelineRasterizationStateCreateFlags(..)) where

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
-- | VkPipelineRasterizationStateCreateFlags - Reserved for future use
--
-- = Description
--
-- 'PipelineRasterizationStateCreateFlags' is a bitmask type for setting a
-- mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo'
newtype PipelineRasterizationStateCreateFlags = PipelineRasterizationStateCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNamePipelineRasterizationStateCreateFlags :: String
conNamePipelineRasterizationStateCreateFlags = "PipelineRasterizationStateCreateFlags"

enumPrefixPipelineRasterizationStateCreateFlags :: String
enumPrefixPipelineRasterizationStateCreateFlags = ""

showTablePipelineRasterizationStateCreateFlags :: [(PipelineRasterizationStateCreateFlags, String)]
showTablePipelineRasterizationStateCreateFlags = []

instance Show PipelineRasterizationStateCreateFlags where
  showsPrec = enumShowsPrec enumPrefixPipelineRasterizationStateCreateFlags
                            showTablePipelineRasterizationStateCreateFlags
                            conNamePipelineRasterizationStateCreateFlags
                            (\(PipelineRasterizationStateCreateFlags x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read PipelineRasterizationStateCreateFlags where
  readPrec = enumReadPrec enumPrefixPipelineRasterizationStateCreateFlags
                          showTablePipelineRasterizationStateCreateFlags
                          conNamePipelineRasterizationStateCreateFlags
                          PipelineRasterizationStateCreateFlags

