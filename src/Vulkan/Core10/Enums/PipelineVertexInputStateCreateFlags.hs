{-# language CPP #-}
-- No documentation found for Chapter "PipelineVertexInputStateCreateFlags"
module Vulkan.Core10.Enums.PipelineVertexInputStateCreateFlags  (PipelineVertexInputStateCreateFlags(..)) where

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
-- | VkPipelineVertexInputStateCreateFlags - Reserved for future use
--
-- = Description
--
-- 'PipelineVertexInputStateCreateFlags' is a bitmask type for setting a
-- mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'Vulkan.Core10.Pipeline.PipelineVertexInputStateCreateInfo'
newtype PipelineVertexInputStateCreateFlags = PipelineVertexInputStateCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNamePipelineVertexInputStateCreateFlags :: String
conNamePipelineVertexInputStateCreateFlags = "PipelineVertexInputStateCreateFlags"

enumPrefixPipelineVertexInputStateCreateFlags :: String
enumPrefixPipelineVertexInputStateCreateFlags = ""

showTablePipelineVertexInputStateCreateFlags :: [(PipelineVertexInputStateCreateFlags, String)]
showTablePipelineVertexInputStateCreateFlags = []

instance Show PipelineVertexInputStateCreateFlags where
  showsPrec = enumShowsPrec enumPrefixPipelineVertexInputStateCreateFlags
                            showTablePipelineVertexInputStateCreateFlags
                            conNamePipelineVertexInputStateCreateFlags
                            (\(PipelineVertexInputStateCreateFlags x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read PipelineVertexInputStateCreateFlags where
  readPrec = enumReadPrec enumPrefixPipelineVertexInputStateCreateFlags
                          showTablePipelineVertexInputStateCreateFlags
                          conNamePipelineVertexInputStateCreateFlags
                          PipelineVertexInputStateCreateFlags

