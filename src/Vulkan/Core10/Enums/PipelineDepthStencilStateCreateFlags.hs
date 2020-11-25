{-# language CPP #-}
-- No documentation found for Chapter "PipelineDepthStencilStateCreateFlags"
module Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlags  (PipelineDepthStencilStateCreateFlags(..)) where

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
-- No documentation found for TopLevel "VkPipelineDepthStencilStateCreateFlags"
newtype PipelineDepthStencilStateCreateFlags = PipelineDepthStencilStateCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNamePipelineDepthStencilStateCreateFlags :: String
conNamePipelineDepthStencilStateCreateFlags = "PipelineDepthStencilStateCreateFlags"

enumPrefixPipelineDepthStencilStateCreateFlags :: String
enumPrefixPipelineDepthStencilStateCreateFlags = ""

showTablePipelineDepthStencilStateCreateFlags :: [(PipelineDepthStencilStateCreateFlags, String)]
showTablePipelineDepthStencilStateCreateFlags = []


instance Show PipelineDepthStencilStateCreateFlags where
showsPrec = enumShowsPrec enumPrefixPipelineDepthStencilStateCreateFlags
                          showTablePipelineDepthStencilStateCreateFlags
                          conNamePipelineDepthStencilStateCreateFlags
                          (\(PipelineDepthStencilStateCreateFlags x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read PipelineDepthStencilStateCreateFlags where
  readPrec = enumReadPrec enumPrefixPipelineDepthStencilStateCreateFlags
                          showTablePipelineDepthStencilStateCreateFlags
                          conNamePipelineDepthStencilStateCreateFlags
                          PipelineDepthStencilStateCreateFlags

