{-# language CPP #-}
-- No documentation found for Chapter "PipelineRasterizationStateCreateFlags"
module Vulkan.Core10.Enums.PipelineRasterizationStateCreateFlags  (PipelineRasterizationStateCreateFlags(..)) where

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
-- | VkPipelineRasterizationStateCreateFlags - Reserved for future use
--
-- = Description
--
-- 'PipelineRasterizationStateCreateFlags' is a bitmask type for setting a
-- mask, but is currently reserved for future use.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  showsPrec =
    enumShowsPrec
      enumPrefixPipelineRasterizationStateCreateFlags
      showTablePipelineRasterizationStateCreateFlags
      conNamePipelineRasterizationStateCreateFlags
      (\(PipelineRasterizationStateCreateFlags x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read PipelineRasterizationStateCreateFlags where
  readPrec =
    enumReadPrec
      enumPrefixPipelineRasterizationStateCreateFlags
      showTablePipelineRasterizationStateCreateFlags
      conNamePipelineRasterizationStateCreateFlags
      PipelineRasterizationStateCreateFlags
