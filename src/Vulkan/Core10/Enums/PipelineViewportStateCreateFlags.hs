{-# language CPP #-}
-- No documentation found for Chapter "PipelineViewportStateCreateFlags"
module Vulkan.Core10.Enums.PipelineViewportStateCreateFlags  (PipelineViewportStateCreateFlags(..)) where

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
-- | VkPipelineViewportStateCreateFlags - Reserved for future use
--
-- = Description
--
-- 'PipelineViewportStateCreateFlags' is a bitmask type for setting a mask,
-- but is currently reserved for future use.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'
newtype PipelineViewportStateCreateFlags = PipelineViewportStateCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

conNamePipelineViewportStateCreateFlags :: String
conNamePipelineViewportStateCreateFlags = "PipelineViewportStateCreateFlags"

enumPrefixPipelineViewportStateCreateFlags :: String
enumPrefixPipelineViewportStateCreateFlags = ""

showTablePipelineViewportStateCreateFlags :: [(PipelineViewportStateCreateFlags, String)]
showTablePipelineViewportStateCreateFlags = []

instance Show PipelineViewportStateCreateFlags where
  showsPrec =
    enumShowsPrec
      enumPrefixPipelineViewportStateCreateFlags
      showTablePipelineViewportStateCreateFlags
      conNamePipelineViewportStateCreateFlags
      (\(PipelineViewportStateCreateFlags x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read PipelineViewportStateCreateFlags where
  readPrec =
    enumReadPrec
      enumPrefixPipelineViewportStateCreateFlags
      showTablePipelineViewportStateCreateFlags
      conNamePipelineViewportStateCreateFlags
      PipelineViewportStateCreateFlags
