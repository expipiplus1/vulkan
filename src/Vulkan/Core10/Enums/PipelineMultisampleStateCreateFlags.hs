{-# language CPP #-}
-- No documentation found for Chapter "PipelineMultisampleStateCreateFlags"
module Vulkan.Core10.Enums.PipelineMultisampleStateCreateFlags  (PipelineMultisampleStateCreateFlags(..)) where

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
-- | VkPipelineMultisampleStateCreateFlags - Reserved for future use
--
-- = Description
--
-- 'PipelineMultisampleStateCreateFlags' is a bitmask type for setting a
-- mask, but is currently reserved for future use.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo'
newtype PipelineMultisampleStateCreateFlags = PipelineMultisampleStateCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

conNamePipelineMultisampleStateCreateFlags :: String
conNamePipelineMultisampleStateCreateFlags = "PipelineMultisampleStateCreateFlags"

enumPrefixPipelineMultisampleStateCreateFlags :: String
enumPrefixPipelineMultisampleStateCreateFlags = ""

showTablePipelineMultisampleStateCreateFlags :: [(PipelineMultisampleStateCreateFlags, String)]
showTablePipelineMultisampleStateCreateFlags = []

instance Show PipelineMultisampleStateCreateFlags where
  showsPrec =
    enumShowsPrec
      enumPrefixPipelineMultisampleStateCreateFlags
      showTablePipelineMultisampleStateCreateFlags
      conNamePipelineMultisampleStateCreateFlags
      (\(PipelineMultisampleStateCreateFlags x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read PipelineMultisampleStateCreateFlags where
  readPrec =
    enumReadPrec
      enumPrefixPipelineMultisampleStateCreateFlags
      showTablePipelineMultisampleStateCreateFlags
      conNamePipelineMultisampleStateCreateFlags
      PipelineMultisampleStateCreateFlags
