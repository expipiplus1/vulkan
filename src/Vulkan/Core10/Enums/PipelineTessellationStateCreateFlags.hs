{-# language CPP #-}
-- No documentation found for Chapter "PipelineTessellationStateCreateFlags"
module Vulkan.Core10.Enums.PipelineTessellationStateCreateFlags  (PipelineTessellationStateCreateFlags(..)) where

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
-- | VkPipelineTessellationStateCreateFlags - Reserved for future use
--
-- = Description
--
-- 'PipelineTessellationStateCreateFlags' is a bitmask type for setting a
-- mask, but is currently reserved for future use.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Pipeline.PipelineTessellationStateCreateInfo'
newtype PipelineTessellationStateCreateFlags = PipelineTessellationStateCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNamePipelineTessellationStateCreateFlags :: String
conNamePipelineTessellationStateCreateFlags = "PipelineTessellationStateCreateFlags"

enumPrefixPipelineTessellationStateCreateFlags :: String
enumPrefixPipelineTessellationStateCreateFlags = ""

showTablePipelineTessellationStateCreateFlags :: [(PipelineTessellationStateCreateFlags, String)]
showTablePipelineTessellationStateCreateFlags = []

instance Show PipelineTessellationStateCreateFlags where
  showsPrec = enumShowsPrec enumPrefixPipelineTessellationStateCreateFlags
                            showTablePipelineTessellationStateCreateFlags
                            conNamePipelineTessellationStateCreateFlags
                            (\(PipelineTessellationStateCreateFlags x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read PipelineTessellationStateCreateFlags where
  readPrec = enumReadPrec enumPrefixPipelineTessellationStateCreateFlags
                          showTablePipelineTessellationStateCreateFlags
                          conNamePipelineTessellationStateCreateFlags
                          PipelineTessellationStateCreateFlags

