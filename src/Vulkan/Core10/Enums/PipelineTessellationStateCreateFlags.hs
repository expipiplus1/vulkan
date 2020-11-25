{-# language CPP #-}
-- No documentation found for Chapter "PipelineTessellationStateCreateFlags"
module Vulkan.Core10.Enums.PipelineTessellationStateCreateFlags  (PipelineTessellationStateCreateFlags(..)) where

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
-- No documentation found for TopLevel "VkPipelineTessellationStateCreateFlags"
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

