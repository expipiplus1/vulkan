{-# language CPP #-}
-- No documentation found for Chapter "PipelineColorBlendStateCreateFlags"
module Vulkan.Core10.Enums.PipelineColorBlendStateCreateFlags  (PipelineColorBlendStateCreateFlags(..)) where

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
-- | VkPipelineColorBlendStateCreateFlags - Reserved for future use
--
-- = Description
--
-- 'PipelineColorBlendStateCreateFlags' is a bitmask type for setting a
-- mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'Vulkan.Core10.Pipeline.PipelineColorBlendStateCreateInfo'
newtype PipelineColorBlendStateCreateFlags = PipelineColorBlendStateCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNamePipelineColorBlendStateCreateFlags :: String
conNamePipelineColorBlendStateCreateFlags = "PipelineColorBlendStateCreateFlags"

enumPrefixPipelineColorBlendStateCreateFlags :: String
enumPrefixPipelineColorBlendStateCreateFlags = ""

showTablePipelineColorBlendStateCreateFlags :: [(PipelineColorBlendStateCreateFlags, String)]
showTablePipelineColorBlendStateCreateFlags = []

instance Show PipelineColorBlendStateCreateFlags where
  showsPrec = enumShowsPrec enumPrefixPipelineColorBlendStateCreateFlags
                            showTablePipelineColorBlendStateCreateFlags
                            conNamePipelineColorBlendStateCreateFlags
                            (\(PipelineColorBlendStateCreateFlags x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read PipelineColorBlendStateCreateFlags where
  readPrec = enumReadPrec enumPrefixPipelineColorBlendStateCreateFlags
                          showTablePipelineColorBlendStateCreateFlags
                          conNamePipelineColorBlendStateCreateFlags
                          PipelineColorBlendStateCreateFlags

