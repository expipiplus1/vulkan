{-# language CPP #-}
-- No documentation found for Chapter "PipelineDynamicStateCreateFlags"
module Vulkan.Core10.Enums.PipelineDynamicStateCreateFlags  (PipelineDynamicStateCreateFlags(..)) where

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
-- | VkPipelineDynamicStateCreateFlags - Reserved for future use
--
-- = Description
--
-- 'PipelineDynamicStateCreateFlags' is a bitmask type for setting a mask,
-- but is currently reserved for future use.
--
-- = See Also
--
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'
newtype PipelineDynamicStateCreateFlags = PipelineDynamicStateCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNamePipelineDynamicStateCreateFlags :: String
conNamePipelineDynamicStateCreateFlags = "PipelineDynamicStateCreateFlags"

enumPrefixPipelineDynamicStateCreateFlags :: String
enumPrefixPipelineDynamicStateCreateFlags = ""

showTablePipelineDynamicStateCreateFlags :: [(PipelineDynamicStateCreateFlags, String)]
showTablePipelineDynamicStateCreateFlags = []

instance Show PipelineDynamicStateCreateFlags where
  showsPrec = enumShowsPrec enumPrefixPipelineDynamicStateCreateFlags
                            showTablePipelineDynamicStateCreateFlags
                            conNamePipelineDynamicStateCreateFlags
                            (\(PipelineDynamicStateCreateFlags x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read PipelineDynamicStateCreateFlags where
  readPrec = enumReadPrec enumPrefixPipelineDynamicStateCreateFlags
                          showTablePipelineDynamicStateCreateFlags
                          conNamePipelineDynamicStateCreateFlags
                          PipelineDynamicStateCreateFlags

