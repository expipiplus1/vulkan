{-# language CPP #-}
-- No documentation found for Chapter "PipelineMultisampleStateCreateFlags"
module Vulkan.Core10.Enums.PipelineMultisampleStateCreateFlags  (PipelineMultisampleStateCreateFlags(..)) where

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
-- No documentation found for TopLevel "VkPipelineMultisampleStateCreateFlags"
newtype PipelineMultisampleStateCreateFlags = PipelineMultisampleStateCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNamePipelineMultisampleStateCreateFlags :: String
conNamePipelineMultisampleStateCreateFlags = "PipelineMultisampleStateCreateFlags"

enumPrefixPipelineMultisampleStateCreateFlags :: String
enumPrefixPipelineMultisampleStateCreateFlags = ""

showTablePipelineMultisampleStateCreateFlags :: [(PipelineMultisampleStateCreateFlags, String)]
showTablePipelineMultisampleStateCreateFlags = []


instance Show PipelineMultisampleStateCreateFlags where
showsPrec = enumShowsPrec enumPrefixPipelineMultisampleStateCreateFlags
                          showTablePipelineMultisampleStateCreateFlags
                          conNamePipelineMultisampleStateCreateFlags
                          (\(PipelineMultisampleStateCreateFlags x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read PipelineMultisampleStateCreateFlags where
  readPrec = enumReadPrec enumPrefixPipelineMultisampleStateCreateFlags
                          showTablePipelineMultisampleStateCreateFlags
                          conNamePipelineMultisampleStateCreateFlags
                          PipelineMultisampleStateCreateFlags

