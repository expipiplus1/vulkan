{-# language CPP #-}
-- No documentation found for Chapter "PipelineCacheCreateFlagBits"
module Vulkan.Core10.Enums.PipelineCacheCreateFlagBits  ( PipelineCacheCreateFlags
                                                        , PipelineCacheCreateFlagBits( PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT_EXT
                                                                                     , ..
                                                                                     )
                                                        ) where

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
type PipelineCacheCreateFlags = PipelineCacheCreateFlagBits

-- No documentation found for TopLevel "VkPipelineCacheCreateFlagBits"
newtype PipelineCacheCreateFlagBits = PipelineCacheCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkPipelineCacheCreateFlagBits" "VK_PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT_EXT"
pattern PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT_EXT = PipelineCacheCreateFlagBits 0x00000001

conNamePipelineCacheCreateFlagBits :: String
conNamePipelineCacheCreateFlagBits = "PipelineCacheCreateFlagBits"

enumPrefixPipelineCacheCreateFlagBits :: String
enumPrefixPipelineCacheCreateFlagBits = "PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT_EXT"

showTablePipelineCacheCreateFlagBits :: [(PipelineCacheCreateFlagBits, String)]
showTablePipelineCacheCreateFlagBits = [(PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT_EXT, "")]


instance Show PipelineCacheCreateFlagBits where
showsPrec = enumShowsPrec enumPrefixPipelineCacheCreateFlagBits
                          showTablePipelineCacheCreateFlagBits
                          conNamePipelineCacheCreateFlagBits
                          (\(PipelineCacheCreateFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read PipelineCacheCreateFlagBits where
  readPrec = enumReadPrec enumPrefixPipelineCacheCreateFlagBits
                          showTablePipelineCacheCreateFlagBits
                          conNamePipelineCacheCreateFlagBits
                          PipelineCacheCreateFlagBits

