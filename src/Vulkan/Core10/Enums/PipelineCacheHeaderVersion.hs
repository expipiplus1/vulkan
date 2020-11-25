{-# language CPP #-}
-- No documentation found for Chapter "PipelineCacheHeaderVersion"
module Vulkan.Core10.Enums.PipelineCacheHeaderVersion  (PipelineCacheHeaderVersion( PIPELINE_CACHE_HEADER_VERSION_ONE
                                                                                  , ..
                                                                                  )) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Zero (Zero)
-- No documentation found for TopLevel "VkPipelineCacheHeaderVersion"
newtype PipelineCacheHeaderVersion = PipelineCacheHeaderVersion Int32
  deriving newtype (Eq, Ord, Storable, Zero)
-- Note that the zero instance does not produce a valid value, passing 'zero' to Vulkan will result in an error

-- No documentation found for Nested "VkPipelineCacheHeaderVersion" "VK_PIPELINE_CACHE_HEADER_VERSION_ONE"
pattern PIPELINE_CACHE_HEADER_VERSION_ONE = PipelineCacheHeaderVersion 1
{-# complete PIPELINE_CACHE_HEADER_VERSION_ONE :: PipelineCacheHeaderVersion #-}

conNamePipelineCacheHeaderVersion :: String
conNamePipelineCacheHeaderVersion = "PipelineCacheHeaderVersion"

enumPrefixPipelineCacheHeaderVersion :: String
enumPrefixPipelineCacheHeaderVersion = "PIPELINE_CACHE_HEADER_VERSION_ONE"

showTablePipelineCacheHeaderVersion :: [(PipelineCacheHeaderVersion, String)]
showTablePipelineCacheHeaderVersion = [(PIPELINE_CACHE_HEADER_VERSION_ONE, "")]


instance Show PipelineCacheHeaderVersion where
showsPrec = enumShowsPrec enumPrefixPipelineCacheHeaderVersion
                          showTablePipelineCacheHeaderVersion
                          conNamePipelineCacheHeaderVersion
                          (\(PipelineCacheHeaderVersion x) -> x)
                          (showsPrec 11)


instance Read PipelineCacheHeaderVersion where
  readPrec = enumReadPrec enumPrefixPipelineCacheHeaderVersion
                          showTablePipelineCacheHeaderVersion
                          conNamePipelineCacheHeaderVersion
                          PipelineCacheHeaderVersion

