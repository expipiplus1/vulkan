{-# language CPP #-}
-- No documentation found for Chapter "PipelineCacheHeaderVersion"
module Vulkan.Core10.Enums.PipelineCacheHeaderVersion  (PipelineCacheHeaderVersion( PIPELINE_CACHE_HEADER_VERSION_ONE
                                                                                  , PIPELINE_CACHE_HEADER_VERSION_SAFETY_CRITICAL_ONE
                                                                                  , ..
                                                                                  )) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))

-- | VkPipelineCacheHeaderVersion - Encode pipeline cache version
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.OtherTypes.PipelineCacheHeaderVersionOne',
-- 'Vulkan.Core10.PipelineCache.createPipelineCache',
-- 'Vulkan.Core10.PipelineCache.getPipelineCacheData'
newtype PipelineCacheHeaderVersion = PipelineCacheHeaderVersion Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- Note that the zero instance does not produce a valid value, passing 'zero' to Vulkan will result in an error

-- | 'PIPELINE_CACHE_HEADER_VERSION_ONE' specifies version one of the
-- pipeline cache, described by
-- 'Vulkan.Core10.OtherTypes.PipelineCacheHeaderVersionOne'.
pattern PIPELINE_CACHE_HEADER_VERSION_ONE = PipelineCacheHeaderVersion 1

-- No documentation found for Nested "VkPipelineCacheHeaderVersion" "VK_PIPELINE_CACHE_HEADER_VERSION_SAFETY_CRITICAL_ONE"
pattern PIPELINE_CACHE_HEADER_VERSION_SAFETY_CRITICAL_ONE = PipelineCacheHeaderVersion 1000298001

{-# COMPLETE
  PIPELINE_CACHE_HEADER_VERSION_ONE
  , PIPELINE_CACHE_HEADER_VERSION_SAFETY_CRITICAL_ONE ::
    PipelineCacheHeaderVersion
  #-}

conNamePipelineCacheHeaderVersion :: String
conNamePipelineCacheHeaderVersion = "PipelineCacheHeaderVersion"

enumPrefixPipelineCacheHeaderVersion :: String
enumPrefixPipelineCacheHeaderVersion = "PIPELINE_CACHE_HEADER_VERSION_"

showTablePipelineCacheHeaderVersion :: [(PipelineCacheHeaderVersion, String)]
showTablePipelineCacheHeaderVersion =
  [
    ( PIPELINE_CACHE_HEADER_VERSION_ONE
    , "ONE"
    )
  ,
    ( PIPELINE_CACHE_HEADER_VERSION_SAFETY_CRITICAL_ONE
    , "SAFETY_CRITICAL_ONE"
    )
  ]

instance Show PipelineCacheHeaderVersion where
  showsPrec =
    enumShowsPrec
      enumPrefixPipelineCacheHeaderVersion
      showTablePipelineCacheHeaderVersion
      conNamePipelineCacheHeaderVersion
      (\(PipelineCacheHeaderVersion x) -> x)
      (showsPrec 11)

instance Read PipelineCacheHeaderVersion where
  readPrec =
    enumReadPrec
      enumPrefixPipelineCacheHeaderVersion
      showTablePipelineCacheHeaderVersion
      conNamePipelineCacheHeaderVersion
      PipelineCacheHeaderVersion
