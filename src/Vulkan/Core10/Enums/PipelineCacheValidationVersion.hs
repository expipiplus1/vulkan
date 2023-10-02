{-# language CPP #-}
-- No documentation found for Chapter "PipelineCacheValidationVersion"
module Vulkan.Core10.Enums.PipelineCacheValidationVersion  (PipelineCacheValidationVersion( PIPELINE_CACHE_VALIDATION_VERSION_SAFETY_CRITICAL_ONE
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

-- No documentation found for TopLevel "VkPipelineCacheValidationVersion"
newtype PipelineCacheValidationVersion = PipelineCacheValidationVersion Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- Note that the zero instance does not produce a valid value, passing 'zero' to Vulkan will result in an error

-- No documentation found for Nested "VkPipelineCacheValidationVersion" "VK_PIPELINE_CACHE_VALIDATION_VERSION_SAFETY_CRITICAL_ONE"
pattern PIPELINE_CACHE_VALIDATION_VERSION_SAFETY_CRITICAL_ONE = PipelineCacheValidationVersion 1

{-# COMPLETE PIPELINE_CACHE_VALIDATION_VERSION_SAFETY_CRITICAL_ONE :: PipelineCacheValidationVersion #-}

conNamePipelineCacheValidationVersion :: String
conNamePipelineCacheValidationVersion = "PipelineCacheValidationVersion"

enumPrefixPipelineCacheValidationVersion :: String
enumPrefixPipelineCacheValidationVersion = "PIPELINE_CACHE_VALIDATION_VERSION_SAFETY_CRITICAL_ONE"

showTablePipelineCacheValidationVersion :: [(PipelineCacheValidationVersion, String)]
showTablePipelineCacheValidationVersion =
  [
    ( PIPELINE_CACHE_VALIDATION_VERSION_SAFETY_CRITICAL_ONE
    , ""
    )
  ]

instance Show PipelineCacheValidationVersion where
  showsPrec =
    enumShowsPrec
      enumPrefixPipelineCacheValidationVersion
      showTablePipelineCacheValidationVersion
      conNamePipelineCacheValidationVersion
      (\(PipelineCacheValidationVersion x) -> x)
      (showsPrec 11)

instance Read PipelineCacheValidationVersion where
  readPrec =
    enumReadPrec
      enumPrefixPipelineCacheValidationVersion
      showTablePipelineCacheValidationVersion
      conNamePipelineCacheValidationVersion
      PipelineCacheValidationVersion
