{-# language CPP #-}
-- No documentation found for Chapter "PipelineCacheCreateFlagBits"
module Vulkan.Core10.Enums.PipelineCacheCreateFlagBits  ( PipelineCacheCreateFlags
                                                        , PipelineCacheCreateFlagBits( PIPELINE_CACHE_CREATE_USE_APPLICATION_STORAGE_BIT
                                                                                     , PIPELINE_CACHE_CREATE_READ_ONLY_BIT
                                                                                     , PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT
                                                                                     , ..
                                                                                     )
                                                        ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
type PipelineCacheCreateFlags = PipelineCacheCreateFlagBits

-- | VkPipelineCacheCreateFlagBits - Bitmask specifying the behavior of the
-- pipeline cache
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_pipeline_creation_cache_control VK_EXT_pipeline_creation_cache_control>,
-- 'PipelineCacheCreateFlags'
newtype PipelineCacheCreateFlagBits = PipelineCacheCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkPipelineCacheCreateFlagBits" "VK_PIPELINE_CACHE_CREATE_USE_APPLICATION_STORAGE_BIT"
pattern PIPELINE_CACHE_CREATE_USE_APPLICATION_STORAGE_BIT = PipelineCacheCreateFlagBits 0x00000004

-- No documentation found for Nested "VkPipelineCacheCreateFlagBits" "VK_PIPELINE_CACHE_CREATE_READ_ONLY_BIT"
pattern PIPELINE_CACHE_CREATE_READ_ONLY_BIT = PipelineCacheCreateFlagBits 0x00000002

-- | 'PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT' specifies that all
-- commands that modify the created 'Vulkan.Core10.Handles.PipelineCache'
-- will be
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#fundamentals-threadingbehavior externally synchronized>.
-- When set, the implementation /may/ skip any unnecessary processing
-- needed to support simultaneous modification from multiple threads where
-- allowed.
pattern PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT = PipelineCacheCreateFlagBits 0x00000001

conNamePipelineCacheCreateFlagBits :: String
conNamePipelineCacheCreateFlagBits = "PipelineCacheCreateFlagBits"

enumPrefixPipelineCacheCreateFlagBits :: String
enumPrefixPipelineCacheCreateFlagBits = "PIPELINE_CACHE_CREATE_"

showTablePipelineCacheCreateFlagBits :: [(PipelineCacheCreateFlagBits, String)]
showTablePipelineCacheCreateFlagBits =
  [
    ( PIPELINE_CACHE_CREATE_USE_APPLICATION_STORAGE_BIT
    , "USE_APPLICATION_STORAGE_BIT"
    )
  ,
    ( PIPELINE_CACHE_CREATE_READ_ONLY_BIT
    , "READ_ONLY_BIT"
    )
  ,
    ( PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT
    , "EXTERNALLY_SYNCHRONIZED_BIT"
    )
  ]

instance Show PipelineCacheCreateFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixPipelineCacheCreateFlagBits
      showTablePipelineCacheCreateFlagBits
      conNamePipelineCacheCreateFlagBits
      (\(PipelineCacheCreateFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read PipelineCacheCreateFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixPipelineCacheCreateFlagBits
      showTablePipelineCacheCreateFlagBits
      conNamePipelineCacheCreateFlagBits
      PipelineCacheCreateFlagBits
