{-# language CPP #-}
-- No documentation found for Chapter "PipelineLayoutCreateFlagBits"
module Vulkan.Core10.Enums.PipelineLayoutCreateFlagBits  ( PipelineLayoutCreateFlags
                                                         , PipelineLayoutCreateFlagBits( PIPELINE_LAYOUT_CREATE_INDEPENDENT_SETS_BIT_EXT
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
type PipelineLayoutCreateFlags = PipelineLayoutCreateFlagBits

-- | VkPipelineLayoutCreateFlagBits - Pipeline layout creation flag bits
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_graphics_pipeline_library VK_EXT_graphics_pipeline_library>,
-- 'PipelineLayoutCreateFlags'
newtype PipelineLayoutCreateFlagBits = PipelineLayoutCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'PIPELINE_LAYOUT_CREATE_INDEPENDENT_SETS_BIT_EXT' specifies that
-- implementations /must/ ensure that the properties and\/or absence of a
-- particular descriptor set do not influence any other properties of the
-- pipeline layout. This allows pipelines libraries linked without
-- 'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_LINK_TIME_OPTIMIZATION_BIT_EXT'
-- to be created with a subset of the total descriptor sets.
pattern PIPELINE_LAYOUT_CREATE_INDEPENDENT_SETS_BIT_EXT = PipelineLayoutCreateFlagBits 0x00000002

conNamePipelineLayoutCreateFlagBits :: String
conNamePipelineLayoutCreateFlagBits = "PipelineLayoutCreateFlagBits"

enumPrefixPipelineLayoutCreateFlagBits :: String
enumPrefixPipelineLayoutCreateFlagBits = "PIPELINE_LAYOUT_CREATE_INDEPENDENT_SETS_BIT_EXT"

showTablePipelineLayoutCreateFlagBits :: [(PipelineLayoutCreateFlagBits, String)]
showTablePipelineLayoutCreateFlagBits =
  [
    ( PIPELINE_LAYOUT_CREATE_INDEPENDENT_SETS_BIT_EXT
    , ""
    )
  ]

instance Show PipelineLayoutCreateFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixPipelineLayoutCreateFlagBits
      showTablePipelineLayoutCreateFlagBits
      conNamePipelineLayoutCreateFlagBits
      (\(PipelineLayoutCreateFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read PipelineLayoutCreateFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixPipelineLayoutCreateFlagBits
      showTablePipelineLayoutCreateFlagBits
      conNamePipelineLayoutCreateFlagBits
      PipelineLayoutCreateFlagBits
