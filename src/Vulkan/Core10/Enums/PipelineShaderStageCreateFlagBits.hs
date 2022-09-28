{-# language CPP #-}
-- No documentation found for Chapter "PipelineShaderStageCreateFlagBits"
module Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits  ( PipelineShaderStageCreateFlags
                                                              , PipelineShaderStageCreateFlagBits( PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT
                                                                                                 , PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT
                                                                                                 , ..
                                                                                                 )
                                                              ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Vulkan.Zero (Zero)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
type PipelineShaderStageCreateFlags = PipelineShaderStageCreateFlagBits

-- | VkPipelineShaderStageCreateFlagBits - Bitmask controlling how a pipeline
-- shader stage is created
--
-- = Description
--
-- Note
--
-- If
-- 'Vulkan.Extensions.VK_EXT_subgroup_size_control.PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT'
-- and
-- 'Vulkan.Extensions.VK_EXT_subgroup_size_control.PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT'
-- are specified and
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-minSubgroupSize minSubgroupSize>
-- does not equal
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-maxSubgroupSize maxSubgroupSize>
-- and no
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#pipelines-required-subgroup-size required subgroup size>
-- is specified, then the only way to guarantee that the \'X\' dimension of
-- the local workgroup size is a multiple of
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-sgs SubgroupSize>
-- is to make it a multiple of @maxSubgroupSize@. Under these conditions,
-- you are guaranteed full subgroups but not any particular subgroup size.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'PipelineShaderStageCreateFlags'
newtype PipelineShaderStageCreateFlagBits = PipelineShaderStageCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT' specifies that
-- the subgroup sizes /must/ be launched with all invocations active in the
-- compute stage.
pattern PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT = PipelineShaderStageCreateFlagBits 0x00000002

-- | 'PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT' specifies
-- that the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-sgs SubgroupSize>
-- /may/ vary in the shader stage.
pattern PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT = PipelineShaderStageCreateFlagBits 0x00000001

conNamePipelineShaderStageCreateFlagBits :: String
conNamePipelineShaderStageCreateFlagBits = "PipelineShaderStageCreateFlagBits"

enumPrefixPipelineShaderStageCreateFlagBits :: String
enumPrefixPipelineShaderStageCreateFlagBits = "PIPELINE_SHADER_STAGE_CREATE_"

showTablePipelineShaderStageCreateFlagBits :: [(PipelineShaderStageCreateFlagBits, String)]
showTablePipelineShaderStageCreateFlagBits =
  [
    ( PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT
    , "REQUIRE_FULL_SUBGROUPS_BIT"
    )
  ,
    ( PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT
    , "ALLOW_VARYING_SUBGROUP_SIZE_BIT"
    )
  ]

instance Show PipelineShaderStageCreateFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixPipelineShaderStageCreateFlagBits
      showTablePipelineShaderStageCreateFlagBits
      conNamePipelineShaderStageCreateFlagBits
      (\(PipelineShaderStageCreateFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read PipelineShaderStageCreateFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixPipelineShaderStageCreateFlagBits
      showTablePipelineShaderStageCreateFlagBits
      conNamePipelineShaderStageCreateFlagBits
      PipelineShaderStageCreateFlagBits
