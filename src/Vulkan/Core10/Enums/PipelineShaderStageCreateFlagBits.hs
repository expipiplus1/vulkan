{-# language CPP #-}
module Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits  ( PipelineShaderStageCreateFlagBits( PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT
                                                                                                 , PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT
                                                                                                 , ..
                                                                                                 )
                                                              , PipelineShaderStageCreateFlags
                                                              ) where

import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import Numeric (showHex)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Data.Bits (Bits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Core10.BaseType (Flags)
import Vulkan.Zero (Zero)
-- | VkPipelineShaderStageCreateFlagBits - Bitmask controlling how a pipeline
-- shader stage is created
--
-- = Description
--
-- Note
--
-- If 'PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT'
-- and 'PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT' are
-- specified and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-max-subgroup-size minSubgroupSize>
-- does not equal
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-max-subgroup-size maxSubgroupSize>
-- and no
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#pipelines-required-subgroup-size required subgroup size>
-- is specified, then the only way to guarantee that the \'X\' dimension of
-- the local workgroup size is a multiple of
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-sgs SubgroupSize>
-- is to make it a multiple of @maxSubgroupSize@. Under these conditions,
-- you are guaranteed full subgroups but not any particular subgroup size.
--
-- = See Also
--
-- 'PipelineShaderStageCreateFlags'
newtype PipelineShaderStageCreateFlagBits = PipelineShaderStageCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT' specifies
-- that the subgroup sizes /must/ be launched with all invocations active
-- in the compute stage.
pattern PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT = PipelineShaderStageCreateFlagBits 0x00000002
-- | 'PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT'
-- specifies that the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-sgs SubgroupSize>
-- /may/ vary in the shader stage.
pattern PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT = PipelineShaderStageCreateFlagBits 0x00000001

type PipelineShaderStageCreateFlags = PipelineShaderStageCreateFlagBits

instance Show PipelineShaderStageCreateFlagBits where
  showsPrec p = \case
    PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT -> showString "PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT"
    PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT -> showString "PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT"
    PipelineShaderStageCreateFlagBits x -> showParen (p >= 11) (showString "PipelineShaderStageCreateFlagBits 0x" . showHex x)

instance Read PipelineShaderStageCreateFlagBits where
  readPrec = parens (choose [("PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT", pure PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT)
                            , ("PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT", pure PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT)]
                     +++
                     prec 10 (do
                       expectP (Ident "PipelineShaderStageCreateFlagBits")
                       v <- step readPrec
                       pure (PipelineShaderStageCreateFlagBits v)))

