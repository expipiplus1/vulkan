{-# language CPP #-}
-- No documentation found for Chapter "PipelineShaderStageCreateFlagBits"
module Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits  ( PipelineShaderStageCreateFlags
                                                              , PipelineShaderStageCreateFlagBits( PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT
                                                                                                 , PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT
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
type PipelineShaderStageCreateFlags = PipelineShaderStageCreateFlagBits

-- No documentation found for TopLevel "VkPipelineShaderStageCreateFlagBits"
newtype PipelineShaderStageCreateFlagBits = PipelineShaderStageCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkPipelineShaderStageCreateFlagBits" "VK_PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT"
pattern PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT      = PipelineShaderStageCreateFlagBits 0x00000002
-- No documentation found for Nested "VkPipelineShaderStageCreateFlagBits" "VK_PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT"
pattern PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT = PipelineShaderStageCreateFlagBits 0x00000001

conNamePipelineShaderStageCreateFlagBits :: String
conNamePipelineShaderStageCreateFlagBits = "PipelineShaderStageCreateFlagBits"

enumPrefixPipelineShaderStageCreateFlagBits :: String
enumPrefixPipelineShaderStageCreateFlagBits = "PIPELINE_SHADER_STAGE_CREATE_"

showTablePipelineShaderStageCreateFlagBits :: [(PipelineShaderStageCreateFlagBits, String)]
showTablePipelineShaderStageCreateFlagBits =
  [ (PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT     , "REQUIRE_FULL_SUBGROUPS_BIT_EXT")
  , (PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT, "ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT")
  ]


instance Show PipelineShaderStageCreateFlagBits where
showsPrec = enumShowsPrec enumPrefixPipelineShaderStageCreateFlagBits
                          showTablePipelineShaderStageCreateFlagBits
                          conNamePipelineShaderStageCreateFlagBits
                          (\(PipelineShaderStageCreateFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read PipelineShaderStageCreateFlagBits where
  readPrec = enumReadPrec enumPrefixPipelineShaderStageCreateFlagBits
                          showTablePipelineShaderStageCreateFlagBits
                          conNamePipelineShaderStageCreateFlagBits
                          PipelineShaderStageCreateFlagBits

