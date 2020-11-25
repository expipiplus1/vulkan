{-# language CPP #-}
-- No documentation found for Chapter "ShaderStageFlagBits"
module Vulkan.Core10.Enums.ShaderStageFlagBits  ( ShaderStageFlagBits( SHADER_STAGE_VERTEX_BIT
                                                                     , SHADER_STAGE_TESSELLATION_CONTROL_BIT
                                                                     , SHADER_STAGE_TESSELLATION_EVALUATION_BIT
                                                                     , SHADER_STAGE_GEOMETRY_BIT
                                                                     , SHADER_STAGE_FRAGMENT_BIT
                                                                     , SHADER_STAGE_COMPUTE_BIT
                                                                     , SHADER_STAGE_ALL_GRAPHICS
                                                                     , SHADER_STAGE_ALL
                                                                     , SHADER_STAGE_MESH_BIT_NV
                                                                     , SHADER_STAGE_TASK_BIT_NV
                                                                     , SHADER_STAGE_CALLABLE_BIT_KHR
                                                                     , SHADER_STAGE_INTERSECTION_BIT_KHR
                                                                     , SHADER_STAGE_MISS_BIT_KHR
                                                                     , SHADER_STAGE_CLOSEST_HIT_BIT_KHR
                                                                     , SHADER_STAGE_ANY_HIT_BIT_KHR
                                                                     , SHADER_STAGE_RAYGEN_BIT_KHR
                                                                     , ..
                                                                     )
                                                , ShaderStageFlags
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
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
-- | VkShaderStageFlagBits - Bitmask specifying a pipeline stage
--
-- = Description
--
-- Note
--
-- 'SHADER_STAGE_ALL_GRAPHICS' only includes the original five graphics
-- stages included in Vulkan 1.0, and not any stages added by extensions.
-- Thus, it may not have the desired effect in all cases.
--
-- = See Also
--
-- 'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo',
-- 'ShaderStageFlags',
-- 'Vulkan.Extensions.VK_AMD_shader_info.getShaderInfoAMD'
newtype ShaderStageFlagBits = ShaderStageFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'SHADER_STAGE_VERTEX_BIT' specifies the vertex stage.
pattern SHADER_STAGE_VERTEX_BIT = ShaderStageFlagBits 0x00000001
-- | 'SHADER_STAGE_TESSELLATION_CONTROL_BIT' specifies the tessellation
-- control stage.
pattern SHADER_STAGE_TESSELLATION_CONTROL_BIT = ShaderStageFlagBits 0x00000002
-- | 'SHADER_STAGE_TESSELLATION_EVALUATION_BIT' specifies the tessellation
-- evaluation stage.
pattern SHADER_STAGE_TESSELLATION_EVALUATION_BIT = ShaderStageFlagBits 0x00000004
-- | 'SHADER_STAGE_GEOMETRY_BIT' specifies the geometry stage.
pattern SHADER_STAGE_GEOMETRY_BIT = ShaderStageFlagBits 0x00000008
-- | 'SHADER_STAGE_FRAGMENT_BIT' specifies the fragment stage.
pattern SHADER_STAGE_FRAGMENT_BIT = ShaderStageFlagBits 0x00000010
-- | 'SHADER_STAGE_COMPUTE_BIT' specifies the compute stage.
pattern SHADER_STAGE_COMPUTE_BIT = ShaderStageFlagBits 0x00000020
-- | 'SHADER_STAGE_ALL_GRAPHICS' is a combination of bits used as shorthand
-- to specify all graphics stages defined above (excluding the compute
-- stage).
pattern SHADER_STAGE_ALL_GRAPHICS = ShaderStageFlagBits 0x0000001f
-- | 'SHADER_STAGE_ALL' is a combination of bits used as shorthand to specify
-- all shader stages supported by the device, including all additional
-- stages which are introduced by extensions.
pattern SHADER_STAGE_ALL = ShaderStageFlagBits 0x7fffffff
-- | 'SHADER_STAGE_MESH_BIT_NV' specifies the mesh stage.
pattern SHADER_STAGE_MESH_BIT_NV = ShaderStageFlagBits 0x00000080
-- | 'SHADER_STAGE_TASK_BIT_NV' specifies the task stage.
pattern SHADER_STAGE_TASK_BIT_NV = ShaderStageFlagBits 0x00000040
-- | 'SHADER_STAGE_CALLABLE_BIT_KHR' specifies the callable stage.
pattern SHADER_STAGE_CALLABLE_BIT_KHR = ShaderStageFlagBits 0x00002000
-- | 'SHADER_STAGE_INTERSECTION_BIT_KHR' specifies the intersection stage.
pattern SHADER_STAGE_INTERSECTION_BIT_KHR = ShaderStageFlagBits 0x00001000
-- | 'SHADER_STAGE_MISS_BIT_KHR' specifies the miss stage.
pattern SHADER_STAGE_MISS_BIT_KHR = ShaderStageFlagBits 0x00000800
-- | 'SHADER_STAGE_CLOSEST_HIT_BIT_KHR' specifies the closest hit stage.
pattern SHADER_STAGE_CLOSEST_HIT_BIT_KHR = ShaderStageFlagBits 0x00000400
-- | 'SHADER_STAGE_ANY_HIT_BIT_KHR' specifies the any-hit stage.
pattern SHADER_STAGE_ANY_HIT_BIT_KHR = ShaderStageFlagBits 0x00000200
-- | 'SHADER_STAGE_RAYGEN_BIT_KHR' specifies the ray generation stage.
pattern SHADER_STAGE_RAYGEN_BIT_KHR = ShaderStageFlagBits 0x00000100

type ShaderStageFlags = ShaderStageFlagBits

instance Show ShaderStageFlagBits where
  showsPrec p = \case
    SHADER_STAGE_VERTEX_BIT -> showString "SHADER_STAGE_VERTEX_BIT"
    SHADER_STAGE_TESSELLATION_CONTROL_BIT -> showString "SHADER_STAGE_TESSELLATION_CONTROL_BIT"
    SHADER_STAGE_TESSELLATION_EVALUATION_BIT -> showString "SHADER_STAGE_TESSELLATION_EVALUATION_BIT"
    SHADER_STAGE_GEOMETRY_BIT -> showString "SHADER_STAGE_GEOMETRY_BIT"
    SHADER_STAGE_FRAGMENT_BIT -> showString "SHADER_STAGE_FRAGMENT_BIT"
    SHADER_STAGE_COMPUTE_BIT -> showString "SHADER_STAGE_COMPUTE_BIT"
    SHADER_STAGE_ALL_GRAPHICS -> showString "SHADER_STAGE_ALL_GRAPHICS"
    SHADER_STAGE_ALL -> showString "SHADER_STAGE_ALL"
    SHADER_STAGE_MESH_BIT_NV -> showString "SHADER_STAGE_MESH_BIT_NV"
    SHADER_STAGE_TASK_BIT_NV -> showString "SHADER_STAGE_TASK_BIT_NV"
    SHADER_STAGE_CALLABLE_BIT_KHR -> showString "SHADER_STAGE_CALLABLE_BIT_KHR"
    SHADER_STAGE_INTERSECTION_BIT_KHR -> showString "SHADER_STAGE_INTERSECTION_BIT_KHR"
    SHADER_STAGE_MISS_BIT_KHR -> showString "SHADER_STAGE_MISS_BIT_KHR"
    SHADER_STAGE_CLOSEST_HIT_BIT_KHR -> showString "SHADER_STAGE_CLOSEST_HIT_BIT_KHR"
    SHADER_STAGE_ANY_HIT_BIT_KHR -> showString "SHADER_STAGE_ANY_HIT_BIT_KHR"
    SHADER_STAGE_RAYGEN_BIT_KHR -> showString "SHADER_STAGE_RAYGEN_BIT_KHR"
    ShaderStageFlagBits x -> showParen (p >= 11) (showString "ShaderStageFlagBits 0x" . showHex x)

instance Read ShaderStageFlagBits where
  readPrec = parens (choose [("SHADER_STAGE_VERTEX_BIT", pure SHADER_STAGE_VERTEX_BIT)
                            , ("SHADER_STAGE_TESSELLATION_CONTROL_BIT", pure SHADER_STAGE_TESSELLATION_CONTROL_BIT)
                            , ("SHADER_STAGE_TESSELLATION_EVALUATION_BIT", pure SHADER_STAGE_TESSELLATION_EVALUATION_BIT)
                            , ("SHADER_STAGE_GEOMETRY_BIT", pure SHADER_STAGE_GEOMETRY_BIT)
                            , ("SHADER_STAGE_FRAGMENT_BIT", pure SHADER_STAGE_FRAGMENT_BIT)
                            , ("SHADER_STAGE_COMPUTE_BIT", pure SHADER_STAGE_COMPUTE_BIT)
                            , ("SHADER_STAGE_ALL_GRAPHICS", pure SHADER_STAGE_ALL_GRAPHICS)
                            , ("SHADER_STAGE_ALL", pure SHADER_STAGE_ALL)
                            , ("SHADER_STAGE_MESH_BIT_NV", pure SHADER_STAGE_MESH_BIT_NV)
                            , ("SHADER_STAGE_TASK_BIT_NV", pure SHADER_STAGE_TASK_BIT_NV)
                            , ("SHADER_STAGE_CALLABLE_BIT_KHR", pure SHADER_STAGE_CALLABLE_BIT_KHR)
                            , ("SHADER_STAGE_INTERSECTION_BIT_KHR", pure SHADER_STAGE_INTERSECTION_BIT_KHR)
                            , ("SHADER_STAGE_MISS_BIT_KHR", pure SHADER_STAGE_MISS_BIT_KHR)
                            , ("SHADER_STAGE_CLOSEST_HIT_BIT_KHR", pure SHADER_STAGE_CLOSEST_HIT_BIT_KHR)
                            , ("SHADER_STAGE_ANY_HIT_BIT_KHR", pure SHADER_STAGE_ANY_HIT_BIT_KHR)
                            , ("SHADER_STAGE_RAYGEN_BIT_KHR", pure SHADER_STAGE_RAYGEN_BIT_KHR)]
                     +++
                     prec 10 (do
                       expectP (Ident "ShaderStageFlagBits")
                       v <- step readPrec
                       pure (ShaderStageFlagBits v)))

