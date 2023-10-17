{-# language CPP #-}
-- No documentation found for Chapter "ShaderStageFlagBits"
module Vulkan.Core10.Enums.ShaderStageFlagBits  ( ShaderStageFlags
                                                , ShaderStageFlagBits( SHADER_STAGE_VERTEX_BIT
                                                                     , SHADER_STAGE_TESSELLATION_CONTROL_BIT
                                                                     , SHADER_STAGE_TESSELLATION_EVALUATION_BIT
                                                                     , SHADER_STAGE_GEOMETRY_BIT
                                                                     , SHADER_STAGE_FRAGMENT_BIT
                                                                     , SHADER_STAGE_COMPUTE_BIT
                                                                     , SHADER_STAGE_ALL_GRAPHICS
                                                                     , SHADER_STAGE_ALL
                                                                     , SHADER_STAGE_CLUSTER_CULLING_BIT_HUAWEI
                                                                     , SHADER_STAGE_SUBPASS_SHADING_BIT_HUAWEI
                                                                     , SHADER_STAGE_MESH_BIT_EXT
                                                                     , SHADER_STAGE_TASK_BIT_EXT
                                                                     , SHADER_STAGE_CALLABLE_BIT_KHR
                                                                     , SHADER_STAGE_INTERSECTION_BIT_KHR
                                                                     , SHADER_STAGE_MISS_BIT_KHR
                                                                     , SHADER_STAGE_CLOSEST_HIT_BIT_KHR
                                                                     , SHADER_STAGE_ANY_HIT_BIT_KHR
                                                                     , SHADER_STAGE_RAYGEN_BIT_KHR
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
type ShaderStageFlags = ShaderStageFlagBits

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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo',
-- 'Vulkan.Extensions.VK_EXT_shader_object.ShaderCreateInfoEXT',
-- 'ShaderStageFlags',
-- 'Vulkan.Extensions.VK_EXT_shader_object.cmdBindShadersEXT',
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

-- | 'SHADER_STAGE_CLUSTER_CULLING_BIT_HUAWEI' specifies the cluster culling
-- stage.
pattern SHADER_STAGE_CLUSTER_CULLING_BIT_HUAWEI = ShaderStageFlagBits 0x00080000

-- No documentation found for Nested "VkShaderStageFlagBits" "VK_SHADER_STAGE_SUBPASS_SHADING_BIT_HUAWEI"
pattern SHADER_STAGE_SUBPASS_SHADING_BIT_HUAWEI = ShaderStageFlagBits 0x00004000

-- | 'SHADER_STAGE_MESH_BIT_EXT' specifies the mesh stage.
pattern SHADER_STAGE_MESH_BIT_EXT = ShaderStageFlagBits 0x00000080

-- | 'SHADER_STAGE_TASK_BIT_EXT' specifies the task stage.
pattern SHADER_STAGE_TASK_BIT_EXT = ShaderStageFlagBits 0x00000040

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

conNameShaderStageFlagBits :: String
conNameShaderStageFlagBits = "ShaderStageFlagBits"

enumPrefixShaderStageFlagBits :: String
enumPrefixShaderStageFlagBits = "SHADER_STAGE_"

showTableShaderStageFlagBits :: [(ShaderStageFlagBits, String)]
showTableShaderStageFlagBits =
  [ (SHADER_STAGE_VERTEX_BIT, "VERTEX_BIT")
  ,
    ( SHADER_STAGE_TESSELLATION_CONTROL_BIT
    , "TESSELLATION_CONTROL_BIT"
    )
  ,
    ( SHADER_STAGE_TESSELLATION_EVALUATION_BIT
    , "TESSELLATION_EVALUATION_BIT"
    )
  , (SHADER_STAGE_GEOMETRY_BIT, "GEOMETRY_BIT")
  , (SHADER_STAGE_FRAGMENT_BIT, "FRAGMENT_BIT")
  , (SHADER_STAGE_COMPUTE_BIT, "COMPUTE_BIT")
  , (SHADER_STAGE_ALL_GRAPHICS, "ALL_GRAPHICS")
  , (SHADER_STAGE_ALL, "ALL")
  ,
    ( SHADER_STAGE_CLUSTER_CULLING_BIT_HUAWEI
    , "CLUSTER_CULLING_BIT_HUAWEI"
    )
  ,
    ( SHADER_STAGE_SUBPASS_SHADING_BIT_HUAWEI
    , "SUBPASS_SHADING_BIT_HUAWEI"
    )
  , (SHADER_STAGE_MESH_BIT_EXT, "MESH_BIT_EXT")
  , (SHADER_STAGE_TASK_BIT_EXT, "TASK_BIT_EXT")
  ,
    ( SHADER_STAGE_CALLABLE_BIT_KHR
    , "CALLABLE_BIT_KHR"
    )
  ,
    ( SHADER_STAGE_INTERSECTION_BIT_KHR
    , "INTERSECTION_BIT_KHR"
    )
  , (SHADER_STAGE_MISS_BIT_KHR, "MISS_BIT_KHR")
  ,
    ( SHADER_STAGE_CLOSEST_HIT_BIT_KHR
    , "CLOSEST_HIT_BIT_KHR"
    )
  ,
    ( SHADER_STAGE_ANY_HIT_BIT_KHR
    , "ANY_HIT_BIT_KHR"
    )
  ,
    ( SHADER_STAGE_RAYGEN_BIT_KHR
    , "RAYGEN_BIT_KHR"
    )
  ]

instance Show ShaderStageFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixShaderStageFlagBits
      showTableShaderStageFlagBits
      conNameShaderStageFlagBits
      (\(ShaderStageFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read ShaderStageFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixShaderStageFlagBits
      showTableShaderStageFlagBits
      conNameShaderStageFlagBits
      ShaderStageFlagBits
