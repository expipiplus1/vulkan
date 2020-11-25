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
type ShaderStageFlags = ShaderStageFlagBits

-- No documentation found for TopLevel "VkShaderStageFlagBits"
newtype ShaderStageFlagBits = ShaderStageFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkShaderStageFlagBits" "VK_SHADER_STAGE_VERTEX_BIT"
pattern SHADER_STAGE_VERTEX_BIT                  = ShaderStageFlagBits 0x00000001
-- No documentation found for Nested "VkShaderStageFlagBits" "VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT"
pattern SHADER_STAGE_TESSELLATION_CONTROL_BIT    = ShaderStageFlagBits 0x00000002
-- No documentation found for Nested "VkShaderStageFlagBits" "VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT"
pattern SHADER_STAGE_TESSELLATION_EVALUATION_BIT = ShaderStageFlagBits 0x00000004
-- No documentation found for Nested "VkShaderStageFlagBits" "VK_SHADER_STAGE_GEOMETRY_BIT"
pattern SHADER_STAGE_GEOMETRY_BIT                = ShaderStageFlagBits 0x00000008
-- No documentation found for Nested "VkShaderStageFlagBits" "VK_SHADER_STAGE_FRAGMENT_BIT"
pattern SHADER_STAGE_FRAGMENT_BIT                = ShaderStageFlagBits 0x00000010
-- No documentation found for Nested "VkShaderStageFlagBits" "VK_SHADER_STAGE_COMPUTE_BIT"
pattern SHADER_STAGE_COMPUTE_BIT                 = ShaderStageFlagBits 0x00000020
-- No documentation found for Nested "VkShaderStageFlagBits" "VK_SHADER_STAGE_ALL_GRAPHICS"
pattern SHADER_STAGE_ALL_GRAPHICS                = ShaderStageFlagBits 0x0000001f
-- No documentation found for Nested "VkShaderStageFlagBits" "VK_SHADER_STAGE_ALL"
pattern SHADER_STAGE_ALL                         = ShaderStageFlagBits 0x7fffffff
-- No documentation found for Nested "VkShaderStageFlagBits" "VK_SHADER_STAGE_MESH_BIT_NV"
pattern SHADER_STAGE_MESH_BIT_NV                 = ShaderStageFlagBits 0x00000080
-- No documentation found for Nested "VkShaderStageFlagBits" "VK_SHADER_STAGE_TASK_BIT_NV"
pattern SHADER_STAGE_TASK_BIT_NV                 = ShaderStageFlagBits 0x00000040
-- No documentation found for Nested "VkShaderStageFlagBits" "VK_SHADER_STAGE_CALLABLE_BIT_KHR"
pattern SHADER_STAGE_CALLABLE_BIT_KHR            = ShaderStageFlagBits 0x00002000
-- No documentation found for Nested "VkShaderStageFlagBits" "VK_SHADER_STAGE_INTERSECTION_BIT_KHR"
pattern SHADER_STAGE_INTERSECTION_BIT_KHR        = ShaderStageFlagBits 0x00001000
-- No documentation found for Nested "VkShaderStageFlagBits" "VK_SHADER_STAGE_MISS_BIT_KHR"
pattern SHADER_STAGE_MISS_BIT_KHR                = ShaderStageFlagBits 0x00000800
-- No documentation found for Nested "VkShaderStageFlagBits" "VK_SHADER_STAGE_CLOSEST_HIT_BIT_KHR"
pattern SHADER_STAGE_CLOSEST_HIT_BIT_KHR         = ShaderStageFlagBits 0x00000400
-- No documentation found for Nested "VkShaderStageFlagBits" "VK_SHADER_STAGE_ANY_HIT_BIT_KHR"
pattern SHADER_STAGE_ANY_HIT_BIT_KHR             = ShaderStageFlagBits 0x00000200
-- No documentation found for Nested "VkShaderStageFlagBits" "VK_SHADER_STAGE_RAYGEN_BIT_KHR"
pattern SHADER_STAGE_RAYGEN_BIT_KHR              = ShaderStageFlagBits 0x00000100

conNameShaderStageFlagBits :: String
conNameShaderStageFlagBits = "ShaderStageFlagBits"

enumPrefixShaderStageFlagBits :: String
enumPrefixShaderStageFlagBits = "SHADER_STAGE_"

showTableShaderStageFlagBits :: [(ShaderStageFlagBits, String)]
showTableShaderStageFlagBits =
  [ (SHADER_STAGE_VERTEX_BIT                 , "VERTEX_BIT")
  , (SHADER_STAGE_TESSELLATION_CONTROL_BIT   , "TESSELLATION_CONTROL_BIT")
  , (SHADER_STAGE_TESSELLATION_EVALUATION_BIT, "TESSELLATION_EVALUATION_BIT")
  , (SHADER_STAGE_GEOMETRY_BIT               , "GEOMETRY_BIT")
  , (SHADER_STAGE_FRAGMENT_BIT               , "FRAGMENT_BIT")
  , (SHADER_STAGE_COMPUTE_BIT                , "COMPUTE_BIT")
  , (SHADER_STAGE_ALL_GRAPHICS               , "ALL_GRAPHICS")
  , (SHADER_STAGE_ALL                        , "ALL")
  , (SHADER_STAGE_MESH_BIT_NV                , "MESH_BIT_NV")
  , (SHADER_STAGE_TASK_BIT_NV                , "TASK_BIT_NV")
  , (SHADER_STAGE_CALLABLE_BIT_KHR           , "CALLABLE_BIT_KHR")
  , (SHADER_STAGE_INTERSECTION_BIT_KHR       , "INTERSECTION_BIT_KHR")
  , (SHADER_STAGE_MISS_BIT_KHR               , "MISS_BIT_KHR")
  , (SHADER_STAGE_CLOSEST_HIT_BIT_KHR        , "CLOSEST_HIT_BIT_KHR")
  , (SHADER_STAGE_ANY_HIT_BIT_KHR            , "ANY_HIT_BIT_KHR")
  , (SHADER_STAGE_RAYGEN_BIT_KHR             , "RAYGEN_BIT_KHR")
  ]


instance Show ShaderStageFlagBits where
showsPrec = enumShowsPrec enumPrefixShaderStageFlagBits
                          showTableShaderStageFlagBits
                          conNameShaderStageFlagBits
                          (\(ShaderStageFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read ShaderStageFlagBits where
  readPrec = enumReadPrec enumPrefixShaderStageFlagBits
                          showTableShaderStageFlagBits
                          conNameShaderStageFlagBits
                          ShaderStageFlagBits

