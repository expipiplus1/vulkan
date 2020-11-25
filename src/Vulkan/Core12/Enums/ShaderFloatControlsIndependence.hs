{-# language CPP #-}
-- No documentation found for Chapter "ShaderFloatControlsIndependence"
module Vulkan.Core12.Enums.ShaderFloatControlsIndependence  (ShaderFloatControlsIndependence( SHADER_FLOAT_CONTROLS_INDEPENDENCE_32_BIT_ONLY
                                                                                            , SHADER_FLOAT_CONTROLS_INDEPENDENCE_ALL
                                                                                            , SHADER_FLOAT_CONTROLS_INDEPENDENCE_NONE
                                                                                            , ..
                                                                                            )) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Zero (Zero)
-- | VkShaderFloatControlsIndependence - Enum specifying whether, and how,
-- shader float controls can be set separately
--
-- = See Also
--
-- 'Vulkan.Core12.Promoted_From_VK_KHR_shader_float_controls.PhysicalDeviceFloatControlsProperties',
-- 'Vulkan.Core12.PhysicalDeviceVulkan12Properties'
newtype ShaderFloatControlsIndependence = ShaderFloatControlsIndependence Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'SHADER_FLOAT_CONTROLS_INDEPENDENCE_32_BIT_ONLY' specifies that shader
-- float controls for 32-bit floating point /can/ be set independently;
-- other bit widths /must/ be set identically to each other.
pattern SHADER_FLOAT_CONTROLS_INDEPENDENCE_32_BIT_ONLY = ShaderFloatControlsIndependence 0
-- | 'SHADER_FLOAT_CONTROLS_INDEPENDENCE_ALL' specifies that shader float
-- controls for all bit widths /can/ be set independently.
pattern SHADER_FLOAT_CONTROLS_INDEPENDENCE_ALL         = ShaderFloatControlsIndependence 1
-- | 'SHADER_FLOAT_CONTROLS_INDEPENDENCE_NONE' specifies that shader float
-- controls for all bit widths /must/ be set identically.
pattern SHADER_FLOAT_CONTROLS_INDEPENDENCE_NONE        = ShaderFloatControlsIndependence 2
{-# complete SHADER_FLOAT_CONTROLS_INDEPENDENCE_32_BIT_ONLY,
             SHADER_FLOAT_CONTROLS_INDEPENDENCE_ALL,
             SHADER_FLOAT_CONTROLS_INDEPENDENCE_NONE :: ShaderFloatControlsIndependence #-}

conNameShaderFloatControlsIndependence :: String
conNameShaderFloatControlsIndependence = "ShaderFloatControlsIndependence"

enumPrefixShaderFloatControlsIndependence :: String
enumPrefixShaderFloatControlsIndependence = "SHADER_FLOAT_CONTROLS_INDEPENDENCE_"

showTableShaderFloatControlsIndependence :: [(ShaderFloatControlsIndependence, String)]
showTableShaderFloatControlsIndependence =
  [ (SHADER_FLOAT_CONTROLS_INDEPENDENCE_32_BIT_ONLY, "32_BIT_ONLY")
  , (SHADER_FLOAT_CONTROLS_INDEPENDENCE_ALL        , "ALL")
  , (SHADER_FLOAT_CONTROLS_INDEPENDENCE_NONE       , "NONE")
  ]

instance Show ShaderFloatControlsIndependence where
  showsPrec = enumShowsPrec enumPrefixShaderFloatControlsIndependence
                            showTableShaderFloatControlsIndependence
                            conNameShaderFloatControlsIndependence
                            (\(ShaderFloatControlsIndependence x) -> x)
                            (showsPrec 11)

instance Read ShaderFloatControlsIndependence where
  readPrec = enumReadPrec enumPrefixShaderFloatControlsIndependence
                          showTableShaderFloatControlsIndependence
                          conNameShaderFloatControlsIndependence
                          ShaderFloatControlsIndependence

