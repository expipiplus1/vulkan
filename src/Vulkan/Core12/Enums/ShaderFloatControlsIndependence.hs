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
-- No documentation found for TopLevel "VkShaderFloatControlsIndependence"
newtype ShaderFloatControlsIndependence = ShaderFloatControlsIndependence Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkShaderFloatControlsIndependence" "VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_32_BIT_ONLY"
pattern SHADER_FLOAT_CONTROLS_INDEPENDENCE_32_BIT_ONLY = ShaderFloatControlsIndependence 0
-- No documentation found for Nested "VkShaderFloatControlsIndependence" "VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_ALL"
pattern SHADER_FLOAT_CONTROLS_INDEPENDENCE_ALL         = ShaderFloatControlsIndependence 1
-- No documentation found for Nested "VkShaderFloatControlsIndependence" "VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_NONE"
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

