{-# language CPP #-}
-- No documentation found for Chapter "ShaderFloatControlsIndependence"
module Vulkan.Core12.Enums.ShaderFloatControlsIndependence  (ShaderFloatControlsIndependence( SHADER_FLOAT_CONTROLS_INDEPENDENCE_32_BIT_ONLY
                                                                                            , SHADER_FLOAT_CONTROLS_INDEPENDENCE_ALL
                                                                                            , SHADER_FLOAT_CONTROLS_INDEPENDENCE_NONE
                                                                                            , ..
                                                                                            )) where

import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
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
pattern SHADER_FLOAT_CONTROLS_INDEPENDENCE_ALL = ShaderFloatControlsIndependence 1
-- | 'SHADER_FLOAT_CONTROLS_INDEPENDENCE_NONE' specifies that shader float
-- controls for all bit widths /must/ be set identically.
pattern SHADER_FLOAT_CONTROLS_INDEPENDENCE_NONE = ShaderFloatControlsIndependence 2
{-# complete SHADER_FLOAT_CONTROLS_INDEPENDENCE_32_BIT_ONLY,
             SHADER_FLOAT_CONTROLS_INDEPENDENCE_ALL,
             SHADER_FLOAT_CONTROLS_INDEPENDENCE_NONE :: ShaderFloatControlsIndependence #-}

instance Show ShaderFloatControlsIndependence where
  showsPrec p = \case
    SHADER_FLOAT_CONTROLS_INDEPENDENCE_32_BIT_ONLY -> showString "SHADER_FLOAT_CONTROLS_INDEPENDENCE_32_BIT_ONLY"
    SHADER_FLOAT_CONTROLS_INDEPENDENCE_ALL -> showString "SHADER_FLOAT_CONTROLS_INDEPENDENCE_ALL"
    SHADER_FLOAT_CONTROLS_INDEPENDENCE_NONE -> showString "SHADER_FLOAT_CONTROLS_INDEPENDENCE_NONE"
    ShaderFloatControlsIndependence x -> showParen (p >= 11) (showString "ShaderFloatControlsIndependence " . showsPrec 11 x)

instance Read ShaderFloatControlsIndependence where
  readPrec = parens (choose [("SHADER_FLOAT_CONTROLS_INDEPENDENCE_32_BIT_ONLY", pure SHADER_FLOAT_CONTROLS_INDEPENDENCE_32_BIT_ONLY)
                            , ("SHADER_FLOAT_CONTROLS_INDEPENDENCE_ALL", pure SHADER_FLOAT_CONTROLS_INDEPENDENCE_ALL)
                            , ("SHADER_FLOAT_CONTROLS_INDEPENDENCE_NONE", pure SHADER_FLOAT_CONTROLS_INDEPENDENCE_NONE)]
                     +++
                     prec 10 (do
                       expectP (Ident "ShaderFloatControlsIndependence")
                       v <- step readPrec
                       pure (ShaderFloatControlsIndependence v)))

