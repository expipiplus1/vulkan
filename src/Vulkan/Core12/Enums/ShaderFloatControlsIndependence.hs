{-# language CPP #-}
-- No documentation found for Chapter "ShaderFloatControlsIndependence"
module Vulkan.Core12.Enums.ShaderFloatControlsIndependence  (ShaderFloatControlsIndependence( SHADER_FLOAT_CONTROLS_INDEPENDENCE_32_BIT_ONLY
                                                                                            , SHADER_FLOAT_CONTROLS_INDEPENDENCE_ALL
                                                                                            , SHADER_FLOAT_CONTROLS_INDEPENDENCE_NONE
                                                                                            , ..
                                                                                            )) where

import Data.Foldable (asum)
import GHC.Base ((<$))
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Text.ParserCombinators.ReadP (skipSpaces)
import Text.ParserCombinators.ReadP (string)
import Text.ParserCombinators.ReadPrec ((+++))
import qualified Text.ParserCombinators.ReadPrec (lift)
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
  showsPrec p e = case lookup e showTableShaderFloatControlsIndependence of
    Just s -> showString enumPrefixShaderFloatControlsIndependence . showString s
    Nothing ->
      let ShaderFloatControlsIndependence x = e
      in  showParen (p >= 11) (showString conNameShaderFloatControlsIndependence . showString " " . showsPrec 11 x)

instance Read ShaderFloatControlsIndependence where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixShaderFloatControlsIndependence
          asum ((\(e, s) -> e <$ string s) <$> showTableShaderFloatControlsIndependence)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameShaderFloatControlsIndependence)
            v <- step readPrec
            pure (ShaderFloatControlsIndependence v)
          )
    )

