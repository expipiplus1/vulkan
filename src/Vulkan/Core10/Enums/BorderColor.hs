{-# language CPP #-}
-- No documentation found for Chapter "BorderColor"
module Vulkan.Core10.Enums.BorderColor  (BorderColor( BORDER_COLOR_FLOAT_TRANSPARENT_BLACK
                                                    , BORDER_COLOR_INT_TRANSPARENT_BLACK
                                                    , BORDER_COLOR_FLOAT_OPAQUE_BLACK
                                                    , BORDER_COLOR_INT_OPAQUE_BLACK
                                                    , BORDER_COLOR_FLOAT_OPAQUE_WHITE
                                                    , BORDER_COLOR_INT_OPAQUE_WHITE
                                                    , BORDER_COLOR_INT_CUSTOM_EXT
                                                    , BORDER_COLOR_FLOAT_CUSTOM_EXT
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
-- | VkBorderColor - Specify border color used for texture lookups
--
-- = Description
--
-- These colors are described in detail in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-texel-replacement Texel Replacement>.
--
-- = See Also
--
-- 'Vulkan.Core10.Sampler.SamplerCreateInfo'
newtype BorderColor = BorderColor Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'BORDER_COLOR_FLOAT_TRANSPARENT_BLACK' specifies a transparent,
-- floating-point format, black color.
pattern BORDER_COLOR_FLOAT_TRANSPARENT_BLACK = BorderColor 0
-- | 'BORDER_COLOR_INT_TRANSPARENT_BLACK' specifies a transparent, integer
-- format, black color.
pattern BORDER_COLOR_INT_TRANSPARENT_BLACK   = BorderColor 1
-- | 'BORDER_COLOR_FLOAT_OPAQUE_BLACK' specifies an opaque, floating-point
-- format, black color.
pattern BORDER_COLOR_FLOAT_OPAQUE_BLACK      = BorderColor 2
-- | 'BORDER_COLOR_INT_OPAQUE_BLACK' specifies an opaque, integer format,
-- black color.
pattern BORDER_COLOR_INT_OPAQUE_BLACK        = BorderColor 3
-- | 'BORDER_COLOR_FLOAT_OPAQUE_WHITE' specifies an opaque, floating-point
-- format, white color.
pattern BORDER_COLOR_FLOAT_OPAQUE_WHITE      = BorderColor 4
-- | 'BORDER_COLOR_INT_OPAQUE_WHITE' specifies an opaque, integer format,
-- white color.
pattern BORDER_COLOR_INT_OPAQUE_WHITE        = BorderColor 5
-- | 'BORDER_COLOR_INT_CUSTOM_EXT' indicates that a
-- 'Vulkan.Extensions.VK_EXT_custom_border_color.SamplerCustomBorderColorCreateInfoEXT'
-- structure is present in the
-- 'Vulkan.Core10.Sampler.SamplerCreateInfo'::@pNext@ chain which contains
-- the color data in integer format.
pattern BORDER_COLOR_INT_CUSTOM_EXT          = BorderColor 1000287004
-- | 'BORDER_COLOR_FLOAT_CUSTOM_EXT' indicates that a
-- 'Vulkan.Extensions.VK_EXT_custom_border_color.SamplerCustomBorderColorCreateInfoEXT'
-- structure is present in the
-- 'Vulkan.Core10.Sampler.SamplerCreateInfo'::@pNext@ chain which contains
-- the color data in floating-point format.
pattern BORDER_COLOR_FLOAT_CUSTOM_EXT        = BorderColor 1000287003
{-# complete BORDER_COLOR_FLOAT_TRANSPARENT_BLACK,
             BORDER_COLOR_INT_TRANSPARENT_BLACK,
             BORDER_COLOR_FLOAT_OPAQUE_BLACK,
             BORDER_COLOR_INT_OPAQUE_BLACK,
             BORDER_COLOR_FLOAT_OPAQUE_WHITE,
             BORDER_COLOR_INT_OPAQUE_WHITE,
             BORDER_COLOR_INT_CUSTOM_EXT,
             BORDER_COLOR_FLOAT_CUSTOM_EXT :: BorderColor #-}

conNameBorderColor :: String
conNameBorderColor = "BorderColor"

enumPrefixBorderColor :: String
enumPrefixBorderColor = "BORDER_COLOR_"

showTableBorderColor :: [(BorderColor, String)]
showTableBorderColor =
  [ (BORDER_COLOR_FLOAT_TRANSPARENT_BLACK, "FLOAT_TRANSPARENT_BLACK")
  , (BORDER_COLOR_INT_TRANSPARENT_BLACK  , "INT_TRANSPARENT_BLACK")
  , (BORDER_COLOR_FLOAT_OPAQUE_BLACK     , "FLOAT_OPAQUE_BLACK")
  , (BORDER_COLOR_INT_OPAQUE_BLACK       , "INT_OPAQUE_BLACK")
  , (BORDER_COLOR_FLOAT_OPAQUE_WHITE     , "FLOAT_OPAQUE_WHITE")
  , (BORDER_COLOR_INT_OPAQUE_WHITE       , "INT_OPAQUE_WHITE")
  , (BORDER_COLOR_INT_CUSTOM_EXT         , "INT_CUSTOM_EXT")
  , (BORDER_COLOR_FLOAT_CUSTOM_EXT       , "FLOAT_CUSTOM_EXT")
  ]

instance Show BorderColor where
  showsPrec p e = case lookup e showTableBorderColor of
    Just s -> showString enumPrefixBorderColor . showString s
    Nothing ->
      let BorderColor x = e in showParen (p >= 11) (showString conNameBorderColor . showString " " . showsPrec 11 x)

instance Read BorderColor where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixBorderColor
          asum ((\(e, s) -> e <$ string s) <$> showTableBorderColor)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameBorderColor)
            v <- step readPrec
            pure (BorderColor v)
          )
    )

