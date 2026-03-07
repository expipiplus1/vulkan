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

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))

-- | VkBorderColor - Specify border color used for texture lookups
--
-- = Description
--
-- -   'BORDER_COLOR_FLOAT_TRANSPARENT_BLACK' specifies a transparent,
--     floating-point format, black color.
--
-- -   'BORDER_COLOR_INT_TRANSPARENT_BLACK' specifies a transparent,
--     integer format, black color.
--
-- -   'BORDER_COLOR_FLOAT_OPAQUE_BLACK' specifies an opaque,
--     floating-point format, black color.
--
-- -   'BORDER_COLOR_INT_OPAQUE_BLACK' specifies an opaque, integer format,
--     black color.
--
-- -   'BORDER_COLOR_FLOAT_OPAQUE_WHITE' specifies an opaque,
--     floating-point format, white color.
--
-- -   'BORDER_COLOR_INT_OPAQUE_WHITE' specifies an opaque, integer format,
--     white color.
--
-- -   'BORDER_COLOR_FLOAT_CUSTOM_EXT' specifies that a
--     'Vulkan.Extensions.VK_EXT_custom_border_color.SamplerCustomBorderColorCreateInfoEXT'
--     structure is included in the
--     'Vulkan.Core10.Sampler.SamplerCreateInfo'::@pNext@ chain containing
--     the color data in floating-point format.
--
-- -   'BORDER_COLOR_INT_CUSTOM_EXT' specifies that a
--     'Vulkan.Extensions.VK_EXT_custom_border_color.SamplerCustomBorderColorCreateInfoEXT'
--     structure is included in the
--     'Vulkan.Core10.Sampler.SamplerCreateInfo'::@pNext@ chain containing
--     the color data in integer format.
--
-- These colors are described in detail in
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#textures-border-replacement Border Replacement>.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Sampler.SamplerCreateInfo'
newtype BorderColor = BorderColor Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkBorderColor" "VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK"
pattern BORDER_COLOR_FLOAT_TRANSPARENT_BLACK = BorderColor 0

-- No documentation found for Nested "VkBorderColor" "VK_BORDER_COLOR_INT_TRANSPARENT_BLACK"
pattern BORDER_COLOR_INT_TRANSPARENT_BLACK = BorderColor 1

-- No documentation found for Nested "VkBorderColor" "VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK"
pattern BORDER_COLOR_FLOAT_OPAQUE_BLACK = BorderColor 2

-- No documentation found for Nested "VkBorderColor" "VK_BORDER_COLOR_INT_OPAQUE_BLACK"
pattern BORDER_COLOR_INT_OPAQUE_BLACK = BorderColor 3

-- No documentation found for Nested "VkBorderColor" "VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE"
pattern BORDER_COLOR_FLOAT_OPAQUE_WHITE = BorderColor 4

-- No documentation found for Nested "VkBorderColor" "VK_BORDER_COLOR_INT_OPAQUE_WHITE"
pattern BORDER_COLOR_INT_OPAQUE_WHITE = BorderColor 5

-- No documentation found for Nested "VkBorderColor" "VK_BORDER_COLOR_INT_CUSTOM_EXT"
pattern BORDER_COLOR_INT_CUSTOM_EXT = BorderColor 1000287004

-- No documentation found for Nested "VkBorderColor" "VK_BORDER_COLOR_FLOAT_CUSTOM_EXT"
pattern BORDER_COLOR_FLOAT_CUSTOM_EXT = BorderColor 1000287003

{-# COMPLETE
  BORDER_COLOR_FLOAT_TRANSPARENT_BLACK
  , BORDER_COLOR_INT_TRANSPARENT_BLACK
  , BORDER_COLOR_FLOAT_OPAQUE_BLACK
  , BORDER_COLOR_INT_OPAQUE_BLACK
  , BORDER_COLOR_FLOAT_OPAQUE_WHITE
  , BORDER_COLOR_INT_OPAQUE_WHITE
  , BORDER_COLOR_INT_CUSTOM_EXT
  , BORDER_COLOR_FLOAT_CUSTOM_EXT ::
    BorderColor
  #-}

conNameBorderColor :: String
conNameBorderColor = "BorderColor"

enumPrefixBorderColor :: String
enumPrefixBorderColor = "BORDER_COLOR_"

showTableBorderColor :: [(BorderColor, String)]
showTableBorderColor =
  [
    ( BORDER_COLOR_FLOAT_TRANSPARENT_BLACK
    , "FLOAT_TRANSPARENT_BLACK"
    )
  ,
    ( BORDER_COLOR_INT_TRANSPARENT_BLACK
    , "INT_TRANSPARENT_BLACK"
    )
  , (BORDER_COLOR_FLOAT_OPAQUE_BLACK, "FLOAT_OPAQUE_BLACK")
  , (BORDER_COLOR_INT_OPAQUE_BLACK, "INT_OPAQUE_BLACK")
  , (BORDER_COLOR_FLOAT_OPAQUE_WHITE, "FLOAT_OPAQUE_WHITE")
  , (BORDER_COLOR_INT_OPAQUE_WHITE, "INT_OPAQUE_WHITE")
  , (BORDER_COLOR_INT_CUSTOM_EXT, "INT_CUSTOM_EXT")
  , (BORDER_COLOR_FLOAT_CUSTOM_EXT, "FLOAT_CUSTOM_EXT")
  ]

instance Show BorderColor where
  showsPrec =
    enumShowsPrec
      enumPrefixBorderColor
      showTableBorderColor
      conNameBorderColor
      (\(BorderColor x) -> x)
      (showsPrec 11)

instance Read BorderColor where
  readPrec =
    enumReadPrec
      enumPrefixBorderColor
      showTableBorderColor
      conNameBorderColor
      BorderColor
