{-# language CPP #-}
-- No documentation found for Chapter "LineRasterizationMode"
module Vulkan.Core14.Enums.LineRasterizationMode  (LineRasterizationMode( LINE_RASTERIZATION_MODE_DEFAULT
                                                                        , LINE_RASTERIZATION_MODE_RECTANGULAR
                                                                        , LINE_RASTERIZATION_MODE_BRESENHAM
                                                                        , LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH
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

-- | VkLineRasterizationMode - Line rasterization modes
--
-- = Description
--
-- -   'LINE_RASTERIZATION_MODE_DEFAULT' is equivalent to
--     'LINE_RASTERIZATION_MODE_RECTANGULAR' if
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@strictLines@
--     is 'Vulkan.Core10.FundamentalTypes.TRUE', otherwise lines are drawn
--     as non-@strictLines@ parallelograms. Both of these modes are defined
--     in
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#primsrast-lines-basic Basic Line Segment Rasterization>.
--
-- -   'LINE_RASTERIZATION_MODE_RECTANGULAR' specifies lines drawn as if
--     they were rectangles extruded from the line
--
-- -   'LINE_RASTERIZATION_MODE_BRESENHAM' specifies lines drawn by
--     determining which pixel diamonds the line intersects and exits, as
--     defined in
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#primsrast-lines-bresenham Bresenham Line Segment Rasterization>.
--
-- -   'LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH' specifies lines drawn
--     if they were rectangles extruded from the line, with alpha falloff,
--     as defined in
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#primsrast-lines-smooth Smooth Lines>.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_line_rasterization VK_EXT_line_rasterization>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_line_rasterization VK_KHR_line_rasterization>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core14.Promoted_From_VK_KHR_line_rasterizationRoadmap.PipelineRasterizationLineStateCreateInfo',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetLineRasterizationModeEXT'
newtype LineRasterizationMode = LineRasterizationMode Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkLineRasterizationMode" "VK_LINE_RASTERIZATION_MODE_DEFAULT"
pattern LINE_RASTERIZATION_MODE_DEFAULT = LineRasterizationMode 0

-- No documentation found for Nested "VkLineRasterizationMode" "VK_LINE_RASTERIZATION_MODE_RECTANGULAR"
pattern LINE_RASTERIZATION_MODE_RECTANGULAR = LineRasterizationMode 1

-- No documentation found for Nested "VkLineRasterizationMode" "VK_LINE_RASTERIZATION_MODE_BRESENHAM"
pattern LINE_RASTERIZATION_MODE_BRESENHAM = LineRasterizationMode 2

-- No documentation found for Nested "VkLineRasterizationMode" "VK_LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH"
pattern LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH = LineRasterizationMode 3

{-# COMPLETE
  LINE_RASTERIZATION_MODE_DEFAULT
  , LINE_RASTERIZATION_MODE_RECTANGULAR
  , LINE_RASTERIZATION_MODE_BRESENHAM
  , LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH ::
    LineRasterizationMode
  #-}

conNameLineRasterizationMode :: String
conNameLineRasterizationMode = "LineRasterizationMode"

enumPrefixLineRasterizationMode :: String
enumPrefixLineRasterizationMode = "LINE_RASTERIZATION_MODE_"

showTableLineRasterizationMode :: [(LineRasterizationMode, String)]
showTableLineRasterizationMode =
  [ (LINE_RASTERIZATION_MODE_DEFAULT, "DEFAULT")
  ,
    ( LINE_RASTERIZATION_MODE_RECTANGULAR
    , "RECTANGULAR"
    )
  ,
    ( LINE_RASTERIZATION_MODE_BRESENHAM
    , "BRESENHAM"
    )
  ,
    ( LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH
    , "RECTANGULAR_SMOOTH"
    )
  ]

instance Show LineRasterizationMode where
  showsPrec =
    enumShowsPrec
      enumPrefixLineRasterizationMode
      showTableLineRasterizationMode
      conNameLineRasterizationMode
      (\(LineRasterizationMode x) -> x)
      (showsPrec 11)

instance Read LineRasterizationMode where
  readPrec =
    enumReadPrec
      enumPrefixLineRasterizationMode
      showTableLineRasterizationMode
      conNameLineRasterizationMode
      LineRasterizationMode
