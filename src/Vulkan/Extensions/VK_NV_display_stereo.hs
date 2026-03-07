{-# language CPP #-}
-- | = Name
--
-- VK_NV_display_stereo - instance extension
--
-- = VK_NV_display_stereo
--
-- [__Name String__]
--     @VK_NV_display_stereo@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     552
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_display VK_KHR_display>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_display_properties2 VK_KHR_get_display_properties2>
--
-- [__Contact__]
--
--     -   Russell Chou
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_display_stereo] @russellcnv%0A*Here describe the issue or question you have about the VK_NV_display_stereo extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_NV_display_stereo.adoc VK_NV_display_stereo>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-11-20
--
-- [__Contributors__]
--
--     -   Russell Chou, NVIDIA
--
--     -   Jeff Juliano, NVIDIA
--
--     -   James Jones, NVIDIA
--
-- == Description
--
-- This extension allows the application to choose which type of 3D stereo
-- hardware it wants to use so the driver can configure it properly. This
-- configuration is useful for swapchains created from display surfaces
-- because some environments do not have an intermediate windowing system
-- available for easy configuration. This extension will override any
-- stereo type configuration in the windowing system.
--
-- For HDMI 3D, only some display modes support stereo rendering, and a new
-- structure is needed to expose that information to the application.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_get_display_properties2.DisplayModeProperties2KHR':
--
--     -   'DisplayModeStereoPropertiesNV'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_display.DisplaySurfaceCreateInfoKHR':
--
--     -   'DisplaySurfaceStereoCreateInfoNV'
--
-- == New Enums
--
-- -   'DisplaySurfaceStereoTypeNV'
--
-- == New Enum Constants
--
-- -   'NV_DISPLAY_STEREO_EXTENSION_NAME'
--
-- -   'NV_DISPLAY_STEREO_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DISPLAY_MODE_STEREO_PROPERTIES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DISPLAY_SURFACE_STEREO_CREATE_INFO_NV'
--
-- == Version History
--
-- -   Revision 1, 2024-11-20 (Russell Chou)
--
--     -   Initial release
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_NV_display_stereo Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_display_stereo  ( DisplaySurfaceStereoCreateInfoNV(..)
                                               , DisplayModeStereoPropertiesNV(..)
                                               , DisplaySurfaceStereoTypeNV( DISPLAY_SURFACE_STEREO_TYPE_NONE_NV
                                                                           , DISPLAY_SURFACE_STEREO_TYPE_ONBOARD_DIN_NV
                                                                           , DISPLAY_SURFACE_STEREO_TYPE_HDMI_3D_NV
                                                                           , DISPLAY_SURFACE_STEREO_TYPE_INBAND_DISPLAYPORT_NV
                                                                           , ..
                                                                           )
                                               , NV_DISPLAY_STEREO_SPEC_VERSION
                                               , pattern NV_DISPLAY_STEREO_SPEC_VERSION
                                               , NV_DISPLAY_STEREO_EXTENSION_NAME
                                               , pattern NV_DISPLAY_STEREO_EXTENSION_NAME
                                               ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Data.Int (Int32)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DISPLAY_MODE_STEREO_PROPERTIES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DISPLAY_SURFACE_STEREO_CREATE_INFO_NV))
-- | VkDisplaySurfaceStereoCreateInfoNV - Structure specifying stereo
-- parameters of a newly created display plane surface object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_display_stereo VK_NV_display_stereo>,
-- 'DisplaySurfaceStereoTypeNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DisplaySurfaceStereoCreateInfoNV = DisplaySurfaceStereoCreateInfoNV
  { -- | #wsi-displaySurfaceStereoType# @stereoType@ is a
    -- 'DisplaySurfaceStereoTypeNV' value specifying the type of 3D stereo
    -- presentation the display will be configured for.
    --
    -- #VUID-VkDisplaySurfaceStereoCreateInfoNV-stereoType-parameter#
    -- @stereoType@ /must/ be a valid 'DisplaySurfaceStereoTypeNV' value
    stereoType :: DisplaySurfaceStereoTypeNV }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DisplaySurfaceStereoCreateInfoNV)
#endif
deriving instance Show DisplaySurfaceStereoCreateInfoNV

instance ToCStruct DisplaySurfaceStereoCreateInfoNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DisplaySurfaceStereoCreateInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_SURFACE_STEREO_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DisplaySurfaceStereoTypeNV)) (stereoType)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_SURFACE_STEREO_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DisplaySurfaceStereoTypeNV)) (zero)
    f

instance FromCStruct DisplaySurfaceStereoCreateInfoNV where
  peekCStruct p = do
    stereoType <- peek @DisplaySurfaceStereoTypeNV ((p `plusPtr` 16 :: Ptr DisplaySurfaceStereoTypeNV))
    pure $ DisplaySurfaceStereoCreateInfoNV
             stereoType

instance Storable DisplaySurfaceStereoCreateInfoNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DisplaySurfaceStereoCreateInfoNV where
  zero = DisplaySurfaceStereoCreateInfoNV
           zero


-- | VkDisplayModeStereoPropertiesNV - Structure describing the stereo
-- properties of a display mode
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_display_stereo VK_NV_display_stereo>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DisplayModeStereoPropertiesNV = DisplayModeStereoPropertiesNV
  { -- | @hdmi3DSupported@ indicates whether this display mode can be used for a
    -- display surface configured for 'DISPLAY_SURFACE_STEREO_TYPE_HDMI_3D_NV'.
    hdmi3DSupported :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DisplayModeStereoPropertiesNV)
#endif
deriving instance Show DisplayModeStereoPropertiesNV

instance ToCStruct DisplayModeStereoPropertiesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DisplayModeStereoPropertiesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_MODE_STEREO_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (hdmi3DSupported))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_MODE_STEREO_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct DisplayModeStereoPropertiesNV where
  peekCStruct p = do
    hdmi3DSupported <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ DisplayModeStereoPropertiesNV
             (bool32ToBool hdmi3DSupported)

instance Storable DisplayModeStereoPropertiesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DisplayModeStereoPropertiesNV where
  zero = DisplayModeStereoPropertiesNV
           zero


-- | VkDisplaySurfaceStereoTypeNV - 3D Stereo type
--
-- = Description
--
-- -   'DISPLAY_SURFACE_STEREO_TYPE_NONE_NV' specifies no configuration for
--     stereo presentation. This is the default behavior if
--     'DisplaySurfaceStereoCreateInfoNV' is not provided.
--
-- -   'DISPLAY_SURFACE_STEREO_TYPE_ONBOARD_DIN_NV' specifies configuration
--     for glasses that connect via a DIN connector on the back of the
--     graphics card.
--
-- -   'DISPLAY_SURFACE_STEREO_TYPE_HDMI_3D_NV' specifies configuration for
--     HDMI 3D compatible display devices with their own stereo emitters.
--     This is also known as HDMI Frame Packed Stereo, where the left and
--     right eye images are stacked into a single frame with a doubled
--     pixel clock and refresh rate.
--
-- -   'DISPLAY_SURFACE_STEREO_TYPE_INBAND_DISPLAYPORT_NV' specifies
--     configuration for DisplayPort display devices with in-band stereo
--     signaling and emitters.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_display_stereo VK_NV_display_stereo>,
-- 'DisplaySurfaceStereoCreateInfoNV'
newtype DisplaySurfaceStereoTypeNV = DisplaySurfaceStereoTypeNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkDisplaySurfaceStereoTypeNV" "VK_DISPLAY_SURFACE_STEREO_TYPE_NONE_NV"
pattern DISPLAY_SURFACE_STEREO_TYPE_NONE_NV = DisplaySurfaceStereoTypeNV 0

-- No documentation found for Nested "VkDisplaySurfaceStereoTypeNV" "VK_DISPLAY_SURFACE_STEREO_TYPE_ONBOARD_DIN_NV"
pattern DISPLAY_SURFACE_STEREO_TYPE_ONBOARD_DIN_NV = DisplaySurfaceStereoTypeNV 1

-- No documentation found for Nested "VkDisplaySurfaceStereoTypeNV" "VK_DISPLAY_SURFACE_STEREO_TYPE_HDMI_3D_NV"
pattern DISPLAY_SURFACE_STEREO_TYPE_HDMI_3D_NV = DisplaySurfaceStereoTypeNV 2

-- No documentation found for Nested "VkDisplaySurfaceStereoTypeNV" "VK_DISPLAY_SURFACE_STEREO_TYPE_INBAND_DISPLAYPORT_NV"
pattern DISPLAY_SURFACE_STEREO_TYPE_INBAND_DISPLAYPORT_NV = DisplaySurfaceStereoTypeNV 3

{-# COMPLETE
  DISPLAY_SURFACE_STEREO_TYPE_NONE_NV
  , DISPLAY_SURFACE_STEREO_TYPE_ONBOARD_DIN_NV
  , DISPLAY_SURFACE_STEREO_TYPE_HDMI_3D_NV
  , DISPLAY_SURFACE_STEREO_TYPE_INBAND_DISPLAYPORT_NV ::
    DisplaySurfaceStereoTypeNV
  #-}

conNameDisplaySurfaceStereoTypeNV :: String
conNameDisplaySurfaceStereoTypeNV = "DisplaySurfaceStereoTypeNV"

enumPrefixDisplaySurfaceStereoTypeNV :: String
enumPrefixDisplaySurfaceStereoTypeNV = "DISPLAY_SURFACE_STEREO_TYPE_"

showTableDisplaySurfaceStereoTypeNV :: [(DisplaySurfaceStereoTypeNV, String)]
showTableDisplaySurfaceStereoTypeNV =
  [
    ( DISPLAY_SURFACE_STEREO_TYPE_NONE_NV
    , "NONE_NV"
    )
  ,
    ( DISPLAY_SURFACE_STEREO_TYPE_ONBOARD_DIN_NV
    , "ONBOARD_DIN_NV"
    )
  ,
    ( DISPLAY_SURFACE_STEREO_TYPE_HDMI_3D_NV
    , "HDMI_3D_NV"
    )
  ,
    ( DISPLAY_SURFACE_STEREO_TYPE_INBAND_DISPLAYPORT_NV
    , "INBAND_DISPLAYPORT_NV"
    )
  ]

instance Show DisplaySurfaceStereoTypeNV where
  showsPrec =
    enumShowsPrec
      enumPrefixDisplaySurfaceStereoTypeNV
      showTableDisplaySurfaceStereoTypeNV
      conNameDisplaySurfaceStereoTypeNV
      (\(DisplaySurfaceStereoTypeNV x) -> x)
      (showsPrec 11)

instance Read DisplaySurfaceStereoTypeNV where
  readPrec =
    enumReadPrec
      enumPrefixDisplaySurfaceStereoTypeNV
      showTableDisplaySurfaceStereoTypeNV
      conNameDisplaySurfaceStereoTypeNV
      DisplaySurfaceStereoTypeNV

type NV_DISPLAY_STEREO_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_DISPLAY_STEREO_SPEC_VERSION"
pattern NV_DISPLAY_STEREO_SPEC_VERSION :: forall a . Integral a => a
pattern NV_DISPLAY_STEREO_SPEC_VERSION = 1


type NV_DISPLAY_STEREO_EXTENSION_NAME = "VK_NV_display_stereo"

-- No documentation found for TopLevel "VK_NV_DISPLAY_STEREO_EXTENSION_NAME"
pattern NV_DISPLAY_STEREO_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_DISPLAY_STEREO_EXTENSION_NAME = "VK_NV_display_stereo"

