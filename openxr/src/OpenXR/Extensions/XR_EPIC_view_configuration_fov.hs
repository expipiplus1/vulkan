{-# language CPP #-}
-- | = Name
--
-- XR_EPIC_view_configuration_fov - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_EPIC_view_configuration_fov  XR_EPIC_view_configuration_fov>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 60
--
-- = Revision
--
-- 2
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- = See Also
--
-- 'ViewConfigurationViewFovEPIC'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_EPIC_view_configuration_fov OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_EPIC_view_configuration_fov  ( ViewConfigurationViewFovEPIC(..)
                                                         , EPIC_view_configuration_fov_SPEC_VERSION
                                                         , pattern EPIC_view_configuration_fov_SPEC_VERSION
                                                         , EPIC_VIEW_CONFIGURATION_FOV_EXTENSION_NAME
                                                         , pattern EPIC_VIEW_CONFIGURATION_FOV_EXTENSION_NAME
                                                         ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import OpenXR.Core10.OtherTypes (Fovf)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (FromCStruct(..))
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.CStruct (ToCStruct)
import OpenXR.CStruct (ToCStruct(..))
import OpenXR.Zero (Zero(..))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_VIEW_CONFIGURATION_VIEW_FOV_EPIC))
-- | XrViewConfigurationViewFovEPIC - View Configuration Field-of-View
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrViewConfigurationViewFovEPIC-extension-notenabled# The @@
--     extension /must/ be enabled prior to using
--     'ViewConfigurationViewFovEPIC'
--
-- -   #VUID-XrViewConfigurationViewFovEPIC-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_VIEW_CONFIGURATION_VIEW_FOV_EPIC'
--
-- -   #VUID-XrViewConfigurationViewFovEPIC-next-next# @next@ /must/ be
--     @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- = See Also
--
-- 'OpenXR.Core10.OtherTypes.Fovf',
-- 'OpenXR.Core10.Enums.StructureType.StructureType'
data ViewConfigurationViewFovEPIC = ViewConfigurationViewFovEPIC
  { -- | @recommendedFov@ is the recommended field-of-view based on the current
    -- user IPD.
    recommendedFov :: Fovf
  , -- | @maxMutableFov@ is the maximum field-of-view that the runtime can
    -- display.
    maxMutableFov :: Fovf
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ViewConfigurationViewFovEPIC)
#endif
deriving instance Show ViewConfigurationViewFovEPIC

instance ToCStruct ViewConfigurationViewFovEPIC where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ViewConfigurationViewFovEPIC{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_VIEW_CONFIGURATION_VIEW_FOV_EPIC)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Fovf)) (recommendedFov)
    poke ((p `plusPtr` 32 :: Ptr Fovf)) (maxMutableFov)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_VIEW_CONFIGURATION_VIEW_FOV_EPIC)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Fovf)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Fovf)) (zero)
    f

instance FromCStruct ViewConfigurationViewFovEPIC where
  peekCStruct p = do
    recommendedFov <- peekCStruct @Fovf ((p `plusPtr` 16 :: Ptr Fovf))
    maxMutableFov <- peekCStruct @Fovf ((p `plusPtr` 32 :: Ptr Fovf))
    pure $ ViewConfigurationViewFovEPIC
             recommendedFov maxMutableFov

instance Storable ViewConfigurationViewFovEPIC where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ViewConfigurationViewFovEPIC where
  zero = ViewConfigurationViewFovEPIC
           zero
           zero


type EPIC_view_configuration_fov_SPEC_VERSION = 2

-- No documentation found for TopLevel "XR_EPIC_view_configuration_fov_SPEC_VERSION"
pattern EPIC_view_configuration_fov_SPEC_VERSION :: forall a . Integral a => a
pattern EPIC_view_configuration_fov_SPEC_VERSION = 2


type EPIC_VIEW_CONFIGURATION_FOV_EXTENSION_NAME = "XR_EPIC_view_configuration_fov"

-- No documentation found for TopLevel "XR_EPIC_VIEW_CONFIGURATION_FOV_EXTENSION_NAME"
pattern EPIC_VIEW_CONFIGURATION_FOV_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EPIC_VIEW_CONFIGURATION_FOV_EXTENSION_NAME = "XR_EPIC_view_configuration_fov"

