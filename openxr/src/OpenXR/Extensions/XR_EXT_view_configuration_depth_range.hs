{-# language CPP #-}
-- | = Name
--
-- XR_EXT_view_configuration_depth_range - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_EXT_view_configuration_depth_range  XR_EXT_view_configuration_depth_range>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 47
--
-- = Revision
--
-- 1
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- = See Also
--
-- 'ViewConfigurationDepthRangeEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_EXT_view_configuration_depth_range OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_EXT_view_configuration_depth_range  ( ViewConfigurationDepthRangeEXT(..)
                                                                , EXT_view_configuration_depth_range_SPEC_VERSION
                                                                , pattern EXT_view_configuration_depth_range_SPEC_VERSION
                                                                , EXT_VIEW_CONFIGURATION_DEPTH_RANGE_EXTENSION_NAME
                                                                , pattern EXT_VIEW_CONFIGURATION_DEPTH_RANGE_EXTENSION_NAME
                                                                ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.Coerce (coerce)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (FromCStruct(..))
import OpenXR.CStruct (ToCStruct)
import OpenXR.CStruct (ToCStruct(..))
import OpenXR.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(..))
import Foreign.C.Types (CFloat(CFloat))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_VIEW_CONFIGURATION_DEPTH_RANGE_EXT))
-- | XrViewConfigurationDepthRangeEXT - View configuration depth range
-- information
--
-- == Member Descriptions
--
-- = Description
--
-- When enumerating the view configurations with
-- 'OpenXR.Core10.ViewConfigurations.enumerateViewConfigurationViews', the
-- application /can/ provide a pointer to an
-- 'ViewConfigurationDepthRangeEXT' in the @next@ chain of
-- 'OpenXR.Core10.ViewConfigurations.ViewConfigurationView'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrViewConfigurationDepthRangeEXT-extension-notenabled# The
--     @XR_EXT_view_configuration_depth_range@ extension /must/ be enabled
--     prior to using 'ViewConfigurationDepthRangeEXT'
--
-- -   #VUID-XrViewConfigurationDepthRangeEXT-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_VIEW_CONFIGURATION_DEPTH_RANGE_EXT'
--
-- -   #VUID-XrViewConfigurationDepthRangeEXT-next-next# @next@ /must/ be
--     @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'OpenXR.Core10.ViewConfigurations.ViewConfigurationView',
-- 'OpenXR.Core10.ViewConfigurations.enumerateViewConfigurationViews'
data ViewConfigurationDepthRangeEXT = ViewConfigurationDepthRangeEXT
  { -- | @recommendedNearZ@ is the recommended minimum positive distance in
    -- meters that content should be rendered for the view to achieve the best
    -- user experience.
    recommendedNearZ :: Float
  , -- | @minNearZ@ is the absolute minimum positive distance in meters that
    -- content should be rendered for the view.
    minNearZ :: Float
  , -- | @recommendedFarZ@ is the recommended maximum positive distance in meters
    -- that content should be rendered for the view to achieve the best user
    -- experience.
    recommendedFarZ :: Float
  , -- | @maxFarZ@ is the absolute maximum positive distance in meters that
    -- content should be rendered for the view.
    maxFarZ :: Float
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ViewConfigurationDepthRangeEXT)
#endif
deriving instance Show ViewConfigurationDepthRangeEXT

instance ToCStruct ViewConfigurationDepthRangeEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ViewConfigurationDepthRangeEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_VIEW_CONFIGURATION_DEPTH_RANGE_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CFloat)) (CFloat (recommendedNearZ))
    poke ((p `plusPtr` 20 :: Ptr CFloat)) (CFloat (minNearZ))
    poke ((p `plusPtr` 24 :: Ptr CFloat)) (CFloat (recommendedFarZ))
    poke ((p `plusPtr` 28 :: Ptr CFloat)) (CFloat (maxFarZ))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_VIEW_CONFIGURATION_DEPTH_RANGE_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 20 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 24 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 28 :: Ptr CFloat)) (CFloat (zero))
    f

instance FromCStruct ViewConfigurationDepthRangeEXT where
  peekCStruct p = do
    recommendedNearZ <- peek @CFloat ((p `plusPtr` 16 :: Ptr CFloat))
    minNearZ <- peek @CFloat ((p `plusPtr` 20 :: Ptr CFloat))
    recommendedFarZ <- peek @CFloat ((p `plusPtr` 24 :: Ptr CFloat))
    maxFarZ <- peek @CFloat ((p `plusPtr` 28 :: Ptr CFloat))
    pure $ ViewConfigurationDepthRangeEXT
             (coerce @CFloat @Float recommendedNearZ)
             (coerce @CFloat @Float minNearZ)
             (coerce @CFloat @Float recommendedFarZ)
             (coerce @CFloat @Float maxFarZ)

instance Storable ViewConfigurationDepthRangeEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ViewConfigurationDepthRangeEXT where
  zero = ViewConfigurationDepthRangeEXT
           zero
           zero
           zero
           zero


type EXT_view_configuration_depth_range_SPEC_VERSION = 1

-- No documentation found for TopLevel "XR_EXT_view_configuration_depth_range_SPEC_VERSION"
pattern EXT_view_configuration_depth_range_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_view_configuration_depth_range_SPEC_VERSION = 1


type EXT_VIEW_CONFIGURATION_DEPTH_RANGE_EXTENSION_NAME = "XR_EXT_view_configuration_depth_range"

-- No documentation found for TopLevel "XR_EXT_VIEW_CONFIGURATION_DEPTH_RANGE_EXTENSION_NAME"
pattern EXT_VIEW_CONFIGURATION_DEPTH_RANGE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_VIEW_CONFIGURATION_DEPTH_RANGE_EXTENSION_NAME = "XR_EXT_view_configuration_depth_range"

