{-# language CPP #-}
-- | = Name
--
-- XR_KHR_composition_layer_equirect2 - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_composition_layer_equirect2  XR_KHR_composition_layer_equirect2>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 92
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
-- 'CompositionLayerEquirect2KHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_composition_layer_equirect2 OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_KHR_composition_layer_equirect2  ( CompositionLayerEquirect2KHR(..)
                                                             , KHR_composition_layer_equirect2_SPEC_VERSION
                                                             , pattern KHR_composition_layer_equirect2_SPEC_VERSION
                                                             , KHR_COMPOSITION_LAYER_EQUIRECT2_EXTENSION_NAME
                                                             , pattern KHR_COMPOSITION_LAYER_EQUIRECT2_EXTENSION_NAME
                                                             ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
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
import OpenXR.Core10.OtherTypes (CompositionLayerBaseHeader(..))
import OpenXR.Core10.Enums.CompositionLayerFlagBits (CompositionLayerFlags)
import OpenXR.Core10.Enums.EyeVisibility (EyeVisibility)
import OpenXR.Core10.OtherTypes (IsCompositionLayer(..))
import OpenXR.Core10.Space (Posef)
import OpenXR.Core10.Handles (Space_T)
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.Core10.OtherTypes (SwapchainSubImage)
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_COMPOSITION_LAYER_EQUIRECT2_KHR))
-- | XrCompositionLayerEquirect2KHR - Equirectangular layer composition info
--
-- == Member Descriptions
--
-- = Description
--
-- 'CompositionLayerEquirect2KHR' contains the information needed to render
-- an equirectangular image onto a sphere when calling
-- 'OpenXR.Core10.DisplayTiming.endFrame'. 'CompositionLayerEquirect2KHR'
-- is an alias type for the base struct
-- 'OpenXR.Core10.OtherTypes.CompositionLayerBaseHeader' used in
-- 'OpenXR.Core10.DisplayTiming.FrameEndInfo'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrCompositionLayerEquirect2KHR-extension-notenabled# The @@
--     extension /must/ be enabled prior to using
--     'CompositionLayerEquirect2KHR'
--
-- -   #VUID-XrCompositionLayerEquirect2KHR-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_COMPOSITION_LAYER_EQUIRECT2_KHR'
--
-- -   #VUID-XrCompositionLayerEquirect2KHR-next-next# @next@ /must/ be
--     @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrCompositionLayerEquirect2KHR-layerFlags-parameter#
--     @layerFlags@ /must/ be @0@ or a valid combination of
--     'OpenXR.Core10.Enums.CompositionLayerFlagBits.CompositionLayerFlagBits'
--     values
--
-- -   #VUID-XrCompositionLayerEquirect2KHR-space-parameter# @space@ /must/
--     be a valid 'OpenXR.Core10.Handles.Space' handle
--
-- -   #VUID-XrCompositionLayerEquirect2KHR-eyeVisibility-parameter#
--     @eyeVisibility@ /must/ be a valid
--     'OpenXR.Core10.Enums.EyeVisibility.EyeVisibility' value
--
-- -   #VUID-XrCompositionLayerEquirect2KHR-subImage-parameter# @subImage@
--     /must/ be a valid 'OpenXR.Core10.OtherTypes.SwapchainSubImage'
--     structure
--
-- = See Also
--
-- 'OpenXR.Core10.OtherTypes.CompositionLayerBaseHeader',
-- 'OpenXR.Core10.Enums.CompositionLayerFlagBits.CompositionLayerFlags',
-- 'OpenXR.Core10.Enums.EyeVisibility.EyeVisibility',
-- 'OpenXR.Core10.DisplayTiming.FrameEndInfo', 'OpenXR.Core10.Space.Posef',
-- 'OpenXR.Core10.Handles.Space',
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'OpenXR.Core10.OtherTypes.SwapchainSubImage',
-- 'OpenXR.Core10.DisplayTiming.endFrame'
data CompositionLayerEquirect2KHR = CompositionLayerEquirect2KHR
  { -- | @layerFlags@ specifies options for the layer.
    layerFlags :: CompositionLayerFlags
  , -- | @space@ is the 'OpenXR.Core10.Handles.Space' in which the @pose@ of the
    -- equirect layer is evaluated over time.
    space :: Ptr Space_T
  , -- No documentation found for Nested "XrCompositionLayerEquirect2KHR" "eyeVisibility"
    eyeVisibility :: EyeVisibility
  , -- | @subImage@ identifies the image
    -- 'OpenXR.Core10.OtherTypes.SwapchainSubImage' to use.
    subImage :: SwapchainSubImage
  , -- | @pose@ is an 'OpenXR.Core10.Space.Posef' defining the position and
    -- orientation of the center point of the sphere onto which the equirect
    -- image data is mapped, relative to the reference frame of the @space@.
    pose :: Posef
  , -- | @radius@ is the non-negative radius of the sphere onto which the
    -- equirect image data is mapped. Values of zero or floating point positive
    -- infinity are treated as an infinite sphere.
    radius :: Float
  , -- | @centralHorizontalAngle@ defines the visible horizontal angle of the
    -- sphere, based at 0 radians, in the range of [0, 2π]. It grows
    -- symmetrically around the 0 radian angle.
    centralHorizontalAngle :: Float
  , -- | @upperVerticalAngle@ defines the upper vertical angle of the visible
    -- portion of the sphere, in the range of [-π\/2, π\/2].
    upperVerticalAngle :: Float
  , -- | @lowerVerticalAngle@ defines the lower vertical angle of the visible
    -- portion of the sphere, in the range of [-π\/2, π\/2].
    lowerVerticalAngle :: Float
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CompositionLayerEquirect2KHR)
#endif
deriving instance Show CompositionLayerEquirect2KHR

instance IsCompositionLayer CompositionLayerEquirect2KHR where
  toCompositionLayerBaseHeader CompositionLayerEquirect2KHR{..} = CompositionLayerBaseHeader{type' = TYPE_COMPOSITION_LAYER_EQUIRECT2_KHR, next = (), ..}

instance ToCStruct CompositionLayerEquirect2KHR where
  withCStruct x f = allocaBytesAligned 120 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CompositionLayerEquirect2KHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_COMPOSITION_LAYER_EQUIRECT2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CompositionLayerFlags)) (layerFlags)
    poke ((p `plusPtr` 24 :: Ptr (Ptr Space_T))) (space)
    poke ((p `plusPtr` 32 :: Ptr EyeVisibility)) (eyeVisibility)
    poke ((p `plusPtr` 40 :: Ptr SwapchainSubImage)) (subImage)
    poke ((p `plusPtr` 72 :: Ptr Posef)) (pose)
    poke ((p `plusPtr` 100 :: Ptr CFloat)) (CFloat (radius))
    poke ((p `plusPtr` 104 :: Ptr CFloat)) (CFloat (centralHorizontalAngle))
    poke ((p `plusPtr` 108 :: Ptr CFloat)) (CFloat (upperVerticalAngle))
    poke ((p `plusPtr` 112 :: Ptr CFloat)) (CFloat (lowerVerticalAngle))
    f
  cStructSize = 120
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_COMPOSITION_LAYER_EQUIRECT2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr (Ptr Space_T))) (zero)
    poke ((p `plusPtr` 32 :: Ptr EyeVisibility)) (zero)
    poke ((p `plusPtr` 40 :: Ptr SwapchainSubImage)) (zero)
    poke ((p `plusPtr` 72 :: Ptr Posef)) (zero)
    poke ((p `plusPtr` 100 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 104 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 108 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 112 :: Ptr CFloat)) (CFloat (zero))
    f

instance FromCStruct CompositionLayerEquirect2KHR where
  peekCStruct p = do
    layerFlags <- peek @CompositionLayerFlags ((p `plusPtr` 16 :: Ptr CompositionLayerFlags))
    space <- peek @(Ptr Space_T) ((p `plusPtr` 24 :: Ptr (Ptr Space_T)))
    eyeVisibility <- peek @EyeVisibility ((p `plusPtr` 32 :: Ptr EyeVisibility))
    subImage <- peekCStruct @SwapchainSubImage ((p `plusPtr` 40 :: Ptr SwapchainSubImage))
    pose <- peekCStruct @Posef ((p `plusPtr` 72 :: Ptr Posef))
    radius <- peek @CFloat ((p `plusPtr` 100 :: Ptr CFloat))
    centralHorizontalAngle <- peek @CFloat ((p `plusPtr` 104 :: Ptr CFloat))
    upperVerticalAngle <- peek @CFloat ((p `plusPtr` 108 :: Ptr CFloat))
    lowerVerticalAngle <- peek @CFloat ((p `plusPtr` 112 :: Ptr CFloat))
    pure $ CompositionLayerEquirect2KHR
             layerFlags space eyeVisibility subImage pose (coerce @CFloat @Float radius) (coerce @CFloat @Float centralHorizontalAngle) (coerce @CFloat @Float upperVerticalAngle) (coerce @CFloat @Float lowerVerticalAngle)

instance Storable CompositionLayerEquirect2KHR where
  sizeOf ~_ = 120
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CompositionLayerEquirect2KHR where
  zero = CompositionLayerEquirect2KHR
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


type KHR_composition_layer_equirect2_SPEC_VERSION = 1

-- No documentation found for TopLevel "XR_KHR_composition_layer_equirect2_SPEC_VERSION"
pattern KHR_composition_layer_equirect2_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_composition_layer_equirect2_SPEC_VERSION = 1


type KHR_COMPOSITION_LAYER_EQUIRECT2_EXTENSION_NAME = "XR_KHR_composition_layer_equirect2"

-- No documentation found for TopLevel "XR_KHR_COMPOSITION_LAYER_EQUIRECT2_EXTENSION_NAME"
pattern KHR_COMPOSITION_LAYER_EQUIRECT2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_COMPOSITION_LAYER_EQUIRECT2_EXTENSION_NAME = "XR_KHR_composition_layer_equirect2"

