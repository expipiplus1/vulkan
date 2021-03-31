{-# language CPP #-}
-- | = Name
--
-- XR_KHR_composition_layer_equirect - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_composition_layer_equirect  XR_KHR_composition_layer_equirect>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 19
--
-- = Revision
--
-- 3
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- = See Also
--
-- 'CompositionLayerEquirectKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_composition_layer_equirect OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_KHR_composition_layer_equirect  ( CompositionLayerEquirectKHR(..)
                                                            , KHR_composition_layer_equirect_SPEC_VERSION
                                                            , pattern KHR_composition_layer_equirect_SPEC_VERSION
                                                            , KHR_COMPOSITION_LAYER_EQUIRECT_EXTENSION_NAME
                                                            , pattern KHR_COMPOSITION_LAYER_EQUIRECT_EXTENSION_NAME
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
import OpenXR.Core10.Input (Vector2f)
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_COMPOSITION_LAYER_EQUIRECT_KHR))
-- | XrCompositionLayerEquirectKHR - Equirectangular layer composition info
--
-- == Member Descriptions
--
-- = Description
--
-- 'CompositionLayerEquirectKHR' contains the information needed to render
-- an equirectangular image onto a sphere when calling
-- 'OpenXR.Core10.DisplayTiming.endFrame'. 'CompositionLayerEquirectKHR' is
-- an alias type for the base struct
-- 'OpenXR.Core10.OtherTypes.CompositionLayerBaseHeader' used in
-- 'OpenXR.Core10.DisplayTiming.FrameEndInfo'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrCompositionLayerEquirectKHR-extension-notenabled# The @@
--     extension /must/ be enabled prior to using
--     'CompositionLayerEquirectKHR'
--
-- -   #VUID-XrCompositionLayerEquirectKHR-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_COMPOSITION_LAYER_EQUIRECT_KHR'
--
-- -   #VUID-XrCompositionLayerEquirectKHR-next-next# @next@ /must/ be
--     @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrCompositionLayerEquirectKHR-layerFlags-parameter#
--     @layerFlags@ /must/ be @0@ or a valid combination of
--     'OpenXR.Core10.Enums.CompositionLayerFlagBits.CompositionLayerFlagBits'
--     values
--
-- -   #VUID-XrCompositionLayerEquirectKHR-space-parameter# @space@ /must/
--     be a valid 'OpenXR.Core10.Handles.Space' handle
--
-- -   #VUID-XrCompositionLayerEquirectKHR-eyeVisibility-parameter#
--     @eyeVisibility@ /must/ be a valid
--     'OpenXR.Core10.Enums.EyeVisibility.EyeVisibility' value
--
-- -   #VUID-XrCompositionLayerEquirectKHR-subImage-parameter# @subImage@
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
-- 'OpenXR.Core10.Input.Vector2f', 'OpenXR.Core10.DisplayTiming.endFrame'
data CompositionLayerEquirectKHR = CompositionLayerEquirectKHR
  { -- | @layerFlags@ specifies options for the layer.
    layerFlags :: CompositionLayerFlags
  , -- | @space@ is the 'OpenXR.Core10.Handles.Space' in which the @pose@ of the
    -- equirect layer is evaluated over time.
    space :: Ptr Space_T
  , -- No documentation found for Nested "XrCompositionLayerEquirectKHR" "eyeVisibility"
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
  , -- | @scale@ is an 'OpenXR.Core10.Input.Vector2f' indicating a scale of the
    -- texture coordinates after the mapping to 2D.
    scale :: Vector2f
  , -- | @bias@ is an 'OpenXR.Core10.Input.Vector2f' indicating a bias of the
    -- texture coordinates after the mapping to 2D.
    bias :: Vector2f
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CompositionLayerEquirectKHR)
#endif
deriving instance Show CompositionLayerEquirectKHR

instance IsCompositionLayer CompositionLayerEquirectKHR where
  toCompositionLayerBaseHeader CompositionLayerEquirectKHR{..} = CompositionLayerBaseHeader{type' = TYPE_COMPOSITION_LAYER_EQUIRECT_KHR, next = (), ..}

instance ToCStruct CompositionLayerEquirectKHR where
  withCStruct x f = allocaBytesAligned 120 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CompositionLayerEquirectKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_COMPOSITION_LAYER_EQUIRECT_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CompositionLayerFlags)) (layerFlags)
    poke ((p `plusPtr` 24 :: Ptr (Ptr Space_T))) (space)
    poke ((p `plusPtr` 32 :: Ptr EyeVisibility)) (eyeVisibility)
    poke ((p `plusPtr` 40 :: Ptr SwapchainSubImage)) (subImage)
    poke ((p `plusPtr` 72 :: Ptr Posef)) (pose)
    poke ((p `plusPtr` 100 :: Ptr CFloat)) (CFloat (radius))
    poke ((p `plusPtr` 104 :: Ptr Vector2f)) (scale)
    poke ((p `plusPtr` 112 :: Ptr Vector2f)) (bias)
    f
  cStructSize = 120
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_COMPOSITION_LAYER_EQUIRECT_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr (Ptr Space_T))) (zero)
    poke ((p `plusPtr` 32 :: Ptr EyeVisibility)) (zero)
    poke ((p `plusPtr` 40 :: Ptr SwapchainSubImage)) (zero)
    poke ((p `plusPtr` 72 :: Ptr Posef)) (zero)
    poke ((p `plusPtr` 100 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 104 :: Ptr Vector2f)) (zero)
    poke ((p `plusPtr` 112 :: Ptr Vector2f)) (zero)
    f

instance FromCStruct CompositionLayerEquirectKHR where
  peekCStruct p = do
    layerFlags <- peek @CompositionLayerFlags ((p `plusPtr` 16 :: Ptr CompositionLayerFlags))
    space <- peek @(Ptr Space_T) ((p `plusPtr` 24 :: Ptr (Ptr Space_T)))
    eyeVisibility <- peek @EyeVisibility ((p `plusPtr` 32 :: Ptr EyeVisibility))
    subImage <- peekCStruct @SwapchainSubImage ((p `plusPtr` 40 :: Ptr SwapchainSubImage))
    pose <- peekCStruct @Posef ((p `plusPtr` 72 :: Ptr Posef))
    radius <- peek @CFloat ((p `plusPtr` 100 :: Ptr CFloat))
    scale <- peekCStruct @Vector2f ((p `plusPtr` 104 :: Ptr Vector2f))
    bias <- peekCStruct @Vector2f ((p `plusPtr` 112 :: Ptr Vector2f))
    pure $ CompositionLayerEquirectKHR
             layerFlags space eyeVisibility subImage pose (coerce @CFloat @Float radius) scale bias

instance Storable CompositionLayerEquirectKHR where
  sizeOf ~_ = 120
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CompositionLayerEquirectKHR where
  zero = CompositionLayerEquirectKHR
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


type KHR_composition_layer_equirect_SPEC_VERSION = 3

-- No documentation found for TopLevel "XR_KHR_composition_layer_equirect_SPEC_VERSION"
pattern KHR_composition_layer_equirect_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_composition_layer_equirect_SPEC_VERSION = 3


type KHR_COMPOSITION_LAYER_EQUIRECT_EXTENSION_NAME = "XR_KHR_composition_layer_equirect"

-- No documentation found for TopLevel "XR_KHR_COMPOSITION_LAYER_EQUIRECT_EXTENSION_NAME"
pattern KHR_COMPOSITION_LAYER_EQUIRECT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_COMPOSITION_LAYER_EQUIRECT_EXTENSION_NAME = "XR_KHR_composition_layer_equirect"

