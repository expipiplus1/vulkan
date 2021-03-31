{-# language CPP #-}
-- | = Name
--
-- XR_KHR_composition_layer_cylinder - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_composition_layer_cylinder  XR_KHR_composition_layer_cylinder>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 18
--
-- = Revision
--
-- 4
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- = See Also
--
-- 'CompositionLayerCylinderKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_composition_layer_cylinder OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_KHR_composition_layer_cylinder  ( CompositionLayerCylinderKHR(..)
                                                            , KHR_composition_layer_cylinder_SPEC_VERSION
                                                            , pattern KHR_composition_layer_cylinder_SPEC_VERSION
                                                            , KHR_COMPOSITION_LAYER_CYLINDER_EXTENSION_NAME
                                                            , pattern KHR_COMPOSITION_LAYER_CYLINDER_EXTENSION_NAME
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
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_COMPOSITION_LAYER_CYLINDER_KHR))
-- | XrCompositionLayerCylinderKHR - Cylindrical layer composition info
--
-- == Member Descriptions
--
-- = Description
--
-- 'CompositionLayerCylinderKHR' contains the information needed to render
-- a texture onto a cylinder when calling
-- 'OpenXR.Core10.DisplayTiming.endFrame'. 'CompositionLayerCylinderKHR' is
-- an alias type for the base struct
-- 'OpenXR.Core10.OtherTypes.CompositionLayerBaseHeader' used in
-- 'OpenXR.Core10.DisplayTiming.FrameEndInfo'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrCompositionLayerCylinderKHR-extension-notenabled# The @@
--     extension /must/ be enabled prior to using
--     'CompositionLayerCylinderKHR'
--
-- -   #VUID-XrCompositionLayerCylinderKHR-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_COMPOSITION_LAYER_CYLINDER_KHR'
--
-- -   #VUID-XrCompositionLayerCylinderKHR-next-next# @next@ /must/ be
--     @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrCompositionLayerCylinderKHR-layerFlags-parameter#
--     @layerFlags@ /must/ be @0@ or a valid combination of
--     'OpenXR.Core10.Enums.CompositionLayerFlagBits.CompositionLayerFlagBits'
--     values
--
-- -   #VUID-XrCompositionLayerCylinderKHR-space-parameter# @space@ /must/
--     be a valid 'OpenXR.Core10.Handles.Space' handle
--
-- -   #VUID-XrCompositionLayerCylinderKHR-eyeVisibility-parameter#
--     @eyeVisibility@ /must/ be a valid
--     'OpenXR.Core10.Enums.EyeVisibility.EyeVisibility' value
--
-- -   #VUID-XrCompositionLayerCylinderKHR-subImage-parameter# @subImage@
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
data CompositionLayerCylinderKHR = CompositionLayerCylinderKHR
  { -- | @layerFlags@ specifies options for the layer.
    layerFlags :: CompositionLayerFlags
  , -- | @space@ is the 'OpenXR.Core10.Handles.Space' in which the @pose@ of the
    -- cylinder layer is evaluated over time.
    space :: Ptr Space_T
  , -- No documentation found for Nested "XrCompositionLayerCylinderKHR" "eyeVisibility"
    eyeVisibility :: EyeVisibility
  , -- | @subImage@ identifies the image
    -- 'OpenXR.Core10.OtherTypes.SwapchainSubImage' to use.
    subImage :: SwapchainSubImage
  , -- | @pose@ is an 'OpenXR.Core10.Space.Posef' defining the position and
    -- orientation of the center point of the view of the cylinder within the
    -- reference frame of the @space@.
    pose :: Posef
  , -- | @radius@ is the non-negative radius of the cylinder. Values of zero or
    -- floating point positive infinity are treated as an infinite cylinder.
    radius :: Float
  , -- | @centralAngle@ is the angle of the visible section of the cylinder,
    -- based at 0 radians, in the range of [0, 2π). It grows symmetrically
    -- around the 0 radian angle.
    centralAngle :: Float
  , -- | @aspectRatio@ is the ratio of the visible cylinder section width \/
    -- height. The height of the cylinder is given by: (cylinder radius ×
    -- cylinder angle) \/ aspectRatio.
    aspectRatio :: Float
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CompositionLayerCylinderKHR)
#endif
deriving instance Show CompositionLayerCylinderKHR

instance IsCompositionLayer CompositionLayerCylinderKHR where
  toCompositionLayerBaseHeader CompositionLayerCylinderKHR{..} = CompositionLayerBaseHeader{type' = TYPE_COMPOSITION_LAYER_CYLINDER_KHR, next = (), ..}

instance ToCStruct CompositionLayerCylinderKHR where
  withCStruct x f = allocaBytesAligned 112 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CompositionLayerCylinderKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_COMPOSITION_LAYER_CYLINDER_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CompositionLayerFlags)) (layerFlags)
    poke ((p `plusPtr` 24 :: Ptr (Ptr Space_T))) (space)
    poke ((p `plusPtr` 32 :: Ptr EyeVisibility)) (eyeVisibility)
    poke ((p `plusPtr` 40 :: Ptr SwapchainSubImage)) (subImage)
    poke ((p `plusPtr` 72 :: Ptr Posef)) (pose)
    poke ((p `plusPtr` 100 :: Ptr CFloat)) (CFloat (radius))
    poke ((p `plusPtr` 104 :: Ptr CFloat)) (CFloat (centralAngle))
    poke ((p `plusPtr` 108 :: Ptr CFloat)) (CFloat (aspectRatio))
    f
  cStructSize = 112
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_COMPOSITION_LAYER_CYLINDER_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr (Ptr Space_T))) (zero)
    poke ((p `plusPtr` 32 :: Ptr EyeVisibility)) (zero)
    poke ((p `plusPtr` 40 :: Ptr SwapchainSubImage)) (zero)
    poke ((p `plusPtr` 72 :: Ptr Posef)) (zero)
    poke ((p `plusPtr` 100 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 104 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 108 :: Ptr CFloat)) (CFloat (zero))
    f

instance FromCStruct CompositionLayerCylinderKHR where
  peekCStruct p = do
    layerFlags <- peek @CompositionLayerFlags ((p `plusPtr` 16 :: Ptr CompositionLayerFlags))
    space <- peek @(Ptr Space_T) ((p `plusPtr` 24 :: Ptr (Ptr Space_T)))
    eyeVisibility <- peek @EyeVisibility ((p `plusPtr` 32 :: Ptr EyeVisibility))
    subImage <- peekCStruct @SwapchainSubImage ((p `plusPtr` 40 :: Ptr SwapchainSubImage))
    pose <- peekCStruct @Posef ((p `plusPtr` 72 :: Ptr Posef))
    radius <- peek @CFloat ((p `plusPtr` 100 :: Ptr CFloat))
    centralAngle <- peek @CFloat ((p `plusPtr` 104 :: Ptr CFloat))
    aspectRatio <- peek @CFloat ((p `plusPtr` 108 :: Ptr CFloat))
    pure $ CompositionLayerCylinderKHR
             layerFlags space eyeVisibility subImage pose (coerce @CFloat @Float radius) (coerce @CFloat @Float centralAngle) (coerce @CFloat @Float aspectRatio)

instance Storable CompositionLayerCylinderKHR where
  sizeOf ~_ = 112
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CompositionLayerCylinderKHR where
  zero = CompositionLayerCylinderKHR
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


type KHR_composition_layer_cylinder_SPEC_VERSION = 4

-- No documentation found for TopLevel "XR_KHR_composition_layer_cylinder_SPEC_VERSION"
pattern KHR_composition_layer_cylinder_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_composition_layer_cylinder_SPEC_VERSION = 4


type KHR_COMPOSITION_LAYER_CYLINDER_EXTENSION_NAME = "XR_KHR_composition_layer_cylinder"

-- No documentation found for TopLevel "XR_KHR_COMPOSITION_LAYER_CYLINDER_EXTENSION_NAME"
pattern KHR_COMPOSITION_LAYER_CYLINDER_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_COMPOSITION_LAYER_CYLINDER_EXTENSION_NAME = "XR_KHR_composition_layer_cylinder"

