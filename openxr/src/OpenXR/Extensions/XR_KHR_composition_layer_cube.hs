{-# language CPP #-}
-- | = Name
--
-- XR_KHR_composition_layer_cube - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_composition_layer_cube  XR_KHR_composition_layer_cube>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 7
--
-- = Revision
--
-- 8
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- = See Also
--
-- 'CompositionLayerCubeKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_composition_layer_cube OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_KHR_composition_layer_cube  ( CompositionLayerCubeKHR(..)
                                                        , KHR_composition_layer_cube_SPEC_VERSION
                                                        , pattern KHR_composition_layer_cube_SPEC_VERSION
                                                        , KHR_COMPOSITION_LAYER_CUBE_EXTENSION_NAME
                                                        , pattern KHR_COMPOSITION_LAYER_CUBE_EXTENSION_NAME
                                                        ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (FromCStruct(..))
import OpenXR.CStruct (ToCStruct)
import OpenXR.CStruct (ToCStruct(..))
import OpenXR.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import OpenXR.Core10.OtherTypes (CompositionLayerBaseHeader(..))
import OpenXR.Core10.Enums.CompositionLayerFlagBits (CompositionLayerFlags)
import OpenXR.Core10.Enums.EyeVisibility (EyeVisibility)
import OpenXR.Core10.OtherTypes (IsCompositionLayer(..))
import OpenXR.Core10.Space (Quaternionf)
import OpenXR.Core10.Handles (Space_T)
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.Core10.Handles (Swapchain_T)
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_COMPOSITION_LAYER_CUBE_KHR))
-- | XrCompositionLayerCubeKHR - Cube map layer composition info
--
-- == Member Descriptions
--
-- = Description
--
-- 'CompositionLayerCubeKHR' contains the information needed to render a
-- cube map when calling 'OpenXR.Core10.DisplayTiming.endFrame'.
-- 'CompositionLayerCubeKHR' is an alias type for the base struct
-- 'OpenXR.Core10.OtherTypes.CompositionLayerBaseHeader' used in
-- 'OpenXR.Core10.DisplayTiming.FrameEndInfo'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrCompositionLayerCubeKHR-extension-notenabled# The @@
--     extension /must/ be enabled prior to using 'CompositionLayerCubeKHR'
--
-- -   #VUID-XrCompositionLayerCubeKHR-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_COMPOSITION_LAYER_CUBE_KHR'
--
-- -   #VUID-XrCompositionLayerCubeKHR-next-next# @next@ /must/ be @NULL@
--     or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrCompositionLayerCubeKHR-layerFlags-parameter# @layerFlags@
--     /must/ be @0@ or a valid combination of
--     'OpenXR.Core10.Enums.CompositionLayerFlagBits.CompositionLayerFlagBits'
--     values
--
-- -   #VUID-XrCompositionLayerCubeKHR-space-parameter# @space@ /must/ be a
--     valid 'OpenXR.Core10.Handles.Space' handle
--
-- -   #VUID-XrCompositionLayerCubeKHR-eyeVisibility-parameter#
--     @eyeVisibility@ /must/ be a valid
--     'OpenXR.Core10.Enums.EyeVisibility.EyeVisibility' value
--
-- -   #VUID-XrCompositionLayerCubeKHR-swapchain-parameter# @swapchain@
--     /must/ be a valid 'OpenXR.Core10.Handles.Swapchain' handle
--
-- -   #VUID-XrCompositionLayerCubeKHR-commonparent# Both of @space@ and
--     @swapchain@ /must/ have been created, allocated, or retrieved from
--     the same 'OpenXR.Core10.Handles.Session'
--
-- = See Also
--
-- 'OpenXR.Core10.OtherTypes.CompositionLayerBaseHeader',
-- 'OpenXR.Core10.Enums.CompositionLayerFlagBits.CompositionLayerFlags',
-- 'OpenXR.Core10.Enums.EyeVisibility.EyeVisibility',
-- 'OpenXR.Core10.DisplayTiming.FrameEndInfo',
-- 'OpenXR.Core10.Space.Quaternionf', 'OpenXR.Core10.Handles.Space',
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'OpenXR.Core10.Handles.Swapchain',
-- 'OpenXR.Core10.DisplayTiming.endFrame'
data CompositionLayerCubeKHR = CompositionLayerCubeKHR
  { -- | @layerFlags@ is any flags to apply to this layer.
    layerFlags :: CompositionLayerFlags
  , -- | @space@ is the 'OpenXR.Core10.Handles.Space' in which the @orientation@
    -- of the cube layer is evaluated over time.
    space :: Ptr Space_T
  , -- No documentation found for Nested "XrCompositionLayerCubeKHR" "eyeVisibility"
    eyeVisibility :: EyeVisibility
  , -- | @swapchain@ is the swapchain.
    swapchain :: Ptr Swapchain_T
  , -- | @imageArrayIndex@ is the image array index, with 0 meaning the first or
    -- only array element.
    imageArrayIndex :: Word32
  , -- | @orientation@ is the orientation of the environment map in the @space@.
    orientation :: Quaternionf
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CompositionLayerCubeKHR)
#endif
deriving instance Show CompositionLayerCubeKHR

instance IsCompositionLayer CompositionLayerCubeKHR where
  toCompositionLayerBaseHeader CompositionLayerCubeKHR{..} = CompositionLayerBaseHeader{type' = TYPE_COMPOSITION_LAYER_CUBE_KHR, next = (), ..}

instance ToCStruct CompositionLayerCubeKHR where
  withCStruct x f = allocaBytesAligned 72 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CompositionLayerCubeKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_COMPOSITION_LAYER_CUBE_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CompositionLayerFlags)) (layerFlags)
    poke ((p `plusPtr` 24 :: Ptr (Ptr Space_T))) (space)
    poke ((p `plusPtr` 32 :: Ptr EyeVisibility)) (eyeVisibility)
    poke ((p `plusPtr` 40 :: Ptr (Ptr Swapchain_T))) (swapchain)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (imageArrayIndex)
    poke ((p `plusPtr` 52 :: Ptr Quaternionf)) (orientation)
    f
  cStructSize = 72
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_COMPOSITION_LAYER_CUBE_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr (Ptr Space_T))) (zero)
    poke ((p `plusPtr` 32 :: Ptr EyeVisibility)) (zero)
    poke ((p `plusPtr` 40 :: Ptr (Ptr Swapchain_T))) (zero)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 52 :: Ptr Quaternionf)) (zero)
    f

instance FromCStruct CompositionLayerCubeKHR where
  peekCStruct p = do
    layerFlags <- peek @CompositionLayerFlags ((p `plusPtr` 16 :: Ptr CompositionLayerFlags))
    space <- peek @(Ptr Space_T) ((p `plusPtr` 24 :: Ptr (Ptr Space_T)))
    eyeVisibility <- peek @EyeVisibility ((p `plusPtr` 32 :: Ptr EyeVisibility))
    swapchain <- peek @(Ptr Swapchain_T) ((p `plusPtr` 40 :: Ptr (Ptr Swapchain_T)))
    imageArrayIndex <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    orientation <- peekCStruct @Quaternionf ((p `plusPtr` 52 :: Ptr Quaternionf))
    pure $ CompositionLayerCubeKHR
             layerFlags space eyeVisibility swapchain imageArrayIndex orientation

instance Storable CompositionLayerCubeKHR where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CompositionLayerCubeKHR where
  zero = CompositionLayerCubeKHR
           zero
           zero
           zero
           zero
           zero
           zero


type KHR_composition_layer_cube_SPEC_VERSION = 8

-- No documentation found for TopLevel "XR_KHR_composition_layer_cube_SPEC_VERSION"
pattern KHR_composition_layer_cube_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_composition_layer_cube_SPEC_VERSION = 8


type KHR_COMPOSITION_LAYER_CUBE_EXTENSION_NAME = "XR_KHR_composition_layer_cube"

-- No documentation found for TopLevel "XR_KHR_COMPOSITION_LAYER_CUBE_EXTENSION_NAME"
pattern KHR_COMPOSITION_LAYER_CUBE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_COMPOSITION_LAYER_CUBE_EXTENSION_NAME = "XR_KHR_composition_layer_cube"

