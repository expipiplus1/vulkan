{-# language CPP #-}
-- No documentation found for Chapter "OtherTypes"
module OpenXR.Core10.OtherTypes  ( Vector4f(..)
                                 , Color4f(..)
                                 , Fovf(..)
                                 , SwapchainSubImage(..)
                                 , CompositionLayerBaseHeader(..)
                                 , IsCompositionLayer(..)
                                 , CompositionLayerProjectionView(..)
                                 , CompositionLayerProjection(..)
                                 , CompositionLayerQuad(..)
                                 , HapticVibration(..)
                                 , EventDataBaseHeader(..)
                                 , IsEventData(..)
                                 , EventDataEventsLost(..)
                                 , EventDataInstanceLossPending(..)
                                 , EventDataSessionStateChanged(..)
                                 , EventDataReferenceSpaceChangePending(..)
                                 , EventDataInteractionProfileChanged(..)
                                 , Offset2Df(..)
                                 , Extent2Df(..)
                                 , Rect2Df(..)
                                 , BaseInStructure(..)
                                 , BaseOutStructure(..)
                                 , ObjectType(..)
                                 ) where

import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.Coerce (coerce)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (FromCStruct(..))
import OpenXR.CStruct (ToCStruct)
import OpenXR.CStruct (ToCStruct(..))
import OpenXR.Zero (Zero(..))
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(..))
import Foreign.C.Types (CFloat(CFloat))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import OpenXR.CStruct.Utils (advancePtrBytes)
import OpenXR.Core10.FundamentalTypes (bool32ToBool)
import OpenXR.Core10.FundamentalTypes (boolToBool32)
import OpenXR.CStruct.Extends (forgetExtensions)
import OpenXR.CStruct.Extends (peekSomeCStruct)
import OpenXR.CStruct.Extends (pokeSomeCStruct)
import OpenXR.Core10.FundamentalTypes (Bool32)
import OpenXR.CStruct.Extends (Chain)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_composition_layer_color_scale_bias (CompositionLayerColorScaleBiasKHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_composition_layer_cube (CompositionLayerCubeKHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_composition_layer_cylinder (CompositionLayerCylinderKHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_composition_layer_depth (CompositionLayerDepthInfoKHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_composition_layer_equirect2 (CompositionLayerEquirect2KHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_composition_layer_equirect (CompositionLayerEquirectKHR)
import OpenXR.Core10.Enums.CompositionLayerFlagBits (CompositionLayerFlags)
import OpenXR.Core10.FundamentalTypes (Duration)
import {-# SOURCE #-} OpenXR.Extensions.XR_FB_display_refresh_rate (EventDataDisplayRefreshRateChangedFB)
import {-# SOURCE #-} OpenXR.Extensions.XR_EXTX_overlay (EventDataMainSessionVisibilityChangedEXTX)
import {-# SOURCE #-} OpenXR.Extensions.XR_EXT_performance_settings (EventDataPerfSettingsEXT)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_visibility_mask (EventDataVisibilityMaskChangedKHR)
import OpenXR.CStruct.Extends (Extends)
import OpenXR.CStruct.Extends (Extendss)
import OpenXR.CStruct.Extends (Extensible(..))
import OpenXR.Core10.FundamentalTypes (Extent2Df)
import OpenXR.Core10.Enums.EyeVisibility (EyeVisibility)
import OpenXR.Core10.Haptics (HapticBaseHeader(..))
import OpenXR.CStruct.Extends (Inheritable(..))
import OpenXR.Core10.Haptics (IsHaptic(..))
import OpenXR.CStruct.Extends (PeekChain)
import OpenXR.CStruct.Extends (PeekChain(..))
import OpenXR.CStruct.Extends (PokeChain)
import OpenXR.CStruct.Extends (PokeChain(..))
import OpenXR.Core10.Space (Posef)
import OpenXR.Core10.FundamentalTypes (Rect2Di)
import OpenXR.Core10.Enums.ReferenceSpaceType (ReferenceSpaceType)
import OpenXR.Core10.Enums.SessionState (SessionState)
import OpenXR.Core10.Handles (Session_T)
import OpenXR.CStruct.Extends (SomeChild(..))
import OpenXR.CStruct.Extends (SomeStruct)
import OpenXR.Core10.Handles (Space_T)
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.Core10.Handles (Swapchain_T)
import OpenXR.Core10.FundamentalTypes (Time)
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_COMPOSITION_LAYER_CUBE_KHR))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_COMPOSITION_LAYER_CYLINDER_KHR))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_COMPOSITION_LAYER_EQUIRECT2_KHR))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_COMPOSITION_LAYER_EQUIRECT_KHR))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_COMPOSITION_LAYER_PROJECTION))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_COMPOSITION_LAYER_PROJECTION_VIEW))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_COMPOSITION_LAYER_QUAD))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_EVENT_DATA_DISPLAY_REFRESH_RATE_CHANGED_FB))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_EVENT_DATA_EVENTS_LOST))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_EVENT_DATA_INSTANCE_LOSS_PENDING))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_EVENT_DATA_INTERACTION_PROFILE_CHANGED))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_EVENT_DATA_MAIN_SESSION_VISIBILITY_CHANGED_EXTX))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_EVENT_DATA_PERF_SETTINGS_EXT))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_EVENT_DATA_REFERENCE_SPACE_CHANGE_PENDING))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_EVENT_DATA_SESSION_STATE_CHANGED))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_EVENT_DATA_VISIBILITY_MASK_CHANGED_KHR))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_HAPTIC_VIBRATION))
import OpenXR.CStruct.Extends (BaseInStructure(..))
import OpenXR.CStruct.Extends (BaseOutStructure(..))
import OpenXR.Core10.FundamentalTypes (Extent2Df(..))
import OpenXR.Core10.Enums.ObjectType (ObjectType(..))
import OpenXR.Core10.FundamentalTypes (Offset2Df(..))
import OpenXR.Core10.FundamentalTypes (Rect2Df(..))
-- | XrVector4f - Four-dimensional vector
--
-- == Member Descriptions
--
-- = Description
--
-- If used to represent physical distances, @x@, @y@, and @z@ values /must/
-- be in meters.
--
-- = See Also
--
-- 'OpenXR.Core10.Space.Posef', 'OpenXR.Core10.Space.Quaternionf',
-- 'OpenXR.Core10.Input.Vector2f', 'OpenXR.Core10.Space.Vector3f'
data Vector4f = Vector4f
  { -- | @x@ is the x coordinate of the vector.
    x :: Float
  , -- | @y@ is the y coordinate of the vector.
    y :: Float
  , -- | @z@ is the z coordinate of the vector.
    z :: Float
  , -- | @w@ is the w coordinate of the vector.
    w :: Float
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (Vector4f)
#endif
deriving instance Show Vector4f

instance ToCStruct Vector4f where
  withCStruct x f = allocaBytesAligned 16 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p Vector4f{..} f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (x))
    poke ((p `plusPtr` 4 :: Ptr CFloat)) (CFloat (y))
    poke ((p `plusPtr` 8 :: Ptr CFloat)) (CFloat (z))
    poke ((p `plusPtr` 12 :: Ptr CFloat)) (CFloat (w))
    f
  cStructSize = 16
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 4 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 8 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 12 :: Ptr CFloat)) (CFloat (zero))
    f

instance FromCStruct Vector4f where
  peekCStruct p = do
    x <- peek @CFloat ((p `plusPtr` 0 :: Ptr CFloat))
    y <- peek @CFloat ((p `plusPtr` 4 :: Ptr CFloat))
    z <- peek @CFloat ((p `plusPtr` 8 :: Ptr CFloat))
    w <- peek @CFloat ((p `plusPtr` 12 :: Ptr CFloat))
    pure $ Vector4f
             (coerce @CFloat @Float x) (coerce @CFloat @Float y) (coerce @CFloat @Float z) (coerce @CFloat @Float w)

instance Storable Vector4f where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero Vector4f where
  zero = Vector4f
           zero
           zero
           zero
           zero


-- | XrColor4f - Color Vector
--
-- == Member Descriptions
--
-- = Description
--
-- Unless otherwise specified, colors are encoded as linear (not with sRGB
-- nor other gamma compression) values with individual components being in
-- the range of 0.0 through 1.0, and without the RGB components being
-- premultiplied by the alpha component.
--
-- = See Also
--
-- 'OpenXR.Extensions.XR_KHR_composition_layer_color_scale_bias.CompositionLayerColorScaleBiasKHR'
data Color4f = Color4f
  { -- | @r@ is the red component of the color.
    r :: Float
  , -- | @g@ is the green component of the color.
    g :: Float
  , -- | @b@ is the blue component of the color.
    b :: Float
  , -- | @a@ is the alpha component of the color.
    a :: Float
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (Color4f)
#endif
deriving instance Show Color4f

instance ToCStruct Color4f where
  withCStruct x f = allocaBytesAligned 16 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p Color4f{..} f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (r))
    poke ((p `plusPtr` 4 :: Ptr CFloat)) (CFloat (g))
    poke ((p `plusPtr` 8 :: Ptr CFloat)) (CFloat (b))
    poke ((p `plusPtr` 12 :: Ptr CFloat)) (CFloat (a))
    f
  cStructSize = 16
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 4 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 8 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 12 :: Ptr CFloat)) (CFloat (zero))
    f

instance FromCStruct Color4f where
  peekCStruct p = do
    r <- peek @CFloat ((p `plusPtr` 0 :: Ptr CFloat))
    g <- peek @CFloat ((p `plusPtr` 4 :: Ptr CFloat))
    b <- peek @CFloat ((p `plusPtr` 8 :: Ptr CFloat))
    a <- peek @CFloat ((p `plusPtr` 12 :: Ptr CFloat))
    pure $ Color4f
             (coerce @CFloat @Float r) (coerce @CFloat @Float g) (coerce @CFloat @Float b) (coerce @CFloat @Float a)

instance Storable Color4f where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero Color4f where
  zero = Color4f
           zero
           zero
           zero
           zero


-- | XrFovf - Field of view
--
-- == Member Descriptions
--
-- = Description
--
-- Angles to the right of the center and upwards from the center are
-- positive, and angles to the left of the center and down from the center
-- are negative. The total horizontal field of view is @angleRight@ minus
-- @angleLeft@, and the total vertical field of view is @angleUp@ minus
-- @angleDown@. For a symmetric FoV, @angleRight@ and @angleUp@ will have
-- positive values, @angleLeft@ will be -@angleRight@, and @angleDown@ will
-- be -@angleUp@.
--
-- The angles /must/ be specified in radians, and /must/ be between -π\/2
-- and π\/2 exclusively.
--
-- When @angleLeft@ > @angleRight@, the content of the view /must/ be
-- flipped horizontally. When @angleDown@ > @angleUp@, the content of the
-- view /must/ be flipped vertically.
--
-- = See Also
--
-- 'CompositionLayerProjectionView', 'OpenXR.Core10.DisplayTiming.View',
-- 'OpenXR.Extensions.XR_EPIC_view_configuration_fov.ViewConfigurationViewFovEPIC'
data Fovf = Fovf
  { -- | @angleLeft@ is the angle of the left side of the field of view. For a
    -- symmetric field of view this value is negative.
    angleLeft :: Float
  , -- | @angleRight@ is the angle of the right side of the field of view.
    angleRight :: Float
  , -- | @angleUp@ is the angle of the top part of the field of view.
    angleUp :: Float
  , -- | @angleDown@ is the angle of the bottom part of the field of view. For a
    -- symmetric field of view this value is negative.
    angleDown :: Float
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (Fovf)
#endif
deriving instance Show Fovf

instance ToCStruct Fovf where
  withCStruct x f = allocaBytesAligned 16 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p Fovf{..} f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (angleLeft))
    poke ((p `plusPtr` 4 :: Ptr CFloat)) (CFloat (angleRight))
    poke ((p `plusPtr` 8 :: Ptr CFloat)) (CFloat (angleUp))
    poke ((p `plusPtr` 12 :: Ptr CFloat)) (CFloat (angleDown))
    f
  cStructSize = 16
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 4 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 8 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 12 :: Ptr CFloat)) (CFloat (zero))
    f

instance FromCStruct Fovf where
  peekCStruct p = do
    angleLeft <- peek @CFloat ((p `plusPtr` 0 :: Ptr CFloat))
    angleRight <- peek @CFloat ((p `plusPtr` 4 :: Ptr CFloat))
    angleUp <- peek @CFloat ((p `plusPtr` 8 :: Ptr CFloat))
    angleDown <- peek @CFloat ((p `plusPtr` 12 :: Ptr CFloat))
    pure $ Fovf
             (coerce @CFloat @Float angleLeft) (coerce @CFloat @Float angleRight) (coerce @CFloat @Float angleUp) (coerce @CFloat @Float angleDown)

instance Storable Fovf where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero Fovf where
  zero = Fovf
           zero
           zero
           zero
           zero


-- | XrSwapchainSubImage - Composition layer data
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'OpenXR.Extensions.XR_KHR_composition_layer_cylinder.CompositionLayerCylinderKHR',
-- 'OpenXR.Extensions.XR_KHR_composition_layer_depth.CompositionLayerDepthInfoKHR',
-- 'OpenXR.Extensions.XR_KHR_composition_layer_equirect2.CompositionLayerEquirect2KHR',
-- 'OpenXR.Extensions.XR_KHR_composition_layer_equirect.CompositionLayerEquirectKHR',
-- 'CompositionLayerProjectionView', 'CompositionLayerQuad',
-- 'OpenXR.Core10.DisplayTiming.FrameEndInfo',
-- 'OpenXR.Core10.FundamentalTypes.Rect2Di',
-- 'OpenXR.Core10.Handles.Swapchain'
data SwapchainSubImage = SwapchainSubImage
  { -- | @swapchain@ is the 'OpenXR.Core10.Handles.Swapchain' to be displayed.
    --
    -- #VUID-XrSwapchainSubImage-swapchain-parameter# @swapchain@ /must/ be a
    -- valid 'OpenXR.Core10.Handles.Swapchain' handle
    swapchain :: Ptr Swapchain_T
  , -- | @imageRect@ is an 'OpenXR.Core10.FundamentalTypes.Rect2Di' representing
    -- the valid portion of the image to use, in pixels. It also implicitly
    -- defines the transform from normalized image coordinates into pixel
    -- coordinates. Note that the compositor /may/ bleed in pixels from outside
    -- the bounds in some cases, for instance due to mipmapping.
    imageRect :: Rect2Di
  , -- | @imageArrayIndex@ is the image array index, with 0 meaning the first or
    -- only array element.
    imageArrayIndex :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SwapchainSubImage)
#endif
deriving instance Show SwapchainSubImage

instance ToCStruct SwapchainSubImage where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SwapchainSubImage{..} f = do
    poke ((p `plusPtr` 0 :: Ptr (Ptr Swapchain_T))) (swapchain)
    poke ((p `plusPtr` 8 :: Ptr Rect2Di)) (imageRect)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (imageArrayIndex)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr (Ptr Swapchain_T))) (zero)
    poke ((p `plusPtr` 8 :: Ptr Rect2Di)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    f

instance FromCStruct SwapchainSubImage where
  peekCStruct p = do
    swapchain <- peek @(Ptr Swapchain_T) ((p `plusPtr` 0 :: Ptr (Ptr Swapchain_T)))
    imageRect <- peekCStruct @Rect2Di ((p `plusPtr` 8 :: Ptr Rect2Di))
    imageArrayIndex <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ SwapchainSubImage
             swapchain imageRect imageArrayIndex

instance Storable SwapchainSubImage where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SwapchainSubImage where
  zero = SwapchainSubImage
           zero
           zero
           zero


-- | XrCompositionLayerBaseHeader - Composition layer base header
--
-- == Member Descriptions
--
-- = Description
--
-- All composition layer structures begin with the elements described in
-- the 'CompositionLayerBaseHeader'. The 'CompositionLayerBaseHeader'
-- structure is not intended to be directly used, but forms a basis for
-- defining current and future structures containing composition layer
-- information. The 'OpenXR.Core10.DisplayTiming.FrameEndInfo' structure
-- contains an array of pointers to these polymorphic header structures.
-- All composition layer type pointers /must/ be type-castable as an
-- 'CompositionLayerBaseHeader' pointer.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.CompositionLayerFlagBits.CompositionLayerFlags',
-- 'OpenXR.Core10.DisplayTiming.FrameEndInfo',
-- 'OpenXR.Extensions.XR_MSFT_secondary_view_configuration.SecondaryViewConfigurationLayerInfoMSFT',
-- 'OpenXR.Core10.Handles.Space',
-- 'OpenXR.Core10.Enums.StructureType.StructureType', 'SwapchainSubImage'
data CompositionLayerBaseHeader (es :: [Type]) = CompositionLayerBaseHeader
  { -- | @type@ is the 'OpenXR.Core10.Enums.StructureType.StructureType' of this
    -- structure. This base structure itself has no associated
    -- 'OpenXR.Core10.Enums.StructureType.StructureType' value.
    --
    -- #VUID-XrCompositionLayerBaseHeader-type-type# @type@ /must/ be one of
    -- the following XrStructureType values:
    -- 'OpenXR.Core10.Enums.StructureType.TYPE_COMPOSITION_LAYER_CUBE_KHR',
    -- 'OpenXR.Core10.Enums.StructureType.TYPE_COMPOSITION_LAYER_CYLINDER_KHR',
    -- 'OpenXR.Core10.Enums.StructureType.TYPE_COMPOSITION_LAYER_EQUIRECT2_KHR',
    -- 'OpenXR.Core10.Enums.StructureType.TYPE_COMPOSITION_LAYER_EQUIRECT_KHR',
    -- 'OpenXR.Core10.Enums.StructureType.TYPE_COMPOSITION_LAYER_PROJECTION',
    -- 'OpenXR.Core10.Enums.StructureType.TYPE_COMPOSITION_LAYER_QUAD'
    type' :: StructureType
  , -- | @next@ is @NULL@ or a pointer to the next structure in a structure
    -- chain. No such structures are defined in core OpenXR.
    --
    -- #VUID-XrCompositionLayerBaseHeader-next-next# @next@ /must/ be @NULL@ or
    -- a valid pointer to the
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>.
    -- See also:
    -- 'OpenXR.Extensions.XR_KHR_composition_layer_color_scale_bias.CompositionLayerColorScaleBiasKHR'
    next :: Chain es
  , -- | @layerFlags@ is a bitmask of
    -- 'OpenXR.Core10.Enums.CompositionLayerFlagBits.CompositionLayerFlagBits'
    -- describing flags to apply to the layer.
    --
    -- #VUID-XrCompositionLayerBaseHeader-layerFlags-parameter# @layerFlags@
    -- /must/ be @0@ or a valid combination of
    -- 'OpenXR.Core10.Enums.CompositionLayerFlagBits.CompositionLayerFlagBits'
    -- values
    layerFlags :: CompositionLayerFlags
  , -- | @space@ is the 'OpenXR.Core10.Handles.Space' in which the layer will be
    -- kept stable over time.
    --
    -- #VUID-XrCompositionLayerBaseHeader-space-parameter# @space@ /must/ be a
    -- valid 'OpenXR.Core10.Handles.Space' handle
    space :: Ptr Space_T
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CompositionLayerBaseHeader (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (CompositionLayerBaseHeader es)

instance Extensible CompositionLayerBaseHeader where
  extensibleTypeName = "CompositionLayerBaseHeader"
  setNext x next = x{next = next}
  getNext CompositionLayerBaseHeader{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends CompositionLayerBaseHeader e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @CompositionLayerColorScaleBiasKHR = Just f
    | otherwise = Nothing

class ToCStruct a => IsCompositionLayer a where
  toCompositionLayerBaseHeader :: a -> CompositionLayerBaseHeader '[]

instance Inheritable (CompositionLayerBaseHeader '[]) where
  peekSomeCChild :: Ptr (SomeChild (CompositionLayerBaseHeader '[])) -> IO (SomeChild (CompositionLayerBaseHeader '[]))
  peekSomeCChild p = do
    ty <- peek @StructureType (castPtr @(SomeChild (CompositionLayerBaseHeader '[])) @StructureType p)
    case ty of
      TYPE_COMPOSITION_LAYER_EQUIRECT2_KHR -> SomeChild <$> peekCStruct (castPtr @(SomeChild (CompositionLayerBaseHeader '[])) @CompositionLayerEquirect2KHR p)
      TYPE_COMPOSITION_LAYER_EQUIRECT_KHR -> SomeChild <$> peekCStruct (castPtr @(SomeChild (CompositionLayerBaseHeader '[])) @CompositionLayerEquirectKHR p)
      TYPE_COMPOSITION_LAYER_CUBE_KHR -> SomeChild <$> peekCStruct (castPtr @(SomeChild (CompositionLayerBaseHeader '[])) @CompositionLayerCubeKHR p)
      TYPE_COMPOSITION_LAYER_CYLINDER_KHR -> SomeChild <$> peekCStruct (castPtr @(SomeChild (CompositionLayerBaseHeader '[])) @CompositionLayerCylinderKHR p)
      TYPE_COMPOSITION_LAYER_QUAD -> SomeChild <$> peekCStruct (castPtr @(SomeChild (CompositionLayerBaseHeader '[])) @CompositionLayerQuad p)
      TYPE_COMPOSITION_LAYER_PROJECTION -> SomeChild <$> peekCStruct (castPtr @(SomeChild (CompositionLayerBaseHeader '[])) @CompositionLayerProjection p)
      c -> throwIO $
        IOError
          Nothing
          InvalidArgument
          "peekSomeCChild"
          ("Illegal struct inheritance of CompositionLayerBaseHeader with " <> show c)
          Nothing
          Nothing

instance (Extendss CompositionLayerBaseHeader es, PokeChain es) => ToCStruct (CompositionLayerBaseHeader es) where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CompositionLayerBaseHeader{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (type')
    next'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) next''
    lift $ poke ((p `plusPtr` 16 :: Ptr CompositionLayerFlags)) (layerFlags)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Space_T))) (space)
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (zero)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Space_T))) (zero)
    lift $ f

instance (Extendss CompositionLayerBaseHeader es, PeekChain es) => FromCStruct (CompositionLayerBaseHeader es) where
  peekCStruct p = do
    type' <- peek @StructureType ((p `plusPtr` 0 :: Ptr StructureType))
    next <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next' <- peekChain (castPtr next)
    layerFlags <- peek @CompositionLayerFlags ((p `plusPtr` 16 :: Ptr CompositionLayerFlags))
    space <- peek @(Ptr Space_T) ((p `plusPtr` 24 :: Ptr (Ptr Space_T)))
    pure $ CompositionLayerBaseHeader
             type' next' layerFlags space

instance es ~ '[] => Zero (CompositionLayerBaseHeader es) where
  zero = CompositionLayerBaseHeader
           zero
           ()
           zero
           zero


-- | XrCompositionLayerProjectionView - Projection layer element
--
-- == Member Descriptions
--
-- = Description
--
-- The count and order of view poses submitted with
-- 'CompositionLayerProjection' /must/ be the same order as that returned
-- by 'OpenXR.Core10.DisplayTiming.locateViews'. The
-- 'CompositionLayerProjectionView'::@pose@ and
-- 'CompositionLayerProjectionView'::@fov@ /should/ almost always derive
-- from 'OpenXR.Core10.DisplayTiming.View'::@pose@ and
-- 'OpenXR.Core10.DisplayTiming.View'::@fov@ as found in the
-- 'OpenXR.Core10.DisplayTiming.locateViews'::@views@ array. However,
-- applications /may/ submit an 'CompositionLayerProjectionView' which has
-- a different view or FOV than that from
-- 'OpenXR.Core10.DisplayTiming.locateViews'. In this case, the runtime
-- will map the view and FOV to the system display appropriately. In the
-- case that two submitted views within a single layer overlap, they /must/
-- be composited in view array order.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'CompositionLayerProjection', 'Fovf', 'OpenXR.Core10.Space.Posef',
-- 'OpenXR.Core10.Enums.StructureType.StructureType', 'SwapchainSubImage'
data CompositionLayerProjectionView (es :: [Type]) = CompositionLayerProjectionView
  { -- | @next@ is @NULL@ or a pointer to the next structure in a structure
    -- chain. No such structures are defined in core OpenXR.
    --
    -- #VUID-XrCompositionLayerProjectionView-next-next# @next@ /must/ be
    -- @NULL@ or a valid pointer to the
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>.
    -- See also:
    -- 'OpenXR.Extensions.XR_KHR_composition_layer_depth.CompositionLayerDepthInfoKHR'
    next :: Chain es
  , -- | @pose@ is an 'OpenXR.Core10.Space.Posef' defining the location and
    -- orientation of this projection element in the @space@ of the
    -- corresponding 'CompositionLayerProjectionView'.
    pose :: Posef
  , -- | @fov@ is the 'Fovf' for this projection element.
    fov :: Fovf
  , -- | @subImage@ is the image layer 'SwapchainSubImage' to use.
    --
    -- #VUID-XrCompositionLayerProjectionView-subImage-parameter# @subImage@
    -- /must/ be a valid 'SwapchainSubImage' structure
    subImage :: SwapchainSubImage
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CompositionLayerProjectionView (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (CompositionLayerProjectionView es)

instance Extensible CompositionLayerProjectionView where
  extensibleTypeName = "CompositionLayerProjectionView"
  setNext x next = x{next = next}
  getNext CompositionLayerProjectionView{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends CompositionLayerProjectionView e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @CompositionLayerDepthInfoKHR = Just f
    | otherwise = Nothing

instance (Extendss CompositionLayerProjectionView es, PokeChain es) => ToCStruct (CompositionLayerProjectionView es) where
  withCStruct x f = allocaBytesAligned 96 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CompositionLayerProjectionView{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_COMPOSITION_LAYER_PROJECTION_VIEW)
    next'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) next''
    lift $ poke ((p `plusPtr` 16 :: Ptr Posef)) (pose)
    lift $ poke ((p `plusPtr` 44 :: Ptr Fovf)) (fov)
    lift $ poke ((p `plusPtr` 64 :: Ptr SwapchainSubImage)) (subImage)
    lift $ f
  cStructSize = 96
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_COMPOSITION_LAYER_PROJECTION_VIEW)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr Posef)) (zero)
    lift $ poke ((p `plusPtr` 44 :: Ptr Fovf)) (zero)
    lift $ poke ((p `plusPtr` 64 :: Ptr SwapchainSubImage)) (zero)
    lift $ f

instance (Extendss CompositionLayerProjectionView es, PeekChain es) => FromCStruct (CompositionLayerProjectionView es) where
  peekCStruct p = do
    next <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next' <- peekChain (castPtr next)
    pose <- peekCStruct @Posef ((p `plusPtr` 16 :: Ptr Posef))
    fov <- peekCStruct @Fovf ((p `plusPtr` 44 :: Ptr Fovf))
    subImage <- peekCStruct @SwapchainSubImage ((p `plusPtr` 64 :: Ptr SwapchainSubImage))
    pure $ CompositionLayerProjectionView
             next' pose fov subImage

instance es ~ '[] => Zero (CompositionLayerProjectionView es) where
  zero = CompositionLayerProjectionView
           ()
           zero
           zero
           zero


-- | XrCompositionLayerProjection - Composition layer for projection
--
-- == Member Descriptions
--
-- = Description
--
-- Note
--
-- Because a runtime may reproject the layer over time, a projection layer
-- should specify an 'OpenXR.Core10.Handles.Space' in which to maximize
-- stability of the layer content. For example, a projection layer
-- containing world-locked content should use an
-- 'OpenXR.Core10.Handles.Space' which is also world-locked, such as the
-- @LOCAL@ or @STAGE@ reference spaces. In the case that the projection
-- layer should be head-locked, such as a heads up display, the @VIEW@
-- reference space would provide the highest quality layer reprojection.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrCompositionLayerProjection-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_COMPOSITION_LAYER_PROJECTION'
--
-- -   #VUID-XrCompositionLayerProjection-next-next# @next@ /must/ be
--     @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrCompositionLayerProjection-layerFlags-parameter#
--     @layerFlags@ /must/ be @0@ or a valid combination of
--     'OpenXR.Core10.Enums.CompositionLayerFlagBits.CompositionLayerFlagBits'
--     values
--
-- -   #VUID-XrCompositionLayerProjection-space-parameter# @space@ /must/
--     be a valid 'OpenXR.Core10.Handles.Space' handle
--
-- -   #VUID-XrCompositionLayerProjection-views-parameter# @views@ /must/
--     be a pointer to an array of @viewCount@ valid
--     'CompositionLayerProjectionView' structures
--
-- -   #VUID-XrCompositionLayerProjection-viewCount-arraylength# The
--     @viewCount@ parameter /must/ be greater than @0@
--
-- = See Also
--
-- 'CompositionLayerBaseHeader',
-- 'OpenXR.Core10.Enums.CompositionLayerFlagBits.CompositionLayerFlags',
-- 'CompositionLayerProjectionView', 'OpenXR.Core10.Handles.Space',
-- 'OpenXR.Core10.Enums.StructureType.StructureType', 'SwapchainSubImage'
data CompositionLayerProjection = CompositionLayerProjection
  { -- | @layerFlags@ is a bitmask of
    -- 'OpenXR.Core10.Enums.CompositionLayerFlagBits.CompositionLayerFlagBits'
    -- describing flags to apply to the layer.
    layerFlags :: CompositionLayerFlags
  , -- | @space@ is the 'OpenXR.Core10.Handles.Space' in which the @pose@ of each
    -- 'CompositionLayerProjectionView' is evaluated over time by the
    -- compositor.
    space :: Ptr Space_T
  , -- | @views@ is the array of type 'CompositionLayerProjectionView' containing
    -- each projection layer view.
    views :: Vector (SomeStruct CompositionLayerProjectionView)
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CompositionLayerProjection)
#endif
deriving instance Show CompositionLayerProjection

instance IsCompositionLayer CompositionLayerProjection where
  toCompositionLayerBaseHeader CompositionLayerProjection{..} = CompositionLayerBaseHeader{type' = TYPE_COMPOSITION_LAYER_PROJECTION, next = (), ..}

instance ToCStruct CompositionLayerProjection where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CompositionLayerProjection{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_COMPOSITION_LAYER_PROJECTION)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr CompositionLayerFlags)) (layerFlags)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Space_T))) (space)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (views)) :: Word32))
    pViews' <- ContT $ allocaBytesAligned @(CompositionLayerProjectionView _) ((Data.Vector.length (views)) * 96) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pViews' `plusPtr` (96 * (i)) :: Ptr (CompositionLayerProjectionView _))) (e) . ($ ())) (views)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr (CompositionLayerProjectionView _)))) (pViews')
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_COMPOSITION_LAYER_PROJECTION)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr (Ptr Space_T))) (zero)
    f

instance FromCStruct CompositionLayerProjection where
  peekCStruct p = do
    layerFlags <- peek @CompositionLayerFlags ((p `plusPtr` 16 :: Ptr CompositionLayerFlags))
    space <- peek @(Ptr Space_T) ((p `plusPtr` 24 :: Ptr (Ptr Space_T)))
    viewCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    views <- peek @(Ptr (CompositionLayerProjectionView _)) ((p `plusPtr` 40 :: Ptr (Ptr (CompositionLayerProjectionView _))))
    views' <- generateM (fromIntegral viewCount) (\i -> peekSomeCStruct (forgetExtensions ((views `advancePtrBytes` (96 * (i)) :: Ptr (CompositionLayerProjectionView _)))))
    pure $ CompositionLayerProjection
             layerFlags space views'

instance Zero CompositionLayerProjection where
  zero = CompositionLayerProjection
           zero
           zero
           mempty


-- | XrCompositionLayerQuad - Quad composition layer
--
-- == Member Descriptions
--
-- = Description
--
-- The 'CompositionLayerQuad' layer is useful for user interface elements
-- or 2D content rendered into the virtual world. The layer’s
-- 'SwapchainSubImage'::swapchain image is applied to a quad in the virtual
-- world space. Only front face of the quad surface is visible; the back
-- face is not visible and /must/ not be drawn by the runtime. A quad layer
-- has no thickness; it is a two-dimensional object positioned and oriented
-- in 3D space. The position of a quad refers to the center of the quad
-- within the given 'OpenXR.Core10.Handles.Space'. The orientation of the
-- quad refers to the orientation of the normal vector from the front face.
-- The size of a quad refers to the quad’s size in the x-y plane of the
-- given 'OpenXR.Core10.Handles.Space'’s coordinate system. A quad with a
-- position of {0,0,0}, rotation of {0,0,0,1} (no rotation), and a size of
-- {1,1} refers to a 1 meter x 1 meter quad centered at {0,0,0} with its
-- front face normal vector coinciding with the +z axis.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'CompositionLayerBaseHeader',
-- 'OpenXR.Core10.Enums.CompositionLayerFlagBits.CompositionLayerFlags',
-- 'OpenXR.Core10.FundamentalTypes.Extent2Df',
-- 'OpenXR.Core10.Enums.EyeVisibility.EyeVisibility',
-- 'OpenXR.Core10.Space.Posef', 'OpenXR.Core10.Handles.Space',
-- 'OpenXR.Core10.Enums.StructureType.StructureType', 'SwapchainSubImage'
data CompositionLayerQuad = CompositionLayerQuad
  { -- | @layerFlags@ is a bitmask of
    -- 'OpenXR.Core10.Enums.CompositionLayerFlagBits.CompositionLayerFlagBits'
    -- describing flags to apply to the layer.
    --
    -- #VUID-XrCompositionLayerQuad-layerFlags-parameter# @layerFlags@ /must/
    -- be @0@ or a valid combination of
    -- 'OpenXR.Core10.Enums.CompositionLayerFlagBits.CompositionLayerFlagBits'
    -- values
    layerFlags :: CompositionLayerFlags
  , -- | @space@ is the 'OpenXR.Core10.Handles.Space' in which the @pose@ of the
    -- quad layer is evaluated over time.
    --
    -- #VUID-XrCompositionLayerQuad-space-parameter# @space@ /must/ be a valid
    -- 'OpenXR.Core10.Handles.Space' handle
    space :: Ptr Space_T
  , -- | @eyeVisibility@ is the 'OpenXR.Core10.Enums.EyeVisibility.EyeVisibility'
    -- for this layer.
    --
    -- #VUID-XrCompositionLayerQuad-eyeVisibility-parameter# @eyeVisibility@
    -- /must/ be a valid 'OpenXR.Core10.Enums.EyeVisibility.EyeVisibility'
    -- value
    eyeVisibility :: EyeVisibility
  , -- | @subImage@ is the image layer 'SwapchainSubImage' to use.
    --
    -- #VUID-XrCompositionLayerQuad-subImage-parameter# @subImage@ /must/ be a
    -- valid 'SwapchainSubImage' structure
    subImage :: SwapchainSubImage
  , -- | @pose@ is an 'OpenXR.Core10.Space.Posef' defining the position and
    -- orientation of the quad in the reference frame of the @space@.
    pose :: Posef
  , -- | @size@ is the width and height of the quad in meters.
    size :: Extent2Df
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CompositionLayerQuad)
#endif
deriving instance Show CompositionLayerQuad

instance IsCompositionLayer CompositionLayerQuad where
  toCompositionLayerBaseHeader CompositionLayerQuad{..} = CompositionLayerBaseHeader{type' = TYPE_COMPOSITION_LAYER_QUAD, next = (), ..}

instance ToCStruct CompositionLayerQuad where
  withCStruct x f = allocaBytesAligned 112 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CompositionLayerQuad{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_COMPOSITION_LAYER_QUAD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CompositionLayerFlags)) (layerFlags)
    poke ((p `plusPtr` 24 :: Ptr (Ptr Space_T))) (space)
    poke ((p `plusPtr` 32 :: Ptr EyeVisibility)) (eyeVisibility)
    poke ((p `plusPtr` 40 :: Ptr SwapchainSubImage)) (subImage)
    poke ((p `plusPtr` 72 :: Ptr Posef)) (pose)
    poke ((p `plusPtr` 100 :: Ptr Extent2Df)) (size)
    f
  cStructSize = 112
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_COMPOSITION_LAYER_QUAD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr (Ptr Space_T))) (zero)
    poke ((p `plusPtr` 32 :: Ptr EyeVisibility)) (zero)
    poke ((p `plusPtr` 40 :: Ptr SwapchainSubImage)) (zero)
    poke ((p `plusPtr` 72 :: Ptr Posef)) (zero)
    poke ((p `plusPtr` 100 :: Ptr Extent2Df)) (zero)
    f

instance FromCStruct CompositionLayerQuad where
  peekCStruct p = do
    layerFlags <- peek @CompositionLayerFlags ((p `plusPtr` 16 :: Ptr CompositionLayerFlags))
    space <- peek @(Ptr Space_T) ((p `plusPtr` 24 :: Ptr (Ptr Space_T)))
    eyeVisibility <- peek @EyeVisibility ((p `plusPtr` 32 :: Ptr EyeVisibility))
    subImage <- peekCStruct @SwapchainSubImage ((p `plusPtr` 40 :: Ptr SwapchainSubImage))
    pose <- peekCStruct @Posef ((p `plusPtr` 72 :: Ptr Posef))
    size <- peekCStruct @Extent2Df ((p `plusPtr` 100 :: Ptr Extent2Df))
    pure $ CompositionLayerQuad
             layerFlags space eyeVisibility subImage pose size

instance Storable CompositionLayerQuad where
  sizeOf ~_ = 112
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CompositionLayerQuad where
  zero = CompositionLayerQuad
           zero
           zero
           zero
           zero
           zero
           zero


-- | XrHapticVibration - Base header for haptic feedback
--
-- == Member Descriptions
--
-- = Description
--
-- The 'HapticVibration' is used in calls to
-- 'OpenXR.Core10.Haptics.applyHapticFeedback' that trigger @vibration@
-- output actions.
--
-- The @duration@, and @frequency@ parameters /may/ be clamped to
-- implementation-dependent ranges.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrDuration >,
-- 'OpenXR.Core10.Haptics.HapticBaseHeader',
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'OpenXR.Core10.Haptics.applyHapticFeedback'
data HapticVibration = HapticVibration
  { -- | @duration@ is the number of nanoseconds the vibration /should/ last. If
    -- 'OpenXR.Core10.APIConstants.MIN_HAPTIC_DURATION' is specified, the
    -- runtime /must/ produce a short haptics pulse of minimal supported
    -- duration for the haptic device.
    duration :: Duration
  , -- | @frequency@ is the frequency of the vibration in Hz. If
    -- 'OpenXR.Core10.APIConstants.FREQUENCY_UNSPECIFIED' is specified, it is
    -- left to the runtime to decide the optimal frequency value to use.
    frequency :: Float
  , -- | @amplitude@ is the amplitude of the vibration between 0.0 and 1.0.
    amplitude :: Float
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (HapticVibration)
#endif
deriving instance Show HapticVibration

instance IsHaptic HapticVibration where
  toHapticBaseHeader HapticVibration{} = HapticBaseHeader{type' = TYPE_HAPTIC_VIBRATION}

instance ToCStruct HapticVibration where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p HapticVibration{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_HAPTIC_VIBRATION)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Duration)) (duration)
    poke ((p `plusPtr` 24 :: Ptr CFloat)) (CFloat (frequency))
    poke ((p `plusPtr` 28 :: Ptr CFloat)) (CFloat (amplitude))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_HAPTIC_VIBRATION)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Duration)) (zero)
    poke ((p `plusPtr` 28 :: Ptr CFloat)) (CFloat (zero))
    f

instance FromCStruct HapticVibration where
  peekCStruct p = do
    duration <- peek @Duration ((p `plusPtr` 16 :: Ptr Duration))
    frequency <- peek @CFloat ((p `plusPtr` 24 :: Ptr CFloat))
    amplitude <- peek @CFloat ((p `plusPtr` 28 :: Ptr CFloat))
    pure $ HapticVibration
             duration (coerce @CFloat @Float frequency) (coerce @CFloat @Float amplitude)

instance Storable HapticVibration where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero HapticVibration where
  zero = HapticVibration
           zero
           zero
           zero


-- | XrEventDataBaseHeader - Base header for an event
--
-- == Parameter Descriptions
--
-- = Description
--
-- The 'EventDataBaseHeader' is a generic structure used to identify the
-- common event data elements.
--
-- Upon receipt, the 'EventDataBaseHeader' pointer should be type-cast to a
-- pointer of the appropriate event data based on the @type@ parameter.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'OpenXR.Core10.Instance.pollEvent'
data EventDataBaseHeader = EventDataBaseHeader
  { -- | @type@ is the 'OpenXR.Core10.Enums.StructureType.StructureType' of this
    -- structure. This base structure itself has no associated
    -- 'OpenXR.Core10.Enums.StructureType.StructureType' value.
    --
    -- #VUID-XrEventDataBaseHeader-type-type# @type@ /must/ be one of the
    -- following XrStructureType values:
    -- 'OpenXR.Core10.Enums.StructureType.TYPE_EVENT_DATA_DISPLAY_REFRESH_RATE_CHANGED_FB',
    -- 'OpenXR.Core10.Enums.StructureType.TYPE_EVENT_DATA_EVENTS_LOST',
    -- 'OpenXR.Core10.Enums.StructureType.TYPE_EVENT_DATA_INSTANCE_LOSS_PENDING',
    -- 'OpenXR.Core10.Enums.StructureType.TYPE_EVENT_DATA_INTERACTION_PROFILE_CHANGED',
    -- 'OpenXR.Core10.Enums.StructureType.TYPE_EVENT_DATA_MAIN_SESSION_VISIBILITY_CHANGED_EXTX',
    -- 'OpenXR.Core10.Enums.StructureType.TYPE_EVENT_DATA_PERF_SETTINGS_EXT',
    -- 'OpenXR.Core10.Enums.StructureType.TYPE_EVENT_DATA_REFERENCE_SPACE_CHANGE_PENDING',
    -- 'OpenXR.Core10.Enums.StructureType.TYPE_EVENT_DATA_SESSION_STATE_CHANGED',
    -- 'OpenXR.Core10.Enums.StructureType.TYPE_EVENT_DATA_VISIBILITY_MASK_CHANGED_KHR'
    type' :: StructureType }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (EventDataBaseHeader)
#endif
deriving instance Show EventDataBaseHeader

class ToCStruct a => IsEventData a where
  toEventDataBaseHeader :: a -> EventDataBaseHeader

instance Inheritable EventDataBaseHeader where
  peekSomeCChild :: Ptr (SomeChild EventDataBaseHeader) -> IO (SomeChild EventDataBaseHeader)
  peekSomeCChild p = do
    ty <- peek @StructureType (castPtr @(SomeChild EventDataBaseHeader) @StructureType p)
    case ty of
      TYPE_EVENT_DATA_DISPLAY_REFRESH_RATE_CHANGED_FB -> SomeChild <$> peekCStruct (castPtr @(SomeChild EventDataBaseHeader) @EventDataDisplayRefreshRateChangedFB p)
      TYPE_EVENT_DATA_MAIN_SESSION_VISIBILITY_CHANGED_EXTX -> SomeChild <$> peekCStruct (castPtr @(SomeChild EventDataBaseHeader) @EventDataMainSessionVisibilityChangedEXTX p)
      TYPE_EVENT_DATA_INTERACTION_PROFILE_CHANGED -> SomeChild <$> peekCStruct (castPtr @(SomeChild EventDataBaseHeader) @EventDataInteractionProfileChanged p)
      TYPE_EVENT_DATA_VISIBILITY_MASK_CHANGED_KHR -> SomeChild <$> peekCStruct (castPtr @(SomeChild EventDataBaseHeader) @EventDataVisibilityMaskChangedKHR p)
      TYPE_EVENT_DATA_PERF_SETTINGS_EXT -> SomeChild <$> peekCStruct (castPtr @(SomeChild EventDataBaseHeader) @EventDataPerfSettingsEXT p)
      TYPE_EVENT_DATA_REFERENCE_SPACE_CHANGE_PENDING -> SomeChild <$> peekCStruct (castPtr @(SomeChild EventDataBaseHeader) @EventDataReferenceSpaceChangePending p)
      TYPE_EVENT_DATA_SESSION_STATE_CHANGED -> SomeChild <$> peekCStruct (castPtr @(SomeChild EventDataBaseHeader) @EventDataSessionStateChanged p)
      TYPE_EVENT_DATA_INSTANCE_LOSS_PENDING -> SomeChild <$> peekCStruct (castPtr @(SomeChild EventDataBaseHeader) @EventDataInstanceLossPending p)
      TYPE_EVENT_DATA_EVENTS_LOST -> SomeChild <$> peekCStruct (castPtr @(SomeChild EventDataBaseHeader) @EventDataEventsLost p)
      c -> throwIO $
        IOError
          Nothing
          InvalidArgument
          "peekSomeCChild"
          ("Illegal struct inheritance of EventDataBaseHeader with " <> show c)
          Nothing
          Nothing

instance ToCStruct EventDataBaseHeader where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p EventDataBaseHeader{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (type')
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (zero)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct EventDataBaseHeader where
  peekCStruct p = do
    type' <- peek @StructureType ((p `plusPtr` 0 :: Ptr StructureType))
    pure $ EventDataBaseHeader
             type'

instance Storable EventDataBaseHeader where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero EventDataBaseHeader where
  zero = EventDataBaseHeader
           zero


-- | XrEventDataEventsLost - Event indicating events were lost
--
-- == Member Descriptions
--
-- = Description
--
-- Receiving the 'EventDataEventsLost' event structure indicates that the
-- event queue overflowed and some events were removed at the position
-- within the queue at which this event was found.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'EventDataBaseHeader',
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'OpenXR.Core10.Instance.pollEvent'
data EventDataEventsLost = EventDataEventsLost
  { -- | @lostEventCount@ is the number of events which have overflowed since the
    -- last call to 'OpenXR.Core10.Instance.pollEvent'.
    lostEventCount :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (EventDataEventsLost)
#endif
deriving instance Show EventDataEventsLost

instance IsEventData EventDataEventsLost where
  toEventDataBaseHeader EventDataEventsLost{} = EventDataBaseHeader{type' = TYPE_EVENT_DATA_EVENTS_LOST}

instance ToCStruct EventDataEventsLost where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p EventDataEventsLost{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_EVENT_DATA_EVENTS_LOST)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (lostEventCount)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_EVENT_DATA_EVENTS_LOST)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct EventDataEventsLost where
  peekCStruct p = do
    lostEventCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ EventDataEventsLost
             lostEventCount

instance Storable EventDataEventsLost where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero EventDataEventsLost where
  zero = EventDataEventsLost
           zero


-- | XrEventDataInstanceLossPending - Event indicating instance loss will
-- occur
--
-- = Members
--
-- Receiving the 'EventDataInstanceLossPending' event structure indicates
-- that the application is about to lose the indicated
-- 'OpenXR.Core10.Handles.Instance' at the indicated @lossTime@ in the
-- future. The application should call
-- 'OpenXR.Core10.Instance.destroyInstance' and relinquish any
-- instance-specific resources. This typically occurs to make way for a
-- replacement of the underlying runtime, such as via a software update.
--
-- = Description
--
-- After the application has destroyed all of its instances and their
-- children and waited past the specified time, it may then re-try
-- 'OpenXR.Core10.Instance.createInstance' in a loop waiting for whatever
-- maintenance the runtime is performing to complete. The runtime will
-- return 'OpenXR.Core10.Enums.Result.ERROR_INSTANCE_LOST' from
-- 'OpenXR.Core10.Instance.createInstance' as long as it is unable to
-- create the instance. Once the runtime has returned and is able to
-- continue, it /must/ resume returning
-- 'OpenXR.Core10.Enums.Result.SUCCESS' from
-- 'OpenXR.Core10.Instance.createInstance' if valid data is passed in.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'EventDataBaseHeader',
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >,
-- 'OpenXR.Core10.Instance.pollEvent'
data EventDataInstanceLossPending = EventDataInstanceLossPending
  { -- | @lossTime@ is the absolute time at which the indicated instance will be
    -- considered lost and become unusable.
    lossTime :: Time }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (EventDataInstanceLossPending)
#endif
deriving instance Show EventDataInstanceLossPending

instance IsEventData EventDataInstanceLossPending where
  toEventDataBaseHeader EventDataInstanceLossPending{} = EventDataBaseHeader{type' = TYPE_EVENT_DATA_INSTANCE_LOSS_PENDING}

instance ToCStruct EventDataInstanceLossPending where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p EventDataInstanceLossPending{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_EVENT_DATA_INSTANCE_LOSS_PENDING)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Time)) (lossTime)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_EVENT_DATA_INSTANCE_LOSS_PENDING)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Time)) (zero)
    f

instance FromCStruct EventDataInstanceLossPending where
  peekCStruct p = do
    lossTime <- peek @Time ((p `plusPtr` 16 :: Ptr Time))
    pure $ EventDataInstanceLossPending
             lossTime

instance Storable EventDataInstanceLossPending where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero EventDataInstanceLossPending where
  zero = EventDataInstanceLossPending
           zero


-- | XrEventDataSessionStateChanged - Event indicating session state changed
--
-- == Member Descriptions
--
-- = Description
--
-- Receiving the 'EventDataSessionStateChanged' event structure indicates
-- that the application has changed lifecycle state.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'EventDataBaseHeader', 'OpenXR.Core10.Handles.Session',
-- 'OpenXR.Core10.Enums.SessionState.SessionState',
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >,
-- 'OpenXR.Core10.Instance.pollEvent'
data EventDataSessionStateChanged = EventDataSessionStateChanged
  { -- | @session@ is the 'OpenXR.Core10.Handles.Session' which has changed
    -- state.
    --
    -- #VUID-XrEventDataSessionStateChanged-session-parameter# @session@ /must/
    -- be a valid 'OpenXR.Core10.Handles.Session' handle
    session :: Ptr Session_T
  , -- | @state@ is the current 'OpenXR.Core10.Enums.SessionState.SessionState'
    -- of the @session@.
    --
    -- #VUID-XrEventDataSessionStateChanged-state-parameter# @state@ /must/ be
    -- a valid 'OpenXR.Core10.Enums.SessionState.SessionState' value
    state :: SessionState
  , -- | @time@ is an
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >
    -- which indicates the time of the state change.
    time :: Time
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (EventDataSessionStateChanged)
#endif
deriving instance Show EventDataSessionStateChanged

instance IsEventData EventDataSessionStateChanged where
  toEventDataBaseHeader EventDataSessionStateChanged{} = EventDataBaseHeader{type' = TYPE_EVENT_DATA_SESSION_STATE_CHANGED}

instance ToCStruct EventDataSessionStateChanged where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p EventDataSessionStateChanged{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_EVENT_DATA_SESSION_STATE_CHANGED)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Session_T))) (session)
    poke ((p `plusPtr` 24 :: Ptr SessionState)) (state)
    poke ((p `plusPtr` 32 :: Ptr Time)) (time)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_EVENT_DATA_SESSION_STATE_CHANGED)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Session_T))) (zero)
    poke ((p `plusPtr` 24 :: Ptr SessionState)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Time)) (zero)
    f

instance FromCStruct EventDataSessionStateChanged where
  peekCStruct p = do
    session <- peek @(Ptr Session_T) ((p `plusPtr` 16 :: Ptr (Ptr Session_T)))
    state <- peek @SessionState ((p `plusPtr` 24 :: Ptr SessionState))
    time <- peek @Time ((p `plusPtr` 32 :: Ptr Time))
    pure $ EventDataSessionStateChanged
             session state time

instance Storable EventDataSessionStateChanged where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero EventDataSessionStateChanged where
  zero = EventDataSessionStateChanged
           zero
           zero
           zero


-- | XrEventDataReferenceSpaceChangePending - Notifies the application that a
-- reference space is changing
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.ReferenceSpaceType.REFERENCE_SPACE_TYPE_STAGE',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrBool32 >,
-- 'OpenXR.Core10.Space.Posef',
-- 'OpenXR.Core10.Enums.ReferenceSpaceType.ReferenceSpaceType',
-- 'OpenXR.Core10.Handles.Session',
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >,
-- 'OpenXR.Core10.Space.createReferenceSpace'
data EventDataReferenceSpaceChangePending = EventDataReferenceSpaceChangePending
  { -- | @session@ is the 'OpenXR.Core10.Handles.Session' for which the reference
    -- space is changing.
    --
    -- #VUID-XrEventDataReferenceSpaceChangePending-session-parameter#
    -- @session@ /must/ be a valid 'OpenXR.Core10.Handles.Session' handle
    session :: Ptr Session_T
  , -- | @referenceSpaceType@ is the
    -- 'OpenXR.Core10.Enums.ReferenceSpaceType.ReferenceSpaceType' that is
    -- changing.
    --
    -- #VUID-XrEventDataReferenceSpaceChangePending-referenceSpaceType-parameter#
    -- @referenceSpaceType@ /must/ be a valid
    -- 'OpenXR.Core10.Enums.ReferenceSpaceType.ReferenceSpaceType' value
    referenceSpaceType :: ReferenceSpaceType
  , -- | @changeTime@ is the target
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >
    -- after which 'OpenXR.Core10.Space.locateSpace' or
    -- 'OpenXR.Core10.DisplayTiming.locateViews' will return values that
    -- respect this change.
    changeTime :: Time
  , -- | @poseValid@ is true if the runtime can determine the @pose@ of the new
    -- space in the previous space before the change.
    poseValid :: Bool
  , -- | @poseInPreviousSpace@ is an 'OpenXR.Core10.Space.Posef' defining the
    -- position and orientation of the new reference space’s natural origin
    -- within the natural reference frame of its previous space.
    poseInPreviousSpace :: Posef
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (EventDataReferenceSpaceChangePending)
#endif
deriving instance Show EventDataReferenceSpaceChangePending

instance IsEventData EventDataReferenceSpaceChangePending where
  toEventDataBaseHeader EventDataReferenceSpaceChangePending{} = EventDataBaseHeader{type' = TYPE_EVENT_DATA_REFERENCE_SPACE_CHANGE_PENDING}

instance ToCStruct EventDataReferenceSpaceChangePending where
  withCStruct x f = allocaBytesAligned 72 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p EventDataReferenceSpaceChangePending{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_EVENT_DATA_REFERENCE_SPACE_CHANGE_PENDING)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Session_T))) (session)
    poke ((p `plusPtr` 24 :: Ptr ReferenceSpaceType)) (referenceSpaceType)
    poke ((p `plusPtr` 32 :: Ptr Time)) (changeTime)
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (poseValid))
    poke ((p `plusPtr` 44 :: Ptr Posef)) (poseInPreviousSpace)
    f
  cStructSize = 72
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_EVENT_DATA_REFERENCE_SPACE_CHANGE_PENDING)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Session_T))) (zero)
    poke ((p `plusPtr` 24 :: Ptr ReferenceSpaceType)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Time)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 44 :: Ptr Posef)) (zero)
    f

instance FromCStruct EventDataReferenceSpaceChangePending where
  peekCStruct p = do
    session <- peek @(Ptr Session_T) ((p `plusPtr` 16 :: Ptr (Ptr Session_T)))
    referenceSpaceType <- peek @ReferenceSpaceType ((p `plusPtr` 24 :: Ptr ReferenceSpaceType))
    changeTime <- peek @Time ((p `plusPtr` 32 :: Ptr Time))
    poseValid <- peek @Bool32 ((p `plusPtr` 40 :: Ptr Bool32))
    poseInPreviousSpace <- peekCStruct @Posef ((p `plusPtr` 44 :: Ptr Posef))
    pure $ EventDataReferenceSpaceChangePending
             session referenceSpaceType changeTime (bool32ToBool poseValid) poseInPreviousSpace

instance Storable EventDataReferenceSpaceChangePending where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero EventDataReferenceSpaceChangePending where
  zero = EventDataReferenceSpaceChangePending
           zero
           zero
           zero
           zero
           zero


-- | XrEventDataInteractionProfileChanged - Notifies the application than the
-- active interaction profile has changed
--
-- == Member Descriptions
--
-- = Description
--
-- The 'EventDataInteractionProfileChanged' event is sent to the
-- application to notify it that the active input form factor for one or
-- more top level user paths has changed. This event /must/ only be sent
-- for interaction profiles that the application indicated its support for
-- via 'OpenXR.Core10.Input.suggestInteractionProfileBindings'. This event
-- /must/ only be sent for running sessions.
--
-- The application /can/ call
-- 'OpenXR.Core10.Input.getCurrentInteractionProfile' if it wants to change
-- its own behavior based on the active hardware.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Session',
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'OpenXR.Core10.Input.getCurrentInteractionProfile',
-- 'OpenXR.Core10.Input.suggestInteractionProfileBindings'
data EventDataInteractionProfileChanged = EventDataInteractionProfileChanged
  { -- | @session@ is the 'OpenXR.Core10.Handles.Session' for which at least one
    -- of the interaction profiles for a top level path has changed.
    --
    -- #VUID-XrEventDataInteractionProfileChanged-session-parameter# @session@
    -- /must/ be a valid 'OpenXR.Core10.Handles.Session' handle
    session :: Ptr Session_T }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (EventDataInteractionProfileChanged)
#endif
deriving instance Show EventDataInteractionProfileChanged

instance IsEventData EventDataInteractionProfileChanged where
  toEventDataBaseHeader EventDataInteractionProfileChanged{} = EventDataBaseHeader{type' = TYPE_EVENT_DATA_INTERACTION_PROFILE_CHANGED}

instance ToCStruct EventDataInteractionProfileChanged where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p EventDataInteractionProfileChanged{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_EVENT_DATA_INTERACTION_PROFILE_CHANGED)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Session_T))) (session)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_EVENT_DATA_INTERACTION_PROFILE_CHANGED)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Session_T))) (zero)
    f

instance FromCStruct EventDataInteractionProfileChanged where
  peekCStruct p = do
    session <- peek @(Ptr Session_T) ((p `plusPtr` 16 :: Ptr (Ptr Session_T)))
    pure $ EventDataInteractionProfileChanged
             session

instance Storable EventDataInteractionProfileChanged where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero EventDataInteractionProfileChanged where
  zero = EventDataInteractionProfileChanged
           zero

