{-# language CPP #-}
-- No documentation found for Chapter "FundamentalTypes"
module OpenXR.Core10.FundamentalTypes  ( boolToBool32
                                       , bool32ToBool
                                       , Offset2Df(..)
                                       , Extent2Df(..)
                                       , Rect2Df(..)
                                       , Offset2Di(..)
                                       , Extent2Di(..)
                                       , Rect2Di(..)
                                       , Bool32( FALSE
                                               , TRUE
                                               , ..
                                               )
                                       , Flags64
                                       , Time
                                       , Duration
                                       ) where

import OpenXR.Internal.Utils (enumReadPrec)
import OpenXR.Internal.Utils (enumShowsPrec)
import Data.Bool (bool)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
import Data.Coerce (coerce)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (FromCStruct(..))
import OpenXR.CStruct (ToCStruct)
import OpenXR.CStruct (ToCStruct(..))
import OpenXR.Zero (Zero)
import OpenXR.Zero (Zero(..))
import Data.Typeable (Typeable)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(..))
import Foreign.C.Types (CFloat(CFloat))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Data.Int (Int32)
import Data.Int (Int64)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word64)
import Data.Kind (Type)

boolToBool32 :: Bool -> Bool32
boolToBool32 = bool FALSE TRUE

bool32ToBool :: Bool32 -> Bool
bool32ToBool = \case
  FALSE -> False
  TRUE  -> True


-- | XrOffset2Df - Float offset in two dimensions
--
-- == Member Descriptions
--
-- = Description
--
-- This structure is used for component values that may be fractional
-- (floating-point). If used to represent physical distances, values /must/
-- be in meters.
--
-- = See Also
--
-- 'Extent2Df', 'Rect2Df'
data Offset2Df = Offset2Df
  { -- | @x@ the floating-point offset in the x direction.
    x :: Float
  , -- | @y@ the floating-point offset in the y direction.
    y :: Float
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (Offset2Df)
#endif
deriving instance Show Offset2Df

instance ToCStruct Offset2Df where
  withCStruct x f = allocaBytesAligned 8 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p Offset2Df{..} f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (x))
    poke ((p `plusPtr` 4 :: Ptr CFloat)) (CFloat (y))
    f
  cStructSize = 8
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 4 :: Ptr CFloat)) (CFloat (zero))
    f

instance FromCStruct Offset2Df where
  peekCStruct p = do
    x <- peek @CFloat ((p `plusPtr` 0 :: Ptr CFloat))
    y <- peek @CFloat ((p `plusPtr` 4 :: Ptr CFloat))
    pure $ Offset2Df
             (coerce @CFloat @Float x) (coerce @CFloat @Float y)

instance Storable Offset2Df where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero Offset2Df where
  zero = Offset2Df
           zero
           zero


-- | XrExtent2Df - Extent in two dimensions
--
-- == Member Descriptions
--
-- = Description
--
-- This structure is used for component values that may be fractional
-- (floating-point). If used to represent physical distances, values /must/
-- be in meters.
--
-- The @width@ and @height@ value /must/ be non-negative.
--
-- = See Also
--
-- 'OpenXR.Core10.OtherTypes.CompositionLayerQuad', 'Offset2Df', 'Rect2Df',
-- 'OpenXR.Core10.Space.getReferenceSpaceBoundsRect'
data Extent2Df = Extent2Df
  { -- | @width@ the floating-point width of the extent.
    width :: Float
  , -- | @height@ the floating-point height of the extent.
    height :: Float
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (Extent2Df)
#endif
deriving instance Show Extent2Df

instance ToCStruct Extent2Df where
  withCStruct x f = allocaBytesAligned 8 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p Extent2Df{..} f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (width))
    poke ((p `plusPtr` 4 :: Ptr CFloat)) (CFloat (height))
    f
  cStructSize = 8
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 4 :: Ptr CFloat)) (CFloat (zero))
    f

instance FromCStruct Extent2Df where
  peekCStruct p = do
    width <- peek @CFloat ((p `plusPtr` 0 :: Ptr CFloat))
    height <- peek @CFloat ((p `plusPtr` 4 :: Ptr CFloat))
    pure $ Extent2Df
             (coerce @CFloat @Float width) (coerce @CFloat @Float height)

instance Storable Extent2Df where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero Extent2Df where
  zero = Extent2Df
           zero
           zero


-- | XrRect2Df - Rect in two dimensions
--
-- == Member Descriptions
--
-- = Description
--
-- This structure is used for component values that may be fractional
-- (floating-point).
--
-- = See Also
--
-- 'Extent2Df', 'Offset2Df'
data Rect2Df = Rect2Df
  { -- | @offset@ is the 'Offset2Df' specifying the rectangle offset.
    offset :: Offset2Df
  , -- | @extent@ is the 'Extent2Df' specifying the rectangle extent.
    extent :: Extent2Df
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (Rect2Df)
#endif
deriving instance Show Rect2Df

instance ToCStruct Rect2Df where
  withCStruct x f = allocaBytesAligned 16 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p Rect2Df{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Offset2Df)) (offset)
    poke ((p `plusPtr` 8 :: Ptr Extent2Df)) (extent)
    f
  cStructSize = 16
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Offset2Df)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Extent2Df)) (zero)
    f

instance FromCStruct Rect2Df where
  peekCStruct p = do
    offset <- peekCStruct @Offset2Df ((p `plusPtr` 0 :: Ptr Offset2Df))
    extent <- peekCStruct @Extent2Df ((p `plusPtr` 8 :: Ptr Extent2Df))
    pure $ Rect2Df
             offset extent

instance Storable Rect2Df where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero Rect2Df where
  zero = Rect2Df
           zero
           zero


-- | XrOffset2Di - Offset in two dimensions
--
-- == Member Descriptions
--
-- = Description
--
-- This variant is for representing discrete values such as texels. For
-- representing physical distances, the floating-point variant /must/ be
-- used instead.
--
-- = See Also
--
-- 'Extent2Di', 'Rect2Di'
data Offset2Di = Offset2Di
  { -- | @x@ the integer offset in the x direction.
    x :: Int32
  , -- | @y@ the integer offset in the y direction.
    y :: Int32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (Offset2Di)
#endif
deriving instance Show Offset2Di

instance ToCStruct Offset2Di where
  withCStruct x f = allocaBytesAligned 8 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p Offset2Di{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Int32)) (x)
    poke ((p `plusPtr` 4 :: Ptr Int32)) (y)
    f
  cStructSize = 8
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Int32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Int32)) (zero)
    f

instance FromCStruct Offset2Di where
  peekCStruct p = do
    x <- peek @Int32 ((p `plusPtr` 0 :: Ptr Int32))
    y <- peek @Int32 ((p `plusPtr` 4 :: Ptr Int32))
    pure $ Offset2Di
             x y

instance Storable Offset2Di where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero Offset2Di where
  zero = Offset2Di
           zero
           zero


-- | XrExtent2Di - Extent in two dimensions
--
-- == Member Descriptions
--
-- = Description
--
-- This variant is for representing discrete values such as texels. For
-- representing physical distances, the floating-point variant /must/ be
-- used instead.
--
-- The @width@ and @height@ value /must/ be non-negative.
--
-- = See Also
--
-- 'Offset2Di', 'Rect2Di'
data Extent2Di = Extent2Di
  { -- | @width@ the integer width of the extent.
    width :: Int32
  , -- | @height@ the integer height of the extent.
    height :: Int32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (Extent2Di)
#endif
deriving instance Show Extent2Di

instance ToCStruct Extent2Di where
  withCStruct x f = allocaBytesAligned 8 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p Extent2Di{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Int32)) (width)
    poke ((p `plusPtr` 4 :: Ptr Int32)) (height)
    f
  cStructSize = 8
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Int32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Int32)) (zero)
    f

instance FromCStruct Extent2Di where
  peekCStruct p = do
    width <- peek @Int32 ((p `plusPtr` 0 :: Ptr Int32))
    height <- peek @Int32 ((p `plusPtr` 4 :: Ptr Int32))
    pure $ Extent2Di
             width height

instance Storable Extent2Di where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero Extent2Di where
  zero = Extent2Di
           zero
           zero


-- | XrRect2Di - Rect in two dimensions
--
-- == Member Descriptions
--
-- = Description
--
-- This variant is for representing discrete values such as texels. For
-- representing physical distances, the floating-point variant /must/ be
-- used instead.
--
-- = See Also
--
-- 'Extent2Di', 'Offset2Di', 'OpenXR.Core10.OtherTypes.SwapchainSubImage'
data Rect2Di = Rect2Di
  { -- | @offset@ is the 'Offset2Di' specifying the integer rectangle offset.
    offset :: Offset2Di
  , -- | @extent@ is the 'Extent2Di' specifying the integer rectangle extent.
    extent :: Extent2Di
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (Rect2Di)
#endif
deriving instance Show Rect2Di

instance ToCStruct Rect2Di where
  withCStruct x f = allocaBytesAligned 16 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p Rect2Di{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Offset2Di)) (offset)
    poke ((p `plusPtr` 8 :: Ptr Extent2Di)) (extent)
    f
  cStructSize = 16
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Offset2Di)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Extent2Di)) (zero)
    f

instance FromCStruct Rect2Di where
  peekCStruct p = do
    offset <- peekCStruct @Offset2Di ((p `plusPtr` 0 :: Ptr Offset2Di))
    extent <- peekCStruct @Extent2Di ((p `plusPtr` 8 :: Ptr Extent2Di))
    pure $ Rect2Di
             offset extent

instance Storable Rect2Di where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero Rect2Di where
  zero = Rect2Di
           zero
           zero


-- | XrBool32 - Boolean value
--
-- = Description
--
-- Boolean values used by OpenXR are of type
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrBool32 >
-- and are 32-bits wide as suggested by the name. The only valid values are
-- the following:
--
-- == Enumerant Descriptions
--
-- = See Also
--
-- 'OpenXR.Core10.Input.ActionStateBoolean',
-- 'OpenXR.Core10.Input.ActionStateFloat',
-- 'OpenXR.Core10.Input.ActionStatePose',
-- 'OpenXR.Core10.Input.ActionStateVector2f',
-- 'OpenXR.Extensions.XR_EXTX_overlay.EventDataMainSessionVisibilityChangedEXTX',
-- 'OpenXR.Core10.OtherTypes.EventDataReferenceSpaceChangePending',
-- 'OpenXR.Core10.DisplayTiming.FrameState',
-- 'OpenXR.Extensions.XR_EXT_hand_tracking.HandJointLocationsEXT',
-- 'OpenXR.Extensions.XR_MSFT_hand_tracking_mesh.HandMeshMSFT',
-- 'OpenXR.Extensions.XR_MSFT_secondary_view_configuration.SecondaryViewConfigurationStateMSFT',
-- 'OpenXR.Extensions.XR_EXT_eye_gaze_interaction.SystemEyeGazeInteractionPropertiesEXT',
-- 'OpenXR.Extensions.XR_MSFT_hand_tracking_mesh.SystemHandTrackingMeshPropertiesMSFT',
-- 'OpenXR.Extensions.XR_EXT_hand_tracking.SystemHandTrackingPropertiesEXT',
-- 'OpenXR.Core10.Device.SystemTrackingProperties',
-- 'OpenXR.Core10.ViewConfigurations.ViewConfigurationProperties',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#xrSetInputDeviceActiveEXT xrSetInputDeviceActiveEXT>,
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#xrSetInputDeviceStateBoolEXT xrSetInputDeviceStateBoolEXT>
newtype Bool32 = Bool32 Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'FALSE' represents a false value.
pattern FALSE = Bool32 0
-- | 'TRUE' represents a true value.
pattern TRUE  = Bool32 1
{-# complete FALSE,
             TRUE :: Bool32 #-}

conNameBool32 :: String
conNameBool32 = "Bool32"

enumPrefixBool32 :: String
enumPrefixBool32 = ""

showTableBool32 :: [(Bool32, String)]
showTableBool32 = [(FALSE, "FALSE"), (TRUE, "TRUE")]

instance Show Bool32 where
  showsPrec = enumShowsPrec enumPrefixBool32 showTableBool32 conNameBool32 (\(Bool32 x) -> x) (showsPrec 11)

instance Read Bool32 where
  readPrec = enumReadPrec enumPrefixBool32 showTableBool32 conNameBool32 Bool32


-- | XrFlags64 - OpenXR bitmasks
--
-- = Description
--
-- Bitmasks are passed to many functions and structures to compactly
-- represent options and are stored in memory defined by the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrFlags64 >
-- type. But the API does not use the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrFlags64 >
-- type directly. Instead, a @Xr*Flags@ type is used which is an alias of
-- the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrFlags64 >
-- type. The API also defines a set of constant bit definitions used to set
-- the bitmasks.
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.CompositionLayerFlagBits.CompositionLayerFlags',
-- 'OpenXR.Core10.Enums.InstanceCreateFlagBits.InstanceCreateFlags',
-- 'OpenXR.Core10.Enums.SessionCreateFlagBits.SessionCreateFlags',
-- 'OpenXR.Core10.Enums.SpaceLocationFlagBits.SpaceLocationFlags',
-- 'OpenXR.Core10.Enums.SwapchainCreateFlagBits.SwapchainCreateFlags',
-- 'OpenXR.Core10.Enums.SwapchainUsageFlagBits.SwapchainUsageFlags',
-- 'OpenXR.Core10.Enums.ViewStateFlagBits.ViewStateFlags'
type Flags64 = Word64


-- | XrTime - Basic type for time
--
-- = Description
--
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >
-- is a base value type that represents time as a signed 64-bit integer,
-- representing the monotonically-increasing count of nanoseconds that have
-- elapsed since a runtime-chosen epoch.
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >
-- always represents the time elasped since that constant epoch, rather
-- than a duration or a time point relative to some moving epoch such as
-- vsync time, etc. Durations are instead represented by
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrDuration >.
--
-- A single runtime /must/ use the same epoch for all simultaneous
-- applications. Time /must/ be represented the same regardless of multiple
-- processors or threads present in the system.
--
-- The period precision of time reported by the runtime is
-- runtime-dependent, and /may/ change. One nanosecond is the finest
-- possible period precision. A runtime /may/, for example, report time
-- progression with only microsecond-level granularity.
--
-- Time /must/ not be assumed to correspond to a system clock time.
--
-- Unless specified otherwise, zero or a negative value is not a valid
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >,
-- and related functions /must/ return error
-- 'OpenXR.Core10.Enums.Result.ERROR_TIME_INVALID'. Applications /must/ not
-- initialize such
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >
-- fields to a zero value. Instead, applications /should/ always assign
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >
-- fields to the meaningful point in time they are choosing to reason
-- about, such as a frame’s predicted display time, or an action’s last
-- change time.
--
-- The behavior of a runtime is undefined when time overflows beyond the
-- maximum positive value that can be represented by an
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >.
-- Runtimes /should/ choose an epoch that minimizes the chance of overflow.
-- Runtimes /should/ also choose an epoch that minimizes the chance of
-- underflow below 0 for applications performing a reasonable amount of
-- historical pose lookback. For example, if the runtime chooses an epoch
-- relative to its startup time, it /should/ push the epoch into the past
-- by enough time to avoid applications performing reasonable pose lookback
-- from reaching a negative
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >
-- value.
--
-- An application cannot assume that the system’s clock and the runtime’s
-- clock will maintain a constant relationship across frames and /should/
-- avoid storing such an offset, as this may cause time drift. Applications
-- /should/ instead always use time interop functions to convert a relevant
-- time point across the system’s clock and the runtime’s clock using
-- extensions, for example,
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_win32_convert_performance_counter_time>
-- or
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_convert_timespec_time>.
--
-- = See Also
--
-- 'OpenXR.Core10.Input.ActionStateBoolean',
-- 'OpenXR.Core10.Input.ActionStateFloat',
-- 'OpenXR.Core10.Input.ActionStateVector2f',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrDuration >,
-- 'OpenXR.Core10.OtherTypes.EventDataInstanceLossPending',
-- 'OpenXR.Core10.OtherTypes.EventDataReferenceSpaceChangePending',
-- 'OpenXR.Core10.OtherTypes.EventDataSessionStateChanged',
-- 'OpenXR.Extensions.XR_EXT_eye_gaze_interaction.EyeGazeSampleTimeEXT',
-- 'OpenXR.Core10.DisplayTiming.FrameEndInfo',
-- 'OpenXR.Core10.DisplayTiming.FrameState',
-- 'OpenXR.Extensions.XR_EXT_hand_tracking.HandJointsLocateInfoEXT',
-- 'OpenXR.Extensions.XR_MSFT_hand_tracking_mesh.HandMeshUpdateInfoMSFT',
-- 'OpenXR.Extensions.XR_MSFT_hand_tracking_mesh.HandMeshVertexBufferMSFT',
-- 'OpenXR.Extensions.XR_MSFT_spatial_anchor.SpatialAnchorCreateInfoMSFT',
-- 'OpenXR.Core10.DisplayTiming.ViewLocateInfo',
-- 'OpenXR.Extensions.XR_KHR_convert_timespec_time.convertTimeToTimespecTimeKHR',
-- 'OpenXR.Extensions.XR_KHR_win32_convert_performance_counter_time.convertTimeToWin32PerformanceCounterKHR',
-- 'OpenXR.Extensions.XR_KHR_convert_timespec_time.convertTimespecTimeToTimeKHR',
-- 'OpenXR.Extensions.XR_KHR_win32_convert_performance_counter_time.convertWin32PerformanceCounterToTimeKHR',
-- 'OpenXR.Core10.Space.locateSpace'
type Time = Int64


-- | XrDuration - Bounded range of time
--
-- = Description
--
-- The difference between two timepoints is a duration, and thus the
-- difference between two
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >
-- values is an
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrDuration >
-- value.
--
-- Functions that refer to durations use
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrDuration >
-- as opposed to
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >.
--
-- = See Also
--
-- 'OpenXR.Core10.DisplayTiming.FrameState',
-- 'OpenXR.Core10.OtherTypes.HapticVibration',
-- 'OpenXR.Core10.Image.SwapchainImageWaitInfo',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >
type Duration = Int64

