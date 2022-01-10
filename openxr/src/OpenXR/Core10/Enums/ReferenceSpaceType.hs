{-# language CPP #-}
-- No documentation found for Chapter "ReferenceSpaceType"
module OpenXR.Core10.Enums.ReferenceSpaceType  (ReferenceSpaceType( REFERENCE_SPACE_TYPE_VIEW
                                                                  , REFERENCE_SPACE_TYPE_LOCAL
                                                                  , REFERENCE_SPACE_TYPE_STAGE
                                                                  , REFERENCE_SPACE_TYPE_UNBOUNDED_MSFT
                                                                  , ..
                                                                  )) where

import OpenXR.Internal.Utils (enumReadPrec)
import OpenXR.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import OpenXR.Zero (Zero)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))

-- | XrReferenceSpaceType - Reference space types
--
-- = Description
--
-- Available reference space types are indicated by
-- 'OpenXR.Core10.Space.enumerateReferenceSpaces'. Note that other spaces
-- can be created as well, such as pose action spaces created by
-- 'OpenXR.Core10.Space.createActionSpace', which are not enumerated by
-- that API.
--
-- == Enumerant Descriptions
--
-- = See Also
--
-- 'OpenXR.Core10.OtherTypes.EventDataReferenceSpaceChangePending',
-- 'OpenXR.Core10.Space.ReferenceSpaceCreateInfo',
-- 'OpenXR.Core10.Space.enumerateReferenceSpaces',
-- 'OpenXR.Core10.Space.getReferenceSpaceBoundsRect'
newtype ReferenceSpaceType = ReferenceSpaceType Int32
  deriving newtype (Eq, Ord, Storable, Zero)
-- Note that the zero instance does not produce a valid value, passing 'zero' to Vulkan will result in an error

-- | 'REFERENCE_SPACE_TYPE_VIEW'. The @VIEW@ space tracks the view origin
-- used to generate view transforms for the primary viewer (or centroid of
-- view origins if stereo), with +Y up, +X to the right, and -Z forward.
-- This space points in the forward direction for the viewer without
-- incorporating the user’s eye orientation, and is not gravity-aligned.
--
-- @VIEW@ space is primarily useful when projecting from the user’s
-- perspective into another space to obtain a targeting ray, or when
-- rendering small head-locked content such as a reticle. Content rendered
-- in @VIEW@ space will stay at a fixed point on head-mounted displays and
-- may be uncomfortable to view if too large. To obtain the ideal view and
-- projection transforms to use each frame for rendering world content,
-- applications should call 'OpenXR.Core10.DisplayTiming.locateViews'
-- instead of using this space.
--
-- Runtimes /must/ support this reference space.
pattern REFERENCE_SPACE_TYPE_VIEW           = ReferenceSpaceType 1
-- | 'REFERENCE_SPACE_TYPE_LOCAL'. The @LOCAL@ reference space establishes a
-- world-locked origin, gravity-aligned to exclude pitch and roll, with +Y
-- up, +X to the right, and -Z forward. This space locks in both its
-- initial position and orientation, which the runtime /may/ define to be
-- either the initial position at application launch or some other
-- calibrated zero position.
--
-- @LOCAL@ space is useful when an application needs to render
-- __seated-scale__ content that is not positioned relative to the physical
-- floor.
--
-- When a user needs to recenter @LOCAL@ space, a runtime /may/ offer some
-- system-level recentering interaction that is transparent to the
-- application, but which causes the current leveled head space to become
-- the new @LOCAL@ space. When such a recentering occurs, the runtime
-- /must/ queue the
-- 'OpenXR.Core10.OtherTypes.EventDataReferenceSpaceChangePending' event,
-- with the recentered @LOCAL@ space origin only taking effect for
-- 'OpenXR.Core10.Space.locateSpace' or
-- 'OpenXR.Core10.DisplayTiming.locateViews' calls whose
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >
-- parameter is greater than or equal to the @changeTime@ provided in that
-- event.
--
-- When views, controllers or other spaces experience tracking loss
-- relative to the @LOCAL@ space, runtimes /should/ continue to provide
-- inferred or last-known @position@ and @orientation@ values. These
-- inferred poses can, for example, be based on neck model updates,
-- inertial dead reckoning, or a last-known position, so long as it is
-- still reasonable for the application to use that pose. While a runtime
-- is providing position data, it /must/ continue to set
-- 'OpenXR.Core10.Enums.SpaceLocationFlagBits.SPACE_LOCATION_POSITION_VALID_BIT'
-- and
-- 'OpenXR.Core10.Enums.ViewStateFlagBits.VIEW_STATE_POSITION_VALID_BIT'
-- but it /can/ clear
-- 'OpenXR.Core10.Enums.SpaceLocationFlagBits.SPACE_LOCATION_POSITION_TRACKED_BIT'
-- and
-- 'OpenXR.Core10.Enums.ViewStateFlagBits.VIEW_STATE_POSITION_TRACKED_BIT'
-- to indicate that the position is inferred or last-known in this way.
--
-- When tracking is recovered, runtimes /should/ snap the pose of other
-- spaces back into position relative to the @LOCAL@ space’s original
-- origin.
--
-- Runtimes /must/ support this reference space.
pattern REFERENCE_SPACE_TYPE_LOCAL          = ReferenceSpaceType 2
-- | 'REFERENCE_SPACE_TYPE_STAGE'. The @STAGE@ reference space is a
-- runtime-defined flat, rectangular space that is empty and can be walked
-- around on. The origin is on the floor at the center of the rectangle,
-- with +Y up, and the X and Z axes aligned with the rectangle edges. The
-- runtime /may/ not be able to locate spaces relative to the @STAGE@
-- reference space if the user has not yet defined one within the
-- runtime-specific UI. Applications can use
-- 'OpenXR.Core10.Space.getReferenceSpaceBoundsRect' to determine the
-- extents of the @STAGE@ reference space’s XZ bounds rectangle, if
-- defined.
--
-- @STAGE@ space is useful when an application needs to render
-- __standing-scale__ content (no bounds) or __room-scale__ content (with
-- bounds) that is relative to the physical floor.
--
-- When the user redefines the origin or bounds of the current @STAGE@
-- space, or the runtime otherwise switches to a new @STAGE@ definition,
-- the runtime /must/ queue the
-- 'OpenXR.Core10.OtherTypes.EventDataReferenceSpaceChangePending' event,
-- with the new @STAGE@ space origin only taking effect for
-- 'OpenXR.Core10.Space.locateSpace' or
-- 'OpenXR.Core10.DisplayTiming.locateViews' calls whose
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >
-- parameter is greater than or equal to the @changeTime@ provided in that
-- event.
--
-- When views, controllers or other spaces experience tracking loss
-- relative to the @STAGE@ space, runtimes /should/ continue to provide
-- inferred or last-known @position@ and @orientation@ values. These
-- inferred poses can, for example, be based on neck model updates,
-- inertial dead reckoning, or a last-known position, so long as it is
-- still reasonable for the application to use that pose. While a runtime
-- is providing position data, it /must/ continue to set
-- 'OpenXR.Core10.Enums.SpaceLocationFlagBits.SPACE_LOCATION_POSITION_VALID_BIT'
-- and
-- 'OpenXR.Core10.Enums.ViewStateFlagBits.VIEW_STATE_POSITION_VALID_BIT'
-- but it /can/ clear
-- 'OpenXR.Core10.Enums.SpaceLocationFlagBits.SPACE_LOCATION_POSITION_TRACKED_BIT'
-- and
-- 'OpenXR.Core10.Enums.ViewStateFlagBits.VIEW_STATE_POSITION_TRACKED_BIT'
-- to indicate that the position is inferred or last-known in this way.
--
-- When tracking is recovered, runtimes /should/ snap the pose of other
-- spaces back into position relative to the @STAGE@ space’s original
-- origin.
pattern REFERENCE_SPACE_TYPE_STAGE          = ReferenceSpaceType 3
-- No documentation found for Nested "XrReferenceSpaceType" "XR_REFERENCE_SPACE_TYPE_UNBOUNDED_MSFT"
pattern REFERENCE_SPACE_TYPE_UNBOUNDED_MSFT = ReferenceSpaceType 1000038000
{-# complete REFERENCE_SPACE_TYPE_VIEW,
             REFERENCE_SPACE_TYPE_LOCAL,
             REFERENCE_SPACE_TYPE_STAGE,
             REFERENCE_SPACE_TYPE_UNBOUNDED_MSFT :: ReferenceSpaceType #-}

conNameReferenceSpaceType :: String
conNameReferenceSpaceType = "ReferenceSpaceType"

enumPrefixReferenceSpaceType :: String
enumPrefixReferenceSpaceType = "REFERENCE_SPACE_TYPE_"

showTableReferenceSpaceType :: [(ReferenceSpaceType, String)]
showTableReferenceSpaceType =
  [ (REFERENCE_SPACE_TYPE_VIEW          , "VIEW")
  , (REFERENCE_SPACE_TYPE_LOCAL         , "LOCAL")
  , (REFERENCE_SPACE_TYPE_STAGE         , "STAGE")
  , (REFERENCE_SPACE_TYPE_UNBOUNDED_MSFT, "UNBOUNDED_MSFT")
  ]

instance Show ReferenceSpaceType where
  showsPrec = enumShowsPrec enumPrefixReferenceSpaceType
                            showTableReferenceSpaceType
                            conNameReferenceSpaceType
                            (\(ReferenceSpaceType x) -> x)
                            (showsPrec 11)

instance Read ReferenceSpaceType where
  readPrec =
    enumReadPrec enumPrefixReferenceSpaceType showTableReferenceSpaceType conNameReferenceSpaceType ReferenceSpaceType

