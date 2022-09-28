{-# language CPP #-}
-- No documentation found for Chapter "Handles"
module OpenXR.Extensions.Handles  ( DebugUtilsMessengerEXT(..)
                                  , DebugUtilsMessengerEXT_T
                                  , SpatialAnchorMSFT(..)
                                  , SpatialAnchorMSFT_T
                                  , HandTrackerEXT(..)
                                  , HandTrackerEXT_T
                                  , Instance(..)
                                  , Session(..)
                                  , Action(..)
                                  , Swapchain(..)
                                  , Space(..)
                                  ) where

import Foreign.Ptr (ptrToWordPtr)
import Foreign.Ptr (pattern WordPtr)
import OpenXR.Zero (Zero(..))
import Foreign.Ptr (Ptr)
import OpenXR.Core10.APIConstants (HasObjectType(..))
import OpenXR.Dynamic (InstanceCmds)
import OpenXR.Core10.APIConstants (IsHandle)
import OpenXR.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT))
import OpenXR.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_HAND_TRACKER_EXT))
import OpenXR.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_SPATIAL_ANCHOR_MSFT))
import OpenXR.Core10.Handles (Action(..))
import OpenXR.Core10.Handles (Instance(..))
import OpenXR.Core10.Handles (Session(..))
import OpenXR.Core10.Handles (Space(..))
import OpenXR.Core10.Handles (Swapchain(..))
-- | An opaque type for representing pointers to XrDebugUtilsMessengerEXT handles
data DebugUtilsMessengerEXT_T
-- | XrDebugUtilsMessengerEXT - Callback for debug data
--
-- = Description
--
-- 'DebugUtilsMessengerEXT' represents a callback function and associated
-- filters registered with the runtime.
--
-- = See Also
--
-- 'OpenXR.Extensions.XR_EXT_debug_utils.createDebugUtilsMessengerEXT',
-- 'OpenXR.Extensions.XR_EXT_debug_utils.destroyDebugUtilsMessengerEXT'
data DebugUtilsMessengerEXT = DebugUtilsMessengerEXT
  { debugUtilsMessengerEXTHandle :: Ptr DebugUtilsMessengerEXT_T
  , instanceCmds :: InstanceCmds
  }
  deriving stock (Eq, Show)
  deriving anyclass (IsHandle)
instance Zero DebugUtilsMessengerEXT where
  zero = DebugUtilsMessengerEXT zero zero
instance HasObjectType DebugUtilsMessengerEXT where
  objectTypeAndHandle (DebugUtilsMessengerEXT (ptrToWordPtr -> WordPtr h) _) = ( OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT
                                                                               , fromIntegral h )


-- | An opaque type for representing pointers to XrSpatialAnchorMSFT handles
data SpatialAnchorMSFT_T
-- | XrSpatialAnchorMSFT - Represents a spatial anchor
--
-- = Description
--
-- Spatial anchors are often used in combination with an @UNBOUNDED_MSFT@
-- reference space. @UNBOUNDED_MSFT@ reference spaces adjust their origin
-- as necessary to keep the viewer’s coordinates relative to the space’s
-- origin stable. Such adjustments maintain the visual stability of content
-- currently near the viewer, but may cause content placed far from the
-- viewer to drift in its alignment to the real world by the time the user
-- moves close again. By creating an 'SpatialAnchorMSFT' where a piece of
-- content is placed and then always rendering that content relative to its
-- anchor’s space, an application can ensure that each piece of content
-- stays at a fixed location in the environment.
--
-- = See Also
--
-- 'OpenXR.Extensions.XR_MSFT_spatial_anchor.SpatialAnchorSpaceCreateInfoMSFT',
-- 'OpenXR.Extensions.XR_MSFT_perception_anchor_interop.createSpatialAnchorFromPerceptionAnchorMSFT',
-- 'OpenXR.Extensions.XR_MSFT_spatial_anchor.createSpatialAnchorMSFT',
-- 'OpenXR.Extensions.XR_MSFT_spatial_anchor.destroySpatialAnchorMSFT',
-- 'OpenXR.Extensions.XR_MSFT_perception_anchor_interop.tryGetPerceptionAnchorFromSpatialAnchorMSFT'
data SpatialAnchorMSFT = SpatialAnchorMSFT
  { spatialAnchorMSFTHandle :: Ptr SpatialAnchorMSFT_T
  , instanceCmds :: InstanceCmds
  }
  deriving stock (Eq, Show)
  deriving anyclass (IsHandle)
instance Zero SpatialAnchorMSFT where
  zero = SpatialAnchorMSFT zero zero
instance HasObjectType SpatialAnchorMSFT where
  objectTypeAndHandle (SpatialAnchorMSFT (ptrToWordPtr -> WordPtr h) _) = ( OBJECT_TYPE_SPATIAL_ANCHOR_MSFT
                                                                          , fromIntegral h )


-- | An opaque type for representing pointers to XrHandTrackerEXT handles
data HandTrackerEXT_T
-- | XrHandTrackerEXT - Represents a tracker for a hand joints.
--
-- = Description
--
-- An application creates separate 'HandTrackerEXT' handles for left and
-- right hands. This handle can be used to locate hand joints using
-- 'OpenXR.Extensions.XR_EXT_hand_tracking.locateHandJointsEXT' function.
--
-- A hand tracker provides accurate fidelity to the user’s actual hand
-- shape. When the hand tracking input requires the user to be holding a
-- controller in their hand, the hand tracking input will appear as the
-- user holding the controller. This input can be used to render the hand
-- shape together with rendering the controller in the hand.
--
-- = See Also
--
-- 'OpenXR.Extensions.XR_MSFT_hand_tracking_mesh.createHandMeshSpaceMSFT',
-- 'OpenXR.Extensions.XR_EXT_hand_tracking.createHandTrackerEXT',
-- 'OpenXR.Extensions.XR_EXT_hand_tracking.destroyHandTrackerEXT',
-- 'OpenXR.Extensions.XR_EXT_hand_tracking.locateHandJointsEXT',
-- 'OpenXR.Extensions.XR_MSFT_hand_tracking_mesh.updateHandMeshMSFT'
data HandTrackerEXT = HandTrackerEXT
  { handTrackerEXTHandle :: Ptr HandTrackerEXT_T
  , instanceCmds :: InstanceCmds
  }
  deriving stock (Eq, Show)
  deriving anyclass (IsHandle)
instance Zero HandTrackerEXT where
  zero = HandTrackerEXT zero zero
instance HasObjectType HandTrackerEXT where
  objectTypeAndHandle (HandTrackerEXT (ptrToWordPtr -> WordPtr h) _) = ( OBJECT_TYPE_HAND_TRACKER_EXT
                                                                       , fromIntegral h )

