{-# language CPP #-}
-- | = Name
--
-- XR_EXT_hand_tracking - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_EXT_hand_tracking  XR_EXT_hand_tracking>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 52
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
-- 'OpenXR.Core10.APIConstants.HAND_JOINT_COUNT_EXT', 'HandEXT',
-- 'HandJointEXT', 'HandJointLocationEXT', 'HandJointLocationsEXT',
-- 'HandJointSetEXT', 'HandJointVelocitiesEXT', 'HandJointVelocityEXT',
-- 'HandJointsLocateInfoEXT', 'HandTrackerCreateInfoEXT',
-- 'SystemHandTrackingPropertiesEXT', 'createHandTrackerEXT',
-- 'destroyHandTrackerEXT', 'locateHandJointsEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_EXT_hand_tracking OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_EXT_hand_tracking  ( HandJointLocationEXT
                                               , HandJointLocationsEXT
                                               , HandJointVelocitiesEXT
                                               , HandJointVelocityEXT
                                               , HandJointsLocateInfoEXT
                                               , HandTrackerCreateInfoEXT
                                               , SystemHandTrackingPropertiesEXT
                                               ) where

import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} OpenXR.CStruct.Extends (Chain)
import {-# SOURCE #-} OpenXR.CStruct.Extends (Extendss)
import {-# SOURCE #-} OpenXR.CStruct.Extends (PeekChain)
import {-# SOURCE #-} OpenXR.CStruct.Extends (PokeChain)
data HandJointLocationEXT

instance ToCStruct HandJointLocationEXT
instance Show HandJointLocationEXT

instance FromCStruct HandJointLocationEXT


type role HandJointLocationsEXT nominal
data HandJointLocationsEXT (es :: [Type])

instance ( Extendss HandJointLocationsEXT es
         , PokeChain es ) => ToCStruct (HandJointLocationsEXT es)
instance Show (Chain es) => Show (HandJointLocationsEXT es)

instance ( Extendss HandJointLocationsEXT es
         , PeekChain es ) => FromCStruct (HandJointLocationsEXT es)


data HandJointVelocitiesEXT

instance ToCStruct HandJointVelocitiesEXT
instance Show HandJointVelocitiesEXT

instance FromCStruct HandJointVelocitiesEXT


data HandJointVelocityEXT

instance ToCStruct HandJointVelocityEXT
instance Show HandJointVelocityEXT

instance FromCStruct HandJointVelocityEXT


data HandJointsLocateInfoEXT

instance ToCStruct HandJointsLocateInfoEXT
instance Show HandJointsLocateInfoEXT

instance FromCStruct HandJointsLocateInfoEXT


type role HandTrackerCreateInfoEXT nominal
data HandTrackerCreateInfoEXT (es :: [Type])

instance ( Extendss HandTrackerCreateInfoEXT es
         , PokeChain es ) => ToCStruct (HandTrackerCreateInfoEXT es)
instance Show (Chain es) => Show (HandTrackerCreateInfoEXT es)

instance ( Extendss HandTrackerCreateInfoEXT es
         , PeekChain es ) => FromCStruct (HandTrackerCreateInfoEXT es)


data SystemHandTrackingPropertiesEXT

instance ToCStruct SystemHandTrackingPropertiesEXT
instance Show SystemHandTrackingPropertiesEXT

instance FromCStruct SystemHandTrackingPropertiesEXT

