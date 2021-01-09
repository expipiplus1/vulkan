{-# language CPP #-}
-- No documentation found for Chapter "ObjectType"
module OpenXR.Core10.Enums.ObjectType  (ObjectType( OBJECT_TYPE_UNKNOWN
                                                  , OBJECT_TYPE_INSTANCE
                                                  , OBJECT_TYPE_SESSION
                                                  , OBJECT_TYPE_SWAPCHAIN
                                                  , OBJECT_TYPE_SPACE
                                                  , OBJECT_TYPE_ACTION_SET
                                                  , OBJECT_TYPE_ACTION
                                                  , OBJECT_TYPE_HAND_TRACKER_EXT
                                                  , OBJECT_TYPE_SPATIAL_ANCHOR_MSFT
                                                  , OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT
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

-- | XrObjectType - Specify an enumeration to track object handle types
--
-- = Description
--
-- The 'ObjectType' enumeration defines values, each of which corresponds
-- to a specific OpenXR handle type. These values /can/ be used to
-- associate debug information with a particular type of object through one
-- or more extensions.
--
-- The following table defines 'ObjectType' and OpenXR Handle
-- relationships:
--
-- +---------------------------------+-----------------------------------+
-- | 'ObjectType'                    | OpenXR Handle Type                |
-- +=================================+===================================+
-- | 'OBJECT_TYPE_UNKNOWN'           | Unknown\/Undefined Handle         |
-- +---------------------------------+-----------------------------------+
-- | 'OBJECT_TYPE_INSTANCE'          | 'OpenXR.Core10.Handles.Instance'  |
-- +---------------------------------+-----------------------------------+
-- | 'OBJECT_TYPE_SESSION'           | 'OpenXR.Core10.Handles.Session'   |
-- +---------------------------------+-----------------------------------+
-- | 'OBJECT_TYPE_SWAPCHAIN'         | 'OpenXR.Core10.Handles.Swapchain' |
-- +---------------------------------+-----------------------------------+
-- | 'OBJECT_TYPE_SPACE'             | 'OpenXR.Core10.Handles.Space'     |
-- +---------------------------------+-----------------------------------+
-- | 'OBJECT_TYPE_ACTION_SET'        | 'OpenXR.Core10.Handles.ActionSet' |
-- +---------------------------------+-----------------------------------+
-- | 'OBJECT_TYPE_ACTION'            | 'OpenXR.Core10.Handles.Action'    |
-- +---------------------------------+-----------------------------------+
--
-- = See Also
--
-- 'OpenXR.Extensions.XR_EXT_debug_utils.DebugUtilsObjectNameInfoEXT'
newtype ObjectType = ObjectType Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "XrObjectType" "XR_OBJECT_TYPE_UNKNOWN"
pattern OBJECT_TYPE_UNKNOWN                   = ObjectType 0
-- No documentation found for Nested "XrObjectType" "XR_OBJECT_TYPE_INSTANCE"
pattern OBJECT_TYPE_INSTANCE                  = ObjectType 1
-- No documentation found for Nested "XrObjectType" "XR_OBJECT_TYPE_SESSION"
pattern OBJECT_TYPE_SESSION                   = ObjectType 2
-- No documentation found for Nested "XrObjectType" "XR_OBJECT_TYPE_SWAPCHAIN"
pattern OBJECT_TYPE_SWAPCHAIN                 = ObjectType 3
-- No documentation found for Nested "XrObjectType" "XR_OBJECT_TYPE_SPACE"
pattern OBJECT_TYPE_SPACE                     = ObjectType 4
-- No documentation found for Nested "XrObjectType" "XR_OBJECT_TYPE_ACTION_SET"
pattern OBJECT_TYPE_ACTION_SET                = ObjectType 5
-- No documentation found for Nested "XrObjectType" "XR_OBJECT_TYPE_ACTION"
pattern OBJECT_TYPE_ACTION                    = ObjectType 6
-- No documentation found for Nested "XrObjectType" "XR_OBJECT_TYPE_HAND_TRACKER_EXT"
pattern OBJECT_TYPE_HAND_TRACKER_EXT          = ObjectType 1000051000
-- No documentation found for Nested "XrObjectType" "XR_OBJECT_TYPE_SPATIAL_ANCHOR_MSFT"
pattern OBJECT_TYPE_SPATIAL_ANCHOR_MSFT       = ObjectType 1000039000
-- No documentation found for Nested "XrObjectType" "XR_OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT"
pattern OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT = ObjectType 1000019000
{-# complete OBJECT_TYPE_UNKNOWN,
             OBJECT_TYPE_INSTANCE,
             OBJECT_TYPE_SESSION,
             OBJECT_TYPE_SWAPCHAIN,
             OBJECT_TYPE_SPACE,
             OBJECT_TYPE_ACTION_SET,
             OBJECT_TYPE_ACTION,
             OBJECT_TYPE_HAND_TRACKER_EXT,
             OBJECT_TYPE_SPATIAL_ANCHOR_MSFT,
             OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT :: ObjectType #-}

conNameObjectType :: String
conNameObjectType = "ObjectType"

enumPrefixObjectType :: String
enumPrefixObjectType = "OBJECT_TYPE_"

showTableObjectType :: [(ObjectType, String)]
showTableObjectType =
  [ (OBJECT_TYPE_UNKNOWN                  , "UNKNOWN")
  , (OBJECT_TYPE_INSTANCE                 , "INSTANCE")
  , (OBJECT_TYPE_SESSION                  , "SESSION")
  , (OBJECT_TYPE_SWAPCHAIN                , "SWAPCHAIN")
  , (OBJECT_TYPE_SPACE                    , "SPACE")
  , (OBJECT_TYPE_ACTION_SET               , "ACTION_SET")
  , (OBJECT_TYPE_ACTION                   , "ACTION")
  , (OBJECT_TYPE_HAND_TRACKER_EXT         , "HAND_TRACKER_EXT")
  , (OBJECT_TYPE_SPATIAL_ANCHOR_MSFT      , "SPATIAL_ANCHOR_MSFT")
  , (OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT, "DEBUG_UTILS_MESSENGER_EXT")
  ]

instance Show ObjectType where
  showsPrec =
    enumShowsPrec enumPrefixObjectType showTableObjectType conNameObjectType (\(ObjectType x) -> x) (showsPrec 11)

instance Read ObjectType where
  readPrec = enumReadPrec enumPrefixObjectType showTableObjectType conNameObjectType ObjectType

