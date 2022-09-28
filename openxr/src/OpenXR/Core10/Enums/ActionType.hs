{-# language CPP #-}
-- No documentation found for Chapter "ActionType"
module OpenXR.Core10.Enums.ActionType  (ActionType( ACTION_TYPE_BOOLEAN_INPUT
                                                  , ACTION_TYPE_FLOAT_INPUT
                                                  , ACTION_TYPE_VECTOR2F_INPUT
                                                  , ACTION_TYPE_POSE_INPUT
                                                  , ACTION_TYPE_VIBRATION_OUTPUT
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

-- | XrActionType - XrAction type
--
-- == Enumerant Descriptions
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Action', 'OpenXR.Core10.Input.ActionCreateInfo',
-- 'OpenXR.Core10.Input.createActionSet'
newtype ActionType = ActionType Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- Note that the zero instance does not produce a valid value, passing 'zero' to Vulkan will result in an error

-- | 'ACTION_TYPE_BOOLEAN_INPUT'. The action can be passed to
-- 'OpenXR.Core10.Input.getActionStateBoolean' to retrieve a boolean value.
pattern ACTION_TYPE_BOOLEAN_INPUT = ActionType 1

-- | 'ACTION_TYPE_FLOAT_INPUT'. The action can be passed to
-- 'OpenXR.Core10.Input.getActionStateFloat' to retrieve a float value.
pattern ACTION_TYPE_FLOAT_INPUT = ActionType 2

-- | 'ACTION_TYPE_VECTOR2F_INPUT'. The action can be passed to
-- 'OpenXR.Core10.Input.getActionStateVector2f' to retrieve a 2D float
-- vector.
pattern ACTION_TYPE_VECTOR2F_INPUT = ActionType 3

-- | 'ACTION_TYPE_POSE_INPUT'. The action can can be passed to
-- 'OpenXR.Core10.Space.createActionSpace' to create a space.
pattern ACTION_TYPE_POSE_INPUT = ActionType 4

-- | 'ACTION_TYPE_VIBRATION_OUTPUT'. The action can be passed to
-- 'OpenXR.Core10.Haptics.applyHapticFeedback' to send a haptic event to
-- the runtime.
pattern ACTION_TYPE_VIBRATION_OUTPUT = ActionType 100

{-# COMPLETE
  ACTION_TYPE_BOOLEAN_INPUT
  , ACTION_TYPE_FLOAT_INPUT
  , ACTION_TYPE_VECTOR2F_INPUT
  , ACTION_TYPE_POSE_INPUT
  , ACTION_TYPE_VIBRATION_OUTPUT ::
    ActionType
  #-}

conNameActionType :: String
conNameActionType = "ActionType"

enumPrefixActionType :: String
enumPrefixActionType = "ACTION_TYPE_"

showTableActionType :: [(ActionType, String)]
showTableActionType =
  [ (ACTION_TYPE_BOOLEAN_INPUT, "BOOLEAN_INPUT")
  , (ACTION_TYPE_FLOAT_INPUT, "FLOAT_INPUT")
  , (ACTION_TYPE_VECTOR2F_INPUT, "VECTOR2F_INPUT")
  , (ACTION_TYPE_POSE_INPUT, "POSE_INPUT")
  , (ACTION_TYPE_VIBRATION_OUTPUT, "VIBRATION_OUTPUT")
  ]

instance Show ActionType where
  showsPrec =
    enumShowsPrec
      enumPrefixActionType
      showTableActionType
      conNameActionType
      (\(ActionType x) -> x)
      (showsPrec 11)

instance Read ActionType where
  readPrec =
    enumReadPrec
      enumPrefixActionType
      showTableActionType
      conNameActionType
      ActionType
