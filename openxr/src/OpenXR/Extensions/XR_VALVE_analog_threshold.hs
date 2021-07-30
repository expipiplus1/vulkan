{-# language CPP #-}
-- | = Name
--
-- XR_VALVE_analog_threshold - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_VALVE_analog_threshold  XR_VALVE_analog_threshold>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 80
--
-- = Revision
--
-- 1
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- = See Also
--
-- 'InteractionProfileAnalogThresholdVALVE'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_VALVE_analog_threshold OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_VALVE_analog_threshold  ( InteractionProfileAnalogThresholdVALVE(..)
                                                    , VALVE_analog_threshold_SPEC_VERSION
                                                    , pattern VALVE_analog_threshold_SPEC_VERSION
                                                    , VALVE_ANALOG_THRESHOLD_EXTENSION_NAME
                                                    , pattern VALVE_ANALOG_THRESHOLD_EXTENSION_NAME
                                                    ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (maybePeek)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.Coerce (coerce)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
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
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import OpenXR.CStruct.Extends (withSomeChild)
import OpenXR.Core10.Handles (Action_T)
import OpenXR.Core10.Haptics (HapticBaseHeader)
import OpenXR.CStruct.Extends (Inheritable(peekSomeCChild))
import OpenXR.Core10.SemanticPaths (Path)
import OpenXR.CStruct.Extends (SomeChild)
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_INTERACTION_PROFILE_ANALOG_THRESHOLD_VALVE))
-- | XrInteractionProfileAnalogThresholdVALVE - Interaction profile dpad
-- binding
--
-- == Member Descriptions
--
-- = Description
--
-- Applications can chain an 'InteractionProfileAnalogThresholdVALVE'
-- struct on the next chain of any
-- 'OpenXR.Core10.Input.suggestInteractionProfileBindings' call for each
-- analog to boolean conversion for which it wants to set the threshold. If
-- a threshold struct is present for a given conversion, the runtime /must/
-- use those thresholds instead of applying its own whenever it is using
-- the binding suggested by the application.
--
-- @onThreshold@ and @offThreshold@ permit allow the application to specify
-- that it wants hysteresis to be applied to the threshold operation. If
-- @onThreshold@ is smaller than @offThreshold@, the runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'.
--
-- @onHaptic@ and @offHaptic@ allow the application to specify that it
-- wants automatic haptic feedback to be generated when the boolean output
-- of the threshold operation changes from false to true or vice versa. If
-- these fields are not NULL, the runtime /must/ trigger a haptic output
-- with the specified characteristics. If the device has multiple haptic
-- outputs, the runtime /should/ use the haptic output that is most
-- appropriate for the specified input path.
--
-- If a suggested binding with @action@ and @binding@ is not in the binding
-- list for this interaction profile, the runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_PATH_UNSUPPORTED'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrInteractionProfileAnalogThresholdVALVE-extension-notenabled#
--     The @XR_VALVE_analog_threshold@ extension /must/ be enabled prior to
--     using 'InteractionProfileAnalogThresholdVALVE'
--
-- -   #VUID-XrInteractionProfileAnalogThresholdVALVE-type-type# @type@
--     /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_INTERACTION_PROFILE_ANALOG_THRESHOLD_VALVE'
--
-- -   #VUID-XrInteractionProfileAnalogThresholdVALVE-next-next# @next@
--     /must/ be @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrInteractionProfileAnalogThresholdVALVE-action-parameter#
--     @action@ /must/ be a valid 'OpenXR.Core10.Handles.Action' handle
--
-- -   #VUID-XrInteractionProfileAnalogThresholdVALVE-onHaptic-parameter#
--     If @onHaptic@ is not @NULL@, @onHaptic@ /must/ be a pointer to a
--     valid 'OpenXR.Core10.Haptics.HapticBaseHeader'-based structure. See
--     also: 'OpenXR.Core10.OtherTypes.HapticVibration'
--
-- -   #VUID-XrInteractionProfileAnalogThresholdVALVE-offHaptic-parameter#
--     If @offHaptic@ is not @NULL@, @offHaptic@ /must/ be a pointer to a
--     valid 'OpenXR.Core10.Haptics.HapticBaseHeader'-based structure. See
--     also: 'OpenXR.Core10.OtherTypes.HapticVibration'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Action',
-- 'OpenXR.Core10.Haptics.HapticBaseHeader',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >,
-- 'OpenXR.Core10.Enums.StructureType.StructureType'
data InteractionProfileAnalogThresholdVALVE = InteractionProfileAnalogThresholdVALVE
  { -- | @action@ is the handle of an action in the suggested binding list.
    action :: Ptr Action_T
  , -- | @binding@ is the input path used for the specified action in the
    -- suggested binding list.
    binding :: Path
  , -- | @onThreshold@ is the value between 0.0 and 1.0 at which the runtime
    -- /must/ consider the binding to be true. The binding must remain true
    -- until the input analog value falls below @offThreshold@.
    onThreshold :: Float
  , -- | @offThreshold@ is the value between 0.0 and 1.0 at which the runtime
    -- /must/ consider the binding to be false if it was previous true.
    offThreshold :: Float
  , -- | @onHaptic@ is the haptic output that the runtime /must/ trigger when the
    -- binding changes from false to true. If this field is NULL, the runtime
    -- /must/ not trigger any haptic output on the threshold. This field /can/
    -- point to any supported sub-type of
    -- 'OpenXR.Core10.Haptics.HapticBaseHeader'.
    onHaptic :: Maybe (SomeChild HapticBaseHeader)
  , -- | @offHaptic@ is the haptic output that the runtime /must/ trigger when
    -- the binding changes from true to false. If this field is NULL, the
    -- runtime /must/ not trigger any haptic output on the threshold. This
    -- field /can/ point to any supported sub-type of
    -- 'OpenXR.Core10.Haptics.HapticBaseHeader'.
    offHaptic :: Maybe (SomeChild HapticBaseHeader)
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (InteractionProfileAnalogThresholdVALVE)
#endif
deriving instance Show InteractionProfileAnalogThresholdVALVE

instance ToCStruct InteractionProfileAnalogThresholdVALVE where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p InteractionProfileAnalogThresholdVALVE{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_INTERACTION_PROFILE_ANALOG_THRESHOLD_VALVE)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr Action_T))) (action)
    lift $ poke ((p `plusPtr` 24 :: Ptr Path)) (binding)
    lift $ poke ((p `plusPtr` 32 :: Ptr CFloat)) (CFloat (onThreshold))
    lift $ poke ((p `plusPtr` 36 :: Ptr CFloat)) (CFloat (offThreshold))
    onHaptic'' <- case (onHaptic) of
      Nothing -> pure nullPtr
      Just j -> ContT $ withSomeChild (j)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr _))) onHaptic''
    offHaptic'' <- case (offHaptic) of
      Nothing -> pure nullPtr
      Just j -> ContT $ withSomeChild (j)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr _))) offHaptic''
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_INTERACTION_PROFILE_ANALOG_THRESHOLD_VALVE)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Action_T))) (zero)
    poke ((p `plusPtr` 24 :: Ptr Path)) (zero)
    poke ((p `plusPtr` 32 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 36 :: Ptr CFloat)) (CFloat (zero))
    f

instance FromCStruct InteractionProfileAnalogThresholdVALVE where
  peekCStruct p = do
    action <- peek @(Ptr Action_T) ((p `plusPtr` 16 :: Ptr (Ptr Action_T)))
    binding <- peek @Path ((p `plusPtr` 24 :: Ptr Path))
    onThreshold <- peek @CFloat ((p `plusPtr` 32 :: Ptr CFloat))
    offThreshold <- peek @CFloat ((p `plusPtr` 36 :: Ptr CFloat))
    onHaptic <- peek @(Ptr _) ((p `plusPtr` 40 :: Ptr (Ptr _)))
    onHaptic' <- maybePeek (\j -> peekSomeCChild (j)) onHaptic
    offHaptic <- peek @(Ptr _) ((p `plusPtr` 48 :: Ptr (Ptr _)))
    offHaptic' <- maybePeek (\j -> peekSomeCChild (j)) offHaptic
    pure $ InteractionProfileAnalogThresholdVALVE
             action binding (coerce @CFloat @Float onThreshold) (coerce @CFloat @Float offThreshold) onHaptic' offHaptic'

instance Zero InteractionProfileAnalogThresholdVALVE where
  zero = InteractionProfileAnalogThresholdVALVE
           zero
           zero
           zero
           zero
           Nothing
           Nothing


type VALVE_analog_threshold_SPEC_VERSION = 1

-- No documentation found for TopLevel "XR_VALVE_analog_threshold_SPEC_VERSION"
pattern VALVE_analog_threshold_SPEC_VERSION :: forall a . Integral a => a
pattern VALVE_analog_threshold_SPEC_VERSION = 1


type VALVE_ANALOG_THRESHOLD_EXTENSION_NAME = "XR_VALVE_analog_threshold"

-- No documentation found for TopLevel "XR_VALVE_ANALOG_THRESHOLD_EXTENSION_NAME"
pattern VALVE_ANALOG_THRESHOLD_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern VALVE_ANALOG_THRESHOLD_EXTENSION_NAME = "XR_VALVE_analog_threshold"

