{-# language CPP #-}
-- | = Name
--
-- XR_EXT_eye_gaze_interaction - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_EXT_eye_gaze_interaction  XR_EXT_eye_gaze_interaction>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 31
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
-- 'EyeGazeSampleTimeEXT', 'SystemEyeGazeInteractionPropertiesEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_EXT_eye_gaze_interaction OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_EXT_eye_gaze_interaction  ( SystemEyeGazeInteractionPropertiesEXT(..)
                                                      , EyeGazeSampleTimeEXT(..)
                                                      , EXT_eye_gaze_interaction_SPEC_VERSION
                                                      , pattern EXT_eye_gaze_interaction_SPEC_VERSION
                                                      , EXT_EYE_GAZE_INTERACTION_EXTENSION_NAME
                                                      , pattern EXT_EYE_GAZE_INTERACTION_EXTENSION_NAME
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
import Data.Kind (Type)
import OpenXR.Core10.FundamentalTypes (bool32ToBool)
import OpenXR.Core10.FundamentalTypes (boolToBool32)
import OpenXR.Core10.FundamentalTypes (Bool32)
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.Core10.FundamentalTypes (Time)
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_EYE_GAZE_SAMPLE_TIME_EXT))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_SYSTEM_EYE_GAZE_INTERACTION_PROPERTIES_EXT))
-- | XrSystemEyeGazeInteractionPropertiesEXT - Eye gaze interaction system
-- properties
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrSystemEyeGazeInteractionPropertiesEXT-extension-notenabled#
--     The @@ extension /must/ be enabled prior to using
--     'SystemEyeGazeInteractionPropertiesEXT'
--
-- -   #VUID-XrSystemEyeGazeInteractionPropertiesEXT-type-type# @type@
--     /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_SYSTEM_EYE_GAZE_INTERACTION_PROPERTIES_EXT'
--
-- -   #VUID-XrSystemEyeGazeInteractionPropertiesEXT-next-next# @next@
--     /must/ be @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- = See Also
--
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrBool32 >,
-- 'OpenXR.Core10.Enums.StructureType.StructureType'
data SystemEyeGazeInteractionPropertiesEXT = SystemEyeGazeInteractionPropertiesEXT
  { -- | @supportsEyeGazeInteraction@ the runtime /must/ set this value to
    -- 'OpenXR.Core10.FundamentalTypes.TRUE' when eye gaze sufficient for use
    -- cases such as aiming or targeting is supported by the current device,
    -- otherwise the runtime /must/ set this to
    -- 'OpenXR.Core10.FundamentalTypes.FALSE'.
    supportsEyeGazeInteraction :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SystemEyeGazeInteractionPropertiesEXT)
#endif
deriving instance Show SystemEyeGazeInteractionPropertiesEXT

instance ToCStruct SystemEyeGazeInteractionPropertiesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SystemEyeGazeInteractionPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SYSTEM_EYE_GAZE_INTERACTION_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (supportsEyeGazeInteraction))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SYSTEM_EYE_GAZE_INTERACTION_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct SystemEyeGazeInteractionPropertiesEXT where
  peekCStruct p = do
    supportsEyeGazeInteraction <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ SystemEyeGazeInteractionPropertiesEXT
             (bool32ToBool supportsEyeGazeInteraction)

instance Storable SystemEyeGazeInteractionPropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SystemEyeGazeInteractionPropertiesEXT where
  zero = SystemEyeGazeInteractionPropertiesEXT
           zero


-- | XrEyeGazeSampleTimeEXT - Eye gaze sample time structure
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrEyeGazeSampleTimeEXT-extension-notenabled# The @@ extension
--     /must/ be enabled prior to using 'EyeGazeSampleTimeEXT'
--
-- -   #VUID-XrEyeGazeSampleTimeEXT-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_EYE_GAZE_SAMPLE_TIME_EXT'
--
-- -   #VUID-XrEyeGazeSampleTimeEXT-next-next# @next@ /must/ be @NULL@ or a
--     valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >
data EyeGazeSampleTimeEXT = EyeGazeSampleTimeEXT
  { -- | @time@ is when in time the eye gaze pose is expressed.
    time :: Time }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (EyeGazeSampleTimeEXT)
#endif
deriving instance Show EyeGazeSampleTimeEXT

instance ToCStruct EyeGazeSampleTimeEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p EyeGazeSampleTimeEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_EYE_GAZE_SAMPLE_TIME_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Time)) (time)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_EYE_GAZE_SAMPLE_TIME_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Time)) (zero)
    f

instance FromCStruct EyeGazeSampleTimeEXT where
  peekCStruct p = do
    time <- peek @Time ((p `plusPtr` 16 :: Ptr Time))
    pure $ EyeGazeSampleTimeEXT
             time

instance Storable EyeGazeSampleTimeEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero EyeGazeSampleTimeEXT where
  zero = EyeGazeSampleTimeEXT
           zero


type EXT_eye_gaze_interaction_SPEC_VERSION = 1

-- No documentation found for TopLevel "XR_EXT_eye_gaze_interaction_SPEC_VERSION"
pattern EXT_eye_gaze_interaction_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_eye_gaze_interaction_SPEC_VERSION = 1


type EXT_EYE_GAZE_INTERACTION_EXTENSION_NAME = "XR_EXT_eye_gaze_interaction"

-- No documentation found for TopLevel "XR_EXT_EYE_GAZE_INTERACTION_EXTENSION_NAME"
pattern EXT_EYE_GAZE_INTERACTION_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_EYE_GAZE_INTERACTION_EXTENSION_NAME = "XR_EXT_eye_gaze_interaction"

