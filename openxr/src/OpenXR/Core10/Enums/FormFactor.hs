{-# language CPP #-}
-- No documentation found for Chapter "FormFactor"
module OpenXR.Core10.Enums.FormFactor  (FormFactor( FORM_FACTOR_HEAD_MOUNTED_DISPLAY
                                                  , FORM_FACTOR_HANDHELD_DISPLAY
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

-- | XrFormFactor - Supported form factors
--
-- = Description
--
-- The predefined form factors which /may/ be supported by OpenXR runtimes
-- are:
--
-- == Enumerant Descriptions
--
-- = See Also
--
-- 'OpenXR.Core10.APIConstants.NULL_SYSTEM_ID',
-- 'OpenXR.Core10.Device.SystemGetInfo',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >,
-- 'OpenXR.Core10.Device.getSystem'
newtype FormFactor = FormFactor Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- Note that the zero instance does not produce a valid value, passing 'zero' to Vulkan will result in an error

-- | 'FORM_FACTOR_HEAD_MOUNTED_DISPLAY'. The tracked display is attached to
-- the user’s head. The user cannot touch the display itself. A VR headset
-- would be an example of this form factor.
pattern FORM_FACTOR_HEAD_MOUNTED_DISPLAY = FormFactor 1

-- | 'FORM_FACTOR_HANDHELD_DISPLAY'. The tracked display is held in the
-- user’s hand, independent from the user’s head. The user /may/ be able to
-- touch the display, allowing for screen-space UI. A mobile phone running
-- an AR experience using pass-through video would be an example of this
-- form factor.
pattern FORM_FACTOR_HANDHELD_DISPLAY = FormFactor 2

{-# COMPLETE
  FORM_FACTOR_HEAD_MOUNTED_DISPLAY
  , FORM_FACTOR_HANDHELD_DISPLAY ::
    FormFactor
  #-}

conNameFormFactor :: String
conNameFormFactor = "FormFactor"

enumPrefixFormFactor :: String
enumPrefixFormFactor = "FORM_FACTOR_H"

showTableFormFactor :: [(FormFactor, String)]
showTableFormFactor =
  [
    ( FORM_FACTOR_HEAD_MOUNTED_DISPLAY
    , "EAD_MOUNTED_DISPLAY"
    )
  , (FORM_FACTOR_HANDHELD_DISPLAY, "ANDHELD_DISPLAY")
  ]

instance Show FormFactor where
  showsPrec =
    enumShowsPrec
      enumPrefixFormFactor
      showTableFormFactor
      conNameFormFactor
      (\(FormFactor x) -> x)
      (showsPrec 11)

instance Read FormFactor where
  readPrec =
    enumReadPrec
      enumPrefixFormFactor
      showTableFormFactor
      conNameFormFactor
      FormFactor
