{-# language CPP #-}
-- No documentation found for Chapter "InputSourceLocalizedNameFlagBits"
module OpenXR.Core10.Enums.InputSourceLocalizedNameFlagBits  ( InputSourceLocalizedNameFlags
                                                             , InputSourceLocalizedNameFlagBits( INPUT_SOURCE_LOCALIZED_NAME_USER_PATH_BIT
                                                                                               , INPUT_SOURCE_LOCALIZED_NAME_INTERACTION_PROFILE_BIT
                                                                                               , INPUT_SOURCE_LOCALIZED_NAME_COMPONENT_BIT
                                                                                               , ..
                                                                                               )
                                                             ) where

import OpenXR.Internal.Utils (enumReadPrec)
import OpenXR.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import OpenXR.Zero (Zero)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import OpenXR.Core10.FundamentalTypes (Flags64)
type InputSourceLocalizedNameFlags = InputSourceLocalizedNameFlagBits

-- No documentation found for TopLevel "XrInputSourceLocalizedNameFlagBits"
newtype InputSourceLocalizedNameFlagBits = InputSourceLocalizedNameFlagBits Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "XrInputSourceLocalizedNameFlagBits" "XR_INPUT_SOURCE_LOCALIZED_NAME_USER_PATH_BIT"
pattern INPUT_SOURCE_LOCALIZED_NAME_USER_PATH_BIT           = InputSourceLocalizedNameFlagBits 0x0000000000000001
-- No documentation found for Nested "XrInputSourceLocalizedNameFlagBits" "XR_INPUT_SOURCE_LOCALIZED_NAME_INTERACTION_PROFILE_BIT"
pattern INPUT_SOURCE_LOCALIZED_NAME_INTERACTION_PROFILE_BIT = InputSourceLocalizedNameFlagBits 0x0000000000000002
-- No documentation found for Nested "XrInputSourceLocalizedNameFlagBits" "XR_INPUT_SOURCE_LOCALIZED_NAME_COMPONENT_BIT"
pattern INPUT_SOURCE_LOCALIZED_NAME_COMPONENT_BIT           = InputSourceLocalizedNameFlagBits 0x0000000000000004

conNameInputSourceLocalizedNameFlagBits :: String
conNameInputSourceLocalizedNameFlagBits = "InputSourceLocalizedNameFlagBits"

enumPrefixInputSourceLocalizedNameFlagBits :: String
enumPrefixInputSourceLocalizedNameFlagBits = "INPUT_SOURCE_LOCALIZED_NAME_"

showTableInputSourceLocalizedNameFlagBits :: [(InputSourceLocalizedNameFlagBits, String)]
showTableInputSourceLocalizedNameFlagBits =
  [ (INPUT_SOURCE_LOCALIZED_NAME_USER_PATH_BIT          , "USER_PATH_BIT")
  , (INPUT_SOURCE_LOCALIZED_NAME_INTERACTION_PROFILE_BIT, "INTERACTION_PROFILE_BIT")
  , (INPUT_SOURCE_LOCALIZED_NAME_COMPONENT_BIT          , "COMPONENT_BIT")
  ]

instance Show InputSourceLocalizedNameFlagBits where
  showsPrec = enumShowsPrec enumPrefixInputSourceLocalizedNameFlagBits
                            showTableInputSourceLocalizedNameFlagBits
                            conNameInputSourceLocalizedNameFlagBits
                            (\(InputSourceLocalizedNameFlagBits x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read InputSourceLocalizedNameFlagBits where
  readPrec = enumReadPrec enumPrefixInputSourceLocalizedNameFlagBits
                          showTableInputSourceLocalizedNameFlagBits
                          conNameInputSourceLocalizedNameFlagBits
                          InputSourceLocalizedNameFlagBits

