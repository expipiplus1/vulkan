{-# language CPP #-}
-- No documentation found for Chapter "InputSourceLocalizedNameFlags"
module OpenXR.Core10.Enums.InputSourceLocalizedNameFlags  (InputSourceLocalizedNameFlags(..)) where

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
-- | XrInputSourceLocalizedNameFlags - Input source localized name flags
--
-- == Flag Descriptions
--
-- = See Also
--
-- 'OpenXR.Core10.Input.InputSourceLocalizedNameGetInfo',
-- 'OpenXR.Core10.Input.getInputSourceLocalizedName'
newtype InputSourceLocalizedNameFlags = InputSourceLocalizedNameFlags Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameInputSourceLocalizedNameFlags :: String
conNameInputSourceLocalizedNameFlags = "InputSourceLocalizedNameFlags"

enumPrefixInputSourceLocalizedNameFlags :: String
enumPrefixInputSourceLocalizedNameFlags = ""

showTableInputSourceLocalizedNameFlags :: [(InputSourceLocalizedNameFlags, String)]
showTableInputSourceLocalizedNameFlags = []

instance Show InputSourceLocalizedNameFlags where
  showsPrec = enumShowsPrec enumPrefixInputSourceLocalizedNameFlags
                            showTableInputSourceLocalizedNameFlags
                            conNameInputSourceLocalizedNameFlags
                            (\(InputSourceLocalizedNameFlags x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read InputSourceLocalizedNameFlags where
  readPrec = enumReadPrec enumPrefixInputSourceLocalizedNameFlags
                          showTableInputSourceLocalizedNameFlags
                          conNameInputSourceLocalizedNameFlags
                          InputSourceLocalizedNameFlags

