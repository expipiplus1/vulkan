{-# language CPP #-}
-- No documentation found for Chapter "SessionCreateFlagBits"
module OpenXR.Core10.Enums.SessionCreateFlagBits  ( SessionCreateFlags
                                                  , SessionCreateFlagBits(..)
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
type SessionCreateFlags = SessionCreateFlagBits

-- No documentation found for TopLevel "XrSessionCreateFlagBits"
newtype SessionCreateFlagBits = SessionCreateFlagBits Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameSessionCreateFlagBits :: String
conNameSessionCreateFlagBits = "SessionCreateFlagBits"

enumPrefixSessionCreateFlagBits :: String
enumPrefixSessionCreateFlagBits = ""

showTableSessionCreateFlagBits :: [(SessionCreateFlagBits, String)]
showTableSessionCreateFlagBits = []

instance Show SessionCreateFlagBits where
  showsPrec = enumShowsPrec enumPrefixSessionCreateFlagBits
                            showTableSessionCreateFlagBits
                            conNameSessionCreateFlagBits
                            (\(SessionCreateFlagBits x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read SessionCreateFlagBits where
  readPrec = enumReadPrec enumPrefixSessionCreateFlagBits
                          showTableSessionCreateFlagBits
                          conNameSessionCreateFlagBits
                          SessionCreateFlagBits

