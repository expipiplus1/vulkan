{-# language CPP #-}
-- No documentation found for Chapter "SessionCreateFlags"
module OpenXR.Core10.Enums.SessionCreateFlags  (SessionCreateFlags(..)) where

import OpenXR.Internal.Utils (enumReadPrec)
import OpenXR.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import OpenXR.Core10.FundamentalTypes (Flags64)
import OpenXR.Zero (Zero)
-- | XrSessionCreateFlags - Session Creation Flags
--
-- = Description
--
-- There are currently no session creation flags. This is reserved for
-- future use.
--
-- = See Also
--
-- 'OpenXR.Core10.Device.SessionCreateInfo',
-- 'OpenXR.Core10.Device.createSession'
newtype SessionCreateFlags = SessionCreateFlags Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameSessionCreateFlags :: String
conNameSessionCreateFlags = "SessionCreateFlags"

enumPrefixSessionCreateFlags :: String
enumPrefixSessionCreateFlags = ""

showTableSessionCreateFlags :: [(SessionCreateFlags, String)]
showTableSessionCreateFlags = []

instance Show SessionCreateFlags where
  showsPrec = enumShowsPrec enumPrefixSessionCreateFlags
                            showTableSessionCreateFlags
                            conNameSessionCreateFlags
                            (\(SessionCreateFlags x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read SessionCreateFlags where
  readPrec =
    enumReadPrec enumPrefixSessionCreateFlags showTableSessionCreateFlags conNameSessionCreateFlags SessionCreateFlags

