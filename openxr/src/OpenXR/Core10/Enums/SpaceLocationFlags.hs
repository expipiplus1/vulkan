{-# language CPP #-}
-- No documentation found for Chapter "SpaceLocationFlags"
module OpenXR.Core10.Enums.SpaceLocationFlags  (SpaceLocationFlags(..)) where

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
-- | XrSpaceLocationFlags - Space location flags
--
-- = Description
--
-- where the flags have the following meaning:
--
-- == Flag Descriptions
--
-- = See Also
--
-- 'OpenXR.Extensions.XR_EXT_hand_tracking.HandJointLocationEXT',
-- 'OpenXR.Core10.Space.SpaceLocation'
newtype SpaceLocationFlags = SpaceLocationFlags Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameSpaceLocationFlags :: String
conNameSpaceLocationFlags = "SpaceLocationFlags"

enumPrefixSpaceLocationFlags :: String
enumPrefixSpaceLocationFlags = ""

showTableSpaceLocationFlags :: [(SpaceLocationFlags, String)]
showTableSpaceLocationFlags = []

instance Show SpaceLocationFlags where
  showsPrec = enumShowsPrec enumPrefixSpaceLocationFlags
                            showTableSpaceLocationFlags
                            conNameSpaceLocationFlags
                            (\(SpaceLocationFlags x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read SpaceLocationFlags where
  readPrec =
    enumReadPrec enumPrefixSpaceLocationFlags showTableSpaceLocationFlags conNameSpaceLocationFlags SpaceLocationFlags

