{-# language CPP #-}
-- No documentation found for Chapter "SpaceVelocityFlags"
module OpenXR.Core10.Enums.SpaceVelocityFlags  (SpaceVelocityFlags(..)) where

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
-- | XrSpaceVelocityFlags - Space velocity flags
--
-- = Description
--
-- where the flags have the following meaning:
--
-- == Flag Descriptions
--
-- = See Also
--
-- 'OpenXR.Extensions.XR_EXT_hand_tracking.HandJointVelocityEXT',
-- 'OpenXR.Core10.Space.SpaceVelocity'
newtype SpaceVelocityFlags = SpaceVelocityFlags Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameSpaceVelocityFlags :: String
conNameSpaceVelocityFlags = "SpaceVelocityFlags"

enumPrefixSpaceVelocityFlags :: String
enumPrefixSpaceVelocityFlags = ""

showTableSpaceVelocityFlags :: [(SpaceVelocityFlags, String)]
showTableSpaceVelocityFlags = []

instance Show SpaceVelocityFlags where
  showsPrec = enumShowsPrec enumPrefixSpaceVelocityFlags
                            showTableSpaceVelocityFlags
                            conNameSpaceVelocityFlags
                            (\(SpaceVelocityFlags x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read SpaceVelocityFlags where
  readPrec =
    enumReadPrec enumPrefixSpaceVelocityFlags showTableSpaceVelocityFlags conNameSpaceVelocityFlags SpaceVelocityFlags

