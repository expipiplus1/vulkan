{-# language CPP #-}
-- No documentation found for Chapter "SpaceLocationFlagBits"
module OpenXR.Core10.Enums.SpaceLocationFlagBits  ( SpaceLocationFlags
                                                  , SpaceLocationFlagBits( SPACE_LOCATION_ORIENTATION_VALID_BIT
                                                                         , SPACE_LOCATION_POSITION_VALID_BIT
                                                                         , SPACE_LOCATION_ORIENTATION_TRACKED_BIT
                                                                         , SPACE_LOCATION_POSITION_TRACKED_BIT
                                                                         , ..
                                                                         )
                                                  ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import OpenXR.Internal.Utils (enumReadPrec)
import OpenXR.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import OpenXR.Zero (Zero)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import OpenXR.Core10.FundamentalTypes (Flags64)
type SpaceLocationFlags = SpaceLocationFlagBits

-- No documentation found for TopLevel "XrSpaceLocationFlagBits"
newtype SpaceLocationFlagBits = SpaceLocationFlagBits Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "XrSpaceLocationFlagBits" "XR_SPACE_LOCATION_ORIENTATION_VALID_BIT"
pattern SPACE_LOCATION_ORIENTATION_VALID_BIT = SpaceLocationFlagBits 0x0000000000000001

-- No documentation found for Nested "XrSpaceLocationFlagBits" "XR_SPACE_LOCATION_POSITION_VALID_BIT"
pattern SPACE_LOCATION_POSITION_VALID_BIT = SpaceLocationFlagBits 0x0000000000000002

-- No documentation found for Nested "XrSpaceLocationFlagBits" "XR_SPACE_LOCATION_ORIENTATION_TRACKED_BIT"
pattern SPACE_LOCATION_ORIENTATION_TRACKED_BIT = SpaceLocationFlagBits 0x0000000000000004

-- No documentation found for Nested "XrSpaceLocationFlagBits" "XR_SPACE_LOCATION_POSITION_TRACKED_BIT"
pattern SPACE_LOCATION_POSITION_TRACKED_BIT = SpaceLocationFlagBits 0x0000000000000008

conNameSpaceLocationFlagBits :: String
conNameSpaceLocationFlagBits = "SpaceLocationFlagBits"

enumPrefixSpaceLocationFlagBits :: String
enumPrefixSpaceLocationFlagBits = "SPACE_LOCATION_"

showTableSpaceLocationFlagBits :: [(SpaceLocationFlagBits, String)]
showTableSpaceLocationFlagBits =
  [
    ( SPACE_LOCATION_ORIENTATION_VALID_BIT
    , "ORIENTATION_VALID_BIT"
    )
  ,
    ( SPACE_LOCATION_POSITION_VALID_BIT
    , "POSITION_VALID_BIT"
    )
  ,
    ( SPACE_LOCATION_ORIENTATION_TRACKED_BIT
    , "ORIENTATION_TRACKED_BIT"
    )
  ,
    ( SPACE_LOCATION_POSITION_TRACKED_BIT
    , "POSITION_TRACKED_BIT"
    )
  ]

instance Show SpaceLocationFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixSpaceLocationFlagBits
      showTableSpaceLocationFlagBits
      conNameSpaceLocationFlagBits
      (\(SpaceLocationFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read SpaceLocationFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixSpaceLocationFlagBits
      showTableSpaceLocationFlagBits
      conNameSpaceLocationFlagBits
      SpaceLocationFlagBits
