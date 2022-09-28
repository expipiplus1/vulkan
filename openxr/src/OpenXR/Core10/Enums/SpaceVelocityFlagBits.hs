{-# language CPP #-}
-- No documentation found for Chapter "SpaceVelocityFlagBits"
module OpenXR.Core10.Enums.SpaceVelocityFlagBits  ( SpaceVelocityFlags
                                                  , SpaceVelocityFlagBits( SPACE_VELOCITY_LINEAR_VALID_BIT
                                                                         , SPACE_VELOCITY_ANGULAR_VALID_BIT
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
type SpaceVelocityFlags = SpaceVelocityFlagBits

-- No documentation found for TopLevel "XrSpaceVelocityFlagBits"
newtype SpaceVelocityFlagBits = SpaceVelocityFlagBits Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "XrSpaceVelocityFlagBits" "XR_SPACE_VELOCITY_LINEAR_VALID_BIT"
pattern SPACE_VELOCITY_LINEAR_VALID_BIT = SpaceVelocityFlagBits 0x0000000000000001

-- No documentation found for Nested "XrSpaceVelocityFlagBits" "XR_SPACE_VELOCITY_ANGULAR_VALID_BIT"
pattern SPACE_VELOCITY_ANGULAR_VALID_BIT = SpaceVelocityFlagBits 0x0000000000000002

conNameSpaceVelocityFlagBits :: String
conNameSpaceVelocityFlagBits = "SpaceVelocityFlagBits"

enumPrefixSpaceVelocityFlagBits :: String
enumPrefixSpaceVelocityFlagBits = "SPACE_VELOCITY_"

showTableSpaceVelocityFlagBits :: [(SpaceVelocityFlagBits, String)]
showTableSpaceVelocityFlagBits =
  [
    ( SPACE_VELOCITY_LINEAR_VALID_BIT
    , "LINEAR_VALID_BIT"
    )
  ,
    ( SPACE_VELOCITY_ANGULAR_VALID_BIT
    , "ANGULAR_VALID_BIT"
    )
  ]

instance Show SpaceVelocityFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixSpaceVelocityFlagBits
      showTableSpaceVelocityFlagBits
      conNameSpaceVelocityFlagBits
      (\(SpaceVelocityFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read SpaceVelocityFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixSpaceVelocityFlagBits
      showTableSpaceVelocityFlagBits
      conNameSpaceVelocityFlagBits
      SpaceVelocityFlagBits
