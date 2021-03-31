{-# language CPP #-}
-- No documentation found for Chapter "ViewStateFlagBits"
module OpenXR.Core10.Enums.ViewStateFlagBits  ( ViewStateFlags
                                              , ViewStateFlagBits( VIEW_STATE_ORIENTATION_VALID_BIT
                                                                 , VIEW_STATE_POSITION_VALID_BIT
                                                                 , VIEW_STATE_ORIENTATION_TRACKED_BIT
                                                                 , VIEW_STATE_POSITION_TRACKED_BIT
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
type ViewStateFlags = ViewStateFlagBits

-- No documentation found for TopLevel "XrViewStateFlagBits"
newtype ViewStateFlagBits = ViewStateFlagBits Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "XrViewStateFlagBits" "XR_VIEW_STATE_ORIENTATION_VALID_BIT"
pattern VIEW_STATE_ORIENTATION_VALID_BIT   = ViewStateFlagBits 0x0000000000000001
-- No documentation found for Nested "XrViewStateFlagBits" "XR_VIEW_STATE_POSITION_VALID_BIT"
pattern VIEW_STATE_POSITION_VALID_BIT      = ViewStateFlagBits 0x0000000000000002
-- No documentation found for Nested "XrViewStateFlagBits" "XR_VIEW_STATE_ORIENTATION_TRACKED_BIT"
pattern VIEW_STATE_ORIENTATION_TRACKED_BIT = ViewStateFlagBits 0x0000000000000004
-- No documentation found for Nested "XrViewStateFlagBits" "XR_VIEW_STATE_POSITION_TRACKED_BIT"
pattern VIEW_STATE_POSITION_TRACKED_BIT    = ViewStateFlagBits 0x0000000000000008

conNameViewStateFlagBits :: String
conNameViewStateFlagBits = "ViewStateFlagBits"

enumPrefixViewStateFlagBits :: String
enumPrefixViewStateFlagBits = "VIEW_STATE_"

showTableViewStateFlagBits :: [(ViewStateFlagBits, String)]
showTableViewStateFlagBits =
  [ (VIEW_STATE_ORIENTATION_VALID_BIT  , "ORIENTATION_VALID_BIT")
  , (VIEW_STATE_POSITION_VALID_BIT     , "POSITION_VALID_BIT")
  , (VIEW_STATE_ORIENTATION_TRACKED_BIT, "ORIENTATION_TRACKED_BIT")
  , (VIEW_STATE_POSITION_TRACKED_BIT   , "POSITION_TRACKED_BIT")
  ]

instance Show ViewStateFlagBits where
  showsPrec = enumShowsPrec enumPrefixViewStateFlagBits
                            showTableViewStateFlagBits
                            conNameViewStateFlagBits
                            (\(ViewStateFlagBits x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read ViewStateFlagBits where
  readPrec =
    enumReadPrec enumPrefixViewStateFlagBits showTableViewStateFlagBits conNameViewStateFlagBits ViewStateFlagBits

