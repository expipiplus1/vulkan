{-# language CPP #-}
-- No documentation found for Chapter "ViewStateFlags"
module OpenXR.Core10.Enums.ViewStateFlags  (ViewStateFlags(..)) where

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
-- | XrViewStateFlags - View state flags
--
-- == Flag Descriptions
--
-- = See Also
--
-- 'OpenXR.Core10.DisplayTiming.View',
-- 'OpenXR.Core10.DisplayTiming.ViewState',
-- 'OpenXR.Core10.DisplayTiming.locateViews'
newtype ViewStateFlags = ViewStateFlags Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameViewStateFlags :: String
conNameViewStateFlags = "ViewStateFlags"

enumPrefixViewStateFlags :: String
enumPrefixViewStateFlags = ""

showTableViewStateFlags :: [(ViewStateFlags, String)]
showTableViewStateFlags = []

instance Show ViewStateFlags where
  showsPrec = enumShowsPrec enumPrefixViewStateFlags
                            showTableViewStateFlags
                            conNameViewStateFlags
                            (\(ViewStateFlags x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read ViewStateFlags where
  readPrec = enumReadPrec enumPrefixViewStateFlags showTableViewStateFlags conNameViewStateFlags ViewStateFlags

