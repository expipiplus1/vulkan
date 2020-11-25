{-# language CPP #-}
-- No documentation found for Chapter "DeviceQueueCreateFlagBits"
module Vulkan.Core10.Enums.DeviceQueueCreateFlagBits  ( DeviceQueueCreateFlags
                                                      , DeviceQueueCreateFlagBits( DEVICE_QUEUE_CREATE_PROTECTED_BIT
                                                                                 , ..
                                                                                 )
                                                      ) where

import Data.Foldable (asum)
import GHC.Base ((<$))
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import Numeric (showHex)
import Text.ParserCombinators.ReadP (skipSpaces)
import Text.ParserCombinators.ReadP (string)
import Text.ParserCombinators.ReadPrec ((+++))
import qualified Text.ParserCombinators.ReadPrec (lift)
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
type DeviceQueueCreateFlags = DeviceQueueCreateFlagBits

-- | VkDeviceQueueCreateFlagBits - Bitmask specifying behavior of the queue
--
-- = See Also
--
-- 'DeviceQueueCreateFlags'
newtype DeviceQueueCreateFlagBits = DeviceQueueCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'DEVICE_QUEUE_CREATE_PROTECTED_BIT' specifies that the device queue is a
-- protected-capable queue.
pattern DEVICE_QUEUE_CREATE_PROTECTED_BIT = DeviceQueueCreateFlagBits 0x00000001

conNameDeviceQueueCreateFlagBits :: String
conNameDeviceQueueCreateFlagBits = "DeviceQueueCreateFlagBits"

enumPrefixDeviceQueueCreateFlagBits :: String
enumPrefixDeviceQueueCreateFlagBits = "DEVICE_QUEUE_CREATE_PROTECTED_BIT"

showTableDeviceQueueCreateFlagBits :: [(DeviceQueueCreateFlagBits, String)]
showTableDeviceQueueCreateFlagBits = [(DEVICE_QUEUE_CREATE_PROTECTED_BIT, "")]

instance Show DeviceQueueCreateFlagBits where
  showsPrec p e = case lookup e showTableDeviceQueueCreateFlagBits of
    Just s -> showString enumPrefixDeviceQueueCreateFlagBits . showString s
    Nothing ->
      let DeviceQueueCreateFlagBits x = e
      in  showParen (p >= 11) (showString conNameDeviceQueueCreateFlagBits . showString " 0x" . showHex x)

instance Read DeviceQueueCreateFlagBits where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixDeviceQueueCreateFlagBits
          asum ((\(e, s) -> e <$ string s) <$> showTableDeviceQueueCreateFlagBits)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameDeviceQueueCreateFlagBits)
            v <- step readPrec
            pure (DeviceQueueCreateFlagBits v)
          )
    )

