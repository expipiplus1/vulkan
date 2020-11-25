{-# language CPP #-}
-- No documentation found for Chapter "DeviceQueueCreateFlagBits"
module Vulkan.Core10.Enums.DeviceQueueCreateFlagBits  ( DeviceQueueCreateFlags
                                                      , DeviceQueueCreateFlagBits( DEVICE_QUEUE_CREATE_PROTECTED_BIT
                                                                                 , ..
                                                                                 )
                                                      ) where

import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import Numeric (showHex)
import Text.ParserCombinators.ReadPrec ((+++))
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

instance Show DeviceQueueCreateFlagBits where
  showsPrec p = \case
    DEVICE_QUEUE_CREATE_PROTECTED_BIT -> showString "DEVICE_QUEUE_CREATE_PROTECTED_BIT"
    DeviceQueueCreateFlagBits x -> showParen (p >= 11) (showString "DeviceQueueCreateFlagBits 0x" . showHex x)

instance Read DeviceQueueCreateFlagBits where
  readPrec = parens (choose [("DEVICE_QUEUE_CREATE_PROTECTED_BIT", pure DEVICE_QUEUE_CREATE_PROTECTED_BIT)]
                     +++
                     prec 10 (do
                       expectP (Ident "DeviceQueueCreateFlagBits")
                       v <- step readPrec
                       pure (DeviceQueueCreateFlagBits v)))

