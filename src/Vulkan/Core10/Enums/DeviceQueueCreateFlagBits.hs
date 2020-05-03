{-# language CPP #-}
module Vulkan.Core10.Enums.DeviceQueueCreateFlagBits  ( DeviceQueueCreateFlagBits( DEVICE_QUEUE_CREATE_PROTECTED_BIT
                                                                                 , ..
                                                                                 )
                                                      , DeviceQueueCreateFlags
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
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Core10.BaseType (Flags)
import Vulkan.Zero (Zero)
-- | VkDeviceQueueCreateFlagBits - Bitmask specifying behavior of the queue
--
-- = See Also
--
-- 'DeviceQueueCreateFlags'
newtype DeviceQueueCreateFlagBits = DeviceQueueCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'DEVICE_QUEUE_CREATE_PROTECTED_BIT' specifies that the device queue is a
-- protected-capable queue.
pattern DEVICE_QUEUE_CREATE_PROTECTED_BIT = DeviceQueueCreateFlagBits 0x00000001

type DeviceQueueCreateFlags = DeviceQueueCreateFlagBits

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

