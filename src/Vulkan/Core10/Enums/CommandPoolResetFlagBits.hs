{-# language CPP #-}
-- No documentation found for Chapter "CommandPoolResetFlagBits"
module Vulkan.Core10.Enums.CommandPoolResetFlagBits  ( CommandPoolResetFlags
                                                     , CommandPoolResetFlagBits( COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT
                                                                               , ..
                                                                               )
                                                     ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Vulkan.Zero (Zero)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
type CommandPoolResetFlags = CommandPoolResetFlagBits

-- | VkCommandPoolResetFlagBits - Bitmask controlling behavior of a command
-- pool reset
--
-- = See Also
--
-- 'CommandPoolResetFlags'
newtype CommandPoolResetFlagBits = CommandPoolResetFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT' specifies that resetting a
-- command pool recycles all of the resources from the command pool back to
-- the system.
pattern COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT = CommandPoolResetFlagBits 0x00000001

conNameCommandPoolResetFlagBits :: String
conNameCommandPoolResetFlagBits = "CommandPoolResetFlagBits"

enumPrefixCommandPoolResetFlagBits :: String
enumPrefixCommandPoolResetFlagBits = "COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT"

showTableCommandPoolResetFlagBits :: [(CommandPoolResetFlagBits, String)]
showTableCommandPoolResetFlagBits = [(COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT, "")]

instance Show CommandPoolResetFlagBits where
  showsPrec = enumShowsPrec enumPrefixCommandPoolResetFlagBits
                            showTableCommandPoolResetFlagBits
                            conNameCommandPoolResetFlagBits
                            (\(CommandPoolResetFlagBits x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read CommandPoolResetFlagBits where
  readPrec = enumReadPrec enumPrefixCommandPoolResetFlagBits
                          showTableCommandPoolResetFlagBits
                          conNameCommandPoolResetFlagBits
                          CommandPoolResetFlagBits

