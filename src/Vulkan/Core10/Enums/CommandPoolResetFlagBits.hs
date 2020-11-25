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
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
type CommandPoolResetFlags = CommandPoolResetFlagBits

-- No documentation found for TopLevel "VkCommandPoolResetFlagBits"
newtype CommandPoolResetFlagBits = CommandPoolResetFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkCommandPoolResetFlagBits" "VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT"
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

