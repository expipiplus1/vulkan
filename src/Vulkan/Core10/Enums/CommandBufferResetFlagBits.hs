{-# language CPP #-}
-- No documentation found for Chapter "CommandBufferResetFlagBits"
module Vulkan.Core10.Enums.CommandBufferResetFlagBits  ( CommandBufferResetFlags
                                                       , CommandBufferResetFlagBits( COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT
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
type CommandBufferResetFlags = CommandBufferResetFlagBits

-- No documentation found for TopLevel "VkCommandBufferResetFlagBits"
newtype CommandBufferResetFlagBits = CommandBufferResetFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkCommandBufferResetFlagBits" "VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT"
pattern COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT = CommandBufferResetFlagBits 0x00000001

conNameCommandBufferResetFlagBits :: String
conNameCommandBufferResetFlagBits = "CommandBufferResetFlagBits"

enumPrefixCommandBufferResetFlagBits :: String
enumPrefixCommandBufferResetFlagBits = "COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT"

showTableCommandBufferResetFlagBits :: [(CommandBufferResetFlagBits, String)]
showTableCommandBufferResetFlagBits = [(COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT, "")]


instance Show CommandBufferResetFlagBits where
showsPrec = enumShowsPrec enumPrefixCommandBufferResetFlagBits
                          showTableCommandBufferResetFlagBits
                          conNameCommandBufferResetFlagBits
                          (\(CommandBufferResetFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read CommandBufferResetFlagBits where
  readPrec = enumReadPrec enumPrefixCommandBufferResetFlagBits
                          showTableCommandBufferResetFlagBits
                          conNameCommandBufferResetFlagBits
                          CommandBufferResetFlagBits

