{-# language CPP #-}
-- No documentation found for Chapter "CommandPoolCreateFlagBits"
module Vulkan.Core10.Enums.CommandPoolCreateFlagBits  ( CommandPoolCreateFlags
                                                      , CommandPoolCreateFlagBits( COMMAND_POOL_CREATE_TRANSIENT_BIT
                                                                                 , COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
                                                                                 , COMMAND_POOL_CREATE_PROTECTED_BIT
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
type CommandPoolCreateFlags = CommandPoolCreateFlagBits

-- No documentation found for TopLevel "VkCommandPoolCreateFlagBits"
newtype CommandPoolCreateFlagBits = CommandPoolCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkCommandPoolCreateFlagBits" "VK_COMMAND_POOL_CREATE_TRANSIENT_BIT"
pattern COMMAND_POOL_CREATE_TRANSIENT_BIT            = CommandPoolCreateFlagBits 0x00000001
-- No documentation found for Nested "VkCommandPoolCreateFlagBits" "VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT"
pattern COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT = CommandPoolCreateFlagBits 0x00000002
-- No documentation found for Nested "VkCommandPoolCreateFlagBits" "VK_COMMAND_POOL_CREATE_PROTECTED_BIT"
pattern COMMAND_POOL_CREATE_PROTECTED_BIT            = CommandPoolCreateFlagBits 0x00000004

conNameCommandPoolCreateFlagBits :: String
conNameCommandPoolCreateFlagBits = "CommandPoolCreateFlagBits"

enumPrefixCommandPoolCreateFlagBits :: String
enumPrefixCommandPoolCreateFlagBits = "COMMAND_POOL_CREATE_"

showTableCommandPoolCreateFlagBits :: [(CommandPoolCreateFlagBits, String)]
showTableCommandPoolCreateFlagBits =
  [ (COMMAND_POOL_CREATE_TRANSIENT_BIT           , "TRANSIENT_BIT")
  , (COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT, "RESET_COMMAND_BUFFER_BIT")
  , (COMMAND_POOL_CREATE_PROTECTED_BIT           , "PROTECTED_BIT")
  ]


instance Show CommandPoolCreateFlagBits where
showsPrec = enumShowsPrec enumPrefixCommandPoolCreateFlagBits
                          showTableCommandPoolCreateFlagBits
                          conNameCommandPoolCreateFlagBits
                          (\(CommandPoolCreateFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read CommandPoolCreateFlagBits where
  readPrec = enumReadPrec enumPrefixCommandPoolCreateFlagBits
                          showTableCommandPoolCreateFlagBits
                          conNameCommandPoolCreateFlagBits
                          CommandPoolCreateFlagBits

