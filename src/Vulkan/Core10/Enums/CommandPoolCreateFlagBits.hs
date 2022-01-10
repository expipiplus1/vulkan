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
import Vulkan.Zero (Zero)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
type CommandPoolCreateFlags = CommandPoolCreateFlagBits

-- | VkCommandPoolCreateFlagBits - Bitmask specifying usage behavior for a
-- command pool
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'CommandPoolCreateFlags'
newtype CommandPoolCreateFlagBits = CommandPoolCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'COMMAND_POOL_CREATE_TRANSIENT_BIT' specifies that command buffers
-- allocated from the pool will be short-lived, meaning that they will be
-- reset or freed in a relatively short timeframe. This flag /may/ be used
-- by the implementation to control memory allocation behavior within the
-- pool.
pattern COMMAND_POOL_CREATE_TRANSIENT_BIT            = CommandPoolCreateFlagBits 0x00000001
-- | 'COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT' allows any command buffer
-- allocated from a pool to be individually reset to the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle initial state>;
-- either by calling 'Vulkan.Core10.CommandBuffer.resetCommandBuffer', or
-- via the implicit reset when calling
-- 'Vulkan.Core10.CommandBuffer.beginCommandBuffer'. If this flag is not
-- set on a pool, then 'Vulkan.Core10.CommandBuffer.resetCommandBuffer'
-- /must/ not be called for any command buffer allocated from that pool.
pattern COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT = CommandPoolCreateFlagBits 0x00000002
-- | 'COMMAND_POOL_CREATE_PROTECTED_BIT' specifies that command buffers
-- allocated from the pool are protected command buffers.
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

