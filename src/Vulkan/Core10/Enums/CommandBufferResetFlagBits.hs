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
import Vulkan.Zero (Zero)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
type CommandBufferResetFlags = CommandBufferResetFlagBits

-- | VkCommandBufferResetFlagBits - Bitmask controlling behavior of a command
-- buffer reset
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'CommandBufferResetFlags'
newtype CommandBufferResetFlagBits = CommandBufferResetFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT' specifies that most or all
-- memory resources currently owned by the command buffer /should/ be
-- returned to the parent command pool. If this flag is not set, then the
-- command buffer /may/ hold onto memory resources and reuse them when
-- recording commands. @commandBuffer@ is moved to the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#commandbuffers-lifecycle initial state>.
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

