{-# language CPP #-}
-- No documentation found for Chapter "CommandBufferUsageFlagBits"
module Vulkan.Core10.Enums.CommandBufferUsageFlagBits  ( CommandBufferUsageFlags
                                                       , CommandBufferUsageFlagBits( COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
                                                                                   , COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT
                                                                                   , COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT
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
type CommandBufferUsageFlags = CommandBufferUsageFlagBits

-- | VkCommandBufferUsageFlagBits - Bitmask specifying usage behavior for
-- command buffer
--
-- = See Also
--
-- 'CommandBufferUsageFlags'
newtype CommandBufferUsageFlagBits = CommandBufferUsageFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT' specifies that each recording
-- of the command buffer will only be submitted once, and the command
-- buffer will be reset and recorded again between each submission.
pattern COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT      = CommandBufferUsageFlagBits 0x00000001
-- | 'COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT' specifies that a
-- secondary command buffer is considered to be entirely inside a render
-- pass. If this is a primary command buffer, then this bit is ignored.
pattern COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT = CommandBufferUsageFlagBits 0x00000002
-- | 'COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT' specifies that a command
-- buffer /can/ be resubmitted to a queue while it is in the /pending
-- state/, and recorded into multiple primary command buffers.
pattern COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT     = CommandBufferUsageFlagBits 0x00000004

conNameCommandBufferUsageFlagBits :: String
conNameCommandBufferUsageFlagBits = "CommandBufferUsageFlagBits"

enumPrefixCommandBufferUsageFlagBits :: String
enumPrefixCommandBufferUsageFlagBits = "COMMAND_BUFFER_USAGE_"

showTableCommandBufferUsageFlagBits :: [(CommandBufferUsageFlagBits, String)]
showTableCommandBufferUsageFlagBits =
  [ (COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT     , "ONE_TIME_SUBMIT_BIT")
  , (COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT, "RENDER_PASS_CONTINUE_BIT")
  , (COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT    , "SIMULTANEOUS_USE_BIT")
  ]

instance Show CommandBufferUsageFlagBits where
  showsPrec = enumShowsPrec enumPrefixCommandBufferUsageFlagBits
                            showTableCommandBufferUsageFlagBits
                            conNameCommandBufferUsageFlagBits
                            (\(CommandBufferUsageFlagBits x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read CommandBufferUsageFlagBits where
  readPrec = enumReadPrec enumPrefixCommandBufferUsageFlagBits
                          showTableCommandBufferUsageFlagBits
                          conNameCommandBufferUsageFlagBits
                          CommandBufferUsageFlagBits

