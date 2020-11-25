{-# language CPP #-}
-- No documentation found for Chapter "CommandBufferLevel"
module Vulkan.Core10.Enums.CommandBufferLevel  (CommandBufferLevel( COMMAND_BUFFER_LEVEL_PRIMARY
                                                                  , COMMAND_BUFFER_LEVEL_SECONDARY
                                                                  , ..
                                                                  )) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Zero (Zero)
-- No documentation found for TopLevel "VkCommandBufferLevel"
newtype CommandBufferLevel = CommandBufferLevel Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkCommandBufferLevel" "VK_COMMAND_BUFFER_LEVEL_PRIMARY"
pattern COMMAND_BUFFER_LEVEL_PRIMARY   = CommandBufferLevel 0
-- No documentation found for Nested "VkCommandBufferLevel" "VK_COMMAND_BUFFER_LEVEL_SECONDARY"
pattern COMMAND_BUFFER_LEVEL_SECONDARY = CommandBufferLevel 1
{-# complete COMMAND_BUFFER_LEVEL_PRIMARY,
             COMMAND_BUFFER_LEVEL_SECONDARY :: CommandBufferLevel #-}

conNameCommandBufferLevel :: String
conNameCommandBufferLevel = "CommandBufferLevel"

enumPrefixCommandBufferLevel :: String
enumPrefixCommandBufferLevel = "COMMAND_BUFFER_LEVEL_"

showTableCommandBufferLevel :: [(CommandBufferLevel, String)]
showTableCommandBufferLevel =
  [(COMMAND_BUFFER_LEVEL_PRIMARY, "PRIMARY"), (COMMAND_BUFFER_LEVEL_SECONDARY, "SECONDARY")]


instance Show CommandBufferLevel where
showsPrec = enumShowsPrec enumPrefixCommandBufferLevel
                          showTableCommandBufferLevel
                          conNameCommandBufferLevel
                          (\(CommandBufferLevel x) -> x)
                          (showsPrec 11)


instance Read CommandBufferLevel where
  readPrec =
    enumReadPrec enumPrefixCommandBufferLevel showTableCommandBufferLevel conNameCommandBufferLevel CommandBufferLevel

