{-# language CPP #-}
-- No documentation found for Chapter "SystemAllocationScope"
module Vulkan.Core10.Enums.SystemAllocationScope  (SystemAllocationScope( SYSTEM_ALLOCATION_SCOPE_COMMAND
                                                                        , SYSTEM_ALLOCATION_SCOPE_OBJECT
                                                                        , SYSTEM_ALLOCATION_SCOPE_CACHE
                                                                        , SYSTEM_ALLOCATION_SCOPE_DEVICE
                                                                        , SYSTEM_ALLOCATION_SCOPE_INSTANCE
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
-- No documentation found for TopLevel "VkSystemAllocationScope"
newtype SystemAllocationScope = SystemAllocationScope Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkSystemAllocationScope" "VK_SYSTEM_ALLOCATION_SCOPE_COMMAND"
pattern SYSTEM_ALLOCATION_SCOPE_COMMAND  = SystemAllocationScope 0
-- No documentation found for Nested "VkSystemAllocationScope" "VK_SYSTEM_ALLOCATION_SCOPE_OBJECT"
pattern SYSTEM_ALLOCATION_SCOPE_OBJECT   = SystemAllocationScope 1
-- No documentation found for Nested "VkSystemAllocationScope" "VK_SYSTEM_ALLOCATION_SCOPE_CACHE"
pattern SYSTEM_ALLOCATION_SCOPE_CACHE    = SystemAllocationScope 2
-- No documentation found for Nested "VkSystemAllocationScope" "VK_SYSTEM_ALLOCATION_SCOPE_DEVICE"
pattern SYSTEM_ALLOCATION_SCOPE_DEVICE   = SystemAllocationScope 3
-- No documentation found for Nested "VkSystemAllocationScope" "VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE"
pattern SYSTEM_ALLOCATION_SCOPE_INSTANCE = SystemAllocationScope 4
{-# complete SYSTEM_ALLOCATION_SCOPE_COMMAND,
             SYSTEM_ALLOCATION_SCOPE_OBJECT,
             SYSTEM_ALLOCATION_SCOPE_CACHE,
             SYSTEM_ALLOCATION_SCOPE_DEVICE,
             SYSTEM_ALLOCATION_SCOPE_INSTANCE :: SystemAllocationScope #-}

conNameSystemAllocationScope :: String
conNameSystemAllocationScope = "SystemAllocationScope"

enumPrefixSystemAllocationScope :: String
enumPrefixSystemAllocationScope = "SYSTEM_ALLOCATION_SCOPE_"

showTableSystemAllocationScope :: [(SystemAllocationScope, String)]
showTableSystemAllocationScope =
  [ (SYSTEM_ALLOCATION_SCOPE_COMMAND , "COMMAND")
  , (SYSTEM_ALLOCATION_SCOPE_OBJECT  , "OBJECT")
  , (SYSTEM_ALLOCATION_SCOPE_CACHE   , "CACHE")
  , (SYSTEM_ALLOCATION_SCOPE_DEVICE  , "DEVICE")
  , (SYSTEM_ALLOCATION_SCOPE_INSTANCE, "INSTANCE")
  ]


instance Show SystemAllocationScope where
showsPrec = enumShowsPrec enumPrefixSystemAllocationScope
                          showTableSystemAllocationScope
                          conNameSystemAllocationScope
                          (\(SystemAllocationScope x) -> x)
                          (showsPrec 11)


instance Read SystemAllocationScope where
  readPrec = enumReadPrec enumPrefixSystemAllocationScope
                          showTableSystemAllocationScope
                          conNameSystemAllocationScope
                          SystemAllocationScope

