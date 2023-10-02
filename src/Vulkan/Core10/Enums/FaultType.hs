{-# language CPP #-}
-- No documentation found for Chapter "FaultType"
module Vulkan.Core10.Enums.FaultType  (FaultType( FAULT_TYPE_INVALID
                                                , FAULT_TYPE_UNASSIGNED
                                                , FAULT_TYPE_IMPLEMENTATION
                                                , FAULT_TYPE_SYSTEM
                                                , FAULT_TYPE_PHYSICAL_DEVICE
                                                , FAULT_TYPE_COMMAND_BUFFER_FULL
                                                , FAULT_TYPE_INVALID_API_USAGE
                                                , ..
                                                )) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))

-- No documentation found for TopLevel "VkFaultType"
newtype FaultType = FaultType Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkFaultType" "VK_FAULT_TYPE_INVALID"
pattern FAULT_TYPE_INVALID = FaultType 0

-- No documentation found for Nested "VkFaultType" "VK_FAULT_TYPE_UNASSIGNED"
pattern FAULT_TYPE_UNASSIGNED = FaultType 1

-- No documentation found for Nested "VkFaultType" "VK_FAULT_TYPE_IMPLEMENTATION"
pattern FAULT_TYPE_IMPLEMENTATION = FaultType 2

-- No documentation found for Nested "VkFaultType" "VK_FAULT_TYPE_SYSTEM"
pattern FAULT_TYPE_SYSTEM = FaultType 3

-- No documentation found for Nested "VkFaultType" "VK_FAULT_TYPE_PHYSICAL_DEVICE"
pattern FAULT_TYPE_PHYSICAL_DEVICE = FaultType 4

-- No documentation found for Nested "VkFaultType" "VK_FAULT_TYPE_COMMAND_BUFFER_FULL"
pattern FAULT_TYPE_COMMAND_BUFFER_FULL = FaultType 5

-- No documentation found for Nested "VkFaultType" "VK_FAULT_TYPE_INVALID_API_USAGE"
pattern FAULT_TYPE_INVALID_API_USAGE = FaultType 6

{-# COMPLETE
  FAULT_TYPE_INVALID
  , FAULT_TYPE_UNASSIGNED
  , FAULT_TYPE_IMPLEMENTATION
  , FAULT_TYPE_SYSTEM
  , FAULT_TYPE_PHYSICAL_DEVICE
  , FAULT_TYPE_COMMAND_BUFFER_FULL
  , FAULT_TYPE_INVALID_API_USAGE ::
    FaultType
  #-}

conNameFaultType :: String
conNameFaultType = "FaultType"

enumPrefixFaultType :: String
enumPrefixFaultType = "FAULT_TYPE_"

showTableFaultType :: [(FaultType, String)]
showTableFaultType =
  [ (FAULT_TYPE_INVALID, "INVALID")
  , (FAULT_TYPE_UNASSIGNED, "UNASSIGNED")
  , (FAULT_TYPE_IMPLEMENTATION, "IMPLEMENTATION")
  , (FAULT_TYPE_SYSTEM, "SYSTEM")
  , (FAULT_TYPE_PHYSICAL_DEVICE, "PHYSICAL_DEVICE")
  , (FAULT_TYPE_COMMAND_BUFFER_FULL, "COMMAND_BUFFER_FULL")
  , (FAULT_TYPE_INVALID_API_USAGE, "INVALID_API_USAGE")
  ]

instance Show FaultType where
  showsPrec =
    enumShowsPrec
      enumPrefixFaultType
      showTableFaultType
      conNameFaultType
      (\(FaultType x) -> x)
      (showsPrec 11)

instance Read FaultType where
  readPrec =
    enumReadPrec
      enumPrefixFaultType
      showTableFaultType
      conNameFaultType
      FaultType
