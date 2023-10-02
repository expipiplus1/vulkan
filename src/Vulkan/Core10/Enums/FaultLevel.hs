{-# language CPP #-}
-- No documentation found for Chapter "FaultLevel"
module Vulkan.Core10.Enums.FaultLevel  (FaultLevel( FAULT_LEVEL_UNASSIGNED
                                                  , FAULT_LEVEL_CRITICAL
                                                  , FAULT_LEVEL_RECOVERABLE
                                                  , FAULT_LEVEL_WARNING
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

-- No documentation found for TopLevel "VkFaultLevel"
newtype FaultLevel = FaultLevel Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkFaultLevel" "VK_FAULT_LEVEL_UNASSIGNED"
pattern FAULT_LEVEL_UNASSIGNED = FaultLevel 0

-- No documentation found for Nested "VkFaultLevel" "VK_FAULT_LEVEL_CRITICAL"
pattern FAULT_LEVEL_CRITICAL = FaultLevel 1

-- No documentation found for Nested "VkFaultLevel" "VK_FAULT_LEVEL_RECOVERABLE"
pattern FAULT_LEVEL_RECOVERABLE = FaultLevel 2

-- No documentation found for Nested "VkFaultLevel" "VK_FAULT_LEVEL_WARNING"
pattern FAULT_LEVEL_WARNING = FaultLevel 3

{-# COMPLETE
  FAULT_LEVEL_UNASSIGNED
  , FAULT_LEVEL_CRITICAL
  , FAULT_LEVEL_RECOVERABLE
  , FAULT_LEVEL_WARNING ::
    FaultLevel
  #-}

conNameFaultLevel :: String
conNameFaultLevel = "FaultLevel"

enumPrefixFaultLevel :: String
enumPrefixFaultLevel = "FAULT_LEVEL_"

showTableFaultLevel :: [(FaultLevel, String)]
showTableFaultLevel =
  [ (FAULT_LEVEL_UNASSIGNED, "UNASSIGNED")
  , (FAULT_LEVEL_CRITICAL, "CRITICAL")
  , (FAULT_LEVEL_RECOVERABLE, "RECOVERABLE")
  , (FAULT_LEVEL_WARNING, "WARNING")
  ]

instance Show FaultLevel where
  showsPrec =
    enumShowsPrec
      enumPrefixFaultLevel
      showTableFaultLevel
      conNameFaultLevel
      (\(FaultLevel x) -> x)
      (showsPrec 11)

instance Read FaultLevel where
  readPrec =
    enumReadPrec
      enumPrefixFaultLevel
      showTableFaultLevel
      conNameFaultLevel
      FaultLevel
