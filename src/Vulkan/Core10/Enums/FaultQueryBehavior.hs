{-# language CPP #-}
-- No documentation found for Chapter "FaultQueryBehavior"
module Vulkan.Core10.Enums.FaultQueryBehavior  (FaultQueryBehavior( FAULT_QUERY_BEHAVIOR_GET_AND_CLEAR_ALL_FAULTS
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

-- No documentation found for TopLevel "VkFaultQueryBehavior"
newtype FaultQueryBehavior = FaultQueryBehavior Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkFaultQueryBehavior" "VK_FAULT_QUERY_BEHAVIOR_GET_AND_CLEAR_ALL_FAULTS"
pattern FAULT_QUERY_BEHAVIOR_GET_AND_CLEAR_ALL_FAULTS = FaultQueryBehavior 0

{-# COMPLETE FAULT_QUERY_BEHAVIOR_GET_AND_CLEAR_ALL_FAULTS :: FaultQueryBehavior #-}

conNameFaultQueryBehavior :: String
conNameFaultQueryBehavior = "FaultQueryBehavior"

enumPrefixFaultQueryBehavior :: String
enumPrefixFaultQueryBehavior = "FAULT_QUERY_BEHAVIOR_GET_AND_CLEAR_ALL_FAULTS"

showTableFaultQueryBehavior :: [(FaultQueryBehavior, String)]
showTableFaultQueryBehavior =
  [
    ( FAULT_QUERY_BEHAVIOR_GET_AND_CLEAR_ALL_FAULTS
    , ""
    )
  ]

instance Show FaultQueryBehavior where
  showsPrec =
    enumShowsPrec
      enumPrefixFaultQueryBehavior
      showTableFaultQueryBehavior
      conNameFaultQueryBehavior
      (\(FaultQueryBehavior x) -> x)
      (showsPrec 11)

instance Read FaultQueryBehavior where
  readPrec =
    enumReadPrec
      enumPrefixFaultQueryBehavior
      showTableFaultQueryBehavior
      conNameFaultQueryBehavior
      FaultQueryBehavior
