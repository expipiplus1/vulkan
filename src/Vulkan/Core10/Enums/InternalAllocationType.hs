{-# language CPP #-}
-- No documentation found for Chapter "InternalAllocationType"
module Vulkan.Core10.Enums.InternalAllocationType  (InternalAllocationType( INTERNAL_ALLOCATION_TYPE_EXECUTABLE
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
-- No documentation found for TopLevel "VkInternalAllocationType"
newtype InternalAllocationType = InternalAllocationType Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkInternalAllocationType" "VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE"
pattern INTERNAL_ALLOCATION_TYPE_EXECUTABLE = InternalAllocationType 0
{-# complete INTERNAL_ALLOCATION_TYPE_EXECUTABLE :: InternalAllocationType #-}

conNameInternalAllocationType :: String
conNameInternalAllocationType = "InternalAllocationType"

enumPrefixInternalAllocationType :: String
enumPrefixInternalAllocationType = "INTERNAL_ALLOCATION_TYPE_EXECUTABLE"

showTableInternalAllocationType :: [(InternalAllocationType, String)]
showTableInternalAllocationType = [(INTERNAL_ALLOCATION_TYPE_EXECUTABLE, "")]


instance Show InternalAllocationType where
showsPrec = enumShowsPrec enumPrefixInternalAllocationType
                          showTableInternalAllocationType
                          conNameInternalAllocationType
                          (\(InternalAllocationType x) -> x)
                          (showsPrec 11)


instance Read InternalAllocationType where
  readPrec = enumReadPrec enumPrefixInternalAllocationType
                          showTableInternalAllocationType
                          conNameInternalAllocationType
                          InternalAllocationType

