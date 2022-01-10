{-# language CPP #-}
-- No documentation found for Chapter "InternalAllocationType"
module Vulkan.Core10.Enums.InternalAllocationType  (InternalAllocationType( INTERNAL_ALLOCATION_TYPE_EXECUTABLE
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

-- | VkInternalAllocationType - Allocation type
--
-- = See Also
--
-- 'Vulkan.Core10.FuncPointers.PFN_vkInternalAllocationNotification',
-- 'Vulkan.Core10.FuncPointers.PFN_vkInternalFreeNotification',
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>
newtype InternalAllocationType = InternalAllocationType Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'INTERNAL_ALLOCATION_TYPE_EXECUTABLE' specifies that the allocation is
-- intended for execution by the host.
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

