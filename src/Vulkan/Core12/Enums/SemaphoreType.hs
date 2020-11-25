{-# language CPP #-}
-- No documentation found for Chapter "SemaphoreType"
module Vulkan.Core12.Enums.SemaphoreType  (SemaphoreType( SEMAPHORE_TYPE_BINARY
                                                        , SEMAPHORE_TYPE_TIMELINE
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
-- No documentation found for TopLevel "VkSemaphoreType"
newtype SemaphoreType = SemaphoreType Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkSemaphoreType" "VK_SEMAPHORE_TYPE_BINARY"
pattern SEMAPHORE_TYPE_BINARY   = SemaphoreType 0
-- No documentation found for Nested "VkSemaphoreType" "VK_SEMAPHORE_TYPE_TIMELINE"
pattern SEMAPHORE_TYPE_TIMELINE = SemaphoreType 1
{-# complete SEMAPHORE_TYPE_BINARY,
             SEMAPHORE_TYPE_TIMELINE :: SemaphoreType #-}

conNameSemaphoreType :: String
conNameSemaphoreType = "SemaphoreType"

enumPrefixSemaphoreType :: String
enumPrefixSemaphoreType = "SEMAPHORE_TYPE_"

showTableSemaphoreType :: [(SemaphoreType, String)]
showTableSemaphoreType = [(SEMAPHORE_TYPE_BINARY, "BINARY"), (SEMAPHORE_TYPE_TIMELINE, "TIMELINE")]


instance Show SemaphoreType where
showsPrec = enumShowsPrec enumPrefixSemaphoreType
                          showTableSemaphoreType
                          conNameSemaphoreType
                          (\(SemaphoreType x) -> x)
                          (showsPrec 11)


instance Read SemaphoreType where
  readPrec = enumReadPrec enumPrefixSemaphoreType showTableSemaphoreType conNameSemaphoreType SemaphoreType

