{-# language CPP #-}
-- No documentation found for Chapter "SemaphoreType"
module Vulkan.Core12.Enums.SemaphoreType  (SemaphoreType( SEMAPHORE_TYPE_BINARY
                                                        , SEMAPHORE_TYPE_TIMELINE
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

-- | VkSemaphoreType - Specifies the type of a semaphore object
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_timeline_semaphore VK_KHR_timeline_semaphore>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_2 VK_VERSION_1_2>,
-- 'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.SemaphoreTypeCreateInfo'
newtype SemaphoreType = SemaphoreType Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'SEMAPHORE_TYPE_BINARY' specifies a /binary semaphore/ type that has a
-- boolean payload indicating whether the semaphore is currently signaled
-- or unsignaled. When created, the semaphore is in the unsignaled state.
pattern SEMAPHORE_TYPE_BINARY   = SemaphoreType 0
-- | 'SEMAPHORE_TYPE_TIMELINE' specifies a /timeline semaphore/ type that has
-- a strictly increasing 64-bit unsigned integer payload indicating whether
-- the semaphore is signaled with respect to a particular reference value.
-- When created, the semaphore payload has the value given by the
-- @initialValue@ field of
-- 'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.SemaphoreTypeCreateInfo'.
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

