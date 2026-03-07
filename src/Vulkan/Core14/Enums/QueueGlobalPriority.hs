{-# language CPP #-}
-- No documentation found for Chapter "QueueGlobalPriority"
module Vulkan.Core14.Enums.QueueGlobalPriority  (QueueGlobalPriority( QUEUE_GLOBAL_PRIORITY_LOW
                                                                    , QUEUE_GLOBAL_PRIORITY_MEDIUM
                                                                    , QUEUE_GLOBAL_PRIORITY_HIGH
                                                                    , QUEUE_GLOBAL_PRIORITY_REALTIME
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

-- | VkQueueGlobalPriority - Values specifying a system-wide queue priority
--
-- = Description
--
-- Priority values are sorted in ascending order. A comparison operation on
-- the enum values can be used to determine the priority order.
--
-- -   'QUEUE_GLOBAL_PRIORITY_LOW' is below the system default. Useful for
--     non-interactive tasks.
--
-- -   'QUEUE_GLOBAL_PRIORITY_MEDIUM' is the system default priority.
--
-- -   'QUEUE_GLOBAL_PRIORITY_HIGH' is above the system default.
--
-- -   'QUEUE_GLOBAL_PRIORITY_REALTIME' is the highest priority. Useful for
--     critical tasks.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_global_priority VK_EXT_global_priority>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_global_priority VK_KHR_global_priority>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core14.Promoted_From_VK_KHR_global_priority.DeviceQueueGlobalPriorityCreateInfo',
-- 'Vulkan.Core14.Promoted_From_VK_KHR_global_priority.QueueFamilyGlobalPriorityProperties'
newtype QueueGlobalPriority = QueueGlobalPriority Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- Note that the zero instance does not produce a valid value, passing 'zero' to Vulkan will result in an error

-- No documentation found for Nested "VkQueueGlobalPriority" "VK_QUEUE_GLOBAL_PRIORITY_LOW"
pattern QUEUE_GLOBAL_PRIORITY_LOW = QueueGlobalPriority 128

-- No documentation found for Nested "VkQueueGlobalPriority" "VK_QUEUE_GLOBAL_PRIORITY_MEDIUM"
pattern QUEUE_GLOBAL_PRIORITY_MEDIUM = QueueGlobalPriority 256

-- No documentation found for Nested "VkQueueGlobalPriority" "VK_QUEUE_GLOBAL_PRIORITY_HIGH"
pattern QUEUE_GLOBAL_PRIORITY_HIGH = QueueGlobalPriority 512

-- No documentation found for Nested "VkQueueGlobalPriority" "VK_QUEUE_GLOBAL_PRIORITY_REALTIME"
pattern QUEUE_GLOBAL_PRIORITY_REALTIME = QueueGlobalPriority 1024

{-# COMPLETE
  QUEUE_GLOBAL_PRIORITY_LOW
  , QUEUE_GLOBAL_PRIORITY_MEDIUM
  , QUEUE_GLOBAL_PRIORITY_HIGH
  , QUEUE_GLOBAL_PRIORITY_REALTIME ::
    QueueGlobalPriority
  #-}

conNameQueueGlobalPriority :: String
conNameQueueGlobalPriority = "QueueGlobalPriority"

enumPrefixQueueGlobalPriority :: String
enumPrefixQueueGlobalPriority = "QUEUE_GLOBAL_PRIORITY_"

showTableQueueGlobalPriority :: [(QueueGlobalPriority, String)]
showTableQueueGlobalPriority =
  [ (QUEUE_GLOBAL_PRIORITY_LOW, "LOW")
  , (QUEUE_GLOBAL_PRIORITY_MEDIUM, "MEDIUM")
  , (QUEUE_GLOBAL_PRIORITY_HIGH, "HIGH")
  , (QUEUE_GLOBAL_PRIORITY_REALTIME, "REALTIME")
  ]

instance Show QueueGlobalPriority where
  showsPrec =
    enumShowsPrec
      enumPrefixQueueGlobalPriority
      showTableQueueGlobalPriority
      conNameQueueGlobalPriority
      (\(QueueGlobalPriority x) -> x)
      (showsPrec 11)

instance Read QueueGlobalPriority where
  readPrec =
    enumReadPrec
      enumPrefixQueueGlobalPriority
      showTableQueueGlobalPriority
      conNameQueueGlobalPriority
      QueueGlobalPriority
