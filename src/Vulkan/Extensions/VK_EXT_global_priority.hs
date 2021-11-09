{-# language CPP #-}
-- | = Name
--
-- VK_EXT_global_priority - device extension
--
-- == VK_EXT_global_priority
--
-- [__Name String__]
--     @VK_EXT_global_priority@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     175
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Contact__]
--
--     -   Andres Rodriguez
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_global_priority] @lostgoat%0A<<Here describe the issue or question you have about the VK_EXT_global_priority extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-10-06
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Andres Rodriguez, Valve
--
--     -   Pierre-Loup Griffais, Valve
--
--     -   Dan Ginsburg, Valve
--
--     -   Mitch Singer, AMD
--
-- == Description
--
-- In Vulkan, users can specify device-scope queue priorities. In some
-- cases it may be useful to extend this concept to a system-wide scope.
-- This extension provides a mechanism for callers to set their system-wide
-- priority. The default queue priority is
-- 'QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT'.
--
-- The driver implementation will attempt to skew hardware resource
-- allocation in favour of the higher-priority task. Therefore,
-- higher-priority work may retain similar latency and throughput
-- characteristics even if the system is congested with lower priority
-- work.
--
-- The global priority level of a queue shall take precedence over the
-- per-process queue priority
-- ('Vulkan.Core10.Device.DeviceQueueCreateInfo'::@pQueuePriorities@).
--
-- Abuse of this feature may result in starving the rest of the system from
-- hardware resources. Therefore, the driver implementation may deny
-- requests to acquire a priority above the default priority
-- ('QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT') if the caller does not have
-- sufficient privileges. In this scenario
-- 'Vulkan.Core10.Enums.Result.ERROR_NOT_PERMITTED_EXT' is returned.
--
-- The driver implementation may fail the queue allocation request if
-- resources required to complete the operation have been exhausted (either
-- by the same process or a different process). In this scenario
-- 'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED' is returned.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.Device.DeviceQueueCreateInfo':
--
--     -   'DeviceQueueGlobalPriorityCreateInfoEXT'
--
-- == New Enums
--
-- -   'QueueGlobalPriorityEXT'
--
-- == New Enum Constants
--
-- -   'EXT_GLOBAL_PRIORITY_EXTENSION_NAME'
--
-- -   'EXT_GLOBAL_PRIORITY_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.Result.Result':
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_NOT_PERMITTED_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT'
--
-- == Version History
--
-- -   Revision 2, 2017-11-03 (Andres Rodriguez)
--
--     -   Fixed VkQueueGlobalPriorityEXT missing _EXT suffix
--
-- -   Revision 1, 2017-10-06 (Andres Rodriguez)
--
--     -   First version.
--
-- == See Also
--
-- 'DeviceQueueGlobalPriorityCreateInfoEXT', 'QueueGlobalPriorityEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_global_priority Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_global_priority  ( DeviceQueueGlobalPriorityCreateInfoEXT(..)
                                                 , QueueGlobalPriorityEXT( QUEUE_GLOBAL_PRIORITY_LOW_EXT
                                                                         , QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT
                                                                         , QUEUE_GLOBAL_PRIORITY_HIGH_EXT
                                                                         , QUEUE_GLOBAL_PRIORITY_REALTIME_EXT
                                                                         , ..
                                                                         )
                                                 , EXT_GLOBAL_PRIORITY_SPEC_VERSION
                                                 , pattern EXT_GLOBAL_PRIORITY_SPEC_VERSION
                                                 , EXT_GLOBAL_PRIORITY_EXTENSION_NAME
                                                 , pattern EXT_GLOBAL_PRIORITY_EXTENSION_NAME
                                                 ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Data.Int (Int32)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Kind (Type)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT))
-- | VkDeviceQueueGlobalPriorityCreateInfoEXT - Specify a system wide
-- priority
--
-- = Description
--
-- A queue created without specifying
-- 'DeviceQueueGlobalPriorityCreateInfoEXT' will default to
-- 'QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_global_priority VK_EXT_global_priority>,
-- 'QueueGlobalPriorityEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DeviceQueueGlobalPriorityCreateInfoEXT = DeviceQueueGlobalPriorityCreateInfoEXT
  { -- | @globalPriority@ is the system-wide priority associated to this queue as
    -- specified by 'QueueGlobalPriorityEXT'
    --
    -- #VUID-VkDeviceQueueGlobalPriorityCreateInfoEXT-globalPriority-parameter#
    -- @globalPriority@ /must/ be a valid 'QueueGlobalPriorityEXT' value
    globalPriority :: QueueGlobalPriorityEXT }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceQueueGlobalPriorityCreateInfoEXT)
#endif
deriving instance Show DeviceQueueGlobalPriorityCreateInfoEXT

instance ToCStruct DeviceQueueGlobalPriorityCreateInfoEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceQueueGlobalPriorityCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr QueueGlobalPriorityEXT)) (globalPriority)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr QueueGlobalPriorityEXT)) (zero)
    f

instance FromCStruct DeviceQueueGlobalPriorityCreateInfoEXT where
  peekCStruct p = do
    globalPriority <- peek @QueueGlobalPriorityEXT ((p `plusPtr` 16 :: Ptr QueueGlobalPriorityEXT))
    pure $ DeviceQueueGlobalPriorityCreateInfoEXT
             globalPriority

instance Storable DeviceQueueGlobalPriorityCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeviceQueueGlobalPriorityCreateInfoEXT where
  zero = DeviceQueueGlobalPriorityCreateInfoEXT
           zero


-- | VkQueueGlobalPriorityEXT - Values specifying a system-wide queue
-- priority
--
-- = Description
--
-- Priority values are sorted in ascending order. A comparison operation on
-- the enum values can be used to determine the priority order.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_global_priority VK_EXT_global_priority>,
-- 'DeviceQueueGlobalPriorityCreateInfoEXT',
-- 'Vulkan.Extensions.VK_EXT_global_priority_query.QueueFamilyGlobalPriorityPropertiesEXT'
newtype QueueGlobalPriorityEXT = QueueGlobalPriorityEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)
-- Note that the zero instance does not produce a valid value, passing 'zero' to Vulkan will result in an error

-- | 'QUEUE_GLOBAL_PRIORITY_LOW_EXT' is below the system default. Useful for
-- non-interactive tasks.
pattern QUEUE_GLOBAL_PRIORITY_LOW_EXT      = QueueGlobalPriorityEXT 128
-- | 'QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT' is the system default priority.
pattern QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT   = QueueGlobalPriorityEXT 256
-- | 'QUEUE_GLOBAL_PRIORITY_HIGH_EXT' is above the system default.
pattern QUEUE_GLOBAL_PRIORITY_HIGH_EXT     = QueueGlobalPriorityEXT 512
-- | 'QUEUE_GLOBAL_PRIORITY_REALTIME_EXT' is the highest priority. Useful for
-- critical tasks.
pattern QUEUE_GLOBAL_PRIORITY_REALTIME_EXT = QueueGlobalPriorityEXT 1024
{-# complete QUEUE_GLOBAL_PRIORITY_LOW_EXT,
             QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT,
             QUEUE_GLOBAL_PRIORITY_HIGH_EXT,
             QUEUE_GLOBAL_PRIORITY_REALTIME_EXT :: QueueGlobalPriorityEXT #-}

conNameQueueGlobalPriorityEXT :: String
conNameQueueGlobalPriorityEXT = "QueueGlobalPriorityEXT"

enumPrefixQueueGlobalPriorityEXT :: String
enumPrefixQueueGlobalPriorityEXT = "QUEUE_GLOBAL_PRIORITY_"

showTableQueueGlobalPriorityEXT :: [(QueueGlobalPriorityEXT, String)]
showTableQueueGlobalPriorityEXT =
  [ (QUEUE_GLOBAL_PRIORITY_LOW_EXT     , "LOW_EXT")
  , (QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT  , "MEDIUM_EXT")
  , (QUEUE_GLOBAL_PRIORITY_HIGH_EXT    , "HIGH_EXT")
  , (QUEUE_GLOBAL_PRIORITY_REALTIME_EXT, "REALTIME_EXT")
  ]

instance Show QueueGlobalPriorityEXT where
  showsPrec = enumShowsPrec enumPrefixQueueGlobalPriorityEXT
                            showTableQueueGlobalPriorityEXT
                            conNameQueueGlobalPriorityEXT
                            (\(QueueGlobalPriorityEXT x) -> x)
                            (showsPrec 11)

instance Read QueueGlobalPriorityEXT where
  readPrec = enumReadPrec enumPrefixQueueGlobalPriorityEXT
                          showTableQueueGlobalPriorityEXT
                          conNameQueueGlobalPriorityEXT
                          QueueGlobalPriorityEXT


type EXT_GLOBAL_PRIORITY_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_EXT_GLOBAL_PRIORITY_SPEC_VERSION"
pattern EXT_GLOBAL_PRIORITY_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_GLOBAL_PRIORITY_SPEC_VERSION = 2


type EXT_GLOBAL_PRIORITY_EXTENSION_NAME = "VK_EXT_global_priority"

-- No documentation found for TopLevel "VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME"
pattern EXT_GLOBAL_PRIORITY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_GLOBAL_PRIORITY_EXTENSION_NAME = "VK_EXT_global_priority"

