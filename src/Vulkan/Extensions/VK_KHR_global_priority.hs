{-# language CPP #-}
-- | = Name
--
-- VK_KHR_global_priority - device extension
--
-- == VK_KHR_global_priority
--
-- [__Name String__]
--     @VK_KHR_global_priority@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     189
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
-- [__Contact__]
--
--     -   Tobias Hector
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_global_priority] @tobski%0A*Here describe the issue or question you have about the VK_KHR_global_priority extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-10-22
--
-- [__Contributors__]
--
--     -   Tobias Hector, AMD
--
--     -   Contributors to @VK_EXT_global_priority@
--
--     -   Contributors to @VK_EXT_global_priority_query@
--
-- == Description
--
-- In Vulkan, users can specify device-scope queue priorities. In some
-- cases it may be useful to extend this concept to a system-wide scope.
-- This device extension allows applications to query the global queue
-- priorities supported by a queue family, and then set a priority when
-- creating queues. The default queue priority is
-- 'QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT'.
--
-- Implementations can report which global priority levels are treated
-- differently by the implementation. It is intended primarily for use in
-- system integration along with certain platform-specific priority
-- enforcement rules.
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
-- 'Vulkan.Extensions.VK_EXT_global_priority.ERROR_NOT_PERMITTED_EXT' is
-- returned.
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
--     -   'DeviceQueueGlobalPriorityCreateInfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceGlobalPriorityQueryFeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.QueueFamilyProperties2':
--
--     -   'QueueFamilyGlobalPriorityPropertiesKHR'
--
-- == New Enums
--
-- -   'QueueGlobalPriorityKHR'
--
-- == New Enum Constants
--
-- -   'KHR_GLOBAL_PRIORITY_EXTENSION_NAME'
--
-- -   'KHR_GLOBAL_PRIORITY_SPEC_VERSION'
--
-- -   'Vulkan.Core10.APIConstants.MAX_GLOBAL_PRIORITY_SIZE_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.Result.Result':
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_NOT_PERMITTED_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_GLOBAL_PRIORITY_QUERY_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_QUEUE_FAMILY_GLOBAL_PRIORITY_PROPERTIES_KHR'
--
-- == Issues
--
-- 1) Can we additionally query whether a caller is permitted to acquire a
-- specific global queue priority in this extension?
--
-- __RESOLVED__: No. Whether a caller has enough privilege goes with the
-- OS, and the Vulkan driver cannot really guarantee that the privilege
-- will not change in between this query and the actual queue creation
-- call.
--
-- 2) If more than 1 queue using global priority is requested, is there a
-- good way to know which queue is failing the device creation?
--
-- __RESOLVED__: No. There is not a good way at this moment, and it is also
-- not quite actionable for the applications to know that because the
-- information may not be accurate. Queue creation can fail because of
-- runtime constraints like insufficient privilege or lack of resource, and
-- the failure is not necessarily tied to that particular queue
-- configuration requested.
--
-- == Version History
--
-- -   Revision 1, 2021-10-22 (Tobias Hector)
--
--     -   Initial draft
--
-- == See Also
--
-- 'Vulkan.Core10.APIConstants.MAX_GLOBAL_PRIORITY_SIZE_KHR',
-- 'DeviceQueueGlobalPriorityCreateInfoKHR',
-- 'PhysicalDeviceGlobalPriorityQueryFeaturesKHR',
-- 'QueueFamilyGlobalPriorityPropertiesKHR', 'QueueGlobalPriorityKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_global_priority Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_global_priority  ( pattern QUEUE_GLOBAL_PRIORITY_LOW_EXT
                                                 , pattern QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT
                                                 , pattern QUEUE_GLOBAL_PRIORITY_HIGH_EXT
                                                 , pattern QUEUE_GLOBAL_PRIORITY_REALTIME_EXT
                                                 , DeviceQueueGlobalPriorityCreateInfoKHR(..)
                                                 , PhysicalDeviceGlobalPriorityQueryFeaturesKHR(..)
                                                 , QueueFamilyGlobalPriorityPropertiesKHR(..)
                                                 , QueueGlobalPriorityKHR( QUEUE_GLOBAL_PRIORITY_LOW_KHR
                                                                         , QUEUE_GLOBAL_PRIORITY_MEDIUM_KHR
                                                                         , QUEUE_GLOBAL_PRIORITY_HIGH_KHR
                                                                         , QUEUE_GLOBAL_PRIORITY_REALTIME_KHR
                                                                         , ..
                                                                         )
                                                 , KHR_GLOBAL_PRIORITY_SPEC_VERSION
                                                 , pattern KHR_GLOBAL_PRIORITY_SPEC_VERSION
                                                 , KHR_GLOBAL_PRIORITY_EXTENSION_NAME
                                                 , pattern KHR_GLOBAL_PRIORITY_EXTENSION_NAME
                                                 , MAX_GLOBAL_PRIORITY_SIZE_KHR
                                                 , pattern MAX_GLOBAL_PRIORITY_SIZE_KHR
                                                 ) where

import Vulkan.CStruct.Utils (FixedArray)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Control.Monad (unless)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.IO (throwIO)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
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
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Kind (Type)
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.APIConstants (MAX_GLOBAL_PRIORITY_SIZE_KHR)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.APIConstants (pattern MAX_GLOBAL_PRIORITY_SIZE_KHR)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_GLOBAL_PRIORITY_QUERY_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_QUEUE_FAMILY_GLOBAL_PRIORITY_PROPERTIES_KHR))
import Vulkan.Core10.APIConstants (MAX_GLOBAL_PRIORITY_SIZE_KHR)
import Vulkan.Core10.APIConstants (pattern MAX_GLOBAL_PRIORITY_SIZE_KHR)
-- No documentation found for TopLevel "VK_QUEUE_GLOBAL_PRIORITY_LOW_EXT"
pattern QUEUE_GLOBAL_PRIORITY_LOW_EXT = QUEUE_GLOBAL_PRIORITY_LOW_KHR


-- No documentation found for TopLevel "VK_QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT"
pattern QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT = QUEUE_GLOBAL_PRIORITY_MEDIUM_KHR


-- No documentation found for TopLevel "VK_QUEUE_GLOBAL_PRIORITY_HIGH_EXT"
pattern QUEUE_GLOBAL_PRIORITY_HIGH_EXT = QUEUE_GLOBAL_PRIORITY_HIGH_KHR


-- No documentation found for TopLevel "VK_QUEUE_GLOBAL_PRIORITY_REALTIME_EXT"
pattern QUEUE_GLOBAL_PRIORITY_REALTIME_EXT = QUEUE_GLOBAL_PRIORITY_REALTIME_KHR


-- | VkDeviceQueueGlobalPriorityCreateInfoKHR - Specify a system wide
-- priority
--
-- = Description
--
-- Queues created without specifying
-- 'DeviceQueueGlobalPriorityCreateInfoKHR' will default to
-- 'QUEUE_GLOBAL_PRIORITY_MEDIUM_KHR'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_global_priority VK_EXT_global_priority>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_global_priority VK_KHR_global_priority>,
-- 'QueueGlobalPriorityKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DeviceQueueGlobalPriorityCreateInfoKHR = DeviceQueueGlobalPriorityCreateInfoKHR
  { -- | @globalPriority@ is the system-wide priority associated to these queues
    -- as specified by
    -- 'Vulkan.Extensions.VK_EXT_global_priority.QueueGlobalPriorityEXT'
    --
    -- #VUID-VkDeviceQueueGlobalPriorityCreateInfoKHR-globalPriority-parameter#
    -- @globalPriority@ /must/ be a valid 'QueueGlobalPriorityKHR' value
    globalPriority :: QueueGlobalPriorityKHR }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceQueueGlobalPriorityCreateInfoKHR)
#endif
deriving instance Show DeviceQueueGlobalPriorityCreateInfoKHR

instance ToCStruct DeviceQueueGlobalPriorityCreateInfoKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceQueueGlobalPriorityCreateInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr QueueGlobalPriorityKHR)) (globalPriority)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr QueueGlobalPriorityKHR)) (zero)
    f

instance FromCStruct DeviceQueueGlobalPriorityCreateInfoKHR where
  peekCStruct p = do
    globalPriority <- peek @QueueGlobalPriorityKHR ((p `plusPtr` 16 :: Ptr QueueGlobalPriorityKHR))
    pure $ DeviceQueueGlobalPriorityCreateInfoKHR
             globalPriority

instance Storable DeviceQueueGlobalPriorityCreateInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeviceQueueGlobalPriorityCreateInfoKHR where
  zero = DeviceQueueGlobalPriorityCreateInfoKHR
           zero


-- | VkPhysicalDeviceGlobalPriorityQueryFeaturesKHR - Structure describing
-- whether global priority query can be supported by an implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceGlobalPriorityQueryFeaturesKHR' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceGlobalPriorityQueryFeaturesKHR' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_global_priority VK_KHR_global_priority>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceGlobalPriorityQueryFeaturesKHR = PhysicalDeviceGlobalPriorityQueryFeaturesKHR
  { -- | #features-globalPriorityQuery# @globalPriorityQuery@ indicates whether
    -- the implementation supports the ability to query global queue
    -- priorities.
    globalPriorityQuery :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceGlobalPriorityQueryFeaturesKHR)
#endif
deriving instance Show PhysicalDeviceGlobalPriorityQueryFeaturesKHR

instance ToCStruct PhysicalDeviceGlobalPriorityQueryFeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceGlobalPriorityQueryFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_GLOBAL_PRIORITY_QUERY_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (globalPriorityQuery))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_GLOBAL_PRIORITY_QUERY_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceGlobalPriorityQueryFeaturesKHR where
  peekCStruct p = do
    globalPriorityQuery <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceGlobalPriorityQueryFeaturesKHR
             (bool32ToBool globalPriorityQuery)

instance Storable PhysicalDeviceGlobalPriorityQueryFeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceGlobalPriorityQueryFeaturesKHR where
  zero = PhysicalDeviceGlobalPriorityQueryFeaturesKHR
           zero


-- | VkQueueFamilyGlobalPriorityPropertiesKHR - Return structure for queue
-- family global priority information query
--
-- = Description
--
-- If the 'QueueFamilyGlobalPriorityPropertiesKHR' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.QueueFamilyProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceQueueFamilyProperties2',
-- it is filled in with the list of supported global queue priorities for
-- the indicated family.
--
-- The valid elements of @priorities@ /must/ not contain any duplicate
-- values.
--
-- The valid elements of @priorities@ /must/ be a continuous sequence of
-- 'QueueGlobalPriorityKHR' enums in the ascending order.
--
-- Note
--
-- For example, returning @priorityCount@ as 3 with supported @priorities@
-- as 'QUEUE_GLOBAL_PRIORITY_LOW_KHR', 'QUEUE_GLOBAL_PRIORITY_MEDIUM_KHR'
-- and 'QUEUE_GLOBAL_PRIORITY_REALTIME_KHR' is not allowed.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkQueueFamilyGlobalPriorityPropertiesKHR-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_QUEUE_FAMILY_GLOBAL_PRIORITY_PROPERTIES_KHR'
--
-- -   #VUID-VkQueueFamilyGlobalPriorityPropertiesKHR-priorities-parameter#
--     Any given element of @priorities@ /must/ be a valid
--     'QueueGlobalPriorityKHR' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_global_priority VK_KHR_global_priority>,
-- 'QueueGlobalPriorityKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data QueueFamilyGlobalPriorityPropertiesKHR = QueueFamilyGlobalPriorityPropertiesKHR
  { -- | @priorityCount@ is the number of supported global queue priorities in
    -- this queue family, and it /must/ be greater than 0.
    priorityCount :: Word32
  , -- | @priorities@ is an array of
    -- 'Vulkan.Core10.APIConstants.MAX_GLOBAL_PRIORITY_SIZE_EXT'
    -- 'Vulkan.Extensions.VK_EXT_global_priority.QueueGlobalPriorityEXT' enums
    -- representing all supported global queue priorities in this queue family.
    -- The first @priorityCount@ elements of the array will be valid.
    priorities :: Vector QueueGlobalPriorityKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (QueueFamilyGlobalPriorityPropertiesKHR)
#endif
deriving instance Show QueueFamilyGlobalPriorityPropertiesKHR

instance ToCStruct QueueFamilyGlobalPriorityPropertiesKHR where
  withCStruct x f = allocaBytes 88 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p QueueFamilyGlobalPriorityPropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_QUEUE_FAMILY_GLOBAL_PRIORITY_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (priorityCount)
    unless ((Data.Vector.length $ (priorities)) <= MAX_GLOBAL_PRIORITY_SIZE_KHR) $
      throwIO $ IOError Nothing InvalidArgument "" "priorities is too long, a maximum of MAX_GLOBAL_PRIORITY_SIZE_KHR elements are allowed" Nothing Nothing
    Data.Vector.imapM_ (\i e -> poke ((lowerArrayPtr ((p `plusPtr` 20 :: Ptr (FixedArray MAX_GLOBAL_PRIORITY_SIZE_KHR QueueGlobalPriorityKHR)))) `plusPtr` (4 * (i)) :: Ptr QueueGlobalPriorityKHR) (e)) (priorities)
    f
  cStructSize = 88
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_QUEUE_FAMILY_GLOBAL_PRIORITY_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct QueueFamilyGlobalPriorityPropertiesKHR where
  peekCStruct p = do
    priorityCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    priorities <- generateM (MAX_GLOBAL_PRIORITY_SIZE_KHR) (\i -> peek @QueueGlobalPriorityKHR (((lowerArrayPtr @QueueGlobalPriorityKHR ((p `plusPtr` 20 :: Ptr (FixedArray MAX_GLOBAL_PRIORITY_SIZE_KHR QueueGlobalPriorityKHR)))) `advancePtrBytes` (4 * (i)) :: Ptr QueueGlobalPriorityKHR)))
    pure $ QueueFamilyGlobalPriorityPropertiesKHR
             priorityCount priorities

instance Storable QueueFamilyGlobalPriorityPropertiesKHR where
  sizeOf ~_ = 88
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero QueueFamilyGlobalPriorityPropertiesKHR where
  zero = QueueFamilyGlobalPriorityPropertiesKHR
           zero
           mempty


-- | VkQueueGlobalPriorityKHR - Values specifying a system-wide queue
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_global_priority VK_KHR_global_priority>,
-- 'DeviceQueueGlobalPriorityCreateInfoKHR',
-- 'QueueFamilyGlobalPriorityPropertiesKHR'
newtype QueueGlobalPriorityKHR = QueueGlobalPriorityKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- Note that the zero instance does not produce a valid value, passing 'zero' to Vulkan will result in an error

-- | 'QUEUE_GLOBAL_PRIORITY_LOW_KHR' is below the system default. Useful for
-- non-interactive tasks.
pattern QUEUE_GLOBAL_PRIORITY_LOW_KHR = QueueGlobalPriorityKHR 128

-- | 'QUEUE_GLOBAL_PRIORITY_MEDIUM_KHR' is the system default priority.
pattern QUEUE_GLOBAL_PRIORITY_MEDIUM_KHR = QueueGlobalPriorityKHR 256

-- | 'QUEUE_GLOBAL_PRIORITY_HIGH_KHR' is above the system default.
pattern QUEUE_GLOBAL_PRIORITY_HIGH_KHR = QueueGlobalPriorityKHR 512

-- | 'QUEUE_GLOBAL_PRIORITY_REALTIME_KHR' is the highest priority. Useful for
-- critical tasks.
pattern QUEUE_GLOBAL_PRIORITY_REALTIME_KHR = QueueGlobalPriorityKHR 1024

{-# COMPLETE
  QUEUE_GLOBAL_PRIORITY_LOW_KHR
  , QUEUE_GLOBAL_PRIORITY_MEDIUM_KHR
  , QUEUE_GLOBAL_PRIORITY_HIGH_KHR
  , QUEUE_GLOBAL_PRIORITY_REALTIME_KHR ::
    QueueGlobalPriorityKHR
  #-}

conNameQueueGlobalPriorityKHR :: String
conNameQueueGlobalPriorityKHR = "QueueGlobalPriorityKHR"

enumPrefixQueueGlobalPriorityKHR :: String
enumPrefixQueueGlobalPriorityKHR = "QUEUE_GLOBAL_PRIORITY_"

showTableQueueGlobalPriorityKHR :: [(QueueGlobalPriorityKHR, String)]
showTableQueueGlobalPriorityKHR =
  [ (QUEUE_GLOBAL_PRIORITY_LOW_KHR, "LOW_KHR")
  ,
    ( QUEUE_GLOBAL_PRIORITY_MEDIUM_KHR
    , "MEDIUM_KHR"
    )
  , (QUEUE_GLOBAL_PRIORITY_HIGH_KHR, "HIGH_KHR")
  ,
    ( QUEUE_GLOBAL_PRIORITY_REALTIME_KHR
    , "REALTIME_KHR"
    )
  ]

instance Show QueueGlobalPriorityKHR where
  showsPrec =
    enumShowsPrec
      enumPrefixQueueGlobalPriorityKHR
      showTableQueueGlobalPriorityKHR
      conNameQueueGlobalPriorityKHR
      (\(QueueGlobalPriorityKHR x) -> x)
      (showsPrec 11)

instance Read QueueGlobalPriorityKHR where
  readPrec =
    enumReadPrec
      enumPrefixQueueGlobalPriorityKHR
      showTableQueueGlobalPriorityKHR
      conNameQueueGlobalPriorityKHR
      QueueGlobalPriorityKHR

type KHR_GLOBAL_PRIORITY_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_GLOBAL_PRIORITY_SPEC_VERSION"
pattern KHR_GLOBAL_PRIORITY_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_GLOBAL_PRIORITY_SPEC_VERSION = 1


type KHR_GLOBAL_PRIORITY_EXTENSION_NAME = "VK_KHR_global_priority"

-- No documentation found for TopLevel "VK_KHR_GLOBAL_PRIORITY_EXTENSION_NAME"
pattern KHR_GLOBAL_PRIORITY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_GLOBAL_PRIORITY_EXTENSION_NAME = "VK_KHR_global_priority"

