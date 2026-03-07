{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_global_priority"
module Vulkan.Core14.Promoted_From_VK_KHR_global_priority  ( DeviceQueueGlobalPriorityCreateInfo(..)
                                                           , PhysicalDeviceGlobalPriorityQueryFeatures(..)
                                                           , QueueFamilyGlobalPriorityProperties(..)
                                                           , StructureType(..)
                                                           , Result(..)
                                                           , QueueGlobalPriority(..)
                                                           , MAX_GLOBAL_PRIORITY_SIZE
                                                           , pattern MAX_GLOBAL_PRIORITY_SIZE
                                                           ) where

import Vulkan.CStruct.Utils (FixedArray)
import Control.Monad (unless)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.IO (throwIO)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.APIConstants (MAX_GLOBAL_PRIORITY_SIZE)
import Vulkan.Core14.Enums.QueueGlobalPriority (QueueGlobalPriority)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.APIConstants (pattern MAX_GLOBAL_PRIORITY_SIZE)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_GLOBAL_PRIORITY_QUERY_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_QUEUE_FAMILY_GLOBAL_PRIORITY_PROPERTIES))
import Vulkan.Core10.APIConstants (MAX_GLOBAL_PRIORITY_SIZE)
import Vulkan.Core14.Enums.QueueGlobalPriority (QueueGlobalPriority(..))
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
import Vulkan.Core10.APIConstants (pattern MAX_GLOBAL_PRIORITY_SIZE)
-- | VkDeviceQueueGlobalPriorityCreateInfo - Specify a system wide priority
--
-- = Description
--
-- Queues created without specifying 'DeviceQueueGlobalPriorityCreateInfo'
-- will default to
-- 'Vulkan.Core14.Enums.QueueGlobalPriority.QUEUE_GLOBAL_PRIORITY_MEDIUM'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_global_priority VK_EXT_global_priority>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_global_priority VK_KHR_global_priority>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core14.Enums.QueueGlobalPriority.QueueGlobalPriority',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DeviceQueueGlobalPriorityCreateInfo = DeviceQueueGlobalPriorityCreateInfo
  { -- | @globalPriority@ is the system-wide priority associated to these queues
    -- as specified by
    -- 'Vulkan.Core14.Enums.QueueGlobalPriority.QueueGlobalPriority'
    --
    -- #VUID-VkDeviceQueueGlobalPriorityCreateInfo-globalPriority-parameter#
    -- @globalPriority@ /must/ be a valid
    -- 'Vulkan.Core14.Enums.QueueGlobalPriority.QueueGlobalPriority' value
    globalPriority :: QueueGlobalPriority }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceQueueGlobalPriorityCreateInfo)
#endif
deriving instance Show DeviceQueueGlobalPriorityCreateInfo

instance ToCStruct DeviceQueueGlobalPriorityCreateInfo where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceQueueGlobalPriorityCreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr QueueGlobalPriority)) (globalPriority)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr QueueGlobalPriority)) (zero)
    f

instance FromCStruct DeviceQueueGlobalPriorityCreateInfo where
  peekCStruct p = do
    globalPriority <- peek @QueueGlobalPriority ((p `plusPtr` 16 :: Ptr QueueGlobalPriority))
    pure $ DeviceQueueGlobalPriorityCreateInfo
             globalPriority

instance Storable DeviceQueueGlobalPriorityCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeviceQueueGlobalPriorityCreateInfo where
  zero = DeviceQueueGlobalPriorityCreateInfo
           zero


-- | VkPhysicalDeviceGlobalPriorityQueryFeatures - Structure describing
-- whether global priority query can be supported by an implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceGlobalPriorityQueryFeatures' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceGlobalPriorityQueryFeatures', it /must/ add an instance
-- of the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_global_priority_query VK_EXT_global_priority_query>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_global_priority VK_KHR_global_priority>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceGlobalPriorityQueryFeatures = PhysicalDeviceGlobalPriorityQueryFeatures
  { -- | #extension-features-globalPriorityQuery# @globalPriorityQuery@ indicates
    -- whether the implementation supports the ability to query global queue
    -- priorities.
    globalPriorityQuery :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceGlobalPriorityQueryFeatures)
#endif
deriving instance Show PhysicalDeviceGlobalPriorityQueryFeatures

instance ToCStruct PhysicalDeviceGlobalPriorityQueryFeatures where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceGlobalPriorityQueryFeatures{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_GLOBAL_PRIORITY_QUERY_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (globalPriorityQuery))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_GLOBAL_PRIORITY_QUERY_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceGlobalPriorityQueryFeatures where
  peekCStruct p = do
    globalPriorityQuery <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceGlobalPriorityQueryFeatures
             (bool32ToBool globalPriorityQuery)

instance Storable PhysicalDeviceGlobalPriorityQueryFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceGlobalPriorityQueryFeatures where
  zero = PhysicalDeviceGlobalPriorityQueryFeatures
           zero


-- | VkQueueFamilyGlobalPriorityProperties - Return structure for queue
-- family global priority information query
--
-- = Description
--
-- If the 'QueueFamilyGlobalPriorityProperties' structure is included in
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
-- 'Vulkan.Core14.Enums.QueueGlobalPriority.QueueGlobalPriority' enums in
-- the ascending order.
--
-- For example, returning @priorityCount@ as 3 with supported @priorities@
-- as 'Vulkan.Core14.Enums.QueueGlobalPriority.QUEUE_GLOBAL_PRIORITY_LOW',
-- 'Vulkan.Core14.Enums.QueueGlobalPriority.QUEUE_GLOBAL_PRIORITY_MEDIUM'
-- and
-- 'Vulkan.Core14.Enums.QueueGlobalPriority.QUEUE_GLOBAL_PRIORITY_REALTIME'
-- is not allowed.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_global_priority_query VK_EXT_global_priority_query>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_global_priority VK_KHR_global_priority>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core14.Enums.QueueGlobalPriority.QueueGlobalPriority',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data QueueFamilyGlobalPriorityProperties = QueueFamilyGlobalPriorityProperties
  { -- | @priorityCount@ is the number of supported global queue priorities in
    -- this queue family, and it /must/ be greater than 0.
    priorityCount :: Word32
  , -- | @priorities@ is an array of
    -- 'Vulkan.Core10.APIConstants.MAX_GLOBAL_PRIORITY_SIZE'
    -- 'Vulkan.Core14.Enums.QueueGlobalPriority.QueueGlobalPriority' enums
    -- representing all supported global queue priorities in this queue family.
    -- The first @priorityCount@ elements of the array will be valid.
    priorities :: Vector QueueGlobalPriority
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (QueueFamilyGlobalPriorityProperties)
#endif
deriving instance Show QueueFamilyGlobalPriorityProperties

instance ToCStruct QueueFamilyGlobalPriorityProperties where
  withCStruct x f = allocaBytes 88 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p QueueFamilyGlobalPriorityProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_QUEUE_FAMILY_GLOBAL_PRIORITY_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (priorityCount)
    unless ((Data.Vector.length $ (priorities)) <= MAX_GLOBAL_PRIORITY_SIZE) $
      throwIO $ IOError Nothing InvalidArgument "" "priorities is too long, a maximum of MAX_GLOBAL_PRIORITY_SIZE elements are allowed" Nothing Nothing
    Data.Vector.imapM_ (\i e -> poke ((lowerArrayPtr ((p `plusPtr` 20 :: Ptr (FixedArray MAX_GLOBAL_PRIORITY_SIZE QueueGlobalPriority)))) `plusPtr` (4 * (i)) :: Ptr QueueGlobalPriority) (e)) (priorities)
    f
  cStructSize = 88
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_QUEUE_FAMILY_GLOBAL_PRIORITY_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct QueueFamilyGlobalPriorityProperties where
  peekCStruct p = do
    priorityCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    priorities <- generateM (MAX_GLOBAL_PRIORITY_SIZE) (\i -> peek @QueueGlobalPriority (((lowerArrayPtr @QueueGlobalPriority ((p `plusPtr` 20 :: Ptr (FixedArray MAX_GLOBAL_PRIORITY_SIZE QueueGlobalPriority)))) `advancePtrBytes` (4 * (i)) :: Ptr QueueGlobalPriority)))
    pure $ QueueFamilyGlobalPriorityProperties
             priorityCount priorities

instance Storable QueueFamilyGlobalPriorityProperties where
  sizeOf ~_ = 88
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero QueueFamilyGlobalPriorityProperties where
  zero = QueueFamilyGlobalPriorityProperties
           zero
           mempty

