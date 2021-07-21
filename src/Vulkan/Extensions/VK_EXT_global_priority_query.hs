{-# language CPP #-}
-- | = Name
--
-- VK_EXT_global_priority_query - device extension
--
-- == VK_EXT_global_priority_query
--
-- [__Name String__]
--     @VK_EXT_global_priority_query@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     389
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_EXT_global_priority@
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Contact__]
--
--     -   Yiwei Zhang
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_global_priority_query:%20&body=@zhangyiwei%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-03-29
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Yiwei Zhang, Google
--
-- == Description
--
-- This device extension allows applications to query the global queue
-- priorities supported by a queue family. It allows implementations to
-- report which global priority levels are treated differently by the
-- implementation, instead of silently mapping multiple requested global
-- priority levels to the same internal priority, or using device creation
-- failure to signal that a requested priority is not supported. It is
-- intended primarily for use by system integration along with certain
-- platform-specific priority enforcement rules.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceGlobalPriorityQueryFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.QueueFamilyProperties2':
--
--     -   'QueueFamilyGlobalPriorityPropertiesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_GLOBAL_PRIORITY_QUERY_EXTENSION_NAME'
--
-- -   'EXT_GLOBAL_PRIORITY_QUERY_SPEC_VERSION'
--
-- -   'Vulkan.Core10.APIConstants.MAX_GLOBAL_PRIORITY_SIZE_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_GLOBAL_PRIORITY_QUERY_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_QUEUE_FAMILY_GLOBAL_PRIORITY_PROPERTIES_EXT'
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
-- -   Revision 1, 2021-03-29 (Yiwei Zhang)
--
-- = See Also
--
-- 'Vulkan.Core10.APIConstants.MAX_GLOBAL_PRIORITY_SIZE_EXT',
-- 'PhysicalDeviceGlobalPriorityQueryFeaturesEXT',
-- 'QueueFamilyGlobalPriorityPropertiesEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_global_priority_query Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_global_priority_query  ( PhysicalDeviceGlobalPriorityQueryFeaturesEXT(..)
                                                       , QueueFamilyGlobalPriorityPropertiesEXT(..)
                                                       , EXT_GLOBAL_PRIORITY_QUERY_SPEC_VERSION
                                                       , pattern EXT_GLOBAL_PRIORITY_QUERY_SPEC_VERSION
                                                       , EXT_GLOBAL_PRIORITY_QUERY_EXTENSION_NAME
                                                       , pattern EXT_GLOBAL_PRIORITY_QUERY_EXTENSION_NAME
                                                       , QueueGlobalPriorityEXT(..)
                                                       , MAX_GLOBAL_PRIORITY_SIZE_EXT
                                                       , pattern MAX_GLOBAL_PRIORITY_SIZE_EXT
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
import Data.String (IsString)
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
import Vulkan.Core10.APIConstants (MAX_GLOBAL_PRIORITY_SIZE_EXT)
import Vulkan.Extensions.VK_EXT_global_priority (QueueGlobalPriorityEXT)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.APIConstants (pattern MAX_GLOBAL_PRIORITY_SIZE_EXT)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_GLOBAL_PRIORITY_QUERY_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_QUEUE_FAMILY_GLOBAL_PRIORITY_PROPERTIES_EXT))
import Vulkan.Core10.APIConstants (MAX_GLOBAL_PRIORITY_SIZE_EXT)
import Vulkan.Extensions.VK_EXT_global_priority (QueueGlobalPriorityEXT(..))
import Vulkan.Core10.APIConstants (pattern MAX_GLOBAL_PRIORITY_SIZE_EXT)
-- | VkPhysicalDeviceGlobalPriorityQueryFeaturesEXT - Structure describing
-- whether global priority query can be supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceGlobalPriorityQueryFeaturesEXT'
-- structure describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceGlobalPriorityQueryFeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceGlobalPriorityQueryFeaturesEXT' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceGlobalPriorityQueryFeaturesEXT = PhysicalDeviceGlobalPriorityQueryFeaturesEXT
  { -- | #features-globalPriorityQuery# @globalPriorityQuery@ indicates whether
    -- the implementation supports the ability to query global queue
    -- priorities.
    globalPriorityQuery :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceGlobalPriorityQueryFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceGlobalPriorityQueryFeaturesEXT

instance ToCStruct PhysicalDeviceGlobalPriorityQueryFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceGlobalPriorityQueryFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_GLOBAL_PRIORITY_QUERY_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (globalPriorityQuery))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_GLOBAL_PRIORITY_QUERY_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceGlobalPriorityQueryFeaturesEXT where
  peekCStruct p = do
    globalPriorityQuery <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceGlobalPriorityQueryFeaturesEXT
             (bool32ToBool globalPriorityQuery)

instance Storable PhysicalDeviceGlobalPriorityQueryFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceGlobalPriorityQueryFeaturesEXT where
  zero = PhysicalDeviceGlobalPriorityQueryFeaturesEXT
           zero


-- | VkQueueFamilyGlobalPriorityPropertiesEXT - Return structure for queue
-- family global priority information query
--
-- = Description
--
-- The valid elements of @priorities@ /must/ not contain any duplicate
-- values.
--
-- The valid elements of @priorities@ /must/ be a continuous sequence of
-- 'Vulkan.Extensions.VK_EXT_global_priority.QueueGlobalPriorityEXT' enums
-- in the ascending order.
--
-- Note
--
-- For example, returning @priorityCount@ as 3 with supported @priorities@
-- as
-- 'Vulkan.Extensions.VK_EXT_global_priority.QUEUE_GLOBAL_PRIORITY_LOW_EXT',
-- 'Vulkan.Extensions.VK_EXT_global_priority.QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT'
-- and
-- 'Vulkan.Extensions.VK_EXT_global_priority.QUEUE_GLOBAL_PRIORITY_REALTIME_EXT'
-- is not allowed.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkQueueFamilyGlobalPriorityPropertiesEXT-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_QUEUE_FAMILY_GLOBAL_PRIORITY_PROPERTIES_EXT'
--
-- -   #VUID-VkQueueFamilyGlobalPriorityPropertiesEXT-priorities-parameter#
--     Any given element of @priorities@ /must/ be a valid
--     'Vulkan.Extensions.VK_EXT_global_priority.QueueGlobalPriorityEXT'
--     value
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_EXT_global_priority.QueueGlobalPriorityEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data QueueFamilyGlobalPriorityPropertiesEXT = QueueFamilyGlobalPriorityPropertiesEXT
  { -- | @priorityCount@ is the number of supported global queue priorities in
    -- this queue family, and it /must/ be greater than 0.
    priorityCount :: Word32
  , -- | @priorities@ is an array of
    -- 'Vulkan.Core10.APIConstants.MAX_GLOBAL_PRIORITY_SIZE_EXT'
    -- 'Vulkan.Extensions.VK_EXT_global_priority.QueueGlobalPriorityEXT' enums
    -- representing all supported global queue priorities in this queue family.
    -- The first @priorityCount@ elements of the array will be valid.
    priorities :: Vector QueueGlobalPriorityEXT
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (QueueFamilyGlobalPriorityPropertiesEXT)
#endif
deriving instance Show QueueFamilyGlobalPriorityPropertiesEXT

instance ToCStruct QueueFamilyGlobalPriorityPropertiesEXT where
  withCStruct x f = allocaBytes 88 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p QueueFamilyGlobalPriorityPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_QUEUE_FAMILY_GLOBAL_PRIORITY_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (priorityCount)
    unless ((Data.Vector.length $ (priorities)) <= MAX_GLOBAL_PRIORITY_SIZE_EXT) $
      throwIO $ IOError Nothing InvalidArgument "" "priorities is too long, a maximum of MAX_GLOBAL_PRIORITY_SIZE_EXT elements are allowed" Nothing Nothing
    Data.Vector.imapM_ (\i e -> poke ((lowerArrayPtr ((p `plusPtr` 20 :: Ptr (FixedArray MAX_GLOBAL_PRIORITY_SIZE_EXT QueueGlobalPriorityEXT)))) `plusPtr` (4 * (i)) :: Ptr QueueGlobalPriorityEXT) (e)) (priorities)
    f
  cStructSize = 88
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_QUEUE_FAMILY_GLOBAL_PRIORITY_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct QueueFamilyGlobalPriorityPropertiesEXT where
  peekCStruct p = do
    priorityCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    priorities <- generateM (MAX_GLOBAL_PRIORITY_SIZE_EXT) (\i -> peek @QueueGlobalPriorityEXT (((lowerArrayPtr @QueueGlobalPriorityEXT ((p `plusPtr` 20 :: Ptr (FixedArray MAX_GLOBAL_PRIORITY_SIZE_EXT QueueGlobalPriorityEXT)))) `advancePtrBytes` (4 * (i)) :: Ptr QueueGlobalPriorityEXT)))
    pure $ QueueFamilyGlobalPriorityPropertiesEXT
             priorityCount priorities

instance Storable QueueFamilyGlobalPriorityPropertiesEXT where
  sizeOf ~_ = 88
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero QueueFamilyGlobalPriorityPropertiesEXT where
  zero = QueueFamilyGlobalPriorityPropertiesEXT
           zero
           mempty


type EXT_GLOBAL_PRIORITY_QUERY_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_GLOBAL_PRIORITY_QUERY_SPEC_VERSION"
pattern EXT_GLOBAL_PRIORITY_QUERY_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_GLOBAL_PRIORITY_QUERY_SPEC_VERSION = 1


type EXT_GLOBAL_PRIORITY_QUERY_EXTENSION_NAME = "VK_EXT_global_priority_query"

-- No documentation found for TopLevel "VK_EXT_GLOBAL_PRIORITY_QUERY_EXTENSION_NAME"
pattern EXT_GLOBAL_PRIORITY_QUERY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_GLOBAL_PRIORITY_QUERY_EXTENSION_NAME = "VK_EXT_global_priority_query"

