{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_16bit_storage"
module Vulkan.Core11.Promoted_From_VK_KHR_16bit_storage  ( PhysicalDevice16BitStorageFeatures(..)
                                                         , StructureType(..)
                                                         ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
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
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
-- | VkPhysicalDevice16BitStorageFeatures - Structure describing features
-- supported by VK_KHR_16bit_storage
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDevice16BitStorageFeatures' structure is included in the
-- @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDevice16BitStorageFeatures' /can/ also be used in
-- the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevice16BitStorageFeatures = PhysicalDevice16BitStorageFeatures
  { -- | #extension-features-storageBuffer16BitAccess# @storageBuffer16BitAccess@
    -- specifies whether objects in the @StorageBuffer@,
    -- @ShaderRecordBufferKHR@, or @PhysicalStorageBuffer@ storage class with
    -- the @Block@ decoration /can/ have 16-bit integer and 16-bit
    -- floating-point members. If this feature is not enabled, 16-bit integer
    -- or 16-bit floating-point members /must/ not be used in such objects.
    -- This also specifies whether shader modules /can/ declare the
    -- @StorageBuffer16BitAccess@ capability.
    storageBuffer16BitAccess :: Bool
  , -- | #extension-features-uniformAndStorageBuffer16BitAccess#
    -- @uniformAndStorageBuffer16BitAccess@ specifies whether objects in the
    -- @Uniform@ storage class with the @Block@ decoration /can/ have 16-bit
    -- integer and 16-bit floating-point members. If this feature is not
    -- enabled, 16-bit integer or 16-bit floating-point members /must/ not be
    -- used in such objects. This also specifies whether shader modules /can/
    -- declare the @UniformAndStorageBuffer16BitAccess@ capability.
    uniformAndStorageBuffer16BitAccess :: Bool
  , -- | #extension-features-storagePushConstant16# @storagePushConstant16@
    -- specifies whether objects in the @PushConstant@ storage class /can/ have
    -- 16-bit integer and 16-bit floating-point members. If this feature is not
    -- enabled, 16-bit integer or floating-point members /must/ not be used in
    -- such objects. This also specifies whether shader modules /can/ declare
    -- the @StoragePushConstant16@ capability.
    storagePushConstant16 :: Bool
  , -- | #extension-features-storageInputOutput16# @storageInputOutput16@
    -- specifies whether objects in the @Input@ and @Output@ storage classes
    -- /can/ have 16-bit integer and 16-bit floating-point members. If this
    -- feature is not enabled, 16-bit integer or 16-bit floating-point members
    -- /must/ not be used in such objects. This also specifies whether shader
    -- modules /can/ declare the @StorageInputOutput16@ capability.
    storageInputOutput16 :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevice16BitStorageFeatures)
#endif
deriving instance Show PhysicalDevice16BitStorageFeatures

instance ToCStruct PhysicalDevice16BitStorageFeatures where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevice16BitStorageFeatures{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (storageBuffer16BitAccess))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (uniformAndStorageBuffer16BitAccess))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (storagePushConstant16))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (storageInputOutput16))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDevice16BitStorageFeatures where
  peekCStruct p = do
    storageBuffer16BitAccess <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    uniformAndStorageBuffer16BitAccess <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    storagePushConstant16 <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    storageInputOutput16 <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    pure $ PhysicalDevice16BitStorageFeatures
             (bool32ToBool storageBuffer16BitAccess) (bool32ToBool uniformAndStorageBuffer16BitAccess) (bool32ToBool storagePushConstant16) (bool32ToBool storageInputOutput16)

instance Storable PhysicalDevice16BitStorageFeatures where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevice16BitStorageFeatures where
  zero = PhysicalDevice16BitStorageFeatures
           zero
           zero
           zero
           zero

