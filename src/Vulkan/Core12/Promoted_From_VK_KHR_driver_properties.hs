{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_driver_properties"
module Vulkan.Core12.Promoted_From_VK_KHR_driver_properties  ( ConformanceVersion(..)
                                                             , PhysicalDeviceDriverProperties(..)
                                                             , StructureType(..)
                                                             , DriverId(..)
                                                             , MAX_DRIVER_NAME_SIZE
                                                             , pattern MAX_DRIVER_NAME_SIZE
                                                             , MAX_DRIVER_INFO_SIZE
                                                             , pattern MAX_DRIVER_INFO_SIZE
                                                             ) where

import Vulkan.CStruct.Utils (FixedArray)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.ByteString (packCString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Word (Word8)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.CStruct.Utils (pokeFixedLengthNullTerminatedByteString)
import Vulkan.Core12.Enums.DriverId (DriverId)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.APIConstants (MAX_DRIVER_INFO_SIZE)
import Vulkan.Core10.APIConstants (MAX_DRIVER_NAME_SIZE)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES))
import Vulkan.Core12.Enums.DriverId (DriverId(..))
import Vulkan.Core10.APIConstants (MAX_DRIVER_INFO_SIZE)
import Vulkan.Core10.APIConstants (MAX_DRIVER_NAME_SIZE)
import Vulkan.Core10.Enums.StructureType (StructureType(..))
import Vulkan.Core10.APIConstants (pattern MAX_DRIVER_INFO_SIZE)
import Vulkan.Core10.APIConstants (pattern MAX_DRIVER_NAME_SIZE)

-- No documentation found for TopLevel "VkConformanceVersion"
data ConformanceVersion = ConformanceVersion
  { -- No documentation found for Nested "VkConformanceVersion" "major"
    major :: Word8
  , -- No documentation found for Nested "VkConformanceVersion" "minor"
    minor :: Word8
  , -- No documentation found for Nested "VkConformanceVersion" "subminor"
    subminor :: Word8
  , -- No documentation found for Nested "VkConformanceVersion" "patch"
    patch :: Word8
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ConformanceVersion)
#endif
deriving instance Show ConformanceVersion

instance ToCStruct ConformanceVersion where
  withCStruct x f = allocaBytesAligned 4 1 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ConformanceVersion{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word8)) (major)
    poke ((p `plusPtr` 1 :: Ptr Word8)) (minor)
    poke ((p `plusPtr` 2 :: Ptr Word8)) (subminor)
    poke ((p `plusPtr` 3 :: Ptr Word8)) (patch)
    f
  cStructSize = 4
  cStructAlignment = 1
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word8)) (zero)
    poke ((p `plusPtr` 1 :: Ptr Word8)) (zero)
    poke ((p `plusPtr` 2 :: Ptr Word8)) (zero)
    poke ((p `plusPtr` 3 :: Ptr Word8)) (zero)
    f

instance FromCStruct ConformanceVersion where
  peekCStruct p = do
    major <- peek @Word8 ((p `plusPtr` 0 :: Ptr Word8))
    minor <- peek @Word8 ((p `plusPtr` 1 :: Ptr Word8))
    subminor <- peek @Word8 ((p `plusPtr` 2 :: Ptr Word8))
    patch <- peek @Word8 ((p `plusPtr` 3 :: Ptr Word8))
    pure $ ConformanceVersion
             major minor subminor patch


instance Storable ConformanceVersion where
  sizeOf ~_ = 4
  alignment ~_ = 1
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ConformanceVersion where
  zero = ConformanceVersion
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkPhysicalDeviceDriverProperties"
data PhysicalDeviceDriverProperties = PhysicalDeviceDriverProperties
  { -- No documentation found for Nested "VkPhysicalDeviceDriverProperties" "driverID"
    driverID :: DriverId
  , -- No documentation found for Nested "VkPhysicalDeviceDriverProperties" "driverName"
    driverName :: ByteString
  , -- No documentation found for Nested "VkPhysicalDeviceDriverProperties" "driverInfo"
    driverInfo :: ByteString
  , -- No documentation found for Nested "VkPhysicalDeviceDriverProperties" "conformanceVersion"
    conformanceVersion :: ConformanceVersion
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDriverProperties)
#endif
deriving instance Show PhysicalDeviceDriverProperties

instance ToCStruct PhysicalDeviceDriverProperties where
  withCStruct x f = allocaBytesAligned 536 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDriverProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DriverId)) (driverID)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 20 :: Ptr (FixedArray MAX_DRIVER_NAME_SIZE CChar))) (driverName)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 276 :: Ptr (FixedArray MAX_DRIVER_INFO_SIZE CChar))) (driverInfo)
    poke ((p `plusPtr` 532 :: Ptr ConformanceVersion)) (conformanceVersion)
    f
  cStructSize = 536
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DriverId)) (zero)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 20 :: Ptr (FixedArray MAX_DRIVER_NAME_SIZE CChar))) (mempty)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 276 :: Ptr (FixedArray MAX_DRIVER_INFO_SIZE CChar))) (mempty)
    poke ((p `plusPtr` 532 :: Ptr ConformanceVersion)) (zero)
    f

instance FromCStruct PhysicalDeviceDriverProperties where
  peekCStruct p = do
    driverID <- peek @DriverId ((p `plusPtr` 16 :: Ptr DriverId))
    driverName <- packCString (lowerArrayPtr ((p `plusPtr` 20 :: Ptr (FixedArray MAX_DRIVER_NAME_SIZE CChar))))
    driverInfo <- packCString (lowerArrayPtr ((p `plusPtr` 276 :: Ptr (FixedArray MAX_DRIVER_INFO_SIZE CChar))))
    conformanceVersion <- peekCStruct @ConformanceVersion ((p `plusPtr` 532 :: Ptr ConformanceVersion))
    pure $ PhysicalDeviceDriverProperties
             driverID driverName driverInfo conformanceVersion


instance Storable PhysicalDeviceDriverProperties where
  sizeOf ~_ = 536
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDriverProperties where
  zero = PhysicalDeviceDriverProperties
           zero
           mempty
           mempty
           zero

