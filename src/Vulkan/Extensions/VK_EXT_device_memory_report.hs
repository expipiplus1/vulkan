{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_device_memory_report"
module Vulkan.Extensions.VK_EXT_device_memory_report  ( PhysicalDeviceDeviceMemoryReportFeaturesEXT(..)
                                                      , DeviceDeviceMemoryReportCreateInfoEXT(..)
                                                      , DeviceMemoryReportCallbackDataEXT(..)
                                                      , DeviceMemoryReportFlagsEXT(..)
                                                      , DeviceMemoryReportEventTypeEXT( DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATE_EXT
                                                                                      , DEVICE_MEMORY_REPORT_EVENT_TYPE_FREE_EXT
                                                                                      , DEVICE_MEMORY_REPORT_EVENT_TYPE_IMPORT_EXT
                                                                                      , DEVICE_MEMORY_REPORT_EVENT_TYPE_UNIMPORT_EXT
                                                                                      , DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATION_FAILED_EXT
                                                                                      , ..
                                                                                      )
                                                      , PFN_vkDeviceMemoryReportCallbackEXT
                                                      , FN_vkDeviceMemoryReportCallbackEXT
                                                      , EXT_DEVICE_MEMORY_REPORT_SPEC_VERSION
                                                      , pattern EXT_DEVICE_MEMORY_REPORT_SPEC_VERSION
                                                      , EXT_DEVICE_MEMORY_REPORT_EXTENSION_NAME
                                                      , pattern EXT_DEVICE_MEMORY_REPORT_EXTENSION_NAME
                                                      ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Numeric (showHex)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.ObjectType (ObjectType)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_DEVICE_MEMORY_REPORT_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_MEMORY_REPORT_CALLBACK_DATA_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_MEMORY_REPORT_FEATURES_EXT))

-- No documentation found for TopLevel "VkPhysicalDeviceDeviceMemoryReportFeaturesEXT"
data PhysicalDeviceDeviceMemoryReportFeaturesEXT = PhysicalDeviceDeviceMemoryReportFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceDeviceMemoryReportFeaturesEXT" "deviceMemoryReport"
    deviceMemoryReport :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDeviceMemoryReportFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceDeviceMemoryReportFeaturesEXT

instance ToCStruct PhysicalDeviceDeviceMemoryReportFeaturesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDeviceMemoryReportFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_MEMORY_REPORT_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (deviceMemoryReport))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_MEMORY_REPORT_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceDeviceMemoryReportFeaturesEXT where
  peekCStruct p = do
    deviceMemoryReport <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceDeviceMemoryReportFeaturesEXT
             (bool32ToBool deviceMemoryReport)


instance Storable PhysicalDeviceDeviceMemoryReportFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDeviceMemoryReportFeaturesEXT where
  zero = PhysicalDeviceDeviceMemoryReportFeaturesEXT
           zero



-- No documentation found for TopLevel "VkDeviceDeviceMemoryReportCreateInfoEXT"
data DeviceDeviceMemoryReportCreateInfoEXT = DeviceDeviceMemoryReportCreateInfoEXT
  { -- No documentation found for Nested "VkDeviceDeviceMemoryReportCreateInfoEXT" "flags"
    flags :: DeviceMemoryReportFlagsEXT
  , -- No documentation found for Nested "VkDeviceDeviceMemoryReportCreateInfoEXT" "pfnUserCallback"
    pfnUserCallback :: PFN_vkDeviceMemoryReportCallbackEXT
  , -- No documentation found for Nested "VkDeviceDeviceMemoryReportCreateInfoEXT" "pUserData"
    userData :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceDeviceMemoryReportCreateInfoEXT)
#endif
deriving instance Show DeviceDeviceMemoryReportCreateInfoEXT

instance ToCStruct DeviceDeviceMemoryReportCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceDeviceMemoryReportCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_DEVICE_MEMORY_REPORT_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceMemoryReportFlagsEXT)) (flags)
    poke ((p `plusPtr` 24 :: Ptr PFN_vkDeviceMemoryReportCallbackEXT)) (pfnUserCallback)
    poke ((p `plusPtr` 32 :: Ptr (Ptr ()))) (userData)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_DEVICE_MEMORY_REPORT_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceMemoryReportFlagsEXT)) (zero)
    poke ((p `plusPtr` 24 :: Ptr PFN_vkDeviceMemoryReportCallbackEXT)) (zero)
    poke ((p `plusPtr` 32 :: Ptr (Ptr ()))) (zero)
    f

instance FromCStruct DeviceDeviceMemoryReportCreateInfoEXT where
  peekCStruct p = do
    flags <- peek @DeviceMemoryReportFlagsEXT ((p `plusPtr` 16 :: Ptr DeviceMemoryReportFlagsEXT))
    pfnUserCallback <- peek @PFN_vkDeviceMemoryReportCallbackEXT ((p `plusPtr` 24 :: Ptr PFN_vkDeviceMemoryReportCallbackEXT))
    pUserData <- peek @(Ptr ()) ((p `plusPtr` 32 :: Ptr (Ptr ())))
    pure $ DeviceDeviceMemoryReportCreateInfoEXT
             flags pfnUserCallback pUserData


instance Storable DeviceDeviceMemoryReportCreateInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeviceDeviceMemoryReportCreateInfoEXT where
  zero = DeviceDeviceMemoryReportCreateInfoEXT
           zero
           zero
           zero



-- No documentation found for TopLevel "VkDeviceMemoryReportCallbackDataEXT"
data DeviceMemoryReportCallbackDataEXT = DeviceMemoryReportCallbackDataEXT
  { -- No documentation found for Nested "VkDeviceMemoryReportCallbackDataEXT" "flags"
    flags :: DeviceMemoryReportFlagsEXT
  , -- No documentation found for Nested "VkDeviceMemoryReportCallbackDataEXT" "type"
    type' :: DeviceMemoryReportEventTypeEXT
  , -- No documentation found for Nested "VkDeviceMemoryReportCallbackDataEXT" "memoryObjectId"
    memoryObjectId :: Word64
  , -- No documentation found for Nested "VkDeviceMemoryReportCallbackDataEXT" "size"
    size :: DeviceSize
  , -- No documentation found for Nested "VkDeviceMemoryReportCallbackDataEXT" "objectType"
    objectType :: ObjectType
  , -- No documentation found for Nested "VkDeviceMemoryReportCallbackDataEXT" "objectHandle"
    objectHandle :: Word64
  , -- No documentation found for Nested "VkDeviceMemoryReportCallbackDataEXT" "heapIndex"
    heapIndex :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceMemoryReportCallbackDataEXT)
#endif
deriving instance Show DeviceMemoryReportCallbackDataEXT

instance ToCStruct DeviceMemoryReportCallbackDataEXT where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceMemoryReportCallbackDataEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_MEMORY_REPORT_CALLBACK_DATA_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceMemoryReportFlagsEXT)) (flags)
    poke ((p `plusPtr` 20 :: Ptr DeviceMemoryReportEventTypeEXT)) (type')
    poke ((p `plusPtr` 24 :: Ptr Word64)) (memoryObjectId)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (size)
    poke ((p `plusPtr` 40 :: Ptr ObjectType)) (objectType)
    poke ((p `plusPtr` 48 :: Ptr Word64)) (objectHandle)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (heapIndex)
    f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_MEMORY_REPORT_CALLBACK_DATA_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceMemoryReportFlagsEXT)) (zero)
    poke ((p `plusPtr` 20 :: Ptr DeviceMemoryReportEventTypeEXT)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    f

instance FromCStruct DeviceMemoryReportCallbackDataEXT where
  peekCStruct p = do
    flags <- peek @DeviceMemoryReportFlagsEXT ((p `plusPtr` 16 :: Ptr DeviceMemoryReportFlagsEXT))
    type' <- peek @DeviceMemoryReportEventTypeEXT ((p `plusPtr` 20 :: Ptr DeviceMemoryReportEventTypeEXT))
    memoryObjectId <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    size <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    objectType <- peek @ObjectType ((p `plusPtr` 40 :: Ptr ObjectType))
    objectHandle <- peek @Word64 ((p `plusPtr` 48 :: Ptr Word64))
    heapIndex <- peek @Word32 ((p `plusPtr` 56 :: Ptr Word32))
    pure $ DeviceMemoryReportCallbackDataEXT
             flags type' memoryObjectId size objectType objectHandle heapIndex


instance Storable DeviceMemoryReportCallbackDataEXT where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeviceMemoryReportCallbackDataEXT where
  zero = DeviceMemoryReportCallbackDataEXT
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- No documentation found for TopLevel "VkDeviceMemoryReportFlagsEXT"
newtype DeviceMemoryReportFlagsEXT = DeviceMemoryReportFlagsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameDeviceMemoryReportFlagsEXT :: String
conNameDeviceMemoryReportFlagsEXT = "DeviceMemoryReportFlagsEXT"

enumPrefixDeviceMemoryReportFlagsEXT :: String
enumPrefixDeviceMemoryReportFlagsEXT = ""

showTableDeviceMemoryReportFlagsEXT :: [(DeviceMemoryReportFlagsEXT, String)]
showTableDeviceMemoryReportFlagsEXT = []


instance Show DeviceMemoryReportFlagsEXT where
showsPrec = enumShowsPrec enumPrefixDeviceMemoryReportFlagsEXT
                          showTableDeviceMemoryReportFlagsEXT
                          conNameDeviceMemoryReportFlagsEXT
                          (\(DeviceMemoryReportFlagsEXT x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read DeviceMemoryReportFlagsEXT where
  readPrec = enumReadPrec enumPrefixDeviceMemoryReportFlagsEXT
                          showTableDeviceMemoryReportFlagsEXT
                          conNameDeviceMemoryReportFlagsEXT
                          DeviceMemoryReportFlagsEXT


-- No documentation found for TopLevel "VkDeviceMemoryReportEventTypeEXT"
newtype DeviceMemoryReportEventTypeEXT = DeviceMemoryReportEventTypeEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkDeviceMemoryReportEventTypeEXT" "VK_DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATE_EXT"
pattern DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATE_EXT          = DeviceMemoryReportEventTypeEXT 0
-- No documentation found for Nested "VkDeviceMemoryReportEventTypeEXT" "VK_DEVICE_MEMORY_REPORT_EVENT_TYPE_FREE_EXT"
pattern DEVICE_MEMORY_REPORT_EVENT_TYPE_FREE_EXT              = DeviceMemoryReportEventTypeEXT 1
-- No documentation found for Nested "VkDeviceMemoryReportEventTypeEXT" "VK_DEVICE_MEMORY_REPORT_EVENT_TYPE_IMPORT_EXT"
pattern DEVICE_MEMORY_REPORT_EVENT_TYPE_IMPORT_EXT            = DeviceMemoryReportEventTypeEXT 2
-- No documentation found for Nested "VkDeviceMemoryReportEventTypeEXT" "VK_DEVICE_MEMORY_REPORT_EVENT_TYPE_UNIMPORT_EXT"
pattern DEVICE_MEMORY_REPORT_EVENT_TYPE_UNIMPORT_EXT          = DeviceMemoryReportEventTypeEXT 3
-- No documentation found for Nested "VkDeviceMemoryReportEventTypeEXT" "VK_DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATION_FAILED_EXT"
pattern DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATION_FAILED_EXT = DeviceMemoryReportEventTypeEXT 4
{-# complete DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATE_EXT,
             DEVICE_MEMORY_REPORT_EVENT_TYPE_FREE_EXT,
             DEVICE_MEMORY_REPORT_EVENT_TYPE_IMPORT_EXT,
             DEVICE_MEMORY_REPORT_EVENT_TYPE_UNIMPORT_EXT,
             DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATION_FAILED_EXT :: DeviceMemoryReportEventTypeEXT #-}

conNameDeviceMemoryReportEventTypeEXT :: String
conNameDeviceMemoryReportEventTypeEXT = "DeviceMemoryReportEventTypeEXT"

enumPrefixDeviceMemoryReportEventTypeEXT :: String
enumPrefixDeviceMemoryReportEventTypeEXT = "DEVICE_MEMORY_REPORT_EVENT_TYPE_"

showTableDeviceMemoryReportEventTypeEXT :: [(DeviceMemoryReportEventTypeEXT, String)]
showTableDeviceMemoryReportEventTypeEXT =
  [ (DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATE_EXT         , "ALLOCATE_EXT")
  , (DEVICE_MEMORY_REPORT_EVENT_TYPE_FREE_EXT             , "FREE_EXT")
  , (DEVICE_MEMORY_REPORT_EVENT_TYPE_IMPORT_EXT           , "IMPORT_EXT")
  , (DEVICE_MEMORY_REPORT_EVENT_TYPE_UNIMPORT_EXT         , "UNIMPORT_EXT")
  , (DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATION_FAILED_EXT, "ALLOCATION_FAILED_EXT")
  ]


instance Show DeviceMemoryReportEventTypeEXT where
showsPrec = enumShowsPrec enumPrefixDeviceMemoryReportEventTypeEXT
                          showTableDeviceMemoryReportEventTypeEXT
                          conNameDeviceMemoryReportEventTypeEXT
                          (\(DeviceMemoryReportEventTypeEXT x) -> x)
                          (showsPrec 11)


instance Read DeviceMemoryReportEventTypeEXT where
  readPrec = enumReadPrec enumPrefixDeviceMemoryReportEventTypeEXT
                          showTableDeviceMemoryReportEventTypeEXT
                          conNameDeviceMemoryReportEventTypeEXT
                          DeviceMemoryReportEventTypeEXT


type FN_vkDeviceMemoryReportCallbackEXT = ("pCallbackData" ::: Ptr DeviceMemoryReportCallbackDataEXT) -> ("pUserData" ::: Ptr ()) -> IO ()
-- No documentation found for TopLevel "PFN_vkDeviceMemoryReportCallbackEXT"
type PFN_vkDeviceMemoryReportCallbackEXT = FunPtr FN_vkDeviceMemoryReportCallbackEXT


type EXT_DEVICE_MEMORY_REPORT_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_DEVICE_MEMORY_REPORT_SPEC_VERSION"
pattern EXT_DEVICE_MEMORY_REPORT_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_DEVICE_MEMORY_REPORT_SPEC_VERSION = 1


type EXT_DEVICE_MEMORY_REPORT_EXTENSION_NAME = "VK_EXT_device_memory_report"

-- No documentation found for TopLevel "VK_EXT_DEVICE_MEMORY_REPORT_EXTENSION_NAME"
pattern EXT_DEVICE_MEMORY_REPORT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_DEVICE_MEMORY_REPORT_EXTENSION_NAME = "VK_EXT_device_memory_report"

