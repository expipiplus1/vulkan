{-# language CPP #-}
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

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Numeric (showHex)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Data.Bits (Bits)
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
import Data.Word (Word32)
import Data.Word (Word64)
import Text.Read.Lex (Lexeme(Ident))
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
-- | VkPhysicalDeviceDeviceMemoryReportFeaturesEXT - Structure describing
-- whether device memory report callback can be supported by an
-- implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceDeviceMemoryReportFeaturesEXT'
-- structure describe the following features:
--
-- = Description
--
-- -   #features-deviceMemoryReport# @deviceMemoryReport@ indicates whether
--     the implementation supports the ability to register device memory
--     report callbacks.
--
-- If the 'PhysicalDeviceDeviceMemoryReportFeaturesEXT' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with a value indicating whether the feature is supported.
-- 'PhysicalDeviceDeviceMemoryReportFeaturesEXT' /can/ also be used in the
-- @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to enable the
-- feature.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPhysicalDeviceDeviceMemoryReportFeaturesEXT-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_MEMORY_REPORT_FEATURES_EXT'
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
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


-- | VkDeviceDeviceMemoryReportCreateInfoEXT - Register device memory report
-- callbacks for a Vulkan device
--
-- = Description
--
-- The callback /may/ be called from multiple threads simultaneously.
--
-- The callback /must/ be called only once by the implementation when a
-- 'DeviceMemoryReportEventTypeEXT' event occurs.
--
-- Note
--
-- The callback could be called from a background thread other than the
-- thread calling the Vulkan commands.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDeviceDeviceMemoryReportCreateInfoEXT-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_DEVICE_MEMORY_REPORT_CREATE_INFO_EXT'
--
-- -   #VUID-VkDeviceDeviceMemoryReportCreateInfoEXT-flags-zerobitmask#
--     @flags@ /must/ be @0@
--
-- -   #VUID-VkDeviceDeviceMemoryReportCreateInfoEXT-pfnUserCallback-parameter#
--     @pfnUserCallback@ /must/ be a valid
--     'PFN_vkDeviceMemoryReportCallbackEXT' value
--
-- -   #VUID-VkDeviceDeviceMemoryReportCreateInfoEXT-pUserData-parameter#
--     @pUserData@ /must/ be a pointer value
--
-- = See Also
--
-- 'PFN_vkDeviceMemoryReportCallbackEXT', 'DeviceMemoryReportFlagsEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DeviceDeviceMemoryReportCreateInfoEXT = DeviceDeviceMemoryReportCreateInfoEXT
  { -- | @flags@ is 0 and reserved for future use.
    flags :: DeviceMemoryReportFlagsEXT
  , -- | @pfnUserCallback@ is the application callback function to call.
    pfnUserCallback :: PFN_vkDeviceMemoryReportCallbackEXT
  , -- | @pUserData@ is user data to be passed to the callback.
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


-- | VkDeviceMemoryReportCallbackDataEXT - Structure specifying parameters
-- returned to the callback
--
-- = Description
--
-- @memoryObjectId@ is used to avoid double-counting on the same memory
-- object.
--
-- If an internally-allocated device memory object or a
-- 'Vulkan.Core10.Handles.DeviceMemory' /cannot/ be exported,
-- @memoryObjectId@ /must/ be unique in the 'Vulkan.Core10.Handles.Device'.
--
-- If an internally-allocated device memory object or a
-- 'Vulkan.Core10.Handles.DeviceMemory' supports being exported,
-- @memoryObjectId@ /must/ be unique system wide.
--
-- If an internal device memory object or a
-- 'Vulkan.Core10.Handles.DeviceMemory' is backed by an imported external
-- memory object, @memoryObjectId@ /must/ be unique system wide.
--
-- Note
--
-- This structure should only be considered valid during the lifetime of
-- the triggered callback.
--
-- For 'DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATE_EXT' and
-- 'DEVICE_MEMORY_REPORT_EVENT_TYPE_IMPORT_EXT' events, @objectHandle@
-- usually will not yet exist when the application or tool receives the
-- callback. @objectHandle@ will only exist when the create or allocate
-- call that triggered the event returns, and if the allocation or import
-- ends up failing @objectHandle@ won’t ever exist.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDeviceMemoryReportCallbackDataEXT-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_MEMORY_REPORT_CALLBACK_DATA_EXT'
--
-- -   #VUID-VkDeviceMemoryReportCallbackDataEXT-pNext-pNext# @pNext@
--     /must/ be @NULL@
--
-- = See Also
--
-- 'DeviceMemoryReportEventTypeEXT', 'DeviceMemoryReportFlagsEXT',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.ObjectType.ObjectType',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DeviceMemoryReportCallbackDataEXT = DeviceMemoryReportCallbackDataEXT
  { -- | @flags@ is 0 and reserved for future use.
    flags :: DeviceMemoryReportFlagsEXT
  , -- | @type@ is a 'DeviceMemoryReportEventTypeEXT' type specifying the type of
    -- event reported in this 'DeviceMemoryReportCallbackDataEXT' structure.
    type' :: DeviceMemoryReportEventTypeEXT
  , -- | @memoryObjectId@ is the unique id for the underlying memory object as
    -- described below.
    memoryObjectId :: Word64
  , -- | @size@ is the size of the memory object in bytes. If @type@ is
    -- 'DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATE_EXT',
    -- 'DEVICE_MEMORY_REPORT_EVENT_TYPE_IMPORT_EXT' or
    -- 'DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATION_FAILED_EXT', @size@ /must/
    -- be a valid 'Vulkan.Core10.FundamentalTypes.DeviceSize' value.
    size :: DeviceSize
  , -- | @objectType@ is a 'Vulkan.Core10.Enums.ObjectType.ObjectType' value
    -- specifying the type of the object associated with this device memory
    -- report event. If @type@ is
    -- 'DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATE_EXT',
    -- 'DEVICE_MEMORY_REPORT_EVENT_TYPE_IMPORT_EXT' or
    -- 'DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATION_FAILED_EXT', @objectType@
    -- /must/ be a valid 'Vulkan.Core10.Enums.ObjectType.ObjectType' enum.
    objectType :: ObjectType
  , -- | @objectHandle@ is the object this device memory report event is
    -- attributed to. If @type@ is
    -- 'DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATE_EXT',
    -- 'DEVICE_MEMORY_REPORT_EVENT_TYPE_FREE_EXT',
    -- 'DEVICE_MEMORY_REPORT_EVENT_TYPE_IMPORT_EXT' or
    -- 'DEVICE_MEMORY_REPORT_EVENT_TYPE_UNIMPORT_EXT', @objectHandle@ /must/ be
    -- a valid Vulkan handle of the type associated with @objectType@ as
    -- defined in the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#debugging-object-types VkObjectType and Vulkan Handle Relationship>
    -- table.
    objectHandle :: Word64
  , -- | @heapIndex@ describes which memory heap this device memory allocation is
    -- made from. If @type@ is 'DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATE_EXT'
    -- or 'DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATION_FAILED_EXT', @heapIndex@
    -- /must/ correspond to one of the valid heaps from the
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceMemoryProperties'
    -- structure. Otherwise, @heapIndex@ is undefined.
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
  deriving newtype (Eq, Ord, Storable, Zero, Bits)



instance Show DeviceMemoryReportFlagsEXT where
  showsPrec p = \case
    DeviceMemoryReportFlagsEXT x -> showParen (p >= 11) (showString "DeviceMemoryReportFlagsEXT 0x" . showHex x)

instance Read DeviceMemoryReportFlagsEXT where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "DeviceMemoryReportFlagsEXT")
                       v <- step readPrec
                       pure (DeviceMemoryReportFlagsEXT v)))


-- | VkDeviceMemoryReportEventTypeEXT - Events that can occur on a device
-- memory object
--
-- = See Also
--
-- 'DeviceMemoryReportCallbackDataEXT'
newtype DeviceMemoryReportEventTypeEXT = DeviceMemoryReportEventTypeEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATE_EXT' specifies this event
-- corresponds to the allocation of an internal device memory object or a
-- 'Vulkan.Core10.Handles.DeviceMemory'.
pattern DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATE_EXT = DeviceMemoryReportEventTypeEXT 0
-- | 'DEVICE_MEMORY_REPORT_EVENT_TYPE_FREE_EXT' specifies this event
-- corresponds to the deallocation of an internally-allocated device memory
-- object or a 'Vulkan.Core10.Handles.DeviceMemory'.
pattern DEVICE_MEMORY_REPORT_EVENT_TYPE_FREE_EXT = DeviceMemoryReportEventTypeEXT 1
-- | 'DEVICE_MEMORY_REPORT_EVENT_TYPE_IMPORT_EXT' specifies this event
-- corresponds to the import of an external memory object.
pattern DEVICE_MEMORY_REPORT_EVENT_TYPE_IMPORT_EXT = DeviceMemoryReportEventTypeEXT 2
-- | 'DEVICE_MEMORY_REPORT_EVENT_TYPE_UNIMPORT_EXT' specifies this event is
-- the release of an imported external memory object.
pattern DEVICE_MEMORY_REPORT_EVENT_TYPE_UNIMPORT_EXT = DeviceMemoryReportEventTypeEXT 3
-- | 'DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATION_FAILED_EXT' specifies this
-- event corresponds to the failed allocation of an internal device memory
-- object or a 'Vulkan.Core10.Handles.DeviceMemory'.
pattern DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATION_FAILED_EXT = DeviceMemoryReportEventTypeEXT 4
{-# complete DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATE_EXT,
             DEVICE_MEMORY_REPORT_EVENT_TYPE_FREE_EXT,
             DEVICE_MEMORY_REPORT_EVENT_TYPE_IMPORT_EXT,
             DEVICE_MEMORY_REPORT_EVENT_TYPE_UNIMPORT_EXT,
             DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATION_FAILED_EXT :: DeviceMemoryReportEventTypeEXT #-}

instance Show DeviceMemoryReportEventTypeEXT where
  showsPrec p = \case
    DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATE_EXT -> showString "DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATE_EXT"
    DEVICE_MEMORY_REPORT_EVENT_TYPE_FREE_EXT -> showString "DEVICE_MEMORY_REPORT_EVENT_TYPE_FREE_EXT"
    DEVICE_MEMORY_REPORT_EVENT_TYPE_IMPORT_EXT -> showString "DEVICE_MEMORY_REPORT_EVENT_TYPE_IMPORT_EXT"
    DEVICE_MEMORY_REPORT_EVENT_TYPE_UNIMPORT_EXT -> showString "DEVICE_MEMORY_REPORT_EVENT_TYPE_UNIMPORT_EXT"
    DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATION_FAILED_EXT -> showString "DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATION_FAILED_EXT"
    DeviceMemoryReportEventTypeEXT x -> showParen (p >= 11) (showString "DeviceMemoryReportEventTypeEXT " . showsPrec 11 x)

instance Read DeviceMemoryReportEventTypeEXT where
  readPrec = parens (choose [("DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATE_EXT", pure DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATE_EXT)
                            , ("DEVICE_MEMORY_REPORT_EVENT_TYPE_FREE_EXT", pure DEVICE_MEMORY_REPORT_EVENT_TYPE_FREE_EXT)
                            , ("DEVICE_MEMORY_REPORT_EVENT_TYPE_IMPORT_EXT", pure DEVICE_MEMORY_REPORT_EVENT_TYPE_IMPORT_EXT)
                            , ("DEVICE_MEMORY_REPORT_EVENT_TYPE_UNIMPORT_EXT", pure DEVICE_MEMORY_REPORT_EVENT_TYPE_UNIMPORT_EXT)
                            , ("DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATION_FAILED_EXT", pure DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATION_FAILED_EXT)]
                     +++
                     prec 10 (do
                       expectP (Ident "DeviceMemoryReportEventTypeEXT")
                       v <- step readPrec
                       pure (DeviceMemoryReportEventTypeEXT v)))


type FN_vkDeviceMemoryReportCallbackEXT = ("pCallbackData" ::: Ptr DeviceMemoryReportCallbackDataEXT) -> ("pUserData" ::: Ptr ()) -> IO ()
-- | PFN_vkDeviceMemoryReportCallbackEXT - Application-defined device memory
-- report callback function
--
-- = Description
--
-- The callback /must/ not make calls to any Vulkan commands.
--
-- = See Also
--
-- 'DeviceDeviceMemoryReportCreateInfoEXT'
type PFN_vkDeviceMemoryReportCallbackEXT = FunPtr FN_vkDeviceMemoryReportCallbackEXT


type EXT_DEVICE_MEMORY_REPORT_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_DEVICE_MEMORY_REPORT_SPEC_VERSION"
pattern EXT_DEVICE_MEMORY_REPORT_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_DEVICE_MEMORY_REPORT_SPEC_VERSION = 1


type EXT_DEVICE_MEMORY_REPORT_EXTENSION_NAME = "VK_EXT_device_memory_report"

-- No documentation found for TopLevel "VK_EXT_DEVICE_MEMORY_REPORT_EXTENSION_NAME"
pattern EXT_DEVICE_MEMORY_REPORT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_DEVICE_MEMORY_REPORT_EXTENSION_NAME = "VK_EXT_device_memory_report"

