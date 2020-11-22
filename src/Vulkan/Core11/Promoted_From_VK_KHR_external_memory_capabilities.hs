{-# language CPP #-}
module Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities  ( getPhysicalDeviceExternalBufferProperties
                                                                        , ExternalMemoryProperties(..)
                                                                        , PhysicalDeviceExternalImageFormatInfo(..)
                                                                        , ExternalImageFormatProperties(..)
                                                                        , PhysicalDeviceExternalBufferInfo(..)
                                                                        , ExternalBufferProperties(..)
                                                                        , PhysicalDeviceIDProperties(..)
                                                                        , StructureType(..)
                                                                        , ExternalMemoryHandleTypeFlagBits(..)
                                                                        , ExternalMemoryHandleTypeFlags
                                                                        , ExternalMemoryFeatureFlagBits(..)
                                                                        , ExternalMemoryFeatureFlags
                                                                        , LUID_SIZE
                                                                        , pattern LUID_SIZE
                                                                        ) where

import Vulkan.CStruct.Utils (FixedArray)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.IO.Class (MonadIO)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Word (Word8)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Utils (peekByteStringFromSizedVectorPtr)
import Vulkan.CStruct.Utils (pokeFixedLengthByteString)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Enums.BufferCreateFlagBits (BufferCreateFlags)
import Vulkan.Core10.Enums.BufferUsageFlagBits (BufferUsageFlags)
import Vulkan.Core11.Enums.ExternalMemoryFeatureFlagBits (ExternalMemoryFeatureFlags)
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlagBits)
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceExternalBufferProperties))
import Vulkan.Core10.APIConstants (LUID_SIZE)
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Core10.APIConstants (UUID_SIZE)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES))
import Vulkan.Core11.Enums.ExternalMemoryFeatureFlagBits (ExternalMemoryFeatureFlagBits(..))
import Vulkan.Core11.Enums.ExternalMemoryFeatureFlagBits (ExternalMemoryFeatureFlags)
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlagBits(..))
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlags)
import Vulkan.Core10.APIConstants (LUID_SIZE)
import Vulkan.Core10.Enums.StructureType (StructureType(..))
import Vulkan.Core10.APIConstants (pattern LUID_SIZE)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceExternalBufferProperties
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr PhysicalDeviceExternalBufferInfo -> Ptr ExternalBufferProperties -> IO ()) -> Ptr PhysicalDevice_T -> Ptr PhysicalDeviceExternalBufferInfo -> Ptr ExternalBufferProperties -> IO ()

-- | vkGetPhysicalDeviceExternalBufferProperties - Query external handle
-- types supported by buffers
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'ExternalBufferProperties', 'Vulkan.Core10.Handles.PhysicalDevice',
-- 'PhysicalDeviceExternalBufferInfo'
getPhysicalDeviceExternalBufferProperties :: forall io
                                           . (MonadIO io)
                                          => -- | @physicalDevice@ is the physical device from which to query the buffer
                                             -- capabilities.
                                             --
                                             -- #VUID-vkGetPhysicalDeviceExternalBufferProperties-physicalDevice-parameter#
                                             -- @physicalDevice@ /must/ be a valid
                                             -- 'Vulkan.Core10.Handles.PhysicalDevice' handle
                                             PhysicalDevice
                                          -> -- | @pExternalBufferInfo@ is a pointer to a
                                             -- 'PhysicalDeviceExternalBufferInfo' structure describing the parameters
                                             -- that would be consumed by 'Vulkan.Core10.Buffer.createBuffer'.
                                             --
                                             -- #VUID-vkGetPhysicalDeviceExternalBufferProperties-pExternalBufferInfo-parameter#
                                             -- @pExternalBufferInfo@ /must/ be a valid pointer to a valid
                                             -- 'PhysicalDeviceExternalBufferInfo' structure
                                             PhysicalDeviceExternalBufferInfo
                                          -> io (ExternalBufferProperties)
getPhysicalDeviceExternalBufferProperties physicalDevice externalBufferInfo = liftIO . evalContT $ do
  let vkGetPhysicalDeviceExternalBufferPropertiesPtr = pVkGetPhysicalDeviceExternalBufferProperties (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceExternalBufferPropertiesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceExternalBufferProperties is null" Nothing Nothing
  let vkGetPhysicalDeviceExternalBufferProperties' = mkVkGetPhysicalDeviceExternalBufferProperties vkGetPhysicalDeviceExternalBufferPropertiesPtr
  pExternalBufferInfo <- ContT $ withCStruct (externalBufferInfo)
  pPExternalBufferProperties <- ContT (withZeroCStruct @ExternalBufferProperties)
  lift $ vkGetPhysicalDeviceExternalBufferProperties' (physicalDeviceHandle (physicalDevice)) pExternalBufferInfo (pPExternalBufferProperties)
  pExternalBufferProperties <- lift $ peekCStruct @ExternalBufferProperties pPExternalBufferProperties
  pure $ (pExternalBufferProperties)


-- | VkExternalMemoryProperties - Structure specifying external memory handle
-- type capabilities
--
-- = Description
--
-- @compatibleHandleTypes@ /must/ include at least @handleType@. Inclusion
-- of a handle type in @compatibleHandleTypes@ does not imply the values
-- returned in
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.ImageFormatProperties2'
-- will be the same when
-- 'PhysicalDeviceExternalImageFormatInfo'::@handleType@ is set to that
-- type. The application is responsible for querying the capabilities of
-- all handle types intended for concurrent use in a single image and
-- intersecting them to obtain the compatible set of capabilities.
--
-- = See Also
--
-- 'ExternalBufferProperties', 'ExternalImageFormatProperties',
-- 'Vulkan.Core11.Enums.ExternalMemoryFeatureFlagBits.ExternalMemoryFeatureFlags',
-- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlags'
data ExternalMemoryProperties = ExternalMemoryProperties
  { -- | @externalMemoryFeatures@ is a bitmask of
    -- 'Vulkan.Core11.Enums.ExternalMemoryFeatureFlagBits.ExternalMemoryFeatureFlagBits'
    -- specifying the features of @handleType@.
    externalMemoryFeatures :: ExternalMemoryFeatureFlags
  , -- | @exportFromImportedHandleTypes@ is a bitmask of
    -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
    -- specifying which types of imported handle @handleType@ /can/ be exported
    -- from.
    exportFromImportedHandleTypes :: ExternalMemoryHandleTypeFlags
  , -- | @compatibleHandleTypes@ is a bitmask of
    -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
    -- specifying handle types which /can/ be specified at the same time as
    -- @handleType@ when creating an image compatible with external memory.
    compatibleHandleTypes :: ExternalMemoryHandleTypeFlags
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExternalMemoryProperties)
#endif
deriving instance Show ExternalMemoryProperties

instance ToCStruct ExternalMemoryProperties where
  withCStruct x f = allocaBytesAligned 12 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExternalMemoryProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr ExternalMemoryFeatureFlags)) (externalMemoryFeatures)
    poke ((p `plusPtr` 4 :: Ptr ExternalMemoryHandleTypeFlags)) (exportFromImportedHandleTypes)
    poke ((p `plusPtr` 8 :: Ptr ExternalMemoryHandleTypeFlags)) (compatibleHandleTypes)
    f
  cStructSize = 12
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr ExternalMemoryFeatureFlags)) (zero)
    poke ((p `plusPtr` 8 :: Ptr ExternalMemoryHandleTypeFlags)) (zero)
    f

instance FromCStruct ExternalMemoryProperties where
  peekCStruct p = do
    externalMemoryFeatures <- peek @ExternalMemoryFeatureFlags ((p `plusPtr` 0 :: Ptr ExternalMemoryFeatureFlags))
    exportFromImportedHandleTypes <- peek @ExternalMemoryHandleTypeFlags ((p `plusPtr` 4 :: Ptr ExternalMemoryHandleTypeFlags))
    compatibleHandleTypes <- peek @ExternalMemoryHandleTypeFlags ((p `plusPtr` 8 :: Ptr ExternalMemoryHandleTypeFlags))
    pure $ ExternalMemoryProperties
             externalMemoryFeatures exportFromImportedHandleTypes compatibleHandleTypes

instance Storable ExternalMemoryProperties where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExternalMemoryProperties where
  zero = ExternalMemoryProperties
           zero
           zero
           zero


-- | VkPhysicalDeviceExternalImageFormatInfo - Structure specifying external
-- image creation parameters
--
-- = Description
--
-- If @handleType@ is 0,
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
-- will behave as if 'PhysicalDeviceExternalImageFormatInfo' was not
-- present, and 'ExternalImageFormatProperties' will be ignored.
--
-- If @handleType@ is not compatible with the @format@, @type@, @tiling@,
-- @usage@, and @flags@ specified in
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2',
-- then
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
-- returns 'Vulkan.Core10.Enums.Result.ERROR_FORMAT_NOT_SUPPORTED'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPhysicalDeviceExternalImageFormatInfo-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO'
--
-- -   #VUID-VkPhysicalDeviceExternalImageFormatInfo-handleType-parameter#
--     If @handleType@ is not @0@, @handleType@ /must/ be a valid
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
--     value
--
-- = See Also
--
-- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceExternalImageFormatInfo = PhysicalDeviceExternalImageFormatInfo
  { -- | @handleType@ is a
    -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
    -- value specifying the memory handle type that will be used with the
    -- memory associated with the image.
    handleType :: ExternalMemoryHandleTypeFlagBits }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceExternalImageFormatInfo)
#endif
deriving instance Show PhysicalDeviceExternalImageFormatInfo

instance ToCStruct PhysicalDeviceExternalImageFormatInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceExternalImageFormatInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlagBits)) (handleType)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct PhysicalDeviceExternalImageFormatInfo where
  peekCStruct p = do
    handleType <- peek @ExternalMemoryHandleTypeFlagBits ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlagBits))
    pure $ PhysicalDeviceExternalImageFormatInfo
             handleType

instance Storable PhysicalDeviceExternalImageFormatInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceExternalImageFormatInfo where
  zero = PhysicalDeviceExternalImageFormatInfo
           zero


-- | VkExternalImageFormatProperties - Structure specifying supported
-- external handle properties
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'ExternalMemoryProperties',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ExternalImageFormatProperties = ExternalImageFormatProperties
  { -- | @externalMemoryProperties@ is a 'ExternalMemoryProperties' structure
    -- specifying various capabilities of the external handle type when used
    -- with the specified image creation parameters.
    externalMemoryProperties :: ExternalMemoryProperties }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExternalImageFormatProperties)
#endif
deriving instance Show ExternalImageFormatProperties

instance ToCStruct ExternalImageFormatProperties where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExternalImageFormatProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalMemoryProperties)) (externalMemoryProperties)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalMemoryProperties)) (zero)
    f

instance FromCStruct ExternalImageFormatProperties where
  peekCStruct p = do
    externalMemoryProperties <- peekCStruct @ExternalMemoryProperties ((p `plusPtr` 16 :: Ptr ExternalMemoryProperties))
    pure $ ExternalImageFormatProperties
             externalMemoryProperties

instance Storable ExternalImageFormatProperties where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExternalImageFormatProperties where
  zero = ExternalImageFormatProperties
           zero


-- | VkPhysicalDeviceExternalBufferInfo - Structure specifying buffer
-- creation parameters
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.BufferCreateFlagBits.BufferCreateFlags',
-- 'Vulkan.Core10.Enums.BufferUsageFlagBits.BufferUsageFlags',
-- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPhysicalDeviceExternalBufferProperties',
-- 'Vulkan.Extensions.VK_KHR_external_memory_capabilities.getPhysicalDeviceExternalBufferPropertiesKHR'
data PhysicalDeviceExternalBufferInfo = PhysicalDeviceExternalBufferInfo
  { -- | @flags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.BufferCreateFlagBits.BufferCreateFlagBits'
    -- describing additional parameters of the buffer, corresponding to
    -- 'Vulkan.Core10.Buffer.BufferCreateInfo'::@flags@.
    --
    -- #VUID-VkPhysicalDeviceExternalBufferInfo-flags-parameter# @flags@ /must/
    -- be a valid combination of
    -- 'Vulkan.Core10.Enums.BufferCreateFlagBits.BufferCreateFlagBits' values
    flags :: BufferCreateFlags
  , -- | @usage@ is a bitmask of
    -- 'Vulkan.Core10.Enums.BufferUsageFlagBits.BufferUsageFlagBits' describing
    -- the intended usage of the buffer, corresponding to
    -- 'Vulkan.Core10.Buffer.BufferCreateInfo'::@usage@.
    --
    -- #VUID-VkPhysicalDeviceExternalBufferInfo-usage-parameter# @usage@ /must/
    -- be a valid combination of
    -- 'Vulkan.Core10.Enums.BufferUsageFlagBits.BufferUsageFlagBits' values
    --
    -- #VUID-VkPhysicalDeviceExternalBufferInfo-usage-requiredbitmask# @usage@
    -- /must/ not be @0@
    usage :: BufferUsageFlags
  , -- | @handleType@ is a
    -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
    -- value specifying the memory handle type that will be used with the
    -- memory associated with the buffer.
    --
    -- #VUID-VkPhysicalDeviceExternalBufferInfo-handleType-parameter#
    -- @handleType@ /must/ be a valid
    -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
    -- value
    handleType :: ExternalMemoryHandleTypeFlagBits
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceExternalBufferInfo)
#endif
deriving instance Show PhysicalDeviceExternalBufferInfo

instance ToCStruct PhysicalDeviceExternalBufferInfo where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceExternalBufferInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr BufferCreateFlags)) (flags)
    poke ((p `plusPtr` 20 :: Ptr BufferUsageFlags)) (usage)
    poke ((p `plusPtr` 24 :: Ptr ExternalMemoryHandleTypeFlagBits)) (handleType)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 20 :: Ptr BufferUsageFlags)) (zero)
    poke ((p `plusPtr` 24 :: Ptr ExternalMemoryHandleTypeFlagBits)) (zero)
    f

instance FromCStruct PhysicalDeviceExternalBufferInfo where
  peekCStruct p = do
    flags <- peek @BufferCreateFlags ((p `plusPtr` 16 :: Ptr BufferCreateFlags))
    usage <- peek @BufferUsageFlags ((p `plusPtr` 20 :: Ptr BufferUsageFlags))
    handleType <- peek @ExternalMemoryHandleTypeFlagBits ((p `plusPtr` 24 :: Ptr ExternalMemoryHandleTypeFlagBits))
    pure $ PhysicalDeviceExternalBufferInfo
             flags usage handleType

instance Storable PhysicalDeviceExternalBufferInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceExternalBufferInfo where
  zero = PhysicalDeviceExternalBufferInfo
           zero
           zero
           zero


-- | VkExternalBufferProperties - Structure specifying supported external
-- handle capabilities
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'ExternalMemoryProperties',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPhysicalDeviceExternalBufferProperties',
-- 'Vulkan.Extensions.VK_KHR_external_memory_capabilities.getPhysicalDeviceExternalBufferPropertiesKHR'
data ExternalBufferProperties = ExternalBufferProperties
  { -- | @externalMemoryProperties@ is a 'ExternalMemoryProperties' structure
    -- specifying various capabilities of the external handle type when used
    -- with the specified buffer creation parameters.
    externalMemoryProperties :: ExternalMemoryProperties }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExternalBufferProperties)
#endif
deriving instance Show ExternalBufferProperties

instance ToCStruct ExternalBufferProperties where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExternalBufferProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalMemoryProperties)) (externalMemoryProperties)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalMemoryProperties)) (zero)
    f

instance FromCStruct ExternalBufferProperties where
  peekCStruct p = do
    externalMemoryProperties <- peekCStruct @ExternalMemoryProperties ((p `plusPtr` 16 :: Ptr ExternalMemoryProperties))
    pure $ ExternalBufferProperties
             externalMemoryProperties

instance Storable ExternalBufferProperties where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExternalBufferProperties where
  zero = ExternalBufferProperties
           zero


-- | VkPhysicalDeviceIDProperties - Structure specifying IDs related to the
-- physical device
--
-- = Description
--
-- -   @deviceUUID@ is an array of 'Vulkan.Core10.APIConstants.UUID_SIZE'
--     @uint8_t@ values representing a universally unique identifier for
--     the device.
--
-- -   @driverUUID@ is an array of 'Vulkan.Core10.APIConstants.UUID_SIZE'
--     @uint8_t@ values representing a universally unique identifier for
--     the driver build in use by the device.
--
-- -   @deviceLUID@ is an array of 'Vulkan.Core10.APIConstants.LUID_SIZE'
--     @uint8_t@ values representing a locally unique identifier for the
--     device.
--
-- -   @deviceNodeMask@ is a @uint32_t@ bitfield identifying the node
--     within a linked device adapter corresponding to the device.
--
-- -   @deviceLUIDValid@ is a boolean value that will be
--     'Vulkan.Core10.FundamentalTypes.TRUE' if @deviceLUID@ contains a
--     valid LUID and @deviceNodeMask@ contains a valid node mask, and
--     'Vulkan.Core10.FundamentalTypes.FALSE' if they do not.
--
-- @deviceUUID@ /must/ be immutable for a given device across instances,
-- processes, driver APIs, driver versions, and system reboots.
--
-- Applications /can/ compare the @driverUUID@ value across instance and
-- process boundaries, and /can/ make similar queries in external APIs to
-- determine whether they are capable of sharing memory objects and
-- resources using them with the device.
--
-- @deviceUUID@ and\/or @driverUUID@ /must/ be used to determine whether a
-- particular external object can be shared between driver components,
-- where such a restriction exists as defined in the compatibility table
-- for the particular object type:
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#external-memory-handle-types-compatibility External memory handle types compatibility>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#external-semaphore-handle-types-compatibility External semaphore handle types compatibility>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#external-fence-handle-types-compatibility External fence handle types compatibility>
--
-- If @deviceLUIDValid@ is 'Vulkan.Core10.FundamentalTypes.FALSE', the
-- values of @deviceLUID@ and @deviceNodeMask@ are undefined. If
-- @deviceLUIDValid@ is 'Vulkan.Core10.FundamentalTypes.TRUE' and Vulkan is
-- running on the Windows operating system, the contents of @deviceLUID@
-- /can/ be cast to an @LUID@ object and /must/ be equal to the locally
-- unique identifier of a @IDXGIAdapter1@ object that corresponds to
-- @physicalDevice@. If @deviceLUIDValid@ is
-- 'Vulkan.Core10.FundamentalTypes.TRUE', @deviceNodeMask@ /must/ contain
-- exactly one bit. If Vulkan is running on an operating system that
-- supports the Direct3D 12 API and @physicalDevice@ corresponds to an
-- individual device in a linked device adapter, @deviceNodeMask@
-- identifies the Direct3D 12 node corresponding to @physicalDevice@.
-- Otherwise, @deviceNodeMask@ /must/ be @1@.
--
-- Note
--
-- Although they have identical descriptions,
-- 'PhysicalDeviceIDProperties'::@deviceUUID@ may differ from
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'::@pipelineCacheUUID@.
-- The former is intended to identify and correlate devices across API and
-- driver boundaries, while the latter is used to identify a compatible
-- device and driver combination to use when serializing and de-serializing
-- pipeline state.
--
-- Implementations /should/ return @deviceUUID@ values which are likely to
-- be unique even in the presence of multiple Vulkan implementations (such
-- as a GPU driver and a software renderer; two drivers for different GPUs;
-- or the same Vulkan driver running on two logically different devices).
--
-- Khronos\' conformance testing can not guarantee that @deviceUUID@ values
-- are actually unique, so implementors should make their own best efforts
-- to ensure this. In particular, hard-coded @deviceUUID@ values,
-- especially all-@0@ bits, /should/ never be used.
--
-- A combination of values unique to the vendor, the driver, and the
-- hardware environment can be used to provide a @deviceUUID@ which is
-- unique to a high degree of certainty. Some possible inputs to such a
-- computation are:
--
-- -   Information reported by
--     'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceProperties'
--
-- -   PCI device ID (if defined)
--
-- -   PCI bus ID, or similar system configuration information.
--
-- -   Driver binary checksums.
--
-- Note
--
-- While 'PhysicalDeviceIDProperties'::@deviceUUID@ is specified to remain
-- consistent across driver versions and system reboots, it is not intended
-- to be usable as a serializable persistent identifier for a device. It
-- may change when a device is physically added to, removed from, or moved
-- to a different connector in a system while that system is powered down.
-- Further, there is no reasonable way to verify with conformance testing
-- that a given device retains the same UUID in a given system across all
-- driver versions supported in that system. While implementations should
-- make every effort to report consistent device UUIDs across driver
-- versions, applications should avoid relying on the persistence of this
-- value for uses other than identifying compatible devices for external
-- object sharing purposes.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPhysicalDeviceIDProperties-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES'
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceIDProperties = PhysicalDeviceIDProperties
  { -- No documentation found for Nested "VkPhysicalDeviceIDProperties" "deviceUUID"
    deviceUUID :: ByteString
  , -- No documentation found for Nested "VkPhysicalDeviceIDProperties" "driverUUID"
    driverUUID :: ByteString
  , -- No documentation found for Nested "VkPhysicalDeviceIDProperties" "deviceLUID"
    deviceLUID :: ByteString
  , -- No documentation found for Nested "VkPhysicalDeviceIDProperties" "deviceNodeMask"
    deviceNodeMask :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceIDProperties" "deviceLUIDValid"
    deviceLUIDValid :: Bool
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceIDProperties)
#endif
deriving instance Show PhysicalDeviceIDProperties

instance ToCStruct PhysicalDeviceIDProperties where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceIDProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthByteString ((p `plusPtr` 16 :: Ptr (FixedArray UUID_SIZE Word8))) (deviceUUID)
    pokeFixedLengthByteString ((p `plusPtr` 32 :: Ptr (FixedArray UUID_SIZE Word8))) (driverUUID)
    pokeFixedLengthByteString ((p `plusPtr` 48 :: Ptr (FixedArray LUID_SIZE Word8))) (deviceLUID)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (deviceNodeMask)
    poke ((p `plusPtr` 60 :: Ptr Bool32)) (boolToBool32 (deviceLUIDValid))
    f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthByteString ((p `plusPtr` 16 :: Ptr (FixedArray UUID_SIZE Word8))) (mempty)
    pokeFixedLengthByteString ((p `plusPtr` 32 :: Ptr (FixedArray UUID_SIZE Word8))) (mempty)
    pokeFixedLengthByteString ((p `plusPtr` 48 :: Ptr (FixedArray LUID_SIZE Word8))) (mempty)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 60 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceIDProperties where
  peekCStruct p = do
    deviceUUID <- peekByteStringFromSizedVectorPtr ((p `plusPtr` 16 :: Ptr (FixedArray UUID_SIZE Word8)))
    driverUUID <- peekByteStringFromSizedVectorPtr ((p `plusPtr` 32 :: Ptr (FixedArray UUID_SIZE Word8)))
    deviceLUID <- peekByteStringFromSizedVectorPtr ((p `plusPtr` 48 :: Ptr (FixedArray LUID_SIZE Word8)))
    deviceNodeMask <- peek @Word32 ((p `plusPtr` 56 :: Ptr Word32))
    deviceLUIDValid <- peek @Bool32 ((p `plusPtr` 60 :: Ptr Bool32))
    pure $ PhysicalDeviceIDProperties
             deviceUUID driverUUID deviceLUID deviceNodeMask (bool32ToBool deviceLUIDValid)

instance Storable PhysicalDeviceIDProperties where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceIDProperties where
  zero = PhysicalDeviceIDProperties
           mempty
           mempty
           mempty
           zero
           zero

