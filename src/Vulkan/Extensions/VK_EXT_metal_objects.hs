{-# language CPP #-}
-- | = Name
--
-- VK_EXT_metal_objects - device extension
--
-- == VK_EXT_metal_objects
--
-- [__Name String__]
--     @VK_EXT_metal_objects@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     312
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__; __Contact__]
--
--     -   Bill Hollings
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_metal_objects] @billhollings%0A*Here describe the issue or question you have about the VK_EXT_metal_objects extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_metal_objects.adoc VK_EXT_metal_objects>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-05-28
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Bill Hollings, The Brenwill Workshop Ltd.
--
--     -   Dzmitry Malyshau, Mozilla Corp.
--
-- == Description
--
-- In a Vulkan implementation that is layered on top of Metal on Apple
-- device platforms, this extension provides the ability to import and
-- export the underlying Metal objects associated with specific Vulkan
-- objects.
--
-- As detailed in the
-- <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_metal_objects.adoc extension proposal document>,
-- this extension adds one new Vulkan command, 'exportMetalObjectsEXT', to
-- export underlying Metal objects from Vulkan objects, and supports
-- importing the appropriate existing Metal objects when creating Vulkan
-- objects of types 'Vulkan.Core10.Handles.DeviceMemory',
-- 'Vulkan.Core10.Handles.Image', 'Vulkan.Core10.Handles.Semaphore', and
-- 'Vulkan.Core10.Handles.Event',
--
-- The intent is that this extension will be advertised and supported only
-- on implementations that are layered on top of Metal on Apple device
-- platforms.
--
-- == New Base Types
--
-- -   'IOSurfaceRef'
--
-- -   'MTLBuffer_id'
--
-- -   'MTLCommandQueue_id'
--
-- -   'MTLDevice_id'
--
-- -   'MTLSharedEvent_id'
--
-- -   'MTLTexture_id'
--
-- == New Commands
--
-- -   'exportMetalObjectsEXT'
--
-- == New Structures
--
-- -   'ExportMetalObjectsInfoEXT'
--
-- -   Extending 'ExportMetalObjectsInfoEXT':
--
--     -   'ExportMetalBufferInfoEXT'
--
--     -   'ExportMetalCommandQueueInfoEXT'
--
--     -   'ExportMetalDeviceInfoEXT'
--
--     -   'ExportMetalIOSurfaceInfoEXT'
--
--     -   'ExportMetalSharedEventInfoEXT'
--
--     -   'ExportMetalTextureInfoEXT'
--
-- -   Extending 'Vulkan.Core10.Image.ImageCreateInfo':
--
--     -   'ImportMetalIOSurfaceInfoEXT'
--
--     -   'ImportMetalTextureInfoEXT'
--
-- -   Extending 'Vulkan.Core10.DeviceInitialization.InstanceCreateInfo',
--     'Vulkan.Core10.Memory.MemoryAllocateInfo',
--     'Vulkan.Core10.Image.ImageCreateInfo',
--     'Vulkan.Core10.ImageView.ImageViewCreateInfo',
--     'Vulkan.Core10.BufferView.BufferViewCreateInfo',
--     'Vulkan.Core10.QueueSemaphore.SemaphoreCreateInfo',
--     'Vulkan.Core10.Event.EventCreateInfo':
--
--     -   'ExportMetalObjectCreateInfoEXT'
--
-- -   Extending 'Vulkan.Core10.Memory.MemoryAllocateInfo':
--
--     -   'ImportMetalBufferInfoEXT'
--
-- -   Extending 'Vulkan.Core10.QueueSemaphore.SemaphoreCreateInfo',
--     'Vulkan.Core10.Event.EventCreateInfo':
--
--     -   'ImportMetalSharedEventInfoEXT'
--
-- == New Enums
--
-- -   'ExportMetalObjectTypeFlagBitsEXT'
--
-- == New Bitmasks
--
-- -   'ExportMetalObjectTypeFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_METAL_OBJECTS_EXTENSION_NAME'
--
-- -   'EXT_METAL_OBJECTS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXPORT_METAL_BUFFER_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXPORT_METAL_COMMAND_QUEUE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXPORT_METAL_DEVICE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXPORT_METAL_IO_SURFACE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXPORT_METAL_OBJECTS_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXPORT_METAL_OBJECT_CREATE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXPORT_METAL_SHARED_EVENT_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXPORT_METAL_TEXTURE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMPORT_METAL_BUFFER_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMPORT_METAL_IO_SURFACE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMPORT_METAL_SHARED_EVENT_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMPORT_METAL_TEXTURE_INFO_EXT'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2022-05-28 (Bill Hollings)
--
--     -   Initial draft.
--
--     -   Incorporated feedback from review by the Vulkan Working Group.
--         Renamed many structures, moved import\/export of MTLBuffer to
--         VkDeviceMemory, added export of MTLSharedEvent, added import of
--         MTLSharedEvent for VkSemaphore and VkEvent, and changed used bit
--         mask fields to individual bit fields to simplify Valid Usage
--         rules.
--
-- == See Also
--
-- 'IOSurfaceRef', 'MTLBuffer_id', 'MTLCommandQueue_id', 'MTLDevice_id',
-- 'MTLSharedEvent_id', 'MTLTexture_id', 'ExportMetalBufferInfoEXT',
-- 'ExportMetalCommandQueueInfoEXT', 'ExportMetalDeviceInfoEXT',
-- 'ExportMetalIOSurfaceInfoEXT', 'ExportMetalObjectCreateInfoEXT',
-- 'ExportMetalObjectTypeFlagBitsEXT', 'ExportMetalObjectTypeFlagsEXT',
-- 'ExportMetalObjectsInfoEXT', 'ExportMetalSharedEventInfoEXT',
-- 'ExportMetalTextureInfoEXT', 'ImportMetalBufferInfoEXT',
-- 'ImportMetalIOSurfaceInfoEXT', 'ImportMetalSharedEventInfoEXT',
-- 'ImportMetalTextureInfoEXT', 'exportMetalObjectsEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_metal_objects Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_metal_objects  ( exportMetalObjectsEXT
                                               , ExportMetalObjectCreateInfoEXT(..)
                                               , ExportMetalObjectsInfoEXT(..)
                                               , ExportMetalDeviceInfoEXT(..)
                                               , ExportMetalCommandQueueInfoEXT(..)
                                               , ExportMetalBufferInfoEXT(..)
                                               , ImportMetalBufferInfoEXT(..)
                                               , ExportMetalTextureInfoEXT(..)
                                               , ImportMetalTextureInfoEXT(..)
                                               , ExportMetalIOSurfaceInfoEXT(..)
                                               , ImportMetalIOSurfaceInfoEXT(..)
                                               , ExportMetalSharedEventInfoEXT(..)
                                               , ImportMetalSharedEventInfoEXT(..)
                                               , ExportMetalObjectTypeFlagsEXT
                                               , ExportMetalObjectTypeFlagBitsEXT( EXPORT_METAL_OBJECT_TYPE_METAL_DEVICE_BIT_EXT
                                                                                 , EXPORT_METAL_OBJECT_TYPE_METAL_COMMAND_QUEUE_BIT_EXT
                                                                                 , EXPORT_METAL_OBJECT_TYPE_METAL_BUFFER_BIT_EXT
                                                                                 , EXPORT_METAL_OBJECT_TYPE_METAL_TEXTURE_BIT_EXT
                                                                                 , EXPORT_METAL_OBJECT_TYPE_METAL_IOSURFACE_BIT_EXT
                                                                                 , EXPORT_METAL_OBJECT_TYPE_METAL_SHARED_EVENT_BIT_EXT
                                                                                 , ..
                                                                                 )
                                               , EXT_METAL_OBJECTS_SPEC_VERSION
                                               , pattern EXT_METAL_OBJECTS_SPEC_VERSION
                                               , EXT_METAL_OBJECTS_EXTENSION_NAME
                                               , pattern EXT_METAL_OBJECTS_EXTENSION_NAME
                                               , MTLDevice_id
                                               , MTLCommandQueue_id
                                               , MTLBuffer_id
                                               , MTLTexture_id
                                               , IOSurfaceRef
                                               , MTLSharedEvent_id
                                               ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import Numeric (showHex)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Type.Equality ((:~:)(Refl))
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
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.Core10.Handles (BufferView)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkExportMetalObjectsEXT))
import Vulkan.Core10.Handles (DeviceMemory)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.Handles (Event)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Handles (Image)
import Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlagBits)
import Vulkan.Core10.Handles (ImageView)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Handles (Queue_T)
import Vulkan.Core10.Handles (Semaphore)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXPORT_METAL_BUFFER_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXPORT_METAL_COMMAND_QUEUE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXPORT_METAL_DEVICE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXPORT_METAL_IO_SURFACE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXPORT_METAL_OBJECTS_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXPORT_METAL_OBJECT_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXPORT_METAL_SHARED_EVENT_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXPORT_METAL_TEXTURE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMPORT_METAL_BUFFER_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMPORT_METAL_IO_SURFACE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMPORT_METAL_SHARED_EVENT_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMPORT_METAL_TEXTURE_INFO_EXT))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkExportMetalObjectsEXT
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct ExportMetalObjectsInfoEXT) -> IO ()) -> Ptr Device_T -> Ptr (SomeStruct ExportMetalObjectsInfoEXT) -> IO ()

-- | vkExportMetalObjectsEXT - Export Metal objects from the corresponding
-- Vulkan objects
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_metal_objects VK_EXT_metal_objects>,
-- 'Vulkan.Core10.Handles.Device', 'ExportMetalObjectsInfoEXT'
exportMetalObjectsEXT :: forall a io
                       . ( Extendss ExportMetalObjectsInfoEXT a
                         , PokeChain a
                         , PeekChain a
                         , MonadIO io )
                      => -- | @device@ is the device that created the Vulkan objects.
                         --
                         -- #VUID-vkExportMetalObjectsEXT-device-parameter# @device@ /must/ be a
                         -- valid 'Vulkan.Core10.Handles.Device' handle
                         Device
                      -> io (ExportMetalObjectsInfoEXT a)
exportMetalObjectsEXT device = liftIO . evalContT $ do
  let vkExportMetalObjectsEXTPtr = pVkExportMetalObjectsEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkExportMetalObjectsEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkExportMetalObjectsEXT is null" Nothing Nothing
  let vkExportMetalObjectsEXT' = mkVkExportMetalObjectsEXT vkExportMetalObjectsEXTPtr
  pPMetalObjectsInfo <- ContT (withZeroCStruct @(ExportMetalObjectsInfoEXT _))
  lift $ traceAroundEvent "vkExportMetalObjectsEXT" (vkExportMetalObjectsEXT'
                                                       (deviceHandle (device))
                                                       (forgetExtensions (pPMetalObjectsInfo)))
  pMetalObjectsInfo <- lift $ peekCStruct @(ExportMetalObjectsInfoEXT _) pPMetalObjectsInfo
  pure $ (pMetalObjectsInfo)


-- | VkExportMetalObjectCreateInfoEXT - Structure that identifies the Metal
-- objects that can be exported from Vulkan objects
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkExportMetalObjectCreateInfoEXT-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXPORT_METAL_OBJECT_CREATE_INFO_EXT'
--
-- -   #VUID-VkExportMetalObjectCreateInfoEXT-exportObjectType-parameter#
--     If @exportObjectType@ is not @0@, @exportObjectType@ /must/ be a
--     valid 'ExportMetalObjectTypeFlagBitsEXT' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_metal_objects VK_EXT_metal_objects>,
-- 'ExportMetalObjectTypeFlagBitsEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ExportMetalObjectCreateInfoEXT = ExportMetalObjectCreateInfoEXT
  { -- | @exportObjectType@ is a 'ExportMetalObjectTypeFlagBitsEXT' indicating
    -- the type of Metal object that the application may request to be exported
    -- from the Vulkan object.
    exportObjectType :: ExportMetalObjectTypeFlagBitsEXT }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExportMetalObjectCreateInfoEXT)
#endif
deriving instance Show ExportMetalObjectCreateInfoEXT

instance ToCStruct ExportMetalObjectCreateInfoEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExportMetalObjectCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_METAL_OBJECT_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExportMetalObjectTypeFlagBitsEXT)) (exportObjectType)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_METAL_OBJECT_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct ExportMetalObjectCreateInfoEXT where
  peekCStruct p = do
    exportObjectType <- peek @ExportMetalObjectTypeFlagBitsEXT ((p `plusPtr` 16 :: Ptr ExportMetalObjectTypeFlagBitsEXT))
    pure $ ExportMetalObjectCreateInfoEXT
             exportObjectType

instance Storable ExportMetalObjectCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExportMetalObjectCreateInfoEXT where
  zero = ExportMetalObjectCreateInfoEXT
           zero


-- | VkExportMetalObjectsInfoEXT - Structure whose pNext chain identifies
-- Vulkan objects and corresponding Metal objects
--
-- == Valid Usage
--
-- -   #VUID-VkExportMetalObjectsInfoEXT-pNext-06791# If the @pNext@ chain
--     includes a 'ExportMetalDeviceInfoEXT' structure, the
--     'Vulkan.Core10.Handles.Instance' /must/ have been created with
--     'EXPORT_METAL_OBJECT_TYPE_METAL_DEVICE_BIT_EXT' in the
--     @exportObjectType@ member of a 'ExportMetalObjectCreateInfoEXT'
--     structure in the @pNext@ chain of the
--     'Vulkan.Core10.DeviceInitialization.InstanceCreateInfo' structure in
--     the 'Vulkan.Core10.DeviceInitialization.createInstance' command
--
-- -   #VUID-VkExportMetalObjectsInfoEXT-pNext-06792# If the @pNext@ chain
--     includes a 'ExportMetalCommandQueueInfoEXT' structure, the
--     'Vulkan.Core10.Handles.Instance' /must/ have been created with
--     'EXPORT_METAL_OBJECT_TYPE_METAL_COMMAND_QUEUE_BIT_EXT' in the
--     @exportObjectType@ member of a 'ExportMetalObjectCreateInfoEXT'
--     structure in the @pNext@ chain of the
--     'Vulkan.Core10.DeviceInitialization.InstanceCreateInfo' structure in
--     the 'Vulkan.Core10.DeviceInitialization.createInstance' command
--
-- -   #VUID-VkExportMetalObjectsInfoEXT-pNext-06793# If the @pNext@ chain
--     includes a 'ExportMetalBufferInfoEXT' structure, the
--     'Vulkan.Core10.Handles.DeviceMemory' in its @memory@ member /must/
--     have been allocated with
--     'EXPORT_METAL_OBJECT_TYPE_METAL_BUFFER_BIT_EXT' in the
--     @exportObjectType@ member of a 'ExportMetalObjectCreateInfoEXT'
--     structure in the @pNext@ chain of the
--     'Vulkan.Core10.Memory.MemoryAllocateInfo' structure in the
--     'Vulkan.Core10.Memory.allocateMemory' command
--
-- -   #VUID-VkExportMetalObjectsInfoEXT-pNext-06794# If the @pNext@ chain
--     includes a 'ExportMetalTextureInfoEXT' structure, exactly one of its
--     @image@, @imageView@, or @bufferView@ members /must/ not be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkExportMetalObjectsInfoEXT-pNext-06795# If the @pNext@ chain
--     includes a 'ExportMetalTextureInfoEXT' structure, and its @image@
--     member is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the
--     'Vulkan.Core10.Handles.Image' in its @image@ member /must/ have been
--     created with 'EXPORT_METAL_OBJECT_TYPE_METAL_TEXTURE_BIT_EXT' in the
--     @exportObjectType@ member of a 'ExportMetalObjectCreateInfoEXT'
--     structure in the @pNext@ chain of the
--     'Vulkan.Core10.Image.ImageCreateInfo' structure in the
--     'Vulkan.Core10.Image.createImage' command
--
-- -   #VUID-VkExportMetalObjectsInfoEXT-pNext-06796# If the @pNext@ chain
--     includes a 'ExportMetalTextureInfoEXT' structure, and its
--     @imageView@ member is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     the 'Vulkan.Core10.Handles.ImageView' in its @imageView@ member
--     /must/ have been created with
--     'EXPORT_METAL_OBJECT_TYPE_METAL_TEXTURE_BIT_EXT' in the
--     @exportObjectType@ member of a 'ExportMetalObjectCreateInfoEXT'
--     structure in the @pNext@ chain of the
--     'Vulkan.Core10.ImageView.ImageViewCreateInfo' structure in the
--     'Vulkan.Core10.ImageView.createImageView' command
--
-- -   #VUID-VkExportMetalObjectsInfoEXT-pNext-06797# If the @pNext@ chain
--     includes a 'ExportMetalTextureInfoEXT' structure, and its
--     @bufferView@ member is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     the 'Vulkan.Core10.Handles.BufferView' in its @bufferView@ member
--     /must/ have been created with
--     'EXPORT_METAL_OBJECT_TYPE_METAL_TEXTURE_BIT_EXT' in the
--     @exportObjectType@ member of a 'ExportMetalObjectCreateInfoEXT'
--     structure in the @pNext@ chain of the
--     'Vulkan.Core10.BufferView.BufferViewCreateInfo' structure in the
--     'Vulkan.Core10.BufferView.createBufferView' command
--
-- -   #VUID-VkExportMetalObjectsInfoEXT-pNext-06798# If the @pNext@ chain
--     includes a 'ExportMetalTextureInfoEXT' structure, and if either its
--     @image@ or @imageView@ member is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', then @plane@ /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT',
--     or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'
--
-- -   #VUID-VkExportMetalObjectsInfoEXT-pNext-06799# If the @pNext@ chain
--     includes a 'ExportMetalTextureInfoEXT' structure, and if the
--     'Vulkan.Core10.Handles.Image' in its @image@ member does not have a
--     multi-planar format, then its @plane@ member /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT'
--
-- -   #VUID-VkExportMetalObjectsInfoEXT-pNext-06800# If the @pNext@ chain
--     includes a 'ExportMetalTextureInfoEXT' structure, and if the
--     'Vulkan.Core10.Handles.Image' in its @image@ member has a
--     multi-planar format with only two planes, then its @plane@ member
--     /must/ not be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'
--
-- -   #VUID-VkExportMetalObjectsInfoEXT-pNext-06801# If the @pNext@ chain
--     includes a 'ExportMetalTextureInfoEXT' structure, and if the
--     'Vulkan.Core10.Handles.ImageView' in its @imageView@ member does not
--     have a multi-planar format, then its @plane@ member /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT'
--
-- -   #VUID-VkExportMetalObjectsInfoEXT-pNext-06802# If the @pNext@ chain
--     includes a 'ExportMetalTextureInfoEXT' structure, and if the
--     'Vulkan.Core10.Handles.ImageView' in its @imageView@ member has a
--     multi-planar format with only two planes, then its @plane@ member
--     /must/ not be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'
--
-- -   #VUID-VkExportMetalObjectsInfoEXT-pNext-06803# If the @pNext@ chain
--     includes a 'ExportMetalIOSurfaceInfoEXT' structure, the
--     'Vulkan.Core10.Handles.Image' in its @image@ member /must/ have been
--     created with 'EXPORT_METAL_OBJECT_TYPE_METAL_IOSURFACE_BIT_EXT' in
--     the @exportObjectType@ member of a 'ExportMetalObjectCreateInfoEXT'
--     structure in the @pNext@ chain of the
--     'Vulkan.Core10.Image.ImageCreateInfo' structure in the
--     'Vulkan.Core10.Image.createImage' command
--
-- -   #VUID-VkExportMetalObjectsInfoEXT-pNext-06804# If the @pNext@ chain
--     includes a 'ExportMetalSharedEventInfoEXT' structure, exactly one of
--     its @semaphore@ or @event@ members /must/ not be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkExportMetalObjectsInfoEXT-pNext-06805# If the @pNext@ chain
--     includes a 'ExportMetalSharedEventInfoEXT' structure, and its
--     @semaphore@ member is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     the 'Vulkan.Core10.Handles.Semaphore' in its @semaphore@ member
--     /must/ have been created with
--     'EXPORT_METAL_OBJECT_TYPE_METAL_SHARED_EVENT_BIT_EXT' in the
--     @exportObjectType@ member of a 'ExportMetalObjectCreateInfoEXT'
--     structure in the @pNext@ chain of the
--     'Vulkan.Core10.QueueSemaphore.SemaphoreCreateInfo' structure in the
--     'Vulkan.Core10.QueueSemaphore.createSemaphore' command
--
-- -   #VUID-VkExportMetalObjectsInfoEXT-pNext-06806# If the @pNext@ chain
--     includes a 'ExportMetalSharedEventInfoEXT' structure, and its
--     @event@ member is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the
--     'Vulkan.Core10.Handles.Event' in its @event@ member /must/ have been
--     created with 'EXPORT_METAL_OBJECT_TYPE_METAL_SHARED_EVENT_BIT_EXT'
--     in the @exportObjectType@ member of a
--     'ExportMetalObjectCreateInfoEXT' structure in the @pNext@ chain of
--     the 'Vulkan.Core10.Event.EventCreateInfo' structure in the
--     'Vulkan.Core10.Event.createEvent' command
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkExportMetalObjectsInfoEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXPORT_METAL_OBJECTS_INFO_EXT'
--
-- -   #VUID-VkExportMetalObjectsInfoEXT-pNext-pNext# Each @pNext@ member
--     of any structure (including this one) in the @pNext@ chain /must/ be
--     either @NULL@ or a pointer to a valid instance of
--     'ExportMetalBufferInfoEXT', 'ExportMetalCommandQueueInfoEXT',
--     'ExportMetalDeviceInfoEXT', 'ExportMetalIOSurfaceInfoEXT',
--     'ExportMetalSharedEventInfoEXT', or 'ExportMetalTextureInfoEXT'
--
-- -   #VUID-VkExportMetalObjectsInfoEXT-sType-unique# The @sType@ value of
--     each struct in the @pNext@ chain /must/ be unique, with the
--     exception of structures of type 'ExportMetalBufferInfoEXT',
--     'ExportMetalCommandQueueInfoEXT', 'ExportMetalIOSurfaceInfoEXT',
--     'ExportMetalSharedEventInfoEXT', or 'ExportMetalTextureInfoEXT'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_metal_objects VK_EXT_metal_objects>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'exportMetalObjectsEXT'
data ExportMetalObjectsInfoEXT (es :: [Type]) = ExportMetalObjectsInfoEXT
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExportMetalObjectsInfoEXT (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (ExportMetalObjectsInfoEXT es)

instance Extensible ExportMetalObjectsInfoEXT where
  extensibleTypeName = "ExportMetalObjectsInfoEXT"
  setNext _ next' = ExportMetalObjectsInfoEXT{next = next'}
  getNext ExportMetalObjectsInfoEXT{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends ExportMetalObjectsInfoEXT e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @ExportMetalSharedEventInfoEXT = Just f
    | Just Refl <- eqT @e @ExportMetalIOSurfaceInfoEXT = Just f
    | Just Refl <- eqT @e @ExportMetalTextureInfoEXT = Just f
    | Just Refl <- eqT @e @ExportMetalBufferInfoEXT = Just f
    | Just Refl <- eqT @e @ExportMetalCommandQueueInfoEXT = Just f
    | Just Refl <- eqT @e @ExportMetalDeviceInfoEXT = Just f
    | otherwise = Nothing

instance ( Extendss ExportMetalObjectsInfoEXT es
         , PokeChain es ) => ToCStruct (ExportMetalObjectsInfoEXT es) where
  withCStruct x f = allocaBytes 16 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExportMetalObjectsInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_METAL_OBJECTS_INFO_EXT)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_METAL_OBJECTS_INFO_EXT)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ f

instance ( Extendss ExportMetalObjectsInfoEXT es
         , PeekChain es ) => FromCStruct (ExportMetalObjectsInfoEXT es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    pure $ ExportMetalObjectsInfoEXT
             next

instance es ~ '[] => Zero (ExportMetalObjectsInfoEXT es) where
  zero = ExportMetalObjectsInfoEXT
           ()


-- | VkExportMetalDeviceInfoEXT - Structure that identifies a VkDevice object
-- and corresponding Metal MTLDevice object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_metal_objects VK_EXT_metal_objects>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ExportMetalDeviceInfoEXT = ExportMetalDeviceInfoEXT
  { -- | @mtlDevice@ is the Metal @id\<MTLDevice>@ object underlying the
    -- 'Vulkan.Core10.Handles.PhysicalDevice' associated with the
    -- 'Vulkan.Core10.Handles.Device' object identified in the call. The
    -- implementation will return the @MTLDevice@ in this member, or it will
    -- return @NULL@ if no @MTLDevice@ could be found underlying the
    -- 'Vulkan.Core10.Handles.PhysicalDevice' object.
    mtlDevice :: MTLDevice_id }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExportMetalDeviceInfoEXT)
#endif
deriving instance Show ExportMetalDeviceInfoEXT

instance ToCStruct ExportMetalDeviceInfoEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExportMetalDeviceInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_METAL_DEVICE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr MTLDevice_id)) (mtlDevice)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_METAL_DEVICE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr MTLDevice_id)) (zero)
    f

instance FromCStruct ExportMetalDeviceInfoEXT where
  peekCStruct p = do
    mtlDevice <- peek @MTLDevice_id ((p `plusPtr` 16 :: Ptr MTLDevice_id))
    pure $ ExportMetalDeviceInfoEXT
             mtlDevice

instance Storable ExportMetalDeviceInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExportMetalDeviceInfoEXT where
  zero = ExportMetalDeviceInfoEXT
           zero


-- | VkExportMetalCommandQueueInfoEXT - Structure that identifies a VkQueue
-- object and corresponding Metal MTLCommandQueue object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_metal_objects VK_EXT_metal_objects>,
-- 'Vulkan.Core10.Handles.Queue',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ExportMetalCommandQueueInfoEXT = ExportMetalCommandQueueInfoEXT
  { -- | @queue@ is a 'Vulkan.Core10.Handles.Queue'.
    --
    -- #VUID-VkExportMetalCommandQueueInfoEXT-queue-parameter# @queue@ /must/
    -- be a valid 'Vulkan.Core10.Handles.Queue' handle
    queue :: Ptr Queue_T
  , -- | @mtlCommandQueue@ is the Metal @id\<MTLCommandQueue>@ object underlying
    -- the 'Vulkan.Core10.Handles.Queue' object in @queue@. The implementation
    -- will return the @MTLCommandQueue@ in this member, or it will return
    -- @NULL@ if no @MTLCommandQueue@ could be found underlying the
    -- 'Vulkan.Core10.Handles.Queue' object.
    mtlCommandQueue :: MTLCommandQueue_id
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExportMetalCommandQueueInfoEXT)
#endif
deriving instance Show ExportMetalCommandQueueInfoEXT

instance ToCStruct ExportMetalCommandQueueInfoEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExportMetalCommandQueueInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_METAL_COMMAND_QUEUE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Queue_T))) (queue)
    poke ((p `plusPtr` 24 :: Ptr MTLCommandQueue_id)) (mtlCommandQueue)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_METAL_COMMAND_QUEUE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Queue_T))) (zero)
    poke ((p `plusPtr` 24 :: Ptr MTLCommandQueue_id)) (zero)
    f

instance FromCStruct ExportMetalCommandQueueInfoEXT where
  peekCStruct p = do
    queue <- peek @(Ptr Queue_T) ((p `plusPtr` 16 :: Ptr (Ptr Queue_T)))
    mtlCommandQueue <- peek @MTLCommandQueue_id ((p `plusPtr` 24 :: Ptr MTLCommandQueue_id))
    pure $ ExportMetalCommandQueueInfoEXT
             queue mtlCommandQueue

instance Storable ExportMetalCommandQueueInfoEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExportMetalCommandQueueInfoEXT where
  zero = ExportMetalCommandQueueInfoEXT
           zero
           zero


-- | VkExportMetalBufferInfoEXT - Structure that identifies a VkDeviceMemory
-- object and corresponding Metal MTLBuffer object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_metal_objects VK_EXT_metal_objects>,
-- 'Vulkan.Core10.Handles.DeviceMemory',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ExportMetalBufferInfoEXT = ExportMetalBufferInfoEXT
  { -- | @memory@ is a 'Vulkan.Core10.Handles.DeviceMemory'.
    --
    -- #VUID-VkExportMetalBufferInfoEXT-memory-parameter# @memory@ /must/ be a
    -- valid 'Vulkan.Core10.Handles.DeviceMemory' handle
    memory :: DeviceMemory
  , -- | @mtlBuffer@ is the Metal @id\<MTLBuffer>@ object underlying the
    -- 'Vulkan.Core10.Handles.DeviceMemory' object in @memory@. The
    -- implementation will return the @MTLBuffer@ in this member, or it will
    -- return @NULL@ if no @MTLBuffer@ could be found underlying the
    -- 'Vulkan.Core10.Handles.DeviceMemory' object.
    mtlBuffer :: MTLBuffer_id
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExportMetalBufferInfoEXT)
#endif
deriving instance Show ExportMetalBufferInfoEXT

instance ToCStruct ExportMetalBufferInfoEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExportMetalBufferInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_METAL_BUFFER_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceMemory)) (memory)
    poke ((p `plusPtr` 24 :: Ptr MTLBuffer_id)) (mtlBuffer)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_METAL_BUFFER_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceMemory)) (zero)
    poke ((p `plusPtr` 24 :: Ptr MTLBuffer_id)) (zero)
    f

instance FromCStruct ExportMetalBufferInfoEXT where
  peekCStruct p = do
    memory <- peek @DeviceMemory ((p `plusPtr` 16 :: Ptr DeviceMemory))
    mtlBuffer <- peek @MTLBuffer_id ((p `plusPtr` 24 :: Ptr MTLBuffer_id))
    pure $ ExportMetalBufferInfoEXT
             memory mtlBuffer

instance Storable ExportMetalBufferInfoEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExportMetalBufferInfoEXT where
  zero = ExportMetalBufferInfoEXT
           zero
           zero


-- | VkImportMetalBufferInfoEXT - Structure that identifies a Metal MTLBuffer
-- object to use when creating a VkDeviceMemory object.
--
-- = Description
--
-- The app /must/ ensure that the configuration of the @id\<MTLBuffer>@
-- object is compatible with the configuration of the
-- 'Vulkan.Core10.Handles.DeviceMemory'. Failure to do so results in
-- undefined behavior.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_metal_objects VK_EXT_metal_objects>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ImportMetalBufferInfoEXT = ImportMetalBufferInfoEXT
  { -- | @mtlBuffer@ is the Metal @id\<MTLBuffer>@ object that is to underlie the
    -- 'Vulkan.Core10.Handles.DeviceMemory'.
    mtlBuffer :: MTLBuffer_id }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImportMetalBufferInfoEXT)
#endif
deriving instance Show ImportMetalBufferInfoEXT

instance ToCStruct ImportMetalBufferInfoEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImportMetalBufferInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_METAL_BUFFER_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr MTLBuffer_id)) (mtlBuffer)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_METAL_BUFFER_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr MTLBuffer_id)) (zero)
    f

instance FromCStruct ImportMetalBufferInfoEXT where
  peekCStruct p = do
    mtlBuffer <- peek @MTLBuffer_id ((p `plusPtr` 16 :: Ptr MTLBuffer_id))
    pure $ ImportMetalBufferInfoEXT
             mtlBuffer

instance Storable ImportMetalBufferInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImportMetalBufferInfoEXT where
  zero = ImportMetalBufferInfoEXT
           zero


-- | VkExportMetalTextureInfoEXT - Structure that identifies a VkImage,
-- VkImageView, or VkBufferView object and corresponding Metal MTLTexture
-- object
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkExportMetalTextureInfoEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXPORT_METAL_TEXTURE_INFO_EXT'
--
-- -   #VUID-VkExportMetalTextureInfoEXT-image-parameter# If @image@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @image@ /must/ be a valid
--     'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-VkExportMetalTextureInfoEXT-imageView-parameter# If
--     @imageView@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @imageView@ /must/ be a valid 'Vulkan.Core10.Handles.ImageView'
--     handle
--
-- -   #VUID-VkExportMetalTextureInfoEXT-bufferView-parameter# If
--     @bufferView@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @bufferView@ /must/ be a valid 'Vulkan.Core10.Handles.BufferView'
--     handle
--
-- -   #VUID-VkExportMetalTextureInfoEXT-plane-parameter# @plane@ /must/ be
--     a valid
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlagBits' value
--
-- -   #VUID-VkExportMetalTextureInfoEXT-commonparent# Each of
--     @bufferView@, @image@, and @imageView@ that are valid handles of
--     non-ignored parameters /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_metal_objects VK_EXT_metal_objects>,
-- 'Vulkan.Core10.Handles.BufferView', 'Vulkan.Core10.Handles.Image',
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlagBits',
-- 'Vulkan.Core10.Handles.ImageView',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ExportMetalTextureInfoEXT = ExportMetalTextureInfoEXT
  { -- | @image@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE' or a
    -- 'Vulkan.Core10.Handles.Image'.
    image :: Image
  , -- | @imageView@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE' or a
    -- 'Vulkan.Core10.Handles.ImageView'.
    imageView :: ImageView
  , -- | @bufferView@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE' or a
    -- 'Vulkan.Core10.Handles.BufferView'.
    bufferView :: BufferView
  , -- | @plane@ indicates the plane of a multi-planar
    -- 'Vulkan.Core10.Handles.Image' or 'Vulkan.Core10.Handles.ImageView'.
    plane :: ImageAspectFlagBits
  , -- | @mtlTexture@ is the Metal @id\<MTLTexture>@ object underlying the
    -- 'Vulkan.Core10.Handles.Image', 'Vulkan.Core10.Handles.ImageView', or
    -- 'Vulkan.Core10.Handles.BufferView' object in @image@, @imageView@, or
    -- @bufferView@, respectively, at the plane indicated in @aspectMask@. The
    -- implementation will return the @MTLTexture@ in this member, or it will
    -- return @NULL@ if no @MTLTexture@ could be found underlying the
    -- 'Vulkan.Core10.Handles.Image', 'Vulkan.Core10.Handles.ImageView', or
    -- 'Vulkan.Core10.Handles.BufferView' object, at the plane indicated in
    -- @aspectMask@.
    mtlTexture :: MTLTexture_id
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExportMetalTextureInfoEXT)
#endif
deriving instance Show ExportMetalTextureInfoEXT

instance ToCStruct ExportMetalTextureInfoEXT where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExportMetalTextureInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_METAL_TEXTURE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Image)) (image)
    poke ((p `plusPtr` 24 :: Ptr ImageView)) (imageView)
    poke ((p `plusPtr` 32 :: Ptr BufferView)) (bufferView)
    poke ((p `plusPtr` 40 :: Ptr ImageAspectFlagBits)) (plane)
    poke ((p `plusPtr` 48 :: Ptr MTLTexture_id)) (mtlTexture)
    f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_METAL_TEXTURE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 40 :: Ptr ImageAspectFlagBits)) (zero)
    poke ((p `plusPtr` 48 :: Ptr MTLTexture_id)) (zero)
    f

instance FromCStruct ExportMetalTextureInfoEXT where
  peekCStruct p = do
    image <- peek @Image ((p `plusPtr` 16 :: Ptr Image))
    imageView <- peek @ImageView ((p `plusPtr` 24 :: Ptr ImageView))
    bufferView <- peek @BufferView ((p `plusPtr` 32 :: Ptr BufferView))
    plane <- peek @ImageAspectFlagBits ((p `plusPtr` 40 :: Ptr ImageAspectFlagBits))
    mtlTexture <- peek @MTLTexture_id ((p `plusPtr` 48 :: Ptr MTLTexture_id))
    pure $ ExportMetalTextureInfoEXT
             image imageView bufferView plane mtlTexture

instance Storable ExportMetalTextureInfoEXT where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExportMetalTextureInfoEXT where
  zero = ExportMetalTextureInfoEXT
           zero
           zero
           zero
           zero
           zero


-- | VkImportMetalTextureInfoEXT - Structure that identifies Metal MTLTexture
-- objects to use when creating a VkImage.
--
-- = Description
--
-- The @pNext@ chain /must/ include one 'ImportMetalTextureInfoEXT'
-- structure for each plane in the 'Vulkan.Core10.Handles.Image'. The app
-- /must/ ensure that the configuration of the Metal @id\<MTLTexture>@
-- objects are compatible with the configuration of the
-- 'Vulkan.Core10.Handles.Image'. Failure to do so results in undefined
-- behavior.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_metal_objects VK_EXT_metal_objects>,
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlagBits',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ImportMetalTextureInfoEXT = ImportMetalTextureInfoEXT
  { -- | @plane@ indicates the plane of the 'Vulkan.Core10.Handles.Image' that
    -- the @id\<MTLTexture>@ object should be attached to.
    --
    -- #VUID-VkImportMetalTextureInfoEXT-plane-parameter# @plane@ /must/ be a
    -- valid 'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlagBits'
    -- value
    plane :: ImageAspectFlagBits
  , -- | @mtlTexture@ is a the Metal @id\<MTLTexture>@ object that is to underlie
    -- the 'Vulkan.Core10.Handles.Image' plane.
    mtlTexture :: MTLTexture_id
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImportMetalTextureInfoEXT)
#endif
deriving instance Show ImportMetalTextureInfoEXT

instance ToCStruct ImportMetalTextureInfoEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImportMetalTextureInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_METAL_TEXTURE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageAspectFlagBits)) (plane)
    poke ((p `plusPtr` 24 :: Ptr MTLTexture_id)) (mtlTexture)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_METAL_TEXTURE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageAspectFlagBits)) (zero)
    poke ((p `plusPtr` 24 :: Ptr MTLTexture_id)) (zero)
    f

instance FromCStruct ImportMetalTextureInfoEXT where
  peekCStruct p = do
    plane <- peek @ImageAspectFlagBits ((p `plusPtr` 16 :: Ptr ImageAspectFlagBits))
    mtlTexture <- peek @MTLTexture_id ((p `plusPtr` 24 :: Ptr MTLTexture_id))
    pure $ ImportMetalTextureInfoEXT
             plane mtlTexture

instance Storable ImportMetalTextureInfoEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImportMetalTextureInfoEXT where
  zero = ImportMetalTextureInfoEXT
           zero
           zero


-- | VkExportMetalIOSurfaceInfoEXT - Structure that identifies a VkImage
-- object and corresponding Metal IOSurfaceRef object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_metal_objects VK_EXT_metal_objects>,
-- 'Vulkan.Core10.Handles.Image',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ExportMetalIOSurfaceInfoEXT = ExportMetalIOSurfaceInfoEXT
  { -- | @image@ is a 'Vulkan.Core10.Handles.Image'.
    --
    -- #VUID-VkExportMetalIOSurfaceInfoEXT-image-parameter# @image@ /must/ be a
    -- valid 'Vulkan.Core10.Handles.Image' handle
    image :: Image
  , -- | @ioSurface@ is the Metal 'IOSurfaceRef' object underlying the
    -- 'Vulkan.Core10.Handles.Image' object in @image@. The implementation will
    -- return the 'IOSurfaceRef' in this member, or it will return @NULL@ if no
    -- 'IOSurfaceRef' could be found underlying the
    -- 'Vulkan.Core10.Handles.Image' object.
    ioSurface :: IOSurfaceRef
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExportMetalIOSurfaceInfoEXT)
#endif
deriving instance Show ExportMetalIOSurfaceInfoEXT

instance ToCStruct ExportMetalIOSurfaceInfoEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExportMetalIOSurfaceInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_METAL_IO_SURFACE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Image)) (image)
    poke ((p `plusPtr` 24 :: Ptr IOSurfaceRef)) (ioSurface)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_METAL_IO_SURFACE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Image)) (zero)
    poke ((p `plusPtr` 24 :: Ptr IOSurfaceRef)) (zero)
    f

instance FromCStruct ExportMetalIOSurfaceInfoEXT where
  peekCStruct p = do
    image <- peek @Image ((p `plusPtr` 16 :: Ptr Image))
    ioSurface <- peek @IOSurfaceRef ((p `plusPtr` 24 :: Ptr IOSurfaceRef))
    pure $ ExportMetalIOSurfaceInfoEXT
             image ioSurface

instance Storable ExportMetalIOSurfaceInfoEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExportMetalIOSurfaceInfoEXT where
  zero = ExportMetalIOSurfaceInfoEXT
           zero
           zero


-- | VkImportMetalIOSurfaceInfoEXT - Structure that identifies a VkImage
-- object and corresponding Metal IOSurfaceRef object to use.
--
-- = Description
--
-- If @ioSurface@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', it will
-- be used to underlie the 'Vulkan.Core10.Handles.Image'. If @ioSurface@ is
-- 'Vulkan.Core10.APIConstants.NULL_HANDLE', the implementation will create
-- a new @IOSurface@ to underlie the 'Vulkan.Core10.Handles.Image'.
--
-- If provided, the app /must/ ensure that the configuration of the
-- 'IOSurfaceRef' object is compatible with the configuration of the
-- 'Vulkan.Core10.Handles.Image'. Failure to do so results in undefined
-- behavior.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_metal_objects VK_EXT_metal_objects>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ImportMetalIOSurfaceInfoEXT = ImportMetalIOSurfaceInfoEXT
  { -- | @ioSurface@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE' or the Metal
    -- 'IOSurfaceRef' object that is to underlie the
    -- 'Vulkan.Core10.Handles.Image'.
    ioSurface :: IOSurfaceRef }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImportMetalIOSurfaceInfoEXT)
#endif
deriving instance Show ImportMetalIOSurfaceInfoEXT

instance ToCStruct ImportMetalIOSurfaceInfoEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImportMetalIOSurfaceInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_METAL_IO_SURFACE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr IOSurfaceRef)) (ioSurface)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_METAL_IO_SURFACE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct ImportMetalIOSurfaceInfoEXT where
  peekCStruct p = do
    ioSurface <- peek @IOSurfaceRef ((p `plusPtr` 16 :: Ptr IOSurfaceRef))
    pure $ ImportMetalIOSurfaceInfoEXT
             ioSurface

instance Storable ImportMetalIOSurfaceInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImportMetalIOSurfaceInfoEXT where
  zero = ImportMetalIOSurfaceInfoEXT
           zero


-- | VkExportMetalSharedEventInfoEXT - Structure that identifies a
-- VkSemaphore or VkEvent object and corresponding Metal MTLSharedEvent
-- object
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkExportMetalSharedEventInfoEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXPORT_METAL_SHARED_EVENT_INFO_EXT'
--
-- -   #VUID-VkExportMetalSharedEventInfoEXT-semaphore-parameter# If
--     @semaphore@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @semaphore@ /must/ be a valid 'Vulkan.Core10.Handles.Semaphore'
--     handle
--
-- -   #VUID-VkExportMetalSharedEventInfoEXT-event-parameter# If @event@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @event@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Event' handle
--
-- -   #VUID-VkExportMetalSharedEventInfoEXT-commonparent# Both of @event@,
--     and @semaphore@ that are valid handles of non-ignored parameters
--     /must/ have been created, allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_metal_objects VK_EXT_metal_objects>,
-- 'Vulkan.Core10.Handles.Event', 'Vulkan.Core10.Handles.Semaphore',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ExportMetalSharedEventInfoEXT = ExportMetalSharedEventInfoEXT
  { -- | @semaphore@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE' or a
    -- 'Vulkan.Core10.Handles.Semaphore'.
    semaphore :: Semaphore
  , -- | @event@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE' or a
    -- 'Vulkan.Core10.Handles.Event'.
    event :: Event
  , -- | @mtlSharedEvent@ is the Metal @id\<MTLSharedEvent>@ object underlying
    -- the 'Vulkan.Core10.Handles.Semaphore' or 'Vulkan.Core10.Handles.Event'
    -- object in @semaphore@ or @event@, respectively. The implementation will
    -- return the @MTLSharedEvent@ in this member, or it will return @NULL@ if
    -- no @MTLSharedEvent@ could be found underlying the
    -- 'Vulkan.Core10.Handles.Semaphore' or 'Vulkan.Core10.Handles.Event'
    -- object.
    mtlSharedEvent :: MTLSharedEvent_id
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExportMetalSharedEventInfoEXT)
#endif
deriving instance Show ExportMetalSharedEventInfoEXT

instance ToCStruct ExportMetalSharedEventInfoEXT where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExportMetalSharedEventInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_METAL_SHARED_EVENT_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Semaphore)) (semaphore)
    poke ((p `plusPtr` 24 :: Ptr Event)) (event)
    poke ((p `plusPtr` 32 :: Ptr MTLSharedEvent_id)) (mtlSharedEvent)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_METAL_SHARED_EVENT_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 32 :: Ptr MTLSharedEvent_id)) (zero)
    f

instance FromCStruct ExportMetalSharedEventInfoEXT where
  peekCStruct p = do
    semaphore <- peek @Semaphore ((p `plusPtr` 16 :: Ptr Semaphore))
    event <- peek @Event ((p `plusPtr` 24 :: Ptr Event))
    mtlSharedEvent <- peek @MTLSharedEvent_id ((p `plusPtr` 32 :: Ptr MTLSharedEvent_id))
    pure $ ExportMetalSharedEventInfoEXT
             semaphore event mtlSharedEvent

instance Storable ExportMetalSharedEventInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExportMetalSharedEventInfoEXT where
  zero = ExportMetalSharedEventInfoEXT
           zero
           zero
           zero


-- | VkImportMetalSharedEventInfoEXT - Structure that identifies a
-- VkSemaphore or VkEvent object and corresponding Metal Shared Event
-- object to use.
--
-- = Description
--
-- If the @pNext@ chain of the
-- 'Vulkan.Core10.QueueSemaphore.SemaphoreCreateInfo' structure includes
-- both 'ImportMetalSharedEventInfoEXT' and
-- 'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.SemaphoreTypeCreateInfo',
-- the @signaledValue@ property of the imported @id\<MTLSharedEvent>@
-- object will be set to @initialValue@ of
-- 'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.SemaphoreTypeCreateInfo'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_metal_objects VK_EXT_metal_objects>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ImportMetalSharedEventInfoEXT = ImportMetalSharedEventInfoEXT
  { -- | @mtlSharedEvent@ is the Metal @id\<MTLSharedEvent>@ object that is to
    -- underlie the 'Vulkan.Core10.Handles.Semaphore' or
    -- 'Vulkan.Core10.Handles.Event'.
    mtlSharedEvent :: MTLSharedEvent_id }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImportMetalSharedEventInfoEXT)
#endif
deriving instance Show ImportMetalSharedEventInfoEXT

instance ToCStruct ImportMetalSharedEventInfoEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImportMetalSharedEventInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_METAL_SHARED_EVENT_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr MTLSharedEvent_id)) (mtlSharedEvent)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_METAL_SHARED_EVENT_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr MTLSharedEvent_id)) (zero)
    f

instance FromCStruct ImportMetalSharedEventInfoEXT where
  peekCStruct p = do
    mtlSharedEvent <- peek @MTLSharedEvent_id ((p `plusPtr` 16 :: Ptr MTLSharedEvent_id))
    pure $ ImportMetalSharedEventInfoEXT
             mtlSharedEvent

instance Storable ImportMetalSharedEventInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImportMetalSharedEventInfoEXT where
  zero = ImportMetalSharedEventInfoEXT
           zero


type ExportMetalObjectTypeFlagsEXT = ExportMetalObjectTypeFlagBitsEXT

-- | VkExportMetalObjectTypeFlagBitsEXT - Bitmask specifying Metal object
-- types that can be exported from a Vulkan object
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_metal_objects VK_EXT_metal_objects>,
-- 'ExportMetalObjectCreateInfoEXT', 'ExportMetalObjectTypeFlagsEXT'
newtype ExportMetalObjectTypeFlagBitsEXT = ExportMetalObjectTypeFlagBitsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'EXPORT_METAL_OBJECT_TYPE_METAL_DEVICE_BIT_EXT' indicates a Metal
-- @MTLDevice@ may be exported.
pattern EXPORT_METAL_OBJECT_TYPE_METAL_DEVICE_BIT_EXT = ExportMetalObjectTypeFlagBitsEXT 0x00000001

-- | 'EXPORT_METAL_OBJECT_TYPE_METAL_COMMAND_QUEUE_BIT_EXT' indicates a Metal
-- @MTLCommandQueue@ may be exported.
pattern EXPORT_METAL_OBJECT_TYPE_METAL_COMMAND_QUEUE_BIT_EXT = ExportMetalObjectTypeFlagBitsEXT 0x00000002

-- | 'EXPORT_METAL_OBJECT_TYPE_METAL_BUFFER_BIT_EXT' indicates a Metal
-- @MTLBuffer@ may be exported.
pattern EXPORT_METAL_OBJECT_TYPE_METAL_BUFFER_BIT_EXT = ExportMetalObjectTypeFlagBitsEXT 0x00000004

-- | 'EXPORT_METAL_OBJECT_TYPE_METAL_TEXTURE_BIT_EXT' indicates a Metal
-- @MTLTexture@ may be exported.
pattern EXPORT_METAL_OBJECT_TYPE_METAL_TEXTURE_BIT_EXT = ExportMetalObjectTypeFlagBitsEXT 0x00000008

-- | 'EXPORT_METAL_OBJECT_TYPE_METAL_IOSURFACE_BIT_EXT' indicates a Metal
-- @IOSurface@ may be exported.
pattern EXPORT_METAL_OBJECT_TYPE_METAL_IOSURFACE_BIT_EXT = ExportMetalObjectTypeFlagBitsEXT 0x00000010

-- | 'EXPORT_METAL_OBJECT_TYPE_METAL_SHARED_EVENT_BIT_EXT' indicates a Metal
-- @MTLSharedEvent@ may be exported.
pattern EXPORT_METAL_OBJECT_TYPE_METAL_SHARED_EVENT_BIT_EXT = ExportMetalObjectTypeFlagBitsEXT 0x00000020

conNameExportMetalObjectTypeFlagBitsEXT :: String
conNameExportMetalObjectTypeFlagBitsEXT = "ExportMetalObjectTypeFlagBitsEXT"

enumPrefixExportMetalObjectTypeFlagBitsEXT :: String
enumPrefixExportMetalObjectTypeFlagBitsEXT = "EXPORT_METAL_OBJECT_TYPE_METAL_"

showTableExportMetalObjectTypeFlagBitsEXT :: [(ExportMetalObjectTypeFlagBitsEXT, String)]
showTableExportMetalObjectTypeFlagBitsEXT =
  [
    ( EXPORT_METAL_OBJECT_TYPE_METAL_DEVICE_BIT_EXT
    , "DEVICE_BIT_EXT"
    )
  ,
    ( EXPORT_METAL_OBJECT_TYPE_METAL_COMMAND_QUEUE_BIT_EXT
    , "COMMAND_QUEUE_BIT_EXT"
    )
  ,
    ( EXPORT_METAL_OBJECT_TYPE_METAL_BUFFER_BIT_EXT
    , "BUFFER_BIT_EXT"
    )
  ,
    ( EXPORT_METAL_OBJECT_TYPE_METAL_TEXTURE_BIT_EXT
    , "TEXTURE_BIT_EXT"
    )
  ,
    ( EXPORT_METAL_OBJECT_TYPE_METAL_IOSURFACE_BIT_EXT
    , "IOSURFACE_BIT_EXT"
    )
  ,
    ( EXPORT_METAL_OBJECT_TYPE_METAL_SHARED_EVENT_BIT_EXT
    , "SHARED_EVENT_BIT_EXT"
    )
  ]

instance Show ExportMetalObjectTypeFlagBitsEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixExportMetalObjectTypeFlagBitsEXT
      showTableExportMetalObjectTypeFlagBitsEXT
      conNameExportMetalObjectTypeFlagBitsEXT
      (\(ExportMetalObjectTypeFlagBitsEXT x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read ExportMetalObjectTypeFlagBitsEXT where
  readPrec =
    enumReadPrec
      enumPrefixExportMetalObjectTypeFlagBitsEXT
      showTableExportMetalObjectTypeFlagBitsEXT
      conNameExportMetalObjectTypeFlagBitsEXT
      ExportMetalObjectTypeFlagBitsEXT

type EXT_METAL_OBJECTS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_METAL_OBJECTS_SPEC_VERSION"
pattern EXT_METAL_OBJECTS_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_METAL_OBJECTS_SPEC_VERSION = 1


type EXT_METAL_OBJECTS_EXTENSION_NAME = "VK_EXT_metal_objects"

-- No documentation found for TopLevel "VK_EXT_METAL_OBJECTS_EXTENSION_NAME"
pattern EXT_METAL_OBJECTS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_METAL_OBJECTS_EXTENSION_NAME = "VK_EXT_metal_objects"


type MTLDevice_id = Ptr ()


type MTLCommandQueue_id = Ptr ()


type MTLBuffer_id = Ptr ()


type MTLTexture_id = Ptr ()


type IOSurfaceRef = Ptr ()


type MTLSharedEvent_id = Ptr ()

