{-# language CPP #-}
-- | = Name
--
-- VK_NV_external_memory_win32 - device extension
--
-- == VK_NV_external_memory_win32
--
-- [__Name String__]
--     @VK_NV_external_memory_win32@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     58
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_NV_external_memory@
--
-- [__Deprecation state__]
--
--     -   /Deprecated/ by @VK_KHR_external_memory_win32@ extension
--
-- [__Contact__]
--
--     -   James Jones
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_NV_external_memory_win32:%20&body=@cubanismo%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2016-08-19
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   James Jones, NVIDIA
--
--     -   Carsten Rohde, NVIDIA
--
-- == Description
--
-- Applications may wish to export memory to other Vulkan instances or
-- other APIs, or import memory from other Vulkan instances or other APIs
-- to enable Vulkan workloads to be split up across application module,
-- process, or API boundaries. This extension enables win32 applications to
-- export win32 handles from Vulkan memory objects such that the underlying
-- resources can be referenced outside the Vulkan instance that created
-- them, and import win32 handles created in the Direct3D API to Vulkan
-- memory objects.
--
-- == New Commands
--
-- -   'getMemoryWin32HandleNV'
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.Memory.MemoryAllocateInfo':
--
--     -   'ExportMemoryWin32HandleInfoNV'
--
--     -   'ImportMemoryWin32HandleInfoNV'
--
-- == New Enum Constants
--
-- -   'NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME'
--
-- -   'NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV'
--
-- == Issues
--
-- 1) If memory objects are shared between processes and APIs, is this
-- considered aliasing according to the rules outlined in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-memory-aliasing Memory Aliasing>
-- section?
--
-- __RESOLVED__: Yes, but strict exceptions to the rules are added to allow
-- some forms of aliasing in these cases. Further, other extensions may
-- build upon these new aliasing rules to define specific support usage
-- within Vulkan for imported native memory objects, or memory objects from
-- other APIs.
--
-- 2) Are new image layouts or metadata required to specify image layouts
-- and layout transitions compatible with non-Vulkan APIs, or with other
-- instances of the same Vulkan driver?
--
-- __RESOLVED__: No. Separate instances of the same Vulkan driver running
-- on the same GPU should have identical internal layout semantics, so
-- applictions have the tools they need to ensure views of images are
-- consistent between the two instances. Other APIs will fall into two
-- categories: Those that are Vulkan compatible (a term to be defined by
-- subsequent interopability extensions), or Vulkan incompatible. When
-- sharing images with Vulkan incompatible APIs, the Vulkan image must be
-- transitioned to the
-- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL' layout before
-- handing it off to the external API.
--
-- Note this does not attempt to address cross-device transitions, nor
-- transitions to engines on the same device which are not visible within
-- the Vulkan API. Both of these are beyond the scope of this extension.
--
-- 3) Do applications need to call @CloseHandle@() on the values returned
-- from 'getMemoryWin32HandleNV' when @handleType@ is
-- 'Vulkan.Extensions.VK_NV_external_memory_capabilities.EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV'?
--
-- __RESOLVED__: Yes, unless it is passed back in to another driver
-- instance to import the object. A successful get call transfers ownership
-- of the handle to the application, while an import transfers ownership to
-- the associated driver. Destroying the memory object will not destroy the
-- handle or the handle’s reference to the underlying memory resource.
--
-- == Examples
--
-- >     //
-- >     // Create an exportable memory object and export an external
-- >     // handle from it.
-- >     //
-- >
-- >     // Pick an external format and handle type.
-- >     static const VkFormat format = VK_FORMAT_R8G8B8A8_UNORM;
-- >     static const VkExternalMemoryHandleTypeFlagsNV handleType =
-- >         VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV;
-- >
-- >     extern VkPhysicalDevice physicalDevice;
-- >     extern VkDevice device;
-- >
-- >     VkPhysicalDeviceMemoryProperties memoryProperties;
-- >     VkExternalImageFormatPropertiesNV properties;
-- >     VkExternalMemoryImageCreateInfoNV externalMemoryImageCreateInfo;
-- >     VkDedicatedAllocationImageCreateInfoNV dedicatedImageCreateInfo;
-- >     VkImageCreateInfo imageCreateInfo;
-- >     VkImage image;
-- >     VkMemoryRequirements imageMemoryRequirements;
-- >     uint32_t numMemoryTypes;
-- >     uint32_t memoryType;
-- >     VkExportMemoryAllocateInfoNV exportMemoryAllocateInfo;
-- >     VkDedicatedAllocationMemoryAllocateInfoNV dedicatedAllocationInfo;
-- >     VkMemoryAllocateInfo memoryAllocateInfo;
-- >     VkDeviceMemory memory;
-- >     VkResult result;
-- >     HANDLE memoryHnd;
-- >
-- >     // Figure out how many memory types the device supports
-- >     vkGetPhysicalDeviceMemoryProperties(physicalDevice,
-- >                                         &memoryProperties);
-- >     numMemoryTypes = memoryProperties.memoryTypeCount;
-- >
-- >     // Check the external handle type capabilities for the chosen format
-- >     // Exportable 2D image support with at least 1 mip level, 1 array
-- >     // layer, and VK_SAMPLE_COUNT_1_BIT using optimal tiling and supporting
-- >     // texturing and color rendering is required.
-- >     result = vkGetPhysicalDeviceExternalImageFormatPropertiesNV(
-- >         physicalDevice,
-- >         format,
-- >         VK_IMAGE_TYPE_2D,
-- >         VK_IMAGE_TILING_OPTIMAL,
-- >         VK_IMAGE_USAGE_SAMPLED_BIT |
-- >         VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT,
-- >         0,
-- >         handleType,
-- >         &properties);
-- >
-- >     if ((result != VK_SUCCESS) ||
-- >         !(properties.externalMemoryFeatures &
-- >           VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV)) {
-- >         abort();
-- >     }
-- >
-- >     // Set up the external memory image creation info
-- >     memset(&externalMemoryImageCreateInfo,
-- >            0, sizeof(externalMemoryImageCreateInfo));
-- >     externalMemoryImageCreateInfo.sType =
-- >         VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV;
-- >     externalMemoryImageCreateInfo.handleTypes = handleType;
-- >     if (properties.externalMemoryFeatures &
-- >         VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV) {
-- >         memset(&dedicatedImageCreateInfo, 0, sizeof(dedicatedImageCreateInfo));
-- >         dedicatedImageCreateInfo.sType =
-- >             VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV;
-- >         dedicatedImageCreateInfo.dedicatedAllocation = VK_TRUE;
-- >         externalMemoryImageCreateInfo.pNext = &dedicatedImageCreateInfo;
-- >     }
-- >     // Set up the  core image creation info
-- >     memset(&imageCreateInfo, 0, sizeof(imageCreateInfo));
-- >     imageCreateInfo.sType = VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
-- >     imageCreateInfo.pNext = &externalMemoryImageCreateInfo;
-- >     imageCreateInfo.format = format;
-- >     imageCreateInfo.extent.width = 64;
-- >     imageCreateInfo.extent.height = 64;
-- >     imageCreateInfo.extent.depth = 1;
-- >     imageCreateInfo.mipLevels = 1;
-- >     imageCreateInfo.arrayLayers = 1;
-- >     imageCreateInfo.samples = VK_SAMPLE_COUNT_1_BIT;
-- >     imageCreateInfo.tiling = VK_IMAGE_TILING_OPTIMAL;
-- >     imageCreateInfo.usage = VK_IMAGE_USAGE_SAMPLED_BIT |
-- >         VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT;
-- >     imageCreateInfo.sharingMode = VK_SHARING_MODE_EXCLUSIVE;
-- >     imageCreateInfo.initialLayout = VK_IMAGE_LAYOUT_UNDEFINED;
-- >
-- >     vkCreateImage(device, &imageCreateInfo, NULL, &image);
-- >
-- >     vkGetImageMemoryRequirements(device,
-- >                                  image,
-- >                                  &imageMemoryRequirements);
-- >
-- >     // For simplicity, just pick the first compatible memory type.
-- >     for (memoryType = 0; memoryType < numMemoryTypes; memoryType++) {
-- >         if ((1 << memoryType) & imageMemoryRequirements.memoryTypeBits) {
-- >             break;
-- >         }
-- >     }
-- >
-- >     // At least one memory type must be supported given the prior external
-- >     // handle capability check.
-- >     assert(memoryType < numMemoryTypes);
-- >
-- >     // Allocate the external memory object.
-- >     memset(&exportMemoryAllocateInfo, 0, sizeof(exportMemoryAllocateInfo));
-- >     exportMemoryAllocateInfo.sType =
-- >         VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV;
-- >     exportMemoryAllocateInfo.handleTypes = handleType;
-- >     if (properties.externalMemoryFeatures &
-- >         VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV) {
-- >         memset(&dedicatedAllocationInfo, 0, sizeof(dedicatedAllocationInfo));
-- >         dedicatedAllocationInfo.sType =
-- >             VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV;
-- >         dedicatedAllocationInfo.image = image;
-- >         exportMemoryAllocateInfo.pNext = &dedicatedAllocationInfo;
-- >     }
-- >     memset(&memoryAllocateInfo, 0, sizeof(memoryAllocateInfo));
-- >     memoryAllocateInfo.sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
-- >     memoryAllocateInfo.pNext = &exportMemoryAllocateInfo;
-- >     memoryAllocateInfo.allocationSize = imageMemoryRequirements.size;
-- >     memoryAllocateInfo.memoryTypeIndex = memoryType;
-- >
-- >     vkAllocateMemory(device, &memoryAllocateInfo, NULL, &memory);
-- >
-- >     if (!(properties.externalMemoryFeatures &
-- >           VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV)) {
-- >         vkBindImageMemory(device, image, memory, 0);
-- >     }
-- >
-- >     // Get the external memory opaque FD handle
-- >     vkGetMemoryWin32HandleNV(device, memory, &memoryHnd);
--
-- == Version History
--
-- -   Revision 1, 2016-08-11 (James Jones)
--
--     -   Initial draft
--
-- = See Also
--
-- 'ExportMemoryWin32HandleInfoNV', 'ImportMemoryWin32HandleInfoNV',
-- 'getMemoryWin32HandleNV'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_external_memory_win32 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_external_memory_win32  ( getMemoryWin32HandleNV
                                                      , ImportMemoryWin32HandleInfoNV(..)
                                                      , ExportMemoryWin32HandleInfoNV(..)
                                                      , NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION
                                                      , pattern NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION
                                                      , NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
                                                      , pattern NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
                                                      , HANDLE
                                                      , DWORD
                                                      , SECURITY_ATTRIBUTES
                                                      , ExternalMemoryHandleTypeFlagBitsNV(..)
                                                      , ExternalMemoryHandleTypeFlagsNV
                                                      ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
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
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkGetMemoryWin32HandleNV))
import Vulkan.Core10.Handles (DeviceMemory)
import Vulkan.Core10.Handles (DeviceMemory(..))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Extensions.VK_NV_external_memory_capabilities (ExternalMemoryHandleTypeFlagBitsNV(..))
import Vulkan.Extensions.VK_NV_external_memory_capabilities (ExternalMemoryHandleTypeFlagsNV)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_NV_external_memory_capabilities (ExternalMemoryHandleTypeFlagBitsNV(..))
import Vulkan.Extensions.VK_NV_external_memory_capabilities (ExternalMemoryHandleTypeFlagsNV)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetMemoryWin32HandleNV
  :: FunPtr (Ptr Device_T -> DeviceMemory -> ExternalMemoryHandleTypeFlagsNV -> Ptr HANDLE -> IO Result) -> Ptr Device_T -> DeviceMemory -> ExternalMemoryHandleTypeFlagsNV -> Ptr HANDLE -> IO Result

-- | vkGetMemoryWin32HandleNV - retrieve Win32 handle to a device memory
-- object
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_TOO_MANY_OBJECTS'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.DeviceMemory',
-- 'Vulkan.Extensions.VK_NV_external_memory_capabilities.ExternalMemoryHandleTypeFlagsNV'
getMemoryWin32HandleNV :: forall io
                        . (MonadIO io)
                       => -- | @device@ is the logical device that owns the memory.
                          --
                          -- #VUID-vkGetMemoryWin32HandleNV-device-parameter# @device@ /must/ be a
                          -- valid 'Vulkan.Core10.Handles.Device' handle
                          Device
                       -> -- | @memory@ is the 'Vulkan.Core10.Handles.DeviceMemory' object.
                          --
                          -- #VUID-vkGetMemoryWin32HandleNV-memory-parameter# @memory@ /must/ be a
                          -- valid 'Vulkan.Core10.Handles.DeviceMemory' handle
                          --
                          -- #VUID-vkGetMemoryWin32HandleNV-memory-parent# @memory@ /must/ have been
                          -- created, allocated, or retrieved from @device@
                          DeviceMemory
                       -> -- | @handleType@ is a bitmask of
                          -- 'Vulkan.Extensions.VK_NV_external_memory_capabilities.ExternalMemoryHandleTypeFlagBitsNV'
                          -- containing a single bit specifying the type of handle requested.
                          --
                          -- #VUID-vkGetMemoryWin32HandleNV-handleType-01326# @handleType@ /must/ be
                          -- a flag specified in
                          -- 'Vulkan.Extensions.VK_NV_external_memory.ExportMemoryAllocateInfoNV'::@handleTypes@
                          -- when allocating @memory@
                          --
                          -- #VUID-vkGetMemoryWin32HandleNV-handleType-parameter# @handleType@ /must/
                          -- be a valid combination of
                          -- 'Vulkan.Extensions.VK_NV_external_memory_capabilities.ExternalMemoryHandleTypeFlagBitsNV'
                          -- values
                          --
                          -- #VUID-vkGetMemoryWin32HandleNV-handleType-requiredbitmask# @handleType@
                          -- /must/ not be @0@
                          ExternalMemoryHandleTypeFlagsNV
                       -> io (HANDLE)
getMemoryWin32HandleNV device memory handleType = liftIO . evalContT $ do
  let vkGetMemoryWin32HandleNVPtr = pVkGetMemoryWin32HandleNV (deviceCmds (device :: Device))
  lift $ unless (vkGetMemoryWin32HandleNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetMemoryWin32HandleNV is null" Nothing Nothing
  let vkGetMemoryWin32HandleNV' = mkVkGetMemoryWin32HandleNV vkGetMemoryWin32HandleNVPtr
  pPHandle <- ContT $ bracket (callocBytes @HANDLE 8) free
  r <- lift $ traceAroundEvent "vkGetMemoryWin32HandleNV" (vkGetMemoryWin32HandleNV' (deviceHandle (device)) (memory) (handleType) (pPHandle))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pHandle <- lift $ peek @HANDLE pPHandle
  pure $ (pHandle)


-- | VkImportMemoryWin32HandleInfoNV - import Win32 memory created on the
-- same physical device
--
-- = Description
--
-- If @handleType@ is @0@, this structure is ignored by consumers of the
-- 'Vulkan.Core10.Memory.MemoryAllocateInfo' structure it is chained from.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_NV_external_memory_capabilities.ExternalMemoryHandleTypeFlagsNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ImportMemoryWin32HandleInfoNV = ImportMemoryWin32HandleInfoNV
  { -- | @handleType@ is @0@ or a
    -- 'Vulkan.Extensions.VK_NV_external_memory_capabilities.ExternalMemoryHandleTypeFlagBitsNV'
    -- value specifying the type of memory handle in @handle@.
    --
    -- #VUID-VkImportMemoryWin32HandleInfoNV-handleType-01327# @handleType@
    -- /must/ not have more than one bit set
    --
    -- #VUID-VkImportMemoryWin32HandleInfoNV-handleType-parameter# @handleType@
    -- /must/ be a valid combination of
    -- 'Vulkan.Extensions.VK_NV_external_memory_capabilities.ExternalMemoryHandleTypeFlagBitsNV'
    -- values
    handleType :: ExternalMemoryHandleTypeFlagsNV
  , -- | @handle@ is a Windows 'HANDLE' referring to the memory.
    --
    -- #VUID-VkImportMemoryWin32HandleInfoNV-handle-01328# @handle@ /must/ be a
    -- valid handle to memory, obtained as specified by @handleType@
    handle :: HANDLE
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImportMemoryWin32HandleInfoNV)
#endif
deriving instance Show ImportMemoryWin32HandleInfoNV

instance ToCStruct ImportMemoryWin32HandleInfoNV where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImportMemoryWin32HandleInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlagsNV)) (handleType)
    poke ((p `plusPtr` 24 :: Ptr HANDLE)) (handle)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct ImportMemoryWin32HandleInfoNV where
  peekCStruct p = do
    handleType <- peek @ExternalMemoryHandleTypeFlagsNV ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlagsNV))
    handle <- peek @HANDLE ((p `plusPtr` 24 :: Ptr HANDLE))
    pure $ ImportMemoryWin32HandleInfoNV
             handleType handle

instance Storable ImportMemoryWin32HandleInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImportMemoryWin32HandleInfoNV where
  zero = ImportMemoryWin32HandleInfoNV
           zero
           zero


-- | VkExportMemoryWin32HandleInfoNV - specify security attributes and access
-- rights for Win32 memory handles
--
-- = Description
--
-- If this structure is not present, or if @pAttributes@ is set to @NULL@,
-- default security descriptor values will be used, and child processes
-- created by the application will not inherit the handle, as described in
-- the MSDN documentation for “Synchronization Object Security and Access
-- Rights”1. Further, if the structure is not present, the access rights
-- will be
--
-- @DXGI_SHARED_RESOURCE_READ@ | @DXGI_SHARED_RESOURCE_WRITE@
--
-- [1]
--     <https://docs.microsoft.com/en-us/windows/win32/sync/synchronization-object-security-and-access-rights>
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkExportMemoryWin32HandleInfoNV-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV'
--
-- -   #VUID-VkExportMemoryWin32HandleInfoNV-pAttributes-parameter# If
--     @pAttributes@ is not @NULL@, @pAttributes@ /must/ be a valid pointer
--     to a valid 'SECURITY_ATTRIBUTES' value
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ExportMemoryWin32HandleInfoNV = ExportMemoryWin32HandleInfoNV
  { -- | @pAttributes@ is a pointer to a Windows 'SECURITY_ATTRIBUTES' structure
    -- specifying security attributes of the handle.
    attributes :: Ptr SECURITY_ATTRIBUTES
  , -- | @dwAccess@ is a 'DWORD' specifying access rights of the handle.
    dwAccess :: DWORD
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExportMemoryWin32HandleInfoNV)
#endif
deriving instance Show ExportMemoryWin32HandleInfoNV

instance ToCStruct ExportMemoryWin32HandleInfoNV where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExportMemoryWin32HandleInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr SECURITY_ATTRIBUTES))) (attributes)
    poke ((p `plusPtr` 24 :: Ptr DWORD)) (dwAccess)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct ExportMemoryWin32HandleInfoNV where
  peekCStruct p = do
    pAttributes <- peek @(Ptr SECURITY_ATTRIBUTES) ((p `plusPtr` 16 :: Ptr (Ptr SECURITY_ATTRIBUTES)))
    dwAccess <- peek @DWORD ((p `plusPtr` 24 :: Ptr DWORD))
    pure $ ExportMemoryWin32HandleInfoNV
             pAttributes dwAccess

instance Storable ExportMemoryWin32HandleInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExportMemoryWin32HandleInfoNV where
  zero = ExportMemoryWin32HandleInfoNV
           zero
           zero


type NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION"
pattern NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION :: forall a . Integral a => a
pattern NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION = 1


type NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME = "VK_NV_external_memory_win32"

-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME"
pattern NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME = "VK_NV_external_memory_win32"


type HANDLE = Ptr ()


type DWORD = Word32


data SECURITY_ATTRIBUTES

