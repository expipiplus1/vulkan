{-# language CPP #-}
-- | = Name
--
-- VK_NV_win32_keyed_mutex - device extension
--
-- == VK_NV_win32_keyed_mutex
--
-- [__Name String__]
--     @VK_NV_win32_keyed_mutex@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     59
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_NV_external_memory_win32@
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to @VK_KHR_win32_keyed_mutex@ extension
--
-- [__Contact__]
--
--     -   Carsten Rohde
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_NV_win32_keyed_mutex:%20&body=@crohde%20 >
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
-- Applications that wish to import Direct3D 11 memory objects into the
-- Vulkan API may wish to use the native keyed mutex mechanism to
-- synchronize access to the memory between Vulkan and Direct3D. This
-- extension provides a way for an application to access the keyed mutex
-- associated with an imported Vulkan memory object when submitting command
-- buffers to a queue.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.Queue.SubmitInfo':
--
--     -   'Win32KeyedMutexAcquireReleaseInfoNV'
--
-- == New Enum Constants
--
-- -   'NV_WIN32_KEYED_MUTEX_EXTENSION_NAME'
--
-- -   'NV_WIN32_KEYED_MUTEX_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV'
--
-- == Examples
--
-- >     //
-- >     // Import a memory object from Direct3D 11, and synchronize
-- >     // access to it in Vulkan using keyed mutex objects.
-- >     //
-- >
-- >     extern VkPhysicalDevice physicalDevice;
-- >     extern VkDevice device;
-- >     extern HANDLE sharedNtHandle;
-- >
-- >     static const VkFormat format = VK_FORMAT_R8G8B8A8_UNORM;
-- >     static const VkExternalMemoryHandleTypeFlagsNV handleType =
-- >         VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV;
-- >
-- >     VkPhysicalDeviceMemoryProperties memoryProperties;
-- >     VkExternalImageFormatPropertiesNV properties;
-- >     VkExternalMemoryImageCreateInfoNV externalMemoryImageCreateInfo;
-- >     VkImageCreateInfo imageCreateInfo;
-- >     VkImage image;
-- >     VkMemoryRequirements imageMemoryRequirements;
-- >     uint32_t numMemoryTypes;
-- >     uint32_t memoryType;
-- >     VkImportMemoryWin32HandleInfoNV importMemoryInfo;
-- >     VkMemoryAllocateInfo memoryAllocateInfo;
-- >     VkDeviceMemory mem;
-- >     VkResult result;
-- >
-- >     // Figure out how many memory types the device supports
-- >     vkGetPhysicalDeviceMemoryProperties(physicalDevice,
-- >                                         &memoryProperties);
-- >     numMemoryTypes = memoryProperties.memoryTypeCount;
-- >
-- >     // Check the external handle type capabilities for the chosen format
-- >     // Importable 2D image support with at least 1 mip level, 1 array
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
-- >           VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV)) {
-- >         abort();
-- >     }
-- >
-- >     // Set up the external memory image creation info
-- >     memset(&externalMemoryImageCreateInfo,
-- >            0, sizeof(externalMemoryImageCreateInfo));
-- >     externalMemoryImageCreateInfo.sType =
-- >         VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV;
-- >     externalMemoryImageCreateInfo.handleTypes = handleType;
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
-- >     importMemoryInfo.handleTypes = handleType;
-- >     importMemoryInfo.handle = sharedNtHandle;
-- >
-- >     memset(&memoryAllocateInfo, 0, sizeof(memoryAllocateInfo));
-- >     memoryAllocateInfo.sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
-- >     memoryAllocateInfo.pNext = &exportMemoryAllocateInfo;
-- >     memoryAllocateInfo.allocationSize = imageMemoryRequirements.size;
-- >     memoryAllocateInfo.memoryTypeIndex = memoryType;
-- >
-- >     vkAllocateMemory(device, &memoryAllocateInfo, NULL, &mem);
-- >
-- >     vkBindImageMemory(device, image, mem, 0);
-- >
-- >     ...
-- >
-- >     const uint64_t acquireKey = 1;
-- >     const uint32_t timeout = INFINITE;
-- >     const uint64_t releaseKey = 2;
-- >
-- >     VkWin32KeyedMutexAcquireReleaseInfoNV keyedMutex =
-- >         { VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV };
-- >     keyedMutex.acquireCount = 1;
-- >     keyedMutex.pAcquireSyncs = &mem;
-- >     keyedMutex.pAcquireKeys = &acquireKey;
-- >     keyedMutex.pAcquireTimeoutMilliseconds = &timeout;
-- >     keyedMutex.releaseCount = 1;
-- >     keyedMutex.pReleaseSyncs = &mem;
-- >     keyedMutex.pReleaseKeys = &releaseKey;
-- >
-- >     VkSubmitInfo submit_info = { VK_STRUCTURE_TYPE_SUBMIT_INFO, &keyedMutex };
-- >     submit_info.commandBufferCount = 1;
-- >     submit_info.pCommandBuffers = &cmd_buf;
-- >     vkQueueSubmit(queue, 1, &submit_info, VK_NULL_HANDLE);
--
-- == Version History
--
-- -   Revision 2, 2016-08-11 (James Jones)
--
--     -   Updated sample code based on the NV external memory extensions.
--
--     -   Renamed from NVX to NV extension.
--
--     -   Added Overview and Description sections.
--
--     -   Updated sample code to use the NV external memory extensions.
--
-- -   Revision 1, 2016-06-14 (Carsten Rohde)
--
--     -   Initial draft.
--
-- = See Also
--
-- 'Win32KeyedMutexAcquireReleaseInfoNV'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_win32_keyed_mutex Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_win32_keyed_mutex  ( Win32KeyedMutexAcquireReleaseInfoNV(..)
                                                  , NV_WIN32_KEYED_MUTEX_SPEC_VERSION
                                                  , pattern NV_WIN32_KEYED_MUTEX_SPEC_VERSION
                                                  , NV_WIN32_KEYED_MUTEX_EXTENSION_NAME
                                                  , pattern NV_WIN32_KEYED_MUTEX_EXTENSION_NAME
                                                  ) where

import Control.Monad (unless)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.IO (throwIO)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
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
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.Handles (DeviceMemory)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV))
-- | VkWin32KeyedMutexAcquireReleaseInfoNV - use Windows keyex mutex
-- mechanism to synchronize work
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkWin32KeyedMutexAcquireReleaseInfoNV-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV'
--
-- -   #VUID-VkWin32KeyedMutexAcquireReleaseInfoNV-pAcquireSyncs-parameter#
--     If @acquireCount@ is not @0@, @pAcquireSyncs@ /must/ be a valid
--     pointer to an array of @acquireCount@ valid
--     'Vulkan.Core10.Handles.DeviceMemory' handles
--
-- -   #VUID-VkWin32KeyedMutexAcquireReleaseInfoNV-pAcquireKeys-parameter#
--     If @acquireCount@ is not @0@, @pAcquireKeys@ /must/ be a valid
--     pointer to an array of @acquireCount@ @uint64_t@ values
--
-- -   #VUID-VkWin32KeyedMutexAcquireReleaseInfoNV-pAcquireTimeoutMilliseconds-parameter#
--     If @acquireCount@ is not @0@, @pAcquireTimeoutMilliseconds@ /must/
--     be a valid pointer to an array of @acquireCount@ @uint32_t@ values
--
-- -   #VUID-VkWin32KeyedMutexAcquireReleaseInfoNV-pReleaseSyncs-parameter#
--     If @releaseCount@ is not @0@, @pReleaseSyncs@ /must/ be a valid
--     pointer to an array of @releaseCount@ valid
--     'Vulkan.Core10.Handles.DeviceMemory' handles
--
-- -   #VUID-VkWin32KeyedMutexAcquireReleaseInfoNV-pReleaseKeys-parameter#
--     If @releaseCount@ is not @0@, @pReleaseKeys@ /must/ be a valid
--     pointer to an array of @releaseCount@ @uint64_t@ values
--
-- -   #VUID-VkWin32KeyedMutexAcquireReleaseInfoNV-commonparent# Both of
--     the elements of @pAcquireSyncs@, and the elements of @pReleaseSyncs@
--     that are valid handles of non-ignored parameters /must/ have been
--     created, allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.DeviceMemory',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data Win32KeyedMutexAcquireReleaseInfoNV = Win32KeyedMutexAcquireReleaseInfoNV
  { -- | @pAcquireSyncs@ is a pointer to an array of
    -- 'Vulkan.Core10.Handles.DeviceMemory' objects which were imported from
    -- Direct3D 11 resources.
    acquireSyncs :: Vector DeviceMemory
  , -- | @pAcquireKeys@ is a pointer to an array of mutex key values to wait for
    -- prior to beginning the submitted work. Entries refer to the keyed mutex
    -- associated with the corresponding entries in @pAcquireSyncs@.
    acquireKeys :: Vector Word64
  , -- | @pAcquireTimeoutMilliseconds@ is a pointer to an array of timeout
    -- values, in millisecond units, for each acquire specified in
    -- @pAcquireKeys@.
    acquireTimeoutMilliseconds :: Vector Word32
  , -- | @pReleaseSyncs@ is a pointer to an array of
    -- 'Vulkan.Core10.Handles.DeviceMemory' objects which were imported from
    -- Direct3D 11 resources.
    releaseSyncs :: Vector DeviceMemory
  , -- | @pReleaseKeys@ is a pointer to an array of mutex key values to set when
    -- the submitted work has completed. Entries refer to the keyed mutex
    -- associated with the corresponding entries in @pReleaseSyncs@.
    releaseKeys :: Vector Word64
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (Win32KeyedMutexAcquireReleaseInfoNV)
#endif
deriving instance Show Win32KeyedMutexAcquireReleaseInfoNV

instance ToCStruct Win32KeyedMutexAcquireReleaseInfoNV where
  withCStruct x f = allocaBytesAligned 72 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p Win32KeyedMutexAcquireReleaseInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    let pAcquireSyncsLength = Data.Vector.length $ (acquireSyncs)
    lift $ unless ((Data.Vector.length $ (acquireKeys)) == pAcquireSyncsLength) $
      throwIO $ IOError Nothing InvalidArgument "" "pAcquireKeys and pAcquireSyncs must have the same length" Nothing Nothing
    lift $ unless ((Data.Vector.length $ (acquireTimeoutMilliseconds)) == pAcquireSyncsLength) $
      throwIO $ IOError Nothing InvalidArgument "" "pAcquireTimeoutMilliseconds and pAcquireSyncs must have the same length" Nothing Nothing
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral pAcquireSyncsLength :: Word32))
    pPAcquireSyncs' <- ContT $ allocaBytesAligned @DeviceMemory ((Data.Vector.length (acquireSyncs)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPAcquireSyncs' `plusPtr` (8 * (i)) :: Ptr DeviceMemory) (e)) (acquireSyncs)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr DeviceMemory))) (pPAcquireSyncs')
    pPAcquireKeys' <- ContT $ allocaBytesAligned @Word64 ((Data.Vector.length (acquireKeys)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPAcquireKeys' `plusPtr` (8 * (i)) :: Ptr Word64) (e)) (acquireKeys)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr Word64))) (pPAcquireKeys')
    pPAcquireTimeoutMilliseconds' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (acquireTimeoutMilliseconds)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPAcquireTimeoutMilliseconds' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (acquireTimeoutMilliseconds)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr Word32))) (pPAcquireTimeoutMilliseconds')
    let pReleaseSyncsLength = Data.Vector.length $ (releaseSyncs)
    lift $ unless ((Data.Vector.length $ (releaseKeys)) == pReleaseSyncsLength) $
      throwIO $ IOError Nothing InvalidArgument "" "pReleaseKeys and pReleaseSyncs must have the same length" Nothing Nothing
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) ((fromIntegral pReleaseSyncsLength :: Word32))
    pPReleaseSyncs' <- ContT $ allocaBytesAligned @DeviceMemory ((Data.Vector.length (releaseSyncs)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPReleaseSyncs' `plusPtr` (8 * (i)) :: Ptr DeviceMemory) (e)) (releaseSyncs)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr DeviceMemory))) (pPReleaseSyncs')
    pPReleaseKeys' <- ContT $ allocaBytesAligned @Word64 ((Data.Vector.length (releaseKeys)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPReleaseKeys' `plusPtr` (8 * (i)) :: Ptr Word64) (e)) (releaseKeys)
    lift $ poke ((p `plusPtr` 64 :: Ptr (Ptr Word64))) (pPReleaseKeys')
    lift $ f
  cStructSize = 72
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct Win32KeyedMutexAcquireReleaseInfoNV where
  peekCStruct p = do
    acquireCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pAcquireSyncs <- peek @(Ptr DeviceMemory) ((p `plusPtr` 24 :: Ptr (Ptr DeviceMemory)))
    pAcquireSyncs' <- generateM (fromIntegral acquireCount) (\i -> peek @DeviceMemory ((pAcquireSyncs `advancePtrBytes` (8 * (i)) :: Ptr DeviceMemory)))
    pAcquireKeys <- peek @(Ptr Word64) ((p `plusPtr` 32 :: Ptr (Ptr Word64)))
    pAcquireKeys' <- generateM (fromIntegral acquireCount) (\i -> peek @Word64 ((pAcquireKeys `advancePtrBytes` (8 * (i)) :: Ptr Word64)))
    pAcquireTimeoutMilliseconds <- peek @(Ptr Word32) ((p `plusPtr` 40 :: Ptr (Ptr Word32)))
    pAcquireTimeoutMilliseconds' <- generateM (fromIntegral acquireCount) (\i -> peek @Word32 ((pAcquireTimeoutMilliseconds `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    releaseCount <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    pReleaseSyncs <- peek @(Ptr DeviceMemory) ((p `plusPtr` 56 :: Ptr (Ptr DeviceMemory)))
    pReleaseSyncs' <- generateM (fromIntegral releaseCount) (\i -> peek @DeviceMemory ((pReleaseSyncs `advancePtrBytes` (8 * (i)) :: Ptr DeviceMemory)))
    pReleaseKeys <- peek @(Ptr Word64) ((p `plusPtr` 64 :: Ptr (Ptr Word64)))
    pReleaseKeys' <- generateM (fromIntegral releaseCount) (\i -> peek @Word64 ((pReleaseKeys `advancePtrBytes` (8 * (i)) :: Ptr Word64)))
    pure $ Win32KeyedMutexAcquireReleaseInfoNV
             pAcquireSyncs' pAcquireKeys' pAcquireTimeoutMilliseconds' pReleaseSyncs' pReleaseKeys'

instance Zero Win32KeyedMutexAcquireReleaseInfoNV where
  zero = Win32KeyedMutexAcquireReleaseInfoNV
           mempty
           mempty
           mempty
           mempty
           mempty


type NV_WIN32_KEYED_MUTEX_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_NV_WIN32_KEYED_MUTEX_SPEC_VERSION"
pattern NV_WIN32_KEYED_MUTEX_SPEC_VERSION :: forall a . Integral a => a
pattern NV_WIN32_KEYED_MUTEX_SPEC_VERSION = 2


type NV_WIN32_KEYED_MUTEX_EXTENSION_NAME = "VK_NV_win32_keyed_mutex"

-- No documentation found for TopLevel "VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME"
pattern NV_WIN32_KEYED_MUTEX_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_WIN32_KEYED_MUTEX_EXTENSION_NAME = "VK_NV_win32_keyed_mutex"

