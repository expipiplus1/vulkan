{-# language CPP #-}
-- | = Name
--
-- VK_KHR_win32_keyed_mutex - device extension
--
-- == VK_KHR_win32_keyed_mutex
--
-- [__Name String__]
--     @VK_KHR_win32_keyed_mutex@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     76
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_external_memory_win32@
--
-- [__Contact__]
--
--     -   Carsten Rohde
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_KHR_win32_keyed_mutex:%20&body=@crohde%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2016-10-21
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   James Jones, NVIDIA
--
--     -   Jeff Juliano, NVIDIA
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
--     -   'Win32KeyedMutexAcquireReleaseInfoKHR'
--
-- == New Enum Constants
--
-- -   'KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME'
--
-- -   'KHR_WIN32_KEYED_MUTEX_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR'
--
-- == Version History
--
-- -   Revision 1, 2016-10-21 (James Jones)
--
--     -   Initial revision
--
-- = See Also
--
-- 'Win32KeyedMutexAcquireReleaseInfoKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_win32_keyed_mutex Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_win32_keyed_mutex  ( Win32KeyedMutexAcquireReleaseInfoKHR(..)
                                                   , KHR_WIN32_KEYED_MUTEX_SPEC_VERSION
                                                   , pattern KHR_WIN32_KEYED_MUTEX_SPEC_VERSION
                                                   , KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME
                                                   , pattern KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR))
-- | VkWin32KeyedMutexAcquireReleaseInfoKHR - Use the Windows keyed mutex
-- mechanism to synchronize work
--
-- == Valid Usage
--
-- -   #VUID-VkWin32KeyedMutexAcquireReleaseInfoKHR-pAcquireSyncs-00081#
--     Each member of @pAcquireSyncs@ and @pReleaseSyncs@ /must/ be a
--     device memory object imported by setting
--     'Vulkan.Extensions.VK_KHR_external_memory_win32.ImportMemoryWin32HandleInfoKHR'::@handleType@
--     to
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT'
--     or
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkWin32KeyedMutexAcquireReleaseInfoKHR-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR'
--
-- -   #VUID-VkWin32KeyedMutexAcquireReleaseInfoKHR-pAcquireSyncs-parameter#
--     If @acquireCount@ is not @0@, @pAcquireSyncs@ /must/ be a valid
--     pointer to an array of @acquireCount@ valid
--     'Vulkan.Core10.Handles.DeviceMemory' handles
--
-- -   #VUID-VkWin32KeyedMutexAcquireReleaseInfoKHR-pAcquireKeys-parameter#
--     If @acquireCount@ is not @0@, @pAcquireKeys@ /must/ be a valid
--     pointer to an array of @acquireCount@ @uint64_t@ values
--
-- -   #VUID-VkWin32KeyedMutexAcquireReleaseInfoKHR-pAcquireTimeouts-parameter#
--     If @acquireCount@ is not @0@, @pAcquireTimeouts@ /must/ be a valid
--     pointer to an array of @acquireCount@ @uint32_t@ values
--
-- -   #VUID-VkWin32KeyedMutexAcquireReleaseInfoKHR-pReleaseSyncs-parameter#
--     If @releaseCount@ is not @0@, @pReleaseSyncs@ /must/ be a valid
--     pointer to an array of @releaseCount@ valid
--     'Vulkan.Core10.Handles.DeviceMemory' handles
--
-- -   #VUID-VkWin32KeyedMutexAcquireReleaseInfoKHR-pReleaseKeys-parameter#
--     If @releaseCount@ is not @0@, @pReleaseKeys@ /must/ be a valid
--     pointer to an array of @releaseCount@ @uint64_t@ values
--
-- -   #VUID-VkWin32KeyedMutexAcquireReleaseInfoKHR-commonparent# Both of
--     the elements of @pAcquireSyncs@, and the elements of @pReleaseSyncs@
--     that are valid handles of non-ignored parameters /must/ have been
--     created, allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.DeviceMemory',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data Win32KeyedMutexAcquireReleaseInfoKHR = Win32KeyedMutexAcquireReleaseInfoKHR
  { -- | @pAcquireSyncs@ is a pointer to an array of
    -- 'Vulkan.Core10.Handles.DeviceMemory' objects which were imported from
    -- Direct3D 11 resources.
    acquireSyncs :: Vector DeviceMemory
  , -- | @pAcquireKeys@ is a pointer to an array of mutex key values to wait for
    -- prior to beginning the submitted work. Entries refer to the keyed mutex
    -- associated with the corresponding entries in @pAcquireSyncs@.
    acquireKeys :: Vector Word64
  , -- No documentation found for Nested "VkWin32KeyedMutexAcquireReleaseInfoKHR" "pAcquireTimeouts"
    acquireTimeouts :: Vector Word32
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
deriving instance Generic (Win32KeyedMutexAcquireReleaseInfoKHR)
#endif
deriving instance Show Win32KeyedMutexAcquireReleaseInfoKHR

instance ToCStruct Win32KeyedMutexAcquireReleaseInfoKHR where
  withCStruct x f = allocaBytesAligned 72 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p Win32KeyedMutexAcquireReleaseInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    let pAcquireSyncsLength = Data.Vector.length $ (acquireSyncs)
    lift $ unless ((Data.Vector.length $ (acquireKeys)) == pAcquireSyncsLength) $
      throwIO $ IOError Nothing InvalidArgument "" "pAcquireKeys and pAcquireSyncs must have the same length" Nothing Nothing
    lift $ unless ((Data.Vector.length $ (acquireTimeouts)) == pAcquireSyncsLength) $
      throwIO $ IOError Nothing InvalidArgument "" "pAcquireTimeouts and pAcquireSyncs must have the same length" Nothing Nothing
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral pAcquireSyncsLength :: Word32))
    pPAcquireSyncs' <- ContT $ allocaBytesAligned @DeviceMemory ((Data.Vector.length (acquireSyncs)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPAcquireSyncs' `plusPtr` (8 * (i)) :: Ptr DeviceMemory) (e)) (acquireSyncs)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr DeviceMemory))) (pPAcquireSyncs')
    pPAcquireKeys' <- ContT $ allocaBytesAligned @Word64 ((Data.Vector.length (acquireKeys)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPAcquireKeys' `plusPtr` (8 * (i)) :: Ptr Word64) (e)) (acquireKeys)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr Word64))) (pPAcquireKeys')
    pPAcquireTimeouts' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (acquireTimeouts)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPAcquireTimeouts' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (acquireTimeouts)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr Word32))) (pPAcquireTimeouts')
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
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct Win32KeyedMutexAcquireReleaseInfoKHR where
  peekCStruct p = do
    acquireCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pAcquireSyncs <- peek @(Ptr DeviceMemory) ((p `plusPtr` 24 :: Ptr (Ptr DeviceMemory)))
    pAcquireSyncs' <- generateM (fromIntegral acquireCount) (\i -> peek @DeviceMemory ((pAcquireSyncs `advancePtrBytes` (8 * (i)) :: Ptr DeviceMemory)))
    pAcquireKeys <- peek @(Ptr Word64) ((p `plusPtr` 32 :: Ptr (Ptr Word64)))
    pAcquireKeys' <- generateM (fromIntegral acquireCount) (\i -> peek @Word64 ((pAcquireKeys `advancePtrBytes` (8 * (i)) :: Ptr Word64)))
    pAcquireTimeouts <- peek @(Ptr Word32) ((p `plusPtr` 40 :: Ptr (Ptr Word32)))
    pAcquireTimeouts' <- generateM (fromIntegral acquireCount) (\i -> peek @Word32 ((pAcquireTimeouts `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    releaseCount <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    pReleaseSyncs <- peek @(Ptr DeviceMemory) ((p `plusPtr` 56 :: Ptr (Ptr DeviceMemory)))
    pReleaseSyncs' <- generateM (fromIntegral releaseCount) (\i -> peek @DeviceMemory ((pReleaseSyncs `advancePtrBytes` (8 * (i)) :: Ptr DeviceMemory)))
    pReleaseKeys <- peek @(Ptr Word64) ((p `plusPtr` 64 :: Ptr (Ptr Word64)))
    pReleaseKeys' <- generateM (fromIntegral releaseCount) (\i -> peek @Word64 ((pReleaseKeys `advancePtrBytes` (8 * (i)) :: Ptr Word64)))
    pure $ Win32KeyedMutexAcquireReleaseInfoKHR
             pAcquireSyncs' pAcquireKeys' pAcquireTimeouts' pReleaseSyncs' pReleaseKeys'

instance Zero Win32KeyedMutexAcquireReleaseInfoKHR where
  zero = Win32KeyedMutexAcquireReleaseInfoKHR
           mempty
           mempty
           mempty
           mempty
           mempty


type KHR_WIN32_KEYED_MUTEX_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_WIN32_KEYED_MUTEX_SPEC_VERSION"
pattern KHR_WIN32_KEYED_MUTEX_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_WIN32_KEYED_MUTEX_SPEC_VERSION = 1


type KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME = "VK_KHR_win32_keyed_mutex"

-- No documentation found for TopLevel "VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME"
pattern KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME = "VK_KHR_win32_keyed_mutex"

