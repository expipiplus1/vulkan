{-# language CPP #-}
-- No documentation found for Chapter "VK_NV_win32_keyed_mutex"
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
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV))

-- No documentation found for TopLevel "VkWin32KeyedMutexAcquireReleaseInfoNV"
data Win32KeyedMutexAcquireReleaseInfoNV = Win32KeyedMutexAcquireReleaseInfoNV
  { -- No documentation found for Nested "VkWin32KeyedMutexAcquireReleaseInfoNV" "pAcquireSyncs"
    acquireSyncs :: Vector DeviceMemory
  , -- No documentation found for Nested "VkWin32KeyedMutexAcquireReleaseInfoNV" "pAcquireKeys"
    acquireKeys :: Vector Word64
  , -- No documentation found for Nested "VkWin32KeyedMutexAcquireReleaseInfoNV" "pAcquireTimeoutMilliseconds"
    acquireTimeoutMilliseconds :: Vector Word32
  , -- No documentation found for Nested "VkWin32KeyedMutexAcquireReleaseInfoNV" "pReleaseSyncs"
    releaseSyncs :: Vector DeviceMemory
  , -- No documentation found for Nested "VkWin32KeyedMutexAcquireReleaseInfoNV" "pReleaseKeys"
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
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pPAcquireSyncs' <- ContT $ allocaBytesAligned @DeviceMemory ((Data.Vector.length (mempty)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPAcquireSyncs' `plusPtr` (8 * (i)) :: Ptr DeviceMemory) (e)) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr DeviceMemory))) (pPAcquireSyncs')
    pPAcquireKeys' <- ContT $ allocaBytesAligned @Word64 ((Data.Vector.length (mempty)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPAcquireKeys' `plusPtr` (8 * (i)) :: Ptr Word64) (e)) (mempty)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr Word64))) (pPAcquireKeys')
    pPAcquireTimeoutMilliseconds' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPAcquireTimeoutMilliseconds' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (mempty)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr Word32))) (pPAcquireTimeoutMilliseconds')
    pPReleaseSyncs' <- ContT $ allocaBytesAligned @DeviceMemory ((Data.Vector.length (mempty)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPReleaseSyncs' `plusPtr` (8 * (i)) :: Ptr DeviceMemory) (e)) (mempty)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr DeviceMemory))) (pPReleaseSyncs')
    pPReleaseKeys' <- ContT $ allocaBytesAligned @Word64 ((Data.Vector.length (mempty)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPReleaseKeys' `plusPtr` (8 * (i)) :: Ptr Word64) (e)) (mempty)
    lift $ poke ((p `plusPtr` 64 :: Ptr (Ptr Word64))) (pPReleaseKeys')
    lift $ f

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

