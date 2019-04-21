{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NV_win32_keyed_mutex
  ( withCStructWin32KeyedMutexAcquireReleaseInfoNV
  , fromCStructWin32KeyedMutexAcquireReleaseInfoNV
  , Win32KeyedMutexAcquireReleaseInfoNV(..)
  , pattern VK_NV_WIN32_KEYED_MUTEX_SPEC_VERSION
  , pattern VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV
  ) where

import Data.Function
  ( (&)
  )
import Data.List
  ( minimum
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( empty
  , generateM
  , length
  )
import Data.Word
  ( Word32
  , Word64
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_win32_keyed_mutex
  ( VkWin32KeyedMutexAcquireReleaseInfoNV(..)
  , pattern VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV
  )
import Graphics.Vulkan.Core10.Memory
  ( DeviceMemory
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_NV_win32_keyed_mutex
  ( pattern VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME
  , pattern VK_NV_WIN32_KEYED_MUTEX_SPEC_VERSION
  )



-- | VkWin32KeyedMutexAcquireReleaseInfoNV - use Windows keyex mutex
-- mechanism to synchronize work
--
-- = Description
--
-- Unresolved directive in VkWin32KeyedMutexAcquireReleaseInfoNV.txt -
-- include::{generated}\/validity\/structs\/VkWin32KeyedMutexAcquireReleaseInfoNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data Win32KeyedMutexAcquireReleaseInfoNV = Win32KeyedMutexAcquireReleaseInfoNV
  { -- Univalued member elided
  -- No documentation found for Nested "Win32KeyedMutexAcquireReleaseInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "Win32KeyedMutexAcquireReleaseInfoNV" "pAcquireSyncs"
  acquireSyncs :: Vector DeviceMemory
  , -- No documentation found for Nested "Win32KeyedMutexAcquireReleaseInfoNV" "pAcquireKeys"
  acquireKeys :: Vector Word64
  , -- No documentation found for Nested "Win32KeyedMutexAcquireReleaseInfoNV" "pAcquireTimeoutMilliseconds"
  acquireTimeoutMilliseconds :: Vector Word32
  -- Length valued member elided
  , -- No documentation found for Nested "Win32KeyedMutexAcquireReleaseInfoNV" "pReleaseSyncs"
  releaseSyncs :: Vector DeviceMemory
  , -- No documentation found for Nested "Win32KeyedMutexAcquireReleaseInfoNV" "pReleaseKeys"
  releaseKeys :: Vector Word64
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkWin32KeyedMutexAcquireReleaseInfoNV' and
-- marshal a 'Win32KeyedMutexAcquireReleaseInfoNV' into it. The 'VkWin32KeyedMutexAcquireReleaseInfoNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructWin32KeyedMutexAcquireReleaseInfoNV :: Win32KeyedMutexAcquireReleaseInfoNV -> (VkWin32KeyedMutexAcquireReleaseInfoNV -> IO a) -> IO a
withCStructWin32KeyedMutexAcquireReleaseInfoNV marshalled cont = withVec (&) (releaseKeys (marshalled :: Win32KeyedMutexAcquireReleaseInfoNV)) (\pPReleaseKeys -> withVec (&) (releaseSyncs (marshalled :: Win32KeyedMutexAcquireReleaseInfoNV)) (\pPReleaseSyncs -> withVec (&) (acquireTimeoutMilliseconds (marshalled :: Win32KeyedMutexAcquireReleaseInfoNV)) (\pPAcquireTimeoutMilliseconds -> withVec (&) (acquireKeys (marshalled :: Win32KeyedMutexAcquireReleaseInfoNV)) (\pPAcquireKeys -> withVec (&) (acquireSyncs (marshalled :: Win32KeyedMutexAcquireReleaseInfoNV)) (\pPAcquireSyncs -> maybeWith withSomeVkStruct (next (marshalled :: Win32KeyedMutexAcquireReleaseInfoNV)) (\pPNext -> cont (VkWin32KeyedMutexAcquireReleaseInfoNV VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV pPNext (fromIntegral (minimum ([Data.Vector.length (acquireSyncs (marshalled :: Win32KeyedMutexAcquireReleaseInfoNV)), Data.Vector.length (acquireKeys (marshalled :: Win32KeyedMutexAcquireReleaseInfoNV)), Data.Vector.length (acquireTimeoutMilliseconds (marshalled :: Win32KeyedMutexAcquireReleaseInfoNV))]))) pPAcquireSyncs pPAcquireKeys pPAcquireTimeoutMilliseconds (fromIntegral (minimum ([Data.Vector.length (releaseSyncs (marshalled :: Win32KeyedMutexAcquireReleaseInfoNV)), Data.Vector.length (releaseKeys (marshalled :: Win32KeyedMutexAcquireReleaseInfoNV))]))) pPReleaseSyncs pPReleaseKeys)))))))

-- | A function to read a 'VkWin32KeyedMutexAcquireReleaseInfoNV' and all additional
-- structures in the pointer chain into a 'Win32KeyedMutexAcquireReleaseInfoNV'.
fromCStructWin32KeyedMutexAcquireReleaseInfoNV :: VkWin32KeyedMutexAcquireReleaseInfoNV -> IO Win32KeyedMutexAcquireReleaseInfoNV
fromCStructWin32KeyedMutexAcquireReleaseInfoNV c = Win32KeyedMutexAcquireReleaseInfoNV <$> -- Univalued Member elided
                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkWin32KeyedMutexAcquireReleaseInfoNV)))
                                                                                       -- Length valued member elided
                                                                                       <*> (Data.Vector.generateM (fromIntegral (vkAcquireCount (c :: VkWin32KeyedMutexAcquireReleaseInfoNV))) (peekElemOff (vkPAcquireSyncs (c :: VkWin32KeyedMutexAcquireReleaseInfoNV))))
                                                                                       <*> (Data.Vector.generateM (fromIntegral (vkAcquireCount (c :: VkWin32KeyedMutexAcquireReleaseInfoNV))) (peekElemOff (vkPAcquireKeys (c :: VkWin32KeyedMutexAcquireReleaseInfoNV))))
                                                                                       <*> (Data.Vector.generateM (fromIntegral (vkAcquireCount (c :: VkWin32KeyedMutexAcquireReleaseInfoNV))) (peekElemOff (vkPAcquireTimeoutMilliseconds (c :: VkWin32KeyedMutexAcquireReleaseInfoNV))))
                                                                                       -- Length valued member elided
                                                                                       <*> (Data.Vector.generateM (fromIntegral (vkReleaseCount (c :: VkWin32KeyedMutexAcquireReleaseInfoNV))) (peekElemOff (vkPReleaseSyncs (c :: VkWin32KeyedMutexAcquireReleaseInfoNV))))
                                                                                       <*> (Data.Vector.generateM (fromIntegral (vkReleaseCount (c :: VkWin32KeyedMutexAcquireReleaseInfoNV))) (peekElemOff (vkPReleaseKeys (c :: VkWin32KeyedMutexAcquireReleaseInfoNV))))

instance Zero Win32KeyedMutexAcquireReleaseInfoNV where
  zero = Win32KeyedMutexAcquireReleaseInfoNV Nothing
                                             Data.Vector.empty
                                             Data.Vector.empty
                                             Data.Vector.empty
                                             Data.Vector.empty
                                             Data.Vector.empty

