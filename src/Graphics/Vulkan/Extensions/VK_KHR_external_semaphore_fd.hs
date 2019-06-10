{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_fd
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  ImportSemaphoreFdInfoKHR(..)
  , 
  SemaphoreGetFdInfoKHR(..)
#endif
  , getSemaphoreFdKHR
  , importSemaphoreFdKHR
  , pattern KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME
  , pattern KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION
  , pattern STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR
  , pattern STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR
  ) where

import Data.String
  ( IsString
  )
import Foreign.C.Types
  ( CInt(..)
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( with
  )
import Foreign.Storable
  ( peek
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_fd
  ( vkGetSemaphoreFdKHR
  , vkImportSemaphoreFdKHR
  , pattern VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME
  , pattern VK_KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Queue
  ( Semaphore
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore
  ( SemaphoreImportFlags
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities
  ( ExternalSemaphoreHandleTypeFlagBits
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR
  , pattern STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkImportSemaphoreFdInfoKHR"
data ImportSemaphoreFdInfoKHR = ImportSemaphoreFdInfoKHR
  { -- No documentation found for Nested "ImportSemaphoreFdInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImportSemaphoreFdInfoKHR" "semaphore"
  semaphore :: Semaphore
  , -- No documentation found for Nested "ImportSemaphoreFdInfoKHR" "flags"
  flags :: SemaphoreImportFlags
  , -- No documentation found for Nested "ImportSemaphoreFdInfoKHR" "handleType"
  handleType :: ExternalSemaphoreHandleTypeFlagBits
  , -- No documentation found for Nested "ImportSemaphoreFdInfoKHR" "fd"
  fd :: CInt
  }
  deriving (Show, Eq)

instance Zero ImportSemaphoreFdInfoKHR where
  zero = ImportSemaphoreFdInfoKHR Nothing
                                  zero
                                  zero
                                  zero
                                  zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkSemaphoreGetFdInfoKHR"
data SemaphoreGetFdInfoKHR = SemaphoreGetFdInfoKHR
  { -- No documentation found for Nested "SemaphoreGetFdInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SemaphoreGetFdInfoKHR" "semaphore"
  semaphore :: Semaphore
  , -- No documentation found for Nested "SemaphoreGetFdInfoKHR" "handleType"
  handleType :: ExternalSemaphoreHandleTypeFlagBits
  }
  deriving (Show, Eq)

instance Zero SemaphoreGetFdInfoKHR where
  zero = SemaphoreGetFdInfoKHR Nothing
                               zero
                               zero

#endif


-- No documentation found for TopLevel "vkGetSemaphoreFdKHR"
getSemaphoreFdKHR :: Device ->  SemaphoreGetFdInfoKHR ->  IO (CInt)
getSemaphoreFdKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkImportSemaphoreFdKHR"
importSemaphoreFdKHR :: Device ->  ImportSemaphoreFdInfoKHR ->  IO ()
importSemaphoreFdKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME"
pattern KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME = VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION"
pattern KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION :: Integral a => a
pattern KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION = VK_KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION
