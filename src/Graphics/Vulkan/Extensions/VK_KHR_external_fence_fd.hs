{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_fence_fd
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  FenceGetFdInfoKHR(..)
  , 
  ImportFenceFdInfoKHR(..)
#endif
  , getFenceFdKHR
  , importFenceFdKHR
  , pattern KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME
  , pattern KHR_EXTERNAL_FENCE_FD_SPEC_VERSION
  , pattern STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR
  , pattern STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR
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
import Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_fd
  ( vkGetFenceFdKHR
  , vkImportFenceFdKHR
  , pattern VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME
  , pattern VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Queue
  ( Fence
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence
  ( FenceImportFlags
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( ExternalFenceHandleTypeFlagBits
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR
  , pattern STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkFenceGetFdInfoKHR"
data FenceGetFdInfoKHR = FenceGetFdInfoKHR
  { -- No documentation found for Nested "FenceGetFdInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "FenceGetFdInfoKHR" "fence"
  fence :: Fence
  , -- No documentation found for Nested "FenceGetFdInfoKHR" "handleType"
  handleType :: ExternalFenceHandleTypeFlagBits
  }
  deriving (Show, Eq)

instance Zero FenceGetFdInfoKHR where
  zero = FenceGetFdInfoKHR Nothing
                           zero
                           zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkImportFenceFdInfoKHR"
data ImportFenceFdInfoKHR = ImportFenceFdInfoKHR
  { -- No documentation found for Nested "ImportFenceFdInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImportFenceFdInfoKHR" "fence"
  fence :: Fence
  , -- No documentation found for Nested "ImportFenceFdInfoKHR" "flags"
  flags :: FenceImportFlags
  , -- No documentation found for Nested "ImportFenceFdInfoKHR" "handleType"
  handleType :: ExternalFenceHandleTypeFlagBits
  , -- No documentation found for Nested "ImportFenceFdInfoKHR" "fd"
  fd :: CInt
  }
  deriving (Show, Eq)

instance Zero ImportFenceFdInfoKHR where
  zero = ImportFenceFdInfoKHR Nothing
                              zero
                              zero
                              zero
                              zero

#endif


-- No documentation found for TopLevel "vkGetFenceFdKHR"
getFenceFdKHR :: Device ->  FenceGetFdInfoKHR ->  IO (CInt)
getFenceFdKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkImportFenceFdKHR"
importFenceFdKHR :: Device ->  ImportFenceFdInfoKHR ->  IO ()
importFenceFdKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME"
pattern KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME = VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION"
pattern KHR_EXTERNAL_FENCE_FD_SPEC_VERSION :: Integral a => a
pattern KHR_EXTERNAL_FENCE_FD_SPEC_VERSION = VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION
