{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_memory_fd
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  ImportMemoryFdInfoKHR(..)
  , 
  MemoryFdPropertiesKHR(..)
  , MemoryGetFdInfoKHR(..)
#endif
  , getMemoryFdKHR
#if defined(VK_USE_PLATFORM_GGP)
  , getMemoryFdPropertiesKHR
#endif
  , pattern KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME
  , pattern KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION
  , pattern STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR
  , pattern STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR
  , pattern STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR
  ) where


#if defined(VK_USE_PLATFORM_GGP)
import Control.Monad
  ( (<=<)
  )
#endif
import Data.String
  ( IsString
  )

#if defined(VK_USE_PLATFORM_GGP)
import Data.Word
  ( Word32
  )
#endif
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
import Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_fd
  ( vkGetMemoryFdKHR
  , pattern VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME
  , pattern VK_KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_fd
  ( vkGetMemoryFdPropertiesKHR
  )
#endif
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Memory
  ( DeviceMemory
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( ExternalMemoryHandleTypeFlagBits
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( FromCStruct(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR
  , pattern STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR
  , pattern STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkImportMemoryFdInfoKHR"
data ImportMemoryFdInfoKHR = ImportMemoryFdInfoKHR
  { -- No documentation found for Nested "ImportMemoryFdInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImportMemoryFdInfoKHR" "handleType"
  handleType :: ExternalMemoryHandleTypeFlagBits
  , -- No documentation found for Nested "ImportMemoryFdInfoKHR" "fd"
  fd :: CInt
  }
  deriving (Show, Eq)

instance Zero ImportMemoryFdInfoKHR where
  zero = ImportMemoryFdInfoKHR Nothing
                               zero
                               zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkMemoryFdPropertiesKHR"
data MemoryFdPropertiesKHR = MemoryFdPropertiesKHR
  { -- No documentation found for Nested "MemoryFdPropertiesKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MemoryFdPropertiesKHR" "memoryTypeBits"
  memoryTypeBits :: Word32
  }
  deriving (Show, Eq)

instance Zero MemoryFdPropertiesKHR where
  zero = MemoryFdPropertiesKHR Nothing
                               zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkMemoryGetFdInfoKHR"
data MemoryGetFdInfoKHR = MemoryGetFdInfoKHR
  { -- No documentation found for Nested "MemoryGetFdInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MemoryGetFdInfoKHR" "memory"
  memory :: DeviceMemory
  , -- No documentation found for Nested "MemoryGetFdInfoKHR" "handleType"
  handleType :: ExternalMemoryHandleTypeFlagBits
  }
  deriving (Show, Eq)

instance Zero MemoryGetFdInfoKHR where
  zero = MemoryGetFdInfoKHR Nothing
                            zero
                            zero

#endif


-- No documentation found for TopLevel "vkGetMemoryFdKHR"
getMemoryFdKHR :: Device ->  MemoryGetFdInfoKHR ->  IO (CInt)
getMemoryFdKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetMemoryFdPropertiesKHR"
getMemoryFdPropertiesKHR :: Device ->  ExternalMemoryHandleTypeFlagBits ->  CInt ->  IO (MemoryFdPropertiesKHR)
getMemoryFdPropertiesKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}
#endif

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME"
pattern KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME = VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION"
pattern KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION :: Integral a => a
pattern KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION = VK_KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION
