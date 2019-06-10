{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_EXT_external_memory_host
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  ImportMemoryHostPointerInfoEXT(..)
  , 
  MemoryHostPointerPropertiesEXT(..)
  , PhysicalDeviceExternalMemoryHostPropertiesEXT(..)
  , getMemoryHostPointerPropertiesEXT
#endif
  , pattern EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME
  , pattern EXT_EXTERNAL_MEMORY_HOST_SPEC_VERSION
  , ExternalMemoryHandleTypeFlagsKHR
  , ExternalMemoryHandleTypeFlagBitsKHR
  , pattern STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT
  , pattern STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT
  , pattern EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT
  , pattern EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT
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

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Marshal.Alloc
  ( alloca
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Ptr
  ( nullPtr
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Ptr
  ( Ptr
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Storable
  ( peek
  )
#endif



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_host
  ( pattern VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME
  , pattern VK_EXT_EXTERNAL_MEMORY_HOST_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_host
  ( vkGetMemoryHostPointerPropertiesEXT
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  , DeviceSize
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
  ( pattern STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT
  , pattern STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( ExternalMemoryHandleTypeFlagBitsKHR
  , ExternalMemoryHandleTypeFlagsKHR
  , pattern EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT
  , pattern EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkImportMemoryHostPointerInfoEXT"
data ImportMemoryHostPointerInfoEXT = ImportMemoryHostPointerInfoEXT
  { -- No documentation found for Nested "ImportMemoryHostPointerInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImportMemoryHostPointerInfoEXT" "handleType"
  handleType :: ExternalMemoryHandleTypeFlagBits
  , -- No documentation found for Nested "ImportMemoryHostPointerInfoEXT" "pHostPointer"
  hostPointer :: Ptr ()
  }
  deriving (Show, Eq)

instance Zero ImportMemoryHostPointerInfoEXT where
  zero = ImportMemoryHostPointerInfoEXT Nothing
                                        zero
                                        nullPtr

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkMemoryHostPointerPropertiesEXT"
data MemoryHostPointerPropertiesEXT = MemoryHostPointerPropertiesEXT
  { -- No documentation found for Nested "MemoryHostPointerPropertiesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MemoryHostPointerPropertiesEXT" "memoryTypeBits"
  memoryTypeBits :: Word32
  }
  deriving (Show, Eq)

instance Zero MemoryHostPointerPropertiesEXT where
  zero = MemoryHostPointerPropertiesEXT Nothing
                                        zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceExternalMemoryHostPropertiesEXT"
data PhysicalDeviceExternalMemoryHostPropertiesEXT = PhysicalDeviceExternalMemoryHostPropertiesEXT
  { -- No documentation found for Nested "PhysicalDeviceExternalMemoryHostPropertiesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceExternalMemoryHostPropertiesEXT" "minImportedHostPointerAlignment"
  minImportedHostPointerAlignment :: DeviceSize
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceExternalMemoryHostPropertiesEXT where
  zero = PhysicalDeviceExternalMemoryHostPropertiesEXT Nothing
                                                       zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetMemoryHostPointerPropertiesEXT"
getMemoryHostPointerPropertiesEXT :: Device ->  ExternalMemoryHandleTypeFlagBits ->  Ptr () ->  IO (MemoryHostPointerPropertiesEXT)
getMemoryHostPointerPropertiesEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}
#endif

-- No documentation found for TopLevel "VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME"
pattern EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME = VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_EXTERNAL_MEMORY_HOST_SPEC_VERSION"
pattern EXT_EXTERNAL_MEMORY_HOST_SPEC_VERSION :: Integral a => a
pattern EXT_EXTERNAL_MEMORY_HOST_SPEC_VERSION = VK_EXT_EXTERNAL_MEMORY_HOST_SPEC_VERSION
