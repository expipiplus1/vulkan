{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_memory_fd
  ( pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR
  , pattern VK_KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION
  , pattern VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME
  , vkGetMemoryFdKHR
  , vkGetMemoryFdPropertiesKHR
  , VkImportMemoryFdInfoKHR(..)
  , VkMemoryFdPropertiesKHR(..)
  , VkMemoryGetFdInfoKHR(..)
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CInt(..)
  )
import Foreign.Ptr
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkDevice
  )
import Graphics.Vulkan.Core10.Memory
  ( VkDeviceMemory
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( VkExternalMemoryHandleTypeFlagBits(..)
  )


-- | Nothing
pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR = VkStructureType 1000074000
-- | Nothing
pattern VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR = VkStructureType 1000074001
-- | Nothing
pattern VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR = VkStructureType 1000074002
pattern VK_KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION :: Integral a => a
pattern VK_KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION = 1
pattern VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME = "VK_KHR_external_memory_fd"
-- | 
foreign import ccall "vkGetMemoryFdKHR" vkGetMemoryFdKHR :: ("device" ::: VkDevice) -> ("pGetFdInfo" ::: Ptr VkMemoryGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO VkResult
-- | 
foreign import ccall "vkGetMemoryFdPropertiesKHR" vkGetMemoryFdPropertiesKHR :: ("device" ::: VkDevice) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagBits) -> ("fd" ::: CInt) -> ("pMemoryFdProperties" ::: Ptr VkMemoryFdPropertiesKHR) -> IO VkResult
-- | TODO: Struct comments
data VkImportMemoryFdInfoKHR = VkImportMemoryFdInfoKHR
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkHandleType :: VkExternalMemoryHandleTypeFlagBits
  , vkFd :: CInt
  }
  deriving (Eq, Show)

instance Storable VkImportMemoryFdInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkImportMemoryFdInfoKHR <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImportMemoryFdInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImportMemoryFdInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkHandleType (poked :: VkImportMemoryFdInfoKHR))
                *> poke (ptr `plusPtr` 20) (vkFd (poked :: VkImportMemoryFdInfoKHR))
-- | TODO: Struct comments
data VkMemoryFdPropertiesKHR = VkMemoryFdPropertiesKHR
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkMemoryTypeBits :: Word32
  }
  deriving (Eq, Show)

instance Storable VkMemoryFdPropertiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkMemoryFdPropertiesKHR <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMemoryFdPropertiesKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMemoryFdPropertiesKHR))
                *> poke (ptr `plusPtr` 16) (vkMemoryTypeBits (poked :: VkMemoryFdPropertiesKHR))
-- | TODO: Struct comments
data VkMemoryGetFdInfoKHR = VkMemoryGetFdInfoKHR
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkMemory :: VkDeviceMemory
  , vkHandleType :: VkExternalMemoryHandleTypeFlagBits
  }
  deriving (Eq, Show)

instance Storable VkMemoryGetFdInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkMemoryGetFdInfoKHR <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
                                  <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMemoryGetFdInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMemoryGetFdInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkMemory (poked :: VkMemoryGetFdInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkHandleType (poked :: VkMemoryGetFdInfoKHR))
