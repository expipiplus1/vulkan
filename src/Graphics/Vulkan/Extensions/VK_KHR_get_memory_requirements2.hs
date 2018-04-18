{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.Extensions.VK_KHR_get_memory_requirements2
  ( pattern VK_KHR_GET_MEMORY_REQUIREMENTS_2_SPEC_VERSION
  , pattern VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME
  , vkGetBufferMemoryRequirements2KHR
  , vkGetImageMemoryRequirements2KHR
  , vkGetImageSparseMemoryRequirements2KHR
  , VkBufferMemoryRequirementsInfo2KHR
  , pattern VkBufferMemoryRequirementsInfo2KHR
  , VkImageMemoryRequirementsInfo2KHR
  , pattern VkImageMemoryRequirementsInfo2KHR
  , VkImageSparseMemoryRequirementsInfo2KHR
  , pattern VkImageSparseMemoryRequirementsInfo2KHR
  , VkMemoryRequirements2KHR
  , pattern VkMemoryRequirements2KHR
  , VkSparseImageMemoryRequirements2KHR
  , pattern VkSparseImageMemoryRequirements2KHR
  , pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2_KHR
  , pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2_KHR
  , pattern VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2_KHR
  , pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2_KHR
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( Ptr
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Core
  ( VkStructureType(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkDevice
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( VkMemoryRequirements(..)
  , VkImage
  , VkBuffer
  )
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( VkSparseImageMemoryRequirements(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2
  ( pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2
  , pattern VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2
  , pattern VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2
  , pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2
  , pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2
  , VkSparseImageMemoryRequirements2(..)
  , VkImageSparseMemoryRequirementsInfo2(..)
  , vkGetImageSparseMemoryRequirements2
  , VkImageMemoryRequirementsInfo2(..)
  , vkGetImageMemoryRequirements2
  , VkMemoryRequirements2(..)
  , VkBufferMemoryRequirementsInfo2(..)
  , vkGetBufferMemoryRequirements2
  )


pattern VK_KHR_GET_MEMORY_REQUIREMENTS_2_SPEC_VERSION :: Integral a => a
pattern VK_KHR_GET_MEMORY_REQUIREMENTS_2_SPEC_VERSION = 1
pattern VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME = "VK_KHR_get_memory_requirements2"
vkGetBufferMemoryRequirements2KHR :: ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkBufferMemoryRequirementsInfo2) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements2) -> IO ()
vkGetBufferMemoryRequirements2KHR = vkGetBufferMemoryRequirements2
vkGetImageMemoryRequirements2KHR :: ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageMemoryRequirementsInfo2) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements2) -> IO ()
vkGetImageMemoryRequirements2KHR = vkGetImageMemoryRequirements2
vkGetImageSparseMemoryRequirements2KHR :: ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageSparseMemoryRequirementsInfo2) -> ("pSparseMemoryRequirementCount" ::: Ptr Word32) -> ("pSparseMemoryRequirements" ::: Ptr VkSparseImageMemoryRequirements2) -> IO ()
vkGetImageSparseMemoryRequirements2KHR = vkGetImageSparseMemoryRequirements2
type VkBufferMemoryRequirementsInfo2KHR = VkBufferMemoryRequirementsInfo2


pattern VkBufferMemoryRequirementsInfo2KHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("buffer" ::: VkBuffer) -> VkBufferMemoryRequirementsInfo2KHR
pattern VkBufferMemoryRequirementsInfo2KHR vkSType vkNext vkBuffer = VkBufferMemoryRequirementsInfo2 vkSType vkNext vkBuffer
type VkImageMemoryRequirementsInfo2KHR = VkImageMemoryRequirementsInfo2


pattern VkImageMemoryRequirementsInfo2KHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("image" ::: VkImage) -> VkImageMemoryRequirementsInfo2KHR
pattern VkImageMemoryRequirementsInfo2KHR vkSType vkNext vkImage = VkImageMemoryRequirementsInfo2 vkSType vkNext vkImage
type VkImageSparseMemoryRequirementsInfo2KHR = VkImageSparseMemoryRequirementsInfo2


pattern VkImageSparseMemoryRequirementsInfo2KHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("image" ::: VkImage) -> VkImageSparseMemoryRequirementsInfo2KHR
pattern VkImageSparseMemoryRequirementsInfo2KHR vkSType vkNext vkImage = VkImageSparseMemoryRequirementsInfo2 vkSType vkNext vkImage
type VkMemoryRequirements2KHR = VkMemoryRequirements2


pattern VkMemoryRequirements2KHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("memoryRequirements" ::: VkMemoryRequirements) -> VkMemoryRequirements2KHR
pattern VkMemoryRequirements2KHR vkSType vkNext vkMemoryRequirements = VkMemoryRequirements2 vkSType vkNext vkMemoryRequirements
type VkSparseImageMemoryRequirements2KHR = VkSparseImageMemoryRequirements2


pattern VkSparseImageMemoryRequirements2KHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("memoryRequirements" ::: VkSparseImageMemoryRequirements) -> VkSparseImageMemoryRequirements2KHR
pattern VkSparseImageMemoryRequirements2KHR vkSType vkNext vkMemoryRequirements = VkSparseImageMemoryRequirements2 vkSType vkNext vkMemoryRequirements
pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2_KHR = VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2
pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2_KHR = VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2
pattern VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2_KHR = VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2
pattern VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2_KHR = VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2
pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2_KHR = VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2
