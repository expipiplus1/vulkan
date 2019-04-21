{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_get_memory_requirements2
  ( BufferMemoryRequirementsInfo2KHR
  , ImageMemoryRequirementsInfo2KHR
  , ImageSparseMemoryRequirementsInfo2KHR
  , SparseImageMemoryRequirements2KHR
  , getBufferMemoryRequirements2KHR
  , getImageMemoryRequirements2KHR
  , getImageSparseMemoryRequirements2KHR
  , pattern VK_KHR_GET_MEMORY_REQUIREMENTS_2_SPEC_VERSION
  , pattern VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2_KHR
  , pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2
  , pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2_KHR
  , pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2
  , pattern VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2_KHR
  , pattern VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2
  , pattern VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2
  , pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2_KHR
  , pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2
  , MemoryRequirements2KHR
  ) where

import Data.Vector
  ( Vector
  )
import Data.Word
  ( Word32
  )


import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2
  ( BufferMemoryRequirementsInfo2(..)
  , ImageMemoryRequirementsInfo2(..)
  , ImageSparseMemoryRequirementsInfo2(..)
  , MemoryRequirements2(..)
  , SparseImageMemoryRequirements2(..)
  , getBufferMemoryRequirements2
  , getImageMemoryRequirements2
  , getImageSparseMemoryRequirements2
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2
  ( pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2
  , pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2
  , pattern VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2
  , pattern VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2
  , pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_get_memory_requirements2
  ( pattern VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME
  , pattern VK_KHR_GET_MEMORY_REQUIREMENTS_2_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2_KHR
  , pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2_KHR
  , pattern VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2_KHR
  , pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2_KHR
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2
  ( MemoryRequirements2KHR
  )


type BufferMemoryRequirementsInfo2KHR = BufferMemoryRequirementsInfo2
-- TODO: Pattern constructor alias)

type ImageMemoryRequirementsInfo2KHR = ImageMemoryRequirementsInfo2
-- TODO: Pattern constructor alias)

type ImageSparseMemoryRequirementsInfo2KHR = ImageSparseMemoryRequirementsInfo2
-- TODO: Pattern constructor alias)

type SparseImageMemoryRequirements2KHR = SparseImageMemoryRequirements2
-- TODO: Pattern constructor alias)

getBufferMemoryRequirements2KHR :: Device ->  BufferMemoryRequirementsInfo2 ->  IO (MemoryRequirements2)
getBufferMemoryRequirements2KHR = getBufferMemoryRequirements2

getImageMemoryRequirements2KHR :: Device ->  ImageMemoryRequirementsInfo2 ->  IO (MemoryRequirements2)
getImageMemoryRequirements2KHR = getImageMemoryRequirements2

getImageSparseMemoryRequirements2KHR :: Device ->  ImageSparseMemoryRequirementsInfo2 ->  Word32 ->  IO (Vector SparseImageMemoryRequirements2)
getImageSparseMemoryRequirements2KHR = getImageSparseMemoryRequirements2
