{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2
  ( pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2
  , pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2
  , pattern VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2
  , pattern VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2
  , pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2
  , vkGetBufferMemoryRequirements2
  , vkGetImageMemoryRequirements2
  , vkGetImageSparseMemoryRequirements2
  , VkBufferMemoryRequirementsInfo2(..)
  , VkImageMemoryRequirementsInfo2(..)
  , VkImageSparseMemoryRequirementsInfo2(..)
  , VkMemoryRequirements2(..)
  , VkSparseImageMemoryRequirements2(..)
  ) where

import Data.Word
  ( Word32
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


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2"
pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2 = VkStructureType 1000146000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2"
pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2 = VkStructureType 1000146001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2"
pattern VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2 = VkStructureType 1000146002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2"
pattern VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2 = VkStructureType 1000146003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2"
pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2 = VkStructureType 1000146004
-- | vkGetBufferMemoryRequirements2 - Returns the memory requirements for
-- specified Vulkan object
foreign import ccall "vkGetBufferMemoryRequirements2" vkGetBufferMemoryRequirements2 :: ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkBufferMemoryRequirementsInfo2) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements2) -> IO ()
-- | vkGetImageMemoryRequirements2 - Returns the memory requirements for
-- specified Vulkan object
foreign import ccall "vkGetImageMemoryRequirements2" vkGetImageMemoryRequirements2 :: ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageMemoryRequirementsInfo2) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements2) -> IO ()
-- | vkGetImageSparseMemoryRequirements2 - Query the memory requirements for
-- a sparse image
foreign import ccall "vkGetImageSparseMemoryRequirements2" vkGetImageSparseMemoryRequirements2 :: ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageSparseMemoryRequirementsInfo2) -> ("pSparseMemoryRequirementCount" ::: Ptr Word32) -> ("pSparseMemoryRequirements" ::: Ptr VkSparseImageMemoryRequirements2) -> IO ()
-- | VkBufferMemoryRequirementsInfo2 - (None)
data VkBufferMemoryRequirementsInfo2 = VkBufferMemoryRequirementsInfo2
  { -- No documentation found for Nested "VkBufferMemoryRequirementsInfo2" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkBufferMemoryRequirementsInfo2" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkBufferMemoryRequirementsInfo2" "vkBuffer"
  vkBuffer :: VkBuffer
  }
  deriving (Eq, Show)

instance Storable VkBufferMemoryRequirementsInfo2 where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkBufferMemoryRequirementsInfo2 <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 8)
                                             <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBufferMemoryRequirementsInfo2))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBufferMemoryRequirementsInfo2))
                *> poke (ptr `plusPtr` 16) (vkBuffer (poked :: VkBufferMemoryRequirementsInfo2))
-- | VkImageMemoryRequirementsInfo2 - (None)
data VkImageMemoryRequirementsInfo2 = VkImageMemoryRequirementsInfo2
  { -- No documentation found for Nested "VkImageMemoryRequirementsInfo2" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkImageMemoryRequirementsInfo2" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkImageMemoryRequirementsInfo2" "vkImage"
  vkImage :: VkImage
  }
  deriving (Eq, Show)

instance Storable VkImageMemoryRequirementsInfo2 where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkImageMemoryRequirementsInfo2 <$> peek (ptr `plusPtr` 0)
                                            <*> peek (ptr `plusPtr` 8)
                                            <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImageMemoryRequirementsInfo2))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImageMemoryRequirementsInfo2))
                *> poke (ptr `plusPtr` 16) (vkImage (poked :: VkImageMemoryRequirementsInfo2))
-- | VkImageSparseMemoryRequirementsInfo2 - (None)
data VkImageSparseMemoryRequirementsInfo2 = VkImageSparseMemoryRequirementsInfo2
  { -- No documentation found for Nested "VkImageSparseMemoryRequirementsInfo2" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkImageSparseMemoryRequirementsInfo2" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkImageSparseMemoryRequirementsInfo2" "vkImage"
  vkImage :: VkImage
  }
  deriving (Eq, Show)

instance Storable VkImageSparseMemoryRequirementsInfo2 where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkImageSparseMemoryRequirementsInfo2 <$> peek (ptr `plusPtr` 0)
                                                  <*> peek (ptr `plusPtr` 8)
                                                  <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImageSparseMemoryRequirementsInfo2))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImageSparseMemoryRequirementsInfo2))
                *> poke (ptr `plusPtr` 16) (vkImage (poked :: VkImageSparseMemoryRequirementsInfo2))
-- | VkMemoryRequirements2 - Structure specifying memory requirements
data VkMemoryRequirements2 = VkMemoryRequirements2
  { -- No documentation found for Nested "VkMemoryRequirements2" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkMemoryRequirements2" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkMemoryRequirements2" "vkMemoryRequirements"
  vkMemoryRequirements :: VkMemoryRequirements
  }
  deriving (Eq, Show)

instance Storable VkMemoryRequirements2 where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkMemoryRequirements2 <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMemoryRequirements2))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMemoryRequirements2))
                *> poke (ptr `plusPtr` 16) (vkMemoryRequirements (poked :: VkMemoryRequirements2))
-- | VkSparseImageMemoryRequirements2 - (None)
data VkSparseImageMemoryRequirements2 = VkSparseImageMemoryRequirements2
  { -- No documentation found for Nested "VkSparseImageMemoryRequirements2" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkSparseImageMemoryRequirements2" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkSparseImageMemoryRequirements2" "vkMemoryRequirements"
  vkMemoryRequirements :: VkSparseImageMemoryRequirements
  }
  deriving (Eq, Show)

instance Storable VkSparseImageMemoryRequirements2 where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkSparseImageMemoryRequirements2 <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSparseImageMemoryRequirements2))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSparseImageMemoryRequirements2))
                *> poke (ptr `plusPtr` 16) (vkMemoryRequirements (poked :: VkSparseImageMemoryRequirements2))
