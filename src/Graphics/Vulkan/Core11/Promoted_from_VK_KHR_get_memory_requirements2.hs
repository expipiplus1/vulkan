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


-- | Nothing
pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2 = VkStructureType 1000146000
-- | Nothing
pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2 = VkStructureType 1000146001
-- | Nothing
pattern VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2 = VkStructureType 1000146002
-- | Nothing
pattern VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2 = VkStructureType 1000146003
-- | Nothing
pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2 = VkStructureType 1000146004
-- | 
foreign import ccall "vkGetBufferMemoryRequirements2" vkGetBufferMemoryRequirements2 :: ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkBufferMemoryRequirementsInfo2) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements2) -> IO ()
-- | 
foreign import ccall "vkGetImageMemoryRequirements2" vkGetImageMemoryRequirements2 :: ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageMemoryRequirementsInfo2) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements2) -> IO ()
-- | 
foreign import ccall "vkGetImageSparseMemoryRequirements2" vkGetImageSparseMemoryRequirements2 :: ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageSparseMemoryRequirementsInfo2) -> ("pSparseMemoryRequirementCount" ::: Ptr Word32) -> ("pSparseMemoryRequirements" ::: Ptr VkSparseImageMemoryRequirements2) -> IO ()
-- | TODO: Struct comments
data VkBufferMemoryRequirementsInfo2 = VkBufferMemoryRequirementsInfo2
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkBuffer :: VkBuffer
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
-- | TODO: Struct comments
data VkImageMemoryRequirementsInfo2 = VkImageMemoryRequirementsInfo2
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkImage :: VkImage
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
-- | TODO: Struct comments
data VkImageSparseMemoryRequirementsInfo2 = VkImageSparseMemoryRequirementsInfo2
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkImage :: VkImage
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
-- | TODO: Struct comments
data VkMemoryRequirements2 = VkMemoryRequirements2
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkMemoryRequirements :: VkMemoryRequirements
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
-- | TODO: Struct comments
data VkSparseImageMemoryRequirements2 = VkSparseImageMemoryRequirements2
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkMemoryRequirements :: VkSparseImageMemoryRequirements
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
