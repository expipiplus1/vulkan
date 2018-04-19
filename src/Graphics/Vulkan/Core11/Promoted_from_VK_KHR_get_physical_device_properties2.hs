{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2
  ( pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2
  , pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2
  , vkGetPhysicalDeviceFeatures2
  , vkGetPhysicalDeviceProperties2
  , vkGetPhysicalDeviceFormatProperties2
  , vkGetPhysicalDeviceImageFormatProperties2
  , vkGetPhysicalDeviceQueueFamilyProperties2
  , vkGetPhysicalDeviceMemoryProperties2
  , vkGetPhysicalDeviceSparseImageFormatProperties2
  , VkPhysicalDeviceFeatures2(..)
  , VkPhysicalDeviceProperties2(..)
  , VkFormatProperties2(..)
  , VkImageFormatProperties2(..)
  , VkPhysicalDeviceImageFormatInfo2(..)
  , VkQueueFamilyProperties2(..)
  , VkPhysicalDeviceMemoryProperties2(..)
  , VkSparseImageFormatProperties2(..)
  , VkPhysicalDeviceSparseImageFormatInfo2(..)
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
  ( VkResult(..)
  , VkFormat(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkSampleCountFlagBits(..)
  , VkPhysicalDeviceMemoryProperties(..)
  , VkQueueFamilyProperties(..)
  , VkImageCreateFlags
  , VkImageUsageFlags
  , VkImageTiling(..)
  , VkImageType(..)
  , VkImageFormatProperties(..)
  , VkFormatProperties(..)
  , VkPhysicalDeviceProperties(..)
  , VkPhysicalDeviceFeatures(..)
  , VkPhysicalDevice
  )
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( VkSparseImageFormatProperties(..)
  )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2 = VkStructureType 1000059000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2 = VkStructureType 1000059001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2"
pattern VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2 = VkStructureType 1000059002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2"
pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2 = VkStructureType 1000059003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2 = VkStructureType 1000059004
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2"
pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2 = VkStructureType 1000059005
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2 = VkStructureType 1000059006
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2"
pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2 = VkStructureType 1000059007
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2 = VkStructureType 1000059008
-- | vkGetPhysicalDeviceFeatures2 - Reports capabilities of a physical device
foreign import ccall "vkGetPhysicalDeviceFeatures2" vkGetPhysicalDeviceFeatures2 :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkPhysicalDeviceFeatures2) -> IO ()
-- | vkGetPhysicalDeviceProperties2 - Returns properties of a physical device
foreign import ccall "vkGetPhysicalDeviceProperties2" vkGetPhysicalDeviceProperties2 :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pProperties" ::: Ptr VkPhysicalDeviceProperties2) -> IO ()
-- | vkGetPhysicalDeviceFormatProperties2 - Lists physical device’s format
-- capabilities
foreign import ccall "vkGetPhysicalDeviceFormatProperties2" vkGetPhysicalDeviceFormatProperties2 :: ("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("pFormatProperties" ::: Ptr VkFormatProperties2) -> IO ()
-- | vkGetPhysicalDeviceImageFormatProperties2 - Lists physical device’s
-- image format capabilities
foreign import ccall "vkGetPhysicalDeviceImageFormatProperties2" vkGetPhysicalDeviceImageFormatProperties2 :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pImageFormatInfo" ::: Ptr VkPhysicalDeviceImageFormatInfo2) -> ("pImageFormatProperties" ::: Ptr VkImageFormatProperties2) -> IO VkResult
-- | vkGetPhysicalDeviceQueueFamilyProperties2 - Reports properties of the
-- queues of the specified physical device
foreign import ccall "vkGetPhysicalDeviceQueueFamilyProperties2" vkGetPhysicalDeviceQueueFamilyProperties2 :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pQueueFamilyPropertyCount" ::: Ptr Word32) -> ("pQueueFamilyProperties" ::: Ptr VkQueueFamilyProperties2) -> IO ()
-- | vkGetPhysicalDeviceMemoryProperties2 - Reports memory information for
-- the specified physical device
foreign import ccall "vkGetPhysicalDeviceMemoryProperties2" vkGetPhysicalDeviceMemoryProperties2 :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pMemoryProperties" ::: Ptr VkPhysicalDeviceMemoryProperties2) -> IO ()
-- | vkGetPhysicalDeviceSparseImageFormatProperties2 - Retrieve properties of
-- an image format applied to sparse images
foreign import ccall "vkGetPhysicalDeviceSparseImageFormatProperties2" vkGetPhysicalDeviceSparseImageFormatProperties2 :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pFormatInfo" ::: Ptr VkPhysicalDeviceSparseImageFormatInfo2) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkSparseImageFormatProperties2) -> IO ()
-- | VkPhysicalDeviceFeatures2 - Structure describing the fine-grained
-- features that can be supported by an implementation
data VkPhysicalDeviceFeatures2 = VkPhysicalDeviceFeatures2
  { -- No documentation found for Nested "VkPhysicalDeviceFeatures2" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures2" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures2" "vkFeatures"
  vkFeatures :: VkPhysicalDeviceFeatures
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceFeatures2 where
  sizeOf ~_ = 240
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceFeatures2 <$> peek (ptr `plusPtr` 0)
                                       <*> peek (ptr `plusPtr` 8)
                                       <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceFeatures2))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceFeatures2))
                *> poke (ptr `plusPtr` 16) (vkFeatures (poked :: VkPhysicalDeviceFeatures2))
-- | VkPhysicalDeviceProperties2 - Structure specifying physical device
-- properties
data VkPhysicalDeviceProperties2 = VkPhysicalDeviceProperties2
  { -- No documentation found for Nested "VkPhysicalDeviceProperties2" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceProperties2" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceProperties2" "vkProperties"
  vkProperties :: VkPhysicalDeviceProperties
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceProperties2 where
  sizeOf ~_ = 840
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceProperties2 <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
                                         <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceProperties2))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceProperties2))
                *> poke (ptr `plusPtr` 16) (vkProperties (poked :: VkPhysicalDeviceProperties2))
-- | VkFormatProperties2 - Structure specifying image format properties
data VkFormatProperties2 = VkFormatProperties2
  { -- No documentation found for Nested "VkFormatProperties2" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkFormatProperties2" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkFormatProperties2" "vkFormatProperties"
  vkFormatProperties :: VkFormatProperties
  }
  deriving (Eq, Show)

instance Storable VkFormatProperties2 where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkFormatProperties2 <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 8)
                                 <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkFormatProperties2))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkFormatProperties2))
                *> poke (ptr `plusPtr` 16) (vkFormatProperties (poked :: VkFormatProperties2))
-- | VkImageFormatProperties2 - Structure specifying a image format
-- properties
data VkImageFormatProperties2 = VkImageFormatProperties2
  { -- No documentation found for Nested "VkImageFormatProperties2" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkImageFormatProperties2" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkImageFormatProperties2" "vkImageFormatProperties"
  vkImageFormatProperties :: VkImageFormatProperties
  }
  deriving (Eq, Show)

instance Storable VkImageFormatProperties2 where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkImageFormatProperties2 <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImageFormatProperties2))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImageFormatProperties2))
                *> poke (ptr `plusPtr` 16) (vkImageFormatProperties (poked :: VkImageFormatProperties2))
-- | VkPhysicalDeviceImageFormatInfo2 - Structure specifying image creation
-- parameters
data VkPhysicalDeviceImageFormatInfo2 = VkPhysicalDeviceImageFormatInfo2
  { -- No documentation found for Nested "VkPhysicalDeviceImageFormatInfo2" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceImageFormatInfo2" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceImageFormatInfo2" "vkFormat"
  vkFormat :: VkFormat
  , -- No documentation found for Nested "VkPhysicalDeviceImageFormatInfo2" "vkType"
  vkType :: VkImageType
  , -- No documentation found for Nested "VkPhysicalDeviceImageFormatInfo2" "vkTiling"
  vkTiling :: VkImageTiling
  , -- No documentation found for Nested "VkPhysicalDeviceImageFormatInfo2" "vkUsage"
  vkUsage :: VkImageUsageFlags
  , -- No documentation found for Nested "VkPhysicalDeviceImageFormatInfo2" "vkFlags"
  vkFlags :: VkImageCreateFlags
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceImageFormatInfo2 where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceImageFormatInfo2 <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 16)
                                              <*> peek (ptr `plusPtr` 20)
                                              <*> peek (ptr `plusPtr` 24)
                                              <*> peek (ptr `plusPtr` 28)
                                              <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceImageFormatInfo2))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceImageFormatInfo2))
                *> poke (ptr `plusPtr` 16) (vkFormat (poked :: VkPhysicalDeviceImageFormatInfo2))
                *> poke (ptr `plusPtr` 20) (vkType (poked :: VkPhysicalDeviceImageFormatInfo2))
                *> poke (ptr `plusPtr` 24) (vkTiling (poked :: VkPhysicalDeviceImageFormatInfo2))
                *> poke (ptr `plusPtr` 28) (vkUsage (poked :: VkPhysicalDeviceImageFormatInfo2))
                *> poke (ptr `plusPtr` 32) (vkFlags (poked :: VkPhysicalDeviceImageFormatInfo2))
-- | VkQueueFamilyProperties2 - Structure providing information about a queue
-- family
data VkQueueFamilyProperties2 = VkQueueFamilyProperties2
  { -- No documentation found for Nested "VkQueueFamilyProperties2" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkQueueFamilyProperties2" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkQueueFamilyProperties2" "vkQueueFamilyProperties"
  vkQueueFamilyProperties :: VkQueueFamilyProperties
  }
  deriving (Eq, Show)

instance Storable VkQueueFamilyProperties2 where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkQueueFamilyProperties2 <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkQueueFamilyProperties2))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkQueueFamilyProperties2))
                *> poke (ptr `plusPtr` 16) (vkQueueFamilyProperties (poked :: VkQueueFamilyProperties2))
-- | VkPhysicalDeviceMemoryProperties2 - Structure specifying physical device
-- memory properties
data VkPhysicalDeviceMemoryProperties2 = VkPhysicalDeviceMemoryProperties2
  { -- No documentation found for Nested "VkPhysicalDeviceMemoryProperties2" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceMemoryProperties2" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceMemoryProperties2" "vkMemoryProperties"
  vkMemoryProperties :: VkPhysicalDeviceMemoryProperties
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceMemoryProperties2 where
  sizeOf ~_ = 536
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceMemoryProperties2 <$> peek (ptr `plusPtr` 0)
                                               <*> peek (ptr `plusPtr` 8)
                                               <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceMemoryProperties2))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceMemoryProperties2))
                *> poke (ptr `plusPtr` 16) (vkMemoryProperties (poked :: VkPhysicalDeviceMemoryProperties2))
-- | VkSparseImageFormatProperties2 - Structure specifying sparse image
-- format properties
data VkSparseImageFormatProperties2 = VkSparseImageFormatProperties2
  { -- No documentation found for Nested "VkSparseImageFormatProperties2" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkSparseImageFormatProperties2" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkSparseImageFormatProperties2" "vkProperties"
  vkProperties :: VkSparseImageFormatProperties
  }
  deriving (Eq, Show)

instance Storable VkSparseImageFormatProperties2 where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkSparseImageFormatProperties2 <$> peek (ptr `plusPtr` 0)
                                            <*> peek (ptr `plusPtr` 8)
                                            <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSparseImageFormatProperties2))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSparseImageFormatProperties2))
                *> poke (ptr `plusPtr` 16) (vkProperties (poked :: VkSparseImageFormatProperties2))
-- | VkPhysicalDeviceSparseImageFormatInfo2 - Structure specifying sparse
-- image format inputs
data VkPhysicalDeviceSparseImageFormatInfo2 = VkPhysicalDeviceSparseImageFormatInfo2
  { -- No documentation found for Nested "VkPhysicalDeviceSparseImageFormatInfo2" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceSparseImageFormatInfo2" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceSparseImageFormatInfo2" "vkFormat"
  vkFormat :: VkFormat
  , -- No documentation found for Nested "VkPhysicalDeviceSparseImageFormatInfo2" "vkType"
  vkType :: VkImageType
  , -- No documentation found for Nested "VkPhysicalDeviceSparseImageFormatInfo2" "vkSamples"
  vkSamples :: VkSampleCountFlagBits
  , -- No documentation found for Nested "VkPhysicalDeviceSparseImageFormatInfo2" "vkUsage"
  vkUsage :: VkImageUsageFlags
  , -- No documentation found for Nested "VkPhysicalDeviceSparseImageFormatInfo2" "vkTiling"
  vkTiling :: VkImageTiling
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceSparseImageFormatInfo2 where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceSparseImageFormatInfo2 <$> peek (ptr `plusPtr` 0)
                                                    <*> peek (ptr `plusPtr` 8)
                                                    <*> peek (ptr `plusPtr` 16)
                                                    <*> peek (ptr `plusPtr` 20)
                                                    <*> peek (ptr `plusPtr` 24)
                                                    <*> peek (ptr `plusPtr` 28)
                                                    <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceSparseImageFormatInfo2))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceSparseImageFormatInfo2))
                *> poke (ptr `plusPtr` 16) (vkFormat (poked :: VkPhysicalDeviceSparseImageFormatInfo2))
                *> poke (ptr `plusPtr` 20) (vkType (poked :: VkPhysicalDeviceSparseImageFormatInfo2))
                *> poke (ptr `plusPtr` 24) (vkSamples (poked :: VkPhysicalDeviceSparseImageFormatInfo2))
                *> poke (ptr `plusPtr` 28) (vkUsage (poked :: VkPhysicalDeviceSparseImageFormatInfo2))
                *> poke (ptr `plusPtr` 32) (vkTiling (poked :: VkPhysicalDeviceSparseImageFormatInfo2))
