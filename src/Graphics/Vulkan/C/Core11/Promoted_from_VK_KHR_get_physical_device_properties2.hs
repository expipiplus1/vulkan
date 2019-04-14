{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2
  ( VkFormatProperties2(..)
  , VkImageFormatProperties2(..)
  , VkPhysicalDeviceFeatures2(..)
  , VkPhysicalDeviceImageFormatInfo2(..)
  , VkPhysicalDeviceMemoryProperties2(..)
  , VkPhysicalDeviceProperties2(..)
  , VkPhysicalDeviceSparseImageFormatInfo2(..)
  , VkQueueFamilyProperties2(..)
  , VkSparseImageFormatProperties2(..)
#if defined(EXPOSE_CORE11_COMMANDS)
  , vkGetPhysicalDeviceFeatures2
#endif
  , FN_vkGetPhysicalDeviceFeatures2
  , PFN_vkGetPhysicalDeviceFeatures2
#if defined(EXPOSE_CORE11_COMMANDS)
  , vkGetPhysicalDeviceFormatProperties2
#endif
  , FN_vkGetPhysicalDeviceFormatProperties2
  , PFN_vkGetPhysicalDeviceFormatProperties2
#if defined(EXPOSE_CORE11_COMMANDS)
  , vkGetPhysicalDeviceImageFormatProperties2
#endif
  , FN_vkGetPhysicalDeviceImageFormatProperties2
  , PFN_vkGetPhysicalDeviceImageFormatProperties2
#if defined(EXPOSE_CORE11_COMMANDS)
  , vkGetPhysicalDeviceMemoryProperties2
#endif
  , FN_vkGetPhysicalDeviceMemoryProperties2
  , PFN_vkGetPhysicalDeviceMemoryProperties2
#if defined(EXPOSE_CORE11_COMMANDS)
  , vkGetPhysicalDeviceProperties2
#endif
  , FN_vkGetPhysicalDeviceProperties2
  , PFN_vkGetPhysicalDeviceProperties2
#if defined(EXPOSE_CORE11_COMMANDS)
  , vkGetPhysicalDeviceQueueFamilyProperties2
#endif
  , FN_vkGetPhysicalDeviceQueueFamilyProperties2
  , PFN_vkGetPhysicalDeviceQueueFamilyProperties2
#if defined(EXPOSE_CORE11_COMMANDS)
  , vkGetPhysicalDeviceSparseImageFormatProperties2
#endif
  , FN_vkGetPhysicalDeviceSparseImageFormatProperties2
  , PFN_vkGetPhysicalDeviceSparseImageFormatProperties2
  , pattern VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2
  , pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2
  ) where

import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkFormat(..)
  , VkResult(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkFormatProperties(..)
  , VkImageFormatProperties(..)
  , VkImageTiling(..)
  , VkImageType(..)
  , VkPhysicalDeviceFeatures(..)
  , VkPhysicalDeviceMemoryProperties(..)
  , VkPhysicalDeviceProperties(..)
  , VkQueueFamilyProperties(..)
  , VkSampleCountFlagBits(..)
  , VkImageCreateFlags
  , VkImageUsageFlags
  , VkPhysicalDevice
  )
import Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement
  ( VkSparseImageFormatProperties(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkFormatProperties2"
data VkFormatProperties2 = VkFormatProperties2
  { -- No documentation found for Nested "VkFormatProperties2" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkFormatProperties2" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkFormatProperties2" "formatProperties"
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
-- No documentation found for TopLevel "VkImageFormatProperties2"
data VkImageFormatProperties2 = VkImageFormatProperties2
  { -- No documentation found for Nested "VkImageFormatProperties2" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkImageFormatProperties2" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkImageFormatProperties2" "imageFormatProperties"
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
-- No documentation found for TopLevel "VkPhysicalDeviceFeatures2"
data VkPhysicalDeviceFeatures2 = VkPhysicalDeviceFeatures2
  { -- No documentation found for Nested "VkPhysicalDeviceFeatures2" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures2" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceFeatures2" "features"
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
-- No documentation found for TopLevel "VkPhysicalDeviceImageFormatInfo2"
data VkPhysicalDeviceImageFormatInfo2 = VkPhysicalDeviceImageFormatInfo2
  { -- No documentation found for Nested "VkPhysicalDeviceImageFormatInfo2" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceImageFormatInfo2" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceImageFormatInfo2" "format"
  vkFormat :: VkFormat
  , -- No documentation found for Nested "VkPhysicalDeviceImageFormatInfo2" "type"
  vkType :: VkImageType
  , -- No documentation found for Nested "VkPhysicalDeviceImageFormatInfo2" "tiling"
  vkTiling :: VkImageTiling
  , -- No documentation found for Nested "VkPhysicalDeviceImageFormatInfo2" "usage"
  vkUsage :: VkImageUsageFlags
  , -- No documentation found for Nested "VkPhysicalDeviceImageFormatInfo2" "flags"
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
-- No documentation found for TopLevel "VkPhysicalDeviceMemoryProperties2"
data VkPhysicalDeviceMemoryProperties2 = VkPhysicalDeviceMemoryProperties2
  { -- No documentation found for Nested "VkPhysicalDeviceMemoryProperties2" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceMemoryProperties2" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceMemoryProperties2" "memoryProperties"
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
-- No documentation found for TopLevel "VkPhysicalDeviceProperties2"
data VkPhysicalDeviceProperties2 = VkPhysicalDeviceProperties2
  { -- No documentation found for Nested "VkPhysicalDeviceProperties2" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceProperties2" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceProperties2" "properties"
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
-- No documentation found for TopLevel "VkPhysicalDeviceSparseImageFormatInfo2"
data VkPhysicalDeviceSparseImageFormatInfo2 = VkPhysicalDeviceSparseImageFormatInfo2
  { -- No documentation found for Nested "VkPhysicalDeviceSparseImageFormatInfo2" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceSparseImageFormatInfo2" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceSparseImageFormatInfo2" "format"
  vkFormat :: VkFormat
  , -- No documentation found for Nested "VkPhysicalDeviceSparseImageFormatInfo2" "type"
  vkType :: VkImageType
  , -- No documentation found for Nested "VkPhysicalDeviceSparseImageFormatInfo2" "samples"
  vkSamples :: VkSampleCountFlagBits
  , -- No documentation found for Nested "VkPhysicalDeviceSparseImageFormatInfo2" "usage"
  vkUsage :: VkImageUsageFlags
  , -- No documentation found for Nested "VkPhysicalDeviceSparseImageFormatInfo2" "tiling"
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
-- No documentation found for TopLevel "VkQueueFamilyProperties2"
data VkQueueFamilyProperties2 = VkQueueFamilyProperties2
  { -- No documentation found for Nested "VkQueueFamilyProperties2" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkQueueFamilyProperties2" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkQueueFamilyProperties2" "queueFamilyProperties"
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
-- No documentation found for TopLevel "VkSparseImageFormatProperties2"
data VkSparseImageFormatProperties2 = VkSparseImageFormatProperties2
  { -- No documentation found for Nested "VkSparseImageFormatProperties2" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkSparseImageFormatProperties2" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkSparseImageFormatProperties2" "properties"
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
#if defined(EXPOSE_CORE11_COMMANDS)
-- No documentation found for TopLevel "vkGetPhysicalDeviceFeatures2"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceFeatures2" vkGetPhysicalDeviceFeatures2 :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkPhysicalDeviceFeatures2) -> IO ()

#endif
type FN_vkGetPhysicalDeviceFeatures2 = ("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkPhysicalDeviceFeatures2) -> IO ()
type PFN_vkGetPhysicalDeviceFeatures2 = FunPtr FN_vkGetPhysicalDeviceFeatures2
#if defined(EXPOSE_CORE11_COMMANDS)
-- No documentation found for TopLevel "vkGetPhysicalDeviceFormatProperties2"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceFormatProperties2" vkGetPhysicalDeviceFormatProperties2 :: ("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("pFormatProperties" ::: Ptr VkFormatProperties2) -> IO ()

#endif
type FN_vkGetPhysicalDeviceFormatProperties2 = ("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("pFormatProperties" ::: Ptr VkFormatProperties2) -> IO ()
type PFN_vkGetPhysicalDeviceFormatProperties2 = FunPtr FN_vkGetPhysicalDeviceFormatProperties2
#if defined(EXPOSE_CORE11_COMMANDS)
-- No documentation found for TopLevel "vkGetPhysicalDeviceImageFormatProperties2"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceImageFormatProperties2" vkGetPhysicalDeviceImageFormatProperties2 :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pImageFormatInfo" ::: Ptr VkPhysicalDeviceImageFormatInfo2) -> ("pImageFormatProperties" ::: Ptr VkImageFormatProperties2) -> IO VkResult

#endif
type FN_vkGetPhysicalDeviceImageFormatProperties2 = ("physicalDevice" ::: VkPhysicalDevice) -> ("pImageFormatInfo" ::: Ptr VkPhysicalDeviceImageFormatInfo2) -> ("pImageFormatProperties" ::: Ptr VkImageFormatProperties2) -> IO VkResult
type PFN_vkGetPhysicalDeviceImageFormatProperties2 = FunPtr FN_vkGetPhysicalDeviceImageFormatProperties2
#if defined(EXPOSE_CORE11_COMMANDS)
-- No documentation found for TopLevel "vkGetPhysicalDeviceMemoryProperties2"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceMemoryProperties2" vkGetPhysicalDeviceMemoryProperties2 :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pMemoryProperties" ::: Ptr VkPhysicalDeviceMemoryProperties2) -> IO ()

#endif
type FN_vkGetPhysicalDeviceMemoryProperties2 = ("physicalDevice" ::: VkPhysicalDevice) -> ("pMemoryProperties" ::: Ptr VkPhysicalDeviceMemoryProperties2) -> IO ()
type PFN_vkGetPhysicalDeviceMemoryProperties2 = FunPtr FN_vkGetPhysicalDeviceMemoryProperties2
#if defined(EXPOSE_CORE11_COMMANDS)
-- No documentation found for TopLevel "vkGetPhysicalDeviceProperties2"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceProperties2" vkGetPhysicalDeviceProperties2 :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pProperties" ::: Ptr VkPhysicalDeviceProperties2) -> IO ()

#endif
type FN_vkGetPhysicalDeviceProperties2 = ("physicalDevice" ::: VkPhysicalDevice) -> ("pProperties" ::: Ptr VkPhysicalDeviceProperties2) -> IO ()
type PFN_vkGetPhysicalDeviceProperties2 = FunPtr FN_vkGetPhysicalDeviceProperties2
#if defined(EXPOSE_CORE11_COMMANDS)
-- No documentation found for TopLevel "vkGetPhysicalDeviceQueueFamilyProperties2"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceQueueFamilyProperties2" vkGetPhysicalDeviceQueueFamilyProperties2 :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pQueueFamilyPropertyCount" ::: Ptr Word32) -> ("pQueueFamilyProperties" ::: Ptr VkQueueFamilyProperties2) -> IO ()

#endif
type FN_vkGetPhysicalDeviceQueueFamilyProperties2 = ("physicalDevice" ::: VkPhysicalDevice) -> ("pQueueFamilyPropertyCount" ::: Ptr Word32) -> ("pQueueFamilyProperties" ::: Ptr VkQueueFamilyProperties2) -> IO ()
type PFN_vkGetPhysicalDeviceQueueFamilyProperties2 = FunPtr FN_vkGetPhysicalDeviceQueueFamilyProperties2
#if defined(EXPOSE_CORE11_COMMANDS)
-- No documentation found for TopLevel "vkGetPhysicalDeviceSparseImageFormatProperties2"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceSparseImageFormatProperties2" vkGetPhysicalDeviceSparseImageFormatProperties2 :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pFormatInfo" ::: Ptr VkPhysicalDeviceSparseImageFormatInfo2) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkSparseImageFormatProperties2) -> IO ()

#endif
type FN_vkGetPhysicalDeviceSparseImageFormatProperties2 = ("physicalDevice" ::: VkPhysicalDevice) -> ("pFormatInfo" ::: Ptr VkPhysicalDeviceSparseImageFormatInfo2) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkSparseImageFormatProperties2) -> IO ()
type PFN_vkGetPhysicalDeviceSparseImageFormatProperties2 = FunPtr FN_vkGetPhysicalDeviceSparseImageFormatProperties2
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2"
pattern VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2 = VkStructureType 1000059002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2"
pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2 = VkStructureType 1000059003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2 = VkStructureType 1000059000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2 = VkStructureType 1000059004
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2 = VkStructureType 1000059006
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2 = VkStructureType 1000059001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2 = VkStructureType 1000059008
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2"
pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2 = VkStructureType 1000059005
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2"
pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2 = VkStructureType 1000059007
