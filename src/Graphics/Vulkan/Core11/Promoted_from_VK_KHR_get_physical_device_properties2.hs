{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  FormatProperties2(..)
  , 
  ImageFormatProperties2(..)
  , PhysicalDeviceFeatures2(..)
  , PhysicalDeviceImageFormatInfo2(..)
  , PhysicalDeviceMemoryProperties2(..)
  , PhysicalDeviceProperties2(..)
  , PhysicalDeviceSparseImageFormatInfo2(..)
  , QueueFamilyProperties2(..)
  , SparseImageFormatProperties2(..)
  , getPhysicalDeviceFeatures2
  , getPhysicalDeviceFormatProperties2
  , getPhysicalDeviceImageFormatProperties2
  , getPhysicalDeviceMemoryProperties2
  , getPhysicalDeviceProperties2
  , getNumPhysicalDeviceQueueFamilyProperties2
  , getPhysicalDeviceQueueFamilyProperties2
  , getAllPhysicalDeviceQueueFamilyProperties2
  , getNumPhysicalDeviceSparseImageFormatProperties2
  , getPhysicalDeviceSparseImageFormatProperties2
  , getAllPhysicalDeviceSparseImageFormatProperties2
#endif
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2
  , pattern STRUCTURE_TYPE_FORMAT_PROPERTIES_2
  , pattern STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2
  , pattern STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2
  , pattern STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2
  ) where


#if defined(VK_USE_PLATFORM_GGP)
import Control.Monad
  ( (<=<)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Data.Vector
  ( Vector
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import qualified Data.Vector
  ( generateM
  )
#endif

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
import Foreign.Marshal.Array
  ( allocaArray
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Marshal.Utils
  ( with
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Ptr
  ( nullPtr
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Storable
  ( peekElemOff
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

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2
  ( vkGetPhysicalDeviceFeatures2
  , vkGetPhysicalDeviceFormatProperties2
  , vkGetPhysicalDeviceImageFormatProperties2
  , vkGetPhysicalDeviceMemoryProperties2
  , vkGetPhysicalDeviceProperties2
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2
  ( vkGetPhysicalDeviceQueueFamilyProperties2
  , vkGetPhysicalDeviceSparseImageFormatProperties2
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Core
  ( Format
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DeviceInitialization
  ( FormatProperties(..)
  , ImageFormatProperties(..)
  , PhysicalDeviceFeatures(..)
  , PhysicalDeviceMemoryProperties(..)
  , PhysicalDeviceProperties(..)
  , QueueFamilyProperties(..)
  , ImageCreateFlags
  , SampleCountFlagBits
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DeviceInitialization
  ( ImageTiling
  , ImageType
  , ImageUsageFlags
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DeviceInitialization
  ( PhysicalDevice(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( SparseImageFormatProperties(..)
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
  ( pattern STRUCTURE_TYPE_FORMAT_PROPERTIES_2
  , pattern STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2
  , pattern STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2
  , pattern STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkFormatProperties2"
data FormatProperties2 = FormatProperties2
  { -- No documentation found for Nested "FormatProperties2" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "FormatProperties2" "formatProperties"
  formatProperties :: FormatProperties
  }
  deriving (Show, Eq)

instance Zero FormatProperties2 where
  zero = FormatProperties2 Nothing
                           zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkImageFormatProperties2"
data ImageFormatProperties2 = ImageFormatProperties2
  { -- No documentation found for Nested "ImageFormatProperties2" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageFormatProperties2" "imageFormatProperties"
  imageFormatProperties :: ImageFormatProperties
  }
  deriving (Show, Eq)

instance Zero ImageFormatProperties2 where
  zero = ImageFormatProperties2 Nothing
                                zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceFeatures2"
data PhysicalDeviceFeatures2 = PhysicalDeviceFeatures2
  { -- No documentation found for Nested "PhysicalDeviceFeatures2" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceFeatures2" "features"
  features :: PhysicalDeviceFeatures
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceFeatures2 where
  zero = PhysicalDeviceFeatures2 Nothing
                                 zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceImageFormatInfo2"
data PhysicalDeviceImageFormatInfo2 = PhysicalDeviceImageFormatInfo2
  { -- No documentation found for Nested "PhysicalDeviceImageFormatInfo2" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceImageFormatInfo2" "format"
  format :: Format
  , -- No documentation found for Nested "PhysicalDeviceImageFormatInfo2" "type"
  type' :: ImageType
  , -- No documentation found for Nested "PhysicalDeviceImageFormatInfo2" "tiling"
  tiling :: ImageTiling
  , -- No documentation found for Nested "PhysicalDeviceImageFormatInfo2" "usage"
  usage :: ImageUsageFlags
  , -- No documentation found for Nested "PhysicalDeviceImageFormatInfo2" "flags"
  flags :: ImageCreateFlags
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceImageFormatInfo2 where
  zero = PhysicalDeviceImageFormatInfo2 Nothing
                                        zero
                                        zero
                                        zero
                                        zero
                                        zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceMemoryProperties2"
data PhysicalDeviceMemoryProperties2 = PhysicalDeviceMemoryProperties2
  { -- No documentation found for Nested "PhysicalDeviceMemoryProperties2" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceMemoryProperties2" "memoryProperties"
  memoryProperties :: PhysicalDeviceMemoryProperties
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceMemoryProperties2 where
  zero = PhysicalDeviceMemoryProperties2 Nothing
                                         zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceProperties2"
data PhysicalDeviceProperties2 = PhysicalDeviceProperties2
  { -- No documentation found for Nested "PhysicalDeviceProperties2" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceProperties2" "properties"
  properties :: PhysicalDeviceProperties
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceProperties2 where
  zero = PhysicalDeviceProperties2 Nothing
                                   zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceSparseImageFormatInfo2"
data PhysicalDeviceSparseImageFormatInfo2 = PhysicalDeviceSparseImageFormatInfo2
  { -- No documentation found for Nested "PhysicalDeviceSparseImageFormatInfo2" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceSparseImageFormatInfo2" "format"
  format :: Format
  , -- No documentation found for Nested "PhysicalDeviceSparseImageFormatInfo2" "type"
  type' :: ImageType
  , -- No documentation found for Nested "PhysicalDeviceSparseImageFormatInfo2" "samples"
  samples :: SampleCountFlagBits
  , -- No documentation found for Nested "PhysicalDeviceSparseImageFormatInfo2" "usage"
  usage :: ImageUsageFlags
  , -- No documentation found for Nested "PhysicalDeviceSparseImageFormatInfo2" "tiling"
  tiling :: ImageTiling
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceSparseImageFormatInfo2 where
  zero = PhysicalDeviceSparseImageFormatInfo2 Nothing
                                              zero
                                              zero
                                              zero
                                              zero
                                              zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkQueueFamilyProperties2"
data QueueFamilyProperties2 = QueueFamilyProperties2
  { -- No documentation found for Nested "QueueFamilyProperties2" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "QueueFamilyProperties2" "queueFamilyProperties"
  queueFamilyProperties :: QueueFamilyProperties
  }
  deriving (Show, Eq)

instance Zero QueueFamilyProperties2 where
  zero = QueueFamilyProperties2 Nothing
                                zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkSparseImageFormatProperties2"
data SparseImageFormatProperties2 = SparseImageFormatProperties2
  { -- No documentation found for Nested "SparseImageFormatProperties2" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SparseImageFormatProperties2" "properties"
  properties :: SparseImageFormatProperties
  }
  deriving (Show, Eq)

instance Zero SparseImageFormatProperties2 where
  zero = SparseImageFormatProperties2 Nothing
                                      zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetPhysicalDeviceFeatures2"
getPhysicalDeviceFeatures2 :: PhysicalDevice ->  IO (PhysicalDeviceFeatures2)
getPhysicalDeviceFeatures2 = undefined {- {wrapped (pretty cName) :: Doc ()} -}
#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetPhysicalDeviceFormatProperties2"
getPhysicalDeviceFormatProperties2 :: PhysicalDevice ->  Format ->  IO (FormatProperties2)
getPhysicalDeviceFormatProperties2 = undefined {- {wrapped (pretty cName) :: Doc ()} -}
#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetPhysicalDeviceImageFormatProperties2"
getPhysicalDeviceImageFormatProperties2 :: PhysicalDevice ->  PhysicalDeviceImageFormatInfo2 ->  IO (ImageFormatProperties2)
getPhysicalDeviceImageFormatProperties2 = undefined {- {wrapped (pretty cName) :: Doc ()} -}
#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetPhysicalDeviceMemoryProperties2"
getPhysicalDeviceMemoryProperties2 :: PhysicalDevice ->  IO (PhysicalDeviceMemoryProperties2)
getPhysicalDeviceMemoryProperties2 = undefined {- {wrapped (pretty cName) :: Doc ()} -}
#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetPhysicalDeviceProperties2"
getPhysicalDeviceProperties2 :: PhysicalDevice ->  IO (PhysicalDeviceProperties2)
getPhysicalDeviceProperties2 = undefined {- {wrapped (pretty cName) :: Doc ()} -}
#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetPhysicalDeviceQueueFamilyProperties2"
getNumPhysicalDeviceQueueFamilyProperties2 :: PhysicalDevice ->  IO (Word32)
getNumPhysicalDeviceQueueFamilyProperties2 = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "vkGetPhysicalDeviceQueueFamilyProperties2"
getPhysicalDeviceQueueFamilyProperties2 :: PhysicalDevice ->  Word32 ->  IO (Vector QueueFamilyProperties2)
getPhysicalDeviceQueueFamilyProperties2 = undefined {- {wrapped (pretty cName) :: Doc ()} -}
-- | Returns all the values available from 'getPhysicalDeviceQueueFamilyProperties2'.
getAllPhysicalDeviceQueueFamilyProperties2 :: PhysicalDevice ->  IO (Vector QueueFamilyProperties2)
getAllPhysicalDeviceQueueFamilyProperties2 physicalDevice' =
  getNumPhysicalDeviceQueueFamilyProperties2 physicalDevice'
    >>= \num -> getPhysicalDeviceQueueFamilyProperties2 physicalDevice' num

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetPhysicalDeviceSparseImageFormatProperties2"
getNumPhysicalDeviceSparseImageFormatProperties2 :: PhysicalDevice ->  PhysicalDeviceSparseImageFormatInfo2 ->  IO (Word32)
getNumPhysicalDeviceSparseImageFormatProperties2 = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "vkGetPhysicalDeviceSparseImageFormatProperties2"
getPhysicalDeviceSparseImageFormatProperties2 :: PhysicalDevice ->  PhysicalDeviceSparseImageFormatInfo2 ->  Word32 ->  IO (Vector SparseImageFormatProperties2)
getPhysicalDeviceSparseImageFormatProperties2 = undefined {- {wrapped (pretty cName) :: Doc ()} -}
-- | Returns all the values available from 'getPhysicalDeviceSparseImageFormatProperties2'.
getAllPhysicalDeviceSparseImageFormatProperties2 :: PhysicalDevice ->  PhysicalDeviceSparseImageFormatInfo2 ->  IO (Vector SparseImageFormatProperties2)
getAllPhysicalDeviceSparseImageFormatProperties2 physicalDevice' pFormatInfo' =
  getNumPhysicalDeviceSparseImageFormatProperties2 physicalDevice' pFormatInfo'
    >>= \num -> getPhysicalDeviceSparseImageFormatProperties2 physicalDevice' pFormatInfo' num

#endif
