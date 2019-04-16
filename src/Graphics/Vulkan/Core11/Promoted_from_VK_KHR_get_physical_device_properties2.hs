{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2
  ( withCStructFormatProperties2
  , fromCStructFormatProperties2
  , FormatProperties2(..)
  , withCStructImageFormatProperties2
  , fromCStructImageFormatProperties2
  , ImageFormatProperties2(..)
  , withCStructPhysicalDeviceFeatures2
  , fromCStructPhysicalDeviceFeatures2
  , PhysicalDeviceFeatures2(..)
  , withCStructPhysicalDeviceImageFormatInfo2
  , fromCStructPhysicalDeviceImageFormatInfo2
  , PhysicalDeviceImageFormatInfo2(..)
  , withCStructPhysicalDeviceMemoryProperties2
  , fromCStructPhysicalDeviceMemoryProperties2
  , PhysicalDeviceMemoryProperties2(..)
  , withCStructPhysicalDeviceProperties2
  , fromCStructPhysicalDeviceProperties2
  , PhysicalDeviceProperties2(..)
  , withCStructPhysicalDeviceSparseImageFormatInfo2
  , fromCStructPhysicalDeviceSparseImageFormatInfo2
  , PhysicalDeviceSparseImageFormatInfo2(..)
  , withCStructQueueFamilyProperties2
  , fromCStructQueueFamilyProperties2
  , QueueFamilyProperties2(..)
  , withCStructSparseImageFormatProperties2
  , fromCStructSparseImageFormatProperties2
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
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2
  , pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( (<=<)
  , when
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( generateM
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Array
  ( allocaArray
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  , nullPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( getPhysicalDeviceFeatures2
  , getPhysicalDeviceFormatProperties2
  , getPhysicalDeviceImageFormatProperties2
  , getPhysicalDeviceMemoryProperties2
  , getPhysicalDeviceProperties2
  , getPhysicalDeviceQueueFamilyProperties2
  , getPhysicalDeviceSparseImageFormatProperties2
  )


import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2
  ( VkFormatProperties2(..)
  , VkImageFormatProperties2(..)
  , VkPhysicalDeviceFeatures2(..)
  , VkPhysicalDeviceImageFormatInfo2(..)
  , VkPhysicalDeviceMemoryProperties2(..)
  , VkPhysicalDeviceProperties2(..)
  , VkPhysicalDeviceSparseImageFormatInfo2(..)
  , VkQueueFamilyProperties2(..)
  , VkSparseImageFormatProperties2(..)
  , pattern VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2
  , pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2
  )
import Graphics.Vulkan.Core10.Core
  ( Format
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( FormatProperties(..)
  , ImageFormatProperties(..)
  , PhysicalDevice(..)
  , PhysicalDeviceFeatures(..)
  , PhysicalDeviceMemoryProperties(..)
  , PhysicalDeviceProperties(..)
  , QueueFamilyProperties(..)
  , ImageCreateFlags
  , ImageTiling
  , ImageType
  , ImageUsageFlags
  , SampleCountFlagBits
  , fromCStructFormatProperties
  , fromCStructImageFormatProperties
  , fromCStructPhysicalDeviceFeatures
  , fromCStructPhysicalDeviceMemoryProperties
  , fromCStructPhysicalDeviceProperties
  , fromCStructQueueFamilyProperties
  , withCStructFormatProperties
  , withCStructImageFormatProperties
  , withCStructPhysicalDeviceFeatures
  , withCStructPhysicalDeviceMemoryProperties
  , withCStructPhysicalDeviceProperties
  , withCStructQueueFamilyProperties
  )
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( SparseImageFormatProperties(..)
  , fromCStructSparseImageFormatProperties
  , withCStructSparseImageFormatProperties
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )


-- No documentation found for TopLevel "FormatProperties2"
data FormatProperties2 = FormatProperties2
  { -- Univalued Member elided
  -- No documentation found for Nested "FormatProperties2" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "FormatProperties2" "formatProperties"
  vkFormatProperties :: FormatProperties
  }
  deriving (Show, Eq)
withCStructFormatProperties2 :: FormatProperties2 -> (VkFormatProperties2 -> IO a) -> IO a
withCStructFormatProperties2 from cont = withCStructFormatProperties (vkFormatProperties (from :: FormatProperties2)) (\formatProperties -> maybeWith withSomeVkStruct (vkPNext (from :: FormatProperties2)) (\pPNext -> cont (VkFormatProperties2 VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2 pPNext formatProperties)))
fromCStructFormatProperties2 :: VkFormatProperties2 -> IO FormatProperties2
fromCStructFormatProperties2 c = FormatProperties2 <$> -- Univalued Member elided
                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkFormatProperties2)))
                                                   <*> (fromCStructFormatProperties (vkFormatProperties (c :: VkFormatProperties2)))
-- No documentation found for TopLevel "ImageFormatProperties2"
data ImageFormatProperties2 = ImageFormatProperties2
  { -- Univalued Member elided
  -- No documentation found for Nested "ImageFormatProperties2" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageFormatProperties2" "imageFormatProperties"
  vkImageFormatProperties :: ImageFormatProperties
  }
  deriving (Show, Eq)
withCStructImageFormatProperties2 :: ImageFormatProperties2 -> (VkImageFormatProperties2 -> IO a) -> IO a
withCStructImageFormatProperties2 from cont = withCStructImageFormatProperties (vkImageFormatProperties (from :: ImageFormatProperties2)) (\imageFormatProperties -> maybeWith withSomeVkStruct (vkPNext (from :: ImageFormatProperties2)) (\pPNext -> cont (VkImageFormatProperties2 VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2 pPNext imageFormatProperties)))
fromCStructImageFormatProperties2 :: VkImageFormatProperties2 -> IO ImageFormatProperties2
fromCStructImageFormatProperties2 c = ImageFormatProperties2 <$> -- Univalued Member elided
                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImageFormatProperties2)))
                                                             <*> (fromCStructImageFormatProperties (vkImageFormatProperties (c :: VkImageFormatProperties2)))
-- No documentation found for TopLevel "PhysicalDeviceFeatures2"
data PhysicalDeviceFeatures2 = PhysicalDeviceFeatures2
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceFeatures2" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceFeatures2" "features"
  vkFeatures :: PhysicalDeviceFeatures
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceFeatures2 :: PhysicalDeviceFeatures2 -> (VkPhysicalDeviceFeatures2 -> IO a) -> IO a
withCStructPhysicalDeviceFeatures2 from cont = withCStructPhysicalDeviceFeatures (vkFeatures (from :: PhysicalDeviceFeatures2)) (\features -> maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceFeatures2)) (\pPNext -> cont (VkPhysicalDeviceFeatures2 VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2 pPNext features)))
fromCStructPhysicalDeviceFeatures2 :: VkPhysicalDeviceFeatures2 -> IO PhysicalDeviceFeatures2
fromCStructPhysicalDeviceFeatures2 c = PhysicalDeviceFeatures2 <$> -- Univalued Member elided
                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceFeatures2)))
                                                               <*> (fromCStructPhysicalDeviceFeatures (vkFeatures (c :: VkPhysicalDeviceFeatures2)))
-- No documentation found for TopLevel "PhysicalDeviceImageFormatInfo2"
data PhysicalDeviceImageFormatInfo2 = PhysicalDeviceImageFormatInfo2
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceImageFormatInfo2" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceImageFormatInfo2" "format"
  vkFormat :: Format
  , -- No documentation found for Nested "PhysicalDeviceImageFormatInfo2" "type"
  vkType :: ImageType
  , -- No documentation found for Nested "PhysicalDeviceImageFormatInfo2" "tiling"
  vkTiling :: ImageTiling
  , -- No documentation found for Nested "PhysicalDeviceImageFormatInfo2" "usage"
  vkUsage :: ImageUsageFlags
  , -- No documentation found for Nested "PhysicalDeviceImageFormatInfo2" "flags"
  vkFlags :: ImageCreateFlags
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceImageFormatInfo2 :: PhysicalDeviceImageFormatInfo2 -> (VkPhysicalDeviceImageFormatInfo2 -> IO a) -> IO a
withCStructPhysicalDeviceImageFormatInfo2 from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceImageFormatInfo2)) (\pPNext -> cont (VkPhysicalDeviceImageFormatInfo2 VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2 pPNext (vkFormat (from :: PhysicalDeviceImageFormatInfo2)) (vkType (from :: PhysicalDeviceImageFormatInfo2)) (vkTiling (from :: PhysicalDeviceImageFormatInfo2)) (vkUsage (from :: PhysicalDeviceImageFormatInfo2)) (vkFlags (from :: PhysicalDeviceImageFormatInfo2))))
fromCStructPhysicalDeviceImageFormatInfo2 :: VkPhysicalDeviceImageFormatInfo2 -> IO PhysicalDeviceImageFormatInfo2
fromCStructPhysicalDeviceImageFormatInfo2 c = PhysicalDeviceImageFormatInfo2 <$> -- Univalued Member elided
                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceImageFormatInfo2)))
                                                                             <*> pure (vkFormat (c :: VkPhysicalDeviceImageFormatInfo2))
                                                                             <*> pure (vkType (c :: VkPhysicalDeviceImageFormatInfo2))
                                                                             <*> pure (vkTiling (c :: VkPhysicalDeviceImageFormatInfo2))
                                                                             <*> pure (vkUsage (c :: VkPhysicalDeviceImageFormatInfo2))
                                                                             <*> pure (vkFlags (c :: VkPhysicalDeviceImageFormatInfo2))
-- No documentation found for TopLevel "PhysicalDeviceMemoryProperties2"
data PhysicalDeviceMemoryProperties2 = PhysicalDeviceMemoryProperties2
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceMemoryProperties2" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceMemoryProperties2" "memoryProperties"
  vkMemoryProperties :: PhysicalDeviceMemoryProperties
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceMemoryProperties2 :: PhysicalDeviceMemoryProperties2 -> (VkPhysicalDeviceMemoryProperties2 -> IO a) -> IO a
withCStructPhysicalDeviceMemoryProperties2 from cont = withCStructPhysicalDeviceMemoryProperties (vkMemoryProperties (from :: PhysicalDeviceMemoryProperties2)) (\memoryProperties -> maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceMemoryProperties2)) (\pPNext -> cont (VkPhysicalDeviceMemoryProperties2 VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2 pPNext memoryProperties)))
fromCStructPhysicalDeviceMemoryProperties2 :: VkPhysicalDeviceMemoryProperties2 -> IO PhysicalDeviceMemoryProperties2
fromCStructPhysicalDeviceMemoryProperties2 c = PhysicalDeviceMemoryProperties2 <$> -- Univalued Member elided
                                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceMemoryProperties2)))
                                                                               <*> (fromCStructPhysicalDeviceMemoryProperties (vkMemoryProperties (c :: VkPhysicalDeviceMemoryProperties2)))
-- No documentation found for TopLevel "PhysicalDeviceProperties2"
data PhysicalDeviceProperties2 = PhysicalDeviceProperties2
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceProperties2" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceProperties2" "properties"
  vkProperties :: PhysicalDeviceProperties
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceProperties2 :: PhysicalDeviceProperties2 -> (VkPhysicalDeviceProperties2 -> IO a) -> IO a
withCStructPhysicalDeviceProperties2 from cont = withCStructPhysicalDeviceProperties (vkProperties (from :: PhysicalDeviceProperties2)) (\roperties -> maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceProperties2)) (\pPNext -> cont (VkPhysicalDeviceProperties2 VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2 pPNext roperties)))
fromCStructPhysicalDeviceProperties2 :: VkPhysicalDeviceProperties2 -> IO PhysicalDeviceProperties2
fromCStructPhysicalDeviceProperties2 c = PhysicalDeviceProperties2 <$> -- Univalued Member elided
                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceProperties2)))
                                                                   <*> (fromCStructPhysicalDeviceProperties (vkProperties (c :: VkPhysicalDeviceProperties2)))
-- No documentation found for TopLevel "PhysicalDeviceSparseImageFormatInfo2"
data PhysicalDeviceSparseImageFormatInfo2 = PhysicalDeviceSparseImageFormatInfo2
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceSparseImageFormatInfo2" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceSparseImageFormatInfo2" "format"
  vkFormat :: Format
  , -- No documentation found for Nested "PhysicalDeviceSparseImageFormatInfo2" "type"
  vkType :: ImageType
  , -- No documentation found for Nested "PhysicalDeviceSparseImageFormatInfo2" "samples"
  vkSamples :: SampleCountFlagBits
  , -- No documentation found for Nested "PhysicalDeviceSparseImageFormatInfo2" "usage"
  vkUsage :: ImageUsageFlags
  , -- No documentation found for Nested "PhysicalDeviceSparseImageFormatInfo2" "tiling"
  vkTiling :: ImageTiling
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceSparseImageFormatInfo2 :: PhysicalDeviceSparseImageFormatInfo2 -> (VkPhysicalDeviceSparseImageFormatInfo2 -> IO a) -> IO a
withCStructPhysicalDeviceSparseImageFormatInfo2 from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceSparseImageFormatInfo2)) (\pPNext -> cont (VkPhysicalDeviceSparseImageFormatInfo2 VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2 pPNext (vkFormat (from :: PhysicalDeviceSparseImageFormatInfo2)) (vkType (from :: PhysicalDeviceSparseImageFormatInfo2)) (vkSamples (from :: PhysicalDeviceSparseImageFormatInfo2)) (vkUsage (from :: PhysicalDeviceSparseImageFormatInfo2)) (vkTiling (from :: PhysicalDeviceSparseImageFormatInfo2))))
fromCStructPhysicalDeviceSparseImageFormatInfo2 :: VkPhysicalDeviceSparseImageFormatInfo2 -> IO PhysicalDeviceSparseImageFormatInfo2
fromCStructPhysicalDeviceSparseImageFormatInfo2 c = PhysicalDeviceSparseImageFormatInfo2 <$> -- Univalued Member elided
                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceSparseImageFormatInfo2)))
                                                                                         <*> pure (vkFormat (c :: VkPhysicalDeviceSparseImageFormatInfo2))
                                                                                         <*> pure (vkType (c :: VkPhysicalDeviceSparseImageFormatInfo2))
                                                                                         <*> pure (vkSamples (c :: VkPhysicalDeviceSparseImageFormatInfo2))
                                                                                         <*> pure (vkUsage (c :: VkPhysicalDeviceSparseImageFormatInfo2))
                                                                                         <*> pure (vkTiling (c :: VkPhysicalDeviceSparseImageFormatInfo2))
-- No documentation found for TopLevel "QueueFamilyProperties2"
data QueueFamilyProperties2 = QueueFamilyProperties2
  { -- Univalued Member elided
  -- No documentation found for Nested "QueueFamilyProperties2" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "QueueFamilyProperties2" "queueFamilyProperties"
  vkQueueFamilyProperties :: QueueFamilyProperties
  }
  deriving (Show, Eq)
withCStructQueueFamilyProperties2 :: QueueFamilyProperties2 -> (VkQueueFamilyProperties2 -> IO a) -> IO a
withCStructQueueFamilyProperties2 from cont = withCStructQueueFamilyProperties (vkQueueFamilyProperties (from :: QueueFamilyProperties2)) (\queueFamilyProperties -> maybeWith withSomeVkStruct (vkPNext (from :: QueueFamilyProperties2)) (\pPNext -> cont (VkQueueFamilyProperties2 VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2 pPNext queueFamilyProperties)))
fromCStructQueueFamilyProperties2 :: VkQueueFamilyProperties2 -> IO QueueFamilyProperties2
fromCStructQueueFamilyProperties2 c = QueueFamilyProperties2 <$> -- Univalued Member elided
                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkQueueFamilyProperties2)))
                                                             <*> (fromCStructQueueFamilyProperties (vkQueueFamilyProperties (c :: VkQueueFamilyProperties2)))
-- No documentation found for TopLevel "SparseImageFormatProperties2"
data SparseImageFormatProperties2 = SparseImageFormatProperties2
  { -- Univalued Member elided
  -- No documentation found for Nested "SparseImageFormatProperties2" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SparseImageFormatProperties2" "properties"
  vkProperties :: SparseImageFormatProperties
  }
  deriving (Show, Eq)
withCStructSparseImageFormatProperties2 :: SparseImageFormatProperties2 -> (VkSparseImageFormatProperties2 -> IO a) -> IO a
withCStructSparseImageFormatProperties2 from cont = withCStructSparseImageFormatProperties (vkProperties (from :: SparseImageFormatProperties2)) (\roperties -> maybeWith withSomeVkStruct (vkPNext (from :: SparseImageFormatProperties2)) (\pPNext -> cont (VkSparseImageFormatProperties2 VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2 pPNext roperties)))
fromCStructSparseImageFormatProperties2 :: VkSparseImageFormatProperties2 -> IO SparseImageFormatProperties2
fromCStructSparseImageFormatProperties2 c = SparseImageFormatProperties2 <$> -- Univalued Member elided
                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSparseImageFormatProperties2)))
                                                                         <*> (fromCStructSparseImageFormatProperties (vkProperties (c :: VkSparseImageFormatProperties2)))

-- | Wrapper for 'vkGetPhysicalDeviceFeatures2'
getPhysicalDeviceFeatures2 :: PhysicalDevice ->  IO (PhysicalDeviceFeatures2)
getPhysicalDeviceFeatures2 = \(PhysicalDevice physicalDevice commandTable) -> alloca (\pFeatures -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceFeatures2 commandTable physicalDevice pFeatures *> ((fromCStructPhysicalDeviceFeatures2 <=< peek) pFeatures))

-- | Wrapper for 'vkGetPhysicalDeviceFormatProperties2'
getPhysicalDeviceFormatProperties2 :: PhysicalDevice ->  Format ->  IO (FormatProperties2)
getPhysicalDeviceFormatProperties2 = \(PhysicalDevice physicalDevice commandTable) -> \format -> alloca (\pFormatProperties -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceFormatProperties2 commandTable physicalDevice format pFormatProperties *> ((fromCStructFormatProperties2 <=< peek) pFormatProperties))

-- | Wrapper for 'vkGetPhysicalDeviceImageFormatProperties2'
getPhysicalDeviceImageFormatProperties2 :: PhysicalDevice ->  PhysicalDeviceImageFormatInfo2 ->  IO ( ImageFormatProperties2 )
getPhysicalDeviceImageFormatProperties2 = \(PhysicalDevice physicalDevice commandTable) -> \imageFormatInfo -> alloca (\pImageFormatProperties -> (\a -> withCStructPhysicalDeviceImageFormatInfo2 a . flip with) imageFormatInfo (\pImageFormatInfo -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceImageFormatProperties2 commandTable physicalDevice pImageFormatInfo pImageFormatProperties >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((fromCStructImageFormatProperties2 <=< peek) pImageFormatProperties))))

-- | Wrapper for 'vkGetPhysicalDeviceMemoryProperties2'
getPhysicalDeviceMemoryProperties2 :: PhysicalDevice ->  IO (PhysicalDeviceMemoryProperties2)
getPhysicalDeviceMemoryProperties2 = \(PhysicalDevice physicalDevice commandTable) -> alloca (\pMemoryProperties -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceMemoryProperties2 commandTable physicalDevice pMemoryProperties *> ((fromCStructPhysicalDeviceMemoryProperties2 <=< peek) pMemoryProperties))

-- | Wrapper for 'vkGetPhysicalDeviceProperties2'
getPhysicalDeviceProperties2 :: PhysicalDevice ->  IO (PhysicalDeviceProperties2)
getPhysicalDeviceProperties2 = \(PhysicalDevice physicalDevice commandTable) -> alloca (\pProperties -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceProperties2 commandTable physicalDevice pProperties *> ((fromCStructPhysicalDeviceProperties2 <=< peek) pProperties))

-- | Wrapper for 'vkGetPhysicalDeviceQueueFamilyProperties2'
getNumPhysicalDeviceQueueFamilyProperties2 :: PhysicalDevice ->  IO (Word32)
getNumPhysicalDeviceQueueFamilyProperties2 = \(PhysicalDevice physicalDevice commandTable) -> alloca (\pQueueFamilyPropertyCount -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceQueueFamilyProperties2 commandTable physicalDevice pQueueFamilyPropertyCount nullPtr *> (peek pQueueFamilyPropertyCount))

-- | Wrapper for 'vkGetPhysicalDeviceQueueFamilyProperties2'
getPhysicalDeviceQueueFamilyProperties2 :: PhysicalDevice ->  Word32 ->  IO (Vector QueueFamilyProperties2)
getPhysicalDeviceQueueFamilyProperties2 = \(PhysicalDevice physicalDevice commandTable) -> \queueFamilyPropertyCount -> allocaArray (fromIntegral queueFamilyPropertyCount) (\pQueueFamilyProperties -> with queueFamilyPropertyCount (\pQueueFamilyPropertyCount -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceQueueFamilyProperties2 commandTable physicalDevice pQueueFamilyPropertyCount pQueueFamilyProperties *> ((flip Data.Vector.generateM ((\p -> fromCStructQueueFamilyProperties2 <=< peekElemOff p) pQueueFamilyProperties) =<< (fromIntegral <$> (peek pQueueFamilyPropertyCount))))))
-- | Call 'getNumPhysicalDeviceQueueFamilyProperties2' to get the number of return values, then use that
-- number to call 'getPhysicalDeviceQueueFamilyProperties2' to get all the values.
getAllPhysicalDeviceQueueFamilyProperties2 :: PhysicalDevice ->  IO (Vector QueueFamilyProperties2)
getAllPhysicalDeviceQueueFamilyProperties2 physicalDevice =
  getNumPhysicalDeviceQueueFamilyProperties2 physicalDevice
    >>= \num -> getPhysicalDeviceQueueFamilyProperties2 physicalDevice num


-- | Wrapper for 'vkGetPhysicalDeviceSparseImageFormatProperties2'
getNumPhysicalDeviceSparseImageFormatProperties2 :: PhysicalDevice ->  PhysicalDeviceSparseImageFormatInfo2 ->  IO (Word32)
getNumPhysicalDeviceSparseImageFormatProperties2 = \(PhysicalDevice physicalDevice commandTable) -> \formatInfo -> alloca (\pPropertyCount -> (\a -> withCStructPhysicalDeviceSparseImageFormatInfo2 a . flip with) formatInfo (\pFormatInfo -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceSparseImageFormatProperties2 commandTable physicalDevice pFormatInfo pPropertyCount nullPtr *> (peek pPropertyCount)))

-- | Wrapper for 'vkGetPhysicalDeviceSparseImageFormatProperties2'
getPhysicalDeviceSparseImageFormatProperties2 :: PhysicalDevice ->  PhysicalDeviceSparseImageFormatInfo2 ->  Word32 ->  IO ( Vector SparseImageFormatProperties2 )
getPhysicalDeviceSparseImageFormatProperties2 = \(PhysicalDevice physicalDevice commandTable) -> \formatInfo -> \propertyCount -> allocaArray (fromIntegral propertyCount) (\pProperties -> with propertyCount (\pPropertyCount -> (\a -> withCStructPhysicalDeviceSparseImageFormatInfo2 a . flip with) formatInfo (\pFormatInfo -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceSparseImageFormatProperties2 commandTable physicalDevice pFormatInfo pPropertyCount pProperties *> ((flip Data.Vector.generateM ((\p -> fromCStructSparseImageFormatProperties2 <=< peekElemOff p) pProperties) =<< (fromIntegral <$> (peek pPropertyCount)))))))
-- | Call 'getNumPhysicalDeviceSparseImageFormatProperties2' to get the number of return values, then use that
-- number to call 'getPhysicalDeviceSparseImageFormatProperties2' to get all the values.
getAllPhysicalDeviceSparseImageFormatProperties2 :: PhysicalDevice ->  PhysicalDeviceSparseImageFormatInfo2 ->  IO (Vector SparseImageFormatProperties2)
getAllPhysicalDeviceSparseImageFormatProperties2 physicalDevice pFormatInfo =
  getNumPhysicalDeviceSparseImageFormatProperties2 physicalDevice pFormatInfo
    >>= \num -> getPhysicalDeviceSparseImageFormatProperties2 physicalDevice pFormatInfo num

