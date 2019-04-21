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


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_SUCCESS
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
  , vkGetPhysicalDeviceFeatures2
  , vkGetPhysicalDeviceFormatProperties2
  , vkGetPhysicalDeviceImageFormatProperties2
  , vkGetPhysicalDeviceMemoryProperties2
  , vkGetPhysicalDeviceProperties2
  , vkGetPhysicalDeviceQueueFamilyProperties2
  , vkGetPhysicalDeviceSparseImageFormatProperties2
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



-- | VkFormatProperties2 - Structure specifying image format properties
--
-- = Description
--
-- Unresolved directive in VkFormatProperties2.txt -
-- include::{generated}\/validity\/structs\/VkFormatProperties2.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkFormatProperties',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceFormatProperties2'
data FormatProperties2 = FormatProperties2
  { -- Univalued member elided
  -- No documentation found for Nested "FormatProperties2" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "FormatProperties2" "formatProperties"
  formatProperties :: FormatProperties
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkFormatProperties2' and
-- marshal a 'FormatProperties2' into it. The 'VkFormatProperties2' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructFormatProperties2 :: FormatProperties2 -> (VkFormatProperties2 -> IO a) -> IO a
withCStructFormatProperties2 marshalled cont = withCStructFormatProperties (formatProperties (marshalled :: FormatProperties2)) (\formatProperties'' -> maybeWith withSomeVkStruct (next (marshalled :: FormatProperties2)) (\pPNext -> cont (VkFormatProperties2 VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2 pPNext formatProperties'')))

-- | A function to read a 'VkFormatProperties2' and all additional
-- structures in the pointer chain into a 'FormatProperties2'.
fromCStructFormatProperties2 :: VkFormatProperties2 -> IO FormatProperties2
fromCStructFormatProperties2 c = FormatProperties2 <$> -- Univalued Member elided
                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkFormatProperties2)))
                                                   <*> (fromCStructFormatProperties (vkFormatProperties (c :: VkFormatProperties2)))

instance Zero FormatProperties2 where
  zero = FormatProperties2 Nothing
                           zero



-- | VkImageFormatProperties2 - Structure specifying an image format
-- properties
--
-- = Description
--
-- If the combination of parameters to
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceImageFormatProperties2'
-- is not supported by the implementation for use in
-- 'Graphics.Vulkan.C.Core10.Image.vkCreateImage', then all members of
-- @imageFormatProperties@ will be filled with zero.
--
-- __Note__
--
-- Filling @imageFormatProperties@ with zero for unsupported formats is an
-- exception to the usual rule that output structures have undefined
-- contents on error. This exception was unintentional, but is preserved
-- for backwards compatibility. This exeption only applies to
-- @imageFormatProperties@, not @sType@, @pNext@, or any structures chained
-- from @pNext@.
--
-- Unresolved directive in VkImageFormatProperties2.txt -
-- include::{generated}\/validity\/structs\/VkImageFormatProperties2.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageFormatProperties',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceImageFormatProperties2'
data ImageFormatProperties2 = ImageFormatProperties2
  { -- Univalued member elided
  -- No documentation found for Nested "ImageFormatProperties2" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageFormatProperties2" "imageFormatProperties"
  imageFormatProperties :: ImageFormatProperties
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkImageFormatProperties2' and
-- marshal a 'ImageFormatProperties2' into it. The 'VkImageFormatProperties2' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructImageFormatProperties2 :: ImageFormatProperties2 -> (VkImageFormatProperties2 -> IO a) -> IO a
withCStructImageFormatProperties2 marshalled cont = withCStructImageFormatProperties (imageFormatProperties (marshalled :: ImageFormatProperties2)) (\imageFormatProperties'' -> maybeWith withSomeVkStruct (next (marshalled :: ImageFormatProperties2)) (\pPNext -> cont (VkImageFormatProperties2 VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2 pPNext imageFormatProperties'')))

-- | A function to read a 'VkImageFormatProperties2' and all additional
-- structures in the pointer chain into a 'ImageFormatProperties2'.
fromCStructImageFormatProperties2 :: VkImageFormatProperties2 -> IO ImageFormatProperties2
fromCStructImageFormatProperties2 c = ImageFormatProperties2 <$> -- Univalued Member elided
                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImageFormatProperties2)))
                                                             <*> (fromCStructImageFormatProperties (vkImageFormatProperties (c :: VkImageFormatProperties2)))

instance Zero ImageFormatProperties2 where
  zero = ImageFormatProperties2 Nothing
                                zero



-- | VkPhysicalDeviceFeatures2 - Structure describing the fine-grained
-- features that can be supported by an implementation
--
-- = Members
--
-- The
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2'
-- structure is defined as:
--
-- = Description
--
-- The @pNext@ chain of this structure is used to extend the structure with
-- features defined by extensions. This structure /can/ be used in
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceFeatures2'
-- or /can/ be in the @pNext@ chain of a
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' structure, in which
-- case it controls which features are enabled in the device in lieu of
-- @pEnabledFeatures@.
--
-- Unresolved directive in VkPhysicalDeviceFeatures2.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceFeatures2.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceFeatures',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceFeatures2'
data PhysicalDeviceFeatures2 = PhysicalDeviceFeatures2
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceFeatures2" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceFeatures2" "features"
  features :: PhysicalDeviceFeatures
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceFeatures2' and
-- marshal a 'PhysicalDeviceFeatures2' into it. The 'VkPhysicalDeviceFeatures2' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceFeatures2 :: PhysicalDeviceFeatures2 -> (VkPhysicalDeviceFeatures2 -> IO a) -> IO a
withCStructPhysicalDeviceFeatures2 marshalled cont = withCStructPhysicalDeviceFeatures (features (marshalled :: PhysicalDeviceFeatures2)) (\features'' -> maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceFeatures2)) (\pPNext -> cont (VkPhysicalDeviceFeatures2 VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2 pPNext features'')))

-- | A function to read a 'VkPhysicalDeviceFeatures2' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceFeatures2'.
fromCStructPhysicalDeviceFeatures2 :: VkPhysicalDeviceFeatures2 -> IO PhysicalDeviceFeatures2
fromCStructPhysicalDeviceFeatures2 c = PhysicalDeviceFeatures2 <$> -- Univalued Member elided
                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceFeatures2)))
                                                               <*> (fromCStructPhysicalDeviceFeatures (vkFeatures (c :: VkPhysicalDeviceFeatures2)))

instance Zero PhysicalDeviceFeatures2 where
  zero = PhysicalDeviceFeatures2 Nothing
                                 zero



-- | VkPhysicalDeviceImageFormatInfo2 - Structure specifying image creation
-- parameters
--
-- = Description
--
-- The members of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2'
-- correspond to the arguments to
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceImageFormatProperties',
-- with @sType@ and @pNext@ added for extensibility.
--
-- == Valid Usage
--
-- -   @tiling@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier.VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT'
--     if and only if the @pNext@ chain contains
--     'Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier.VkPhysicalDeviceImageDrmFormatModifierInfoEXT'.
--
-- -   If @tiling@ is
--     'Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier.VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT'
--     and @flags@ contains
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT',
--     then the @pNext@ chain /must/ contain
--     'Graphics.Vulkan.C.Extensions.VK_KHR_image_format_list.VkImageFormatListCreateInfoKHR'
--     with non-zero @viewFormatCount@.
--
-- Unresolved directive in VkPhysicalDeviceImageFormatInfo2.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceImageFormatInfo2.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkFormat',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageCreateFlags',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageTiling',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageType',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageUsageFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceImageFormatProperties2'
data PhysicalDeviceImageFormatInfo2 = PhysicalDeviceImageFormatInfo2
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceImageFormatInfo2" "pNext"
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

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceImageFormatInfo2' and
-- marshal a 'PhysicalDeviceImageFormatInfo2' into it. The 'VkPhysicalDeviceImageFormatInfo2' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceImageFormatInfo2 :: PhysicalDeviceImageFormatInfo2 -> (VkPhysicalDeviceImageFormatInfo2 -> IO a) -> IO a
withCStructPhysicalDeviceImageFormatInfo2 marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceImageFormatInfo2)) (\pPNext -> cont (VkPhysicalDeviceImageFormatInfo2 VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2 pPNext (format (marshalled :: PhysicalDeviceImageFormatInfo2)) (type' (marshalled :: PhysicalDeviceImageFormatInfo2)) (tiling (marshalled :: PhysicalDeviceImageFormatInfo2)) (usage (marshalled :: PhysicalDeviceImageFormatInfo2)) (flags (marshalled :: PhysicalDeviceImageFormatInfo2))))

-- | A function to read a 'VkPhysicalDeviceImageFormatInfo2' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceImageFormatInfo2'.
fromCStructPhysicalDeviceImageFormatInfo2 :: VkPhysicalDeviceImageFormatInfo2 -> IO PhysicalDeviceImageFormatInfo2
fromCStructPhysicalDeviceImageFormatInfo2 c = PhysicalDeviceImageFormatInfo2 <$> -- Univalued Member elided
                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceImageFormatInfo2)))
                                                                             <*> pure (vkFormat (c :: VkPhysicalDeviceImageFormatInfo2))
                                                                             <*> pure (vkType (c :: VkPhysicalDeviceImageFormatInfo2))
                                                                             <*> pure (vkTiling (c :: VkPhysicalDeviceImageFormatInfo2))
                                                                             <*> pure (vkUsage (c :: VkPhysicalDeviceImageFormatInfo2))
                                                                             <*> pure (vkFlags (c :: VkPhysicalDeviceImageFormatInfo2))

instance Zero PhysicalDeviceImageFormatInfo2 where
  zero = PhysicalDeviceImageFormatInfo2 Nothing
                                        zero
                                        zero
                                        zero
                                        zero
                                        zero



-- | VkPhysicalDeviceMemoryProperties2 - Structure specifying physical device
-- memory properties
--
-- = Description
--
-- Unresolved directive in VkPhysicalDeviceMemoryProperties2.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceMemoryProperties2.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceMemoryProperties',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceMemoryProperties2'
data PhysicalDeviceMemoryProperties2 = PhysicalDeviceMemoryProperties2
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceMemoryProperties2" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceMemoryProperties2" "memoryProperties"
  memoryProperties :: PhysicalDeviceMemoryProperties
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceMemoryProperties2' and
-- marshal a 'PhysicalDeviceMemoryProperties2' into it. The 'VkPhysicalDeviceMemoryProperties2' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceMemoryProperties2 :: PhysicalDeviceMemoryProperties2 -> (VkPhysicalDeviceMemoryProperties2 -> IO a) -> IO a
withCStructPhysicalDeviceMemoryProperties2 marshalled cont = withCStructPhysicalDeviceMemoryProperties (memoryProperties (marshalled :: PhysicalDeviceMemoryProperties2)) (\memoryProperties'' -> maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceMemoryProperties2)) (\pPNext -> cont (VkPhysicalDeviceMemoryProperties2 VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2 pPNext memoryProperties'')))

-- | A function to read a 'VkPhysicalDeviceMemoryProperties2' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceMemoryProperties2'.
fromCStructPhysicalDeviceMemoryProperties2 :: VkPhysicalDeviceMemoryProperties2 -> IO PhysicalDeviceMemoryProperties2
fromCStructPhysicalDeviceMemoryProperties2 c = PhysicalDeviceMemoryProperties2 <$> -- Univalued Member elided
                                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceMemoryProperties2)))
                                                                               <*> (fromCStructPhysicalDeviceMemoryProperties (vkMemoryProperties (c :: VkPhysicalDeviceMemoryProperties2)))

instance Zero PhysicalDeviceMemoryProperties2 where
  zero = PhysicalDeviceMemoryProperties2 Nothing
                                         zero



-- | VkPhysicalDeviceProperties2 - Structure specifying physical device
-- properties
--
-- = Description
--
-- The @pNext@ chain of this structure is used to extend the structure with
-- properties defined by extensions.
--
-- Unresolved directive in VkPhysicalDeviceProperties2.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceProperties2.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceProperties',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceProperties2'
data PhysicalDeviceProperties2 = PhysicalDeviceProperties2
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceProperties2" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceProperties2" "properties"
  properties :: PhysicalDeviceProperties
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceProperties2' and
-- marshal a 'PhysicalDeviceProperties2' into it. The 'VkPhysicalDeviceProperties2' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceProperties2 :: PhysicalDeviceProperties2 -> (VkPhysicalDeviceProperties2 -> IO a) -> IO a
withCStructPhysicalDeviceProperties2 marshalled cont = withCStructPhysicalDeviceProperties (properties (marshalled :: PhysicalDeviceProperties2)) (\properties'' -> maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceProperties2)) (\pPNext -> cont (VkPhysicalDeviceProperties2 VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2 pPNext properties'')))

-- | A function to read a 'VkPhysicalDeviceProperties2' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceProperties2'.
fromCStructPhysicalDeviceProperties2 :: VkPhysicalDeviceProperties2 -> IO PhysicalDeviceProperties2
fromCStructPhysicalDeviceProperties2 c = PhysicalDeviceProperties2 <$> -- Univalued Member elided
                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceProperties2)))
                                                                   <*> (fromCStructPhysicalDeviceProperties (vkProperties (c :: VkPhysicalDeviceProperties2)))

instance Zero PhysicalDeviceProperties2 where
  zero = PhysicalDeviceProperties2 Nothing
                                   zero



-- | VkPhysicalDeviceSparseImageFormatInfo2 - Structure specifying sparse
-- image format inputs
--
-- == Valid Usage
--
-- Unresolved directive in VkPhysicalDeviceSparseImageFormatInfo2.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceSparseImageFormatInfo2.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkFormat',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageTiling',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageType',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageUsageFlags',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkSampleCountFlagBits',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceSparseImageFormatProperties2'
data PhysicalDeviceSparseImageFormatInfo2 = PhysicalDeviceSparseImageFormatInfo2
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceSparseImageFormatInfo2" "pNext"
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

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceSparseImageFormatInfo2' and
-- marshal a 'PhysicalDeviceSparseImageFormatInfo2' into it. The 'VkPhysicalDeviceSparseImageFormatInfo2' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceSparseImageFormatInfo2 :: PhysicalDeviceSparseImageFormatInfo2 -> (VkPhysicalDeviceSparseImageFormatInfo2 -> IO a) -> IO a
withCStructPhysicalDeviceSparseImageFormatInfo2 marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceSparseImageFormatInfo2)) (\pPNext -> cont (VkPhysicalDeviceSparseImageFormatInfo2 VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2 pPNext (format (marshalled :: PhysicalDeviceSparseImageFormatInfo2)) (type' (marshalled :: PhysicalDeviceSparseImageFormatInfo2)) (samples (marshalled :: PhysicalDeviceSparseImageFormatInfo2)) (usage (marshalled :: PhysicalDeviceSparseImageFormatInfo2)) (tiling (marshalled :: PhysicalDeviceSparseImageFormatInfo2))))

-- | A function to read a 'VkPhysicalDeviceSparseImageFormatInfo2' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceSparseImageFormatInfo2'.
fromCStructPhysicalDeviceSparseImageFormatInfo2 :: VkPhysicalDeviceSparseImageFormatInfo2 -> IO PhysicalDeviceSparseImageFormatInfo2
fromCStructPhysicalDeviceSparseImageFormatInfo2 c = PhysicalDeviceSparseImageFormatInfo2 <$> -- Univalued Member elided
                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceSparseImageFormatInfo2)))
                                                                                         <*> pure (vkFormat (c :: VkPhysicalDeviceSparseImageFormatInfo2))
                                                                                         <*> pure (vkType (c :: VkPhysicalDeviceSparseImageFormatInfo2))
                                                                                         <*> pure (vkSamples (c :: VkPhysicalDeviceSparseImageFormatInfo2))
                                                                                         <*> pure (vkUsage (c :: VkPhysicalDeviceSparseImageFormatInfo2))
                                                                                         <*> pure (vkTiling (c :: VkPhysicalDeviceSparseImageFormatInfo2))

instance Zero PhysicalDeviceSparseImageFormatInfo2 where
  zero = PhysicalDeviceSparseImageFormatInfo2 Nothing
                                              zero
                                              zero
                                              zero
                                              zero
                                              zero



-- | VkQueueFamilyProperties2 - Structure providing information about a queue
-- family
--
-- = Description
--
-- Unresolved directive in VkQueueFamilyProperties2.txt -
-- include::{generated}\/validity\/structs\/VkQueueFamilyProperties2.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkQueueFamilyProperties',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceQueueFamilyProperties2'
data QueueFamilyProperties2 = QueueFamilyProperties2
  { -- Univalued member elided
  -- No documentation found for Nested "QueueFamilyProperties2" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "QueueFamilyProperties2" "queueFamilyProperties"
  queueFamilyProperties :: QueueFamilyProperties
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkQueueFamilyProperties2' and
-- marshal a 'QueueFamilyProperties2' into it. The 'VkQueueFamilyProperties2' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructQueueFamilyProperties2 :: QueueFamilyProperties2 -> (VkQueueFamilyProperties2 -> IO a) -> IO a
withCStructQueueFamilyProperties2 marshalled cont = withCStructQueueFamilyProperties (queueFamilyProperties (marshalled :: QueueFamilyProperties2)) (\queueFamilyProperties'' -> maybeWith withSomeVkStruct (next (marshalled :: QueueFamilyProperties2)) (\pPNext -> cont (VkQueueFamilyProperties2 VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2 pPNext queueFamilyProperties'')))

-- | A function to read a 'VkQueueFamilyProperties2' and all additional
-- structures in the pointer chain into a 'QueueFamilyProperties2'.
fromCStructQueueFamilyProperties2 :: VkQueueFamilyProperties2 -> IO QueueFamilyProperties2
fromCStructQueueFamilyProperties2 c = QueueFamilyProperties2 <$> -- Univalued Member elided
                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkQueueFamilyProperties2)))
                                                             <*> (fromCStructQueueFamilyProperties (vkQueueFamilyProperties (c :: VkQueueFamilyProperties2)))

instance Zero QueueFamilyProperties2 where
  zero = QueueFamilyProperties2 Nothing
                                zero



-- | VkSparseImageFormatProperties2 - Structure specifying sparse image
-- format properties
--
-- = Description
--
-- Unresolved directive in VkSparseImageFormatProperties2.txt -
-- include::{generated}\/validity\/structs\/VkSparseImageFormatProperties2.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageFormatProperties',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceSparseImageFormatProperties2'
data SparseImageFormatProperties2 = SparseImageFormatProperties2
  { -- Univalued member elided
  -- No documentation found for Nested "SparseImageFormatProperties2" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SparseImageFormatProperties2" "properties"
  properties :: SparseImageFormatProperties
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSparseImageFormatProperties2' and
-- marshal a 'SparseImageFormatProperties2' into it. The 'VkSparseImageFormatProperties2' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSparseImageFormatProperties2 :: SparseImageFormatProperties2 -> (VkSparseImageFormatProperties2 -> IO a) -> IO a
withCStructSparseImageFormatProperties2 marshalled cont = withCStructSparseImageFormatProperties (properties (marshalled :: SparseImageFormatProperties2)) (\properties'' -> maybeWith withSomeVkStruct (next (marshalled :: SparseImageFormatProperties2)) (\pPNext -> cont (VkSparseImageFormatProperties2 VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2 pPNext properties'')))

-- | A function to read a 'VkSparseImageFormatProperties2' and all additional
-- structures in the pointer chain into a 'SparseImageFormatProperties2'.
fromCStructSparseImageFormatProperties2 :: VkSparseImageFormatProperties2 -> IO SparseImageFormatProperties2
fromCStructSparseImageFormatProperties2 c = SparseImageFormatProperties2 <$> -- Univalued Member elided
                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSparseImageFormatProperties2)))
                                                                         <*> (fromCStructSparseImageFormatProperties (vkProperties (c :: VkSparseImageFormatProperties2)))

instance Zero SparseImageFormatProperties2 where
  zero = SparseImageFormatProperties2 Nothing
                                      zero



-- | vkGetPhysicalDeviceFeatures2 - Reports capabilities of a physical device
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device from which to query the
--     supported features.
--
-- -   @pFeatures@ is a pointer to a
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2'
--     structure in which the physical device features are returned.
--
-- = Description
--
-- Each structure in @pFeatures@ and its @pNext@ chain contain members
-- corresponding to fine-grained features.
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceFeatures2'
-- writes each member to a boolean value indicating whether that feature is
-- supported.
--
-- Unresolved directive in vkGetPhysicalDeviceFeatures2.txt -
-- include::{generated}\/validity\/protos\/vkGetPhysicalDeviceFeatures2.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2'
getPhysicalDeviceFeatures2 :: PhysicalDevice ->  IO (PhysicalDeviceFeatures2)
getPhysicalDeviceFeatures2 = \(PhysicalDevice physicalDevice' commandTable) -> alloca (\pFeatures' -> vkGetPhysicalDeviceFeatures2 commandTable physicalDevice' pFeatures' *> ((fromCStructPhysicalDeviceFeatures2 <=< peek) pFeatures'))


-- | vkGetPhysicalDeviceFormatProperties2 - Lists physical device’s format
-- capabilities
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device from which to query the
--     format properties.
--
-- -   @format@ is the format whose properties are queried.
--
-- -   @pFormatProperties@ is a pointer to a
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkFormatProperties2'
--     structure in which physical device properties for @format@ are
--     returned.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceFormatProperties2'
-- behaves similarly to
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceFormatProperties',
-- with the ability to return extended information in a @pNext@ chain of
-- output structures.
--
-- Unresolved directive in vkGetPhysicalDeviceFormatProperties2.txt -
-- include::{generated}\/validity\/protos\/vkGetPhysicalDeviceFormatProperties2.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkFormat',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkFormatProperties2',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
getPhysicalDeviceFormatProperties2 :: PhysicalDevice ->  Format ->  IO (FormatProperties2)
getPhysicalDeviceFormatProperties2 = \(PhysicalDevice physicalDevice' commandTable) -> \format' -> alloca (\pFormatProperties' -> vkGetPhysicalDeviceFormatProperties2 commandTable physicalDevice' format' pFormatProperties' *> ((fromCStructFormatProperties2 <=< peek) pFormatProperties'))


-- | vkGetPhysicalDeviceImageFormatProperties2 - Lists physical device’s
-- image format capabilities
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device from which to query the
--     image capabilities.
--
-- -   @pImageFormatInfo@ points to an instance of the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2'
--     structure, describing the parameters that would be consumed by
--     'Graphics.Vulkan.C.Core10.Image.vkCreateImage'.
--
-- -   @pImageFormatProperties@ points to an instance of the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkImageFormatProperties2'
--     structure in which capabilities are returned.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceImageFormatProperties2'
-- behaves similarly to
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceImageFormatProperties',
-- with the ability to return extended information in a @pNext@ chain of
-- output structures.
--
-- == Valid Usage
--
-- -   If the @pNext@ chain of @pImageFormatProperties@ contains an
--     instance of
--     'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkAndroidHardwareBufferUsageANDROID',
--     the @pNext@ chain of @pImageFormatInfo@ /must/ contain an instance
--     of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkPhysicalDeviceExternalImageFormatInfo'
--     with @handleType@ set to
--     'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID'.
--
-- Unresolved directive in vkGetPhysicalDeviceImageFormatProperties2.txt -
-- include::{generated}\/validity\/protos\/vkGetPhysicalDeviceImageFormatProperties2.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkImageFormatProperties2',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2'
getPhysicalDeviceImageFormatProperties2 :: PhysicalDevice ->  PhysicalDeviceImageFormatInfo2 ->  IO (ImageFormatProperties2)
getPhysicalDeviceImageFormatProperties2 = \(PhysicalDevice physicalDevice' commandTable) -> \imageFormatInfo' -> alloca (\pImageFormatProperties' -> (\marshalled -> withCStructPhysicalDeviceImageFormatInfo2 marshalled . flip with) imageFormatInfo' (\pImageFormatInfo' -> vkGetPhysicalDeviceImageFormatProperties2 commandTable physicalDevice' pImageFormatInfo' pImageFormatProperties' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((fromCStructImageFormatProperties2 <=< peek) pImageFormatProperties'))))


-- | vkGetPhysicalDeviceMemoryProperties2 - Reports memory information for
-- the specified physical device
--
-- = Parameters
--
-- -   @physicalDevice@ is the handle to the device to query.
--
-- -   @pMemoryProperties@ points to an instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceMemoryProperties2'
--     structure in which the properties are returned.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceMemoryProperties2'
-- behaves similarly to
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceMemoryProperties',
-- with the ability to return extended information in a @pNext@ chain of
-- output structures.
--
-- Unresolved directive in vkGetPhysicalDeviceMemoryProperties2.txt -
-- include::{generated}\/validity\/protos\/vkGetPhysicalDeviceMemoryProperties2.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceMemoryProperties2'
getPhysicalDeviceMemoryProperties2 :: PhysicalDevice ->  IO (PhysicalDeviceMemoryProperties2)
getPhysicalDeviceMemoryProperties2 = \(PhysicalDevice physicalDevice' commandTable) -> alloca (\pMemoryProperties' -> vkGetPhysicalDeviceMemoryProperties2 commandTable physicalDevice' pMemoryProperties' *> ((fromCStructPhysicalDeviceMemoryProperties2 <=< peek) pMemoryProperties'))


-- | vkGetPhysicalDeviceProperties2 - Returns properties of a physical device
--
-- = Parameters
--
-- -   @physicalDevice@ is the handle to the physical device whose
--     properties will be queried.
--
-- -   @pProperties@ points to an instance of the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2'
--     structure, that will be filled with returned information.
--
-- = Description
--
-- Each structure in @pProperties@ and its @pNext@ chain contain members
-- corresponding to properties or implementation-dependent limits.
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceProperties2'
-- writes each member to a value indicating the value of that property or
-- limit.
--
-- Unresolved directive in vkGetPhysicalDeviceProperties2.txt -
-- include::{generated}\/validity\/protos\/vkGetPhysicalDeviceProperties2.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2'
getPhysicalDeviceProperties2 :: PhysicalDevice ->  IO (PhysicalDeviceProperties2)
getPhysicalDeviceProperties2 = \(PhysicalDevice physicalDevice' commandTable) -> alloca (\pProperties' -> vkGetPhysicalDeviceProperties2 commandTable physicalDevice' pProperties' *> ((fromCStructPhysicalDeviceProperties2 <=< peek) pProperties'))


-- | vkGetPhysicalDeviceQueueFamilyProperties2 - Reports properties of the
-- queues of the specified physical device
--
-- = Parameters
--
-- -   @physicalDevice@ is the handle to the physical device whose
--     properties will be queried.
--
-- -   @pQueueFamilyPropertyCount@ is a pointer to an integer related to
--     the number of queue families available or queried, as described in
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceQueueFamilyProperties'.
--
-- -   @pQueueFamilyProperties@ is either @NULL@ or a pointer to an array
--     of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkQueueFamilyProperties2'
--     structures.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceQueueFamilyProperties2'
-- behaves similarly to
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceQueueFamilyProperties',
-- with the ability to return extended information in a @pNext@ chain of
-- output structures.
--
-- Unresolved directive in vkGetPhysicalDeviceQueueFamilyProperties2.txt -
-- include::{generated}\/validity\/protos\/vkGetPhysicalDeviceQueueFamilyProperties2.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkQueueFamilyProperties2'
getNumPhysicalDeviceQueueFamilyProperties2 :: PhysicalDevice ->  IO (Word32)
getNumPhysicalDeviceQueueFamilyProperties2 = \(PhysicalDevice physicalDevice' commandTable) -> alloca (\pQueueFamilyPropertyCount' -> vkGetPhysicalDeviceQueueFamilyProperties2 commandTable physicalDevice' pQueueFamilyPropertyCount' nullPtr *> (peek pQueueFamilyPropertyCount'))

-- | vkGetPhysicalDeviceQueueFamilyProperties2 - Reports properties of the
-- queues of the specified physical device
--
-- = Parameters
--
-- -   @physicalDevice@ is the handle to the physical device whose
--     properties will be queried.
--
-- -   @pQueueFamilyPropertyCount@ is a pointer to an integer related to
--     the number of queue families available or queried, as described in
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceQueueFamilyProperties'.
--
-- -   @pQueueFamilyProperties@ is either @NULL@ or a pointer to an array
--     of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkQueueFamilyProperties2'
--     structures.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceQueueFamilyProperties2'
-- behaves similarly to
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceQueueFamilyProperties',
-- with the ability to return extended information in a @pNext@ chain of
-- output structures.
--
-- Unresolved directive in vkGetPhysicalDeviceQueueFamilyProperties2.txt -
-- include::{generated}\/validity\/protos\/vkGetPhysicalDeviceQueueFamilyProperties2.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkQueueFamilyProperties2'
getPhysicalDeviceQueueFamilyProperties2 :: PhysicalDevice ->  Word32 ->  IO (Vector QueueFamilyProperties2)
getPhysicalDeviceQueueFamilyProperties2 = \(PhysicalDevice physicalDevice' commandTable) -> \queueFamilyPropertyCount' -> allocaArray (fromIntegral queueFamilyPropertyCount') (\pQueueFamilyProperties' -> with queueFamilyPropertyCount' (\pQueueFamilyPropertyCount' -> vkGetPhysicalDeviceQueueFamilyProperties2 commandTable physicalDevice' pQueueFamilyPropertyCount' pQueueFamilyProperties' *> ((flip Data.Vector.generateM ((\p -> fromCStructQueueFamilyProperties2 <=< peekElemOff p) pQueueFamilyProperties') =<< (fromIntegral <$> (peek pQueueFamilyPropertyCount'))))))
-- | Returns all the values available from 'getPhysicalDeviceQueueFamilyProperties2'.
getAllPhysicalDeviceQueueFamilyProperties2 :: PhysicalDevice ->  IO (Vector QueueFamilyProperties2)
getAllPhysicalDeviceQueueFamilyProperties2 physicalDevice' =
  getNumPhysicalDeviceQueueFamilyProperties2 physicalDevice'
    >>= \num -> getPhysicalDeviceQueueFamilyProperties2 physicalDevice' num



-- | vkGetPhysicalDeviceSparseImageFormatProperties2 - Retrieve properties of
-- an image format applied to sparse images
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device from which to query the
--     sparse image capabilities.
--
-- -   @pFormatInfo@ is a pointer to a structure of type
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceSparseImageFormatInfo2'
--     containing input parameters to the command.
--
-- -   @pPropertyCount@ is a pointer to an integer related to the number of
--     sparse format properties available or queried, as described below.
--
-- -   @pProperties@ is either @NULL@ or a pointer to an array of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkSparseImageFormatProperties2'
--     structures.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceSparseImageFormatProperties2'
-- behaves identically to
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.vkGetPhysicalDeviceSparseImageFormatProperties',
-- with the ability to return extended information by adding extension
-- structures to the @pNext@ chain of its @pProperties@ parameter.
--
-- Unresolved directive in
-- vkGetPhysicalDeviceSparseImageFormatProperties2.txt -
-- include::{generated}\/validity\/protos\/vkGetPhysicalDeviceSparseImageFormatProperties2.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceSparseImageFormatInfo2',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkSparseImageFormatProperties2'
getNumPhysicalDeviceSparseImageFormatProperties2 :: PhysicalDevice ->  PhysicalDeviceSparseImageFormatInfo2 ->  IO (Word32)
getNumPhysicalDeviceSparseImageFormatProperties2 = \(PhysicalDevice physicalDevice' commandTable) -> \formatInfo' -> alloca (\pPropertyCount' -> (\marshalled -> withCStructPhysicalDeviceSparseImageFormatInfo2 marshalled . flip with) formatInfo' (\pFormatInfo' -> vkGetPhysicalDeviceSparseImageFormatProperties2 commandTable physicalDevice' pFormatInfo' pPropertyCount' nullPtr *> (peek pPropertyCount')))

-- | vkGetPhysicalDeviceSparseImageFormatProperties2 - Retrieve properties of
-- an image format applied to sparse images
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device from which to query the
--     sparse image capabilities.
--
-- -   @pFormatInfo@ is a pointer to a structure of type
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceSparseImageFormatInfo2'
--     containing input parameters to the command.
--
-- -   @pPropertyCount@ is a pointer to an integer related to the number of
--     sparse format properties available or queried, as described below.
--
-- -   @pProperties@ is either @NULL@ or a pointer to an array of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkSparseImageFormatProperties2'
--     structures.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceSparseImageFormatProperties2'
-- behaves identically to
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.vkGetPhysicalDeviceSparseImageFormatProperties',
-- with the ability to return extended information by adding extension
-- structures to the @pNext@ chain of its @pProperties@ parameter.
--
-- Unresolved directive in
-- vkGetPhysicalDeviceSparseImageFormatProperties2.txt -
-- include::{generated}\/validity\/protos\/vkGetPhysicalDeviceSparseImageFormatProperties2.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceSparseImageFormatInfo2',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkSparseImageFormatProperties2'
getPhysicalDeviceSparseImageFormatProperties2 :: PhysicalDevice ->  PhysicalDeviceSparseImageFormatInfo2 ->  Word32 ->  IO (Vector SparseImageFormatProperties2)
getPhysicalDeviceSparseImageFormatProperties2 = \(PhysicalDevice physicalDevice' commandTable) -> \formatInfo' -> \propertyCount' -> allocaArray (fromIntegral propertyCount') (\pProperties' -> with propertyCount' (\pPropertyCount' -> (\marshalled -> withCStructPhysicalDeviceSparseImageFormatInfo2 marshalled . flip with) formatInfo' (\pFormatInfo' -> vkGetPhysicalDeviceSparseImageFormatProperties2 commandTable physicalDevice' pFormatInfo' pPropertyCount' pProperties' *> ((flip Data.Vector.generateM ((\p -> fromCStructSparseImageFormatProperties2 <=< peekElemOff p) pProperties') =<< (fromIntegral <$> (peek pPropertyCount')))))))
-- | Returns all the values available from 'getPhysicalDeviceSparseImageFormatProperties2'.
getAllPhysicalDeviceSparseImageFormatProperties2 :: PhysicalDevice ->  PhysicalDeviceSparseImageFormatInfo2 ->  IO (Vector SparseImageFormatProperties2)
getAllPhysicalDeviceSparseImageFormatProperties2 physicalDevice' pFormatInfo' =
  getNumPhysicalDeviceSparseImageFormatProperties2 physicalDevice' pFormatInfo'
    >>= \num -> getPhysicalDeviceSparseImageFormatProperties2 physicalDevice' pFormatInfo' num

