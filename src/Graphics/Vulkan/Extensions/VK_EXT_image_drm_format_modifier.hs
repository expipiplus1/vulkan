{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_EXT_image_drm_format_modifier
  ( withCStructDrmFormatModifierPropertiesEXT
  , fromCStructDrmFormatModifierPropertiesEXT
  , DrmFormatModifierPropertiesEXT(..)
  , withCStructDrmFormatModifierPropertiesListEXT
  , fromCStructDrmFormatModifierPropertiesListEXT
  , DrmFormatModifierPropertiesListEXT(..)
  , withCStructImageDrmFormatModifierExplicitCreateInfoEXT
  , fromCStructImageDrmFormatModifierExplicitCreateInfoEXT
  , ImageDrmFormatModifierExplicitCreateInfoEXT(..)
  , withCStructImageDrmFormatModifierListCreateInfoEXT
  , fromCStructImageDrmFormatModifierListCreateInfoEXT
  , ImageDrmFormatModifierListCreateInfoEXT(..)
  , withCStructImageDrmFormatModifierPropertiesEXT
  , fromCStructImageDrmFormatModifierPropertiesEXT
  , ImageDrmFormatModifierPropertiesEXT(..)
  , withCStructPhysicalDeviceImageDrmFormatModifierInfoEXT
  , fromCStructPhysicalDeviceImageDrmFormatModifierInfoEXT
  , PhysicalDeviceImageDrmFormatModifierInfoEXT(..)
  , getImageDrmFormatModifierPropertiesEXT
  , pattern VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_SPEC_VERSION
  , pattern VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME
  , pattern VK_ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT
  , pattern VK_STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT
  , pattern VK_STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT
  , pattern VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT
  , pattern VK_IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT
  , pattern VK_IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT
  , pattern VK_IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT
  , pattern VK_IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( (<=<)
  , when
  )
import Data.Function
  ( (&)
  )
import Data.Maybe
  ( maybe
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( generateM
  , length
  )
import Data.Word
  ( Word32
  , Word64
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( getImageDrmFormatModifierPropertiesEXT
  )


import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier
  ( VkDrmFormatModifierPropertiesEXT(..)
  , VkDrmFormatModifierPropertiesListEXT(..)
  , VkImageDrmFormatModifierExplicitCreateInfoEXT(..)
  , VkImageDrmFormatModifierListCreateInfoEXT(..)
  , VkImageDrmFormatModifierPropertiesEXT(..)
  , VkPhysicalDeviceImageDrmFormatModifierInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT
  , pattern VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT
  )
import Graphics.Vulkan.Core10.Buffer
  ( SharingMode
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  , FormatFeatureFlags
  )
import Graphics.Vulkan.Core10.Image
  ( SubresourceLayout(..)
  , fromCStructSubresourceLayout
  , withCStructSubresourceLayout
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( Image
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier
  ( pattern VK_ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT
  , pattern VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME
  , pattern VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_SPEC_VERSION
  , pattern VK_IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT
  , pattern VK_IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT
  , pattern VK_IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT
  , pattern VK_IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT
  , pattern VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT
  , pattern VK_STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT
  )


-- No documentation found for TopLevel "DrmFormatModifierPropertiesEXT"
data DrmFormatModifierPropertiesEXT = DrmFormatModifierPropertiesEXT
  { -- No documentation found for Nested "DrmFormatModifierPropertiesEXT" "drmFormatModifier"
  vkDrmFormatModifier :: Word64
  , -- No documentation found for Nested "DrmFormatModifierPropertiesEXT" "drmFormatModifierPlaneCount"
  vkDrmFormatModifierPlaneCount :: Word32
  , -- No documentation found for Nested "DrmFormatModifierPropertiesEXT" "drmFormatModifierTilingFeatures"
  vkDrmFormatModifierTilingFeatures :: FormatFeatureFlags
  }
  deriving (Show, Eq)
withCStructDrmFormatModifierPropertiesEXT :: DrmFormatModifierPropertiesEXT -> (VkDrmFormatModifierPropertiesEXT -> IO a) -> IO a
withCStructDrmFormatModifierPropertiesEXT from cont = cont (VkDrmFormatModifierPropertiesEXT (vkDrmFormatModifier (from :: DrmFormatModifierPropertiesEXT)) (vkDrmFormatModifierPlaneCount (from :: DrmFormatModifierPropertiesEXT)) (vkDrmFormatModifierTilingFeatures (from :: DrmFormatModifierPropertiesEXT)))
fromCStructDrmFormatModifierPropertiesEXT :: VkDrmFormatModifierPropertiesEXT -> IO DrmFormatModifierPropertiesEXT
fromCStructDrmFormatModifierPropertiesEXT c = DrmFormatModifierPropertiesEXT <$> pure (vkDrmFormatModifier (c :: VkDrmFormatModifierPropertiesEXT))
                                                                             <*> pure (vkDrmFormatModifierPlaneCount (c :: VkDrmFormatModifierPropertiesEXT))
                                                                             <*> pure (vkDrmFormatModifierTilingFeatures (c :: VkDrmFormatModifierPropertiesEXT))
-- No documentation found for TopLevel "DrmFormatModifierPropertiesListEXT"
data DrmFormatModifierPropertiesListEXT = DrmFormatModifierPropertiesListEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "DrmFormatModifierPropertiesListEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  -- Optional length valued member elided
  , -- No documentation found for Nested "DrmFormatModifierPropertiesListEXT" "pDrmFormatModifierProperties"
  vkPDrmFormatModifierProperties :: Maybe (Vector DrmFormatModifierPropertiesEXT)
  }
  deriving (Show, Eq)
withCStructDrmFormatModifierPropertiesListEXT :: DrmFormatModifierPropertiesListEXT -> (VkDrmFormatModifierPropertiesListEXT -> IO a) -> IO a
withCStructDrmFormatModifierPropertiesListEXT from cont = maybeWith (withVec withCStructDrmFormatModifierPropertiesEXT) (vkPDrmFormatModifierProperties (from :: DrmFormatModifierPropertiesListEXT)) (\pDrmFormatModifierProperties -> maybeWith withSomeVkStruct (vkPNext (from :: DrmFormatModifierPropertiesListEXT)) (\pPNext -> cont (VkDrmFormatModifierPropertiesListEXT VK_STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT pPNext (maybe 0 (fromIntegral . Data.Vector.length) (vkPDrmFormatModifierProperties (from :: DrmFormatModifierPropertiesListEXT))) pDrmFormatModifierProperties)))
fromCStructDrmFormatModifierPropertiesListEXT :: VkDrmFormatModifierPropertiesListEXT -> IO DrmFormatModifierPropertiesListEXT
fromCStructDrmFormatModifierPropertiesListEXT c = DrmFormatModifierPropertiesListEXT <$> -- Univalued Member elided
                                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDrmFormatModifierPropertiesListEXT)))
                                                                                     -- Optional length valued member elided
                                                                                     <*> maybePeek (\p -> Data.Vector.generateM (fromIntegral (vkDrmFormatModifierCount (c :: VkDrmFormatModifierPropertiesListEXT))) (((fromCStructDrmFormatModifierPropertiesEXT <=<) . peekElemOff) p)) (vkPDrmFormatModifierProperties (c :: VkDrmFormatModifierPropertiesListEXT))
-- No documentation found for TopLevel "ImageDrmFormatModifierExplicitCreateInfoEXT"
data ImageDrmFormatModifierExplicitCreateInfoEXT = ImageDrmFormatModifierExplicitCreateInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "ImageDrmFormatModifierExplicitCreateInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageDrmFormatModifierExplicitCreateInfoEXT" "drmFormatModifier"
  vkDrmFormatModifier :: Word64
  -- Length valued member elided
  , -- No documentation found for Nested "ImageDrmFormatModifierExplicitCreateInfoEXT" "pPlaneLayouts"
  vkPPlaneLayouts :: Vector SubresourceLayout
  }
  deriving (Show, Eq)
withCStructImageDrmFormatModifierExplicitCreateInfoEXT :: ImageDrmFormatModifierExplicitCreateInfoEXT -> (VkImageDrmFormatModifierExplicitCreateInfoEXT -> IO a) -> IO a
withCStructImageDrmFormatModifierExplicitCreateInfoEXT from cont = withVec withCStructSubresourceLayout (vkPPlaneLayouts (from :: ImageDrmFormatModifierExplicitCreateInfoEXT)) (\pPlaneLayouts -> maybeWith withSomeVkStruct (vkPNext (from :: ImageDrmFormatModifierExplicitCreateInfoEXT)) (\pPNext -> cont (VkImageDrmFormatModifierExplicitCreateInfoEXT VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT pPNext (vkDrmFormatModifier (from :: ImageDrmFormatModifierExplicitCreateInfoEXT)) (fromIntegral (Data.Vector.length (vkPPlaneLayouts (from :: ImageDrmFormatModifierExplicitCreateInfoEXT)))) pPlaneLayouts)))
fromCStructImageDrmFormatModifierExplicitCreateInfoEXT :: VkImageDrmFormatModifierExplicitCreateInfoEXT -> IO ImageDrmFormatModifierExplicitCreateInfoEXT
fromCStructImageDrmFormatModifierExplicitCreateInfoEXT c = ImageDrmFormatModifierExplicitCreateInfoEXT <$> -- Univalued Member elided
                                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImageDrmFormatModifierExplicitCreateInfoEXT)))
                                                                                                       <*> pure (vkDrmFormatModifier (c :: VkImageDrmFormatModifierExplicitCreateInfoEXT))
                                                                                                       -- Length valued member elided
                                                                                                       <*> (Data.Vector.generateM (fromIntegral (vkDrmFormatModifierPlaneCount (c :: VkImageDrmFormatModifierExplicitCreateInfoEXT))) (((fromCStructSubresourceLayout <=<) . peekElemOff) (vkPPlaneLayouts (c :: VkImageDrmFormatModifierExplicitCreateInfoEXT))))
-- No documentation found for TopLevel "ImageDrmFormatModifierListCreateInfoEXT"
data ImageDrmFormatModifierListCreateInfoEXT = ImageDrmFormatModifierListCreateInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "ImageDrmFormatModifierListCreateInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "ImageDrmFormatModifierListCreateInfoEXT" "pDrmFormatModifiers"
  vkPDrmFormatModifiers :: Vector Word64
  }
  deriving (Show, Eq)
withCStructImageDrmFormatModifierListCreateInfoEXT :: ImageDrmFormatModifierListCreateInfoEXT -> (VkImageDrmFormatModifierListCreateInfoEXT -> IO a) -> IO a
withCStructImageDrmFormatModifierListCreateInfoEXT from cont = withVec (&) (vkPDrmFormatModifiers (from :: ImageDrmFormatModifierListCreateInfoEXT)) (\pDrmFormatModifiers -> maybeWith withSomeVkStruct (vkPNext (from :: ImageDrmFormatModifierListCreateInfoEXT)) (\pPNext -> cont (VkImageDrmFormatModifierListCreateInfoEXT VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT pPNext (fromIntegral (Data.Vector.length (vkPDrmFormatModifiers (from :: ImageDrmFormatModifierListCreateInfoEXT)))) pDrmFormatModifiers)))
fromCStructImageDrmFormatModifierListCreateInfoEXT :: VkImageDrmFormatModifierListCreateInfoEXT -> IO ImageDrmFormatModifierListCreateInfoEXT
fromCStructImageDrmFormatModifierListCreateInfoEXT c = ImageDrmFormatModifierListCreateInfoEXT <$> -- Univalued Member elided
                                                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImageDrmFormatModifierListCreateInfoEXT)))
                                                                                               -- Length valued member elided
                                                                                               <*> (Data.Vector.generateM (fromIntegral (vkDrmFormatModifierCount (c :: VkImageDrmFormatModifierListCreateInfoEXT))) (peekElemOff (vkPDrmFormatModifiers (c :: VkImageDrmFormatModifierListCreateInfoEXT))))
-- No documentation found for TopLevel "ImageDrmFormatModifierPropertiesEXT"
data ImageDrmFormatModifierPropertiesEXT = ImageDrmFormatModifierPropertiesEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "ImageDrmFormatModifierPropertiesEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageDrmFormatModifierPropertiesEXT" "drmFormatModifier"
  vkDrmFormatModifier :: Word64
  }
  deriving (Show, Eq)
withCStructImageDrmFormatModifierPropertiesEXT :: ImageDrmFormatModifierPropertiesEXT -> (VkImageDrmFormatModifierPropertiesEXT -> IO a) -> IO a
withCStructImageDrmFormatModifierPropertiesEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: ImageDrmFormatModifierPropertiesEXT)) (\pPNext -> cont (VkImageDrmFormatModifierPropertiesEXT VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT pPNext (vkDrmFormatModifier (from :: ImageDrmFormatModifierPropertiesEXT))))
fromCStructImageDrmFormatModifierPropertiesEXT :: VkImageDrmFormatModifierPropertiesEXT -> IO ImageDrmFormatModifierPropertiesEXT
fromCStructImageDrmFormatModifierPropertiesEXT c = ImageDrmFormatModifierPropertiesEXT <$> -- Univalued Member elided
                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImageDrmFormatModifierPropertiesEXT)))
                                                                                       <*> pure (vkDrmFormatModifier (c :: VkImageDrmFormatModifierPropertiesEXT))
-- No documentation found for TopLevel "PhysicalDeviceImageDrmFormatModifierInfoEXT"
data PhysicalDeviceImageDrmFormatModifierInfoEXT = PhysicalDeviceImageDrmFormatModifierInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceImageDrmFormatModifierInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceImageDrmFormatModifierInfoEXT" "drmFormatModifier"
  vkDrmFormatModifier :: Word64
  , -- No documentation found for Nested "PhysicalDeviceImageDrmFormatModifierInfoEXT" "sharingMode"
  vkSharingMode :: SharingMode
  -- Length valued member elided
  , -- No documentation found for Nested "PhysicalDeviceImageDrmFormatModifierInfoEXT" "pQueueFamilyIndices"
  vkPQueueFamilyIndices :: Vector Word32
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceImageDrmFormatModifierInfoEXT :: PhysicalDeviceImageDrmFormatModifierInfoEXT -> (VkPhysicalDeviceImageDrmFormatModifierInfoEXT -> IO a) -> IO a
withCStructPhysicalDeviceImageDrmFormatModifierInfoEXT from cont = withVec (&) (vkPQueueFamilyIndices (from :: PhysicalDeviceImageDrmFormatModifierInfoEXT)) (\pQueueFamilyIndices -> maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceImageDrmFormatModifierInfoEXT)) (\pPNext -> cont (VkPhysicalDeviceImageDrmFormatModifierInfoEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT pPNext (vkDrmFormatModifier (from :: PhysicalDeviceImageDrmFormatModifierInfoEXT)) (vkSharingMode (from :: PhysicalDeviceImageDrmFormatModifierInfoEXT)) (fromIntegral (Data.Vector.length (vkPQueueFamilyIndices (from :: PhysicalDeviceImageDrmFormatModifierInfoEXT)))) pQueueFamilyIndices)))
fromCStructPhysicalDeviceImageDrmFormatModifierInfoEXT :: VkPhysicalDeviceImageDrmFormatModifierInfoEXT -> IO PhysicalDeviceImageDrmFormatModifierInfoEXT
fromCStructPhysicalDeviceImageDrmFormatModifierInfoEXT c = PhysicalDeviceImageDrmFormatModifierInfoEXT <$> -- Univalued Member elided
                                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceImageDrmFormatModifierInfoEXT)))
                                                                                                       <*> pure (vkDrmFormatModifier (c :: VkPhysicalDeviceImageDrmFormatModifierInfoEXT))
                                                                                                       <*> pure (vkSharingMode (c :: VkPhysicalDeviceImageDrmFormatModifierInfoEXT))
                                                                                                       -- Length valued member elided
                                                                                                       <*> (Data.Vector.generateM (fromIntegral (vkQueueFamilyIndexCount (c :: VkPhysicalDeviceImageDrmFormatModifierInfoEXT))) (peekElemOff (vkPQueueFamilyIndices (c :: VkPhysicalDeviceImageDrmFormatModifierInfoEXT))))

-- | Wrapper for 'vkGetImageDrmFormatModifierPropertiesEXT'
getImageDrmFormatModifierPropertiesEXT :: Device ->  Image ->  IO (ImageDrmFormatModifierPropertiesEXT)
getImageDrmFormatModifierPropertiesEXT = \(Device device commandTable) -> \image -> alloca (\pProperties -> Graphics.Vulkan.C.Dynamic.getImageDrmFormatModifierPropertiesEXT commandTable device image pProperties >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((fromCStructImageDrmFormatModifierPropertiesEXT <=< peek) pProperties)))
