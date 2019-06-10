{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_EXT_image_drm_format_modifier
  ( DrmFormatModifierPropertiesEXT(..)
#if defined(VK_USE_PLATFORM_GGP)
  , DrmFormatModifierPropertiesListEXT(..)
  , ImageDrmFormatModifierExplicitCreateInfoEXT(..)
  , ImageDrmFormatModifierListCreateInfoEXT(..)
  , ImageDrmFormatModifierPropertiesEXT(..)
  , PhysicalDeviceImageDrmFormatModifierInfoEXT(..)
  , getImageDrmFormatModifierPropertiesEXT
#endif
  , pattern EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME
  , pattern EXT_IMAGE_DRM_FORMAT_MODIFIER_SPEC_VERSION
  , pattern ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT
  , pattern STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT
  , pattern STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT
  , pattern STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT
  , pattern IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT
  , pattern IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT
  , pattern IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT
  , pattern IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT
  , pattern IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT
  ) where


#if defined(VK_USE_PLATFORM_GGP)
import Control.Monad
  ( (<=<)
  )
#endif
import Data.String
  ( IsString
  )

#if defined(VK_USE_PLATFORM_GGP)
import Data.Vector
  ( Vector
  )
#endif
import Data.Word
  ( Word32
  , Word64
  )

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Marshal.Alloc
  ( alloca
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Ptr
  ( Ptr
  , nullPtr
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Storable
  ( peek
  )
#endif


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier
  ( pattern VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME
  , pattern VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier
  ( VkDrmFormatModifierPropertiesEXT(..)
  , vkGetImageDrmFormatModifierPropertiesEXT
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Buffer
  ( SharingMode
  )
#endif
import Graphics.Vulkan.Core10.DeviceInitialization
  ( FormatFeatureFlags
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Image
  ( SubresourceLayout(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.MemoryManagement
  ( Image
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
  ( pattern ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT
  , pattern STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT
  , pattern STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( pattern IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT
  )
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( pattern IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT
  , pattern IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT
  , pattern IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT
  , pattern IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT
  )



-- No documentation found for TopLevel "VkDrmFormatModifierPropertiesEXT"
data DrmFormatModifierPropertiesEXT = DrmFormatModifierPropertiesEXT
  { -- No documentation found for Nested "DrmFormatModifierPropertiesEXT" "drmFormatModifier"
  drmFormatModifier :: Word64
  , -- No documentation found for Nested "DrmFormatModifierPropertiesEXT" "drmFormatModifierPlaneCount"
  drmFormatModifierPlaneCount :: Word32
  , -- No documentation found for Nested "DrmFormatModifierPropertiesEXT" "drmFormatModifierTilingFeatures"
  drmFormatModifierTilingFeatures :: FormatFeatureFlags
  }
  deriving (Show, Eq)

instance Zero DrmFormatModifierPropertiesEXT where
  zero = DrmFormatModifierPropertiesEXT zero
                                        zero
                                        zero



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDrmFormatModifierPropertiesListEXT"
data DrmFormatModifierPropertiesListEXT = DrmFormatModifierPropertiesListEXT
  { -- No documentation found for Nested "DrmFormatModifierPropertiesListEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DrmFormatModifierPropertiesListEXT" "drmFormatModifierCount"
  drmFormatModifierCount :: Word32
  , -- No documentation found for Nested "DrmFormatModifierPropertiesListEXT" "pDrmFormatModifierProperties"
  drmFormatModifierProperties :: Ptr VkDrmFormatModifierPropertiesEXT
  }
  deriving (Show, Eq)

instance Zero DrmFormatModifierPropertiesListEXT where
  zero = DrmFormatModifierPropertiesListEXT Nothing
                                            zero
                                            nullPtr

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkImageDrmFormatModifierExplicitCreateInfoEXT"
data ImageDrmFormatModifierExplicitCreateInfoEXT = ImageDrmFormatModifierExplicitCreateInfoEXT
  { -- No documentation found for Nested "ImageDrmFormatModifierExplicitCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageDrmFormatModifierExplicitCreateInfoEXT" "drmFormatModifier"
  drmFormatModifier :: Word64
  , -- No documentation found for Nested "ImageDrmFormatModifierExplicitCreateInfoEXT" "pPlaneLayouts"
  planeLayouts :: Vector SubresourceLayout
  }
  deriving (Show, Eq)

instance Zero ImageDrmFormatModifierExplicitCreateInfoEXT where
  zero = ImageDrmFormatModifierExplicitCreateInfoEXT Nothing
                                                     zero
                                                     mempty

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkImageDrmFormatModifierListCreateInfoEXT"
data ImageDrmFormatModifierListCreateInfoEXT = ImageDrmFormatModifierListCreateInfoEXT
  { -- No documentation found for Nested "ImageDrmFormatModifierListCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageDrmFormatModifierListCreateInfoEXT" "pDrmFormatModifiers"
  drmFormatModifiers :: Vector Word64
  }
  deriving (Show, Eq)

instance Zero ImageDrmFormatModifierListCreateInfoEXT where
  zero = ImageDrmFormatModifierListCreateInfoEXT Nothing
                                                 mempty

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkImageDrmFormatModifierPropertiesEXT"
data ImageDrmFormatModifierPropertiesEXT = ImageDrmFormatModifierPropertiesEXT
  { -- No documentation found for Nested "ImageDrmFormatModifierPropertiesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageDrmFormatModifierPropertiesEXT" "drmFormatModifier"
  drmFormatModifier :: Word64
  }
  deriving (Show, Eq)

instance Zero ImageDrmFormatModifierPropertiesEXT where
  zero = ImageDrmFormatModifierPropertiesEXT Nothing
                                             zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceImageDrmFormatModifierInfoEXT"
data PhysicalDeviceImageDrmFormatModifierInfoEXT = PhysicalDeviceImageDrmFormatModifierInfoEXT
  { -- No documentation found for Nested "PhysicalDeviceImageDrmFormatModifierInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceImageDrmFormatModifierInfoEXT" "drmFormatModifier"
  drmFormatModifier :: Word64
  , -- No documentation found for Nested "PhysicalDeviceImageDrmFormatModifierInfoEXT" "sharingMode"
  sharingMode :: SharingMode
  , -- No documentation found for Nested "PhysicalDeviceImageDrmFormatModifierInfoEXT" "pQueueFamilyIndices"
  queueFamilyIndices :: Vector Word32
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceImageDrmFormatModifierInfoEXT where
  zero = PhysicalDeviceImageDrmFormatModifierInfoEXT Nothing
                                                     zero
                                                     zero
                                                     mempty

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetImageDrmFormatModifierPropertiesEXT"
getImageDrmFormatModifierPropertiesEXT :: Device ->  Image ->  IO (ImageDrmFormatModifierPropertiesEXT)
getImageDrmFormatModifierPropertiesEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}
#endif

-- No documentation found for TopLevel "VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME"
pattern EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME = VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_SPEC_VERSION"
pattern EXT_IMAGE_DRM_FORMAT_MODIFIER_SPEC_VERSION :: Integral a => a
pattern EXT_IMAGE_DRM_FORMAT_MODIFIER_SPEC_VERSION = VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_SPEC_VERSION
