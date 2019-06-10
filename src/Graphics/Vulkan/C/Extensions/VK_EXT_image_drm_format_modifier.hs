{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier
  ( VkDrmFormatModifierPropertiesEXT(..)
  , VkDrmFormatModifierPropertiesListEXT(..)
  , VkImageDrmFormatModifierExplicitCreateInfoEXT(..)
  , VkImageDrmFormatModifierListCreateInfoEXT(..)
  , VkImageDrmFormatModifierPropertiesEXT(..)
  , VkPhysicalDeviceImageDrmFormatModifierInfoEXT(..)
  , FN_vkGetImageDrmFormatModifierPropertiesEXT
  , PFN_vkGetImageDrmFormatModifierPropertiesEXT
  , vkGetImageDrmFormatModifierPropertiesEXT
  , pattern VK_ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT
  , pattern VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME
  , pattern VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_SPEC_VERSION
  , pattern VK_IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT
  , pattern VK_IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT
  , pattern VK_IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT
  , pattern VK_IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT
  , pattern VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT
  , pattern VK_STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT
  , pattern VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  , Word64
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


import Graphics.Vulkan.C.Core10.Buffer
  ( VkSharingMode(..)
  )
import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkImageTiling(..)
  , VkDevice
  , VkFormatFeatureFlags
  )
import Graphics.Vulkan.C.Core10.Image
  ( VkSubresourceLayout(..)
  )
import Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkImage
  )
import Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement
  ( VkImageAspectFlagBits(..)
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkDrmFormatModifierPropertiesEXT"
data VkDrmFormatModifierPropertiesEXT = VkDrmFormatModifierPropertiesEXT
  { -- No documentation found for Nested "VkDrmFormatModifierPropertiesEXT" "drmFormatModifier"
  vkDrmFormatModifier :: Word64
  , -- No documentation found for Nested "VkDrmFormatModifierPropertiesEXT" "drmFormatModifierPlaneCount"
  vkDrmFormatModifierPlaneCount :: Word32
  , -- No documentation found for Nested "VkDrmFormatModifierPropertiesEXT" "drmFormatModifierTilingFeatures"
  vkDrmFormatModifierTilingFeatures :: VkFormatFeatureFlags
  }
  deriving (Eq, Show)

instance Storable VkDrmFormatModifierPropertiesEXT where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek ptr = VkDrmFormatModifierPropertiesEXT <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 12)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkDrmFormatModifier (poked :: VkDrmFormatModifierPropertiesEXT))
                *> poke (ptr `plusPtr` 8) (vkDrmFormatModifierPlaneCount (poked :: VkDrmFormatModifierPropertiesEXT))
                *> poke (ptr `plusPtr` 12) (vkDrmFormatModifierTilingFeatures (poked :: VkDrmFormatModifierPropertiesEXT))

instance Zero VkDrmFormatModifierPropertiesEXT where
  zero = VkDrmFormatModifierPropertiesEXT zero
                                          zero
                                          zero

-- No documentation found for TopLevel "VkDrmFormatModifierPropertiesListEXT"
data VkDrmFormatModifierPropertiesListEXT = VkDrmFormatModifierPropertiesListEXT
  { -- No documentation found for Nested "VkDrmFormatModifierPropertiesListEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDrmFormatModifierPropertiesListEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDrmFormatModifierPropertiesListEXT" "drmFormatModifierCount"
  vkDrmFormatModifierCount :: Word32
  , -- No documentation found for Nested "VkDrmFormatModifierPropertiesListEXT" "pDrmFormatModifierProperties"
  vkPDrmFormatModifierProperties :: Ptr VkDrmFormatModifierPropertiesEXT
  }
  deriving (Eq, Show)

instance Storable VkDrmFormatModifierPropertiesListEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkDrmFormatModifierPropertiesListEXT <$> peek (ptr `plusPtr` 0)
                                                  <*> peek (ptr `plusPtr` 8)
                                                  <*> peek (ptr `plusPtr` 16)
                                                  <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDrmFormatModifierPropertiesListEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDrmFormatModifierPropertiesListEXT))
                *> poke (ptr `plusPtr` 16) (vkDrmFormatModifierCount (poked :: VkDrmFormatModifierPropertiesListEXT))
                *> poke (ptr `plusPtr` 24) (vkPDrmFormatModifierProperties (poked :: VkDrmFormatModifierPropertiesListEXT))

instance Zero VkDrmFormatModifierPropertiesListEXT where
  zero = VkDrmFormatModifierPropertiesListEXT VK_STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT
                                              zero
                                              zero
                                              zero

-- No documentation found for TopLevel "VkImageDrmFormatModifierExplicitCreateInfoEXT"
data VkImageDrmFormatModifierExplicitCreateInfoEXT = VkImageDrmFormatModifierExplicitCreateInfoEXT
  { -- No documentation found for Nested "VkImageDrmFormatModifierExplicitCreateInfoEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkImageDrmFormatModifierExplicitCreateInfoEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkImageDrmFormatModifierExplicitCreateInfoEXT" "drmFormatModifier"
  vkDrmFormatModifier :: Word64
  , -- No documentation found for Nested "VkImageDrmFormatModifierExplicitCreateInfoEXT" "drmFormatModifierPlaneCount"
  vkDrmFormatModifierPlaneCount :: Word32
  , -- No documentation found for Nested "VkImageDrmFormatModifierExplicitCreateInfoEXT" "pPlaneLayouts"
  vkPPlaneLayouts :: Ptr VkSubresourceLayout
  }
  deriving (Eq, Show)

instance Storable VkImageDrmFormatModifierExplicitCreateInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkImageDrmFormatModifierExplicitCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                                           <*> peek (ptr `plusPtr` 8)
                                                           <*> peek (ptr `plusPtr` 16)
                                                           <*> peek (ptr `plusPtr` 24)
                                                           <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImageDrmFormatModifierExplicitCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImageDrmFormatModifierExplicitCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkDrmFormatModifier (poked :: VkImageDrmFormatModifierExplicitCreateInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkDrmFormatModifierPlaneCount (poked :: VkImageDrmFormatModifierExplicitCreateInfoEXT))
                *> poke (ptr `plusPtr` 32) (vkPPlaneLayouts (poked :: VkImageDrmFormatModifierExplicitCreateInfoEXT))

instance Zero VkImageDrmFormatModifierExplicitCreateInfoEXT where
  zero = VkImageDrmFormatModifierExplicitCreateInfoEXT VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT
                                                       zero
                                                       zero
                                                       zero
                                                       zero

-- No documentation found for TopLevel "VkImageDrmFormatModifierListCreateInfoEXT"
data VkImageDrmFormatModifierListCreateInfoEXT = VkImageDrmFormatModifierListCreateInfoEXT
  { -- No documentation found for Nested "VkImageDrmFormatModifierListCreateInfoEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkImageDrmFormatModifierListCreateInfoEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkImageDrmFormatModifierListCreateInfoEXT" "drmFormatModifierCount"
  vkDrmFormatModifierCount :: Word32
  , -- No documentation found for Nested "VkImageDrmFormatModifierListCreateInfoEXT" "pDrmFormatModifiers"
  vkPDrmFormatModifiers :: Ptr Word64
  }
  deriving (Eq, Show)

instance Storable VkImageDrmFormatModifierListCreateInfoEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkImageDrmFormatModifierListCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                                       <*> peek (ptr `plusPtr` 8)
                                                       <*> peek (ptr `plusPtr` 16)
                                                       <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImageDrmFormatModifierListCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImageDrmFormatModifierListCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkDrmFormatModifierCount (poked :: VkImageDrmFormatModifierListCreateInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkPDrmFormatModifiers (poked :: VkImageDrmFormatModifierListCreateInfoEXT))

instance Zero VkImageDrmFormatModifierListCreateInfoEXT where
  zero = VkImageDrmFormatModifierListCreateInfoEXT VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT
                                                   zero
                                                   zero
                                                   zero

-- No documentation found for TopLevel "VkImageDrmFormatModifierPropertiesEXT"
data VkImageDrmFormatModifierPropertiesEXT = VkImageDrmFormatModifierPropertiesEXT
  { -- No documentation found for Nested "VkImageDrmFormatModifierPropertiesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkImageDrmFormatModifierPropertiesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkImageDrmFormatModifierPropertiesEXT" "drmFormatModifier"
  vkDrmFormatModifier :: Word64
  }
  deriving (Eq, Show)

instance Storable VkImageDrmFormatModifierPropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkImageDrmFormatModifierPropertiesEXT <$> peek (ptr `plusPtr` 0)
                                                   <*> peek (ptr `plusPtr` 8)
                                                   <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImageDrmFormatModifierPropertiesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImageDrmFormatModifierPropertiesEXT))
                *> poke (ptr `plusPtr` 16) (vkDrmFormatModifier (poked :: VkImageDrmFormatModifierPropertiesEXT))

instance Zero VkImageDrmFormatModifierPropertiesEXT where
  zero = VkImageDrmFormatModifierPropertiesEXT VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT
                                               zero
                                               zero

-- No documentation found for TopLevel "VkPhysicalDeviceImageDrmFormatModifierInfoEXT"
data VkPhysicalDeviceImageDrmFormatModifierInfoEXT = VkPhysicalDeviceImageDrmFormatModifierInfoEXT
  { -- No documentation found for Nested "VkPhysicalDeviceImageDrmFormatModifierInfoEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceImageDrmFormatModifierInfoEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceImageDrmFormatModifierInfoEXT" "drmFormatModifier"
  vkDrmFormatModifier :: Word64
  , -- No documentation found for Nested "VkPhysicalDeviceImageDrmFormatModifierInfoEXT" "sharingMode"
  vkSharingMode :: VkSharingMode
  , -- No documentation found for Nested "VkPhysicalDeviceImageDrmFormatModifierInfoEXT" "queueFamilyIndexCount"
  vkQueueFamilyIndexCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceImageDrmFormatModifierInfoEXT" "pQueueFamilyIndices"
  vkPQueueFamilyIndices :: Ptr Word32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceImageDrmFormatModifierInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceImageDrmFormatModifierInfoEXT <$> peek (ptr `plusPtr` 0)
                                                           <*> peek (ptr `plusPtr` 8)
                                                           <*> peek (ptr `plusPtr` 16)
                                                           <*> peek (ptr `plusPtr` 24)
                                                           <*> peek (ptr `plusPtr` 28)
                                                           <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceImageDrmFormatModifierInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceImageDrmFormatModifierInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkDrmFormatModifier (poked :: VkPhysicalDeviceImageDrmFormatModifierInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkSharingMode (poked :: VkPhysicalDeviceImageDrmFormatModifierInfoEXT))
                *> poke (ptr `plusPtr` 28) (vkQueueFamilyIndexCount (poked :: VkPhysicalDeviceImageDrmFormatModifierInfoEXT))
                *> poke (ptr `plusPtr` 32) (vkPQueueFamilyIndices (poked :: VkPhysicalDeviceImageDrmFormatModifierInfoEXT))

instance Zero VkPhysicalDeviceImageDrmFormatModifierInfoEXT where
  zero = VkPhysicalDeviceImageDrmFormatModifierInfoEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT
                                                       zero
                                                       zero
                                                       zero
                                                       zero
                                                       zero

-- No documentation found for TopLevel "vkGetImageDrmFormatModifierPropertiesEXT"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetImageDrmFormatModifierPropertiesEXT" vkGetImageDrmFormatModifierPropertiesEXT :: ("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pProperties" ::: Ptr VkImageDrmFormatModifierPropertiesEXT) -> IO VkResult
#else
vkGetImageDrmFormatModifierPropertiesEXT :: DeviceCmds -> ("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pProperties" ::: Ptr VkImageDrmFormatModifierPropertiesEXT) -> IO VkResult
vkGetImageDrmFormatModifierPropertiesEXT deviceCmds = mkVkGetImageDrmFormatModifierPropertiesEXT (pVkGetImageDrmFormatModifierPropertiesEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetImageDrmFormatModifierPropertiesEXT
  :: FunPtr (("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pProperties" ::: Ptr VkImageDrmFormatModifierPropertiesEXT) -> IO VkResult) -> (("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pProperties" ::: Ptr VkImageDrmFormatModifierPropertiesEXT) -> IO VkResult)
#endif

type FN_vkGetImageDrmFormatModifierPropertiesEXT = ("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pProperties" ::: Ptr VkImageDrmFormatModifierPropertiesEXT) -> IO VkResult
type PFN_vkGetImageDrmFormatModifierPropertiesEXT = FunPtr FN_vkGetImageDrmFormatModifierPropertiesEXT

-- No documentation found for Nested "VkResult" "VK_ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT"
pattern VK_ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT :: VkResult
pattern VK_ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT = VkResult (-1000158000)

-- No documentation found for TopLevel "VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME"
pattern VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME = "VK_EXT_image_drm_format_modifier"

-- No documentation found for TopLevel "VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_SPEC_VERSION"
pattern VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_SPEC_VERSION :: Integral a => a
pattern VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_SPEC_VERSION = 1

-- No documentation found for Nested "VkImageAspectFlagBits" "VK_IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT"
pattern VK_IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT :: VkImageAspectFlagBits
pattern VK_IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT = VkImageAspectFlagBits 0x00000080

-- No documentation found for Nested "VkImageAspectFlagBits" "VK_IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT"
pattern VK_IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT :: VkImageAspectFlagBits
pattern VK_IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT = VkImageAspectFlagBits 0x00000100

-- No documentation found for Nested "VkImageAspectFlagBits" "VK_IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT"
pattern VK_IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT :: VkImageAspectFlagBits
pattern VK_IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT = VkImageAspectFlagBits 0x00000200

-- No documentation found for Nested "VkImageAspectFlagBits" "VK_IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT"
pattern VK_IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT :: VkImageAspectFlagBits
pattern VK_IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT = VkImageAspectFlagBits 0x00000400

-- No documentation found for Nested "VkImageTiling" "VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT"
pattern VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT :: VkImageTiling
pattern VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT = VkImageTiling 1000158000

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT"
pattern VK_STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT = VkStructureType 1000158001

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT"
pattern VK_STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT = VkStructureType 1000158000

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT = VkStructureType 1000158004

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT = VkStructureType 1000158003

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT"
pattern VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT = VkStructureType 1000158005

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT = VkStructureType 1000158002
