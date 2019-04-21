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


-- | VkDrmFormatModifierPropertiesEXT - Structure specifying properties of a
-- format when combined with a DRM format modifier
--
-- = Description
--
-- The returned @drmFormatModifierTilingFeatures@ /must/ contain at least
-- one bit.
--
-- The implementation /must/ not return @DRM_FORMAT_MOD_INVALID@ in
-- @drmFormatModifier@.
--
-- An image’s /memory planecount/ (as returned by
-- @drmFormatModifierPlaneCount@) is distinct from its /format planecount/
-- (in the sense of
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar>
-- Y’CBCR formats). In
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkImageAspectFlags',
-- each @VK_IMAGE_ASPECT_MEMORY_PLANE@//i/_BIT_EXT represents a _memory
-- plane/ and each @VK_IMAGE_ASPECT_PLANE@//i/_BIT a _format plane/.
--
-- An image’s set of /format planes/ is an ordered partition of the image’s
-- __content__ into separable groups of format channels. The ordered
-- partition is encoded in the name of each
-- 'Graphics.Vulkan.C.Core10.Core.VkFormat'. For example,
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_FORMAT_G8_B8R8_2PLANE_420_UNORM'
-- contains two /format planes/; the first plane contains the green channel
-- and the second plane contains the blue channel and red channel. If the
-- format name does not contain @PLANE@, then the format contains a single
-- plane; for example,
-- 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R8G8B8A8_UNORM'. Some commands,
-- such as
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyBufferToImage',
-- do not operate on all format channels in the image, but instead operate
-- only on the /format planes/ explicitly chosen by the application and
-- operate on each /format plane/ independently.
--
-- An image’s set of /memory planes/ is an ordered partition of the image’s
-- __memory__ rather than the image’s __content__. Each /memory plane/ is a
-- contiguous range of memory. The union of an image’s /memory planes/ is
-- not necessarily contiguous.
--
-- If an image is
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#glossary-linear-resource linear>,
-- then the partition is the same for /memory planes/ and for /format
-- planes/. Therefore, if the returned @drmFormatModifier@ is
-- @DRM_FORMAT_MOD_LINEAR@, then @drmFormatModifierPlaneCount@ /must/ equal
-- the /format planecount/, and @drmFormatModifierTilingFeatures@ /must/ be
-- identical to the
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkFormatProperties2'::@linearTilingFeatures@
-- returned in the same @pNext@ chain.
--
-- If an image is
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#glossary-linear-resource non-linear>,
-- then the partition of the image’s __memory__ into /memory planes/ is
-- implementation-specific and /may/ be unrelated to the partition of the
-- image’s __content__ into /format planes/. For example, consider an image
-- whose @format@ is
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM',
-- @tiling@ is 'VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT', whose
-- @drmFormatModifier@ is not @DRM_FORMAT_MOD_LINEAR@, and @flags@ lacks
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_IMAGE_CREATE_DISJOINT_BIT'.
-- The image has 3 /format planes/, and commands such
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyBufferToImage'
-- act on each /format plane/ independently as if the data of each /format
-- plane/ were separable from the data of the other planes. In a
-- straightforward implementation, the implementation /may/ store the
-- image’s content in 3 adjacent /memory planes/ where each /memory plane/
-- corresponds exactly to a /format plane/. However, the implementation
-- /may/ also store the image’s content in a single /memory plane/ where
-- all format channels are combined using an implementation-private
-- block-compressed format; or the implementation /may/ store the image’s
-- content in a collection of 7 adjacent /memory planes/ using an
-- implementation-private sharding technique. Because the image is
-- non-linear and non-disjoint, the implementation has much freedom when
-- choosing the image’s placement in memory.
--
-- The /memory planecount/ applies to function parameters and structures
-- only when the API specifies an explicit requirement on
-- @drmFormatModifierPlaneCount@. In all other cases, the /memory
-- planecount/ is ignored.
--
-- Unresolved directive in VkDrmFormatModifierPropertiesEXT.txt -
-- include::{generated}\/validity\/structs\/VkDrmFormatModifierPropertiesEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkDrmFormatModifierPropertiesEXT = VkDrmFormatModifierPropertiesEXT
  { -- | @drmFormatModifier@ is a /Linux DRM format modifier/.
  vkDrmFormatModifier :: Word64
  , -- | @drmFormatModifierPlaneCount@ is the number of /memory planes/ in any
  -- image created with @format@ and @drmFormatModifier@. An image’s /memory
  -- planecount/ is distinct from its /format planecount/, as explained
  -- below.
  vkDrmFormatModifierPlaneCount :: Word32
  , -- | @drmFormatModifierTilingFeatures@ is a bitmask of
  -- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkFormatFeatureFlagBits'
  -- that are supported by any image created with @format@ and
  -- @drmFormatModifier@.
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

-- | VkDrmFormatModifierPropertiesListEXT - Structure specifying the list of
-- DRM format modifiers supported for a format
--
-- = Description
--
-- If @pDrmFormatModifierProperties@ is @NULL@, then the function returns
-- in @drmFormatModifierCount@ the number of modifiers compatible with the
-- queried @format@. Otherwise, the application /must/ set
-- @drmFormatModifierCount@ to the length of the array
-- @pDrmFormatModifierProperties@; the function will write at most
-- @drmFormatModifierCount@ elements to the array, and will return in
-- @drmFormatModifierCount@ the number of elements written.
--
-- Among the elements in array @pDrmFormatModifierProperties@, each
-- returned @drmFormatModifier@ /must/ be unique.
--
-- Unresolved directive in VkDrmFormatModifierPropertiesListEXT.txt -
-- include::{generated}\/validity\/structs\/VkDrmFormatModifierPropertiesListEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkDrmFormatModifierPropertiesListEXT = VkDrmFormatModifierPropertiesListEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @drmFormatModifierCount@ is an inout parameter related to the number of
  -- modifiers compatible with the @format@, as described below.
  vkDrmFormatModifierCount :: Word32
  , -- | @pDrmFormatModifierProperties@ is either @NULL@ or an array of
  -- 'VkDrmFormatModifierPropertiesEXT' structures.
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

-- | VkImageDrmFormatModifierExplicitCreateInfoEXT - Specify that an image be
-- created with the provided DRM format modifier and explicit memory layout
--
-- = Description
--
-- The @i@th member of @pPlaneLayouts@ describes the layout of the image’s
-- @i@th /memory plane/ (that is,
-- @VK_IMAGE_ASPECT_MEMORY_PLANE_i_BIT_EXT@). In each element of
-- @pPlaneLayouts@, the implementation /must/ ignore @size@. The
-- implementation calculates the size of each plane, which the application
-- /can/ query with
-- 'Graphics.Vulkan.C.Core10.Image.vkGetImageSubresourceLayout'.
--
-- When creating an image with
-- 'VkImageDrmFormatModifierExplicitCreateInfoEXT', it is the application’s
-- responsibility to satisfy all Valid Usage requirements. However, the
-- implementation /must/ validate that the provided @pPlaneLayouts@, when
-- combined with the provided @drmFormatModifier@ and other creation
-- parameters in 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' and its
-- @pNext@ chain, produce a valid image. (This validation is necessarily
-- implementation-dependent and outside the scope of Vulkan, and therefore
-- not described by Valid Usage requirements). If this validation fails,
-- then 'Graphics.Vulkan.C.Core10.Image.vkCreateImage' returns
-- 'VK_ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT'.
--
-- == Valid Usage
--
-- -   @drmFormatModifier@ must be compatible with the parameters in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' and its @pNext@
--     chain, as determined by querying
--     'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2KHR'
--     extended with 'VkPhysicalDeviceImageDrmFormatModifierInfoEXT'.
--
-- -   @drmFormatModifierPlaneCount@ /must/ be equal to the
--     'VkDrmFormatModifierPropertiesEXT'::@drmFormatModifierPlaneCount@
--     associated with
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@format@ and
--     @drmFormatModifier@, as found by querying
--     'VkDrmFormatModifierPropertiesListEXT'.
--
-- -   For each element of @pPlaneLayouts@, @size@ /must/ be 0
--
-- -   For each element of @pPlaneLayouts@, @arrayPitch@ /must/ be 0 if
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@arrayLayers@ is
--     1.
--
-- -   For each element of @pPlaneLayouts@, @depthPitch@ /must/ be 0 if
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@extent@::@depth@
--     is 1.
--
-- Unresolved directive in
-- VkImageDrmFormatModifierExplicitCreateInfoEXT.txt -
-- include::{generated}\/validity\/structs\/VkImageDrmFormatModifierExplicitCreateInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkImageDrmFormatModifierExplicitCreateInfoEXT = VkImageDrmFormatModifierExplicitCreateInfoEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @drmFormatModifier@ is the /Linux DRM format modifier/ with which the
  -- image will be created.
  vkDrmFormatModifier :: Word64
  , -- | @drmFormatModifierPlaneCount@ is the number of /memory planes/ in the
  -- image (as reported by 'VkDrmFormatModifierPropertiesEXT') as well as the
  -- length of the @pPlaneLayouts@ array.
  vkDrmFormatModifierPlaneCount :: Word32
  , -- | @pPlaneLayouts@ is an array of
  -- 'Graphics.Vulkan.C.Core10.Image.VkSubresourceLayout' structures that
  -- describe the image’s /memory planes/.
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

-- | VkImageDrmFormatModifierListCreateInfoEXT - Specify that an image must
-- be created with a DRM format modifier from the provided list
--
-- == Valid Usage
--
-- -   Each /modifier/ in @pDrmFormatModifiers@ must be compatible with the
--     parameters in 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' and
--     its @pNext@ chain, as determined by querying
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2'
--     extended with 'VkPhysicalDeviceImageDrmFormatModifierInfoEXT'.
--
-- Unresolved directive in VkImageDrmFormatModifierListCreateInfoEXT.txt -
-- include::{generated}\/validity\/structs\/VkImageDrmFormatModifierListCreateInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkImageDrmFormatModifierListCreateInfoEXT = VkImageDrmFormatModifierListCreateInfoEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @drmFormatModifierCount@ is the length of the @pDrmFormatModifiers@
  -- array.
  vkDrmFormatModifierCount :: Word32
  , -- | @pDrmFormatModifiers@ is an array of /Linux DRM format modifiers/.
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

-- | VkImageDrmFormatModifierPropertiesEXT - Properties of an image’s Linux
-- DRM format modifier
--
-- = Description
--
-- If the @image@ was created with
-- 'VkImageDrmFormatModifierListCreateInfoEXT', then the returned
-- @drmFormatModifier@ /must/ belong to the list of modifiers provided at
-- time of image creation in
-- 'VkImageDrmFormatModifierListCreateInfoEXT'::@pDrmFormatModifiers@. If
-- the @image@ was created with
-- 'VkImageDrmFormatModifierExplicitCreateInfoEXT', then the returned
-- @drmFormatModifier@ /must/ be the modifier provided at time of image
-- creation in
-- 'VkImageDrmFormatModifierExplicitCreateInfoEXT'::@drmFormatModifier@.
--
-- Unresolved directive in VkImageDrmFormatModifierPropertiesEXT.txt -
-- include::{generated}\/validity\/structs\/VkImageDrmFormatModifierPropertiesEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkImageDrmFormatModifierPropertiesEXT = VkImageDrmFormatModifierPropertiesEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @drmFormatModifier@ returns the image’s
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#glossary-drm-format-modifier Linux DRM format modifier>.
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

-- | VkPhysicalDeviceImageDrmFormatModifierInfoEXT - Structure specifying a
-- DRM format modifier as image creation parameter
--
-- = Description
--
-- If the @drmFormatModifier@ is incompatible with the parameters specified
-- in
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2'
-- and its @pNext@ chain, then
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceImageFormatProperties2'
-- returns 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_FORMAT_NOT_SUPPORTED'.
-- The implementation /must/ support the query of any @drmFormatModifier@,
-- including unknown and invalid modifier values.
--
-- == Valid Usage
--
-- -   If @sharingMode@ is
--     'Graphics.Vulkan.C.Core10.Buffer.VK_SHARING_MODE_CONCURRENT', then
--     @pQueueFamilyIndices@ /must/ be a valid pointer to an array of
--     @queueFamilyIndexCount@ @uint32_t@ values.
--
-- -   If @sharingMode@ is
--     'Graphics.Vulkan.C.Core10.Buffer.VK_SHARING_MODE_CONCURRENT', then
--     @queueFamilyIndexCount@ /must/ be greater than @1@.
--
-- -   If @sharingMode@ is
--     'Graphics.Vulkan.C.Core10.Buffer.VK_SHARING_MODE_CONCURRENT', each
--     element of @pQueueFamilyIndices@ /must/ be unique and /must/ be less
--     than the @pQueueFamilyPropertyCount@ returned by
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceQueueFamilyProperties2'
--     for the @physicalDevice@ that was used to create @device@.
--
-- Unresolved directive in
-- VkPhysicalDeviceImageDrmFormatModifierInfoEXT.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceImageDrmFormatModifierInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkPhysicalDeviceImageDrmFormatModifierInfoEXT = VkPhysicalDeviceImageDrmFormatModifierInfoEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @drmFormatModifier@ is the image’s /Linux DRM format modifier/,
  -- corresponding to
  -- 'VkImageDrmFormatModifierExplicitCreateInfoEXT'::@modifier@ or to
  -- 'VkImageDrmFormatModifierListCreateInfoEXT'::@pModifiers@.
  vkDrmFormatModifier :: Word64
  , -- | @sharingMode@ specifies how the image will be accessed by multiple queue
  -- families.
  vkSharingMode :: VkSharingMode
  , -- | @queueFamilyIndexCount@ is the number of entries in the
  -- @pQueueFamilyIndices@ array.
  vkQueueFamilyIndexCount :: Word32
  , -- | @pQueueFamilyIndices@ is a list of queue families that will access the
  -- image (ignored if @sharingMode@ is not
  -- 'Graphics.Vulkan.C.Core10.Buffer.VK_SHARING_MODE_CONCURRENT').
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

-- | vkGetImageDrmFormatModifierPropertiesEXT - Returns an image’s DRM format
-- modifier
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the image.
--
-- -   @image@ is the queried image.
--
-- -   @pProperties@ will return properties of the image’s /DRM format
--     modifier/.
--
-- == Valid Usage
--
-- Unresolved directive in vkGetImageDrmFormatModifierPropertiesEXT.txt -
-- include::{generated}\/validity\/protos\/vkGetImageDrmFormatModifierPropertiesEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
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
pattern VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME :: (Eq a ,IsString a) => a
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

-- | 'VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT' indicates that the image’s
-- tiling is defined by a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#glossary-drm-format-modifier Linux DRM format modifier>.
-- The modifier is specified at image creation with
-- 'VkImageDrmFormatModifierListCreateInfoEXT' or
-- 'VkImageDrmFormatModifierExplicitCreateInfoEXT', and /can/ be queried
-- with 'vkGetImageDrmFormatModifierPropertiesEXT'.
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
