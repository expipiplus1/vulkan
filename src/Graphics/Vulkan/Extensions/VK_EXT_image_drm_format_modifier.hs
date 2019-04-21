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
  ( empty
  , generateM
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


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier
  ( VkDrmFormatModifierPropertiesEXT(..)
  , VkDrmFormatModifierPropertiesListEXT(..)
  , VkImageDrmFormatModifierExplicitCreateInfoEXT(..)
  , VkImageDrmFormatModifierListCreateInfoEXT(..)
  , VkImageDrmFormatModifierPropertiesEXT(..)
  , VkPhysicalDeviceImageDrmFormatModifierInfoEXT(..)
  , vkGetImageDrmFormatModifierPropertiesEXT
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
-- @tiling@ is
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier.VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT',
-- whose @drmFormatModifier@ is not @DRM_FORMAT_MOD_LINEAR@, and @flags@
-- lacks
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
data DrmFormatModifierPropertiesEXT = DrmFormatModifierPropertiesEXT
  { -- No documentation found for Nested "DrmFormatModifierPropertiesEXT" "drmFormatModifier"
  drmFormatModifier :: Word64
  , -- No documentation found for Nested "DrmFormatModifierPropertiesEXT" "drmFormatModifierPlaneCount"
  drmFormatModifierPlaneCount :: Word32
  , -- No documentation found for Nested "DrmFormatModifierPropertiesEXT" "drmFormatModifierTilingFeatures"
  drmFormatModifierTilingFeatures :: FormatFeatureFlags
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDrmFormatModifierPropertiesEXT' and
-- marshal a 'DrmFormatModifierPropertiesEXT' into it. The 'VkDrmFormatModifierPropertiesEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDrmFormatModifierPropertiesEXT :: DrmFormatModifierPropertiesEXT -> (VkDrmFormatModifierPropertiesEXT -> IO a) -> IO a
withCStructDrmFormatModifierPropertiesEXT marshalled cont = cont (VkDrmFormatModifierPropertiesEXT (drmFormatModifier (marshalled :: DrmFormatModifierPropertiesEXT)) (drmFormatModifierPlaneCount (marshalled :: DrmFormatModifierPropertiesEXT)) (drmFormatModifierTilingFeatures (marshalled :: DrmFormatModifierPropertiesEXT)))

-- | A function to read a 'VkDrmFormatModifierPropertiesEXT' and all additional
-- structures in the pointer chain into a 'DrmFormatModifierPropertiesEXT'.
fromCStructDrmFormatModifierPropertiesEXT :: VkDrmFormatModifierPropertiesEXT -> IO DrmFormatModifierPropertiesEXT
fromCStructDrmFormatModifierPropertiesEXT c = DrmFormatModifierPropertiesEXT <$> pure (vkDrmFormatModifier (c :: VkDrmFormatModifierPropertiesEXT))
                                                                             <*> pure (vkDrmFormatModifierPlaneCount (c :: VkDrmFormatModifierPropertiesEXT))
                                                                             <*> pure (vkDrmFormatModifierTilingFeatures (c :: VkDrmFormatModifierPropertiesEXT))

instance Zero DrmFormatModifierPropertiesEXT where
  zero = DrmFormatModifierPropertiesEXT zero
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
data DrmFormatModifierPropertiesListEXT = DrmFormatModifierPropertiesListEXT
  { -- Univalued member elided
  -- No documentation found for Nested "DrmFormatModifierPropertiesListEXT" "pNext"
  next :: Maybe SomeVkStruct
  -- Optional length valued member elided
  , -- No documentation found for Nested "DrmFormatModifierPropertiesListEXT" "pDrmFormatModifierProperties"
  drmFormatModifierProperties :: Maybe (Vector DrmFormatModifierPropertiesEXT)
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDrmFormatModifierPropertiesListEXT' and
-- marshal a 'DrmFormatModifierPropertiesListEXT' into it. The 'VkDrmFormatModifierPropertiesListEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDrmFormatModifierPropertiesListEXT :: DrmFormatModifierPropertiesListEXT -> (VkDrmFormatModifierPropertiesListEXT -> IO a) -> IO a
withCStructDrmFormatModifierPropertiesListEXT marshalled cont = maybeWith (withVec withCStructDrmFormatModifierPropertiesEXT) (drmFormatModifierProperties (marshalled :: DrmFormatModifierPropertiesListEXT)) (\pPDrmFormatModifierProperties -> maybeWith withSomeVkStruct (next (marshalled :: DrmFormatModifierPropertiesListEXT)) (\pPNext -> cont (VkDrmFormatModifierPropertiesListEXT VK_STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT pPNext (maybe 0 (fromIntegral . Data.Vector.length) (drmFormatModifierProperties (marshalled :: DrmFormatModifierPropertiesListEXT))) pPDrmFormatModifierProperties)))

-- | A function to read a 'VkDrmFormatModifierPropertiesListEXT' and all additional
-- structures in the pointer chain into a 'DrmFormatModifierPropertiesListEXT'.
fromCStructDrmFormatModifierPropertiesListEXT :: VkDrmFormatModifierPropertiesListEXT -> IO DrmFormatModifierPropertiesListEXT
fromCStructDrmFormatModifierPropertiesListEXT c = DrmFormatModifierPropertiesListEXT <$> -- Univalued Member elided
                                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDrmFormatModifierPropertiesListEXT)))
                                                                                     -- Optional length valued member elided
                                                                                     <*> maybePeek (\p -> Data.Vector.generateM (fromIntegral (vkDrmFormatModifierCount (c :: VkDrmFormatModifierPropertiesListEXT))) (((fromCStructDrmFormatModifierPropertiesEXT <=<) . peekElemOff) p)) (vkPDrmFormatModifierProperties (c :: VkDrmFormatModifierPropertiesListEXT))

instance Zero DrmFormatModifierPropertiesListEXT where
  zero = DrmFormatModifierPropertiesListEXT Nothing
                                            Nothing



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
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier.VkImageDrmFormatModifierExplicitCreateInfoEXT',
-- it is the application’s responsibility to satisfy all Valid Usage
-- requirements. However, the implementation /must/ validate that the
-- provided @pPlaneLayouts@, when combined with the provided
-- @drmFormatModifier@ and other creation parameters in
-- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' and its @pNext@
-- chain, produce a valid image. (This validation is necessarily
-- implementation-dependent and outside the scope of Vulkan, and therefore
-- not described by Valid Usage requirements). If this validation fails,
-- then 'Graphics.Vulkan.C.Core10.Image.vkCreateImage' returns
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier.VK_ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT'.
--
-- == Valid Usage
--
-- -   @drmFormatModifier@ must be compatible with the parameters in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' and its @pNext@
--     chain, as determined by querying
--     'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2KHR'
--     extended with
--     'Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier.VkPhysicalDeviceImageDrmFormatModifierInfoEXT'.
--
-- -   @drmFormatModifierPlaneCount@ /must/ be equal to the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier.VkDrmFormatModifierPropertiesEXT'::@drmFormatModifierPlaneCount@
--     associated with
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@format@ and
--     @drmFormatModifier@, as found by querying
--     'Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier.VkDrmFormatModifierPropertiesListEXT'.
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
data ImageDrmFormatModifierExplicitCreateInfoEXT = ImageDrmFormatModifierExplicitCreateInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "ImageDrmFormatModifierExplicitCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageDrmFormatModifierExplicitCreateInfoEXT" "drmFormatModifier"
  drmFormatModifier :: Word64
  -- Length valued member elided
  , -- No documentation found for Nested "ImageDrmFormatModifierExplicitCreateInfoEXT" "pPlaneLayouts"
  planeLayouts :: Vector SubresourceLayout
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkImageDrmFormatModifierExplicitCreateInfoEXT' and
-- marshal a 'ImageDrmFormatModifierExplicitCreateInfoEXT' into it. The 'VkImageDrmFormatModifierExplicitCreateInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructImageDrmFormatModifierExplicitCreateInfoEXT :: ImageDrmFormatModifierExplicitCreateInfoEXT -> (VkImageDrmFormatModifierExplicitCreateInfoEXT -> IO a) -> IO a
withCStructImageDrmFormatModifierExplicitCreateInfoEXT marshalled cont = withVec withCStructSubresourceLayout (planeLayouts (marshalled :: ImageDrmFormatModifierExplicitCreateInfoEXT)) (\pPPlaneLayouts -> maybeWith withSomeVkStruct (next (marshalled :: ImageDrmFormatModifierExplicitCreateInfoEXT)) (\pPNext -> cont (VkImageDrmFormatModifierExplicitCreateInfoEXT VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT pPNext (drmFormatModifier (marshalled :: ImageDrmFormatModifierExplicitCreateInfoEXT)) (fromIntegral (Data.Vector.length (planeLayouts (marshalled :: ImageDrmFormatModifierExplicitCreateInfoEXT)))) pPPlaneLayouts)))

-- | A function to read a 'VkImageDrmFormatModifierExplicitCreateInfoEXT' and all additional
-- structures in the pointer chain into a 'ImageDrmFormatModifierExplicitCreateInfoEXT'.
fromCStructImageDrmFormatModifierExplicitCreateInfoEXT :: VkImageDrmFormatModifierExplicitCreateInfoEXT -> IO ImageDrmFormatModifierExplicitCreateInfoEXT
fromCStructImageDrmFormatModifierExplicitCreateInfoEXT c = ImageDrmFormatModifierExplicitCreateInfoEXT <$> -- Univalued Member elided
                                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImageDrmFormatModifierExplicitCreateInfoEXT)))
                                                                                                       <*> pure (vkDrmFormatModifier (c :: VkImageDrmFormatModifierExplicitCreateInfoEXT))
                                                                                                       -- Length valued member elided
                                                                                                       <*> (Data.Vector.generateM (fromIntegral (vkDrmFormatModifierPlaneCount (c :: VkImageDrmFormatModifierExplicitCreateInfoEXT))) (((fromCStructSubresourceLayout <=<) . peekElemOff) (vkPPlaneLayouts (c :: VkImageDrmFormatModifierExplicitCreateInfoEXT))))

instance Zero ImageDrmFormatModifierExplicitCreateInfoEXT where
  zero = ImageDrmFormatModifierExplicitCreateInfoEXT Nothing
                                                     zero
                                                     Data.Vector.empty



-- | VkImageDrmFormatModifierListCreateInfoEXT - Specify that an image must
-- be created with a DRM format modifier from the provided list
--
-- == Valid Usage
--
-- -   Each /modifier/ in @pDrmFormatModifiers@ must be compatible with the
--     parameters in 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' and
--     its @pNext@ chain, as determined by querying
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2'
--     extended with
--     'Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier.VkPhysicalDeviceImageDrmFormatModifierInfoEXT'.
--
-- Unresolved directive in VkImageDrmFormatModifierListCreateInfoEXT.txt -
-- include::{generated}\/validity\/structs\/VkImageDrmFormatModifierListCreateInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data ImageDrmFormatModifierListCreateInfoEXT = ImageDrmFormatModifierListCreateInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "ImageDrmFormatModifierListCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "ImageDrmFormatModifierListCreateInfoEXT" "pDrmFormatModifiers"
  drmFormatModifiers :: Vector Word64
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkImageDrmFormatModifierListCreateInfoEXT' and
-- marshal a 'ImageDrmFormatModifierListCreateInfoEXT' into it. The 'VkImageDrmFormatModifierListCreateInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructImageDrmFormatModifierListCreateInfoEXT :: ImageDrmFormatModifierListCreateInfoEXT -> (VkImageDrmFormatModifierListCreateInfoEXT -> IO a) -> IO a
withCStructImageDrmFormatModifierListCreateInfoEXT marshalled cont = withVec (&) (drmFormatModifiers (marshalled :: ImageDrmFormatModifierListCreateInfoEXT)) (\pPDrmFormatModifiers -> maybeWith withSomeVkStruct (next (marshalled :: ImageDrmFormatModifierListCreateInfoEXT)) (\pPNext -> cont (VkImageDrmFormatModifierListCreateInfoEXT VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT pPNext (fromIntegral (Data.Vector.length (drmFormatModifiers (marshalled :: ImageDrmFormatModifierListCreateInfoEXT)))) pPDrmFormatModifiers)))

-- | A function to read a 'VkImageDrmFormatModifierListCreateInfoEXT' and all additional
-- structures in the pointer chain into a 'ImageDrmFormatModifierListCreateInfoEXT'.
fromCStructImageDrmFormatModifierListCreateInfoEXT :: VkImageDrmFormatModifierListCreateInfoEXT -> IO ImageDrmFormatModifierListCreateInfoEXT
fromCStructImageDrmFormatModifierListCreateInfoEXT c = ImageDrmFormatModifierListCreateInfoEXT <$> -- Univalued Member elided
                                                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImageDrmFormatModifierListCreateInfoEXT)))
                                                                                               -- Length valued member elided
                                                                                               <*> (Data.Vector.generateM (fromIntegral (vkDrmFormatModifierCount (c :: VkImageDrmFormatModifierListCreateInfoEXT))) (peekElemOff (vkPDrmFormatModifiers (c :: VkImageDrmFormatModifierListCreateInfoEXT))))

instance Zero ImageDrmFormatModifierListCreateInfoEXT where
  zero = ImageDrmFormatModifierListCreateInfoEXT Nothing
                                                 Data.Vector.empty



-- | VkImageDrmFormatModifierPropertiesEXT - Properties of an image’s Linux
-- DRM format modifier
--
-- = Description
--
-- If the @image@ was created with
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier.VkImageDrmFormatModifierListCreateInfoEXT',
-- then the returned @drmFormatModifier@ /must/ belong to the list of
-- modifiers provided at time of image creation in
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier.VkImageDrmFormatModifierListCreateInfoEXT'::@pDrmFormatModifiers@.
-- If the @image@ was created with
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier.VkImageDrmFormatModifierExplicitCreateInfoEXT',
-- then the returned @drmFormatModifier@ /must/ be the modifier provided at
-- time of image creation in
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier.VkImageDrmFormatModifierExplicitCreateInfoEXT'::@drmFormatModifier@.
--
-- Unresolved directive in VkImageDrmFormatModifierPropertiesEXT.txt -
-- include::{generated}\/validity\/structs\/VkImageDrmFormatModifierPropertiesEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data ImageDrmFormatModifierPropertiesEXT = ImageDrmFormatModifierPropertiesEXT
  { -- Univalued member elided
  -- No documentation found for Nested "ImageDrmFormatModifierPropertiesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageDrmFormatModifierPropertiesEXT" "drmFormatModifier"
  drmFormatModifier :: Word64
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkImageDrmFormatModifierPropertiesEXT' and
-- marshal a 'ImageDrmFormatModifierPropertiesEXT' into it. The 'VkImageDrmFormatModifierPropertiesEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructImageDrmFormatModifierPropertiesEXT :: ImageDrmFormatModifierPropertiesEXT -> (VkImageDrmFormatModifierPropertiesEXT -> IO a) -> IO a
withCStructImageDrmFormatModifierPropertiesEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: ImageDrmFormatModifierPropertiesEXT)) (\pPNext -> cont (VkImageDrmFormatModifierPropertiesEXT VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT pPNext (drmFormatModifier (marshalled :: ImageDrmFormatModifierPropertiesEXT))))

-- | A function to read a 'VkImageDrmFormatModifierPropertiesEXT' and all additional
-- structures in the pointer chain into a 'ImageDrmFormatModifierPropertiesEXT'.
fromCStructImageDrmFormatModifierPropertiesEXT :: VkImageDrmFormatModifierPropertiesEXT -> IO ImageDrmFormatModifierPropertiesEXT
fromCStructImageDrmFormatModifierPropertiesEXT c = ImageDrmFormatModifierPropertiesEXT <$> -- Univalued Member elided
                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImageDrmFormatModifierPropertiesEXT)))
                                                                                       <*> pure (vkDrmFormatModifier (c :: VkImageDrmFormatModifierPropertiesEXT))

instance Zero ImageDrmFormatModifierPropertiesEXT where
  zero = ImageDrmFormatModifierPropertiesEXT Nothing
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
data PhysicalDeviceImageDrmFormatModifierInfoEXT = PhysicalDeviceImageDrmFormatModifierInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceImageDrmFormatModifierInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceImageDrmFormatModifierInfoEXT" "drmFormatModifier"
  drmFormatModifier :: Word64
  , -- No documentation found for Nested "PhysicalDeviceImageDrmFormatModifierInfoEXT" "sharingMode"
  sharingMode :: SharingMode
  -- Length valued member elided
  , -- No documentation found for Nested "PhysicalDeviceImageDrmFormatModifierInfoEXT" "pQueueFamilyIndices"
  queueFamilyIndices :: Vector Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceImageDrmFormatModifierInfoEXT' and
-- marshal a 'PhysicalDeviceImageDrmFormatModifierInfoEXT' into it. The 'VkPhysicalDeviceImageDrmFormatModifierInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceImageDrmFormatModifierInfoEXT :: PhysicalDeviceImageDrmFormatModifierInfoEXT -> (VkPhysicalDeviceImageDrmFormatModifierInfoEXT -> IO a) -> IO a
withCStructPhysicalDeviceImageDrmFormatModifierInfoEXT marshalled cont = withVec (&) (queueFamilyIndices (marshalled :: PhysicalDeviceImageDrmFormatModifierInfoEXT)) (\pPQueueFamilyIndices -> maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceImageDrmFormatModifierInfoEXT)) (\pPNext -> cont (VkPhysicalDeviceImageDrmFormatModifierInfoEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT pPNext (drmFormatModifier (marshalled :: PhysicalDeviceImageDrmFormatModifierInfoEXT)) (sharingMode (marshalled :: PhysicalDeviceImageDrmFormatModifierInfoEXT)) (fromIntegral (Data.Vector.length (queueFamilyIndices (marshalled :: PhysicalDeviceImageDrmFormatModifierInfoEXT)))) pPQueueFamilyIndices)))

-- | A function to read a 'VkPhysicalDeviceImageDrmFormatModifierInfoEXT' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceImageDrmFormatModifierInfoEXT'.
fromCStructPhysicalDeviceImageDrmFormatModifierInfoEXT :: VkPhysicalDeviceImageDrmFormatModifierInfoEXT -> IO PhysicalDeviceImageDrmFormatModifierInfoEXT
fromCStructPhysicalDeviceImageDrmFormatModifierInfoEXT c = PhysicalDeviceImageDrmFormatModifierInfoEXT <$> -- Univalued Member elided
                                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceImageDrmFormatModifierInfoEXT)))
                                                                                                       <*> pure (vkDrmFormatModifier (c :: VkPhysicalDeviceImageDrmFormatModifierInfoEXT))
                                                                                                       <*> pure (vkSharingMode (c :: VkPhysicalDeviceImageDrmFormatModifierInfoEXT))
                                                                                                       -- Length valued member elided
                                                                                                       <*> (Data.Vector.generateM (fromIntegral (vkQueueFamilyIndexCount (c :: VkPhysicalDeviceImageDrmFormatModifierInfoEXT))) (peekElemOff (vkPQueueFamilyIndices (c :: VkPhysicalDeviceImageDrmFormatModifierInfoEXT))))

instance Zero PhysicalDeviceImageDrmFormatModifierInfoEXT where
  zero = PhysicalDeviceImageDrmFormatModifierInfoEXT Nothing
                                                     zero
                                                     zero
                                                     Data.Vector.empty



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
getImageDrmFormatModifierPropertiesEXT :: Device ->  Image ->  IO (ImageDrmFormatModifierPropertiesEXT)
getImageDrmFormatModifierPropertiesEXT = \(Device device' commandTable) -> \image' -> alloca (\pProperties' -> vkGetImageDrmFormatModifierPropertiesEXT commandTable device' image' pProperties' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((fromCStructImageDrmFormatModifierPropertiesEXT <=< peek) pProperties')))
