{-# language CPP #-}
-- No documentation found for Chapter "ImageTiling"
module Vulkan.Core10.Enums.ImageTiling  (ImageTiling( IMAGE_TILING_OPTIMAL
                                                    , IMAGE_TILING_LINEAR
                                                    , IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT
                                                    , ..
                                                    )) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))

-- | VkImageTiling - Specifies the tiling arrangement of data in an image
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Image.ImageCreateInfo',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceSparseImageFormatInfo2',
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoFormatPropertiesKHR VkVideoFormatPropertiesKHR>,
-- 'Vulkan.Extensions.VK_NV_external_memory_capabilities.getPhysicalDeviceExternalImageFormatPropertiesNV',
-- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceImageFormatProperties',
-- 'Vulkan.Core10.SparseResourceMemoryManagement.getPhysicalDeviceSparseImageFormatProperties'
newtype ImageTiling = ImageTiling Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'IMAGE_TILING_OPTIMAL' specifies optimal tiling (texels are laid out in
-- an implementation-dependent arrangement, for more efficient memory
-- access).
pattern IMAGE_TILING_OPTIMAL                 = ImageTiling 0
-- | 'IMAGE_TILING_LINEAR' specifies linear tiling (texels are laid out in
-- memory in row-major order, possibly with some padding on each row).
pattern IMAGE_TILING_LINEAR                  = ImageTiling 1
-- | 'IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT' indicates that the image’s tiling
-- is defined by a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#glossary-drm-format-modifier Linux DRM format modifier>.
-- The modifier is specified at image creation with
-- 'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.ImageDrmFormatModifierListCreateInfoEXT'
-- or
-- 'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.ImageDrmFormatModifierExplicitCreateInfoEXT',
-- and /can/ be queried with
-- 'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.getImageDrmFormatModifierPropertiesEXT'.
pattern IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT = ImageTiling 1000158000
{-# complete IMAGE_TILING_OPTIMAL,
             IMAGE_TILING_LINEAR,
             IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT :: ImageTiling #-}

conNameImageTiling :: String
conNameImageTiling = "ImageTiling"

enumPrefixImageTiling :: String
enumPrefixImageTiling = "IMAGE_TILING_"

showTableImageTiling :: [(ImageTiling, String)]
showTableImageTiling =
  [ (IMAGE_TILING_OPTIMAL                , "OPTIMAL")
  , (IMAGE_TILING_LINEAR                 , "LINEAR")
  , (IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT, "DRM_FORMAT_MODIFIER_EXT")
  ]

instance Show ImageTiling where
  showsPrec =
    enumShowsPrec enumPrefixImageTiling showTableImageTiling conNameImageTiling (\(ImageTiling x) -> x) (showsPrec 11)

instance Read ImageTiling where
  readPrec = enumReadPrec enumPrefixImageTiling showTableImageTiling conNameImageTiling ImageTiling

