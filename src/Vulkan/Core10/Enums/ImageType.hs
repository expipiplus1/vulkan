{-# language CPP #-}
-- No documentation found for Chapter "ImageType"
module Vulkan.Core10.Enums.ImageType  (ImageType( IMAGE_TYPE_1D
                                                , IMAGE_TYPE_2D
                                                , IMAGE_TYPE_3D
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

-- | VkImageType - Specifies the type of an image object
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Image.ImageCreateInfo',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceSparseImageFormatInfo2',
-- 'Vulkan.Extensions.VK_NV_external_memory_capabilities.getPhysicalDeviceExternalImageFormatPropertiesNV',
-- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceImageFormatProperties',
-- 'Vulkan.Core10.SparseResourceMemoryManagement.getPhysicalDeviceSparseImageFormatProperties'
newtype ImageType = ImageType Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'IMAGE_TYPE_1D' specifies a one-dimensional image.
pattern IMAGE_TYPE_1D = ImageType 0
-- | 'IMAGE_TYPE_2D' specifies a two-dimensional image.
pattern IMAGE_TYPE_2D = ImageType 1
-- | 'IMAGE_TYPE_3D' specifies a three-dimensional image.
pattern IMAGE_TYPE_3D = ImageType 2
{-# complete IMAGE_TYPE_1D,
             IMAGE_TYPE_2D,
             IMAGE_TYPE_3D :: ImageType #-}

conNameImageType :: String
conNameImageType = "ImageType"

enumPrefixImageType :: String
enumPrefixImageType = "IMAGE_TYPE_"

showTableImageType :: [(ImageType, String)]
showTableImageType = [(IMAGE_TYPE_1D, "1D"), (IMAGE_TYPE_2D, "2D"), (IMAGE_TYPE_3D, "3D")]

instance Show ImageType where
  showsPrec =
    enumShowsPrec enumPrefixImageType showTableImageType conNameImageType (\(ImageType x) -> x) (showsPrec 11)

instance Read ImageType where
  readPrec = enumReadPrec enumPrefixImageType showTableImageType conNameImageType ImageType

