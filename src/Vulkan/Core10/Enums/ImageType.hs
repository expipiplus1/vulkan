{-# language CPP #-}
-- No documentation found for Chapter "ImageType"
module Vulkan.Core10.Enums.ImageType  (ImageType( IMAGE_TYPE_1D
                                                , IMAGE_TYPE_2D
                                                , IMAGE_TYPE_3D
                                                , ..
                                                )) where

import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Zero (Zero)
-- | VkImageType - Specifies the type of an image object
--
-- = See Also
--
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

instance Show ImageType where
  showsPrec p = \case
    IMAGE_TYPE_1D -> showString "IMAGE_TYPE_1D"
    IMAGE_TYPE_2D -> showString "IMAGE_TYPE_2D"
    IMAGE_TYPE_3D -> showString "IMAGE_TYPE_3D"
    ImageType x -> showParen (p >= 11) (showString "ImageType " . showsPrec 11 x)

instance Read ImageType where
  readPrec = parens (choose [("IMAGE_TYPE_1D", pure IMAGE_TYPE_1D)
                            , ("IMAGE_TYPE_2D", pure IMAGE_TYPE_2D)
                            , ("IMAGE_TYPE_3D", pure IMAGE_TYPE_3D)]
                     +++
                     prec 10 (do
                       expectP (Ident "ImageType")
                       v <- step readPrec
                       pure (ImageType v)))

