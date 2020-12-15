{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_EXT_separate_stencil_usage"
module Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage  ( ImageStencilUsageCreateInfo(..)
                                                                  , StructureType(..)
                                                                  ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
-- | VkImageStencilUsageCreateInfo - Specify separate usage flags for the
-- stencil aspect of a depth-stencil image
--
-- = Description
--
-- If the @pNext@ chain of 'Vulkan.Core10.Image.ImageCreateInfo' includes a
-- 'ImageStencilUsageCreateInfo' structure, then that structure includes
-- the usage flags specific to the stencil aspect of the image for an image
-- with a depth-stencil format.
--
-- This structure specifies image usages which only apply to the stencil
-- aspect of a depth\/stencil format image. When this structure is included
-- in the @pNext@ chain of 'Vulkan.Core10.Image.ImageCreateInfo', the
-- stencil aspect of the image /must/ only be used as specified by
-- @stencilUsage@. When this structure is not included in the @pNext@ chain
-- of 'Vulkan.Core10.Image.ImageCreateInfo', the stencil aspect of an image
-- /must/ only be used as specified
-- 'Vulkan.Core10.Image.ImageCreateInfo'::@usage@. Use of other aspects of
-- an image are unaffected by this structure.
--
-- This structure /can/ also be included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2'
-- to query additional capabilities specific to image creation parameter
-- combinations including a separate set of usage flags for the stencil
-- aspect of the image using
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'.
-- When this structure is not included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2'
-- then the implicit value of @stencilUsage@ matches that of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2'::@usage@.
--
-- == Valid Usage
--
-- -   #VUID-VkImageStencilUsageCreateInfo-stencilUsage-02539# If
--     @stencilUsage@ includes
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT',
--     it /must/ not include bits other than
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--     or
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImageStencilUsageCreateInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO'
--
-- -   #VUID-VkImageStencilUsageCreateInfo-stencilUsage-parameter#
--     @stencilUsage@ /must/ be a valid combination of
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits' values
--
-- -   #VUID-VkImageStencilUsageCreateInfo-stencilUsage-requiredbitmask#
--     @stencilUsage@ /must/ not be @0@
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ImageStencilUsageCreateInfo = ImageStencilUsageCreateInfo
  { -- | @stencilUsage@ is a bitmask of
    -- 'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits' describing
    -- the intended usage of the stencil aspect of the image.
    stencilUsage :: ImageUsageFlags }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageStencilUsageCreateInfo)
#endif
deriving instance Show ImageStencilUsageCreateInfo

instance ToCStruct ImageStencilUsageCreateInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageStencilUsageCreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageUsageFlags)) (stencilUsage)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageUsageFlags)) (zero)
    f

instance FromCStruct ImageStencilUsageCreateInfo where
  peekCStruct p = do
    stencilUsage <- peek @ImageUsageFlags ((p `plusPtr` 16 :: Ptr ImageUsageFlags))
    pure $ ImageStencilUsageCreateInfo
             stencilUsage

instance Storable ImageStencilUsageCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageStencilUsageCreateInfo where
  zero = ImageStencilUsageCreateInfo
           zero

