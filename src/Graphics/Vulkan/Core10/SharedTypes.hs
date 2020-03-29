{-# language CPP #-}
module Graphics.Vulkan.Core10.SharedTypes  ( Offset2D(..)
                                           , Offset3D(..)
                                           , Extent2D(..)
                                           , Extent3D(..)
                                           , ImageSubresourceLayers(..)
                                           , ImageSubresourceRange(..)
                                           , ClearDepthStencilValue(..)
                                           , ClearColorValue(..)
                                           , ClearValue(..)
                                           ) where

import Control.Exception.Base (bracket)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Ptr (castPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (runContT)
import Data.Typeable (Typeable)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(CFloat))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Data.Int (Int32)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import qualified Data.Vector.Storable.Sized (Vector)
import Graphics.Vulkan.CStruct.Utils (lowerArrayPtr)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlags)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Zero (Zero(..))
-- | VkOffset2D - Structure specifying a two-dimensional offset
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.DisplayPlaneCapabilitiesKHR',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.Rect2D',
-- 'Graphics.Vulkan.Extensions.VK_KHR_incremental_present.RectLayerKHR'
data Offset2D = Offset2D
  { -- | @x@ is the x offset.
    x :: Int32
  , -- | @y@ is the y offset.
    y :: Int32
  }
  deriving (Typeable)
deriving instance Show Offset2D

instance ToCStruct Offset2D where
  withCStruct x f = allocaBytesAligned 8 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p Offset2D{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Int32)) (x)
    poke ((p `plusPtr` 4 :: Ptr Int32)) (y)
    f
  cStructSize = 8
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Int32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Int32)) (zero)
    f

instance FromCStruct Offset2D where
  peekCStruct p = do
    x <- peek @Int32 ((p `plusPtr` 0 :: Ptr Int32))
    y <- peek @Int32 ((p `plusPtr` 4 :: Ptr Int32))
    pure $ Offset2D
             x y

instance Storable Offset2D where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero Offset2D where
  zero = Offset2D
           zero
           zero


-- | VkOffset3D - Structure specifying a three-dimensional offset
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.BufferImageCopy',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.ImageBlit',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.ImageCopy',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.ImageResolve',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.SparseImageMemoryBind'
data Offset3D = Offset3D
  { -- | @x@ is the x offset.
    x :: Int32
  , -- | @y@ is the y offset.
    y :: Int32
  , -- | @z@ is the z offset.
    z :: Int32
  }
  deriving (Typeable)
deriving instance Show Offset3D

instance ToCStruct Offset3D where
  withCStruct x f = allocaBytesAligned 12 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p Offset3D{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Int32)) (x)
    poke ((p `plusPtr` 4 :: Ptr Int32)) (y)
    poke ((p `plusPtr` 8 :: Ptr Int32)) (z)
    f
  cStructSize = 12
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Int32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Int32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Int32)) (zero)
    f

instance FromCStruct Offset3D where
  peekCStruct p = do
    x <- peek @Int32 ((p `plusPtr` 0 :: Ptr Int32))
    y <- peek @Int32 ((p `plusPtr` 4 :: Ptr Int32))
    z <- peek @Int32 ((p `plusPtr` 8 :: Ptr Int32))
    pure $ Offset3D
             x y z

instance Storable Offset3D where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero Offset3D where
  zero = Offset3D
           zero
           zero
           zero


-- | VkExtent2D - Structure specifying a two-dimensional extent
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.DisplayModeParametersKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.DisplayPlaneCapabilitiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.DisplayPropertiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.DisplaySurfaceCreateInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.MultisamplePropertiesEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_fragment_density_map.PhysicalDeviceFragmentDensityMapPropertiesEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.PhysicalDeviceSampleLocationsPropertiesEXT',
-- 'Graphics.Vulkan.Extensions.VK_NV_shading_rate_image.PhysicalDeviceShadingRateImagePropertiesNV',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.Rect2D',
-- 'Graphics.Vulkan.Extensions.VK_KHR_incremental_present.RectLayerKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.SampleLocationsInfoEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter.SurfaceCapabilities2EXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR',
-- 'Graphics.Vulkan.Core10.Pass.getRenderAreaGranularity'
data Extent2D = Extent2D
  { -- | @width@ is the width of the extent.
    width :: Word32
  , -- | @height@ is the height of the extent.
    height :: Word32
  }
  deriving (Typeable)
deriving instance Show Extent2D

instance ToCStruct Extent2D where
  withCStruct x f = allocaBytesAligned 8 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p Extent2D{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (width)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (height)
    f
  cStructSize = 8
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    f

instance FromCStruct Extent2D where
  peekCStruct p = do
    width <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    height <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    pure $ Extent2D
             width height

instance Storable Extent2D where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero Extent2D where
  zero = Extent2D
           zero
           zero


-- | VkExtent3D - Structure specifying a three-dimensional extent
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.BufferImageCopy',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.ImageCopy',
-- 'Graphics.Vulkan.Core10.Image.ImageCreateInfo',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.ImageFormatProperties',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.ImageResolve',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.QueueFamilyProperties',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.SparseImageFormatProperties',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.SparseImageMemoryBind'
data Extent3D = Extent3D
  { -- | @width@ is the width of the extent.
    width :: Word32
  , -- | @height@ is the height of the extent.
    height :: Word32
  , -- | @depth@ is the depth of the extent.
    depth :: Word32
  }
  deriving (Typeable)
deriving instance Show Extent3D

instance ToCStruct Extent3D where
  withCStruct x f = allocaBytesAligned 12 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p Extent3D{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (width)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (height)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (depth)
    f
  cStructSize = 12
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    f

instance FromCStruct Extent3D where
  peekCStruct p = do
    width <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    height <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    depth <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    pure $ Extent3D
             width height depth

instance Storable Extent3D where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero Extent3D where
  zero = Extent3D
           zero
           zero
           zero


-- | VkImageSubresourceLayers - Structure specifying an image subresource
-- layers
--
-- == Valid Usage
--
-- -   If @aspectMask@ contains
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT',
--     it /must/ not contain either of
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT'
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- -   @aspectMask@ /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_METADATA_BIT'
--
-- -   @aspectMask@ /must/ not include
--     @VK_IMAGE_ASPECT_MEMORY_PLANE_i_BIT_EXT@ for any index @i@.
--
-- -   @layerCount@ /must/ be greater than 0
--
-- == Valid Usage (Implicit)
--
-- -   @aspectMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlagBits'
--     values
--
-- -   @aspectMask@ /must/ not be @0@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.BufferImageCopy',
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlags',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.ImageBlit',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.ImageCopy',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.ImageResolve'
data ImageSubresourceLayers = ImageSubresourceLayers
  { -- | @aspectMask@ is a combination of
    -- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlagBits',
    -- selecting the color, depth and\/or stencil aspects to be copied.
    aspectMask :: ImageAspectFlags
  , -- | @mipLevel@ is the mipmap level to copy from.
    mipLevel :: Word32
  , -- | @baseArrayLayer@ and @layerCount@ are the starting layer and number of
    -- layers to copy.
    baseArrayLayer :: Word32
  , -- No documentation found for Nested "VkImageSubresourceLayers" "layerCount"
    layerCount :: Word32
  }
  deriving (Typeable)
deriving instance Show ImageSubresourceLayers

instance ToCStruct ImageSubresourceLayers where
  withCStruct x f = allocaBytesAligned 16 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageSubresourceLayers{..} f = do
    poke ((p `plusPtr` 0 :: Ptr ImageAspectFlags)) (aspectMask)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (mipLevel)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (baseArrayLayer)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (layerCount)
    f
  cStructSize = 16
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr ImageAspectFlags)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (zero)
    f

instance FromCStruct ImageSubresourceLayers where
  peekCStruct p = do
    aspectMask <- peek @ImageAspectFlags ((p `plusPtr` 0 :: Ptr ImageAspectFlags))
    mipLevel <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    baseArrayLayer <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    layerCount <- peek @Word32 ((p `plusPtr` 12 :: Ptr Word32))
    pure $ ImageSubresourceLayers
             aspectMask mipLevel baseArrayLayer layerCount

instance Storable ImageSubresourceLayers where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageSubresourceLayers where
  zero = ImageSubresourceLayers
           zero
           zero
           zero
           zero


-- | VkImageSubresourceRange - Structure specifying an image subresource
-- range
--
-- = Description
--
-- The number of mipmap levels and array layers /must/ be a subset of the
-- image subresources in the image. If an application wants to use all mip
-- levels or layers in an image after the @baseMipLevel@ or
-- @baseArrayLayer@, it /can/ set @levelCount@ and @layerCount@ to the
-- special values
-- 'Graphics.Vulkan.Core10.APIConstants.REMAINING_MIP_LEVELS' and
-- 'Graphics.Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS' without
-- knowing the exact number of mip levels or layers.
--
-- For cube and cube array image views, the layers of the image view
-- starting at @baseArrayLayer@ correspond to faces in the order +X, -X,
-- +Y, -Y, +Z, -Z. For cube arrays, each set of six sequential layers is a
-- single cube, so the number of cube maps in a cube map array view is
-- /@layerCount@ \/ 6/, and image array layer (@baseArrayLayer@ + i) is
-- face index (i mod 6) of cube /i \/ 6/. If the number of layers in the
-- view, whether set explicitly in @layerCount@ or implied by
-- 'Graphics.Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS', is not a
-- multiple of 6, the last cube map in the array /must/ not be accessed.
--
-- @aspectMask@ /must/ be only
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT',
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT'
-- or
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
-- if 'Graphics.Vulkan.Core10.Enums.Format.Format' is a color, depth-only
-- or stencil-only format, respectively, except if
-- 'Graphics.Vulkan.Core10.Enums.Format.Format' is a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>.
-- If using a depth\/stencil format with both depth and stencil components,
-- @aspectMask@ /must/ include at least one of
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT'
-- and
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT',
-- and /can/ include both.
--
-- When the 'ImageSubresourceRange' structure is used to select a subset of
-- the slices of a 3D image’s mip level in order to create a 2D or 2D array
-- image view of a 3D image created with
-- 'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT',
-- @baseArrayLayer@ and @layerCount@ specify the first slice index and the
-- number of slices to include in the created image view. Such an image
-- view /can/ be used as a framebuffer attachment that refers only to the
-- specified range of slices of the selected mip level. However, any layout
-- transitions performed on such an attachment view during a render pass
-- instance still apply to the entire subresource referenced which includes
-- all the slices of the selected mip level.
--
-- When using an image view of a depth\/stencil image to populate a
-- descriptor set (e.g. for sampling in the shader, or for use as an input
-- attachment), the @aspectMask@ /must/ only include one bit and selects
-- whether the image view is used for depth reads (i.e. using a
-- floating-point sampler or input attachment in the shader) or stencil
-- reads (i.e. using an unsigned integer sampler or input attachment in the
-- shader). When an image view of a depth\/stencil image is used as a
-- depth\/stencil framebuffer attachment, the @aspectMask@ is ignored and
-- both depth and stencil image subresources are used.
--
-- The 'Graphics.Vulkan.Core10.ImageView.ComponentMapping' @components@
-- member describes a remapping from components of the image to components
-- of the vector returned by shader image instructions. This remapping
-- /must/ be identity for storage image descriptors, input attachment
-- descriptors, framebuffer attachments, and any
-- 'Graphics.Vulkan.Core10.Handles.ImageView' used with a combined image
-- sampler that enables
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>.
--
-- When creating a 'Graphics.Vulkan.Core10.Handles.ImageView', if
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>
-- is enabled in the sampler, the @aspectMask@ of a @subresourceRange@ used
-- by the 'Graphics.Vulkan.Core10.Handles.ImageView' /must/ be
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'.
--
-- When creating a 'Graphics.Vulkan.Core10.Handles.ImageView', if sampler
-- Y′CBCR conversion is not enabled in the sampler and the image
-- 'Graphics.Vulkan.Core10.Enums.Format.Format' is
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar>,
-- the image /must/ have been created with
-- 'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_MUTABLE_FORMAT_BIT',
-- and the @aspectMask@ of the 'Graphics.Vulkan.Core10.Handles.ImageView'’s
-- @subresourceRange@ /must/ be
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT'
-- or
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'.
--
-- == Valid Usage
--
-- -   If @levelCount@ is not
--     'Graphics.Vulkan.Core10.APIConstants.REMAINING_MIP_LEVELS', it
--     /must/ be greater than @0@
--
-- -   If @layerCount@ is not
--     'Graphics.Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS', it
--     /must/ be greater than @0@
--
-- -   If @aspectMask@ includes
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT',
--     then it /must/ not include any of
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT',
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'
--
-- -   @aspectMask@ /must/ not include
--     @VK_IMAGE_ASPECT_MEMORY_PLANE_i_BIT_EXT@ for any index @i@
--
-- == Valid Usage (Implicit)
--
-- -   @aspectMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlagBits'
--     values
--
-- -   @aspectMask@ /must/ not be @0@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlags',
-- 'Graphics.Vulkan.Core10.OtherTypes.ImageMemoryBarrier',
-- 'Graphics.Vulkan.Core10.ImageView.ImageViewCreateInfo',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdClearColorImage',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdClearDepthStencilImage'
data ImageSubresourceRange = ImageSubresourceRange
  { -- | @aspectMask@ is a bitmask of
    -- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlagBits'
    -- specifying which aspect(s) of the image are included in the view.
    aspectMask :: ImageAspectFlags
  , -- | @baseMipLevel@ is the first mipmap level accessible to the view.
    baseMipLevel :: Word32
  , -- | @levelCount@ is the number of mipmap levels (starting from
    -- @baseMipLevel@) accessible to the view.
    levelCount :: Word32
  , -- | @baseArrayLayer@ is the first array layer accessible to the view.
    baseArrayLayer :: Word32
  , -- | @layerCount@ is the number of array layers (starting from
    -- @baseArrayLayer@) accessible to the view.
    layerCount :: Word32
  }
  deriving (Typeable)
deriving instance Show ImageSubresourceRange

instance ToCStruct ImageSubresourceRange where
  withCStruct x f = allocaBytesAligned 20 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageSubresourceRange{..} f = do
    poke ((p `plusPtr` 0 :: Ptr ImageAspectFlags)) (aspectMask)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (baseMipLevel)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (levelCount)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (baseArrayLayer)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (layerCount)
    f
  cStructSize = 20
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr ImageAspectFlags)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct ImageSubresourceRange where
  peekCStruct p = do
    aspectMask <- peek @ImageAspectFlags ((p `plusPtr` 0 :: Ptr ImageAspectFlags))
    baseMipLevel <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    levelCount <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    baseArrayLayer <- peek @Word32 ((p `plusPtr` 12 :: Ptr Word32))
    layerCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ ImageSubresourceRange
             aspectMask baseMipLevel levelCount baseArrayLayer layerCount

instance Storable ImageSubresourceRange where
  sizeOf ~_ = 20
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageSubresourceRange where
  zero = ImageSubresourceRange
           zero
           zero
           zero
           zero
           zero


-- | VkClearDepthStencilValue - Structure specifying a clear depth stencil
-- value
--
-- == Valid Usage
--
-- -   Unless the
--     @https:\/\/www.khronos.org\/registry\/vulkan\/specs\/1.2-extensions\/html\/vkspec.html#VK_EXT_depth_range_unrestricted@
--     extension is enabled @depth@ /must/ be between @0.0@ and @1.0@,
--     inclusive
--
-- = See Also
--
-- 'ClearValue',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdClearDepthStencilImage'
data ClearDepthStencilValue = ClearDepthStencilValue
  { -- | @depth@ is the clear value for the depth aspect of the depth\/stencil
    -- attachment. It is a floating-point value which is automatically
    -- converted to the attachment’s format.
    depth :: Float
  , -- | @stencil@ is the clear value for the stencil aspect of the
    -- depth\/stencil attachment. It is a 32-bit integer value which is
    -- converted to the attachment’s format by taking the appropriate number of
    -- LSBs.
    stencil :: Word32
  }
  deriving (Typeable)
deriving instance Show ClearDepthStencilValue

instance ToCStruct ClearDepthStencilValue where
  withCStruct x f = allocaBytesAligned 8 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ClearDepthStencilValue{..} f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (depth))
    poke ((p `plusPtr` 4 :: Ptr Word32)) (stencil)
    f
  cStructSize = 8
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    f

instance FromCStruct ClearDepthStencilValue where
  peekCStruct p = do
    depth <- peek @CFloat ((p `plusPtr` 0 :: Ptr CFloat))
    stencil <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    pure $ ClearDepthStencilValue
             ((\(CFloat a) -> a) depth) stencil

instance Storable ClearDepthStencilValue where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ClearDepthStencilValue where
  zero = ClearDepthStencilValue
           zero
           zero


data ClearColorValue
  = Float32 ((Float, Float, Float, Float))
  | Int32 ((Int32, Int32, Int32, Int32))
  | Uint32 ((Word32, Word32, Word32, Word32))
  deriving (Show)

instance ToCStruct ClearColorValue where
  withCStruct x f = allocaBytesAligned 16 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct :: Ptr ClearColorValue -> ClearColorValue -> IO a -> IO a
  pokeCStruct p = (. const) . runContT .  \case
    Float32 v -> lift $ do
      let pFloat32 = lowerArrayPtr (castPtr @_ @(Data.Vector.Storable.Sized.Vector 4 CFloat) p)
      case (v) of
        (e0, e1, e2, e3) -> do
          poke (pFloat32 :: Ptr CFloat) (CFloat (e0))
          poke (pFloat32 `plusPtr` 4 :: Ptr CFloat) (CFloat (e1))
          poke (pFloat32 `plusPtr` 8 :: Ptr CFloat) (CFloat (e2))
          poke (pFloat32 `plusPtr` 12 :: Ptr CFloat) (CFloat (e3))
    Int32 v -> lift $ do
      let pInt32 = lowerArrayPtr (castPtr @_ @(Data.Vector.Storable.Sized.Vector 4 Int32) p)
      case (v) of
        (e0, e1, e2, e3) -> do
          poke (pInt32 :: Ptr Int32) (e0)
          poke (pInt32 `plusPtr` 4 :: Ptr Int32) (e1)
          poke (pInt32 `plusPtr` 8 :: Ptr Int32) (e2)
          poke (pInt32 `plusPtr` 12 :: Ptr Int32) (e3)
    Uint32 v -> lift $ do
      let pUint32 = lowerArrayPtr (castPtr @_ @(Data.Vector.Storable.Sized.Vector 4 Word32) p)
      case (v) of
        (e0, e1, e2, e3) -> do
          poke (pUint32 :: Ptr Word32) (e0)
          poke (pUint32 `plusPtr` 4 :: Ptr Word32) (e1)
          poke (pUint32 `plusPtr` 8 :: Ptr Word32) (e2)
          poke (pUint32 `plusPtr` 12 :: Ptr Word32) (e3)
  pokeZeroCStruct :: Ptr ClearColorValue -> IO b -> IO b
  pokeZeroCStruct _ f = f
  cStructSize = 16
  cStructAlignment = 4

instance Zero ClearColorValue where
  zero = Float32 (zero, zero, zero, zero)


data ClearValue
  = Color ClearColorValue
  | DepthStencil ClearDepthStencilValue
  deriving (Show)

instance ToCStruct ClearValue where
  withCStruct x f = allocaBytesAligned 16 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct :: Ptr ClearValue -> ClearValue -> IO a -> IO a
  pokeCStruct p = (. const) . runContT .  \case
    Color v -> ContT $ pokeCStruct (castPtr @_ @ClearColorValue p) (v) . ($ ())
    DepthStencil v -> ContT $ pokeCStruct (castPtr @_ @ClearDepthStencilValue p) (v) . ($ ())
  pokeZeroCStruct :: Ptr ClearValue -> IO b -> IO b
  pokeZeroCStruct _ f = f
  cStructSize = 16
  cStructAlignment = 4

instance Zero ClearValue where
  zero = Color zero

