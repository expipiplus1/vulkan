{-# language CPP #-}
-- No documentation found for Chapter "ImageView"
module Vulkan.Core10.ImageView  ( createImageView
                                , withImageView
                                , destroyImageView
                                , ComponentMapping(..)
                                , ImageSubresourceRange(..)
                                , ImageViewCreateInfo(..)
                                , ImageView(..)
                                , ImageViewType(..)
                                , ComponentSwizzle(..)
                                , ImageViewCreateFlagBits(..)
                                , ImageViewCreateFlags
                                ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Enums.ComponentSwizzle (ComponentSwizzle)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkCreateImageView))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyImageView))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.Core10.Handles (Image)
import Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlags)
import Vulkan.Core10.Handles (ImageView)
import Vulkan.Core10.Handles (ImageView(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_astc_decode_mode (ImageViewASTCDecodeModeEXT)
import Vulkan.Core10.Enums.ImageViewCreateFlagBits (ImageViewCreateFlags)
import Vulkan.Core10.Enums.ImageViewType (ImageViewType)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_maintenance2 (ImageViewUsageCreateInfo)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (SamplerYcbcrConversionInfo)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Enums.ComponentSwizzle (ComponentSwizzle(..))
import Vulkan.Core10.Handles (ImageView(..))
import Vulkan.Core10.Enums.ImageViewCreateFlagBits (ImageViewCreateFlagBits(..))
import Vulkan.Core10.Enums.ImageViewCreateFlagBits (ImageViewCreateFlags)
import Vulkan.Core10.Enums.ImageViewType (ImageViewType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateImageView
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct ImageViewCreateInfo) -> Ptr AllocationCallbacks -> Ptr ImageView -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct ImageViewCreateInfo) -> Ptr AllocationCallbacks -> Ptr ImageView -> IO Result

-- | vkCreateImageView - Create an image view from an existing image
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateImageView-device-parameter# @device@ /must/ be a valid
--     'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateImageView-pCreateInfo-parameter# @pCreateInfo@ /must/
--     be a valid pointer to a valid 'ImageViewCreateInfo' structure
--
-- -   #VUID-vkCreateImageView-pAllocator-parameter# If @pAllocator@ is not
--     @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkCreateImageView-pView-parameter# @pView@ /must/ be a valid
--     pointer to a 'Vulkan.Core10.Handles.ImageView' handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.ImageView',
-- 'ImageViewCreateInfo'
createImageView :: forall a io
                 . (Extendss ImageViewCreateInfo a, PokeChain a, MonadIO io)
                => -- | @device@ is the logical device that creates the image view.
                   Device
                -> -- | @pCreateInfo@ is a pointer to a 'ImageViewCreateInfo' structure
                   -- containing parameters to be used to create the image view.
                   (ImageViewCreateInfo a)
                -> -- | @pAllocator@ controls host memory allocation as described in the
                   -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                   -- chapter.
                   ("allocator" ::: Maybe AllocationCallbacks)
                -> io (ImageView)
createImageView device createInfo allocator = liftIO . evalContT $ do
  let vkCreateImageViewPtr = pVkCreateImageView (deviceCmds (device :: Device))
  lift $ unless (vkCreateImageViewPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateImageView is null" Nothing Nothing
  let vkCreateImageView' = mkVkCreateImageView vkCreateImageViewPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPView <- ContT $ bracket (callocBytes @ImageView 8) free
  r <- lift $ traceAroundEvent "vkCreateImageView" (vkCreateImageView' (deviceHandle (device)) (forgetExtensions pCreateInfo) pAllocator (pPView))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pView <- lift $ peek @ImageView pPView
  pure $ (pView)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createImageView' and 'destroyImageView'
--
-- To ensure that 'destroyImageView' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withImageView :: forall a io r . (Extendss ImageViewCreateInfo a, PokeChain a, MonadIO io) => Device -> ImageViewCreateInfo a -> Maybe AllocationCallbacks -> (io ImageView -> (ImageView -> io ()) -> r) -> r
withImageView device pCreateInfo pAllocator b =
  b (createImageView device pCreateInfo pAllocator)
    (\(o0) -> destroyImageView device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyImageView
  :: FunPtr (Ptr Device_T -> ImageView -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> ImageView -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyImageView - Destroy an image view object
--
-- == Valid Usage
--
-- -   #VUID-vkDestroyImageView-imageView-01026# All submitted commands
--     that refer to @imageView@ /must/ have completed execution
--
-- -   #VUID-vkDestroyImageView-imageView-01027# If
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @imageView@ was created, a compatible set of callbacks
--     /must/ be provided here
--
-- -   #VUID-vkDestroyImageView-imageView-01028# If no
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @imageView@ was created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroyImageView-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDestroyImageView-imageView-parameter# If @imageView@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @imageView@ /must/ be a
--     valid 'Vulkan.Core10.Handles.ImageView' handle
--
-- -   #VUID-vkDestroyImageView-pAllocator-parameter# If @pAllocator@ is
--     not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkDestroyImageView-imageView-parent# If @imageView@ is a valid
--     handle, it /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Host Synchronization
--
-- -   Host access to @imageView@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.ImageView'
destroyImageView :: forall io
                  . (MonadIO io)
                 => -- | @device@ is the logical device that destroys the image view.
                    Device
                 -> -- | @imageView@ is the image view to destroy.
                    ImageView
                 -> -- | @pAllocator@ controls host memory allocation as described in the
                    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                    -- chapter.
                    ("allocator" ::: Maybe AllocationCallbacks)
                 -> io ()
destroyImageView device imageView allocator = liftIO . evalContT $ do
  let vkDestroyImageViewPtr = pVkDestroyImageView (deviceCmds (device :: Device))
  lift $ unless (vkDestroyImageViewPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyImageView is null" Nothing Nothing
  let vkDestroyImageView' = mkVkDestroyImageView vkDestroyImageViewPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyImageView" (vkDestroyImageView' (deviceHandle (device)) (imageView) pAllocator)
  pure $ ()


-- | VkComponentMapping - Structure specifying a color component mapping
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.AndroidHardwareBufferFormatPropertiesANDROID',
-- 'Vulkan.Core10.Enums.ComponentSwizzle.ComponentSwizzle',
-- 'ImageViewCreateInfo',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionCreateInfo'
data ComponentMapping = ComponentMapping
  { -- | @r@ is a 'Vulkan.Core10.Enums.ComponentSwizzle.ComponentSwizzle'
    -- specifying the component value placed in the R component of the output
    -- vector.
    --
    -- #VUID-VkComponentMapping-r-parameter# @r@ /must/ be a valid
    -- 'Vulkan.Core10.Enums.ComponentSwizzle.ComponentSwizzle' value
    r :: ComponentSwizzle
  , -- | @g@ is a 'Vulkan.Core10.Enums.ComponentSwizzle.ComponentSwizzle'
    -- specifying the component value placed in the G component of the output
    -- vector.
    --
    -- #VUID-VkComponentMapping-g-parameter# @g@ /must/ be a valid
    -- 'Vulkan.Core10.Enums.ComponentSwizzle.ComponentSwizzle' value
    g :: ComponentSwizzle
  , -- | @b@ is a 'Vulkan.Core10.Enums.ComponentSwizzle.ComponentSwizzle'
    -- specifying the component value placed in the B component of the output
    -- vector.
    --
    -- #VUID-VkComponentMapping-b-parameter# @b@ /must/ be a valid
    -- 'Vulkan.Core10.Enums.ComponentSwizzle.ComponentSwizzle' value
    b :: ComponentSwizzle
  , -- | @a@ is a 'Vulkan.Core10.Enums.ComponentSwizzle.ComponentSwizzle'
    -- specifying the component value placed in the A component of the output
    -- vector.
    --
    -- #VUID-VkComponentMapping-a-parameter# @a@ /must/ be a valid
    -- 'Vulkan.Core10.Enums.ComponentSwizzle.ComponentSwizzle' value
    a :: ComponentSwizzle
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ComponentMapping)
#endif
deriving instance Show ComponentMapping

instance ToCStruct ComponentMapping where
  withCStruct x f = allocaBytesAligned 16 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ComponentMapping{..} f = do
    poke ((p `plusPtr` 0 :: Ptr ComponentSwizzle)) (r)
    poke ((p `plusPtr` 4 :: Ptr ComponentSwizzle)) (g)
    poke ((p `plusPtr` 8 :: Ptr ComponentSwizzle)) (b)
    poke ((p `plusPtr` 12 :: Ptr ComponentSwizzle)) (a)
    f
  cStructSize = 16
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr ComponentSwizzle)) (zero)
    poke ((p `plusPtr` 4 :: Ptr ComponentSwizzle)) (zero)
    poke ((p `plusPtr` 8 :: Ptr ComponentSwizzle)) (zero)
    poke ((p `plusPtr` 12 :: Ptr ComponentSwizzle)) (zero)
    f

instance FromCStruct ComponentMapping where
  peekCStruct p = do
    r <- peek @ComponentSwizzle ((p `plusPtr` 0 :: Ptr ComponentSwizzle))
    g <- peek @ComponentSwizzle ((p `plusPtr` 4 :: Ptr ComponentSwizzle))
    b <- peek @ComponentSwizzle ((p `plusPtr` 8 :: Ptr ComponentSwizzle))
    a <- peek @ComponentSwizzle ((p `plusPtr` 12 :: Ptr ComponentSwizzle))
    pure $ ComponentMapping
             r g b a

instance Storable ComponentMapping where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ComponentMapping where
  zero = ComponentMapping
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
-- special values 'Vulkan.Core10.APIConstants.REMAINING_MIP_LEVELS' and
-- 'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS' without knowing the
-- exact number of mip levels or layers.
--
-- For cube and cube array image views, the layers of the image view
-- starting at @baseArrayLayer@ correspond to faces in the order +X, -X,
-- +Y, -Y, +Z, -Z. For cube arrays, each set of six sequential layers is a
-- single cube, so the number of cube maps in a cube map array view is
-- /@layerCount@ \/ 6/, and image array layer (@baseArrayLayer@ + i) is
-- face index (i mod 6) of cube /i \/ 6/. If the number of layers in the
-- view, whether set explicitly in @layerCount@ or implied by
-- 'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS', is not a multiple
-- of 6, the last cube map in the array /must/ not be accessed.
--
-- @aspectMask@ /must/ be only
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT',
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT' or
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT' if
-- @format@ is a color, depth-only or stencil-only format, respectively,
-- except if @format@ is a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>.
-- If using a depth\/stencil format with both depth and stencil components,
-- @aspectMask@ /must/ include at least one of
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT' and
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT', and
-- /can/ include both.
--
-- When the 'ImageSubresourceRange' structure is used to select a subset of
-- the slices of a 3D image’s mip level in order to create a 2D or 2D array
-- image view of a 3D image created with
-- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT',
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
-- When creating a 'Vulkan.Core10.Handles.ImageView', if
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>
-- is enabled in the sampler, the @aspectMask@ of a @subresourceRange@ used
-- by the 'Vulkan.Core10.Handles.ImageView' /must/ be
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'.
--
-- When creating a 'Vulkan.Core10.Handles.ImageView', if sampler Y′CBCR
-- conversion is not enabled in the sampler and the image @format@ is
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar>,
-- the image /must/ have been created with
-- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_MUTABLE_FORMAT_BIT',
-- and the @aspectMask@ of the 'Vulkan.Core10.Handles.ImageView'’s
-- @subresourceRange@ /must/ be
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT' or
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'.
--
-- == Valid Usage
--
-- -   #VUID-VkImageSubresourceRange-levelCount-01720# If @levelCount@ is
--     not 'Vulkan.Core10.APIConstants.REMAINING_MIP_LEVELS', it /must/ be
--     greater than @0@
--
-- -   #VUID-VkImageSubresourceRange-layerCount-01721# If @layerCount@ is
--     not 'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS', it /must/
--     be greater than @0@
--
-- -   #VUID-VkImageSubresourceRange-aspectMask-01670# If @aspectMask@
--     includes
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT',
--     then it /must/ not include any of
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT',
--     or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'
--
-- -   #VUID-VkImageSubresourceRange-aspectMask-02278# @aspectMask@ /must/
--     not include @VK_IMAGE_ASPECT_MEMORY_PLANE_i_BIT_EXT@ for any index
--     @i@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImageSubresourceRange-aspectMask-parameter# @aspectMask@
--     /must/ be a valid combination of
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlagBits' values
--
-- -   #VUID-VkImageSubresourceRange-aspectMask-requiredbitmask#
--     @aspectMask@ /must/ not be @0@
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlags',
-- 'Vulkan.Core10.OtherTypes.ImageMemoryBarrier', 'ImageViewCreateInfo',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdClearColorImage',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdClearDepthStencilImage'
data ImageSubresourceRange = ImageSubresourceRange
  { -- | @aspectMask@ is a bitmask of
    -- 'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlagBits' specifying
    -- which aspect(s) of the image are included in the view.
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
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageSubresourceRange)
#endif
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


-- | VkImageViewCreateInfo - Structure specifying parameters of a newly
-- created image view
--
-- = Description
--
-- Some of the @image@ creation parameters are inherited by the view. In
-- particular, image view creation inherits the implicit parameter @usage@
-- specifying the allowed usages of the image view that, by default, takes
-- the value of the corresponding @usage@ parameter specified in
-- 'Vulkan.Core10.Image.ImageCreateInfo' at image creation time. The
-- implicit @usage@ /can/ be overriden by adding a
-- 'Vulkan.Core11.Promoted_From_VK_KHR_maintenance2.ImageViewUsageCreateInfo'
-- structure to the @pNext@ chain, but the view usage /must/ be a subset of
-- the image usage. If the image was has a depth-stencil format and was
-- created with a
-- 'Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'
-- structure included in the @pNext@ chain of
-- 'Vulkan.Core10.Image.ImageCreateInfo', the usage is calculated based on
-- the @subresource.aspectMask@ provided:
--
-- -   If @aspectMask@ includes only
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT',
--     the implicit @usage@ is equal to
--     'Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'::@stencilUsage@.
--
-- -   If @aspectMask@ includes only
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT',
--     the implicit @usage@ is equal to
--     'Vulkan.Core10.Image.ImageCreateInfo'::@usage@.
--
-- -   If both aspects are included in @aspectMask@, the implicit @usage@
--     is equal to the intersection of
--     'Vulkan.Core10.Image.ImageCreateInfo'::@usage@ and
--     'Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'::@stencilUsage@.
--
-- If @image@ was created with the
-- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_MUTABLE_FORMAT_BIT'
-- flag, and if the @format@ of the image is not
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar>,
-- @format@ /can/ be different from the image’s format, but if @image@ was
-- created without the
-- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT'
-- flag and they are not equal they /must/ be /compatible/. Image format
-- compatibility is defined in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes Format Compatibility Classes>
-- section. Views of compatible formats will have the same mapping between
-- texel coordinates and memory locations irrespective of the @format@,
-- with only the interpretation of the bit pattern changing.
--
-- Note
--
-- Values intended to be used with one view format /may/ not be exactly
-- preserved when written or read through a different format. For example,
-- an integer value that happens to have the bit pattern of a floating
-- point denorm or NaN /may/ be flushed or canonicalized when written or
-- read through a view with a floating point format. Similarly, a value
-- written through a signed normalized format that has a bit pattern
-- exactly equal to -2b /may/ be changed to -2b + 1 as described in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-fixedfpconv Conversion from Normalized Fixed-Point to Floating-Point>.
--
-- If @image@ was created with the
-- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT'
-- flag, @format@ /must/ be /compatible/ with the image’s format as
-- described above, or /must/ be an uncompressed format in which case it
-- /must/ be /size-compatible/ with the image’s format, as defined for
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#copies-images-format-size-compatibility copying data between images>
-- In this case the resulting image view’s texel dimensions equal the
-- dimensions of the selected mip level divided by the compressed texel
-- block size and rounded up.
--
-- The 'ComponentMapping' @components@ member describes a remapping from
-- components of the image to components of the vector returned by shader
-- image instructions. This remapping /must/ be the identity swizzle for
-- storage image descriptors, input attachment descriptors, framebuffer
-- attachments, and any 'Vulkan.Core10.Handles.ImageView' used with a
-- combined image sampler that enables
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y’CBCR conversion>.
--
-- If the image view is to be used with a sampler which supports
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>,
-- an /identically defined object/ of type
-- 'Vulkan.Core11.Handles.SamplerYcbcrConversion' to that used to create
-- the sampler /must/ be passed to 'createImageView' in a
-- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionInfo'
-- included in the @pNext@ chain of 'ImageViewCreateInfo'. Conversely, if a
-- 'Vulkan.Core11.Handles.SamplerYcbcrConversion' object is passed to
-- 'createImageView', an identically defined
-- 'Vulkan.Core11.Handles.SamplerYcbcrConversion' object /must/ be used
-- when sampling the image.
--
-- If the image has a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar>
-- @format@ and @subresourceRange.aspectMask@ is
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT',
-- @format@ /must/ be identical to the image @format@, and the sampler to
-- be used with the image view /must/ enable
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>.
--
-- If @image@ was created with the
-- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_MUTABLE_FORMAT_BIT'
-- and the image has a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar>
-- @format@, and if @subresourceRange.aspectMask@ is
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT', or
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT',
-- @format@ /must/ be
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatible-planes compatible>
-- with the corresponding plane of the image, and the sampler to be used
-- with the image view /must/ not enable
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>.
-- The @width@ and @height@ of the single-plane image view /must/ be
-- derived from the multi-planar image’s dimensions in the manner listed
-- for
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatible-planes plane compatibility>
-- for the plane.
--
-- Any view of an image plane will have the same mapping between texel
-- coordinates and memory locations as used by the channels of the color
-- aspect, subject to the formulae relating texel coordinates to
-- lower-resolution planes as described in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-chroma-reconstruction Chroma Reconstruction>.
-- That is, if an R or B plane has a reduced resolution relative to the G
-- plane of the multi-planar image, the image view operates using the
-- (/uplane/, /vplane/) unnormalized coordinates of the reduced-resolution
-- plane, and these coordinates access the same memory locations as the
-- (/ucolor/, /vcolor/) unnormalized coordinates of the color aspect for
-- which chroma reconstruction operations operate on the same (/uplane/,
-- /vplane/) or (/iplane/, /jplane/) coordinates.
--
-- +----------+--------------------------------------------------------------------------------+----------------------------------------------------------------+
-- | Dim,     | Image parameters                                                               | View parameters                                                |
-- | Arrayed, |                                                                                |                                                                |
-- | MS       |                                                                                |                                                                |
-- +==========+================================================================================+================================================================+
-- |          | @imageType@ = ci.@imageType@                                                   | @baseArrayLayer@, @layerCount@, and @levelCount@ are members   |
-- |          | @width@ = ci.@extent.width@                                                    | of the @subresourceRange@ member.                              |
-- |          | @height@ = ci.@extent.height@                                                  |                                                                |
-- |          | @depth@ = ci.@extent.depth@                                                    |                                                                |
-- |          | @arrayLayers@ = ci.@arrayLayers@                                               |                                                                |
-- |          | @samples@ = ci.@samples@                                                       |                                                                |
-- |          | @flags@ = ci.@flags@                                                           |                                                                |
-- |          | where ci is the 'Vulkan.Core10.Image.ImageCreateInfo' used to create @image@.  |                                                                |
-- +----------+--------------------------------------------------------------------------------+----------------------------------------------------------------+
-- | __1D, 0, | @imageType@ = 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D'                    | @viewType@ =                                                   |
-- | 0__      | @width@ ≥ 1                                                                    | 'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_1D'         |
-- |          | @height@ = 1                                                                   | @baseArrayLayer@ ≥ 0                                           |
-- |          | @depth@ = 1                                                                    | @layerCount@ = 1                                               |
-- |          | @arrayLayers@ ≥ 1                                                              |                                                                |
-- |          | @samples@ = 1                                                                  |                                                                |
-- +----------+--------------------------------------------------------------------------------+----------------------------------------------------------------+
-- | __1D, 1, | @imageType@ = 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D'                    | @viewType@ =                                                   |
-- | 0__      | @width@ ≥ 1                                                                    | 'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_1D_ARRAY'   |
-- |          | @height@ = 1                                                                   | @baseArrayLayer@ ≥ 0                                           |
-- |          | @depth@ = 1                                                                    | @layerCount@ ≥ 1                                               |
-- |          | @arrayLayers@ ≥ 1                                                              |                                                                |
-- |          | @samples@ = 1                                                                  |                                                                |
-- +----------+--------------------------------------------------------------------------------+----------------------------------------------------------------+
-- | __2D, 0, | @imageType@ = 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'                    | @viewType@ =                                                   |
-- | 0__      | @width@ ≥ 1                                                                    | 'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D'         |
-- |          | @height@ ≥ 1                                                                   | @baseArrayLayer@ ≥ 0                                           |
-- |          | @depth@ = 1                                                                    | @layerCount@ = 1                                               |
-- |          | @arrayLayers@ ≥ 1                                                              |                                                                |
-- |          | @samples@ = 1                                                                  |                                                                |
-- +----------+--------------------------------------------------------------------------------+----------------------------------------------------------------+
-- | __2D, 1, | @imageType@ = 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'                    | @viewType@ =                                                   |
-- | 0__      | @width@ ≥ 1                                                                    | 'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY'   |
-- |          | @height@ ≥ 1                                                                   | @baseArrayLayer@ ≥ 0                                           |
-- |          | @depth@ = 1                                                                    | @layerCount@ ≥ 1                                               |
-- |          | @arrayLayers@ ≥ 1                                                              |                                                                |
-- |          | @samples@ = 1                                                                  |                                                                |
-- +----------+--------------------------------------------------------------------------------+----------------------------------------------------------------+
-- | __2D, 0, | @imageType@ = 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'                    | @viewType@ =                                                   |
-- | 1__      | @width@ ≥ 1                                                                    | 'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D'         |
-- |          | @height@ ≥ 1                                                                   | @baseArrayLayer@ ≥ 0                                           |
-- |          | @depth@ = 1                                                                    | @layerCount@ = 1                                               |
-- |          | @arrayLayers@ ≥ 1                                                              |                                                                |
-- |          | @samples@ > 1                                                                  |                                                                |
-- +----------+--------------------------------------------------------------------------------+----------------------------------------------------------------+
-- | __2D, 1, | @imageType@ = 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'                    | @viewType@ =                                                   |
-- | 1__      | @width@ ≥ 1                                                                    | 'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY'   |
-- |          | @height@ ≥ 1                                                                   | @baseArrayLayer@ ≥ 0                                           |
-- |          | @depth@ = 1                                                                    | @layerCount@ ≥ 1                                               |
-- |          | @arrayLayers@ ≥ 1                                                              |                                                                |
-- |          | @samples@ > 1                                                                  |                                                                |
-- +----------+--------------------------------------------------------------------------------+----------------------------------------------------------------+
-- | __CUBE,  | @imageType@ = 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'                    | @viewType@ =                                                   |
-- | 0, 0__   | @width@ ≥ 1                                                                    | 'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE'       |
-- |          | @height@ = @width@                                                             | @baseArrayLayer@ ≥ 0                                           |
-- |          | @depth@ = 1                                                                    | @layerCount@ = 6                                               |
-- |          | @arrayLayers@ ≥ 6                                                              |                                                                |
-- |          | @samples@ = 1                                                                  |                                                                |
-- |          | @flags@ includes                                                               |                                                                |
-- |          | 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CUBE_COMPATIBLE_BIT'     |                                                                |
-- +----------+--------------------------------------------------------------------------------+----------------------------------------------------------------+
-- | __CUBE,  | @imageType@ = 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'                    | @viewType@ =                                                   |
-- | 1, 0__   | @width@ ≥ 1                                                                    | 'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY' |
-- |          | @height@ = width                                                               | @baseArrayLayer@ ≥ 0                                           |
-- |          | @depth@ = 1                                                                    | @layerCount@ = 6 × /N/, /N/ ≥ 1                                |
-- |          | /N/ ≥ 1                                                                        |                                                                |
-- |          | @arrayLayers@ ≥ 6 × /N/                                                        |                                                                |
-- |          | @samples@ = 1                                                                  |                                                                |
-- |          | @flags@ includes                                                               |                                                                |
-- |          | 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CUBE_COMPATIBLE_BIT'     |                                                                |
-- +----------+--------------------------------------------------------------------------------+----------------------------------------------------------------+
-- | __3D, 0, | @imageType@ = 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D'                    | @viewType@ =                                                   |
-- | 0__      | @width@ ≥ 1                                                                    | 'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_3D'         |
-- |          | @height@ ≥ 1                                                                   | @baseArrayLayer@ = 0                                           |
-- |          | @depth@ ≥ 1                                                                    | @layerCount@ = 1                                               |
-- |          | @arrayLayers@ = 1                                                              |                                                                |
-- |          | @samples@ = 1                                                                  |                                                                |
-- +----------+--------------------------------------------------------------------------------+----------------------------------------------------------------+
-- | __3D, 0, | @imageType@ = 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D'                    | @viewType@ =                                                   |
-- | 0__      | @width@ ≥ 1                                                                    | 'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D'         |
-- |          | @height@ ≥ 1                                                                   | @levelCount@ = 1                                               |
-- |          | @depth@ ≥ 1                                                                    | @baseArrayLayer@ ≥ 0                                           |
-- |          | @arrayLayers@ = 1                                                              | @layerCount@ = 1                                               |
-- |          | @samples@ = 1                                                                  |                                                                |
-- |          | @flags@ includes                                                               |                                                                |
-- |          | 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT' |                                                                |
-- |          | @flags@ does not include                                                       |                                                                |
-- |          | 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_BINDING_BIT',     |                                                                |
-- |          | 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT',   |                                                                |
-- |          | and 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_ALIASED_BIT'  |                                                                |
-- +----------+--------------------------------------------------------------------------------+----------------------------------------------------------------+
-- | __3D, 0, | @imageType@ = 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D'                    | @viewType@ =                                                   |
-- | 0__      | @width@ ≥ 1                                                                    | 'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY'   |
-- |          | @height@ ≥ 1                                                                   | @levelCount@ = 1                                               |
-- |          | @depth@ ≥ 1                                                                    | @baseArrayLayer@ ≥ 0                                           |
-- |          | @arrayLayers@ = 1                                                              | @layerCount@ ≥ 1                                               |
-- |          | @samples@ = 1                                                                  |                                                                |
-- |          | @flags@ includes                                                               |                                                                |
-- |          | 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT' |                                                                |
-- |          | @flags@ does not include                                                       |                                                                |
-- |          | 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_BINDING_BIT',     |                                                                |
-- |          | 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT',   |                                                                |
-- |          | and 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_ALIASED_BIT'  |                                                                |
-- +----------+--------------------------------------------------------------------------------+----------------------------------------------------------------+
--
-- Image and image view parameter compatibility requirements
--
-- == Valid Usage
--
-- -   #VUID-VkImageViewCreateInfo-image-01003# If @image@ was not created
--     with
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CUBE_COMPATIBLE_BIT'
--     then @viewType@ /must/ not be
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE' or
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY'
--
-- -   #VUID-VkImageViewCreateInfo-viewType-01004# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-imageCubeArray image cubemap arrays>
--     feature is not enabled, @viewType@ /must/ not be
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY'
--
-- -   #VUID-VkImageViewCreateInfo-image-01005# If @image@ was created with
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D' but without
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT'
--     set then @viewType@ /must/ not be
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D' or
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY'
--
-- -   #VUID-VkImageViewCreateInfo-image-04441# @image@ /must/ have been
--     created with a @usage@ value containing at least one of the usages
--     defined in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#valid-imageview-imageusage valid image usage>
--     list for image views
--
-- -   #VUID-VkImageViewCreateInfo-None-02273# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     of the resultant image view /must/ contain at least one bit
--
-- -   #VUID-VkImageViewCreateInfo-usage-02274# If @usage@ contains
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT',
--     then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     of the resultant image view /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_BIT'
--
-- -   #VUID-VkImageViewCreateInfo-usage-02275# If @usage@ contains
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_STORAGE_BIT',
--     then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_BIT'
--
-- -   #VUID-VkImageViewCreateInfo-usage-02276# If @usage@ contains
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT',
--     then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageViewCreateInfo-usage-02277# If @usage@ contains
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT',
--     then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageViewCreateInfo-usage-02652# If @usage@ contains
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT',
--     then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain at least one of
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BIT'
--     or
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageViewCreateInfo-subresourceRange-01478#
--     @subresourceRange.baseMipLevel@ /must/ be less than the @mipLevels@
--     specified in 'Vulkan.Core10.Image.ImageCreateInfo' when @image@ was
--     created
--
-- -   #VUID-VkImageViewCreateInfo-subresourceRange-01718# If
--     @subresourceRange.levelCount@ is not
--     'Vulkan.Core10.APIConstants.REMAINING_MIP_LEVELS',
--     @subresourceRange.baseMipLevel@ + @subresourceRange.levelCount@
--     /must/ be less than or equal to the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @image@ was created
--
-- -   #VUID-VkImageViewCreateInfo-image-02571# If @image@ was created with
--     @usage@ containing
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT',
--     @subresourceRange.levelCount@ /must/ be @1@
--
-- -   #VUID-VkImageViewCreateInfo-image-01482# If @image@ is not a 3D
--     image created with
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT'
--     set, or @viewType@ is not
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D' or
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY',
--     @subresourceRange.baseArrayLayer@ /must/ be less than the
--     @arrayLayers@ specified in 'Vulkan.Core10.Image.ImageCreateInfo'
--     when @image@ was created
--
-- -   #VUID-VkImageViewCreateInfo-subresourceRange-01483# If
--     @subresourceRange.layerCount@ is not
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS', @image@ is not
--     a 3D image created with
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT'
--     set, or @viewType@ is not
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D' or
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY',
--     @subresourceRange.layerCount@ /must/ be non-zero and
--     @subresourceRange.baseArrayLayer@ + @subresourceRange.layerCount@
--     /must/ be less than or equal to the @arrayLayers@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @image@ was created
--
-- -   #VUID-VkImageViewCreateInfo-image-02724# If @image@ is a 3D image
--     created with
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT'
--     set, and @viewType@ is
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D' or
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY',
--     @subresourceRange.baseArrayLayer@ /must/ be less than the depth
--     computed from @baseMipLevel@ and @extent.depth@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @image@ was created,
--     according to the formula defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-miplevel-sizing Image Miplevel Sizing>
--
-- -   #VUID-VkImageViewCreateInfo-subresourceRange-02725# If
--     @subresourceRange.layerCount@ is not
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS', @image@ is a 3D
--     image created with
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT'
--     set, and @viewType@ is
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D' or
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY',
--     @subresourceRange.layerCount@ /must/ be non-zero and
--     @subresourceRange.baseArrayLayer@ + @subresourceRange.layerCount@
--     /must/ be less than or equal to the depth computed from
--     @baseMipLevel@ and @extent.depth@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @image@ was created,
--     according to the formula defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-miplevel-sizing Image Miplevel Sizing>
--
-- -   #VUID-VkImageViewCreateInfo-image-01761# If @image@ was created with
--     the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_MUTABLE_FORMAT_BIT'
--     flag, but without the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT'
--     flag, and if the @format@ of the @image@ is not a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar>
--     format, @format@ /must/ be compatible with the @format@ used to
--     create @image@, as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes Format Compatibility Classes>
--
-- -   #VUID-VkImageViewCreateInfo-image-01583# If @image@ was created with
--     the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT'
--     flag, @format@ /must/ be compatible with, or /must/ be an
--     uncompressed format that is size-compatible with, the @format@ used
--     to create @image@
--
-- -   #VUID-VkImageViewCreateInfo-image-01584# If @image@ was created with
--     the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT'
--     flag, the @levelCount@ and @layerCount@ members of
--     @subresourceRange@ /must/ both be @1@
--
-- -   #VUID-VkImageViewCreateInfo-pNext-01585# If a
--     'Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo'
--     structure was included in the @pNext@ chain of the
--     'Vulkan.Core10.Image.ImageCreateInfo' structure used when creating
--     @image@ and
--     'Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo'::@viewFormatCount@
--     is not zero then @format@ /must/ be one of the formats in
--     'Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo'::@pViewFormats@
--
-- -   #VUID-VkImageViewCreateInfo-pNext-04082# If a
--     'Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo'
--     structure was included in the @pNext@ chain of the
--     'Vulkan.Core10.Image.ImageCreateInfo' structure used when creating
--     @image@ and
--     'Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo'::@viewFormatCount@
--     is not zero then all of the formats in
--     'Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo'::@pViewFormats@
--     /must/ be compatible with the @format@ as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility compatibility table>
--
-- -   #VUID-VkImageViewCreateInfo-flags-04083# If @flags@ does not contain
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_MUTABLE_FORMAT_BIT'
--     and the @pNext@ chain include a
--     'Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo'
--     structure then
--     'Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo'::@viewFormatCount@
--     /must/ be @0@ or @1@
--
-- -   #VUID-VkImageViewCreateInfo-image-01586# If @image@ was created with
--     the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_MUTABLE_FORMAT_BIT'
--     flag, if the @format@ of the @image@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar>
--     format, and if @subresourceRange.aspectMask@ is one of
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT',
--     or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT',
--     then @format@ /must/ be compatible with the
--     'Vulkan.Core10.Enums.Format.Format' for the plane of the @image@
--     @format@ indicated by @subresourceRange.aspectMask@, as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatible-planes>
--
-- -   #VUID-VkImageViewCreateInfo-image-01762# If @image@ was not created
--     with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_MUTABLE_FORMAT_BIT'
--     flag, or if the @format@ of the @image@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar>
--     format and if @subresourceRange.aspectMask@ is
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT',
--     @format@ /must/ be identical to the @format@ used to create @image@
--
-- -   #VUID-VkImageViewCreateInfo-pNext-01970# If the @pNext@ chain
--     includes a
--     'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionInfo'
--     structure with a @conversion@ value other than
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', all members of
--     @components@ /must/ have the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-views-identity-mappings identity swizzle>
--
-- -   #VUID-VkImageViewCreateInfo-image-01020# If @image@ is non-sparse
--     then it /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkImageViewCreateInfo-subResourceRange-01021#
--     @subresourceRange@ and @viewType@ /must/ be compatible with the
--     image, as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-views-compatibility compatibility table>
--
-- -   #VUID-VkImageViewCreateInfo-image-02399# If @image@ has an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-external-android-hardware-buffer-external-formats external format>,
--     @format@ /must/ be 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   #VUID-VkImageViewCreateInfo-image-02400# If @image@ has an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-external-android-hardware-buffer-external-formats external format>,
--     the @pNext@ chain /must/ include a
--     'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionInfo'
--     structure with a @conversion@ object created with the same external
--     format as @image@
--
-- -   #VUID-VkImageViewCreateInfo-image-02401# If @image@ has an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-external-android-hardware-buffer-external-formats external format>,
--     all members of @components@ /must/ be the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-views-identity-mappings identity swizzle>
--
-- -   #VUID-VkImageViewCreateInfo-image-02086# If @image@ was created with
--     @usage@ containing
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR',
--     @viewType@ /must/ be
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D' or
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY'
--
-- -   #VUID-VkImageViewCreateInfo-image-02087# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shadingRateImage feature>
--     is enabled, and If @image@ was created with @usage@ containing
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV',
--     @format@ /must/ be 'Vulkan.Core10.Enums.Format.FORMAT_R8_UINT'
--
-- -   #VUID-VkImageViewCreateInfo-usage-04550# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate feature>
--     is enabled, and the @usage@ for the image view includes
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR',
--     then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   #VUID-VkImageViewCreateInfo-usage-04551# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate feature>
--     is enabled, the @usage@ for the image view includes
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR',
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-layeredShadingRateAttachments layeredShadingRateAttachments>
--     is 'Vulkan.Core10.FundamentalTypes.FALSE',
--     @subresourceRange.layerCount@ /must/ be @1@
--
-- -   #VUID-VkImageViewCreateInfo-flags-02572# If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMapDynamic dynamic fragment density map>
--     feature is not enabled, @flags@ /must/ not contain
--     'Vulkan.Core10.Enums.ImageViewCreateFlagBits.IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT'
--
-- -   #VUID-VkImageViewCreateInfo-flags-03567# If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMapDeferred deferred fragment density map>
--     feature is not enabled, @flags@ /must/ not contain
--     'Vulkan.Core10.Enums.ImageViewCreateFlagBits.IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DEFERRED_BIT_EXT'
--
-- -   #VUID-VkImageViewCreateInfo-flags-03568# If @flags@ contains
--     'Vulkan.Core10.Enums.ImageViewCreateFlagBits.IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DEFERRED_BIT_EXT',
--     @flags@ /must/ not contain
--     'Vulkan.Core10.Enums.ImageViewCreateFlagBits.IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT'
--
-- -   #VUID-VkImageViewCreateInfo-image-03569# If @image@ was created with
--     @flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--     and @usage@ containing
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT',
--     @subresourceRange.layerCount@ /must/ be less than or equal to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxSubsampledArrayLayers ::maxSubsampledArrayLayers>
--
-- -   #VUID-VkImageViewCreateInfo-flags-04116# If @flags@ does not contain
--     'Vulkan.Core10.Enums.ImageViewCreateFlagBits.IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT'
--     and @image@ was created with @usage@ containing
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT',
--     its @flags@ /must/ not contain any of
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_PROTECTED_BIT',
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_BINDING_BIT',
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT',
--     or
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_ALIASED_BIT'
--
-- -   #VUID-VkImageViewCreateInfo-pNext-02662# If the @pNext@ chain
--     includes a
--     'Vulkan.Core11.Promoted_From_VK_KHR_maintenance2.ImageViewUsageCreateInfo'
--     structure, and @image@ was not created with a
--     'Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'
--     structure included in the @pNext@ chain of
--     'Vulkan.Core10.Image.ImageCreateInfo', its @usage@ member /must/ not
--     include any bits that were not set in the @usage@ member of the
--     'Vulkan.Core10.Image.ImageCreateInfo' structure used to create
--     @image@
--
-- -   #VUID-VkImageViewCreateInfo-pNext-02663# If the @pNext@ chain
--     includes a
--     'Vulkan.Core11.Promoted_From_VK_KHR_maintenance2.ImageViewUsageCreateInfo'
--     structure, @image@ was created with a
--     'Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'
--     structure included in the @pNext@ chain of
--     'Vulkan.Core10.Image.ImageCreateInfo', and
--     @subResourceRange.aspectMask@ includes
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT',
--     the @usage@ member of the
--     'Vulkan.Core11.Promoted_From_VK_KHR_maintenance2.ImageViewUsageCreateInfo'
--     instance /must/ not include any bits that were not set in the
--     @usage@ member of the
--     'Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'
--     structure used to create @image@
--
-- -   #VUID-VkImageViewCreateInfo-pNext-02664# If the @pNext@ chain
--     includes a
--     'Vulkan.Core11.Promoted_From_VK_KHR_maintenance2.ImageViewUsageCreateInfo'
--     structure, @image@ was created with a
--     'Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'
--     structure included in the @pNext@ chain of
--     'Vulkan.Core10.Image.ImageCreateInfo', and
--     @subResourceRange.aspectMask@ includes bits other than
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT',
--     the @usage@ member of the
--     'Vulkan.Core11.Promoted_From_VK_KHR_maintenance2.ImageViewUsageCreateInfo'
--     structure /must/ not include any bits that were not set in the
--     @usage@ member of the 'Vulkan.Core10.Image.ImageCreateInfo'
--     structure used to create @image@
--
-- -   #VUID-VkImageViewCreateInfo-viewType-02960# If @viewType@ is
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE' and
--     @subresourceRange.layerCount@ is not
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS',
--     @subresourceRange.layerCount@ /must/ be @6@
--
-- -   #VUID-VkImageViewCreateInfo-viewType-02961# If @viewType@ is
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY' and
--     @subresourceRange.layerCount@ is not
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS',
--     @subresourceRange.layerCount@ /must/ be a multiple of @6@
--
-- -   #VUID-VkImageViewCreateInfo-viewType-02962# If @viewType@ is
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE' and
--     @subresourceRange.layerCount@ is
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS', the remaining
--     number of layers /must/ be @6@
--
-- -   #VUID-VkImageViewCreateInfo-viewType-02963# If @viewType@ is
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY' and
--     @subresourceRange.layerCount@ is
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS', the remaining
--     number of layers /must/ be a multiple of @6@
--
-- -   #VUID-VkImageViewCreateInfo-imageViewFormatSwizzle-04465# If the
--     @VK_KHR_portability_subset@ extension is enabled, and
--     'Vulkan.Extensions.VK_KHR_portability_subset.PhysicalDevicePortabilitySubsetFeaturesKHR'::@imageViewFormatSwizzle@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', all elements of
--     @components@ /must/ be
--     'Vulkan.Core10.Enums.ComponentSwizzle.COMPONENT_SWIZZLE_IDENTITY'.
--
-- -   #VUID-VkImageViewCreateInfo-imageViewFormatReinterpretation-04466#
--     If the @VK_KHR_portability_subset@ extension is enabled, and
--     'Vulkan.Extensions.VK_KHR_portability_subset.PhysicalDevicePortabilitySubsetFeaturesKHR'::@imageViewFormatReinterpretation@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', the
--     'Vulkan.Core10.Enums.Format.Format' in @format@ /must/ not contain a
--     different number of components, or a different number of bits in
--     each component, than the format of the 'Vulkan.Core10.Handles.Image'
--     in @image@.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImageViewCreateInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO'
--
-- -   #VUID-VkImageViewCreateInfo-pNext-pNext# Each @pNext@ member of any
--     structure (including this one) in the @pNext@ chain /must/ be either
--     @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_EXT_astc_decode_mode.ImageViewASTCDecodeModeEXT',
--     'Vulkan.Core11.Promoted_From_VK_KHR_maintenance2.ImageViewUsageCreateInfo',
--     or
--     'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionInfo'
--
-- -   #VUID-VkImageViewCreateInfo-sType-unique# The @sType@ value of each
--     struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkImageViewCreateInfo-flags-parameter# @flags@ /must/ be a
--     valid combination of
--     'Vulkan.Core10.Enums.ImageViewCreateFlagBits.ImageViewCreateFlagBits'
--     values
--
-- -   #VUID-VkImageViewCreateInfo-image-parameter# @image@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-VkImageViewCreateInfo-viewType-parameter# @viewType@ /must/ be
--     a valid 'Vulkan.Core10.Enums.ImageViewType.ImageViewType' value
--
-- -   #VUID-VkImageViewCreateInfo-format-parameter# @format@ /must/ be a
--     valid 'Vulkan.Core10.Enums.Format.Format' value
--
-- -   #VUID-VkImageViewCreateInfo-components-parameter# @components@
--     /must/ be a valid 'ComponentMapping' structure
--
-- -   #VUID-VkImageViewCreateInfo-subresourceRange-parameter#
--     @subresourceRange@ /must/ be a valid 'ImageSubresourceRange'
--     structure
--
-- = See Also
--
-- 'ComponentMapping', 'Vulkan.Core10.Enums.Format.Format',
-- 'Vulkan.Core10.Handles.Image', 'ImageSubresourceRange',
-- 'Vulkan.Core10.Enums.ImageViewCreateFlagBits.ImageViewCreateFlags',
-- 'Vulkan.Core10.Enums.ImageViewType.ImageViewType',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'createImageView'
data ImageViewCreateInfo (es :: [Type]) = ImageViewCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.ImageViewCreateFlagBits.ImageViewCreateFlagBits'
    -- describing additional parameters of the image view.
    flags :: ImageViewCreateFlags
  , -- | @image@ is a 'Vulkan.Core10.Handles.Image' on which the view will be
    -- created.
    image :: Image
  , -- | @viewType@ is a 'Vulkan.Core10.Enums.ImageViewType.ImageViewType' value
    -- specifying the type of the image view.
    viewType :: ImageViewType
  , -- | @format@ is a 'Vulkan.Core10.Enums.Format.Format' describing the format
    -- and type used to interpret texel blocks in the image.
    format :: Format
  , -- | @components@ is a 'ComponentMapping' specifies a remapping of color
    -- components (or of depth or stencil components after they have been
    -- converted into color components).
    components :: ComponentMapping
  , -- | @subresourceRange@ is a 'ImageSubresourceRange' selecting the set of
    -- mipmap levels and array layers to be accessible to the view.
    subresourceRange :: ImageSubresourceRange
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageViewCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (ImageViewCreateInfo es)

instance Extensible ImageViewCreateInfo where
  extensibleTypeName = "ImageViewCreateInfo"
  setNext x next = x{next = next}
  getNext ImageViewCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends ImageViewCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @ImageViewASTCDecodeModeEXT = Just f
    | Just Refl <- eqT @e @SamplerYcbcrConversionInfo = Just f
    | Just Refl <- eqT @e @ImageViewUsageCreateInfo = Just f
    | otherwise = Nothing

instance (Extendss ImageViewCreateInfo es, PokeChain es) => ToCStruct (ImageViewCreateInfo es) where
  withCStruct x f = allocaBytesAligned 80 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageViewCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr ImageViewCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 24 :: Ptr Image)) (image)
    lift $ poke ((p `plusPtr` 32 :: Ptr ImageViewType)) (viewType)
    lift $ poke ((p `plusPtr` 36 :: Ptr Format)) (format)
    lift $ poke ((p `plusPtr` 40 :: Ptr ComponentMapping)) (components)
    lift $ poke ((p `plusPtr` 56 :: Ptr ImageSubresourceRange)) (subresourceRange)
    lift $ f
  cStructSize = 80
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 24 :: Ptr Image)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr ImageViewType)) (zero)
    lift $ poke ((p `plusPtr` 36 :: Ptr Format)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr ComponentMapping)) (zero)
    lift $ poke ((p `plusPtr` 56 :: Ptr ImageSubresourceRange)) (zero)
    lift $ f

instance (Extendss ImageViewCreateInfo es, PeekChain es) => FromCStruct (ImageViewCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @ImageViewCreateFlags ((p `plusPtr` 16 :: Ptr ImageViewCreateFlags))
    image <- peek @Image ((p `plusPtr` 24 :: Ptr Image))
    viewType <- peek @ImageViewType ((p `plusPtr` 32 :: Ptr ImageViewType))
    format <- peek @Format ((p `plusPtr` 36 :: Ptr Format))
    components <- peekCStruct @ComponentMapping ((p `plusPtr` 40 :: Ptr ComponentMapping))
    subresourceRange <- peekCStruct @ImageSubresourceRange ((p `plusPtr` 56 :: Ptr ImageSubresourceRange))
    pure $ ImageViewCreateInfo
             next flags image viewType format components subresourceRange

instance es ~ '[] => Zero (ImageViewCreateInfo es) where
  zero = ImageViewCreateInfo
           ()
           zero
           zero
           zero
           zero
           zero
           zero

