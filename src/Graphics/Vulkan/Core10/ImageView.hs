{-# language CPP #-}
module Graphics.Vulkan.Core10.ImageView  ( createImageView
                                         , withImageView
                                         , destroyImageView
                                         , ComponentMapping(..)
                                         , ImageViewCreateInfo(..)
                                         ) where

import Control.Exception.Base (bracket)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Graphics.Vulkan.NamedType ((:::))
import Graphics.Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Graphics.Vulkan.CStruct.Extends (Chain)
import Graphics.Vulkan.Core10.Enums.ComponentSwizzle (ComponentSwizzle)
import Graphics.Vulkan.Core10.Handles (Device)
import Graphics.Vulkan.Core10.Handles (Device(..))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCreateImageView))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkDestroyImageView))
import Graphics.Vulkan.Core10.Handles (Device_T)
import Graphics.Vulkan.CStruct.Extends (Extends)
import Graphics.Vulkan.CStruct.Extends (Extensible(..))
import Graphics.Vulkan.Core10.Enums.Format (Format)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Handles (Image)
import Graphics.Vulkan.Core10.SharedTypes (ImageSubresourceRange)
import Graphics.Vulkan.Core10.Handles (ImageView)
import Graphics.Vulkan.Core10.Handles (ImageView(..))
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_EXT_astc_decode_mode (ImageViewASTCDecodeModeEXT)
import Graphics.Vulkan.Core10.Enums.ImageViewCreateFlagBits (ImageViewCreateFlags)
import Graphics.Vulkan.Core10.Enums.ImageViewType (ImageViewType)
import {-# SOURCE #-} Graphics.Vulkan.Core11.Promoted_From_VK_KHR_maintenance2 (ImageViewUsageCreateInfo)
import Graphics.Vulkan.CStruct.Extends (PeekChain)
import Graphics.Vulkan.CStruct.Extends (PeekChain(..))
import Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct.Extends (PokeChain(..))
import Graphics.Vulkan.Core10.Enums.Result (Result)
import Graphics.Vulkan.Core10.Enums.Result (Result(..))
import {-# SOURCE #-} Graphics.Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (SamplerYcbcrConversionInfo)
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Exception (VulkanException(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO))
import Graphics.Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateImageView
  :: FunPtr (Ptr Device_T -> Ptr (ImageViewCreateInfo a) -> Ptr AllocationCallbacks -> Ptr ImageView -> IO Result) -> Ptr Device_T -> Ptr (ImageViewCreateInfo a) -> Ptr AllocationCallbacks -> Ptr ImageView -> IO Result

-- | vkCreateImageView - Create an image view from an existing image
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' is the logical device that
--     creates the image view.
--
-- -   @pCreateInfo@ is a pointer to a 'ImageViewCreateInfo' structure
--     containing parameters to be used to create the image view.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pView@ is a pointer to a 'Graphics.Vulkan.Core10.Handles.ImageView'
--     handle in which the resulting image view object is returned.
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Device' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'ImageViewCreateInfo' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   @pView@ /must/ be a valid pointer to a
--     'Graphics.Vulkan.Core10.Handles.ImageView' handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Graphics.Vulkan.Core10.Handles.Device',
-- 'Graphics.Vulkan.Core10.Handles.ImageView', 'ImageViewCreateInfo'
createImageView :: PokeChain a => Device -> ImageViewCreateInfo a -> ("allocator" ::: Maybe AllocationCallbacks) -> IO (ImageView)
createImageView device createInfo allocator = evalContT $ do
  let vkCreateImageView' = mkVkCreateImageView (pVkCreateImageView (deviceCmds (device :: Device)))
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPView <- ContT $ bracket (callocBytes @ImageView 8) free
  r <- lift $ vkCreateImageView' (deviceHandle (device)) pCreateInfo pAllocator (pPView)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pView <- lift $ peek @ImageView pPView
  pure $ (pView)

-- | A safe wrapper for 'createImageView' and 'destroyImageView' using
-- 'bracket'
--
-- The allocated value must not be returned from the provided computation
withImageView :: PokeChain a => Device -> ImageViewCreateInfo a -> Maybe AllocationCallbacks -> ((ImageView) -> IO r) -> IO r
withImageView device pCreateInfo pAllocator =
  bracket
    (createImageView device pCreateInfo pAllocator)
    (\(o0) -> destroyImageView device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyImageView
  :: FunPtr (Ptr Device_T -> ImageView -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> ImageView -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyImageView - Destroy an image view object
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' is the logical device that
--     destroys the image view.
--
-- -   'Graphics.Vulkan.Core10.Handles.ImageView' is the image view to
--     destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to
--     'Graphics.Vulkan.Core10.Handles.ImageView' /must/ have completed
--     execution
--
-- -   If 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     were provided when 'Graphics.Vulkan.Core10.Handles.ImageView' was
--     created, a compatible set of callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     were provided when 'Graphics.Vulkan.Core10.Handles.ImageView' was
--     created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Device' handle
--
-- -   If 'Graphics.Vulkan.Core10.Handles.ImageView' is not
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE',
--     'Graphics.Vulkan.Core10.Handles.ImageView' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.ImageView' handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   If 'Graphics.Vulkan.Core10.Handles.ImageView' is a valid handle, it
--     /must/ have been created, allocated, or retrieved from
--     'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.ImageView' /must/ be
--     externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Graphics.Vulkan.Core10.Handles.Device',
-- 'Graphics.Vulkan.Core10.Handles.ImageView'
destroyImageView :: Device -> ImageView -> ("allocator" ::: Maybe AllocationCallbacks) -> IO ()
destroyImageView device imageView allocator = evalContT $ do
  let vkDestroyImageView' = mkVkDestroyImageView (pVkDestroyImageView (deviceCmds (device :: Device)))
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroyImageView' (deviceHandle (device)) (imageView) pAllocator
  pure $ ()


-- | VkComponentMapping - Structure specifying a color component mapping
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.AndroidHardwareBufferFormatPropertiesANDROID',
-- 'Graphics.Vulkan.Core10.Enums.ComponentSwizzle.ComponentSwizzle',
-- 'ImageViewCreateInfo',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionCreateInfo'
data ComponentMapping = ComponentMapping
  { -- | @r@ /must/ be a valid
    -- 'Graphics.Vulkan.Core10.Enums.ComponentSwizzle.ComponentSwizzle' value
    r :: ComponentSwizzle
  , -- | @g@ /must/ be a valid
    -- 'Graphics.Vulkan.Core10.Enums.ComponentSwizzle.ComponentSwizzle' value
    g :: ComponentSwizzle
  , -- | @b@ /must/ be a valid
    -- 'Graphics.Vulkan.Core10.Enums.ComponentSwizzle.ComponentSwizzle' value
    b :: ComponentSwizzle
  , -- | @a@ /must/ be a valid
    -- 'Graphics.Vulkan.Core10.Enums.ComponentSwizzle.ComponentSwizzle' value
    a :: ComponentSwizzle
  }
  deriving (Typeable)
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


-- | VkImageViewCreateInfo - Structure specifying parameters of a newly
-- created image view
--
-- = Description
--
-- Some of the 'Graphics.Vulkan.Core10.Handles.Image' creation parameters
-- are inherited by the view. In particular, image view creation inherits
-- the implicit parameter @usage@ specifying the allowed usages of the
-- image view that, by default, takes the value of the corresponding
-- @usage@ parameter specified in
-- 'Graphics.Vulkan.Core10.Image.ImageCreateInfo' at image creation time.
-- If the image was has a depth-stencil format and was created with a
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'
-- structure included in the @pNext@ chain of
-- 'Graphics.Vulkan.Core10.Image.ImageCreateInfo', the usage is calculated
-- based on the @subresource.aspectMask@ provided:
--
-- -   If @aspectMask@ includes only
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT',
--     the implicit @usage@ is equal to
--     'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'::@stencilUsage@.
--
-- -   If @aspectMask@ includes only
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT',
--     the implicit @usage@ is equal to
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo'::@usage@.
--
-- -   If both aspects are included in @aspectMask@, the implicit @usage@
--     is equal to the intersection of
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo'::@usage@ and
--     'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'::@stencilUsage@.
--     The implicit @usage@ /can/ be overriden by adding a
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_maintenance2.ImageViewUsageCreateInfo'
--     structure to the @pNext@ chain.
--
-- If 'Graphics.Vulkan.Core10.Handles.Image' was created with the
-- 'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_MUTABLE_FORMAT_BIT'
-- flag, and if the 'Graphics.Vulkan.Core10.Enums.Format.Format' of the
-- image is not
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar>,
-- 'Graphics.Vulkan.Core10.Enums.Format.Format' /can/ be different from the
-- image’s format, but if 'Graphics.Vulkan.Core10.Handles.Image' was
-- created without the
-- 'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT'
-- flag and they are not equal they /must/ be /compatible/. Image format
-- compatibility is defined in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes Format Compatibility Classes>
-- section. Views of compatible formats will have the same mapping between
-- texel coordinates and memory locations irrespective of the
-- 'Graphics.Vulkan.Core10.Enums.Format.Format', with only the
-- interpretation of the bit pattern changing.
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
-- If 'Graphics.Vulkan.Core10.Handles.Image' was created with the
-- 'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT'
-- flag, 'Graphics.Vulkan.Core10.Enums.Format.Format' /must/ be
-- /compatible/ with the image’s format as described above, or /must/ be an
-- uncompressed format in which case it /must/ be /size-compatible/ with
-- the image’s format, as defined for
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#copies-images-format-size-compatibility copying data between images>
-- In this case the resulting image view’s texel dimensions equal the
-- dimensions of the selected mip level divided by the compressed texel
-- block size and rounded up.
--
-- If the image view is to be used with a sampler which supports
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>,
-- an /identically defined object/ of type
-- 'Graphics.Vulkan.Core11.Handles.SamplerYcbcrConversion' to that used to
-- create the sampler /must/ be passed to 'createImageView' in a
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionInfo'
-- included in the @pNext@ chain of 'ImageViewCreateInfo'. Conversely, if a
-- 'Graphics.Vulkan.Core11.Handles.SamplerYcbcrConversion' object is passed
-- to 'createImageView', an identically defined
-- 'Graphics.Vulkan.Core11.Handles.SamplerYcbcrConversion' object /must/ be
-- used when sampling the image.
--
-- If the image has a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar>
-- 'Graphics.Vulkan.Core10.Enums.Format.Format' and
-- @subresourceRange.aspectMask@ is
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT',
-- 'Graphics.Vulkan.Core10.Enums.Format.Format' /must/ be identical to the
-- image 'Graphics.Vulkan.Core10.Enums.Format.Format', and the sampler to
-- be used with the image view /must/ enable
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>.
--
-- If 'Graphics.Vulkan.Core10.Handles.Image' was created with the
-- 'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_MUTABLE_FORMAT_BIT'
-- and the image has a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar>
-- 'Graphics.Vulkan.Core10.Enums.Format.Format', and if
-- @subresourceRange.aspectMask@ is
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT',
-- or
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT',
-- 'Graphics.Vulkan.Core10.Enums.Format.Format' /must/ be
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
-- +----------+-----------------------------------------------------------------------------------------+-------------------------------------------------------------------------+
-- | Dim,     | Image parameters                                                                        | View parameters                                                         |
-- | Arrayed, |                                                                                         |                                                                         |
-- | MS       |                                                                                         |                                                                         |
-- +==========+=========================================================================================+=========================================================================+
-- |          | 'Graphics.Vulkan.Core10.Enums.ImageType.ImageType' =                                    | @baseArrayLayer@, @layerCount@, and @levelCount@ are members of the     |
-- |          | ci.'Graphics.Vulkan.Core10.Enums.ImageType.ImageType'                                   | @subresourceRange@ member.                                              |
-- |          | @width@ = ci.@extent.width@                                                             |                                                                         |
-- |          | @height@ = ci.@extent.height@                                                           |                                                                         |
-- |          | @depth@ = ci.@extent.depth@                                                             |                                                                         |
-- |          | @arrayLayers@ = ci.@arrayLayers@                                                        |                                                                         |
-- |          | @samples@ = ci.@samples@                                                                |                                                                         |
-- |          | 'Graphics.Vulkan.Core10.BaseType.Flags' = ci.'Graphics.Vulkan.Core10.BaseType.Flags'    |                                                                         |
-- |          | where ci is the 'Graphics.Vulkan.Core10.Image.ImageCreateInfo' used to create           |                                                                         |
-- |          | 'Graphics.Vulkan.Core10.Handles.Image'.                                                 |                                                                         |
-- +----------+-----------------------------------------------------------------------------------------+-------------------------------------------------------------------------+
-- | __1D, 0, | 'Graphics.Vulkan.Core10.Enums.ImageType.ImageType' =                                    | @viewType@ =                                                            |
-- | 0__      | 'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D'                                  | 'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_1D'         |
-- |          | @width@ ≥ 1                                                                             | @baseArrayLayer@ ≥ 0                                                    |
-- |          | @height@ = 1                                                                            | @layerCount@ = 1                                                        |
-- |          | @depth@ = 1                                                                             |                                                                         |
-- |          | @arrayLayers@ ≥ 1                                                                       |                                                                         |
-- |          | @samples@ = 1                                                                           |                                                                         |
-- +----------+-----------------------------------------------------------------------------------------+-------------------------------------------------------------------------+
-- | __1D, 1, | 'Graphics.Vulkan.Core10.Enums.ImageType.ImageType' =                                    | @viewType@ =                                                            |
-- | 0__      | 'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D'                                  | 'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_1D_ARRAY'   |
-- |          | @width@ ≥ 1                                                                             | @baseArrayLayer@ ≥ 0                                                    |
-- |          | @height@ = 1                                                                            | @layerCount@ ≥ 1                                                        |
-- |          | @depth@ = 1                                                                             |                                                                         |
-- |          | @arrayLayers@ ≥ 1                                                                       |                                                                         |
-- |          | @samples@ = 1                                                                           |                                                                         |
-- +----------+-----------------------------------------------------------------------------------------+-------------------------------------------------------------------------+
-- | __2D, 0, | 'Graphics.Vulkan.Core10.Enums.ImageType.ImageType' =                                    | @viewType@ =                                                            |
-- | 0__      | 'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'                                  | 'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D'         |
-- |          | @width@ ≥ 1                                                                             | @baseArrayLayer@ ≥ 0                                                    |
-- |          | @height@ ≥ 1                                                                            | @layerCount@ = 1                                                        |
-- |          | @depth@ = 1                                                                             |                                                                         |
-- |          | @arrayLayers@ ≥ 1                                                                       |                                                                         |
-- |          | @samples@ = 1                                                                           |                                                                         |
-- +----------+-----------------------------------------------------------------------------------------+-------------------------------------------------------------------------+
-- | __2D, 1, | 'Graphics.Vulkan.Core10.Enums.ImageType.ImageType' =                                    | @viewType@ =                                                            |
-- | 0__      | 'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'                                  | 'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY'   |
-- |          | @width@ ≥ 1                                                                             | @baseArrayLayer@ ≥ 0                                                    |
-- |          | @height@ ≥ 1                                                                            | @layerCount@ ≥ 1                                                        |
-- |          | @depth@ = 1                                                                             |                                                                         |
-- |          | @arrayLayers@ ≥ 1                                                                       |                                                                         |
-- |          | @samples@ = 1                                                                           |                                                                         |
-- +----------+-----------------------------------------------------------------------------------------+-------------------------------------------------------------------------+
-- | __2D, 0, | 'Graphics.Vulkan.Core10.Enums.ImageType.ImageType' =                                    | @viewType@ =                                                            |
-- | 1__      | 'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'                                  | 'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D'         |
-- |          | @width@ ≥ 1                                                                             | @baseArrayLayer@ ≥ 0                                                    |
-- |          | @height@ ≥ 1                                                                            | @layerCount@ = 1                                                        |
-- |          | @depth@ = 1                                                                             |                                                                         |
-- |          | @arrayLayers@ ≥ 1                                                                       |                                                                         |
-- |          | @samples@ > 1                                                                           |                                                                         |
-- +----------+-----------------------------------------------------------------------------------------+-------------------------------------------------------------------------+
-- | __2D, 1, | 'Graphics.Vulkan.Core10.Enums.ImageType.ImageType' =                                    | @viewType@ =                                                            |
-- | 1__      | 'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'                                  | 'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY'   |
-- |          | @width@ ≥ 1                                                                             | @baseArrayLayer@ ≥ 0                                                    |
-- |          | @height@ ≥ 1                                                                            | @layerCount@ ≥ 1                                                        |
-- |          | @depth@ = 1                                                                             |                                                                         |
-- |          | @arrayLayers@ ≥ 1                                                                       |                                                                         |
-- |          | @samples@ > 1                                                                           |                                                                         |
-- +----------+-----------------------------------------------------------------------------------------+-------------------------------------------------------------------------+
-- | __CUBE,  | 'Graphics.Vulkan.Core10.Enums.ImageType.ImageType' =                                    | @viewType@ =                                                            |
-- | 0, 0__   | 'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'                                  | 'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE'       |
-- |          | @width@ ≥ 1                                                                             | @baseArrayLayer@ ≥ 0                                                    |
-- |          | @height@ = @width@                                                                      | @layerCount@ = 6                                                        |
-- |          | @depth@ = 1                                                                             |                                                                         |
-- |          | @arrayLayers@ ≥ 6                                                                       |                                                                         |
-- |          | @samples@ = 1                                                                           |                                                                         |
-- |          | 'Graphics.Vulkan.Core10.BaseType.Flags' includes                                        |                                                                         |
-- |          | 'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CUBE_COMPATIBLE_BIT'     |                                                                         |
-- +----------+-----------------------------------------------------------------------------------------+-------------------------------------------------------------------------+
-- | __CUBE,  | 'Graphics.Vulkan.Core10.Enums.ImageType.ImageType' =                                    | @viewType@ =                                                            |
-- | 1, 0__   | 'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'                                  | 'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY' |
-- |          | @width@ ≥ 1                                                                             | @baseArrayLayer@ ≥ 0                                                    |
-- |          | @height@ = width                                                                        | @layerCount@ = 6 × /N/, /N/ ≥ 1                                         |
-- |          | @depth@ = 1                                                                             |                                                                         |
-- |          | /N/ ≥ 1                                                                                 |                                                                         |
-- |          | @arrayLayers@ ≥ 6 × /N/                                                                 |                                                                         |
-- |          | @samples@ = 1                                                                           |                                                                         |
-- |          | 'Graphics.Vulkan.Core10.BaseType.Flags' includes                                        |                                                                         |
-- |          | 'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CUBE_COMPATIBLE_BIT'     |                                                                         |
-- +----------+-----------------------------------------------------------------------------------------+-------------------------------------------------------------------------+
-- | __3D, 0, | 'Graphics.Vulkan.Core10.Enums.ImageType.ImageType' =                                    | @viewType@ =                                                            |
-- | 0__      | 'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D'                                  | 'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_3D'         |
-- |          | @width@ ≥ 1                                                                             | @baseArrayLayer@ = 0                                                    |
-- |          | @height@ ≥ 1                                                                            | @layerCount@ = 1                                                        |
-- |          | @depth@ ≥ 1                                                                             |                                                                         |
-- |          | @arrayLayers@ = 1                                                                       |                                                                         |
-- |          | @samples@ = 1                                                                           |                                                                         |
-- +----------+-----------------------------------------------------------------------------------------+-------------------------------------------------------------------------+
-- | __3D, 0, | 'Graphics.Vulkan.Core10.Enums.ImageType.ImageType' =                                    | @viewType@ =                                                            |
-- | 0__      | 'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D'                                  | 'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D'         |
-- |          | @width@ ≥ 1                                                                             | @levelCount@ = 1                                                        |
-- |          | @height@ ≥ 1                                                                            | @baseArrayLayer@ ≥ 0                                                    |
-- |          | @depth@ ≥ 1                                                                             | @layerCount@ = 1                                                        |
-- |          | @arrayLayers@ = 1                                                                       |                                                                         |
-- |          | @samples@ = 1                                                                           |                                                                         |
-- |          | 'Graphics.Vulkan.Core10.BaseType.Flags' includes                                        |                                                                         |
-- |          | 'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT' |                                                                         |
-- |          | 'Graphics.Vulkan.Core10.BaseType.Flags' does not include                                |                                                                         |
-- |          | 'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_BINDING_BIT',     |                                                                         |
-- |          | 'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT',   |                                                                         |
-- |          | and 'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_ALIASED_BIT'  |                                                                         |
-- +----------+-----------------------------------------------------------------------------------------+-------------------------------------------------------------------------+
-- | __3D, 0, | 'Graphics.Vulkan.Core10.Enums.ImageType.ImageType' =                                    | @viewType@ =                                                            |
-- | 0__      | 'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D'                                  | 'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY'   |
-- |          | @width@ ≥ 1                                                                             | @levelCount@ = 1                                                        |
-- |          | @height@ ≥ 1                                                                            | @baseArrayLayer@ ≥ 0                                                    |
-- |          | @depth@ ≥ 1                                                                             | @layerCount@ ≥ 1                                                        |
-- |          | @arrayLayers@ = 1                                                                       |                                                                         |
-- |          | @samples@ = 1                                                                           |                                                                         |
-- |          | 'Graphics.Vulkan.Core10.BaseType.Flags' includes                                        |                                                                         |
-- |          | 'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT' |                                                                         |
-- |          | 'Graphics.Vulkan.Core10.BaseType.Flags' does not include                                |                                                                         |
-- |          | 'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_BINDING_BIT',     |                                                                         |
-- |          | 'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT',   |                                                                         |
-- |          | and 'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_ALIASED_BIT'  |                                                                         |
-- +----------+-----------------------------------------------------------------------------------------+-------------------------------------------------------------------------+
--
-- Image and image view parameter compatibility requirements
--
-- == Valid Usage
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Image' was not created with
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CUBE_COMPATIBLE_BIT'
--     then @viewType@ /must/ not be
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE' or
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-imageCubeArray image cubemap arrays>
--     feature is not enabled, @viewType@ /must/ not be
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY'
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Image' was created with
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D' but without
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT'
--     set then @viewType@ /must/ not be
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D' or
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY'
--
-- -   'Graphics.Vulkan.Core10.Handles.Image' /must/ have been created with
--     a @usage@ value containing at least one of
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT',
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_STORAGE_BIT',
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT',
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT',
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT',
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV',
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT'
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     of the resultant image view /must/ contain at least one bit.
--
-- -   If @usage@ contains
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT',
--     then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     of the resultant image view /must/ contain
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_BIT'.
--
-- -   If @usage@ contains
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_STORAGE_BIT',
--     then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_BIT'.
--
-- -   If @usage@ contains
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT',
--     then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BIT'.
--
-- -   If @usage@ contains
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT',
--     then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT'.
--
-- -   If @usage@ contains
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT',
--     then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain at least one of
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BIT'
--     or
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT'.
--
-- -   @subresourceRange.baseMipLevel@ /must/ be less than the @mipLevels@
--     specified in 'Graphics.Vulkan.Core10.Image.ImageCreateInfo' when
--     'Graphics.Vulkan.Core10.Handles.Image' was created
--
-- -   If @subresourceRange.levelCount@ is not
--     'Graphics.Vulkan.Core10.APIConstants.REMAINING_MIP_LEVELS',
--     @subresourceRange.baseMipLevel@ + @subresourceRange.levelCount@
--     /must/ be less than or equal to the @mipLevels@ specified in
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo' when
--     'Graphics.Vulkan.Core10.Handles.Image' was created
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Image' was created with @usage@
--     containing
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT',
--     @subresourceRange.levelCount@ /must/ be @1@
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Image' is not a 3D image created
--     with
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT'
--     set, or @viewType@ is not
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D' or
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY',
--     @subresourceRange.baseArrayLayer@ /must/ be less than the
--     @arrayLayers@ specified in
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo' when
--     'Graphics.Vulkan.Core10.Handles.Image' was created
--
-- -   If @subresourceRange.layerCount@ is not
--     'Graphics.Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS',
--     'Graphics.Vulkan.Core10.Handles.Image' is not a 3D image created
--     with
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT'
--     set, or @viewType@ is not
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D' or
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY',
--     @subresourceRange.layerCount@ /must/ be non-zero and
--     @subresourceRange.baseArrayLayer@ + @subresourceRange.layerCount@
--     /must/ be less than or equal to the @arrayLayers@ specified in
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo' when
--     'Graphics.Vulkan.Core10.Handles.Image' was created
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Image' is a 3D image created with
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT'
--     set, and @viewType@ is
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D' or
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY',
--     @subresourceRange.baseArrayLayer@ /must/ be less than the depth
--     computed from @baseMipLevel@ and @extent.depth@ specified in
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo' when
--     'Graphics.Vulkan.Core10.Handles.Image' was created, according to the
--     formula defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-miplevel-sizing Image Miplevel Sizing>.
--
-- -   If @subresourceRange.layerCount@ is not
--     'Graphics.Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS',
--     'Graphics.Vulkan.Core10.Handles.Image' is a 3D image created with
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT'
--     set, and @viewType@ is
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D' or
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY',
--     @subresourceRange.layerCount@ /must/ be non-zero and
--     @subresourceRange.baseArrayLayer@ + @subresourceRange.layerCount@
--     /must/ be less than or equal to the depth computed from
--     @baseMipLevel@ and @extent.depth@ specified in
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo' when
--     'Graphics.Vulkan.Core10.Handles.Image' was created, according to the
--     formula defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-miplevel-sizing Image Miplevel Sizing>.
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Image' was created with the
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_MUTABLE_FORMAT_BIT'
--     flag, 'Graphics.Vulkan.Core10.Enums.Format.Format' /must/ be
--     compatible with the 'Graphics.Vulkan.Core10.Enums.Format.Format'
--     used to create 'Graphics.Vulkan.Core10.Handles.Image', as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes Format Compatibility Classes>
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Image' was created with the
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_MUTABLE_FORMAT_BIT'
--     flag, but without the
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT'
--     flag, and if the 'Graphics.Vulkan.Core10.Enums.Format.Format' of the
--     'Graphics.Vulkan.Core10.Handles.Image' is not a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar>
--     format, 'Graphics.Vulkan.Core10.Enums.Format.Format' /must/ be
--     compatible with the 'Graphics.Vulkan.Core10.Enums.Format.Format'
--     used to create 'Graphics.Vulkan.Core10.Handles.Image', as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes Format Compatibility Classes>
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Image' was created with the
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT'
--     flag, 'Graphics.Vulkan.Core10.Enums.Format.Format' /must/ be
--     compatible with, or /must/ be an uncompressed format that is
--     size-compatible with, the
--     'Graphics.Vulkan.Core10.Enums.Format.Format' used to create
--     'Graphics.Vulkan.Core10.Handles.Image'.
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Image' was created with the
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT'
--     flag, the @levelCount@ and @layerCount@ members of
--     @subresourceRange@ /must/ both be @1@.
--
-- -   If a
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo'
--     structure was included in the @pNext@ chain of the
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo' structure used when
--     creating 'Graphics.Vulkan.Core10.Handles.Image' and the
--     @viewFormatCount@ field of
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo'
--     is not zero then 'Graphics.Vulkan.Core10.Enums.Format.Format' /must/
--     be one of the formats in
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo'::@pViewFormats@.
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Image' was created with the
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_MUTABLE_FORMAT_BIT'
--     flag, if the 'Graphics.Vulkan.Core10.Enums.Format.Format' of the
--     'Graphics.Vulkan.Core10.Handles.Image' is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar>
--     format, and if @subresourceRange.aspectMask@ is one of
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT',
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT',
--     then 'Graphics.Vulkan.Core10.Enums.Format.Format' /must/ be
--     compatible with the 'Graphics.Vulkan.Core10.Enums.Format.Format' for
--     the plane of the 'Graphics.Vulkan.Core10.Handles.Image'
--     'Graphics.Vulkan.Core10.Enums.Format.Format' indicated by
--     @subresourceRange.aspectMask@, as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatible-planes>
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Image' was not created with the
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_MUTABLE_FORMAT_BIT'
--     flag, or if the 'Graphics.Vulkan.Core10.Enums.Format.Format' of the
--     'Graphics.Vulkan.Core10.Handles.Image' is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar>
--     format and if @subresourceRange.aspectMask@ is
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT',
--     'Graphics.Vulkan.Core10.Enums.Format.Format' /must/ be identical to
--     the 'Graphics.Vulkan.Core10.Enums.Format.Format' used to create
--     'Graphics.Vulkan.Core10.Handles.Image'
--
-- -   If the @pNext@ chain includes a
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionInfo'
--     structure with a @conversion@ value other than
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE', all members of
--     @components@ /must/ have the value
--     'Graphics.Vulkan.Core10.Enums.ComponentSwizzle.COMPONENT_SWIZZLE_IDENTITY'.
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Image' is non-sparse then it
--     /must/ be bound completely and contiguously to a single
--     'Graphics.Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   @subresourceRange@ and @viewType@ /must/ be compatible with the
--     image, as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-views-compatibility compatibility table>
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Image' has an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-external-android-hardware-buffer-external-formats external format>,
--     'Graphics.Vulkan.Core10.Enums.Format.Format' /must/ be
--     'Graphics.Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'.
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Image' has an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-external-android-hardware-buffer-external-formats external format>,
--     the @pNext@ chain /must/ include a
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionInfo'
--     structure with a @conversion@ object created with the same external
--     format as 'Graphics.Vulkan.Core10.Handles.Image'.
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Image' has an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-external-android-hardware-buffer-external-formats external format>,
--     all members of @components@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.ComponentSwizzle.COMPONENT_SWIZZLE_IDENTITY'.
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Image' was created with @usage@
--     containing
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV',
--     @viewType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D' or
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY'
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Image' was created with @usage@
--     containing
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV',
--     'Graphics.Vulkan.Core10.Enums.Format.Format' /must/ be
--     'Graphics.Vulkan.Core10.Enums.Format.FORMAT_R8_UINT'
--
-- -   If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentdensitymapdynamic dynamic fragment density map>
--     feature is not enabled, 'Graphics.Vulkan.Core10.BaseType.Flags'
--     /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.ImageViewCreateFlagBits.IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT'
--
-- -   If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentdensitymapdynamic dynamic fragment density map>
--     feature is not enabled and 'Graphics.Vulkan.Core10.Handles.Image'
--     was created with @usage@ containing
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT',
--     'Graphics.Vulkan.Core10.BaseType.Flags' /must/ not contain any of
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_PROTECTED_BIT',
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_BINDING_BIT',
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT',
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_ALIASED_BIT'
--
-- -   If the @pNext@ chain includes a
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_maintenance2.ImageViewUsageCreateInfo'
--     structure, and 'Graphics.Vulkan.Core10.Handles.Image' was not
--     created with a
--     'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'
--     structure included in the @pNext@ chain of
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo', its @usage@ member
--     /must/ not include any bits that were not set in the @usage@ member
--     of the 'Graphics.Vulkan.Core10.Image.ImageCreateInfo' structure used
--     to create 'Graphics.Vulkan.Core10.Handles.Image'
--
-- -   If the @pNext@ chain includes a
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_maintenance2.ImageViewUsageCreateInfo'
--     structure, 'Graphics.Vulkan.Core10.Handles.Image' was created with a
--     'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'
--     structure included in the @pNext@ chain of
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo', and
--     @subResourceRange.aspectMask@ includes
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT',
--     the @usage@ member of the
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_maintenance2.ImageViewUsageCreateInfo'
--     instance /must/ not include any bits that were not set in the
--     @usage@ member of the
--     'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'
--     structure used to create 'Graphics.Vulkan.Core10.Handles.Image'
--
-- -   If the @pNext@ chain includes a
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_maintenance2.ImageViewUsageCreateInfo'
--     structure, 'Graphics.Vulkan.Core10.Handles.Image' was created with a
--     'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'
--     structure included in the @pNext@ chain of
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo', and
--     @subResourceRange.aspectMask@ includes bits other than
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT',
--     the @usage@ member of the
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_maintenance2.ImageViewUsageCreateInfo'
--     structure /must/ not include any bits that were not set in the
--     @usage@ member of the 'Graphics.Vulkan.Core10.Image.ImageCreateInfo'
--     structure used to create 'Graphics.Vulkan.Core10.Handles.Image'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO'
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.Extensions.VK_EXT_astc_decode_mode.ImageViewASTCDecodeModeEXT',
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_maintenance2.ImageViewUsageCreateInfo',
--     or
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionInfo'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   'Graphics.Vulkan.Core10.BaseType.Flags' /must/ be a valid
--     combination of
--     'Graphics.Vulkan.Core10.Enums.ImageViewCreateFlagBits.ImageViewCreateFlagBits'
--     values
--
-- -   'Graphics.Vulkan.Core10.Handles.Image' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Image' handle
--
-- -   @viewType@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.ImageViewType' value
--
-- -   'Graphics.Vulkan.Core10.Enums.Format.Format' /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.Format.Format' value
--
-- -   @components@ /must/ be a valid 'ComponentMapping' structure
--
-- -   @subresourceRange@ /must/ be a valid
--     'Graphics.Vulkan.Core10.SharedTypes.ImageSubresourceRange' structure
--
-- = See Also
--
-- 'ComponentMapping', 'Graphics.Vulkan.Core10.Enums.Format.Format',
-- 'Graphics.Vulkan.Core10.Handles.Image',
-- 'Graphics.Vulkan.Core10.SharedTypes.ImageSubresourceRange',
-- 'Graphics.Vulkan.Core10.Enums.ImageViewCreateFlagBits.ImageViewCreateFlags',
-- 'Graphics.Vulkan.Core10.Enums.ImageViewType.ImageViewType',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createImageView'
data ImageViewCreateInfo (es :: [Type]) = ImageViewCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
    next :: Chain es
  , -- | 'Graphics.Vulkan.Core10.BaseType.Flags' is a bitmask of
    -- 'Graphics.Vulkan.Core10.Enums.ImageViewCreateFlagBits.ImageViewCreateFlagBits'
    -- describing additional parameters of the image view.
    flags :: ImageViewCreateFlags
  , -- | 'Graphics.Vulkan.Core10.Handles.Image' is a
    -- 'Graphics.Vulkan.Core10.Handles.Image' on which the view will be
    -- created.
    image :: Image
  , -- | @viewType@ is a
    -- 'Graphics.Vulkan.Core10.Enums.ImageViewType.ImageViewType' value
    -- specifying the type of the image view.
    viewType :: ImageViewType
  , -- | 'Graphics.Vulkan.Core10.Enums.Format.Format' is a
    -- 'Graphics.Vulkan.Core10.Enums.Format.Format' describing the format and
    -- type used to interpret texel blocks in the image.
    format :: Format
  , -- | @components@ is a 'ComponentMapping' specifies a remapping of color
    -- components (or of depth or stencil components after they have been
    -- converted into color components).
    components :: ComponentMapping
  , -- | @subresourceRange@ is a
    -- 'Graphics.Vulkan.Core10.SharedTypes.ImageSubresourceRange' selecting the
    -- set of mipmap levels and array layers to be accessible to the view.
    subresourceRange :: ImageSubresourceRange
  }
  deriving (Typeable)
deriving instance Show (Chain es) => Show (ImageViewCreateInfo es)

instance Extensible ImageViewCreateInfo where
  extensibleType = STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO
  setNext x next = x{next = next}
  getNext ImageViewCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends ImageViewCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @ImageViewASTCDecodeModeEXT = Just f
    | Just Refl <- eqT @e @SamplerYcbcrConversionInfo = Just f
    | Just Refl <- eqT @e @ImageViewUsageCreateInfo = Just f
    | otherwise = Nothing

instance PokeChain es => ToCStruct (ImageViewCreateInfo es) where
  withCStruct x f = allocaBytesAligned 80 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageViewCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr ImageViewCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 24 :: Ptr Image)) (image)
    lift $ poke ((p `plusPtr` 32 :: Ptr ImageViewType)) (viewType)
    lift $ poke ((p `plusPtr` 36 :: Ptr Format)) (format)
    ContT $ pokeCStruct ((p `plusPtr` 40 :: Ptr ComponentMapping)) (components) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 56 :: Ptr ImageSubresourceRange)) (subresourceRange) . ($ ())
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
    ContT $ pokeCStruct ((p `plusPtr` 40 :: Ptr ComponentMapping)) (zero) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 56 :: Ptr ImageSubresourceRange)) (zero) . ($ ())
    lift $ f

instance PeekChain es => FromCStruct (ImageViewCreateInfo es) where
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

