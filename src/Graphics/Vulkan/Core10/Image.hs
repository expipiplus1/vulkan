{-# language CPP #-}
module Graphics.Vulkan.Core10.Image  ( createImage
                                     , withImage
                                     , destroyImage
                                     , getImageSubresourceLayout
                                     , ImageSubresource(..)
                                     , ImageCreateInfo(..)
                                     , SubresourceLayout(..)
                                     ) where

import Control.Exception.Base (bracket)
import Control.Monad.IO.Class (liftIO)
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
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Control.Monad.IO.Class (MonadIO)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Graphics.Vulkan.CStruct.Utils (advancePtrBytes)
import Graphics.Vulkan.NamedType ((:::))
import Graphics.Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Graphics.Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation (DedicatedAllocationImageCreateInfoNV)
import Graphics.Vulkan.Core10.Handles (Device)
import Graphics.Vulkan.Core10.Handles (Device(..))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCreateImage))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkDestroyImage))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkGetImageSubresourceLayout))
import Graphics.Vulkan.Core10.BaseType (DeviceSize)
import Graphics.Vulkan.Core10.Handles (Device_T)
import Graphics.Vulkan.CStruct.Extends (Extends)
import Graphics.Vulkan.CStruct.Extends (Extensible(..))
import Graphics.Vulkan.Core10.SharedTypes (Extent3D)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer (ExternalFormatANDROID)
import {-# SOURCE #-} Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory (ExternalMemoryImageCreateInfo)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_NV_external_memory (ExternalMemoryImageCreateInfoNV)
import Graphics.Vulkan.Core10.Enums.Format (Format)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Handles (Image)
import Graphics.Vulkan.Core10.Handles (Image(..))
import Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlags)
import Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlags)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_EXT_image_drm_format_modifier (ImageDrmFormatModifierExplicitCreateInfoEXT)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_EXT_image_drm_format_modifier (ImageDrmFormatModifierListCreateInfoEXT)
import {-# SOURCE #-} Graphics.Vulkan.Core12.Promoted_From_VK_KHR_image_format_list (ImageFormatListCreateInfo)
import Graphics.Vulkan.Core10.Enums.ImageLayout (ImageLayout)
import {-# SOURCE #-} Graphics.Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage (ImageStencilUsageCreateInfo)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_KHR_swapchain (ImageSwapchainCreateInfoKHR)
import Graphics.Vulkan.Core10.Enums.ImageTiling (ImageTiling)
import Graphics.Vulkan.Core10.Enums.ImageType (ImageType)
import Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlags)
import Graphics.Vulkan.CStruct.Extends (PeekChain)
import Graphics.Vulkan.CStruct.Extends (PeekChain(..))
import Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct.Extends (PokeChain(..))
import Graphics.Vulkan.Core10.Enums.Result (Result)
import Graphics.Vulkan.Core10.Enums.Result (Result(..))
import Graphics.Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlagBits)
import Graphics.Vulkan.Core10.Enums.SharingMode (SharingMode)
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Exception (VulkanException(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_CREATE_INFO))
import Graphics.Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateImage
  :: FunPtr (Ptr Device_T -> Ptr (ImageCreateInfo a) -> Ptr AllocationCallbacks -> Ptr Image -> IO Result) -> Ptr Device_T -> Ptr (ImageCreateInfo a) -> Ptr AllocationCallbacks -> Ptr Image -> IO Result

-- | vkCreateImage - Create a new image object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the image.
--
-- -   @pCreateInfo@ is a pointer to a 'ImageCreateInfo' structure
--     containing parameters to be used to create the image.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pImage@ is a pointer to a 'Graphics.Vulkan.Core10.Handles.Image'
--     handle in which the resulting image object is returned.
--
-- == Valid Usage
--
-- -   If the @flags@ member of @pCreateInfo@ includes
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_BINDING_BIT',
--     creating this 'Graphics.Vulkan.Core10.Handles.Image' /must/ not
--     cause the total required sparse memory for all currently valid
--     sparse resources on the device to exceed
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@sparseAddressSpaceSize@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Device'
--     handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid 'ImageCreateInfo'
--     structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   @pImage@ /must/ be a valid pointer to a
--     'Graphics.Vulkan.Core10.Handles.Image' handle
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
-- 'Graphics.Vulkan.Core10.Handles.Image', 'ImageCreateInfo'
createImage :: forall a io . (PokeChain a, MonadIO io) => Device -> ImageCreateInfo a -> ("allocator" ::: Maybe AllocationCallbacks) -> io (Image)
createImage device createInfo allocator = liftIO . evalContT $ do
  let vkCreateImage' = mkVkCreateImage (pVkCreateImage (deviceCmds (device :: Device)))
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPImage <- ContT $ bracket (callocBytes @Image 8) free
  r <- lift $ vkCreateImage' (deviceHandle (device)) pCreateInfo pAllocator (pPImage)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pImage <- lift $ peek @Image pPImage
  pure $ (pImage)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createImage' and 'destroyImage'
--
-- To ensure that 'destroyImage' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the first argument.
-- To just extract the pair pass '(,)' as the first argument.
--
withImage :: forall a io r . (PokeChain a, MonadIO io) => (io (Image) -> ((Image) -> io ()) -> r) -> Device -> ImageCreateInfo a -> Maybe AllocationCallbacks -> r
withImage b device pCreateInfo pAllocator =
  b (createImage device pCreateInfo pAllocator)
    (\(o0) -> destroyImage device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyImage
  :: FunPtr (Ptr Device_T -> Image -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> Image -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyImage - Destroy an image object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the image.
--
-- -   @image@ is the image to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @image@, either directly or via
--     a 'Graphics.Vulkan.Core10.Handles.ImageView', /must/ have completed
--     execution
--
-- -   If 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     were provided when @image@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     were provided when @image@ was created, @pAllocator@ /must/ be
--     @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Device'
--     handle
--
-- -   If @image@ is not 'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @image@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Image'
--     handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   If @image@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @image@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Graphics.Vulkan.Core10.Handles.Device',
-- 'Graphics.Vulkan.Core10.Handles.Image'
destroyImage :: forall io . MonadIO io => Device -> Image -> ("allocator" ::: Maybe AllocationCallbacks) -> io ()
destroyImage device image allocator = liftIO . evalContT $ do
  let vkDestroyImage' = mkVkDestroyImage (pVkDestroyImage (deviceCmds (device :: Device)))
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroyImage' (deviceHandle (device)) (image) pAllocator
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetImageSubresourceLayout
  :: FunPtr (Ptr Device_T -> Image -> Ptr ImageSubresource -> Ptr SubresourceLayout -> IO ()) -> Ptr Device_T -> Image -> Ptr ImageSubresource -> Ptr SubresourceLayout -> IO ()

-- | vkGetImageSubresourceLayout - Retrieve information about an image
-- subresource
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the image.
--
-- -   @image@ is the image whose layout is being queried.
--
-- -   @pSubresource@ is a pointer to a 'ImageSubresource' structure
--     selecting a specific image for the image subresource.
--
-- -   @pLayout@ is a pointer to a 'SubresourceLayout' structure in which
--     the layout is returned.
--
-- = Description
--
-- If the image is
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#glossary-linear-resource linear>,
-- then the returned layout is valid for
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-device-hostaccess host access>.
--
-- If the image’s tiling is
-- 'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR' and its
-- format is a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
-- then 'getImageSubresourceLayout' describes one /format plane/ of the
-- image. If the image’s tiling is
-- 'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT',
-- then 'getImageSubresourceLayout' describes one /memory plane/ of the
-- image. If the image’s tiling is
-- 'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT'
-- and the image is
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#glossary-linear-resource non-linear>,
-- then the returned layout has an implementation-dependent meaning; the
-- vendor of the image’s
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#glossary-drm-format-modifier DRM format modifier>
-- /may/ provide documentation that explains how to interpret the returned
-- layout.
--
-- 'getImageSubresourceLayout' is invariant for the lifetime of a single
-- image. However, the subresource layout of images in Android hardware
-- buffer external memory is not known until the image has been bound to
-- memory, so applications /must/ not call 'getImageSubresourceLayout' for
-- such an image before it has been bound.
--
-- == Valid Usage
--
-- -   @image@ /must/ have been created with @tiling@ equal to
--     'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR' or
--     'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT'
--
-- -   The @aspectMask@ member of @pSubresource@ /must/ only have a single
--     bit set
--
-- -   The @mipLevel@ member of @pSubresource@ /must/ be less than the
--     @mipLevels@ specified in 'ImageCreateInfo' when @image@ was created
--
-- -   The @arrayLayer@ member of @pSubresource@ /must/ be less than the
--     @arrayLayers@ specified in 'ImageCreateInfo' when @image@ was
--     created
--
-- -   If the @tiling@ of the @image@ is
--     'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR' and
--     its @format@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>
--     with two planes, the @aspectMask@ member of @pSubresource@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT'
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT'
--
-- -   If the @tiling@ of the @image@ is
--     'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR' and
--     its @format@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>
--     with three planes, the @aspectMask@ member of @pSubresource@ /must/
--     be
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT'
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'
--
-- -   If @image@ was created with the
--     'Graphics.Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID'
--     external memory handle type, then @image@ /must/ be bound to memory.
--
-- -   If the @tiling@ of the @image@ is
--     'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT',
--     then the @aspectMask@ member of @pSubresource@ /must/ be
--     @VK_IMAGE_ASPECT_MEMORY_PLANE_i_BIT_EXT@ and the index @i@ /must/ be
--     less than the
--     <VkDrmFormatModifierPropertiesEXT.html drmFormatModifierPlaneCount>
--     associated with the image’s <VkImageCreateInfo.html format> and
--     <VkImageDrmFormatModifierPropertiesEXT.html drmFormatModifier>.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Device'
--     handle
--
-- -   @image@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Image'
--     handle
--
-- -   @pSubresource@ /must/ be a valid pointer to a valid
--     'ImageSubresource' structure
--
-- -   @pLayout@ /must/ be a valid pointer to a 'SubresourceLayout'
--     structure
--
-- -   @image@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Device',
-- 'Graphics.Vulkan.Core10.Handles.Image', 'ImageSubresource',
-- 'SubresourceLayout'
getImageSubresourceLayout :: forall io . MonadIO io => Device -> Image -> ImageSubresource -> io (SubresourceLayout)
getImageSubresourceLayout device image subresource = liftIO . evalContT $ do
  let vkGetImageSubresourceLayout' = mkVkGetImageSubresourceLayout (pVkGetImageSubresourceLayout (deviceCmds (device :: Device)))
  pSubresource <- ContT $ withCStruct (subresource)
  pPLayout <- ContT (withZeroCStruct @SubresourceLayout)
  lift $ vkGetImageSubresourceLayout' (deviceHandle (device)) (image) pSubresource (pPLayout)
  pLayout <- lift $ peekCStruct @SubresourceLayout pPLayout
  pure $ (pLayout)


-- | VkImageSubresource - Structure specifying an image subresource
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlags',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.SparseImageMemoryBind',
-- 'getImageSubresourceLayout'
data ImageSubresource = ImageSubresource
  { -- | @aspectMask@ /must/ not be @0@
    aspectMask :: ImageAspectFlags
  , -- | @mipLevel@ selects the mipmap level.
    mipLevel :: Word32
  , -- | @arrayLayer@ selects the array layer.
    arrayLayer :: Word32
  }
  deriving (Typeable)
deriving instance Show ImageSubresource

instance ToCStruct ImageSubresource where
  withCStruct x f = allocaBytesAligned 12 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageSubresource{..} f = do
    poke ((p `plusPtr` 0 :: Ptr ImageAspectFlags)) (aspectMask)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (mipLevel)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (arrayLayer)
    f
  cStructSize = 12
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr ImageAspectFlags)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    f

instance FromCStruct ImageSubresource where
  peekCStruct p = do
    aspectMask <- peek @ImageAspectFlags ((p `plusPtr` 0 :: Ptr ImageAspectFlags))
    mipLevel <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    arrayLayer <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    pure $ ImageSubresource
             aspectMask mipLevel arrayLayer

instance Storable ImageSubresource where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageSubresource where
  zero = ImageSubresource
           zero
           zero
           zero


-- | VkImageCreateInfo - Structure specifying the parameters of a newly
-- created image object
--
-- = Description
--
-- Images created with @tiling@ equal to
-- 'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR' have
-- further restrictions on their limits and capabilities compared to images
-- created with @tiling@ equal to
-- 'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL'.
-- Creation of images with tiling
-- 'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR' /may/ not
-- be supported unless other parameters meet all of the constraints:
--
-- -   @imageType@ is
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'
--
-- -   @format@ is not a depth\/stencil format
--
-- -   @mipLevels@ is 1
--
-- -   @arrayLayers@ is 1
--
-- -   @samples@ is
--     'Graphics.Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   @usage@ only includes
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_SRC_BIT'
--     and\/or
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
--
-- Images created with a @format@ from one of those listed in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion>
-- have further restrictions on their limits and capabilities compared to
-- images created with other formats. Creation of images with a format
-- requiring
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion Y′CBCR conversion>
-- /may/ not be supported unless other parameters meet all of the
-- constraints:
--
-- -   @imageType@ is
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'
--
-- -   @mipLevels@ is 1
--
-- -   @arrayLayers@ is 1
--
-- -   @samples@ is
--     'Graphics.Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- Implementations /may/ support additional limits and capabilities beyond
-- those listed above.
--
-- To determine the set of valid @usage@ bits for a given format, call
-- 'Graphics.Vulkan.Core10.DeviceInitialization.getPhysicalDeviceFormatProperties'.
--
-- If the size of the resultant image would exceed @maxResourceSize@, then
-- 'createImage' /must/ fail and return
-- 'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'. This
-- failure /may/ occur even when all image creation parameters satisfy
-- their valid usage requirements.
--
-- Note
--
-- For images created without
-- 'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_EXTENDED_USAGE_BIT'
-- a @usage@ bit is valid if it is supported for the format the image is
-- created with.
--
-- For images created with
-- 'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_EXTENDED_USAGE_BIT'
-- a @usage@ bit is valid if it is supported for at least one of the
-- formats a 'Graphics.Vulkan.Core10.Handles.ImageView' created from the
-- image /can/ have (see
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-views Image Views>
-- for more detail).
--
-- Valid values for some image creation parameters are limited by a
-- numerical upper bound or by inclusion in a bitset. For example,
-- 'ImageCreateInfo'::@arrayLayers@ is limited by
-- @imageCreateMaxArrayLayers@, defined below; and
-- 'ImageCreateInfo'::@samples@ is limited by @imageCreateSampleCounts@,
-- also defined below.
--
-- Several limiting values are defined below, as well as assisting values
-- from which the limiting values are derived. The limiting values are
-- referenced by the relevant valid usage statements of 'ImageCreateInfo'.
--
-- -   Let @uint64_t imageCreateDrmFormatModifiers[]@ be the set of
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#glossary-drm-format-modifier Linux DRM format modifiers>
--     that the resultant image /may/ have.
--
--     -   If @tiling@ is not
--         'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT',
--         then @imageCreateDrmFormatModifiers@ is empty.
--
--     -   If 'ImageCreateInfo'::@pNext@ contains
--         'Graphics.Vulkan.Extensions.VK_EXT_image_drm_format_modifier.ImageDrmFormatModifierExplicitCreateInfoEXT',
--         then @imageCreateDrmFormatModifiers@ contains exactly one
--         modifier,
--         'Graphics.Vulkan.Extensions.VK_EXT_image_drm_format_modifier.ImageDrmFormatModifierExplicitCreateInfoEXT'::@drmFormatModifier@.
--
--     -   If 'ImageCreateInfo'::@pNext@ contains
--         'Graphics.Vulkan.Extensions.VK_EXT_image_drm_format_modifier.ImageDrmFormatModifierListCreateInfoEXT',
--         then @imageCreateDrmFormatModifiers@ contains the exactly the
--         modifiers in
--         'Graphics.Vulkan.Extensions.VK_EXT_image_drm_format_modifier.ImageDrmFormatModifierListCreateInfoEXT'::@pDrmFormatModifiers@.
--
-- -   Let @VkBool32 imageCreateMaybeLinear@ indicate if the resultant
--     image may be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#glossary-linear-resource linear>.
--
--     -   If @tiling@ is
--         'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR',
--         then @imageCreateMaybeLinear@ is @true@.
--
--     -   If @tiling@ is
--         'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL',
--         then @imageCreateMaybeLinear@ is @false@.
--
--     -   If @tiling@ is
--         'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT',
--         then @imageCreateMaybeLinear_@ is @true@ if and only if
--         @imageCreateDrmFormatModifiers@ contains
--         @DRM_FORMAT_MOD_LINEAR@.
--
-- -   Let @VkFormatFeatureFlags imageCreateFormatFeatures@ be the set of
--     format features available during image creation.
--
--     -   If @tiling@ is
--         'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR',
--         then @imageCreateFormatFeatures@ is the value of
--         'Graphics.Vulkan.Core10.DeviceInitialization.ImageFormatProperties'::@linearTilingFeatures@
--         found by calling
--         'Graphics.Vulkan.Core10.DeviceInitialization.getPhysicalDeviceFormatProperties'
--         with parameter @format@ equal to 'ImageCreateInfo'::@format@.
--
--     -   If @tiling@ is
--         'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL',
--         and if the @pNext@ chain includes no
--         'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'
--         structure with non-zero @externalFormat@, then
--         @imageCreateFormatFeatures@ is value of
--         'Graphics.Vulkan.Core10.DeviceInitialization.ImageFormatProperties'::@optimalTilingFeatures@
--         found by calling
--         'Graphics.Vulkan.Core10.DeviceInitialization.getPhysicalDeviceFormatProperties'
--         with parameter @format@ equal to 'ImageCreateInfo'::@format@.
--
--     -   If @tiling@ is
--         'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL',
--         and if the @pNext@ chain includes a
--         'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'
--         structure with non-zero @externalFormat@, then
--         @imageCreateFormatFeatures@ is the value of
--         'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.AndroidHardwareBufferFormatPropertiesANDROID'::@formatFeatures@
--         obtained by
--         'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.getAndroidHardwareBufferPropertiesANDROID'
--         with a matching @externalFormat@ value.
--
--     -   If @tiling@ is
--         'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT',
--         then the value of @imageCreateFormatFeatures@ is found by
--         calling
--         'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFormatProperties2'
--         with
--         'Graphics.Vulkan.Core10.DeviceInitialization.ImageFormatProperties'::@format@
--         equal to 'ImageCreateInfo'::@format@ and with
--         'Graphics.Vulkan.Extensions.VK_EXT_image_drm_format_modifier.DrmFormatModifierPropertiesListEXT'
--         chained into
--         'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.ImageFormatProperties2';
--         by collecting all members of the returned array
--         'Graphics.Vulkan.Extensions.VK_EXT_image_drm_format_modifier.DrmFormatModifierPropertiesListEXT'::@pDrmFormatModifierProperties@
--         whose @drmFormatModifier@ belongs to
--         @imageCreateDrmFormatModifiers@; and by taking the bitwise
--         intersection, over the collected array members, of
--         @drmFormatModifierTilingFeatures@. (The resultant
--         @imageCreateFormatFeatures@ /may/ be empty).
--
-- -   Let
--     @VkImageFormatProperties2 imageCreateImageFormatPropertiesList[]@ be
--     defined as follows.
--
--     -   If 'ImageCreateInfo'::@pNext@ contains no
--         'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'
--         structure with non-zero @externalFormat@, then
--         @imageCreateImageFormatPropertiesList@ is the list of structures
--         obtained by calling
--         'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2',
--         possibly multiple times, as follows:
--
--         -   The parameters
--             'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2'::@format@,
--             @imageType@, @tiling@, @usage@, and @flags@ /must/ be equal
--             to those in 'ImageCreateInfo'.
--
--         -   If 'ImageCreateInfo'::@pNext@ contains a
--             'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo'
--             structure whose @handleTypes@ is not @0@, then
--             'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2'::@pNext@
--             /must/ contain a
--             'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.PhysicalDeviceExternalImageFormatInfo'
--             structure whose @handleType@ is not @0@; and
--             'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--             /must/ be called for each handle type in
--             'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo'::@handleTypes@,
--             successively setting
--             'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.PhysicalDeviceExternalImageFormatInfo'::@handleType@
--             on each call.
--
--         -   If 'ImageCreateInfo'::@pNext@ contains no
--             'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo'
--             structure, or contains a structure whose @handleTypes@ is
--             @0@, then
--             'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2'::@pNext@
--             /must/ either contain no
--             'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.PhysicalDeviceExternalImageFormatInfo'
--             structure, or contain a structure whose @handleType@ is @0@.
--
--         -   If @tiling@ is
--             'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT',
--             then
--             'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2'::@pNext@
--             /must/ contain a
--             'Graphics.Vulkan.Extensions.VK_EXT_image_drm_format_modifier.PhysicalDeviceImageDrmFormatModifierInfoEXT'
--             structure where @sharingMode@ is equal to
--             'ImageCreateInfo'::@sharingMode@; and, if @sharingMode@ is
--             'Graphics.Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT',
--             then @queueFamilyIndexCount@ and @pQueueFamilyIndices@
--             /must/ be equal to those in 'ImageCreateInfo'; and, if
--             @flags@ contains
--             'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_MUTABLE_FORMAT_BIT',
--             then the
--             'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo'
--             structure included in the @pNext@ chain of
--             'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2'
--             /must/ be equivalent to the one included in the @pNext@
--             chain of 'ImageCreateInfo'; and
--             'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--             /must/ be called for each modifier in
--             @imageCreateDrmFormatModifiers@, successively setting
--             'Graphics.Vulkan.Extensions.VK_EXT_image_drm_format_modifier.PhysicalDeviceImageDrmFormatModifierInfoEXT'::@drmFormatModifier@
--             on each call.
--
--         -   If @tiling@ is not
--             'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT',
--             then
--             'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2'::@pNext@
--             /must/ contain no
--             'Graphics.Vulkan.Extensions.VK_EXT_image_drm_format_modifier.PhysicalDeviceImageDrmFormatModifierInfoEXT'
--             structure.
--
--         -   If any call to
--             'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--             returns an error, then
--             @imageCreateImageFormatPropertiesList@ is defined to be the
--             empty list.
--
--     -   If 'ImageCreateInfo'::@pNext@ contains a
--         'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'
--         structure with non-zero @externalFormat@, then
--         @imageCreateImageFormatPropertiesList@ contains a single element
--         where:
--
--         -   'Graphics.Vulkan.Core10.DeviceInitialization.ImageFormatProperties'::@maxMipLevels@
--             is ⌊log2(max(@extent.width@, @extent.height@,
--             @extent.depth@))⌋ + 1.
--
--         -   'Graphics.Vulkan.Core10.DeviceInitialization.ImageFormatProperties'::@maxArrayLayers@
--             is
--             'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::maxImageArrayLayers.
--
--         -   Each component of
--             'Graphics.Vulkan.Core10.DeviceInitialization.ImageFormatProperties'::@maxExtent@
--             is
--             'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::maxImageDimension2D.
--
--         -   'Graphics.Vulkan.Core10.DeviceInitialization.ImageFormatProperties'::@sampleCounts@
--             contains exactly
--             'Graphics.Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'.
--
-- -   Let @uint32_t imageCreateMaxMipLevels@ be the minimum value of
--     'Graphics.Vulkan.Core10.DeviceInitialization.ImageFormatProperties'::@maxMipLevels@
--     in @imageCreateImageFormatPropertiesList@. The value is undefined if
--     @imageCreateImageFormatPropertiesList@ is empty.
--
-- -   Let @uint32_t imageCreateMaxArrayLayers@ be the minimum value of
--     'Graphics.Vulkan.Core10.DeviceInitialization.ImageFormatProperties'::@maxArrayLayers@
--     in @imageCreateImageFormatPropertiesList@. The value is undefined if
--     @imageCreateImageFormatPropertiesList@ is empty.
--
-- -   Let @VkExtent3D imageCreateMaxExtent@ be the component-wise minimum
--     over all
--     'Graphics.Vulkan.Core10.DeviceInitialization.ImageFormatProperties'::@maxExtent@
--     values in @imageCreateImageFormatPropertiesList@. The value is
--     undefined if @imageCreateImageFormatPropertiesList@ is empty.
--
-- -   Let @VkSampleCountFlags imageCreateSampleCounts@ be the intersection
--     of each
--     'Graphics.Vulkan.Core10.DeviceInitialization.ImageFormatProperties'::@sampleCounts@
--     in @imageCreateImageFormatPropertiesList@. The value is undefined if
--     @imageCreateImageFormatPropertiesList@ is empty.
--
-- = Valid Usage
--
-- -   Each of the following values (as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>)
--     /must/ not be undefined @imageCreateMaxMipLevels@,
--     @imageCreateMaxArrayLayers@, @imageCreateMaxExtent@, and
--     @imageCreateSampleCounts@.
--
-- -   If @sharingMode@ is
--     'Graphics.Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT',
--     @pQueueFamilyIndices@ /must/ be a valid pointer to an array of
--     @queueFamilyIndexCount@ @uint32_t@ values
--
-- -   If @sharingMode@ is
--     'Graphics.Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT',
--     @queueFamilyIndexCount@ /must/ be greater than @1@
--
-- -   If @sharingMode@ is
--     'Graphics.Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT',
--     each element of @pQueueFamilyIndices@ /must/ be unique and /must/ be
--     less than @pQueueFamilyPropertyCount@ returned by either
--     'Graphics.Vulkan.Core10.DeviceInitialization.getPhysicalDeviceQueueFamilyProperties'
--     or
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceQueueFamilyProperties2'
--     for the @physicalDevice@ that was used to create @device@
--
-- -   If the @pNext@ chain includes a
--     'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'
--     structure, and its @externalFormat@ member is non-zero the @format@
--     /must/ be 'Graphics.Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'.
--
-- -   If the @pNext@ chain does not include a
--     'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'
--     structure, or does and its @externalFormat@ member is @0@, the
--     @format@ /must/ not be
--     'Graphics.Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'.
--
-- -   @extent.width@ /must/ be greater than @0@.
--
-- -   @extent.height@ /must/ be greater than @0@.
--
-- -   @extent.depth@ /must/ be greater than @0@.
--
-- -   @mipLevels@ /must/ be greater than @0@
--
-- -   @arrayLayers@ /must/ be greater than @0@
--
-- -   If @flags@ contains
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CUBE_COMPATIBLE_BIT',
--     @imageType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'
--
-- -   If @flags@ contains
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT',
--     @imageType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'
--
-- -   If @flags@ contains
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT',
--     @imageType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D'
--
-- -   @extent.width@ /must/ be less than or equal to
--     @imageCreateMaxExtent.width@ (as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>).
--
-- -   @extent.height@ /must/ be less than or equal to
--     @imageCreateMaxExtent.height@ (as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>).
--
-- -   @extent.depth@ /must/ be less than or equal to
--     @imageCreateMaxExtent.depth@ (as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>).
--
-- -   If @imageType@ is
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D' and @flags@
--     contains
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CUBE_COMPATIBLE_BIT',
--     @extent.width@ and @extent.height@ /must/ be equal and @arrayLayers@
--     /must/ be greater than or equal to 6
--
-- -   If @imageType@ is
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', both
--     @extent.height@ and @extent.depth@ /must/ be @1@
--
-- -   If @imageType@ is
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D',
--     @extent.depth@ /must/ be @1@
--
-- -   @mipLevels@ /must/ be less than or equal to the number of levels in
--     the complete mipmap chain based on @extent.width@, @extent.height@,
--     and @extent.depth@.
--
-- -   @mipLevels@ /must/ be less than or equal to
--     @imageCreateMaxMipLevels@ (as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>).
--
-- -   @arrayLayers@ /must/ be less than or equal to
--     @imageCreateMaxArrayLayers@ (as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>).
--
-- -   If @imageType@ is
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D',
--     @arrayLayers@ /must/ be @1@.
--
-- -   If @samples@ is not
--     'Graphics.Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT',
--     then @imageType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', @flags@
--     /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CUBE_COMPATIBLE_BIT',
--     @mipLevels@ /must/ be equal to @1@, and @imageCreateMaybeLinear@ (as
--     defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>)
--     /must/ be @false@,
--
-- -   If @samples@ is not
--     'Graphics.Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT',
--     @usage@ /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT'
--
-- -   If @usage@ includes
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT',
--     then bits other than
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT',
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT',
--     and
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--     /must/ not be set
--
-- -   If @usage@ includes
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT',
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT',
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT',
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT',
--     @extent.width@ /must/ be less than or equal to
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxFramebufferWidth@
--
-- -   If @usage@ includes
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT',
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT',
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT',
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT',
--     @extent.height@ /must/ be less than or equal to
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxFramebufferHeight@
--
-- -   If @usage@ includes
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT',
--     @extent.width@ /must/ be less than or equal to
--     \(\lceil{\frac{maxFramebufferWidth}{minFragmentDensityTexelSize_{width}}}\rceil\)
--
-- -   If @usage@ includes
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT',
--     @extent.height@ /must/ be less than or equal to
--     \(\lceil{\frac{maxFramebufferHeight}{minFragmentDensityTexelSize_{height}}}\rceil\)
--
-- -   If @usage@ includes
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT',
--     @usage@ /must/ also contain at least one of
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT',
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT',
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT'.
--
-- -   @samples@ /must/ be a bit value that is set in
--     @imageCreateSampleCounts@ (as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>).
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderStorageImageMultisample multisampled storage images>
--     feature is not enabled, and @usage@ contains
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_STORAGE_BIT',
--     @samples@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseBinding sparse bindings>
--     feature is not enabled, @flags@ /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_BINDING_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseResidencyAliased sparse aliased residency>
--     feature is not enabled, @flags@ /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_ALIASED_BIT'
--
-- -   If @imageType@ is
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', @flags@
--     /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseResidencyImage2D sparse residency for 2D images>
--     feature is not enabled, and @imageType@ is
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', @flags@
--     /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseResidencyImage3D sparse residency for 3D images>
--     feature is not enabled, and @imageType@ is
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', @flags@
--     /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseResidency2Samples sparse residency for images with 2 samples>
--     feature is not enabled, @imageType@ is
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', and
--     @samples@ is
--     'Graphics.Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_2_BIT',
--     @flags@ /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseResidency4Samples sparse residency for images with 4 samples>
--     feature is not enabled, @imageType@ is
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', and
--     @samples@ is
--     'Graphics.Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_4_BIT',
--     @flags@ /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseResidency8Samples sparse residency for images with 8 samples>
--     feature is not enabled, @imageType@ is
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', and
--     @samples@ is
--     'Graphics.Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_8_BIT',
--     @flags@ /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseResidency16Samples sparse residency for images with 16 samples>
--     feature is not enabled, @imageType@ is
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', and
--     @samples@ is
--     'Graphics.Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_16_BIT',
--     @flags@ /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--
-- -   If @flags@ contains
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_ALIASED_BIT',
--     it /must/ also contain
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_BINDING_BIT'
--
-- -   If any of the bits
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_BINDING_BIT',
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT',
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_ALIASED_BIT'
--     are set,
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT'
--     /must/ not also be set
--
-- -   If the protected memory feature is not enabled, @flags@ /must/ not
--     contain
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_PROTECTED_BIT'.
--
-- -   If any of the bits
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_BINDING_BIT',
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT',
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_ALIASED_BIT'
--     are set,
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_PROTECTED_BIT'
--     /must/ not also be set.
--
-- -   If the @pNext@ chain includes a
--     'Graphics.Vulkan.Extensions.VK_NV_external_memory.ExternalMemoryImageCreateInfoNV'
--     structure, it /must/ not contain a
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo'
--     structure.
--
-- -   If the @pNext@ chain includes a
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo'
--     structure, its @handleTypes@ member /must/ only contain bits that
--     are also in
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.ExternalImageFormatProperties'::@externalMemoryProperties.compatibleHandleTypes@,
--     as returned by
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--     with @format@, @imageType@, @tiling@, @usage@, and @flags@ equal to
--     those in this structure, and with a
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.PhysicalDeviceExternalImageFormatInfo'
--     structure included in the @pNext@ chain, with a @handleType@ equal
--     to any one of the handle types specified in
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo'::@handleTypes@
--
-- -   If the @pNext@ chain includes a
--     'Graphics.Vulkan.Extensions.VK_NV_external_memory.ExternalMemoryImageCreateInfoNV'
--     structure, its @handleTypes@ member /must/ only contain bits that
--     are also in
--     'Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities.ExternalImageFormatPropertiesNV'::@externalMemoryProperties.compatibleHandleTypes@,
--     as returned by
--     'Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities.getPhysicalDeviceExternalImageFormatPropertiesNV'
--     with @format@, @imageType@, @tiling@, @usage@, and @flags@ equal to
--     those in this structure, and with @externalHandleType@ equal to any
--     one of the handle types specified in
--     'Graphics.Vulkan.Extensions.VK_NV_external_memory.ExternalMemoryImageCreateInfoNV'::@handleTypes@
--
-- -   If the logical device was created with
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation.DeviceGroupDeviceCreateInfo'::@physicalDeviceCount@
--     equal to 1, @flags@ /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT'
--
-- -   If @flags@ contains
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT',
--     then @mipLevels@ /must/ be one, @arrayLayers@ /must/ be one,
--     @imageType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'. and
--     @imageCreateMaybeLinear@ (as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>)
--     /must/ be @false@.
--
-- -   If @flags@ contains
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT',
--     then @format@ /must/ be a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#appendix-compressedtex-bc block-compressed image format>,
--     an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#appendix-compressedtex-etc2 ETC compressed image format>,
--     or an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#appendix-compressedtex-astc ASTC compressed image format>.
--
-- -   If @flags@ contains
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT',
--     then @flags@ /must/ also contain
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_MUTABLE_FORMAT_BIT'.
--
-- -   @initialLayout@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED' or
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_PREINITIALIZED'.
--
-- -   If the @pNext@ chain includes a
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo'
--     or
--     'Graphics.Vulkan.Extensions.VK_NV_external_memory.ExternalMemoryImageCreateInfoNV'
--     structure whose @handleTypes@ member is not @0@, @initialLayout@
--     /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED'
--
-- -   If the image @format@ is one of those listed in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion>,
--     then @mipLevels@ /must/ be 1
--
-- -   If the image @format@ is one of those listed in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion>,
--     @samples@ must be
--     'Graphics.Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   If the image @format@ is one of those listed in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion>,
--     @imageType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'
--
-- -   If the image @format@ is one of those listed in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion>,
--     and the @ycbcrImageArrays@ feature is not enabled, @arrayLayers@
--     /must/ be 1
--
-- -   If @format@ is a /multi-planar/ format, and if
--     @imageCreateFormatFeatures@ (as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>)
--     does not contain
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_DISJOINT_BIT',
--     then @flags@ /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_DISJOINT_BIT'
--
-- -   If @format@ is not a /multi-planar/ format, and @flags@ does not
--     include
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_ALIAS_BIT',
--     @flags@ /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_DISJOINT_BIT'
--
-- -   If @tiling@ is
--     'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT',
--     then the @pNext@ chain /must/ include exactly one of
--     'Graphics.Vulkan.Extensions.VK_EXT_image_drm_format_modifier.ImageDrmFormatModifierListCreateInfoEXT'
--     or
--     'Graphics.Vulkan.Extensions.VK_EXT_image_drm_format_modifier.ImageDrmFormatModifierExplicitCreateInfoEXT'
--     structures
--
-- -   If the @pNext@ chain includes a
--     'Graphics.Vulkan.Extensions.VK_EXT_image_drm_format_modifier.ImageDrmFormatModifierListCreateInfoEXT'
--     or
--     'Graphics.Vulkan.Extensions.VK_EXT_image_drm_format_modifier.ImageDrmFormatModifierExplicitCreateInfoEXT'
--     structure, then @tiling@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT'
--
-- -   If @tiling@ is
--     'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT'
--     and @flags@ contains
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_MUTABLE_FORMAT_BIT',
--     then the @pNext@ chain /must/ include a
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo'
--     structure with non-zero @viewFormatCount@.
--
-- -   If @flags@ contains
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT'
--     @format@ /must/ be a depth or depth\/stencil format
--
-- -   If the @pNext@ chain includes a
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo'
--     structure whose @handleTypes@ member includes
--     'Graphics.Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID',
--     @imageType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'.
--
-- -   If the @pNext@ chain includes a
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo'
--     structure whose @handleTypes@ member includes
--     'Graphics.Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID',
--     @mipLevels@ /must/ either be @1@ or equal to the number of levels in
--     the complete mipmap chain based on @extent.width@, @extent.height@,
--     and @extent.depth@.
--
-- -   If the @pNext@ chain includes a
--     'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'
--     structure whose @externalFormat@ member is not @0@, @flags@ /must/
--     not include
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_MUTABLE_FORMAT_BIT'.
--
-- -   If the @pNext@ chain includes a
--     'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'
--     structure whose @externalFormat@ member is not @0@, @usage@ /must/
--     not include any usages except
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT'.
--
-- -   If the @pNext@ chain includes a
--     'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'
--     structure whose @externalFormat@ member is not @0@, @tiling@ /must/
--     be 'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL'.
--
-- -   If @format@ is a depth-stencil format, @usage@ includes
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT',
--     and the @pNext@ chain includes a
--     'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'
--     structure, then its
--     'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'::@stencilUsage@
--     member /must/ also include
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   If @format@ is a depth-stencil format, @usage@ does not include
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT',
--     and the @pNext@ chain includes a
--     'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'
--     structure, then its
--     'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'::@stencilUsage@
--     member /must/ also not include
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   If @format@ is a depth-stencil format, @usage@ includes
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT',
--     and the @pNext@ chain includes a
--     'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'
--     structure, then its
--     'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'::@stencilUsage@
--     member /must/ also include
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT'
--
-- -   If @format@ is a depth-stencil format, @usage@ does not include
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT',
--     and the @pNext@ chain includes a
--     'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'
--     structure, then its
--     'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'::@stencilUsage@
--     member /must/ also not include
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT'
--
-- -   If 'Graphics.Vulkan.Core10.Enums.Format.Format' is a depth-stencil
--     format and the @pNext@ chain includes a
--     'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'
--     structure with its @stencilUsage@ member including
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT',
--     @extent.width@ /must/ be less than or equal to
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxFramebufferWidth@
--
-- -   If @format@ is a depth-stencil format and the @pNext@ chain includes
--     a
--     'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'
--     structure with its @stencilUsage@ member including
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT',
--     @extent.height@ /must/ be less than or equal to
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxFramebufferHeight@
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderStorageImageMultisample multisampled storage images>
--     feature is not enabled, @format@ is a depth-stencil format and the
--     @pNext@ chain includes a
--     'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'
--     structure with its @stencilUsage@ including
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_STORAGE_BIT',
--     @samples@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   If @flags@ contains
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV',
--     @imageType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D' or
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D'
--
-- -   If @flags@ contains
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV',
--     it /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CUBE_COMPATIBLE_BIT'
--     and the @format@ /must/ not be a depth\/stencil format
--
-- -   If @flags@ contains
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     and @imageType@ is
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D',
--     @extent.width@ and @extent.height@ /must/ be greater than @1@
--
-- -   If @flags@ contains
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     and @imageType@ is
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D',
--     @extent.width@, @extent.height@, and @extent.depth@ /must/ be
--     greater than @1@
--
-- -   If @usage@ includes
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV',
--     @imageType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'.
--
-- -   If @usage@ includes
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV',
--     @samples@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'.
--
-- -   If @usage@ includes
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV',
--     @tiling@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL'.
--
-- -   If @flags@ contains
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT',
--     @tiling@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL'
--
-- -   If @flags@ contains
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT',
--     @imageType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'
--
-- -   If @flags@ contains
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT',
--     @flags@ /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CUBE_COMPATIBLE_BIT'
--
-- -   If @flags@ contains
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT',
--     @mipLevels@ /must/ be @1@
--
-- = Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_CREATE_INFO'
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation.DedicatedAllocationImageCreateInfoNV',
--     'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID',
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo',
--     'Graphics.Vulkan.Extensions.VK_NV_external_memory.ExternalMemoryImageCreateInfoNV',
--     'Graphics.Vulkan.Extensions.VK_EXT_image_drm_format_modifier.ImageDrmFormatModifierExplicitCreateInfoEXT',
--     'Graphics.Vulkan.Extensions.VK_EXT_image_drm_format_modifier.ImageDrmFormatModifierListCreateInfoEXT',
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo',
--     'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo',
--     or
--     'Graphics.Vulkan.Extensions.VK_KHR_swapchain.ImageSwapchainCreateInfoKHR'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   @flags@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlagBits'
--     values
--
-- -   @imageType@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.ImageType.ImageType' value
--
-- -   @format@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.Format.Format' value
--
-- -   @samples@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits'
--     value
--
-- -   @tiling@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.ImageTiling.ImageTiling' value
--
-- -   @usage@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits'
--     values
--
-- -   @usage@ /must/ not be @0@
--
-- -   @sharingMode@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.SharingMode.SharingMode' value
--
-- -   @initialLayout@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- \<\/section>
-- = See Also
--
-- 'Graphics.Vulkan.Core10.SharedTypes.Extent3D',
-- 'Graphics.Vulkan.Core10.Enums.Format.Format',
-- 'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlags',
-- 'Graphics.Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Graphics.Vulkan.Core10.Enums.ImageTiling.ImageTiling',
-- 'Graphics.Vulkan.Core10.Enums.ImageType.ImageType',
-- 'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlags',
-- 'Graphics.Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits',
-- 'Graphics.Vulkan.Core10.Enums.SharingMode.SharingMode',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createImage'
data ImageCreateInfo (es :: [Type]) = ImageCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
    next :: Chain es
  , -- | @flags@ is a bitmask of
    -- 'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlagBits'
    -- describing additional parameters of the image.
    flags :: ImageCreateFlags
  , -- | @imageType@ is a 'Graphics.Vulkan.Core10.Enums.ImageType.ImageType'
    -- value specifying the basic dimensionality of the image. Layers in array
    -- textures do not count as a dimension for the purposes of the image type.
    imageType :: ImageType
  , -- | @format@ is a 'Graphics.Vulkan.Core10.Enums.Format.Format' describing
    -- the format and type of the texel blocks that will be contained in the
    -- image.
    format :: Format
  , -- | @extent@ is a 'Graphics.Vulkan.Core10.SharedTypes.Extent3D' describing
    -- the number of data elements in each dimension of the base level.
    extent :: Extent3D
  , -- | @mipLevels@ describes the number of levels of detail available for
    -- minified sampling of the image.
    mipLevels :: Word32
  , -- | @arrayLayers@ is the number of layers in the image.
    arrayLayers :: Word32
  , -- | @samples@ is a
    -- 'Graphics.Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits'
    -- specifying the number of
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-multisampling samples per texel>.
    samples :: SampleCountFlagBits
  , -- | @tiling@ is a 'Graphics.Vulkan.Core10.Enums.ImageTiling.ImageTiling'
    -- value specifying the tiling arrangement of the texel blocks in memory.
    tiling :: ImageTiling
  , -- | @usage@ is a bitmask of
    -- 'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits'
    -- describing the intended usage of the image.
    usage :: ImageUsageFlags
  , -- | @sharingMode@ is a
    -- 'Graphics.Vulkan.Core10.Enums.SharingMode.SharingMode' value specifying
    -- the sharing mode of the image when it will be accessed by multiple queue
    -- families.
    sharingMode :: SharingMode
  , -- | @pQueueFamilyIndices@ is a list of queue families that will access this
    -- image (ignored if @sharingMode@ is not
    -- 'Graphics.Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT').
    queueFamilyIndices :: Vector Word32
  , -- | @initialLayout@ is a
    -- 'Graphics.Vulkan.Core10.Enums.ImageLayout.ImageLayout' value specifying
    -- the initial 'Graphics.Vulkan.Core10.Enums.ImageLayout.ImageLayout' of
    -- all image subresources of the image. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-layouts Image Layouts>.
    initialLayout :: ImageLayout
  }
  deriving (Typeable)
deriving instance Show (Chain es) => Show (ImageCreateInfo es)

instance Extensible ImageCreateInfo where
  extensibleType = STRUCTURE_TYPE_IMAGE_CREATE_INFO
  setNext x next = x{next = next}
  getNext ImageCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends ImageCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @ImageStencilUsageCreateInfo = Just f
    | Just Refl <- eqT @e @ImageDrmFormatModifierExplicitCreateInfoEXT = Just f
    | Just Refl <- eqT @e @ImageDrmFormatModifierListCreateInfoEXT = Just f
    | Just Refl <- eqT @e @ExternalFormatANDROID = Just f
    | Just Refl <- eqT @e @ImageFormatListCreateInfo = Just f
    | Just Refl <- eqT @e @ImageSwapchainCreateInfoKHR = Just f
    | Just Refl <- eqT @e @ExternalMemoryImageCreateInfo = Just f
    | Just Refl <- eqT @e @ExternalMemoryImageCreateInfoNV = Just f
    | Just Refl <- eqT @e @DedicatedAllocationImageCreateInfoNV = Just f
    | otherwise = Nothing

instance PokeChain es => ToCStruct (ImageCreateInfo es) where
  withCStruct x f = allocaBytesAligned 88 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr ImageCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr ImageType)) (imageType)
    lift $ poke ((p `plusPtr` 24 :: Ptr Format)) (format)
    ContT $ pokeCStruct ((p `plusPtr` 28 :: Ptr Extent3D)) (extent) . ($ ())
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) (mipLevels)
    lift $ poke ((p `plusPtr` 44 :: Ptr Word32)) (arrayLayers)
    lift $ poke ((p `plusPtr` 48 :: Ptr SampleCountFlagBits)) (samples)
    lift $ poke ((p `plusPtr` 52 :: Ptr ImageTiling)) (tiling)
    lift $ poke ((p `plusPtr` 56 :: Ptr ImageUsageFlags)) (usage)
    lift $ poke ((p `plusPtr` 60 :: Ptr SharingMode)) (sharingMode)
    lift $ poke ((p `plusPtr` 64 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (queueFamilyIndices)) :: Word32))
    pPQueueFamilyIndices' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (queueFamilyIndices)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPQueueFamilyIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (queueFamilyIndices)
    lift $ poke ((p `plusPtr` 72 :: Ptr (Ptr Word32))) (pPQueueFamilyIndices')
    lift $ poke ((p `plusPtr` 80 :: Ptr ImageLayout)) (initialLayout)
    lift $ f
  cStructSize = 88
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 20 :: Ptr ImageType)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr Format)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 28 :: Ptr Extent3D)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 44 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 48 :: Ptr SampleCountFlagBits)) (zero)
    lift $ poke ((p `plusPtr` 52 :: Ptr ImageTiling)) (zero)
    lift $ poke ((p `plusPtr` 56 :: Ptr ImageUsageFlags)) (zero)
    lift $ poke ((p `plusPtr` 60 :: Ptr SharingMode)) (zero)
    pPQueueFamilyIndices' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPQueueFamilyIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (mempty)
    lift $ poke ((p `plusPtr` 72 :: Ptr (Ptr Word32))) (pPQueueFamilyIndices')
    lift $ poke ((p `plusPtr` 80 :: Ptr ImageLayout)) (zero)
    lift $ f

instance PeekChain es => FromCStruct (ImageCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @ImageCreateFlags ((p `plusPtr` 16 :: Ptr ImageCreateFlags))
    imageType <- peek @ImageType ((p `plusPtr` 20 :: Ptr ImageType))
    format <- peek @Format ((p `plusPtr` 24 :: Ptr Format))
    extent <- peekCStruct @Extent3D ((p `plusPtr` 28 :: Ptr Extent3D))
    mipLevels <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    arrayLayers <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    samples <- peek @SampleCountFlagBits ((p `plusPtr` 48 :: Ptr SampleCountFlagBits))
    tiling <- peek @ImageTiling ((p `plusPtr` 52 :: Ptr ImageTiling))
    usage <- peek @ImageUsageFlags ((p `plusPtr` 56 :: Ptr ImageUsageFlags))
    sharingMode <- peek @SharingMode ((p `plusPtr` 60 :: Ptr SharingMode))
    queueFamilyIndexCount <- peek @Word32 ((p `plusPtr` 64 :: Ptr Word32))
    pQueueFamilyIndices <- peek @(Ptr Word32) ((p `plusPtr` 72 :: Ptr (Ptr Word32)))
    pQueueFamilyIndices' <- generateM (fromIntegral queueFamilyIndexCount) (\i -> peek @Word32 ((pQueueFamilyIndices `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    initialLayout <- peek @ImageLayout ((p `plusPtr` 80 :: Ptr ImageLayout))
    pure $ ImageCreateInfo
             next flags imageType format extent mipLevels arrayLayers samples tiling usage sharingMode pQueueFamilyIndices' initialLayout

instance es ~ '[] => Zero (ImageCreateInfo es) where
  zero = ImageCreateInfo
           ()
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           mempty
           zero


-- | VkSubresourceLayout - Structure specifying subresource layout
--
-- = Description
--
-- If the image is
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#glossary-linear-resource linear>,
-- then @rowPitch@, @arrayPitch@ and @depthPitch@ describe the layout of
-- the image subresource in linear memory. For uncompressed formats,
-- @rowPitch@ is the number of bytes between texels with the same x
-- coordinate in adjacent rows (y coordinates differ by one). @arrayPitch@
-- is the number of bytes between texels with the same x and y coordinate
-- in adjacent array layers of the image (array layer values differ by
-- one). @depthPitch@ is the number of bytes between texels with the same x
-- and y coordinate in adjacent slices of a 3D image (z coordinates differ
-- by one). Expressed as an addressing formula, the starting byte of a
-- texel in the image subresource has address:
--
-- > // (x,y,z,layer) are in texel coordinates
-- > address(x,y,z,layer) = layer*arrayPitch + z*depthPitch + y*rowPitch + x*elementSize + offset
--
-- For compressed formats, the @rowPitch@ is the number of bytes between
-- compressed texel blocks in adjacent rows. @arrayPitch@ is the number of
-- bytes between compressed texel blocks in adjacent array layers.
-- @depthPitch@ is the number of bytes between compressed texel blocks in
-- adjacent slices of a 3D image.
--
-- > // (x,y,z,layer) are in compressed texel block coordinates
-- > address(x,y,z,layer) = layer*arrayPitch + z*depthPitch + y*rowPitch + x*compressedTexelBlockByteSize + offset;
--
-- The value of @arrayPitch@ is undefined for images that were not created
-- as arrays. @depthPitch@ is defined only for 3D images.
--
-- If the image has a /single-plane/ color format and its tiling is
-- 'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR' , then
-- the @aspectMask@ member of 'ImageSubresource' /must/ be
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'.
--
-- If the image has a depth\/stencil format and its tiling is
-- 'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR' , then
-- @aspectMask@ /must/ be either
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT'
-- or
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'.
-- On implementations that store depth and stencil aspects separately,
-- querying each of these image subresource layouts will return a different
-- @offset@ and @size@ representing the region of memory used for that
-- aspect. On implementations that store depth and stencil aspects
-- interleaved, the same @offset@ and @size@ are returned and represent the
-- interleaved memory allocation.
--
-- If the image has a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>
-- and its tiling is
-- 'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR' , then
-- the @aspectMask@ member of 'ImageSubresource' /must/ be
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT',
-- or (for 3-plane formats only)
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'.
-- Querying each of these image subresource layouts will return a different
-- @offset@ and @size@ representing the region of memory used for that
-- plane. If the image is /disjoint/, then the @offset@ is relative to the
-- base address of the plane. If the image is /non-disjoint/, then the
-- @offset@ is relative to the base address of the image.
--
-- If the image’s tiling is
-- 'Graphics.Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT',
-- then the @aspectMask@ member of 'ImageSubresource' /must/ be one of
-- @VK_IMAGE_ASPECT_MEMORY_PLANE_i_BIT_EXT@, where the maximum allowed
-- plane index @i@ is defined by the
-- <VkDrmFormatModifierPropertiesEXT.html drmFormatModifierPlaneCount>
-- associated with the image’s <VkImageCreateInfo.html format> and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#glossary-drm-format-modifier modifier>.
-- The memory range used by the subresource is described by @offset@ and
-- @size@. If the image is /disjoint/, then the @offset@ is relative to the
-- base address of the /memory plane/. If the image is /non-disjoint/, then
-- the @offset@ is relative to the base address of the image. If the image
-- is
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#glossary-linear-resource non-linear>,
-- then @rowPitch@, @arrayPitch@, and @depthPitch@ have an
-- implementation-dependent meaning.
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.DeviceSize',
-- 'Graphics.Vulkan.Extensions.VK_EXT_image_drm_format_modifier.ImageDrmFormatModifierExplicitCreateInfoEXT',
-- 'getImageSubresourceLayout'
data SubresourceLayout = SubresourceLayout
  { -- | @offset@ is the byte offset from the start of the image or the plane
    -- where the image subresource begins.
    offset :: DeviceSize
  , -- | @size@ is the size in bytes of the image subresource. @size@ includes
    -- any extra memory that is required based on @rowPitch@.
    size :: DeviceSize
  , -- | @rowPitch@ describes the number of bytes between each row of texels in
    -- an image.
    rowPitch :: DeviceSize
  , -- | @arrayPitch@ describes the number of bytes between each array layer of
    -- an image.
    arrayPitch :: DeviceSize
  , -- | @depthPitch@ describes the number of bytes between each slice of 3D
    -- image.
    depthPitch :: DeviceSize
  }
  deriving (Typeable)
deriving instance Show SubresourceLayout

instance ToCStruct SubresourceLayout where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SubresourceLayout{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceSize)) (offset)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (size)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (rowPitch)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (arrayPitch)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (depthPitch)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct SubresourceLayout where
  peekCStruct p = do
    offset <- peek @DeviceSize ((p `plusPtr` 0 :: Ptr DeviceSize))
    size <- peek @DeviceSize ((p `plusPtr` 8 :: Ptr DeviceSize))
    rowPitch <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    arrayPitch <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    depthPitch <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    pure $ SubresourceLayout
             offset size rowPitch arrayPitch depthPitch

instance Storable SubresourceLayout where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SubresourceLayout where
  zero = SubresourceLayout
           zero
           zero
           zero
           zero
           zero

