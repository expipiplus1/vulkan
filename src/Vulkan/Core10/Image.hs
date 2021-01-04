{-# language CPP #-}
-- No documentation found for Chapter "Image"
module Vulkan.Core10.Image  ( createImage
                            , withImage
                            , destroyImage
                            , getImageSubresourceLayout
                            , ImageCreateInfo(..)
                            , SubresourceLayout(..)
                            , Image(..)
                            , ImageLayout(..)
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
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
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
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_dedicated_allocation (DedicatedAllocationImageCreateInfoNV)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkCreateImage))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyImage))
import Vulkan.Dynamic (DeviceCmds(pVkGetImageSubresourceLayout))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.FundamentalTypes (Extent3D)
import {-# SOURCE #-} Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer (ExternalFormatANDROID)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_memory (ExternalMemoryImageCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_external_memory (ExternalMemoryImageCreateInfoNV)
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.Core10.Handles (Image)
import Vulkan.Core10.Handles (Image(..))
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_drm_format_modifier (ImageDrmFormatModifierExplicitCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_drm_format_modifier (ImageDrmFormatModifierListCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_image_format_list (ImageFormatListCreateInfo)
import Vulkan.Core10.Enums.ImageLayout (ImageLayout)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage (ImageStencilUsageCreateInfo)
import Vulkan.Core10.SparseResourceMemoryManagement (ImageSubresource)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_swapchain (ImageSwapchainCreateInfoKHR)
import Vulkan.Core10.Enums.ImageTiling (ImageTiling)
import Vulkan.Core10.Enums.ImageType (ImageType)
import Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlags)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlagBits)
import Vulkan.Core10.Enums.SharingMode (SharingMode)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_CREATE_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Handles (Image(..))
import Vulkan.Core10.Enums.ImageLayout (ImageLayout(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateImage
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct ImageCreateInfo) -> Ptr AllocationCallbacks -> Ptr Image -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct ImageCreateInfo) -> Ptr AllocationCallbacks -> Ptr Image -> IO Result

-- | vkCreateImage - Create a new image object
--
-- == Valid Usage
--
-- -   #VUID-vkCreateImage-flags-00939# If the @flags@ member of
--     @pCreateInfo@ includes
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_BINDING_BIT',
--     creating this 'Vulkan.Core10.Handles.Image' /must/ not cause the
--     total required sparse memory for all currently valid sparse
--     resources on the device to exceed
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@sparseAddressSpaceSize@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateImage-device-parameter# @device@ /must/ be a valid
--     'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateImage-pCreateInfo-parameter# @pCreateInfo@ /must/ be a
--     valid pointer to a valid 'ImageCreateInfo' structure
--
-- -   #VUID-vkCreateImage-pAllocator-parameter# If @pAllocator@ is not
--     @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkCreateImage-pImage-parameter# @pImage@ /must/ be a valid
--     pointer to a 'Vulkan.Core10.Handles.Image' handle
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
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Image',
-- 'ImageCreateInfo'
createImage :: forall a io
             . (Extendss ImageCreateInfo a, PokeChain a, MonadIO io)
            => -- | @device@ is the logical device that creates the image.
               Device
            -> -- | @pCreateInfo@ is a pointer to a 'ImageCreateInfo' structure containing
               -- parameters to be used to create the image.
               (ImageCreateInfo a)
            -> -- | @pAllocator@ controls host memory allocation as described in the
               -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
               -- chapter.
               ("allocator" ::: Maybe AllocationCallbacks)
            -> io (Image)
createImage device createInfo allocator = liftIO . evalContT $ do
  let vkCreateImagePtr = pVkCreateImage (deviceCmds (device :: Device))
  lift $ unless (vkCreateImagePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateImage is null" Nothing Nothing
  let vkCreateImage' = mkVkCreateImage vkCreateImagePtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPImage <- ContT $ bracket (callocBytes @Image 8) free
  r <- lift $ traceAroundEvent "vkCreateImage" (vkCreateImage' (deviceHandle (device)) (forgetExtensions pCreateInfo) pAllocator (pPImage))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pImage <- lift $ peek @Image pPImage
  pure $ (pImage)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createImage' and 'destroyImage'
--
-- To ensure that 'destroyImage' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withImage :: forall a io r . (Extendss ImageCreateInfo a, PokeChain a, MonadIO io) => Device -> ImageCreateInfo a -> Maybe AllocationCallbacks -> (io Image -> (Image -> io ()) -> r) -> r
withImage device pCreateInfo pAllocator b =
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
-- == Valid Usage
--
-- -   #VUID-vkDestroyImage-image-01000# All submitted commands that refer
--     to @image@, either directly or via a
--     'Vulkan.Core10.Handles.ImageView', /must/ have completed execution
--
-- -   #VUID-vkDestroyImage-image-01001# If
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @image@ was created, a compatible set of callbacks
--     /must/ be provided here
--
-- -   #VUID-vkDestroyImage-image-01002# If no
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @image@ was created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroyImage-device-parameter# @device@ /must/ be a valid
--     'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDestroyImage-image-parameter# If @image@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @image@ /must/ be a valid
--     'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-vkDestroyImage-pAllocator-parameter# If @pAllocator@ is not
--     @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkDestroyImage-image-parent# If @image@ is a valid handle, it
--     /must/ have been created, allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @image@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Image'
destroyImage :: forall io
              . (MonadIO io)
             => -- | @device@ is the logical device that destroys the image.
                Device
             -> -- | @image@ is the image to destroy.
                Image
             -> -- | @pAllocator@ controls host memory allocation as described in the
                -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                -- chapter.
                ("allocator" ::: Maybe AllocationCallbacks)
             -> io ()
destroyImage device image allocator = liftIO . evalContT $ do
  let vkDestroyImagePtr = pVkDestroyImage (deviceCmds (device :: Device))
  lift $ unless (vkDestroyImagePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyImage is null" Nothing Nothing
  let vkDestroyImage' = mkVkDestroyImage vkDestroyImagePtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyImage" (vkDestroyImage' (deviceHandle (device)) (image) pAllocator)
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
-- = Description
--
-- If the image is
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#glossary-linear-resource linear>,
-- then the returned layout is valid for
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-device-hostaccess host access>.
--
-- If the image’s tiling is
-- 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR' and its format is
-- a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
-- then 'getImageSubresourceLayout' describes one /format plane/ of the
-- image. If the image’s tiling is
-- 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT',
-- then 'getImageSubresourceLayout' describes one /memory plane/ of the
-- image. If the image’s tiling is
-- 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT'
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
-- -   #VUID-vkGetImageSubresourceLayout-image-02270# @image@ /must/ have
--     been created with @tiling@ equal to
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR' or
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT'
--
-- -   #VUID-vkGetImageSubresourceLayout-aspectMask-00997# The @aspectMask@
--     member of @pSubresource@ /must/ only have a single bit set
--
-- -   #VUID-vkGetImageSubresourceLayout-mipLevel-01716# The @mipLevel@
--     member of @pSubresource@ /must/ be less than the @mipLevels@
--     specified in 'ImageCreateInfo' when @image@ was created
--
-- -   #VUID-vkGetImageSubresourceLayout-arrayLayer-01717# The @arrayLayer@
--     member of @pSubresource@ /must/ be less than the @arrayLayers@
--     specified in 'ImageCreateInfo' when @image@ was created
--
-- -   #VUID-vkGetImageSubresourceLayout-format-04461# If @format@ is a
--     color format, the @aspectMask@ member of @pSubresource@ /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
--
-- -   #VUID-vkGetImageSubresourceLayout-format-04462# If @format@ has a
--     depth component, the @aspectMask@ member of @pSubresource@ /must/
--     contain
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT'
--
-- -   #VUID-vkGetImageSubresourceLayout-format-04463# If @format@ has a
--     stencil component, the @aspectMask@ member of @pSubresource@ /must/
--     contain
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- -   #VUID-vkGetImageSubresourceLayout-format-04464# If @format@ does not
--     contain a stencil or depth component, the @aspectMask@ member of
--     @pSubresource@ /must/ not contain
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT' or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- -   #VUID-vkGetImageSubresourceLayout-format-01581# If the @tiling@ of
--     the @image@ is 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR'
--     and its @format@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>
--     with two planes, the @aspectMask@ member of @pSubresource@ /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT'
--     or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT'
--
-- -   #VUID-vkGetImageSubresourceLayout-format-01582# If the @tiling@ of
--     the @image@ is 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR'
--     and its @format@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>
--     with three planes, the @aspectMask@ member of @pSubresource@ /must/
--     be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT'
--     or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'
--
-- -   #VUID-vkGetImageSubresourceLayout-image-01895# If @image@ was
--     created with the
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID'
--     external memory handle type, then @image@ /must/ be bound to memory
--
-- -   #VUID-vkGetImageSubresourceLayout-tiling-02271# If the @tiling@ of
--     the @image@ is
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT',
--     then the @aspectMask@ member of @pSubresource@ /must/ be
--     @VK_IMAGE_ASPECT_MEMORY_PLANE_i_BIT_EXT@ and the index @i@ /must/ be
--     less than the
--     'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.DrmFormatModifierPropertiesEXT'::@drmFormatModifierPlaneCount@
--     associated with the image’s @format@ and
--     'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.ImageDrmFormatModifierPropertiesEXT'::@drmFormatModifier@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetImageSubresourceLayout-device-parameter# @device@ /must/
--     be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetImageSubresourceLayout-image-parameter# @image@ /must/ be
--     a valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-vkGetImageSubresourceLayout-pSubresource-parameter#
--     @pSubresource@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.SparseResourceMemoryManagement.ImageSubresource'
--     structure
--
-- -   #VUID-vkGetImageSubresourceLayout-pLayout-parameter# @pLayout@
--     /must/ be a valid pointer to a 'SubresourceLayout' structure
--
-- -   #VUID-vkGetImageSubresourceLayout-image-parent# @image@ /must/ have
--     been created, allocated, or retrieved from @device@
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Image',
-- 'Vulkan.Core10.SparseResourceMemoryManagement.ImageSubresource',
-- 'SubresourceLayout'
getImageSubresourceLayout :: forall io
                           . (MonadIO io)
                          => -- | @device@ is the logical device that owns the image.
                             Device
                          -> -- | @image@ is the image whose layout is being queried.
                             Image
                          -> -- | @pSubresource@ is a pointer to a
                             -- 'Vulkan.Core10.SparseResourceMemoryManagement.ImageSubresource'
                             -- structure selecting a specific image for the image subresource.
                             ImageSubresource
                          -> io (SubresourceLayout)
getImageSubresourceLayout device image subresource = liftIO . evalContT $ do
  let vkGetImageSubresourceLayoutPtr = pVkGetImageSubresourceLayout (deviceCmds (device :: Device))
  lift $ unless (vkGetImageSubresourceLayoutPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetImageSubresourceLayout is null" Nothing Nothing
  let vkGetImageSubresourceLayout' = mkVkGetImageSubresourceLayout vkGetImageSubresourceLayoutPtr
  pSubresource <- ContT $ withCStruct (subresource)
  pPLayout <- ContT (withZeroCStruct @SubresourceLayout)
  lift $ traceAroundEvent "vkGetImageSubresourceLayout" (vkGetImageSubresourceLayout' (deviceHandle (device)) (image) pSubresource (pPLayout))
  pLayout <- lift $ peekCStruct @SubresourceLayout pPLayout
  pure $ (pLayout)


-- | VkImageCreateInfo - Structure specifying the parameters of a newly
-- created image object
--
-- = Description
--
-- Images created with @tiling@ equal to
-- 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR' have further
-- restrictions on their limits and capabilities compared to images created
-- with @tiling@ equal to
-- 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL'. Creation of
-- images with tiling 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR'
-- /may/ not be supported unless other parameters meet all of the
-- constraints:
--
-- -   @imageType@ is 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'
--
-- -   @format@ is not a depth\/stencil format
--
-- -   @mipLevels@ is 1
--
-- -   @arrayLayers@ is 1
--
-- -   @samples@ is
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   @usage@ only includes
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_SRC_BIT'
--     and\/or
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
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
-- -   @imageType@ is 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'
--
-- -   @mipLevels@ is 1
--
-- -   @arrayLayers@ is 1
--
-- -   @samples@ is
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- Implementations /may/ support additional limits and capabilities beyond
-- those listed above.
--
-- To determine the set of valid @usage@ bits for a given format, call
-- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceFormatProperties'.
--
-- If the size of the resultant image would exceed @maxResourceSize@, then
-- 'createImage' /must/ fail and return
-- 'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'. This failure
-- /may/ occur even when all image creation parameters satisfy their valid
-- usage requirements.
--
-- Note
--
-- For images created without
-- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_EXTENDED_USAGE_BIT'
-- a @usage@ bit is valid if it is supported for the format the image is
-- created with.
--
-- For images created with
-- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_EXTENDED_USAGE_BIT'
-- a @usage@ bit is valid if it is supported for at least one of the
-- formats a 'Vulkan.Core10.Handles.ImageView' created from the image /can/
-- have (see
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
--         'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT',
--         then @imageCreateDrmFormatModifiers@ is empty.
--
--     -   If 'ImageCreateInfo'::@pNext@ contains
--         'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.ImageDrmFormatModifierExplicitCreateInfoEXT',
--         then @imageCreateDrmFormatModifiers@ contains exactly one
--         modifier,
--         'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.ImageDrmFormatModifierExplicitCreateInfoEXT'::@drmFormatModifier@.
--
--     -   If 'ImageCreateInfo'::@pNext@ contains
--         'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.ImageDrmFormatModifierListCreateInfoEXT',
--         then @imageCreateDrmFormatModifiers@ contains the entire array
--         'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.ImageDrmFormatModifierListCreateInfoEXT'::@pDrmFormatModifiers@.
--
-- -   Let @VkBool32 imageCreateMaybeLinear@ indicate if the resultant
--     image may be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#glossary-linear-resource linear>.
--
--     -   If @tiling@ is
--         'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR', then
--         @imageCreateMaybeLinear@ is @true@.
--
--     -   If @tiling@ is
--         'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL', then
--         @imageCreateMaybeLinear@ is @false@.
--
--     -   If @tiling@ is
--         'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT',
--         then @imageCreateMaybeLinear_@ is @true@ if and only if
--         @imageCreateDrmFormatModifiers@ contains
--         @DRM_FORMAT_MOD_LINEAR@.
--
-- -   Let @VkFormatFeatureFlags imageCreateFormatFeatures@ be the set of
--     valid /format features/ available during image creation.
--
--     -   If @tiling@ is
--         'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR', then
--         @imageCreateFormatFeatures@ is the value of
--         'Vulkan.Core10.DeviceInitialization.FormatProperties'::@linearTilingFeatures@
--         found by calling
--         'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceFormatProperties'
--         with parameter @format@ equal to 'ImageCreateInfo'::@format@.
--
--     -   If @tiling@ is
--         'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL', and if
--         the @pNext@ chain includes no
--         'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'
--         structure with non-zero @externalFormat@, then
--         @imageCreateFormatFeatures@ is value of
--         'Vulkan.Core10.DeviceInitialization.FormatProperties'::@optimalTilingFeatures@
--         found by calling
--         'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceFormatProperties'
--         with parameter @format@ equal to 'ImageCreateInfo'::@format@.
--
--     -   If @tiling@ is
--         'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL', and if
--         the @pNext@ chain includes a
--         'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'
--         structure with non-zero @externalFormat@, then
--         @imageCreateFormatFeatures@ is the value of
--         'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.AndroidHardwareBufferFormatPropertiesANDROID'::@formatFeatures@
--         obtained by
--         'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.getAndroidHardwareBufferPropertiesANDROID'
--         with a matching @externalFormat@ value.
--
--     -   If @tiling@ is
--         'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT',
--         then the value of @imageCreateFormatFeatures@ is found by
--         calling
--         'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFormatProperties2'
--         with
--         'Vulkan.Core10.DeviceInitialization.ImageFormatProperties'::@format@
--         equal to 'ImageCreateInfo'::@format@ and with
--         'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.DrmFormatModifierPropertiesListEXT'
--         chained into
--         'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.ImageFormatProperties2';
--         by collecting all members of the returned array
--         'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.DrmFormatModifierPropertiesListEXT'::@pDrmFormatModifierProperties@
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
--         'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'
--         structure with non-zero @externalFormat@, then
--         @imageCreateImageFormatPropertiesList@ is the list of structures
--         obtained by calling
--         'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2',
--         possibly multiple times, as follows:
--
--         -   The parameters
--             'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2'::@format@,
--             @imageType@, @tiling@, @usage@, and @flags@ /must/ be equal
--             to those in 'ImageCreateInfo'.
--
--         -   If 'ImageCreateInfo'::@pNext@ contains a
--             'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo'
--             structure whose @handleTypes@ is not @0@, then
--             'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2'::@pNext@
--             /must/ contain a
--             'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.PhysicalDeviceExternalImageFormatInfo'
--             structure whose @handleType@ is not @0@; and
--             'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--             /must/ be called for each handle type in
--             'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo'::@handleTypes@,
--             successively setting
--             'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.PhysicalDeviceExternalImageFormatInfo'::@handleType@
--             on each call.
--
--         -   If 'ImageCreateInfo'::@pNext@ contains no
--             'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo'
--             structure, or contains a structure whose @handleTypes@ is
--             @0@, then
--             'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2'::@pNext@
--             /must/ either contain no
--             'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.PhysicalDeviceExternalImageFormatInfo'
--             structure, or contain a structure whose @handleType@ is @0@.
--
--         -   If @tiling@ is
--             'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT',
--             then
--             'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2'::@pNext@
--             /must/ contain a
--             'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.PhysicalDeviceImageDrmFormatModifierInfoEXT'
--             structure where @sharingMode@ is equal to
--             'ImageCreateInfo'::@sharingMode@; and, if @sharingMode@ is
--             'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT',
--             then @queueFamilyIndexCount@ and @pQueueFamilyIndices@
--             /must/ be equal to those in 'ImageCreateInfo'; and, if
--             @flags@ contains
--             'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_MUTABLE_FORMAT_BIT',
--             then the
--             'Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo'
--             structure included in the @pNext@ chain of
--             'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2'
--             /must/ be equivalent to the one included in the @pNext@
--             chain of 'ImageCreateInfo'; and
--             'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--             /must/ be called for each modifier in
--             @imageCreateDrmFormatModifiers@, successively setting
--             'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.PhysicalDeviceImageDrmFormatModifierInfoEXT'::@drmFormatModifier@
--             on each call.
--
--         -   If @tiling@ is not
--             'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT',
--             then
--             'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2'::@pNext@
--             /must/ contain no
--             'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.PhysicalDeviceImageDrmFormatModifierInfoEXT'
--             structure.
--
--         -   If any call to
--             'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--             returns an error, then
--             @imageCreateImageFormatPropertiesList@ is defined to be the
--             empty list.
--
--     -   If 'ImageCreateInfo'::@pNext@ contains a
--         'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'
--         structure with non-zero @externalFormat@, then
--         @imageCreateImageFormatPropertiesList@ contains a single element
--         where:
--
--         -   'Vulkan.Core10.DeviceInitialization.ImageFormatProperties'::@maxMipLevels@
--             is ⌊log2(max(@extent.width@, @extent.height@,
--             @extent.depth@))⌋ + 1.
--
--         -   'Vulkan.Core10.DeviceInitialization.ImageFormatProperties'::@maxArrayLayers@
--             is
--             'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::maxImageArrayLayers.
--
--         -   Each component of
--             'Vulkan.Core10.DeviceInitialization.ImageFormatProperties'::@maxExtent@
--             is
--             'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::maxImageDimension2D.
--
--         -   'Vulkan.Core10.DeviceInitialization.ImageFormatProperties'::@sampleCounts@
--             contains exactly
--             'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'.
--
-- -   Let @uint32_t imageCreateMaxMipLevels@ be the minimum value of
--     'Vulkan.Core10.DeviceInitialization.ImageFormatProperties'::@maxMipLevels@
--     in @imageCreateImageFormatPropertiesList@. The value is undefined if
--     @imageCreateImageFormatPropertiesList@ is empty.
--
-- -   Let @uint32_t imageCreateMaxArrayLayers@ be the minimum value of
--     'Vulkan.Core10.DeviceInitialization.ImageFormatProperties'::@maxArrayLayers@
--     in @imageCreateImageFormatPropertiesList@. The value is undefined if
--     @imageCreateImageFormatPropertiesList@ is empty.
--
-- -   Let @VkExtent3D imageCreateMaxExtent@ be the component-wise minimum
--     over all
--     'Vulkan.Core10.DeviceInitialization.ImageFormatProperties'::@maxExtent@
--     values in @imageCreateImageFormatPropertiesList@. The value is
--     undefined if @imageCreateImageFormatPropertiesList@ is empty.
--
-- -   Let @VkSampleCountFlags imageCreateSampleCounts@ be the intersection
--     of each
--     'Vulkan.Core10.DeviceInitialization.ImageFormatProperties'::@sampleCounts@
--     in @imageCreateImageFormatPropertiesList@. The value is undefined if
--     @imageCreateImageFormatPropertiesList@ is empty.
--
-- = Valid Usage
--
-- -   #VUID-VkImageCreateInfo-imageCreateMaxMipLevels-02251# Each of the
--     following values (as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>)
--     /must/ not be undefined @imageCreateMaxMipLevels@,
--     @imageCreateMaxArrayLayers@, @imageCreateMaxExtent@, and
--     @imageCreateSampleCounts@
--
-- -   #VUID-VkImageCreateInfo-sharingMode-00941# If @sharingMode@ is
--     'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT',
--     @pQueueFamilyIndices@ /must/ be a valid pointer to an array of
--     @queueFamilyIndexCount@ @uint32_t@ values
--
-- -   #VUID-VkImageCreateInfo-sharingMode-00942# If @sharingMode@ is
--     'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT',
--     @queueFamilyIndexCount@ /must/ be greater than @1@
--
-- -   #VUID-VkImageCreateInfo-sharingMode-01420# If @sharingMode@ is
--     'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT', each
--     element of @pQueueFamilyIndices@ /must/ be unique and /must/ be less
--     than @pQueueFamilyPropertyCount@ returned by either
--     'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceQueueFamilyProperties'
--     or
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceQueueFamilyProperties2'
--     for the @physicalDevice@ that was used to create @device@
--
-- -   #VUID-VkImageCreateInfo-pNext-01974# If the @pNext@ chain includes a
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'
--     structure, and its @externalFormat@ member is non-zero the @format@
--     /must/ be 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   #VUID-VkImageCreateInfo-pNext-01975# If the @pNext@ chain does not
--     include a
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'
--     structure, or does and its @externalFormat@ member is @0@, the
--     @format@ /must/ not be 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   #VUID-VkImageCreateInfo-extent-00944# @extent.width@ /must/ be
--     greater than @0@
--
-- -   #VUID-VkImageCreateInfo-extent-00945# @extent.height@ /must/ be
--     greater than @0@
--
-- -   #VUID-VkImageCreateInfo-extent-00946# @extent.depth@ /must/ be
--     greater than @0@
--
-- -   #VUID-VkImageCreateInfo-mipLevels-00947# @mipLevels@ /must/ be
--     greater than @0@
--
-- -   #VUID-VkImageCreateInfo-arrayLayers-00948# @arrayLayers@ /must/ be
--     greater than @0@
--
-- -   #VUID-VkImageCreateInfo-flags-00949# If @flags@ contains
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CUBE_COMPATIBLE_BIT',
--     @imageType@ /must/ be 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'
--
-- -   #VUID-VkImageCreateInfo-flags-02557# If @flags@ contains
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT',
--     @imageType@ /must/ be 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'
--
-- -   #VUID-VkImageCreateInfo-flags-00950# If @flags@ contains
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT',
--     @imageType@ /must/ be 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D'
--
-- -   #VUID-VkImageCreateInfo-extent-02252# @extent.width@ /must/ be less
--     than or equal to @imageCreateMaxExtent.width@ (as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>)
--
-- -   #VUID-VkImageCreateInfo-extent-02253# @extent.height@ /must/ be less
--     than or equal to @imageCreateMaxExtent.height@ (as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>)
--
-- -   #VUID-VkImageCreateInfo-extent-02254# @extent.depth@ /must/ be less
--     than or equal to @imageCreateMaxExtent.depth@ (as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>)
--
-- -   #VUID-VkImageCreateInfo-imageType-00954# If @imageType@ is
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D' and @flags@ contains
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CUBE_COMPATIBLE_BIT',
--     @extent.width@ and @extent.height@ /must/ be equal and @arrayLayers@
--     /must/ be greater than or equal to 6
--
-- -   #VUID-VkImageCreateInfo-imageType-00956# If @imageType@ is
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', both @extent.height@
--     and @extent.depth@ /must/ be @1@
--
-- -   #VUID-VkImageCreateInfo-imageType-00957# If @imageType@ is
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', @extent.depth@ /must/
--     be @1@
--
-- -   #VUID-VkImageCreateInfo-mipLevels-00958# @mipLevels@ /must/ be less
--     than or equal to the number of levels in the complete mipmap chain
--     based on @extent.width@, @extent.height@, and @extent.depth@
--
-- -   #VUID-VkImageCreateInfo-mipLevels-02255# @mipLevels@ /must/ be less
--     than or equal to @imageCreateMaxMipLevels@ (as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>)
--
-- -   #VUID-VkImageCreateInfo-arrayLayers-02256# @arrayLayers@ /must/ be
--     less than or equal to @imageCreateMaxArrayLayers@ (as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>)
--
-- -   #VUID-VkImageCreateInfo-imageType-00961# If @imageType@ is
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', @arrayLayers@ /must/
--     be @1@
--
-- -   #VUID-VkImageCreateInfo-samples-02257# If @samples@ is not
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT', then
--     @imageType@ /must/ be 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D',
--     @flags@ /must/ not contain
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CUBE_COMPATIBLE_BIT',
--     @mipLevels@ /must/ be equal to @1@, and @imageCreateMaybeLinear@ (as
--     defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>)
--     /must/ be @false@,
--
-- -   #VUID-VkImageCreateInfo-samples-02558# If @samples@ is not
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT',
--     @usage@ /must/ not contain
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT'
--
-- -   #VUID-VkImageCreateInfo-usage-00963# If @usage@ includes
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT',
--     then bits other than
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT',
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT',
--     and
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--     /must/ not be set
--
-- -   #VUID-VkImageCreateInfo-usage-00964# If @usage@ includes
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT',
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT',
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT',
--     or
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT',
--     @extent.width@ /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxFramebufferWidth@
--
-- -   #VUID-VkImageCreateInfo-usage-00965# If @usage@ includes
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT',
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT',
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT',
--     or
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT',
--     @extent.height@ /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxFramebufferHeight@
--
-- -   #VUID-VkImageCreateInfo-usage-02559# If @usage@ includes
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT',
--     @extent.width@ /must/ be less than or equal to
--     \(\left\lceil{\frac{maxFramebufferWidth}{minFragmentDensityTexelSize_{width}}}\right\rceil\)
--
-- -   #VUID-VkImageCreateInfo-usage-02560# If @usage@ includes
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT',
--     @extent.height@ /must/ be less than or equal to
--     \(\left\lceil{\frac{maxFramebufferHeight}{minFragmentDensityTexelSize_{height}}}\right\rceil\)
--
-- -   #VUID-VkImageCreateInfo-usage-00966# If @usage@ includes
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT',
--     @usage@ /must/ also contain at least one of
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT',
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT',
--     or
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageCreateInfo-samples-02258# @samples@ /must/ be a bit
--     value that is set in @imageCreateSampleCounts@ (as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>)
--
-- -   #VUID-VkImageCreateInfo-usage-00968# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderStorageImageMultisample multisampled storage images>
--     feature is not enabled, and @usage@ contains
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_STORAGE_BIT',
--     @samples@ /must/ be
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-VkImageCreateInfo-flags-00969# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseBinding sparse bindings>
--     feature is not enabled, @flags@ /must/ not contain
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_BINDING_BIT'
--
-- -   #VUID-VkImageCreateInfo-flags-01924# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseResidencyAliased sparse aliased residency>
--     feature is not enabled, @flags@ /must/ not contain
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_ALIASED_BIT'
--
-- -   #VUID-VkImageCreateInfo-tiling-04121# If @tiling@ is
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR', @flags@
--     /must/ not contain
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--
-- -   #VUID-VkImageCreateInfo-imageType-00970# If @imageType@ is
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', @flags@ /must/ not
--     contain
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--
-- -   #VUID-VkImageCreateInfo-imageType-00971# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseResidencyImage2D sparse residency for 2D images>
--     feature is not enabled, and @imageType@ is
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', @flags@ /must/ not
--     contain
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--
-- -   #VUID-VkImageCreateInfo-imageType-00972# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseResidencyImage3D sparse residency for 3D images>
--     feature is not enabled, and @imageType@ is
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', @flags@ /must/ not
--     contain
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--
-- -   #VUID-VkImageCreateInfo-imageType-00973# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseResidency2Samples sparse residency for images with 2 samples>
--     feature is not enabled, @imageType@ is
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', and @samples@ is
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_2_BIT',
--     @flags@ /must/ not contain
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--
-- -   #VUID-VkImageCreateInfo-imageType-00974# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseResidency4Samples sparse residency for images with 4 samples>
--     feature is not enabled, @imageType@ is
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', and @samples@ is
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_4_BIT',
--     @flags@ /must/ not contain
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--
-- -   #VUID-VkImageCreateInfo-imageType-00975# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseResidency8Samples sparse residency for images with 8 samples>
--     feature is not enabled, @imageType@ is
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', and @samples@ is
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_8_BIT',
--     @flags@ /must/ not contain
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--
-- -   #VUID-VkImageCreateInfo-imageType-00976# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseResidency16Samples sparse residency for images with 16 samples>
--     feature is not enabled, @imageType@ is
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', and @samples@ is
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_16_BIT',
--     @flags@ /must/ not contain
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--
-- -   #VUID-VkImageCreateInfo-flags-00987# If @flags@ contains
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--     or
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_ALIASED_BIT',
--     it /must/ also contain
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_BINDING_BIT'
--
-- -   #VUID-VkImageCreateInfo-None-01925# If any of the bits
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_BINDING_BIT',
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT',
--     or
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_ALIASED_BIT'
--     are set,
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT'
--     /must/ not also be set
--
-- -   #VUID-VkImageCreateInfo-flags-01890# If the protected memory feature
--     is not enabled, @flags@ /must/ not contain
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_PROTECTED_BIT'
--
-- -   #VUID-VkImageCreateInfo-None-01891# If any of the bits
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_BINDING_BIT',
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT',
--     or
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_ALIASED_BIT'
--     are set,
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_PROTECTED_BIT'
--     /must/ not also be set
--
-- -   #VUID-VkImageCreateInfo-pNext-00988# If the @pNext@ chain includes a
--     'Vulkan.Extensions.VK_NV_external_memory.ExternalMemoryImageCreateInfoNV'
--     structure, it /must/ not contain a
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo'
--     structure
--
-- -   #VUID-VkImageCreateInfo-pNext-00990# If the @pNext@ chain includes a
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo'
--     structure, its @handleTypes@ member /must/ only contain bits that
--     are also in
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.ExternalImageFormatProperties'::@externalMemoryProperties.compatibleHandleTypes@,
--     as returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--     with @format@, @imageType@, @tiling@, @usage@, and @flags@ equal to
--     those in this structure, and with a
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.PhysicalDeviceExternalImageFormatInfo'
--     structure included in the @pNext@ chain, with a @handleType@ equal
--     to any one of the handle types specified in
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo'::@handleTypes@
--
-- -   #VUID-VkImageCreateInfo-pNext-00991# If the @pNext@ chain includes a
--     'Vulkan.Extensions.VK_NV_external_memory.ExternalMemoryImageCreateInfoNV'
--     structure, its @handleTypes@ member /must/ only contain bits that
--     are also in
--     'Vulkan.Extensions.VK_NV_external_memory_capabilities.ExternalImageFormatPropertiesNV'::@externalMemoryProperties.compatibleHandleTypes@,
--     as returned by
--     'Vulkan.Extensions.VK_NV_external_memory_capabilities.getPhysicalDeviceExternalImageFormatPropertiesNV'
--     with @format@, @imageType@, @tiling@, @usage@, and @flags@ equal to
--     those in this structure, and with @externalHandleType@ equal to any
--     one of the handle types specified in
--     'Vulkan.Extensions.VK_NV_external_memory.ExternalMemoryImageCreateInfoNV'::@handleTypes@
--
-- -   #VUID-VkImageCreateInfo-physicalDeviceCount-01421# If the logical
--     device was created with
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation.DeviceGroupDeviceCreateInfo'::@physicalDeviceCount@
--     equal to 1, @flags@ /must/ not contain
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT'
--
-- -   #VUID-VkImageCreateInfo-flags-02259# If @flags@ contains
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT',
--     then @mipLevels@ /must/ be one, @arrayLayers@ /must/ be one,
--     @imageType@ /must/ be 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'.
--     and @imageCreateMaybeLinear@ (as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>)
--     /must/ be @false@
--
-- -   #VUID-VkImageCreateInfo-flags-01572# If @flags@ contains
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT',
--     then @format@ /must/ be a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#appendix-compressedtex-bc block-compressed image format>,
--     an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#appendix-compressedtex-etc2 ETC compressed image format>,
--     or an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#appendix-compressedtex-astc ASTC compressed image format>
--
-- -   #VUID-VkImageCreateInfo-flags-01573# If @flags@ contains
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT',
--     then @flags@ /must/ also contain
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_MUTABLE_FORMAT_BIT'
--
-- -   #VUID-VkImageCreateInfo-initialLayout-00993# @initialLayout@ /must/
--     be 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED' or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_PREINITIALIZED'
--
-- -   #VUID-VkImageCreateInfo-pNext-01443# If the @pNext@ chain includes a
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo'
--     or
--     'Vulkan.Extensions.VK_NV_external_memory.ExternalMemoryImageCreateInfoNV'
--     structure whose @handleTypes@ member is not @0@, @initialLayout@
--     /must/ be 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED'
--
-- -   #VUID-VkImageCreateInfo-format-02561# If the image @format@ is one
--     of those listed in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion>,
--     then @mipLevels@ /must/ be 1
--
-- -   #VUID-VkImageCreateInfo-format-02562# If the image @format@ is one
--     of those listed in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion>,
--     @samples@ /must/ be
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-VkImageCreateInfo-format-02563# If the image @format@ is one
--     of those listed in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion>,
--     @imageType@ /must/ be 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'
--
-- -   #VUID-VkImageCreateInfo-format-02653# If the image @format@ is one
--     of those listed in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion>,
--     and the @ycbcrImageArrays@ feature is not enabled, @arrayLayers@
--     /must/ be 1
--
-- -   #VUID-VkImageCreateInfo-imageCreateFormatFeatures-02260# If @format@
--     is a /multi-planar/ format, and if @imageCreateFormatFeatures@ (as
--     defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>)
--     does not contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_DISJOINT_BIT',
--     then @flags@ /must/ not contain
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_DISJOINT_BIT'
--
-- -   #VUID-VkImageCreateInfo-format-01577# If @format@ is not a
--     /multi-planar/ format, and @flags@ does not include
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_ALIAS_BIT',
--     @flags@ /must/ not contain
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_DISJOINT_BIT'
--
-- -   #VUID-VkImageCreateInfo-format-04712# If @format@ has a @_422@ or
--     @_420@ suffix, @width@ /must/ be a multiple of 2
--
-- -   #VUID-VkImageCreateInfo-format-04713# If @format@ has a @_420@
--     suffix, @height@ /must/ be a multiple of 2
--
-- -   #VUID-VkImageCreateInfo-tiling-02261# If @tiling@ is
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT',
--     then the @pNext@ chain /must/ include exactly one of
--     'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.ImageDrmFormatModifierListCreateInfoEXT'
--     or
--     'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.ImageDrmFormatModifierExplicitCreateInfoEXT'
--     structures
--
-- -   #VUID-VkImageCreateInfo-pNext-02262# If the @pNext@ chain includes a
--     'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.ImageDrmFormatModifierListCreateInfoEXT'
--     or
--     'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.ImageDrmFormatModifierExplicitCreateInfoEXT'
--     structure, then @tiling@ /must/ be
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT'
--
-- -   #VUID-VkImageCreateInfo-tiling-02353# If @tiling@ is
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT'
--     and @flags@ contains
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_MUTABLE_FORMAT_BIT',
--     then the @pNext@ chain /must/ include a
--     'Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo'
--     structure with non-zero @viewFormatCount@
--
-- -   #VUID-VkImageCreateInfo-flags-01533# If @flags@ contains
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT'
--     @format@ /must/ be a depth or depth\/stencil format
--
-- -   #VUID-VkImageCreateInfo-pNext-02393# If the @pNext@ chain includes a
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo'
--     structure whose @handleTypes@ member includes
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID',
--     @imageType@ /must/ be 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'
--
-- -   #VUID-VkImageCreateInfo-pNext-02394# If the @pNext@ chain includes a
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo'
--     structure whose @handleTypes@ member includes
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID',
--     @mipLevels@ /must/ either be @1@ or equal to the number of levels in
--     the complete mipmap chain based on @extent.width@, @extent.height@,
--     and @extent.depth@
--
-- -   #VUID-VkImageCreateInfo-pNext-02396# If the @pNext@ chain includes a
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'
--     structure whose @externalFormat@ member is not @0@, @flags@ /must/
--     not include
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_MUTABLE_FORMAT_BIT'
--
-- -   #VUID-VkImageCreateInfo-pNext-02397# If the @pNext@ chain includes a
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'
--     structure whose @externalFormat@ member is not @0@, @usage@ /must/
--     not include any usages except
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT'
--
-- -   #VUID-VkImageCreateInfo-pNext-02398# If the @pNext@ chain includes a
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'
--     structure whose @externalFormat@ member is not @0@, @tiling@ /must/
--     be 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL'
--
-- -   #VUID-VkImageCreateInfo-format-02795# If @format@ is a depth-stencil
--     format, @usage@ includes
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT',
--     and the @pNext@ chain includes a
--     'Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'
--     structure, then its
--     'Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'::@stencilUsage@
--     member /must/ also include
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageCreateInfo-format-02796# If @format@ is a depth-stencil
--     format, @usage@ does not include
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT',
--     and the @pNext@ chain includes a
--     'Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'
--     structure, then its
--     'Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'::@stencilUsage@
--     member /must/ also not include
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageCreateInfo-format-02797# If @format@ is a depth-stencil
--     format, @usage@ includes
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT',
--     and the @pNext@ chain includes a
--     'Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'
--     structure, then its
--     'Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'::@stencilUsage@
--     member /must/ also include
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageCreateInfo-format-02798# If @format@ is a depth-stencil
--     format, @usage@ does not include
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT',
--     and the @pNext@ chain includes a
--     'Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'
--     structure, then its
--     'Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'::@stencilUsage@
--     member /must/ also not include
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageCreateInfo-Format-02536# If
--     'Vulkan.Core10.Enums.Format.Format' is a depth-stencil format and
--     the @pNext@ chain includes a
--     'Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'
--     structure with its @stencilUsage@ member including
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT',
--     @extent.width@ /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxFramebufferWidth@
--
-- -   #VUID-VkImageCreateInfo-format-02537# If @format@ is a depth-stencil
--     format and the @pNext@ chain includes a
--     'Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'
--     structure with its @stencilUsage@ member including
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT',
--     @extent.height@ /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxFramebufferHeight@
--
-- -   #VUID-VkImageCreateInfo-format-02538# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderStorageImageMultisample multisampled storage images>
--     feature is not enabled, @format@ is a depth-stencil format and the
--     @pNext@ chain includes a
--     'Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'
--     structure with its @stencilUsage@ including
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_STORAGE_BIT',
--     @samples@ /must/ be
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-VkImageCreateInfo-flags-02050# If @flags@ contains
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV',
--     @imageType@ /must/ be 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'
--     or 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D'
--
-- -   #VUID-VkImageCreateInfo-flags-02051# If @flags@ contains
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV',
--     it /must/ not contain
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CUBE_COMPATIBLE_BIT'
--     and the @format@ /must/ not be a depth\/stencil format
--
-- -   #VUID-VkImageCreateInfo-flags-02052# If @flags@ contains
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     and @imageType@ is 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D',
--     @extent.width@ and @extent.height@ /must/ be greater than @1@
--
-- -   #VUID-VkImageCreateInfo-flags-02053# If @flags@ contains
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     and @imageType@ is 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D',
--     @extent.width@, @extent.height@, and @extent.depth@ /must/ be
--     greater than @1@
--
-- -   #VUID-VkImageCreateInfo-imageType-02082# If @usage@ includes
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR',
--     @imageType@ /must/ be 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'
--
-- -   #VUID-VkImageCreateInfo-samples-02083# If @usage@ includes
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR',
--     @samples@ /must/ be
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-VkImageCreateInfo-tiling-02084# If @usage@ includes
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV',
--     @tiling@ /must/ be
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL'
--
-- -   #VUID-VkImageCreateInfo-flags-02565# If @flags@ contains
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT',
--     @tiling@ /must/ be
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL'
--
-- -   #VUID-VkImageCreateInfo-flags-02566# If @flags@ contains
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT',
--     @imageType@ /must/ be 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'
--
-- -   #VUID-VkImageCreateInfo-flags-02567# If @flags@ contains
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT',
--     @flags@ /must/ not contain
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CUBE_COMPATIBLE_BIT'
--
-- -   #VUID-VkImageCreateInfo-flags-02568# If @flags@ contains
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT',
--     @mipLevels@ /must/ be @1@
--
-- -   #VUID-VkImageCreateInfo-imageView2DOn3DImage-04459# If the
--     @VK_KHR_portability_subset@ extension is enabled, and
--     'Vulkan.Extensions.VK_KHR_portability_subset.PhysicalDevicePortabilitySubsetFeaturesKHR'::@imageView2DOn3DImage@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', @flags@ /must/ not
--     contain
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT'.
--
-- -   #VUID-VkImageCreateInfo-multisampleArrayImage-04460# If the
--     @VK_KHR_portability_subset@ extension is enabled, and
--     'Vulkan.Extensions.VK_KHR_portability_subset.PhysicalDevicePortabilitySubsetFeaturesKHR'::@multisampleArrayImage@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', and @samples@ is not
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT', then
--     @arrayLayers@ /must/ be @1@.
--
-- = Valid Usage (Implicit)
--
-- -   #VUID-VkImageCreateInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_CREATE_INFO'
--
-- -   #VUID-VkImageCreateInfo-pNext-pNext# Each @pNext@ member of any
--     structure (including this one) in the @pNext@ chain /must/ be either
--     @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_NV_dedicated_allocation.DedicatedAllocationImageCreateInfoNV',
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID',
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo',
--     'Vulkan.Extensions.VK_NV_external_memory.ExternalMemoryImageCreateInfoNV',
--     'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.ImageDrmFormatModifierExplicitCreateInfoEXT',
--     'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.ImageDrmFormatModifierListCreateInfoEXT',
--     'Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo',
--     'Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo',
--     or 'Vulkan.Extensions.VK_KHR_swapchain.ImageSwapchainCreateInfoKHR'
--
-- -   #VUID-VkImageCreateInfo-sType-unique# The @sType@ value of each
--     struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkImageCreateInfo-flags-parameter# @flags@ /must/ be a valid
--     combination of
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlagBits' values
--
-- -   #VUID-VkImageCreateInfo-imageType-parameter# @imageType@ /must/ be a
--     valid 'Vulkan.Core10.Enums.ImageType.ImageType' value
--
-- -   #VUID-VkImageCreateInfo-format-parameter# @format@ /must/ be a valid
--     'Vulkan.Core10.Enums.Format.Format' value
--
-- -   #VUID-VkImageCreateInfo-samples-parameter# @samples@ /must/ be a
--     valid 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits'
--     value
--
-- -   #VUID-VkImageCreateInfo-tiling-parameter# @tiling@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageTiling.ImageTiling' value
--
-- -   #VUID-VkImageCreateInfo-usage-parameter# @usage@ /must/ be a valid
--     combination of
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits' values
--
-- -   #VUID-VkImageCreateInfo-usage-requiredbitmask# @usage@ /must/ not be
--     @0@
--
-- -   #VUID-VkImageCreateInfo-sharingMode-parameter# @sharingMode@ /must/
--     be a valid 'Vulkan.Core10.Enums.SharingMode.SharingMode' value
--
-- -   #VUID-VkImageCreateInfo-initialLayout-parameter# @initialLayout@
--     /must/ be a valid 'Vulkan.Core10.Enums.ImageLayout.ImageLayout'
--     value
--
-- \<\/section>
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Extent3D',
-- 'Vulkan.Core10.Enums.Format.Format',
-- 'Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlags',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Vulkan.Core10.Enums.ImageTiling.ImageTiling',
-- 'Vulkan.Core10.Enums.ImageType.ImageType',
-- 'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlags',
-- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits',
-- 'Vulkan.Core10.Enums.SharingMode.SharingMode',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'createImage'
data ImageCreateInfo (es :: [Type]) = ImageCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlagBits' describing
    -- additional parameters of the image.
    flags :: ImageCreateFlags
  , -- | @imageType@ is a 'Vulkan.Core10.Enums.ImageType.ImageType' value
    -- specifying the basic dimensionality of the image. Layers in array
    -- textures do not count as a dimension for the purposes of the image type.
    imageType :: ImageType
  , -- | @format@ is a 'Vulkan.Core10.Enums.Format.Format' describing the format
    -- and type of the texel blocks that will be contained in the image.
    format :: Format
  , -- | @extent@ is a 'Vulkan.Core10.FundamentalTypes.Extent3D' describing the
    -- number of data elements in each dimension of the base level.
    extent :: Extent3D
  , -- | @mipLevels@ describes the number of levels of detail available for
    -- minified sampling of the image.
    mipLevels :: Word32
  , -- | @arrayLayers@ is the number of layers in the image.
    arrayLayers :: Word32
  , -- | @samples@ is a
    -- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' specifying
    -- the number of
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-multisampling samples per texel>.
    samples :: SampleCountFlagBits
  , -- | @tiling@ is a 'Vulkan.Core10.Enums.ImageTiling.ImageTiling' value
    -- specifying the tiling arrangement of the texel blocks in memory.
    tiling :: ImageTiling
  , -- | @usage@ is a bitmask of
    -- 'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits' describing
    -- the intended usage of the image.
    usage :: ImageUsageFlags
  , -- | @sharingMode@ is a 'Vulkan.Core10.Enums.SharingMode.SharingMode' value
    -- specifying the sharing mode of the image when it will be accessed by
    -- multiple queue families.
    sharingMode :: SharingMode
  , -- | @pQueueFamilyIndices@ is a list of queue families that will access this
    -- image (ignored if @sharingMode@ is not
    -- 'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT').
    queueFamilyIndices :: Vector Word32
  , -- | @initialLayout@ is a 'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
    -- specifying the initial 'Vulkan.Core10.Enums.ImageLayout.ImageLayout' of
    -- all image subresources of the image. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-layouts Image Layouts>.
    initialLayout :: ImageLayout
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (ImageCreateInfo es)

instance Extensible ImageCreateInfo where
  extensibleTypeName = "ImageCreateInfo"
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

instance (Extendss ImageCreateInfo es, PokeChain es) => ToCStruct (ImageCreateInfo es) where
  withCStruct x f = allocaBytesAligned 88 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr ImageCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr ImageType)) (imageType)
    lift $ poke ((p `plusPtr` 24 :: Ptr Format)) (format)
    lift $ poke ((p `plusPtr` 28 :: Ptr Extent3D)) (extent)
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
    lift $ poke ((p `plusPtr` 28 :: Ptr Extent3D)) (zero)
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

instance (Extendss ImageCreateInfo es, PeekChain es) => FromCStruct (ImageCreateInfo es) where
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
-- 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR' , then the
-- @aspectMask@ member of
-- 'Vulkan.Core10.SparseResourceMemoryManagement.ImageSubresource' /must/
-- be 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'.
--
-- If the image has a depth\/stencil format and its tiling is
-- 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR' , then
-- @aspectMask@ /must/ be either
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT' or
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'. On
-- implementations that store depth and stencil aspects separately,
-- querying each of these image subresource layouts will return a different
-- @offset@ and @size@ representing the region of memory used for that
-- aspect. On implementations that store depth and stencil aspects
-- interleaved, the same @offset@ and @size@ are returned and represent the
-- interleaved memory allocation.
--
-- If the image has a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>
-- and its tiling is 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR'
-- , then the @aspectMask@ member of
-- 'Vulkan.Core10.SparseResourceMemoryManagement.ImageSubresource' /must/
-- be 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT', or
-- (for 3-plane formats only)
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'.
-- Querying each of these image subresource layouts will return a different
-- @offset@ and @size@ representing the region of memory used for that
-- plane. If the image is /disjoint/, then the @offset@ is relative to the
-- base address of the plane. If the image is /non-disjoint/, then the
-- @offset@ is relative to the base address of the image.
--
-- If the image’s tiling is
-- 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT',
-- then the @aspectMask@ member of
-- 'Vulkan.Core10.SparseResourceMemoryManagement.ImageSubresource' /must/
-- be one of @VK_IMAGE_ASPECT_MEMORY_PLANE_i_BIT_EXT@, where the maximum
-- allowed plane index @i@ is defined by the
-- 'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.DrmFormatModifierPropertiesEXT'::@drmFormatModifierPlaneCount@
-- associated with the image’s 'ImageCreateInfo'::@format@ and
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
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.ImageDrmFormatModifierExplicitCreateInfoEXT',
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
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SubresourceLayout)
#endif
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

