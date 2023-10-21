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
import Foreign.Marshal.Alloc (allocaBytes)
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
import {-# SOURCE #-} Vulkan.Extensions.VK_FUCHSIA_buffer_collection (BufferCollectionImageCreateInfoFUCHSIA)
import Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_dedicated_allocation (DedicatedAllocationImageCreateInfoNV)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkCreateImage))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyImage))
import Vulkan.Dynamic (DeviceCmds(pVkGetImageSubresourceLayout))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_metal_objects (ExportMetalObjectCreateInfoEXT)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.FundamentalTypes (Extent3D)
import {-# SOURCE #-} Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer (ExternalFormatANDROID)
import {-# SOURCE #-} Vulkan.Extensions.VK_QNX_external_memory_screen_buffer (ExternalFormatQNX)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_memory (ExternalMemoryImageCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_external_memory (ExternalMemoryImageCreateInfoNV)
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.Core10.Handles (Image)
import Vulkan.Core10.Handles (Image(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_compression_control (ImageCompressionControlEXT)
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
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_metal_objects (ImportMetalIOSurfaceInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_metal_objects (ImportMetalTextureInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_descriptor_buffer (OpaqueCaptureDescriptorDataCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_optical_flow (OpticalFlowImageFormatInfoNV)
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
--     and the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-extendedSparseAddressSpace extendedSparseAddressSpace>
--     feature is not enabled, creating this 'Vulkan.Core10.Handles.Image'
--     /must/ not cause the total required sparse memory for all currently
--     valid sparse resources on the device to exceed
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@sparseAddressSpaceSize@
--
-- -   #VUID-vkCreateImage-flags-09385# If the @flags@ member of
--     @pCreateInfo@ includes
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_BINDING_BIT',
--     the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-extendedSparseAddressSpace extendedSparseAddressSpace>
--     feature is enabled, and the @usage@ member of @pCreateInfo@ contains
--     bits not in
--     'Vulkan.Extensions.VK_NV_extended_sparse_address_space.PhysicalDeviceExtendedSparseAddressSpacePropertiesNV'::@extendedSparseImageUsageFlags@,
--     creating this 'Vulkan.Core10.Handles.Image' /must/ not cause the
--     total required sparse memory for all currently valid sparse
--     resources on the device, excluding 'Vulkan.Core10.Handles.Buffer'
--     created with @usage@ member of @pCreateInfo@ containing bits in
--     'Vulkan.Extensions.VK_NV_extended_sparse_address_space.PhysicalDeviceExtendedSparseAddressSpacePropertiesNV'::@extendedSparseBufferUsageFlags@
--     and 'Vulkan.Core10.Handles.Image' created with @usage@ member of
--     @pCreateInfo@ containing bits in
--     'Vulkan.Extensions.VK_NV_extended_sparse_address_space.PhysicalDeviceExtendedSparseAddressSpacePropertiesNV'::@extendedSparseImageUsageFlags@,
--     to exceed
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@sparseAddressSpaceSize@
--
-- -   #VUID-vkCreateImage-flags-09386# If the @flags@ member of
--     @pCreateInfo@ includes
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_BINDING_BIT'
--     and the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-extendedSparseAddressSpace extendedSparseAddressSpace>
--     feature is enabled, creating this 'Vulkan.Core10.Handles.Image'
--     /must/ not cause the total required sparse memory for all currently
--     valid sparse resources on the device to exceed
--     'Vulkan.Extensions.VK_NV_extended_sparse_address_space.PhysicalDeviceExtendedSparseAddressSpacePropertiesNV'::@extendedSparseAddressSpaceSize@
--
-- -   #VUID-vkCreateImage-pNext-06389# If a
--     'Vulkan.Extensions.VK_FUCHSIA_buffer_collection.BufferCollectionImageCreateInfoFUCHSIA'
--     has been chained to @pNext@, @pCreateInfo@ /must/ match the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#sysmem-chosen-create-infos Sysmem chosen >
--     excepting members 'ImageCreateInfo'::@extent@ and
--     'ImageCreateInfo'::@usage@ in the match criteria
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_COMPRESSION_EXHAUSTED_EXT'
--
--     -   'Vulkan.Extensions.VK_KHR_buffer_device_address.ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS_KHR'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
               -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
               -- chapter.
               ("allocator" ::: Maybe AllocationCallbacks)
            -> io (Image)
createImage device createInfo allocator = liftIO . evalContT $ do
  let vkCreateImagePtr = pVkCreateImage (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCreateImagePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateImage is null" Nothing Nothing
  let vkCreateImage' = mkVkCreateImage vkCreateImagePtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPImage <- ContT $ bracket (callocBytes @Image 8) free
  r <- lift $ traceAroundEvent "vkCreateImage" (vkCreateImage'
                                                  (deviceHandle (device))
                                                  (forgetExtensions pCreateInfo)
                                                  pAllocator
                                                  (pPImage))
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
-- -   #VUID-vkDestroyImage-image-04882# @image@ /must/ not have been
--     acquired from
--     'Vulkan.Extensions.VK_KHR_swapchain.getSwapchainImagesKHR'
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Image'
destroyImage :: forall io
              . (MonadIO io)
             => -- | @device@ is the logical device that destroys the image.
                Device
             -> -- | @image@ is the image to destroy.
                Image
             -> -- | @pAllocator@ controls host memory allocation as described in the
                -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                -- chapter.
                ("allocator" ::: Maybe AllocationCallbacks)
             -> io ()
destroyImage device image allocator = liftIO . evalContT $ do
  let vkDestroyImagePtr = pVkDestroyImage (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkDestroyImagePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyImage is null" Nothing Nothing
  let vkDestroyImage' = mkVkDestroyImage vkDestroyImagePtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyImage" (vkDestroyImage'
                                              (deviceHandle (device))
                                              (image)
                                              pAllocator)
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
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#glossary-linear-resource linear>,
-- then the returned layout is valid for
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-device-hostaccess host access>.
--
-- If the image’s tiling is
-- 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR' and its format is
-- a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
-- then 'getImageSubresourceLayout' describes one /format plane/ of the
-- image. If the image’s tiling is
-- 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT',
-- then 'getImageSubresourceLayout' describes one /memory plane/ of the
-- image. If the image’s tiling is
-- 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT'
-- and the image is
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#glossary-linear-resource non-linear>,
-- then the returned layout has an implementation-dependent meaning; the
-- vendor of the image’s
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#glossary-drm-format-modifier DRM format modifier>
-- /may/ provide documentation that explains how to interpret the returned
-- layout.
--
-- 'getImageSubresourceLayout' is invariant for the lifetime of a single
-- image. However, the subresource layout of images in Android hardware
-- buffer or QNX Screen buffer external memory is not known until the image
-- has been bound to memory, so applications /must/ not call
-- 'getImageSubresourceLayout' for such an image before it has been bound.
--
-- == Valid Usage
--
-- -   #VUID-vkGetImageSubresourceLayout-image-07790# @image@ /must/ have
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
-- -   #VUID-vkGetImageSubresourceLayout-format-08886# If @format@ of the
--     @image@ is a color format, @tiling@ of the @image@ is
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR' or
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL', and does not
--     have a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar image format>,
--     the @aspectMask@ member of @pSubresource@ /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
--
-- -   #VUID-vkGetImageSubresourceLayout-format-04462# If @format@ of the
--     @image@ has a depth component, the @aspectMask@ member of
--     @pSubresource@ /must/ contain
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT'
--
-- -   #VUID-vkGetImageSubresourceLayout-format-04463# If @format@ of the
--     @image@ has a stencil component, the @aspectMask@ member of
--     @pSubresource@ /must/ contain
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- -   #VUID-vkGetImageSubresourceLayout-format-04464# If @format@ of the
--     @image@ does not contain a stencil or depth component, the
--     @aspectMask@ member of @pSubresource@ /must/ not contain
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT' or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- -   #VUID-vkGetImageSubresourceLayout-tiling-08717# If the @tiling@ of
--     the @image@ is 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR'
--     and has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar image format>,
--     then the @aspectMask@ member of @pSubresource@ /must/ be a single
--     valid
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-planes-image-aspect multi-planar aspect mask>
--     bit
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
--     @VK_IMAGE_ASPECT_MEMORY_PLANE_i_BIT_EXT@ and the index /i/ /must/ be
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
                             -- structure selecting a specific image subresource from the image.
                             ImageSubresource
                          -> io (SubresourceLayout)
getImageSubresourceLayout device image subresource = liftIO . evalContT $ do
  let vkGetImageSubresourceLayoutPtr = pVkGetImageSubresourceLayout (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetImageSubresourceLayoutPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetImageSubresourceLayout is null" Nothing Nothing
  let vkGetImageSubresourceLayout' = mkVkGetImageSubresourceLayout vkGetImageSubresourceLayoutPtr
  pSubresource <- ContT $ withCStruct (subresource)
  pPLayout <- ContT (withZeroCStruct @SubresourceLayout)
  lift $ traceAroundEvent "vkGetImageSubresourceLayout" (vkGetImageSubresourceLayout'
                                                           (deviceHandle (device))
                                                           (image)
                                                           pSubresource
                                                           (pPLayout))
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
-- Images created with one of the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion formats that require a sampler Y′CBCR conversion>,
-- have further restrictions on their limits and capabilities compared to
-- images created with other formats. Creation of images with a format
-- requiring
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion Y′CBCR conversion>
-- /may/ not be supported unless other parameters meet all of the
-- constraints:
--
-- -   @imageType@ is 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'
--
-- -   @mipLevels@ is 1
--
-- -   @arrayLayers@ is 1, unless the @ycbcrImageArrays@ feature is
--     enabled, or otherwise indicated by
--     'Vulkan.Core10.DeviceInitialization.ImageFormatProperties'::@maxArrayLayers@,
--     as returned by
--     'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceImageFormatProperties'
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
-- If the implementation reports 'Vulkan.Core10.FundamentalTypes.TRUE' in
-- 'Vulkan.Extensions.VK_EXT_host_image_copy.PhysicalDeviceHostImageCopyPropertiesEXT'::@identicalMemoryTypeRequirements@,
-- usage of
-- 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_HOST_TRANSFER_BIT_EXT'
-- /must/ not affect the memory type requirements of the image as described
-- in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#sparsememory-memory-requirements Sparse Resource Memory Requirements>
-- and
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#resources-association Resource Memory Association>.
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
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#resources-image-views Image Views>
-- for more detail).
--
-- == Image Creation Limits
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
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#glossary-drm-format-modifier Linux DRM format modifiers>
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
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#glossary-linear-resource linear>.
--
--     -   If @tiling@ is
--         'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR', then
--         @imageCreateMaybeLinear@ is
--         'Vulkan.Core10.FundamentalTypes.TRUE'.
--
--     -   If @tiling@ is
--         'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL', then
--         @imageCreateMaybeLinear@ is
--         'Vulkan.Core10.FundamentalTypes.FALSE'.
--
--     -   If @tiling@ is
--         'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT',
--         then @imageCreateMaybeLinear@ is
--         'Vulkan.Core10.FundamentalTypes.TRUE' if and only if
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
--         or
--         'Vulkan.Extensions.VK_QNX_external_memory_screen_buffer.ExternalFormatQNX'
--         structure with non-zero @externalFormat@, then
--         @imageCreateFormatFeatures@ is the value of
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
--         'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL', and if
--         the @pNext@ chain includes a
--         'Vulkan.Extensions.VK_QNX_external_memory_screen_buffer.ExternalFormatQNX'
--         structure with non-zero @externalFormat@, then
--         @imageCreateFormatFeatures@ is the value of
--         'Vulkan.Extensions.VK_QNX_external_memory_screen_buffer.ScreenBufferFormatPropertiesQNX'::@formatFeatures@
--         obtained by
--         'Vulkan.Extensions.VK_QNX_external_memory_screen_buffer.getScreenBufferPropertiesQNX'
--         with a matching @externalFormat@ value.
--
--     -   If the @pNext@ chain includes a
--         'Vulkan.Extensions.VK_FUCHSIA_buffer_collection.BufferCollectionImageCreateInfoFUCHSIA'
--         structure, then @imageCreateFormatFeatures@ is the value of
--         'Vulkan.Extensions.VK_FUCHSIA_buffer_collection.BufferCollectionPropertiesFUCHSIA'::@formatFeatures@
--         found by calling
--         'Vulkan.Extensions.VK_FUCHSIA_buffer_collection.getBufferCollectionPropertiesFUCHSIA'
--         with a parameter @collection@ equal to
--         'Vulkan.Extensions.VK_FUCHSIA_buffer_collection.BufferCollectionImageCreateInfoFUCHSIA'::@collection@
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
--         'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.FormatProperties2';
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
--         or
--         'Vulkan.Extensions.VK_QNX_external_memory_screen_buffer.ExternalFormatQNX'
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
--         -   If 'ImageCreateInfo'::@pNext@ contains a
--             <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoProfileListInfoKHR VkVideoProfileListInfoKHR>
--             structure then
--             'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2'::@pNext@
--             /must/ also contain the same
--             <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoProfileListInfoKHR VkVideoProfileListInfoKHR>
--             structure on each call.
--
--         -   If @tiling@ is
--             'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT',
--             then:
--
--             -   'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2'::@pNext@
--                 /must/ contain a
--                 'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.PhysicalDeviceImageDrmFormatModifierInfoEXT'
--                 structure where @sharingMode@ is equal to
--                 'ImageCreateInfo'::@sharingMode@;
--
--             -   if @sharingMode@ is
--                 'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT',
--                 then @queueFamilyIndexCount@ and @pQueueFamilyIndices@
--                 /must/ be equal to those in 'ImageCreateInfo';
--
--             -   if @flags@ contains
--                 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_MUTABLE_FORMAT_BIT',
--                 then the
--                 'Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo'
--                 structure included in the @pNext@ chain of
--                 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2'
--                 /must/ be equivalent to the one included in the @pNext@
--                 chain of 'ImageCreateInfo';
--
--             -   if 'ImageCreateInfo'::@pNext@ contains a
--                 'Vulkan.Extensions.VK_EXT_image_compression_control.ImageCompressionControlEXT'
--                 structure, then the
--                 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2'::@pNext@
--                 chain /must/ contain an equivalent structure;
--
--             -   'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--                 /must/ be called for each modifier in
--                 @imageCreateDrmFormatModifiers@, successively setting
--                 'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.PhysicalDeviceImageDrmFormatModifierInfoEXT'::@drmFormatModifier@
--                 on each call.
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
--             'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxImageArrayLayers@.
--
--         -   Each component of
--             'Vulkan.Core10.DeviceInitialization.ImageFormatProperties'::@maxExtent@
--             is
--             'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxImageDimension2D@.
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
-- -   Let @VkVideoFormatPropertiesKHR videoFormatProperties[]@ be defined
--     as follows.
--
--     -   If 'ImageCreateInfo'::@pNext@ contains a
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoProfileListInfoKHR VkVideoProfileListInfoKHR>
--         structure, then @videoFormatProperties@ is the list of
--         structures obtained by calling
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetPhysicalDeviceVideoFormatPropertiesKHR vkGetPhysicalDeviceVideoFormatPropertiesKHR>
--         with
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceVideoFormatInfoKHR VkPhysicalDeviceVideoFormatInfoKHR>::@imageUsage@
--         equal to the @usage@ member of 'ImageCreateInfo' and
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceVideoFormatInfoKHR VkPhysicalDeviceVideoFormatInfoKHR>::@pNext@
--         containing the same
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoProfileListInfoKHR VkVideoProfileListInfoKHR>
--         structure chained to 'ImageCreateInfo'.
--
--     -   If 'ImageCreateInfo'::@pNext@ contains no
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoProfileListInfoKHR VkVideoProfileListInfoKHR>
--         structure, then @videoFormatProperties@ is an empty list.
--
-- -   Let @VkBool32 supportedVideoFormat@ indicate if the image parameters
--     are supported by the specified video profiles.
--
--     -   @supportedVideoFormat@ is 'Vulkan.Core10.FundamentalTypes.TRUE'
--         if there exists an element in the @videoFormatProperties@ list
--         for which all of the following conditions are true:
--
--         -   'ImageCreateInfo'::@format@ equals
--             <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoFormatPropertiesKHR VkVideoFormatPropertiesKHR>::@format@.
--
--         -   'ImageCreateInfo'::@flags@ only contains bits also set in
--             <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoFormatPropertiesKHR VkVideoFormatPropertiesKHR>::@imageCreateFlags@.
--
--         -   'ImageCreateInfo'::@imageType@ equals
--             <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoFormatPropertiesKHR VkVideoFormatPropertiesKHR>::@imageType@.
--
--         -   'ImageCreateInfo'::@tiling@ equals
--             <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoFormatPropertiesKHR VkVideoFormatPropertiesKHR>::@imageTiling@.
--
--         -   'ImageCreateInfo'::@usage@ only contains bits also set in
--             <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoFormatPropertiesKHR VkVideoFormatPropertiesKHR>::@imageUsageFlags@.
--
--     -   Otherwise @supportedVideoFormat@ is
--         'Vulkan.Core10.FundamentalTypes.FALSE'.
--
-- == Valid Usage
--
-- -   #VUID-VkImageCreateInfo-imageCreateMaxMipLevels-02251# Each of the
--     following values (as described in
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>)
--     /must/ not be undefined : @imageCreateMaxMipLevels@,
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
-- -   #VUID-VkImageCreateInfo-flags-08865# If @flags@ contains
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CUBE_COMPATIBLE_BIT',
--     @extent.width@ and @extent.height@ /must/ be equal
--
-- -   #VUID-VkImageCreateInfo-flags-08866# If @flags@ contains
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CUBE_COMPATIBLE_BIT',
--     @arrayLayers@ /must/ be greater than or equal to 6
--
-- -   #VUID-VkImageCreateInfo-flags-02557# If @flags@ contains
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT',
--     @imageType@ /must/ be 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'
--
-- -   #VUID-VkImageCreateInfo-flags-00950# If @flags@ contains
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT',
--     @imageType@ /must/ be 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D'
--
-- -   #VUID-VkImageCreateInfo-flags-09403# If @flags@ contains
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT',
--     @flags@ /must/ not include
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_ALIASED_BIT',
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_BINDING_BIT',
--     or
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--
-- -   #VUID-VkImageCreateInfo-flags-07755# If @flags@ contains
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_2D_VIEW_COMPATIBLE_BIT_EXT',
--     @imageType@ /must/ be 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D'
--
-- -   #VUID-VkImageCreateInfo-extent-02252# @extent.width@ /must/ be less
--     than or equal to @imageCreateMaxExtent.width@ (as defined in
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>)
--
-- -   #VUID-VkImageCreateInfo-extent-02253# @extent.height@ /must/ be less
--     than or equal to @imageCreateMaxExtent.height@ (as defined in
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>)
--
-- -   #VUID-VkImageCreateInfo-extent-02254# @extent.depth@ /must/ be less
--     than or equal to @imageCreateMaxExtent.depth@ (as defined in
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>)
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
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>)
--
-- -   #VUID-VkImageCreateInfo-arrayLayers-02256# @arrayLayers@ /must/ be
--     less than or equal to @imageCreateMaxArrayLayers@ (as defined in
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>)
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
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>)
--     /must/ be 'Vulkan.Core10.FundamentalTypes.FALSE',
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
-- -   #VUID-VkImageCreateInfo-fragmentDensityMapOffset-06514# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-fragmentDensityMapOffsets fragmentDensityMapOffset>
--     feature is not enabled and @usage@ includes
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT',
--     @extent.width@ /must/ be less than or equal to
--     \(\left\lceil{\frac{maxFramebufferWidth}{minFragmentDensityTexelSize_{width}}}\right\rceil\)
--
-- -   #VUID-VkImageCreateInfo-fragmentDensityMapOffset-06515# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-fragmentDensityMapOffsets fragmentDensityMapOffset>
--     feature is not enabled and @usage@ includes
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
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>)
--
-- -   #VUID-VkImageCreateInfo-usage-00968# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-shaderStorageImageMultisample shaderStorageImageMultisample>
--     feature is not enabled, and @usage@ contains
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_STORAGE_BIT',
--     @samples@ /must/ be
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-VkImageCreateInfo-flags-00969# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-sparseBinding sparseBinding>
--     feature is not enabled, @flags@ /must/ not contain
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_BINDING_BIT'
--
-- -   #VUID-VkImageCreateInfo-flags-01924# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-sparseResidencyAliased sparseResidencyAliased>
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
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-sparseResidencyImage2D sparseResidencyImage2D>
--     feature is not enabled, and @imageType@ is
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', @flags@ /must/ not
--     contain
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--
-- -   #VUID-VkImageCreateInfo-imageType-00972# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-sparseResidencyImage3D sparseResidencyImage3D>
--     feature is not enabled, and @imageType@ is
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', @flags@ /must/ not
--     contain
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--
-- -   #VUID-VkImageCreateInfo-imageType-00973# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-sparseResidency2Samples sparseResidency2Samples>
--     feature is not enabled, @imageType@ is
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', and @samples@ is
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_2_BIT',
--     @flags@ /must/ not contain
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--
-- -   #VUID-VkImageCreateInfo-imageType-00974# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-sparseResidency4Samples sparseResidency4Samples>
--     feature is not enabled, @imageType@ is
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', and @samples@ is
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_4_BIT',
--     @flags@ /must/ not contain
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--
-- -   #VUID-VkImageCreateInfo-imageType-00975# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-sparseResidency8Samples sparseResidency8Samples>
--     feature is not enabled, @imageType@ is
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', and @samples@ is
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_8_BIT',
--     @flags@ /must/ not contain
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--
-- -   #VUID-VkImageCreateInfo-imageType-00976# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-sparseResidency16Samples sparseResidency16Samples>
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
-- -   #VUID-VkImageCreateInfo-flags-01890# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-protectedMemory protectedMemory>
--     feature is not enabled, @flags@ /must/ not contain
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
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>)
--     /must/ be 'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-VkImageCreateInfo-flags-01572# If @flags@ contains
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT',
--     then @format@ /must/ be a
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#compressed_image_formats compressed image format>
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
-- -   #VUID-VkImageCreateInfo-format-06410# If the image @format@ is one
--     of the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion formats that require a sampler Y′CBCR conversion>,
--     @mipLevels@ /must/ be 1
--
-- -   #VUID-VkImageCreateInfo-format-06411# If the image @format@ is one
--     of the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion formats that require a sampler Y′CBCR conversion>,
--     @samples@ /must/ be
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-VkImageCreateInfo-format-06412# If the image @format@ is one
--     of the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion formats that require a sampler Y′CBCR conversion>,
--     @imageType@ /must/ be 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'
--
-- -   #VUID-VkImageCreateInfo-imageCreateFormatFeatures-02260# If @format@
--     is a /multi-planar/ format, and if @imageCreateFormatFeatures@ (as
--     defined in
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>)
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
-- -   #VUID-VkImageCreateInfo-pNext-08951# If the @pNext@ chain includes a
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo'
--     structure whose @handleTypes@ member includes
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_SCREEN_BUFFER_BIT_QNX',
--     @imageType@ /must/ be 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'
--
-- -   #VUID-VkImageCreateInfo-pNext-08952# If the @pNext@ chain includes a
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo'
--     structure whose @handleTypes@ member includes
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_SCREEN_BUFFER_BIT_QNX',
--     @mipLevels@ /must/ either be @1@ or equal to the number of levels in
--     the complete mipmap chain based on @extent.width@, @extent.height@,
--     and @extent.depth@
--
-- -   #VUID-VkImageCreateInfo-pNext-08953# If the @pNext@ chain includes a
--     'Vulkan.Extensions.VK_QNX_external_memory_screen_buffer.ExternalFormatQNX'
--     structure whose @externalFormat@ member is not @0@, @flags@ /must/
--     not include
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_MUTABLE_FORMAT_BIT'
--
-- -   #VUID-VkImageCreateInfo-pNext-08954# If the @pNext@ chain includes a
--     'Vulkan.Extensions.VK_QNX_external_memory_screen_buffer.ExternalFormatQNX'
--     structure whose @externalFormat@ member is not @0@, @usage@ /must/
--     not include any usages except
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT'
--
-- -   #VUID-VkImageCreateInfo-pNext-08955# If the @pNext@ chain includes a
--     'Vulkan.Extensions.VK_QNX_external_memory_screen_buffer.ExternalFormatQNX'
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
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-shaderStorageImageMultisample shaderStorageImageMultisample>
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
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR',
--     @imageType@ /must/ be 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'
--
-- -   #VUID-VkImageCreateInfo-samples-02083# If @usage@ includes
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR',
--     @samples@ /must/ be
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-VkImageCreateInfo-shadingRateImage-07727# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-shadingRateImage shadingRateImage>
--     feature is enabled and @usage@ includes
--     'Vulkan.Extensions.VK_NV_shading_rate_image.IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV',
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
-- -   #VUID-VkImageCreateInfo-usage-04992# If @usage@ includes
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INVOCATION_MASK_BIT_HUAWEI',
--     @tiling@ /must/ be
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR'
--
-- -   #VUID-VkImageCreateInfo-imageView2DOn3DImage-04459# If the
--     @VK_KHR_portability_subset@ extension is enabled, and
--     'Vulkan.Extensions.VK_KHR_portability_subset.PhysicalDevicePortabilitySubsetFeaturesKHR'::@imageView2DOn3DImage@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', @flags@ /must/ not
--     contain
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT'
--
-- -   #VUID-VkImageCreateInfo-multisampleArrayImage-04460# If the
--     @VK_KHR_portability_subset@ extension is enabled, and
--     'Vulkan.Extensions.VK_KHR_portability_subset.PhysicalDevicePortabilitySubsetFeaturesKHR'::@multisampleArrayImage@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', and @samples@ is not
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT', then
--     @arrayLayers@ /must/ be @1@
--
-- -   #VUID-VkImageCreateInfo-pNext-06722# If a
--     'Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo'
--     structure was included in the @pNext@ chain and
--     'Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo'::@viewFormatCount@
--     is not zero, then each format in
--     'Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo'::@pViewFormats@
--     /must/ either be compatible with the @format@ as described in the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#formats-compatibility compatibility table>
--     or, if @flags@ contains
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT',
--     be an uncompressed format that is size-compatible with @format@
--
-- -   #VUID-VkImageCreateInfo-flags-04738# If @flags@ does not contain
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_MUTABLE_FORMAT_BIT'
--     and the @pNext@ chain includes a
--     'Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo'
--     structure, then
--     'Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo'::@viewFormatCount@
--     /must/ be @0@ or @1@
--
-- -   #VUID-VkImageCreateInfo-usage-04815# If @usage@ includes
--     @VK_IMAGE_USAGE_VIDEO_DECODE_SRC_BIT_KHR@,
--     @VK_IMAGE_USAGE_VIDEO_DECODE_DST_BIT_KHR@, or
--     @VK_IMAGE_USAGE_VIDEO_DECODE_DPB_BIT_KHR@, then the @pNext@ chain
--     /must/ include a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoProfileListInfoKHR VkVideoProfileListInfoKHR>
--     structure with @profileCount@ greater than @0@ and @pProfiles@
--     including at least one
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoProfileInfoKHR VkVideoProfileInfoKHR>
--     structure with a @videoCodecOperation@ member specifying a decode
--     operation
--
-- -   #VUID-VkImageCreateInfo-usage-04816# If @usage@ includes
--     @VK_IMAGE_USAGE_VIDEO_ENCODE_SRC_BIT_KHR@,
--     @VK_IMAGE_USAGE_VIDEO_ENCODE_DST_BIT_KHR@, or
--     @VK_IMAGE_USAGE_VIDEO_ENCODE_DPB_BIT_KHR@, then the @pNext@ chain
--     /must/ include a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoProfileListInfoKHR VkVideoProfileListInfoKHR>
--     structure with @profileCount@ greater than @0@ and @pProfiles@
--     including at least one
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoProfileInfoKHR VkVideoProfileInfoKHR>
--     structure with a @videoCodecOperation@ member specifying an encode
--     operation
--
-- -   #VUID-VkImageCreateInfo-pNext-06811# If the @pNext@ chain includes a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoProfileListInfoKHR VkVideoProfileListInfoKHR>
--     structure with @profileCount@ greater than @0@, then
--     @supportedVideoFormat@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-VkImageCreateInfo-pNext-06390# If the
--     'Vulkan.Core10.Handles.Image' is to be used to import memory from a
--     'Vulkan.Extensions.Handles.BufferCollectionFUCHSIA', a
--     'Vulkan.Extensions.VK_FUCHSIA_buffer_collection.BufferCollectionImageCreateInfoFUCHSIA'
--     structure /must/ be chained to @pNext@
--
-- -   #VUID-VkImageCreateInfo-multisampledRenderToSingleSampled-06882# If
--     the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-multisampledRenderToSingleSampled multisampledRenderToSingleSampled>
--     feature is not enabled, @flags@ /must/ not contain
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_BIT_EXT'
--
-- -   #VUID-VkImageCreateInfo-flags-06883# If @flags@ contains
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_BIT_EXT',
--     @samples@ /must/ be
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-VkImageCreateInfo-pNext-06743# If the @pNext@ chain includes a
--     'Vulkan.Extensions.VK_EXT_image_compression_control.ImageCompressionControlEXT'
--     structure, @format@ is a
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar>
--     format, and
--     'Vulkan.Extensions.VK_EXT_image_compression_control.ImageCompressionControlEXT'::@flags@
--     includes
--     'Vulkan.Extensions.VK_EXT_image_compression_control.IMAGE_COMPRESSION_FIXED_RATE_EXPLICIT_EXT',
--     then
--     'Vulkan.Extensions.VK_EXT_image_compression_control.ImageCompressionControlEXT'::@compressionControlPlaneCount@
--     /must/ be equal to the number of planes in @format@
--
-- -   #VUID-VkImageCreateInfo-pNext-06744# If the @pNext@ chain includes a
--     'Vulkan.Extensions.VK_EXT_image_compression_control.ImageCompressionControlEXT'
--     structure, @format@ is not a
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar>
--     format, and
--     'Vulkan.Extensions.VK_EXT_image_compression_control.ImageCompressionControlEXT'::@flags@
--     includes
--     'Vulkan.Extensions.VK_EXT_image_compression_control.IMAGE_COMPRESSION_FIXED_RATE_EXPLICIT_EXT',
--     then
--     'Vulkan.Extensions.VK_EXT_image_compression_control.ImageCompressionControlEXT'::@compressionControlPlaneCount@
--     /must/ be 1
--
-- -   #VUID-VkImageCreateInfo-pNext-06746# If the @pNext@ chain includes a
--     'Vulkan.Extensions.VK_EXT_image_compression_control.ImageCompressionControlEXT'
--     structure, it /must/ not contain a
--     'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.ImageDrmFormatModifierExplicitCreateInfoEXT'
--     structure
--
-- -   #VUID-VkImageCreateInfo-flags-08104# If @flags@ includes
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT',
--     the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-descriptorBufferCaptureReplay descriptorBufferCaptureReplay>
--     feature /must/ be enabled
--
-- -   #VUID-VkImageCreateInfo-pNext-08105# If the @pNext@ chain includes a
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.OpaqueCaptureDescriptorDataCreateInfoEXT'
--     structure, @flags@ /must/ contain
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT'
--
-- -   #VUID-VkImageCreateInfo-pNext-06783# If the @pNext@ chain includes a
--     'Vulkan.Extensions.VK_EXT_metal_objects.ExportMetalObjectCreateInfoEXT'
--     structure, its @exportObjectType@ member /must/ be either
--     'Vulkan.Extensions.VK_EXT_metal_objects.EXPORT_METAL_OBJECT_TYPE_METAL_TEXTURE_BIT_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_metal_objects.EXPORT_METAL_OBJECT_TYPE_METAL_IOSURFACE_BIT_EXT'
--
-- -   #VUID-VkImageCreateInfo-pNext-06784# If the @pNext@ chain includes a
--     'Vulkan.Extensions.VK_EXT_metal_objects.ImportMetalTextureInfoEXT'
--     structure its @plane@ member /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT',
--     or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'
--
-- -   #VUID-VkImageCreateInfo-pNext-06785# If the @pNext@ chain includes a
--     'Vulkan.Extensions.VK_EXT_metal_objects.ImportMetalTextureInfoEXT'
--     structure and the image does not have a multi-planar format, then
--     'Vulkan.Extensions.VK_EXT_metal_objects.ImportMetalTextureInfoEXT'::@plane@
--     /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT'
--
-- -   #VUID-VkImageCreateInfo-pNext-06786# If the @pNext@ chain includes a
--     'Vulkan.Extensions.VK_EXT_metal_objects.ImportMetalTextureInfoEXT'
--     structure and the image has a multi-planar format with only two
--     planes, then
--     'Vulkan.Extensions.VK_EXT_metal_objects.ImportMetalTextureInfoEXT'::@plane@
--     /must/ not be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'
--
-- -   #VUID-VkImageCreateInfo-imageCreateFormatFeatures-09048# If
--     @imageCreateFormatFeatures@ (as defined in
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#resources-image-creation-limits Image Creation Limits>)
--     does not contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_HOST_IMAGE_TRANSFER_BIT_EXT',
--     then @usage@ /must/ not contain
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_HOST_TRANSFER_BIT_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImageCreateInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_CREATE_INFO'
--
-- -   #VUID-VkImageCreateInfo-pNext-pNext# Each @pNext@ member of any
--     structure (including this one) in the @pNext@ chain /must/ be either
--     @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_FUCHSIA_buffer_collection.BufferCollectionImageCreateInfoFUCHSIA',
--     'Vulkan.Extensions.VK_NV_dedicated_allocation.DedicatedAllocationImageCreateInfoNV',
--     'Vulkan.Extensions.VK_EXT_metal_objects.ExportMetalObjectCreateInfoEXT',
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID',
--     'Vulkan.Extensions.VK_QNX_external_memory_screen_buffer.ExternalFormatQNX',
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo',
--     'Vulkan.Extensions.VK_NV_external_memory.ExternalMemoryImageCreateInfoNV',
--     'Vulkan.Extensions.VK_EXT_image_compression_control.ImageCompressionControlEXT',
--     'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.ImageDrmFormatModifierExplicitCreateInfoEXT',
--     'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.ImageDrmFormatModifierListCreateInfoEXT',
--     'Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo',
--     'Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo',
--     'Vulkan.Extensions.VK_KHR_swapchain.ImageSwapchainCreateInfoKHR',
--     'Vulkan.Extensions.VK_EXT_metal_objects.ImportMetalIOSurfaceInfoEXT',
--     'Vulkan.Extensions.VK_EXT_metal_objects.ImportMetalTextureInfoEXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.OpaqueCaptureDescriptorDataCreateInfoEXT',
--     'Vulkan.Extensions.VK_NV_optical_flow.OpticalFlowImageFormatInfoNV',
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoProfileListInfoKHR VkVideoProfileListInfoKHR>
--
-- -   #VUID-VkImageCreateInfo-sType-unique# The @sType@ value of each
--     struct in the @pNext@ chain /must/ be unique, with the exception of
--     structures of type
--     'Vulkan.Extensions.VK_EXT_metal_objects.ExportMetalObjectCreateInfoEXT'
--     or
--     'Vulkan.Extensions.VK_EXT_metal_objects.ImportMetalTextureInfoEXT'
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
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core13.Promoted_From_VK_KHR_maintenance4.DeviceImageMemoryRequirements',
-- 'Vulkan.Extensions.VK_KHR_maintenance5.DeviceImageSubresourceInfoKHR',
-- 'Vulkan.Core10.FundamentalTypes.Extent3D',
-- 'Vulkan.Core10.Enums.Format.Format',
-- 'Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlags',
-- 'Vulkan.Extensions.VK_FUCHSIA_buffer_collection.ImageFormatConstraintsInfoFUCHSIA',
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
    -- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' value
    -- specifying the number of
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#primsrast-multisampling samples per texel>.
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
  , -- | @pQueueFamilyIndices@ is a pointer to an array of queue families that
    -- will access this image. It is ignored if @sharingMode@ is not
    -- 'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT'.
    queueFamilyIndices :: Vector Word32
  , -- | @initialLayout@ is a 'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
    -- specifying the initial 'Vulkan.Core10.Enums.ImageLayout.ImageLayout' of
    -- all image subresources of the image. See
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#resources-image-layouts Image Layouts>.
    initialLayout :: ImageLayout
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (ImageCreateInfo es)

instance Extensible ImageCreateInfo where
  extensibleTypeName = "ImageCreateInfo"
  setNext ImageCreateInfo{..} next' = ImageCreateInfo{next = next', ..}
  getNext ImageCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends ImageCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @ExternalFormatQNX = Just f
    | Just Refl <- eqT @e @OpticalFlowImageFormatInfoNV = Just f
    | Just Refl <- eqT @e @ImportMetalIOSurfaceInfoEXT = Just f
    | Just Refl <- eqT @e @ImportMetalTextureInfoEXT = Just f
    | Just Refl <- eqT @e @ExportMetalObjectCreateInfoEXT = Just f
    | Just Refl <- eqT @e @ImageCompressionControlEXT = Just f
    | Just Refl <- eqT @e @BufferCollectionImageCreateInfoFUCHSIA = Just f
    | Just Refl <- eqT @e @OpaqueCaptureDescriptorDataCreateInfoEXT = Just f
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

instance ( Extendss ImageCreateInfo es
         , PokeChain es ) => ToCStruct (ImageCreateInfo es) where
  withCStruct x f = allocaBytes 88 $ \p -> pokeCStruct p x (f p)
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
    pPQueueFamilyIndices' <- ContT $ allocaBytes @Word32 ((Data.Vector.length (queueFamilyIndices)) * 4)
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
    lift $ poke ((p `plusPtr` 80 :: Ptr ImageLayout)) (zero)
    lift $ f

instance ( Extendss ImageCreateInfo es
         , PeekChain es ) => FromCStruct (ImageCreateInfo es) where
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
             next
             flags
             imageType
             format
             extent
             mipLevels
             arrayLayers
             samples
             tiling
             usage
             sharingMode
             pQueueFamilyIndices'
             initialLayout

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
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#glossary-linear-resource linear>,
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
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>
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
-- allowed plane index /i/ is defined by the
-- 'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.DrmFormatModifierPropertiesEXT'::@drmFormatModifierPlaneCount@
-- associated with the image’s 'ImageCreateInfo'::@format@ and
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#glossary-drm-format-modifier modifier>.
-- The memory range used by the subresource is described by @offset@ and
-- @size@. If the image is /disjoint/, then the @offset@ is relative to the
-- base address of the /memory plane/. If the image is /non-disjoint/, then
-- the @offset@ is relative to the base address of the image. If the image
-- is
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#glossary-linear-resource non-linear>,
-- then @rowPitch@, @arrayPitch@, and @depthPitch@ have an
-- implementation-dependent meaning.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.ImageDrmFormatModifierExplicitCreateInfoEXT',
-- 'Vulkan.Extensions.VK_KHR_maintenance5.SubresourceLayout2KHR',
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
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
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

